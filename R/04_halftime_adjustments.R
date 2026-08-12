# =============================================================================
# 04_halftime_adjustments.R
#
# Purpose: test "Adjustments" as a coach-ranking dimension. The video's
# collaborator listed halftime adjustments as a way to grade coaches, on the
# assumption that some coaches are reliably better at fixing what's broken
# after the first half. This script checks whether that's a repeatable coach
# skill or mostly noise.
#
# Ask: does a coach's second-half improvement over his own first half predict
# anything about him specifically, once we strip out the mechanical parts of
# second-half swings (games mean-revert, blowouts create garbage time)?
#
# Sources:
#   - Play-by-play: local gzipped nflverse pbp,
#     ~/stranger9977/nfl-analysis/data/play_by_play_YYYY.csv.gz, 2000-2025.
#     Falls back to nflreadr::load_pbp() if a local file is missing.
#     Full 2000-2025 window used (26 seasons) for coach sample size; each
#     season's raw pbp is read with only the needed columns and dropped
#     after aggregating to game-half rows, so peak memory stays small.
#   - Coaches per game: ~/stranger9977/nfl-analysis/data/games.csv
#     (home_coach / away_coach). Regular season only (game_type == "REG").
#
# Method:
#   1. Per team-game-half: offensive EPA/play (posteam), pass/run plays only,
#      kneels and spikes excluded (they have their own play_type in nflverse
#      data -- "qb_kneel"/"qb_spike" -- so the pass/run filter already drops
#      them; qb_kneel/qb_spike flags are also checked as a belt-and-suspenders
#      guard for older seasons). Margin = team's H1 (or H2) EPA/play minus the
#      opponent's EPA/play in that same half.
#   2. Adjustment delta = H2 margin - H1 margin, raw.
#   3. The trap: H2 margin mean-reverts mechanically off H1 margin, and
#      garbage time distorts blowouts. Residualize with a league-wide
#      regression: h2_margin ~ h1_margin + halftime_score_diff. A coach's
#      adjustment score is the mean residual across his games.
#   4. Repeatability, the actual test of "is this a skill":
#      a. Split-half: odd vs even games within a coach's career (min 32
#         games so each half has ~16+).
#      b. Year-to-year: coach-season mean residual, season t vs t+1.
#      c. Funnel/noise cone: career mean residual vs games coached, against
#         the spread pure luck would produce given the per-game residual SD.
#
# Out:
#   - docs/figures/halftime_cone.png
#   - docs/figures/halftime_persistence.png
#   - data/derived/halftime_coach.csv       (coach career table)
#   - data/derived/halftime_coach_season.csv (coach-season table, for the
#     year-to-year test)
#   - data/derived/halftime_games.csv       (team-game grain table)
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
  library(ggrepel)
})

source("R/lib/theme_coach.R")

DATA_DIR   <- path.expand("~/stranger9977/nfl-analysis/data")
GAMES_PATH <- file.path(DATA_DIR, "games.csv")
SEASONS    <- 2000:2025

MIN_PLAYS_PER_HALF <- 5   # drop team-game-halves with too few offensive snaps to trust
MIN_GAMES_SPLIT     <- 32 # split-half test: min career games so each arm has ~16+
MIN_GAMES_SEASON    <- 8  # year-to-year test: min games in a season to count it
MIN_GAMES_CONE      <- 8  # floor for coaches shown on the cone chart
Z_95                <- 1.96

FAMOUS_COACHES <- c("Bill Belichick", "Andy Reid", "Mike Tomlin", "Sean McVay",
                     "Kyle Shanahan", "Mike Macdonald", "Pete Carroll")

# ---------------------------------------------------------------------------
# Loaders
# ---------------------------------------------------------------------------

pbp_cols <- c("play_id", "game_id", "season", "week", "season_type",
              "game_half", "posteam_type", "play_type", "epa",
              "qb_kneel", "qb_spike", "total_home_score", "total_away_score",
              "home_team", "away_team")

load_pbp_season <- function(season, cols, data_dir = DATA_DIR) {
  local_path <- file.path(data_dir, sprintf("play_by_play_%d.csv.gz", season))
  if (file.exists(local_path)) {
    read_csv(local_path, col_select = all_of(cols),
             show_col_types = FALSE, progress = FALSE)
  } else {
    message(sprintf("no local pbp for %d, falling back to nflreadr::load_pbp()", season))
    select(nflreadr::load_pbp(season), any_of(cols))
  }
}

# ---------------------------------------------------------------------------
# Build team-game-half margins, season by season, discarding raw pbp as we go
# ---------------------------------------------------------------------------

season_tables <- vector("list", length(SEASONS))

for (i in seq_along(SEASONS)) {
  s <- SEASONS[i]
  raw <- load_pbp_season(s, pbp_cols) %>% filter(season_type == "REG")

  # halftime score, from the last play of Half1 (highest play_id), any play type
  halftime_score <- raw %>%
    filter(game_half == "Half1", !is.na(total_home_score)) %>%
    group_by(game_id) %>%
    slice_max(play_id, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(game_id, home_score_half = total_home_score, away_score_half = total_away_score)

  # offensive EPA/play by team-game-half; pass/run only, kneels/spikes out
  off <- raw %>%
    filter(play_type %in% c("run", "pass"),
           !is.na(epa),
           is.na(qb_kneel) | qb_kneel == 0,
           is.na(qb_spike) | qb_spike == 0,
           game_half %in% c("Half1", "Half2"),
           !is.na(posteam_type)) %>%
    group_by(game_id, season, week, home_team, away_team, game_half, posteam_type) %>%
    summarise(mean_epa = mean(epa), n_plays = n(), .groups = "drop")

  half_wide <- off %>%
    pivot_wider(id_cols = c(game_id, season, week, home_team, away_team, game_half),
                names_from = posteam_type, values_from = c(mean_epa, n_plays)) %>%
    mutate(margin_home = mean_epa_home - mean_epa_away,
           margin_away = mean_epa_away - mean_epa_home)

  game_off <- half_wide %>%
    select(game_id, season, week, home_team, away_team, game_half,
           margin_home, margin_away, n_plays_home, n_plays_away) %>%
    pivot_wider(names_from = game_half,
                values_from = c(margin_home, margin_away, n_plays_home, n_plays_away)) %>%
    left_join(halftime_score, by = "game_id")

  season_tables[[i]] <- game_off
  n_games <- nrow(game_off)
  rm(raw, off, half_wide, halftime_score, game_off); invisible(gc(FALSE))
  cat(sprintf("season %d: %d games processed\n", s, n_games))
}

games_off <- bind_rows(season_tables)
rm(season_tables); invisible(gc(FALSE))

games_off <- games_off %>%
  mutate(halftime_diff_home = home_score_half - away_score_half,
         halftime_diff_away = -halftime_diff_home)

home_rows <- games_off %>%
  transmute(game_id, season, week, team = home_team, opponent = away_team, is_home = TRUE,
            h1_margin = margin_home_Half1, h2_margin = margin_home_Half2,
            n_plays_h1 = n_plays_home_Half1, n_plays_h2 = n_plays_home_Half2,
            halftime_diff = halftime_diff_home)

away_rows <- games_off %>%
  transmute(game_id, season, week, team = away_team, opponent = home_team, is_home = FALSE,
            h1_margin = margin_away_Half1, h2_margin = margin_away_Half2,
            n_plays_h1 = n_plays_away_Half1, n_plays_h2 = n_plays_away_Half2,
            halftime_diff = halftime_diff_away)

team_game <- bind_rows(home_rows, away_rows) %>%
  filter(!is.na(h1_margin), !is.na(h2_margin), !is.na(halftime_diff),
         n_plays_h1 >= MIN_PLAYS_PER_HALF, n_plays_h2 >= MIN_PLAYS_PER_HALF)

# ---------------------------------------------------------------------------
# Attach coaches (regular season games only)
# ---------------------------------------------------------------------------

games_meta <- read_csv(GAMES_PATH,
                        col_select = c(game_id, game_type, home_coach, away_coach),
                        show_col_types = FALSE) %>%
  filter(game_type == "REG")

team_game <- team_game %>%
  inner_join(games_meta, by = "game_id") %>%
  mutate(coach = if_else(is_home, home_coach, away_coach)) %>%
  filter(!is.na(coach), nzchar(coach)) %>%
  select(-home_coach, -away_coach, -game_type)

cat(sprintf("\nteam-game rows after filters: %d (min %d plays/half)\n",
            nrow(team_game), MIN_PLAYS_PER_HALF))

# ---------------------------------------------------------------------------
# League-wide residualizing regression -- the mean-reversion / garbage-time trap
# ---------------------------------------------------------------------------

league_model <- lm(h2_margin ~ h1_margin + halftime_diff, data = team_game)
team_game$adj_residual <- residuals(league_model)

lm_coefs <- coef(league_model)
lm_r2    <- summary(league_model)$r.squared
sigma_game <- sd(team_game$adj_residual)

cat("\n=== league H2 ~ H1 regression (the mean-reversion check) ===\n")
cat(sprintf("intercept:            %.4f\n", lm_coefs[["(Intercept)"]]))
cat(sprintf("H1 margin slope:      %.4f  (how much of a hot/cold H1 mechanically fades in H2)\n",
            lm_coefs[["h1_margin"]]))
cat(sprintf("halftime diff slope:  %.5f  (per point of halftime lead/deficit)\n",
            lm_coefs[["halftime_diff"]]))
cat(sprintf("R^2:                  %.4f\n", lm_r2))
cat(sprintf("residual SD (sigma):  %.4f  (used for the noise cone)\n", sigma_game))

# ---------------------------------------------------------------------------
# Coach rollups
# ---------------------------------------------------------------------------

coach_games <- team_game %>%
  arrange(coach, season, week, game_id) %>%
  group_by(coach) %>%
  mutate(game_number = row_number()) %>%
  ungroup()

coach_career <- coach_games %>%
  group_by(coach) %>%
  summarise(n_games = n(),
            first_season = min(season),
            last_season = max(season),
            mean_residual = mean(adj_residual),
            sd_residual = sd(adj_residual),
            .groups = "drop") %>%
  arrange(desc(mean_residual))

coach_season <- team_game %>%
  group_by(coach, season) %>%
  summarise(n_games = n(), mean_residual = mean(adj_residual), .groups = "drop")

# ---------------------------------------------------------------------------
# Repeatability test A: split-half (odd vs even games within a career)
# ---------------------------------------------------------------------------

split_half_wide <- coach_games %>%
  group_by(coach) %>%
  filter(n() >= MIN_GAMES_SPLIT) %>%
  mutate(half_group = if_else(game_number %% 2 == 1, "odd", "even")) %>%
  group_by(coach, half_group) %>%
  summarise(mean_res = mean(adj_residual), n = n(), .groups = "drop") %>%
  pivot_wider(names_from = half_group, values_from = c(mean_res, n)) %>%
  filter(!is.na(mean_res_odd), !is.na(mean_res_even))

r_split <- cor(split_half_wide$mean_res_odd, split_half_wide$mean_res_even)
n_split <- nrow(split_half_wide)

cat(sprintf("\n=== split-half repeatability (min %d career games) ===\n", MIN_GAMES_SPLIT))
cat(sprintf("coaches: %d\n", n_split))
cat(sprintf("r (odd games vs even games): %.3f\n", r_split))

# ---------------------------------------------------------------------------
# Repeatability test B: year-to-year (coach-season t vs t+1)
# ---------------------------------------------------------------------------

coach_season_qual <- coach_season %>% filter(n_games >= MIN_GAMES_SEASON)

yoy_pairs <- coach_season_qual %>%
  arrange(coach, season) %>%
  group_by(coach) %>%
  mutate(next_season = lead(season), next_mean = lead(mean_residual)) %>%
  ungroup() %>%
  filter(!is.na(next_season), next_season == season + 1) %>%
  transmute(coach, season_t = season, season_t1 = next_season,
            mean_t = mean_residual, mean_t1 = next_mean)

r_yoy <- cor(yoy_pairs$mean_t, yoy_pairs$mean_t1)
n_yoy <- nrow(yoy_pairs)

cat(sprintf("\n=== year-to-year repeatability (min %d games/season) ===\n", MIN_GAMES_SEASON))
cat(sprintf("season pairs: %d\n", n_yoy))
cat(sprintf("r (season t vs season t+1): %.3f\n", r_yoy))

# ---------------------------------------------------------------------------
# Repeatability test C: funnel / noise cone
# ---------------------------------------------------------------------------

coach_cone <- coach_career %>%
  filter(n_games >= MIN_GAMES_CONE) %>%
  mutate(band_95 = Z_95 * sigma_game / sqrt(n_games),
         outside_95 = abs(mean_residual) > band_95)

n_cone_total   <- nrow(coach_cone)
n_cone_outside <- sum(coach_cone$outside_95)
n_cone_inside  <- n_cone_total - n_cone_outside
expected_outside_by_chance <- 0.05 * n_cone_total

cat(sprintf("\n=== noise cone (min %d career games, 95%% band) ===\n", MIN_GAMES_CONE))
cat(sprintf("coaches shown: %d\n", n_cone_total))
cat(sprintf("outside the 95%% band: %d\n", n_cone_outside))
cat(sprintf("inside the 95%% band: %d\n", n_cone_inside))
cat(sprintf("expected outside by pure chance (5%% of %d): %.1f\n",
            n_cone_total, expected_outside_by_chance))

# ---------------------------------------------------------------------------
# Top/bottom 10 (qualified: min career games for split-half, i.e. a real sample)
# ---------------------------------------------------------------------------

coach_qualified <- coach_career %>% filter(n_games >= MIN_GAMES_SPLIT)

top10 <- coach_qualified %>% arrange(desc(mean_residual)) %>% head(10)
bot10 <- coach_qualified %>% arrange(mean_residual) %>% head(10)

cat(sprintf("\n=== top 10 coaches by career mean adjustment residual (min %d games) ===\n",
            MIN_GAMES_SPLIT))
for (r in seq_len(nrow(top10))) {
  cat(sprintf("%2d. %-20s  mean_residual=%+.4f  n_games=%d  (%d-%d)\n",
              r, top10$coach[r], top10$mean_residual[r], top10$n_games[r],
              top10$first_season[r], top10$last_season[r]))
}

cat(sprintf("\n=== bottom 10 coaches by career mean adjustment residual (min %d games) ===\n",
            MIN_GAMES_SPLIT))
for (r in seq_len(nrow(bot10))) {
  cat(sprintf("%2d. %-20s  mean_residual=%+.4f  n_games=%d  (%d-%d)\n",
              r, bot10$coach[r], bot10$mean_residual[r], bot10$n_games[r],
              bot10$first_season[r], bot10$last_season[r]))
}

# ---------------------------------------------------------------------------
# Confound check: is the "adjustment" residual just team quality in disguise?
# Good coaches tend to also have good rosters and keep their jobs, so a
# coach's career mean residual could repeat across games/seasons for a much
# less interesting reason than "he's great at halftime": his team is just
# good, full stop. Check against plain point margin and win rate.
# ---------------------------------------------------------------------------

games_scores <- read_csv(GAMES_PATH,
                          col_select = c(game_id, game_type, home_team, away_team,
                                         home_score, away_score, home_coach, away_coach),
                          show_col_types = FALSE) %>%
  filter(game_type == "REG", !is.na(home_score))

point_margin <- bind_rows(
  transmute(games_scores, coach = home_coach, point_margin = home_score - away_score),
  transmute(games_scores, coach = away_coach, point_margin = away_score - home_score)
) %>%
  filter(!is.na(coach)) %>%
  group_by(coach) %>%
  summarise(mean_point_margin = mean(point_margin),
            win_rate = mean(point_margin > 0) + 0.5 * mean(point_margin == 0),
            .groups = "drop")

confound_check <- coach_career %>%
  filter(n_games >= MIN_GAMES_SPLIT) %>%
  inner_join(point_margin, by = "coach")

r_confound_margin  <- cor(confound_check$mean_residual, confound_check$mean_point_margin)
r_confound_winrate <- cor(confound_check$mean_residual, confound_check$win_rate)

cat(sprintf("\n=== confound check: is the adjustment residual just team quality? (min %d games) ===\n",
            MIN_GAMES_SPLIT))
cat(sprintf("coaches: %d\n", nrow(confound_check)))
cat(sprintf("cor(mean_residual, mean point margin): %.3f\n", r_confound_margin))
cat(sprintf("cor(mean_residual, win rate):          %.3f\n", r_confound_winrate))

# ---------------------------------------------------------------------------
# Macdonald / Carroll block (Seattle angle)
# ---------------------------------------------------------------------------

cat("\n=== Macdonald / Carroll block ===\n")
seattle_names <- c("Mike Macdonald", "Pete Carroll")
coach_career_ranked <- coach_career %>%
  arrange(desc(mean_residual)) %>%
  mutate(rank_all = row_number(), n_total_coaches = n())

for (nm in seattle_names) {
  row <- coach_career_ranked %>% filter(coach == nm)
  if (nrow(row) == 0) {
    cat(sprintf("%s: not found in coach table\n", nm))
    next
  }
  in_cone <- coach_cone %>% filter(coach == nm)
  cone_txt <- if (nrow(in_cone) == 1) {
    sprintf("band=%.4f outside=%s", in_cone$band_95, in_cone$outside_95)
  } else {
    sprintf("not shown on cone chart (fewer than %d games)", MIN_GAMES_CONE)
  }
  cat(sprintf("%-16s mean_residual=%+.4f  n_games=%d  (%d-%d)  rank=%d/%d  %s\n",
              nm, row$mean_residual, row$n_games, row$first_season, row$last_season,
              row$rank_all, row$n_total_coaches, cone_txt))
}

# ---------------------------------------------------------------------------
# Chart A: docs/figures/halftime_cone.png
# ---------------------------------------------------------------------------

x_seq <- seq(MIN_GAMES_CONE, max(coach_cone$n_games), length.out = 300)
cone_band <- tibble(n_games = x_seq,
                     upper = Z_95 * sigma_game / sqrt(x_seq),
                     lower = -Z_95 * sigma_game / sqrt(x_seq))

label_set <- coach_cone %>%
  filter(coach %in% FAMOUS_COACHES | outside_95) %>%
  distinct(coach, .keep_all = TRUE)

verdict_title <- if (abs(r_confound_margin) >= 0.6) {
  "Halftime 'adjusting' tracks team quality, not a separate skill"
} else if (n_cone_outside <= ceiling(expected_outside_by_chance * 1.5)) {
  "Halftime adjustments look like noise, not a coaching skill"
} else {
  "A few coaches beat the halftime noise cone, most don't"
}

p_cone <- ggplot(coach_cone, aes(n_games, mean_residual)) +
  geom_ribbon(data = cone_band, aes(x = n_games, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = ink_grid, alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(colour = outside_95), size = 2.1, alpha = 0.8) +
  geom_text_repel(data = label_set, aes(label = coach), size = 3.2,
                   colour = ink_title, fontface = "bold",
                   segment.colour = "grey70", segment.size = 0.3,
                   max.overlaps = 20, min.segment.length = 0.1) +
  scale_colour_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey55")) +
  scale_x_continuous(name = "Career regular-season games (as head coach, in this sample)") +
  scale_y_continuous(name = "Career mean adjustment residual (EPA/play)") +
  labs(title = verdict_title,
       subtitle = sprintf(
         "Shaded band = spread pure chance produces (95%%) given per-game residual SD = %.3f. %d of %d coaches fall outside it, vs %.1f expected by chance.",
         sigma_game, n_cone_outside, n_cone_total, expected_outside_by_chance),
       caption = fig_caption(
         "nflverse play-by-play 2000-2025 + games.csv",
         sprintf("Regular season, coaches with >= %d games.\nAdjustment residual: from league H2~H1+halftime-diff regression, positive = better than expected H2 EPA margin.",
                 MIN_GAMES_CONE),
         sprintf("That residual correlates r=%.2f with a coach's plain point margin, so most of\nwhat repeats here is team strength, not a distinct in-game skill.",
                 r_confound_margin))) +
  theme_coach(grid = "none")

save_fig("docs/figures/halftime_cone.png", p_cone)

# ---------------------------------------------------------------------------
# Chart B: docs/figures/halftime_persistence.png -- whichever view is clearer
# ---------------------------------------------------------------------------

use_yoy <- n_yoy >= n_split

if (use_yoy) {
  fit_line <- lm(mean_t1 ~ mean_t, data = yoy_pairs)
  p_persist <- ggplot(yoy_pairs, aes(mean_t, mean_t1)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.3) +
    geom_point(colour = "#0072B2", size = 2, alpha = 0.55) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey30", fill = "grey85", linewidth = 0.7) +
    scale_x_continuous(name = "Coach-season mean adjustment residual, season t") +
    scale_y_continuous(name = "Same coach, season t+1") +
    labs(title = "A coach's halftime-adjustment score barely carries into next season",
         subtitle = sprintf("r = %.3f across %d consecutive coach-season pairs (min %d games/season each)",
                             r_yoy, n_yoy, MIN_GAMES_SEASON),
         caption = fig_caption(
           "nflverse play-by-play 2000-2025 + games.csv",
           "Regular season. Adjustment residual from league H2~H1+halftime-diff\nregression, averaged per coach-season.",
           "If adjusting were a stable skill, points would hug the diagonal trend line tightly.")) +
    theme_coach(grid = "y")
} else {
  p_persist <- ggplot(split_half_wide, aes(mean_res_odd, mean_res_even)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.3) +
    geom_point(colour = "#0072B2", size = 2, alpha = 0.55) +
    geom_smooth(method = "lm", se = TRUE, colour = "grey30", fill = "grey85", linewidth = 0.7) +
    scale_x_continuous(name = "Mean adjustment residual, odd-numbered career games") +
    scale_y_continuous(name = "Same coach, even-numbered career games") +
    labs(title = "A coach's halftime-adjustment score barely repeats within his own career",
         subtitle = sprintf("r = %.3f across %d coaches (min %d career games)",
                             r_split, n_split, MIN_GAMES_SPLIT),
         caption = fig_caption(
           "nflverse play-by-play 2000-2025 + games.csv",
           "Regular season. Adjustment residual from league H2~H1+halftime-diff\nregression, split odd vs even games.",
           "If adjusting were a stable skill, points would hug the diagonal trend line tightly.")) +
    theme_coach(grid = "y")
}

save_fig("docs/figures/halftime_persistence.png", p_persist)

# ---------------------------------------------------------------------------
# Write derived data
# ---------------------------------------------------------------------------

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
write_csv(coach_career, "data/derived/halftime_coach.csv")
write_csv(coach_season, "data/derived/halftime_coach_season.csv")
write_csv(team_game, "data/derived/halftime_games.csv")
cat("\nwrote data/derived/halftime_coach.csv\n")
cat("wrote data/derived/halftime_coach_season.csv\n")
cat("wrote data/derived/halftime_games.csv\n")

cat("\ndone.\n")
