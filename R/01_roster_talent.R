# =============================================================================
# 01_roster_talent.R
#
# How much do NFL head coaches over- or under-perform the talent on their
# roster? Built to answer a video collaborator's question about the 2025
# Seahawks: they won "without an elite QB and seemingly without a lot of
# elite players," and separately, the idea of "WAR based on Madden ratings
# or something...like how much is a team overachieving based on their
# player talent." This script builds the roster-talent half of that answer
# using draft capital as the talent measure (a Madden-ratings version is a
# separate script). It fits a mixed model of team point differential on
# roster draft value and QB value, with a random intercept per head coach.
# That intercept, converted to wins, is the coach's over/under-performance
# number, and the Seattle chart shows where 2025 actually landed.
#
# Sources:
#   - ~/stranger9977/nfl-analysis/data/games.csv (local, preferred over
#     network): nflverse schedules/scores/coaches, 1999-2025.
#   - nflreadr::load_rosters(2003:2025): season rosters, draft_number per
#     player-season (network).
#   - nflreadr::load_players(): master player table, used only to backfill
#     draft position where load_rosters' own draft_number is missing (see
#     "known data issues" below).
#   - nflreadr::load_player_stats(2002:2025): per-game passing lines, used
#     to find each team-season's primary starting QB and his prior-season
#     efficiency.
#   - Draft pick value, three charts, all from the same source file: Lee
#     Sharpe's (nflverse co-founder) nfldata repo, data/draft_values.csv
#     (https://github.com/nflverse/nfldata). The Jimmy Johnson column was
#     already saved locally at data/raw/jj_draft_values.csv; the full file
#     (all three charts) is saved at data/raw/nfldata_draft_values.csv.
#       - "johnson": the original Jimmy Johnson trade chart. Values only
#         ever went through pick 224 (built before compensatory picks
#         existed); picks 225+ are valued at 0, same as the original chart.
#       - "stuart": Chase Stuart's Football Perspective chart, fit to
#         Approximate Value (AV) actually produced in a drafted player's
#         first 5 seasons, credited above a 2-AV/year replacement level.
#         https://www.footballperspective.com/draft-value-chart/ . Also
#         floors at 0 by pick 225.
#       - "otc": the Fitzgerald-Spielberger chart (Jason Fitzgerald and
#         Brad Spielberger, Over The Cap), fit to the actual second-contract
#         APY earned by picks from the 2011-2015 draft classes, smoothed
#         across neighboring picks. https://overthecap.com/draft-trade-value-chart
#         Stays positive through pick 262 (no hard floor at 0).
#   - Madden 26 (2025 season only): data/derived/madden_team_seasons.csv,
#     already built by R/03_madden.R (top-25 mean OVR, z-scored within
#     season; see that script for how it was sourced/validated). Not
#     rebuilt here.
#
# Known data issues found and handled while building this (see comments at
# each step): load_rosters()'s game_type field is unreliable for filtering
# to "regular season" roster rows (it mislabels most of a team's season as
# non-REG in some years), so the script uses every distinct player on a
# team's season roster regardless of week. load_rosters()'s own draft_number
# field has a materially higher missing-data rate for 2016-2019 specifically
# (draft position is largely unrecorded for around 45-72% of that era's
# rostered players, vs. ~29-34% before and after); it is backfilled from
# load_players()$draft_pick where possible, which closes most but not all of
# that gap. games.csv's/nflreadr's home_qb_name/away_qb_name fields were
# tested and rejected for identifying starters: they contain a confirmed
# data bug (Philip Rivers, retired since 2020, listed as Indianapolis's
# starter for 3 games in the 2025 season). Dropback counts from
# load_player_stats do not have that problem and are used instead.
#
# Out:
#   - data/derived/team_talent.csv     (team-season roster value + performance,
#                                        now under all three draft charts)
#   - data/derived/coach_vs_roster.csv (coach random effect, wins/season)
#   - docs/figures/coach_vs_roster.png (coach leaderboard)
#   - docs/figures/seahawks_2025.png   (2025 talent vs. performance, 2x2:
#                                        Jimmy Johnson / Stuart / OTC / Madden 26,
#                                        NFL team logos in place of points)
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(nflreadr)
  library(lme4)
  library(ggplot2)
  library(nflplotR)
})

source("R/lib/theme_coach.R")

GAMES_CSV <- "~/stranger9977/nfl-analysis/data/games.csv"
FIRST_SEASON <- 2003
LAST_SEASON  <- 2025
MIN_SEASONS_COACHED <- 4

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------
# 1. Team-season performance: wins (ties = 0.5), point differential/game,
#    and the head coach of record. A team-season's coach is whoever coached
#    the most games that season, which folds in-season firings into
#    whoever had the plurality; games.csv has 38 team-seasons in this
#    window with more than one coach, and this is the simplification used
#    for all of them (see the printed count below and report it as a
#    caveat).
# -----------------------------------------------------------------------
games_raw <- read.csv(path.expand(GAMES_CSV), stringsAsFactors = FALSE)

games <- games_raw %>%
  filter(game_type == "REG", season >= FIRST_SEASON, season <= LAST_SEASON) %>%
  mutate(home_team = clean_team_abbrs(home_team),
         away_team = clean_team_abbrs(away_team))

stopifnot(!anyNA(games$result), !anyNA(games$home_coach), !anyNA(games$away_coach))

team_games <- bind_rows(
  games %>% transmute(season, team = home_team, coach = home_coach,
                       point_diff = result,
                       win = case_when(result > 0 ~ 1, result < 0 ~ 0, TRUE ~ 0.5)),
  games %>% transmute(season, team = away_team, coach = away_coach,
                       point_diff = -result,
                       win = case_when(result < 0 ~ 1, result > 0 ~ 0, TRUE ~ 0.5))
)

team_season <- team_games %>%
  group_by(season, team) %>%
  summarise(games = n(), wins = sum(win), point_diff_pg = mean(point_diff), .groups = "drop")

coach_game_counts <- team_games %>% count(season, team, coach, name = "g_coached")
n_multi_coach_seasons <- coach_game_counts %>% count(season, team) %>% filter(n > 1) %>% nrow()

head_coach_of_record <- coach_game_counts %>%
  group_by(season, team) %>%
  slice_max(g_coached, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(season, team, head_coach = coach, g_coached)

team_season <- team_season %>% left_join(head_coach_of_record, by = c("season", "team"))

# Empirical wins-per-point-differential slope: converts the coach random
# effect (points/game) into wins/season later on.
wins_slope_fit <- lm(wins ~ point_diff_pg, data = team_season)
wins_per_point_diff <- unname(coef(wins_slope_fit)["point_diff_pg"])

# -----------------------------------------------------------------------
# 2. Roster draft capital.
#    load_rosters()'s "game_type" field turns out not to reliably mark
#    regular-season rows (e.g. the 2025 Seahawks roster has only 8 rows
#    tagged REG and 80 tagged SB, despite Seattle not playing in a Super
#    Bowl that season) so it is NOT used as a filter here; every distinct
#    player who ever appears on a team's season roster counts, any week.
#    draft_number is backfilled from load_players()$draft_pick where
#    load_rosters' own field is missing (rosters' own value wins on any
#    disagreement between the two, since spot checks against known picks
#    favored it).
# -----------------------------------------------------------------------
jj_chart <- read.csv("data/raw/jj_draft_values.csv", stringsAsFactors = FALSE)
draft_value <- function(pick) {
  v <- jj_chart$johnson[match(pick, jj_chart$pick)]
  ifelse(is.na(pick) | is.na(v), 0, v)
}

# Two more charts (see header comment for sourcing), same file family as the
# Jimmy Johnson chart above, used for the multi-chart comparison in Chart B
# and the model robustness check below. Primary model input stays JJ (via
# draft_value(), above); these two are never fed to the primary lmer model.
alt_charts <- read.csv("data/raw/nfldata_draft_values.csv", stringsAsFactors = FALSE)
stopifnot(all(alt_charts$johnson == jj_chart$johnson[match(alt_charts$pick, jj_chart$pick)]))

draft_value_stuart <- function(pick) {
  v <- alt_charts$stuart[match(pick, alt_charts$pick)]
  ifelse(is.na(pick) | is.na(v), 0, v)
}
draft_value_otc <- function(pick) {
  v <- alt_charts$otc[match(pick, alt_charts$pick)]
  ifelse(is.na(pick) | is.na(v), 0, v)
}

players_draft <- load_players() %>%
  filter(!is.na(gsis_id)) %>%
  distinct(gsis_id, .keep_all = TRUE) %>%
  select(gsis_id, draft_pick)

rosters <- load_rosters(FIRST_SEASON:LAST_SEASON) %>%
  mutate(team = clean_team_abbrs(team)) %>%
  distinct(season, team, gsis_id, .keep_all = TRUE) %>%
  left_join(players_draft, by = "gsis_id") %>%
  mutate(draft_number_final = ifelse(is.na(draft_number), draft_pick, draft_number),
         pick_value = draft_value(draft_number_final),
         pick_value_stuart = draft_value_stuart(draft_number_final),
         pick_value_otc = draft_value_otc(draft_number_final))

roster_value <- rosters %>%
  group_by(season, team) %>%
  summarise(
    roster_value = sum(pick_value),
    roster_value_stuart = sum(pick_value_stuart),
    roster_value_otc = sum(pick_value_otc),
    roster_value_recent4 = sum(pick_value[!is.na(entry_year) & entry_year > season - 4]),
    n_players = n(),
    .groups = "drop"
  )

# One row per player, for QB lookups later (avoids a many-to-many join if a
# player's draft_number was recorded inconsistently across season snapshots).
player_draft_lookup <- rosters %>%
  group_by(gsis_id) %>%
  summarise(draft_number_final = dplyr::first(na.omit(draft_number_final)), .groups = "drop")

# -----------------------------------------------------------------------
# 3. Primary starting QB per team-season, and his value.
#    Starter = most dropbacks (pass attempts + sacks taken) that season,
#    summed across any teams he played for. See header comment for why
#    games.csv's home_qb_name/away_qb_name fields were rejected in favor
#    of this.
# -----------------------------------------------------------------------
qb_stats_raw <- load_player_stats(seq(FIRST_SEASON - 1, LAST_SEASON)) %>%
  filter(season_type == "REG", position == "QB") %>%
  mutate(team = clean_team_abbrs(team), dropbacks = attempts + sacks_suffered)

# NOTE: group by player_id ONLY, not player_id + player_name. The same
# gsis_id occasionally carries two different player_name spellings within
# one season in this table (a known nflverse data glitch: e.g. gsis_id
# 00-0022942, the real Philip Rivers who retired after 2020, has some 2025
# Indianapolis QB dropbacks misattributed to his ID -- see header comment).
# Grouping on player_name too would silently split one player's season into
# two rows and duplicate downstream joins.
qb_names <- load_players() %>% filter(!is.na(gsis_id)) %>% distinct(gsis_id, .keep_all = TRUE) %>%
  select(gsis_id, display_name)

qb_player_season <- qb_stats_raw %>%
  group_by(season, player_id) %>%
  summarise(dropbacks = sum(dropbacks), passing_epa = sum(passing_epa, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(epa_per_dropback = passing_epa / dropbacks)

starters <- qb_stats_raw %>%
  group_by(season, team, player_id) %>%
  summarise(dropbacks = sum(dropbacks), .groups = "drop") %>%
  group_by(season, team) %>%
  slice_max(dropbacks, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(season >= FIRST_SEASON)

prior_epa <- qb_player_season %>%
  transmute(season = season + 1, player_id, qb_prior_epa_per_db = epa_per_dropback)

starters <- starters %>%
  left_join(prior_epa, by = c("season", "player_id")) %>%
  left_join(player_draft_lookup, by = c("player_id" = "gsis_id")) %>%
  left_join(qb_names, by = c("player_id" = "gsis_id")) %>%
  mutate(qb_value = draft_value(draft_number_final)) %>%
  select(season, team, qb_name = display_name, qb_id = player_id, qb_dropbacks = dropbacks,
         qb_draft_pick = draft_number_final, qb_value, qb_prior_epa_per_db)

n_missing_prior_epa <- sum(is.na(starters$qb_prior_epa_per_db))
pct_missing_prior_epa <- n_missing_prior_epa / nrow(starters)

# -----------------------------------------------------------------------
# 4. Assemble team-season talent table.
# -----------------------------------------------------------------------
team_talent <- team_season %>%
  left_join(roster_value, by = c("season", "team")) %>%
  left_join(starters, by = c("season", "team"))

stopifnot(nrow(team_talent) == nrow(team_season), !anyNA(team_talent$roster_value))

write.csv(team_talent, "data/derived/team_talent.csv", row.names = FALSE)

# -----------------------------------------------------------------------
# 5. Model. QB measure = QB's own draft value (primary), so it stays on the
#    same footing as roster value and no team-season is dropped for a
#    missing covariate. Previous-season QB EPA/dropback is fit as a
#    robustness check on the complete-case subset (see printed output for
#    how much it changes the coach ranking).
# -----------------------------------------------------------------------
model_data <- team_talent %>%
  mutate(roster_z = as.numeric(scale(roster_value)),
         qb_z = as.numeric(scale(qb_value)))

fit_primary <- lmer(point_diff_pg ~ roster_z + qb_z + (1 | head_coach) + (1 | season),
                     data = model_data)

talent_perf_cor <- cor(model_data$roster_value, model_data$point_diff_pg)

# Robustness: swap QB draft value for QB prior-season EPA/dropback on the
# subset where it's available, and check how much the coach ranking moves.
robust_data <- model_data %>%
  filter(!is.na(qb_prior_epa_per_db)) %>%
  mutate(qb_prior_z = as.numeric(scale(qb_prior_epa_per_db)))

fit_robust <- lmer(point_diff_pg ~ roster_z + qb_prior_z + (1 | head_coach) + (1 | season),
                    data = robust_data)

coach_ranef_primary <- ranef(fit_primary)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>%
  rename(effect_primary = `(Intercept)`)
coach_ranef_robust <- ranef(fit_robust)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>%
  rename(effect_robust = `(Intercept)`)
robust_compare <- inner_join(coach_ranef_primary, coach_ranef_robust, by = "head_coach")
robust_cor <- cor(robust_compare$effect_primary, robust_compare$effect_robust)

# Robustness: recent-draft-capital variant of roster value in place of
# career roster value.
model_data_recent <- model_data %>% mutate(roster_recent_z = as.numeric(scale(roster_value_recent4)))
fit_recent <- lmer(point_diff_pg ~ roster_recent_z + qb_z + (1 | head_coach) + (1 | season),
                    data = model_data_recent)
coach_ranef_recent <- ranef(fit_recent)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>%
  rename(effect_recent = `(Intercept)`)
recent_compare <- inner_join(coach_ranef_primary, coach_ranef_recent, by = "head_coach")
recent_cor <- cor(recent_compare$effect_primary, recent_compare$effect_recent)

# Robustness: Chase Stuart AV-based chart in place of Jimmy Johnson for
# roster value (QB value untouched, still his JJ draft value, so only the
# roster measure changes).
model_data_stuart <- model_data %>% mutate(roster_stuart_z = as.numeric(scale(roster_value_stuart)))
fit_stuart <- lmer(point_diff_pg ~ roster_stuart_z + qb_z + (1 | head_coach) + (1 | season),
                    data = model_data_stuart)
coach_ranef_stuart <- ranef(fit_stuart)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>%
  rename(effect_stuart = `(Intercept)`)
stuart_compare <- inner_join(coach_ranef_primary, coach_ranef_stuart, by = "head_coach")
stuart_cor <- cor(stuart_compare$effect_primary, stuart_compare$effect_stuart)

# -----------------------------------------------------------------------
# 6. Coach leaderboard table.
# -----------------------------------------------------------------------
coach_seasons <- model_data %>% count(head_coach, name = "seasons")
coaches_2025 <- model_data %>% filter(season == 2025) %>% pull(head_coach) %>% unique()

coach_table <- coach_ranef_primary %>%
  left_join(coach_seasons, by = "head_coach") %>%
  mutate(
    wins_per_season = effect_primary * wins_per_point_diff,
    active_2025 = head_coach %in% coaches_2025
  ) %>%
  rename(point_diff_effect = effect_primary) %>%
  arrange(desc(wins_per_season))

write.csv(coach_table, "data/derived/coach_vs_roster.csv", row.names = FALSE)

leaderboard_pool <- coach_table %>% filter(seasons >= MIN_SEASONS_COACHED)

# -----------------------------------------------------------------------
# 7. Chart A: coach leaderboard.
# -----------------------------------------------------------------------
top_n <- 12
board <- bind_rows(
  leaderboard_pool %>% slice_max(wins_per_season, n = top_n) %>% mutate(grp = "Outperform roster"),
  leaderboard_pool %>% slice_min(wins_per_season, n = top_n) %>% mutate(grp = "Underperform roster")
) %>%
  mutate(head_coach = factor(head_coach, levels = rev(unique(head_coach[order(wins_per_season)]))))

p_board <- ggplot(board, aes(x = wins_per_season, y = head_coach, fill = active_2025)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = "#045A8D", `FALSE` = "grey65")) +
  labs(
    title = paste0("Coaches who consistently beat their roster's draft capital, and coaches who don't"),
    subtitle = sprintf("Coach random effect from a mixed model of point differential on roster and QB draft value, %d-%d. Min %d seasons. Blue = coaching in 2025.",
                        FIRST_SEASON, LAST_SEASON, MIN_SEASONS_COACHED),
    x = "Wins per season above what roster talent predicts",
    y = NULL
  ) +
  theme_coach(grid = "y") +
  theme(axis.text.y = element_text(size = rel(0.85))) +
  labs(caption = paste(strwrap(fig_caption(
    "games.csv (nflverse schedules) and nflreadr::load_rosters/load_players/load_player_stats, 2003-2025",
    sprintf("Head coaches with >= %d seasons in the window (n=%d of %d coaches). Coach assigned to whoever coached the most games in a season; %d team-seasons had more than one coach.",
            MIN_SEASONS_COACHED, nrow(leaderboard_pool), nrow(coach_table), n_multi_coach_seasons),
    "Roster and QB draft value from the Jimmy Johnson trade chart; QB measure is his own draft value, not in-season play."
  ), width = 130), collapse = "\n"))

save_fig("docs/figures/coach_vs_roster.png", p_board, w = 12, h = 8.5)

# -----------------------------------------------------------------------
# 8. Chart B: 2025 talent vs. performance, four meters, Seattle highlighted.
#    Kept: the original single-chart (Jimmy Johnson) fit, used only for the
#    SEATTLE 2025 print block below, unchanged from before.
# -----------------------------------------------------------------------
data_2025 <- model_data %>% filter(season == 2025)
fit_2025 <- lm(point_diff_pg ~ roster_value, data = data_2025)
sea_resid <- residuals(fit_2025)[data_2025$team == "SEA"]
sea_rank_resid <- rank(-residuals(fit_2025))[data_2025$team == "SEA"]
sea_row <- data_2025 %>% filter(team == "SEA")

# Madden 26 launch talent, 2025 only, read from R/03's output (not rebuilt).
madden_2025 <- read.csv("data/derived/madden_team_seasons.csv", stringsAsFactors = FALSE) %>%
  filter(season == 2025) %>%
  select(team, madden_talent_z = talent_z, madden_top25_ovr = top25_ovr)

data_2025_full <- data_2025 %>% left_join(madden_2025, by = "team")
stopifnot(nrow(data_2025_full) == nrow(data_2025), !anyNA(data_2025_full$madden_talent_z))

meter_levels <- c("Jimmy Johnson", "Chase Stuart (AV)", "Fitzgerald-Spielberger (OTC)", "Madden 26 launch")

panel_data <- bind_rows(
  data_2025_full %>% transmute(team, meter = meter_levels[1], raw_value = roster_value, point_diff_pg),
  data_2025_full %>% transmute(team, meter = meter_levels[2], raw_value = roster_value_stuart, point_diff_pg),
  data_2025_full %>% transmute(team, meter = meter_levels[3], raw_value = roster_value_otc, point_diff_pg),
  data_2025_full %>% transmute(team, meter = meter_levels[4], raw_value = madden_talent_z, point_diff_pg)
) %>%
  mutate(meter = factor(meter, levels = meter_levels)) %>%
  group_by(meter) %>%
  mutate(x_z = as.numeric(scale(raw_value))) %>%
  ungroup()

# Per-meter 2025 stats: correlation (panel annotation + title logic), and
# Seattle's rank (1 = most talent by that meter).
panel_stats <- panel_data %>%
  group_by(meter) %>%
  summarise(r = cor(raw_value, point_diff_pg),
            sea_rank = rank(-raw_value, ties.method = "min")[team == "SEA"],
            sea_resid_sign = sign(residuals(lm(point_diff_pg ~ raw_value))[team == "SEA"]),
            .groups = "drop") %>%
  mutate(label = sprintf("r = %.2f", r))

full_cor_jj     <- talent_perf_cor
full_cor_stuart <- cor(model_data$roster_value_stuart, model_data$point_diff_pg)
full_cor_otc    <- cor(model_data$roster_value_otc, model_data$point_diff_pg)

n_over <- sum(panel_stats$sea_resid_sign > 0)
panel_title <- if (n_over == 4) {
  "Seattle's 2025 point differential beat every talent meter's prediction, draft capital and Madden alike"
} else if (n_over == 0) {
  "Seattle's 2025 point differential fell short of every talent meter's prediction, draft capital and Madden alike"
} else {
  sprintf("Seattle's 2025 point differential beat %d of 4 talent meters' predictions and missed %d",
          n_over, 4 - n_over)
}

p_2025 <- ggplot(panel_data, aes(x = x_z, y = point_diff_pg)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey55", linewidth = 0.6, linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = team, alpha = team == "SEA",
                      width = ifelse(team == "SEA", 0.075, 0.05))) +
  geom_point(data = panel_data %>% filter(team == "SEA"), shape = 21, size = 7,
             stroke = 1.1, colour = ink_title, fill = NA) +
  geom_text(data = panel_stats, aes(x = -Inf, y = Inf, label = label), inherit.aes = FALSE,
            hjust = -0.15, vjust = 1.5, size = 3.5, fontface = "bold", colour = ink_body) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
  facet_wrap(~ meter, ncol = 2) +
  labs(
    title = panel_title,
    subtitle = "Point differential per game vs. four measures of roster talent, all 32 teams, 2025 regular season.\nEach meter z-scored within 2025 so panels are on the same scale. Dashed line is that panel's league-wide fit;\nSeattle is the full-opacity, ringed logo in each panel.",
    x = "talent meter, z-scored within 2025 (higher = more/better roster talent)",
    y = "Point differential per game"
  ) +
  theme_coach(grid = "y") +
  labs(caption = paste(strwrap(fig_caption(
    "games.csv (nflverse schedules), nflreadr::load_rosters/load_players (2025), data/derived/madden_team_seasons.csv (R/03)",
    "32 teams, 2025 regular season.",
    "Draft charts: Jimmy Johnson (trade chart), Chase Stuart (AV-based, Football Perspective), Fitzgerald-Spielberger (contract-based, Over The Cap). Madden 26 = launch top-25 mean OVR."
  ), width = 150), collapse = "\n"))

save_fig("docs/figures/seahawks_2025.png", p_2025, w = 12, h = 10)

# -----------------------------------------------------------------------
# 9. Print headline numbers.
# -----------------------------------------------------------------------
cat("\n================ MODEL: point_diff_pg ~ roster_z + qb_z + (1|head_coach) + (1|season) ================\n")
print(summary(fit_primary))

cat(sprintf("\nWins-per-point-differential slope (lm(wins ~ point_diff_pg)): %.4f wins per point/game\n",
            wins_per_point_diff))

cat(sprintf("\nCorrelation, roster draft value vs. point differential per game (all team-seasons, %d-%d): r = %.3f (r^2 = %.3f)\n",
            FIRST_SEASON, LAST_SEASON, talent_perf_cor, talent_perf_cor^2))
if (abs(talent_perf_cor) < 0.4) {
  cat("That's a weak relationship: draft capital alone leaves most of the variation in team point differential unexplained.\n")
}

cat(sprintf("\nQB prior-season EPA/dropback missing for %d of %d team-season starters (%.1f%%). Primary model uses QB draft value instead; robustness model below uses prior EPA on the complete-case subset (n=%d).\n",
            n_missing_prior_epa, nrow(starters), 100 * pct_missing_prior_epa, nrow(robust_data)))
cat(sprintf("Robustness check, QB draft value vs. QB prior-season EPA/dropback: correlation between the two models' coach effects (n=%d shared coaches) = %.3f\n",
            nrow(robust_compare), robust_cor))
cat(sprintf("Robustness check, career vs. recent(4-draft) roster value: correlation between the two models' coach effects (n=%d shared coaches) = %.3f\n",
            nrow(recent_compare), recent_cor))
cat(sprintf("Robustness check, Jimmy Johnson vs. Chase Stuart roster value: correlation between the two models' coach effects (n=%d shared coaches) = %.3f\n",
            nrow(stuart_compare), stuart_cor))

cat("\n================ MULTI-CHART TALENT COMPARISON (2025 Chart B) ================\n")
cat(sprintf("Full-panel (%d-%d) correlation, roster value vs. point differential per game:\n", FIRST_SEASON, LAST_SEASON))
cat(sprintf("  Jimmy Johnson:                 r = %.3f\n", full_cor_jj))
cat(sprintf("  Chase Stuart (AV):             r = %.3f\n", full_cor_stuart))
cat(sprintf("  Fitzgerald-Spielberger (OTC):  r = %.3f\n", full_cor_otc))
cat("  Madden 26: full-panel r not recomputed here; see R/03_madden.R's own print (different seasons/window).\n")
cat("\n2025-only correlation and Seattle's rank (1 = most talent) under each meter:\n")
print(as.data.frame(panel_stats %>% select(meter, r_2025 = r, sea_rank)), row.names = FALSE)

cat("\n================ TOP 10 COACHES: wins/season above roster talent (min ", MIN_SEASONS_COACHED, " seasons) ================\n", sep = "")
print(as.data.frame(coach_table %>% filter(seasons >= MIN_SEASONS_COACHED) %>% slice_max(wins_per_season, n = 10) %>%
        select(head_coach, seasons, point_diff_effect, wins_per_season, active_2025)))

cat("\n================ BOTTOM 10 COACHES: wins/season above roster talent (min ", MIN_SEASONS_COACHED, " seasons) ================\n", sep = "")
print(as.data.frame(coach_table %>% filter(seasons >= MIN_SEASONS_COACHED) %>% slice_min(wins_per_season, n = 10) %>%
        select(head_coach, seasons, point_diff_effect, wins_per_season, active_2025)))

cat("\n================ SEATTLE 2025 ================\n")
sea_talent_rank <- rank(-data_2025$roster_value)[data_2025$team == "SEA"]
mac_effect <- coach_table %>% filter(head_coach == "Mike Macdonald")
cat(sprintf("Wins: %.1f | Point differential per game: %.2f\n", sea_row$wins, sea_row$point_diff_pg))
cat(sprintf("Roster draft value: %.0f (rank %d of 32 in the NFL)\n", sea_row$roster_value, sea_talent_rank))
cat(sprintf("QB: %s, drafted pick %s, draft value %.0f\n", sea_row$qb_name, as.character(sea_row$qb_draft_pick), sea_row$qb_value))
cat(sprintf("2025 residual vs. league-wide roster-value fit: %.2f points/game (rank %d of 32, positive = overperformed the fit)\n",
            sea_resid, sea_rank_resid))
if (nrow(mac_effect) == 1) {
  cat(sprintf("Mike Macdonald's coach effect (all seasons, %d coached): %.3f points/game -> %.2f wins/season above roster talent (rank %d of %d coaches modeled)\n",
              mac_effect$seasons, mac_effect$point_diff_effect, mac_effect$wins_per_season,
              which(coach_table$head_coach == "Mike Macdonald"), nrow(coach_table)))
} else {
  cat("Mike Macdonald not found in coach_table (check name match).\n")
}

cat("\nWrote data/derived/team_talent.csv, data/derived/coach_vs_roster.csv,\n")
cat("docs/figures/coach_vs_roster.png, docs/figures/seahawks_2025.png\n")
