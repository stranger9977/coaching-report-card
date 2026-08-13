# =============================================================================
# 02_market_expectation.R
#
# Purpose: does beating the closing point spread persist for NFL head
# coaches, or is it luck? The spread already prices in roster talent,
# scheme, injuries, everything the market can see -- including the coach.
# So a genuinely great coach facing a fair market should still land close
# to zero wins above expectation over a career; the market has already
# given him credit. This script checks two things: (1) which coaches sit
# outside the range pure luck would produce across a full career, and
# (2) whether beating the spread one season predicts beating it again the
# next season, or whether it looks like noise.
#
# Answers the collaborator's questions:
#   "Coaching is a tough one, like how do teams grade that?"
#   "how much is a team overachieving based on their player talent"
#   (the spread already bakes in roster talent, so wins above the
#   market-implied win probability isolates what's left: coaching, luck,
#   or both, and the persistence test tells them apart)
#
# Source: ~/stranger9977/nfl-analysis/data/games.csv (nflverse schedules,
#   seasons 1999-2026). Only games with a known result AND a closing
#   spread are used. Regular season only (game_type == "REG"); playoffs
#   are excluded because the sample per coach is tiny and single-elimination
#   pressure is a different environment than an 17-game season -- mixing
#   the two would let a few playoff games swing a career number.
#
# Out:
#   docs/figures/market_funnel.png   - career wins-above-expectation funnel
#   docs/figures/market_2025.png     - 2025 actual vs. expected wins, 32 teams
#   data/derived/coach_market.csv    - one row per head coach, career totals
#   data/derived/market_seasons.csv  - one row per coach-season
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(ggrepel)
  library(nflplotR)
})

source("R/lib/theme_coach.R")

accent_outside <- "#B2182B"  # single accent: careers outside the luck cone

# -----------------------------------------------------------------------
# 1. Load games, restrict to the usable universe.
# -----------------------------------------------------------------------
games <- read_csv("~/stranger9977/nfl-analysis/data/games.csv", show_col_types = FALSE)

games <- games %>%
  filter(game_type == "REG", !is.na(result), !is.na(spread_line))

cat(sprintf("Usable REG games (result + spread_line known): %d (seasons %d-%d)\n",
            nrow(games), min(games$season), max(games$season)))

# -----------------------------------------------------------------------
# 2. Self-calibrating market model: P(home wins) ~ spread_line.
#    Ties (result == 0, 15 games total) can't feed a binomial fit, so the
#    win-probability model is trained on decisive games only. Ties still
#    get an outcome later (actual = 0.5 for both sides) once we score them.
# -----------------------------------------------------------------------
decisive <- games %>% filter(result != 0) %>% mutate(home_win = as.integer(result > 0))

fit <- glm(home_win ~ spread_line, data = decisive, family = binomial)
coefs <- coef(fit)
cat(sprintf("\nLogistic market model: P(home win) ~ spread_line\n"))
cat(sprintf("  intercept = %.4f, spread_line coef = %.4f\n", coefs[1], coefs[2]))
cat(sprintf("  implied 50-50 line: spread_line = %.2f\n", -coefs[1] / coefs[2]))
cat(sprintf("  fit on %d decisive games (%d ties excluded)\n",
            nrow(decisive), sum(games$result == 0)))

games <- games %>%
  mutate(expected_home_wp = predict(fit, newdata = games, type = "response"))

# -----------------------------------------------------------------------
# 3. Coach-game long table: one row per side per game.
# -----------------------------------------------------------------------
home_rows <- games %>%
  transmute(game_id, season, week, team = home_team, coach = home_coach,
            opponent = away_team, is_home = TRUE,
            expected_wp = expected_home_wp,
            actual = case_when(result > 0 ~ 1, result < 0 ~ 0, TRUE ~ 0.5))

away_rows <- games %>%
  transmute(game_id, season, week, team = away_team, coach = away_coach,
            opponent = home_team, is_home = FALSE,
            expected_wp = 1 - expected_home_wp,
            actual = case_when(result < 0 ~ 1, result > 0 ~ 0, TRUE ~ 0.5))

coach_games <- bind_rows(home_rows, away_rows) %>%
  mutate(wae = actual - expected_wp,
         var_i = expected_wp * (1 - expected_wp))

# -----------------------------------------------------------------------
# 4. Career aggregation per head coach.
# -----------------------------------------------------------------------
coach_career <- coach_games %>%
  group_by(coach) %>%
  summarise(
    games = n(),
    first_season = min(season),
    last_season = max(season),
    actual_wins = sum(actual),
    expected_wins = sum(expected_wp),
    wins_above_expectation = sum(wae),
    career_var = sum(var_i),
    .groups = "drop"
  ) %>%
  mutate(
    rate_per17 = wins_above_expectation / games * 17,
    sd_wae = sqrt(career_var),
    sd_rate_per17 = sd_wae / games * 17,
    z = wins_above_expectation / sd_wae,
    outside_cone = abs(z) > 1.96
  ) %>%
  arrange(desc(wins_above_expectation))

min_games_career <- 16  # roughly one season; drops emergency/interim samples
coach_eval <- coach_career %>% filter(games >= min_games_career)

n_outside <- sum(coach_eval$outside_cone)
n_inside <- sum(!coach_eval$outside_cone)
cat(sprintf("\nCareer funnel: %d coaches evaluated (>= %d games).\n",
            nrow(coach_eval), min_games_career))
cat(sprintf("  outside the 95%% luck cone: %d\n", n_outside))
cat(sprintf("  inside the 95%% luck cone:  %d\n", n_inside))

cat("\nTop 10 careers by wins above expectation:\n")
top10 <- coach_eval %>% slice_max(wins_above_expectation, n = 10) %>%
  select(coach, games, first_season, last_season, wins_above_expectation, rate_per17, outside_cone)
print(top10, n = 10)

cat("\nBottom 10 careers by wins above expectation:\n")
bottom10 <- coach_eval %>% slice_min(wins_above_expectation, n = 10) %>%
  select(coach, games, first_season, last_season, wins_above_expectation, rate_per17, outside_cone)
print(bottom10, n = 10)

# -----------------------------------------------------------------------
# 5. Persistence: coach-season rate, correlate season t with t+1.
# -----------------------------------------------------------------------
coach_season <- coach_games %>%
  group_by(coach, season) %>%
  summarise(
    games = n(),
    actual_wins = sum(actual),
    expected_wins = sum(expected_wp),
    wins_above_expectation = sum(wae),
    rate_per17 = wins_above_expectation / games * 17,
    .groups = "drop"
  ) %>%
  arrange(coach, season)

min_games_season <- 8
season_eligible <- coach_season %>% filter(games >= min_games_season)

pairs <- season_eligible %>%
  transmute(coach, season, rate_t = rate_per17, games_t = games) %>%
  inner_join(
    season_eligible %>% transmute(coach, season = season - 1,
                                   rate_t1 = rate_per17, games_t1 = games),
    by = c("coach", "season")
  )

persist_cor <- cor.test(pairs$rate_t, pairs$rate_t1)
cat(sprintf("\nPersistence (season t vs season t+1, min %d games/season): n = %d pairs\n",
            min_games_season, nrow(pairs)))
cat(sprintf("  r = %.3f, 95%% CI [%.3f, %.3f], p = %.3f\n",
            persist_cor$estimate, persist_cor$conf.int[1], persist_cor$conf.int[2],
            persist_cor$p.value))

# -----------------------------------------------------------------------
# 6. Seattle / Macdonald block.
# -----------------------------------------------------------------------
team_2025 <- coach_games %>%
  filter(season == 2025) %>%
  group_by(team) %>%
  summarise(
    games = n(),
    actual_wins = sum(actual),
    expected_wins = sum(expected_wp),
    wins_above_expectation = sum(wae),
    .groups = "drop"
  )

coach_2025_label <- coach_games %>%
  filter(season == 2025) %>%
  count(team, coach) %>%
  group_by(team) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(team, coach)

team_2025 <- team_2025 %>% left_join(coach_2025_label, by = "team")

sea_2025 <- team_2025 %>% filter(team == "SEA")
macdonald_career <- coach_career %>% filter(coach == "Mike Macdonald")
macdonald_rank_wae <- which(coach_career$coach == "Mike Macdonald")
macdonald_rank_rate <- coach_eval %>% arrange(desc(rate_per17)) %>%
  mutate(rank = row_number()) %>% filter(coach == "Mike Macdonald") %>% pull(rank)

cat("\n--- SEATTLE BLOCK ---\n")
cat(sprintf("2025 Seahawks (REG): %.1f actual wins vs %.2f market-expected wins (WAE %+.2f)\n",
            sea_2025$actual_wins, sea_2025$expected_wins, sea_2025$wins_above_expectation))
if (nrow(macdonald_career) == 1) {
  cat(sprintf("Mike Macdonald career-to-date: %d games (%d-%d), WAE %+.2f, rate/17 %+.2f\n",
              macdonald_career$games, macdonald_career$first_season, macdonald_career$last_season,
              macdonald_career$wins_above_expectation, macdonald_career$rate_per17))
  cat(sprintf("  rank by career WAE: %d of %d coaches (all games)\n",
              macdonald_rank_wae, nrow(coach_career)))
  if (length(macdonald_rank_rate) == 1) {
    cat(sprintf("  rank by rate/17 among %d eligible (>= %d games): %d\n",
                nrow(coach_eval), min_games_career, macdonald_rank_rate))
  } else {
    cat(sprintf("  not in the >= %d game eligible pool yet (%d games)\n",
                min_games_career, macdonald_career$games))
  }
} else {
  cat("Mike Macdonald not found in coach_career table.\n")
}

# -----------------------------------------------------------------------
# 7. Chart A: career funnel.
# -----------------------------------------------------------------------
notable_coaches <- c("Andy Reid", "Bill Belichick", "Mike Tomlin", "Sean McVay",
                      "Mike Macdonald", "Pete Carroll")
notable_present <- intersect(notable_coaches, coach_eval$coach)

v_bar <- mean(coach_games$var_i)
games_seq <- seq(min_games_career, max(coach_eval$games), length.out = 200)
cone_df <- tibble(
  games = games_seq,
  se = sqrt(v_bar / games_seq) * 17,
  upper = 1.96 * se,
  lower = -1.96 * se
)

label_set <- coach_eval %>%
  filter(outside_cone | coach %in% notable_present)

funnel_title <- if (n_outside == 0) {
  "No coach's career record clears the range luck alone would produce"
} else {
  sprintf("%d of %d coaches beat (or lag) the market by more than luck should allow", n_outside, nrow(coach_eval))
}

p_funnel <- ggplot(coach_eval, aes(x = games, y = rate_per17)) +
  geom_ribbon(data = cone_df, aes(x = games, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
  geom_line(data = cone_df, aes(x = games, y = upper), inherit.aes = FALSE,
            colour = "grey70", linewidth = 0.3) +
  geom_line(data = cone_df, aes(x = games, y = lower), inherit.aes = FALSE,
            colour = "grey70", linewidth = 0.3) +
  geom_baseline(0) +
  geom_point(aes(colour = outside_cone), size = 1.8, alpha = 0.85) +
  geom_point(data = coach_eval %>% filter(coach %in% notable_present),
             shape = 21, size = 3.2, stroke = 0.9, colour = ink_title, fill = NA) +
  geom_text_repel(data = label_set, aes(label = coach),
                   size = 3.1, colour = ink_title, fontface = "bold",
                   segment.colour = "grey60", segment.size = 0.3,
                   max.overlaps = 30, box.padding = 0.4, min.segment.length = 0.1) +
  scale_colour_manual(values = c(`TRUE` = accent_outside, `FALSE` = "grey55")) +
  scale_x_continuous(name = "career games (regular season, >= 16 game minimum)") +
  scale_y_continuous(name = "wins above market expectation, per 17 games") +
  labs(
    title = funnel_title,
    subtitle = paste0("Each point is one head coach's full career. The shaded band is the range a coach\n",
                       "with zero true edge would land in from luck alone (95% interval). A great coach\n",
                       "facing a fair market can still sit near zero: the spread already prices him in."),
    caption = fig_caption(
      "nflverse schedules, 1999-2025 seasons",
      sprintf("%d REG-season head coaches with >= %d games. Circled names are active/notable coaches (may sit inside the cone).",
              nrow(coach_eval), min_games_career),
      "Market model: empirical logistic P(home win) ~ closing spread, fit on decisive games."
    )
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/market_funnel.png", p_funnel, w = 12, h = 7.5)

# -----------------------------------------------------------------------
# 8. Chart B: 2025 season, all 32 teams, actual vs expected wins.
# -----------------------------------------------------------------------
team_2025 <- team_2025 %>%
  mutate(diff = actual_wins - expected_wins,
         is_sea = team == "SEA")

sea_diff <- team_2025$diff[team_2025$team == "SEA"]
sea_dir <- if (sea_diff > 0.05) "outperformed" else if (sea_diff < -0.05) "underperformed" else "matched"
chart_b_title <- sprintf("Seattle %s its 2025 market expectation by %.1f win%s",
                          sea_dir, abs(sea_diff), if (abs(round(sea_diff,1)) == 1) "" else "s")

p_2025 <- ggplot(team_2025, aes(x = expected_wins, y = actual_wins)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.35) +
  geom_nfl_logos(aes(team_abbr = team, alpha = is_sea, width = ifelse(is_sea, 0.09, 0.06))) +
  geom_point(data = team_2025 %>% filter(is_sea), shape = 21, size = 8.5,
             stroke = 1.1, colour = ink_title, fill = NA) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
  scale_x_continuous(name = "market-expected wins (from closing spreads)") +
  scale_y_continuous(name = "actual wins") +
  coord_equal() +
  labs(
    title = chart_b_title,
    subtitle = paste0("2025 regular season, all 32 teams. Dashed line is exactly meeting the market.\n",
                       "Above the line means the team won more than the closing spread implied."),
    caption = fig_caption(
      "nflverse schedules, 2025 season",
      "All 32 teams, regular season only. Seattle is the full-opacity, ringed logo.",
      NULL
    )
  ) +
  theme_coach(grid = "none")

save_fig("docs/figures/market_2025.png", p_2025, w = 11, h = 9)

# -----------------------------------------------------------------------
# 9. Write derived CSVs.
# -----------------------------------------------------------------------
coach_market_out <- coach_career %>%
  mutate(eligible = games >= min_games_career) %>%
  select(coach, first_season, last_season, games, actual_wins, expected_wins,
         wins_above_expectation, rate_per17, sd_wae, sd_rate_per17, z,
         outside_cone, eligible)
write_csv(coach_market_out, "data/derived/coach_market.csv")
cat(sprintf("\nwrote data/derived/coach_market.csv (%d rows)\n", nrow(coach_market_out)))

market_seasons_out <- coach_season %>%
  mutate(eligible = games >= min_games_season)
write_csv(market_seasons_out, "data/derived/market_seasons.csv")
cat(sprintf("wrote data/derived/market_seasons.csv (%d rows)\n", nrow(market_seasons_out)))

cat("\nDone.\n")
