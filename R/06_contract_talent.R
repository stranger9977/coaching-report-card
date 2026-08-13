# =============================================================================
# 06_contract_talent.R
#
# Nick's ask, verbatim: "draft capital as a proxy for team talent is biased
# in a lot of ways and not really great. a better way would be to quantify
# team talent by looking at contract value that could be helpful." R/01 built
# the draft-capital version of the roster-talent question (three draft
# charts, all r < 0.03 against point differential); this script builds the
# same question answered with real NFL contracts instead: what a team
# actually chose to pay its players, which is the market's own aggregated
# judgment of talent (scouts, coaches, agents, the whole league) rather
# than a single draft slot assigned years earlier that ignores every
# trade, bust, breakout, and second contract since.
#
# Sources:
#   - ~/stranger9977/nfl-analysis/data/games.csv (same as R/01): nflverse
#     schedules/scores/coaches.
#   - nflreadr::load_rosters(): season rosters, gsis_id per player-season.
#     Same "don't filter on game_type" handling as R/01 (see its header;
#     load_rosters()'s game_type field does not reliably mark regular-season
#     rows), so every distinct player who ever appears on a team's season
#     roster counts, any week.
#   - nflreadr::load_contracts(): OverTheCap's full historical contract
#     database via nflverse. One row per SIGNED CONTRACT (not per player),
#     so most players have multiple rows across their career (rookie deal,
#     extension, second contract, etc.), each with year_signed, years, and
#     apy_cap_pct (that contract's average-per-year value as a share of the
#     salary cap IN THE YEAR IT WAS SIGNED -- this self-adjusts for cap
#     growth across the two-decade window, unlike a raw dollar APY would).
#   - nflreadr::load_player_stats(): reused, unchanged from R/01, only to
#     find each team-season's primary starting QB (most dropbacks that
#     season; see R/01's header for why games.csv's own QB fields were
#     rejected).
#
# Method:
#   1. For each contract row, it's "active" in a season if
#      year_signed <= season < year_signed + years. A player can have more
#      than one contract active-looking in edge cases (an extension signed
#      before the old deal's stated end year); most recent year_signed wins.
#      Joined to rosters primarily by gsis_id; ~8% of contract rows have no
#      gsis_id recorded by OTC/nflverse, so a fallback join on cleaned
#      player name + position group recovers some of those (see step 2,
#      match rates printed by season, both stages).
#   2. Player value = apy_cap_pct of his active contract that season. Team
#      talent = SUM of the top 25 players' apy_cap_pct on that team-season's
#      roster (same top-N logic as picking a 53-man-plus-practice-squad
#      "core"; matches R/01's roster_value being a sum, not a mean, so the
#      two are comparable in spirit). A MEAN-of-top-25 variant is fit as a
#      robustness check (see step 6). QB share = the primary starting QB's
#      own apy_cap_pct that season.
#   3. Window: 2012-2025, not R/01's full 2003-2025. Contract coverage was
#      measured season by season before picking this (see print block
#      "CONTRACT DATA COVERAGE"): the number of a team's OWN roster players
#      actually matched to a contract in a season doesn't clear 25 for
#      EVERY team until 2012 (2010-2011 have teams matching as few as 13-20
#      players, which would make a "top 25" sum silently a "top 13" sum and
#      quietly deflate that team-season's talent). From 2012 on, every
#      team-season matches at least 29 players, and primary-QB-starter
#      contract match rate is >= 90% in all but one season (2019, 90.6%
#      itself). Pre-2012 seasons are dropped entirely rather than patched.
#   4. Known bias, quantified rather than hidden (step 7 below): a rookie-
#      scale contract badly underpays a genuine star in his first four
#      years (a cheap, great, 22-year-old reads as low talent by
#      apy_cap_pct alone). Reported: team talent's correlation to point
#      differential with all players counted vs. with rookie-scale players
#      excluded from the top-25 pool, plus each team's "rookie share" of
#      its own top-25 sum correlated against how much it beat the model,
#      and Seattle's own rookie share and residual called out directly.
#
# Out:
#   - data/derived/contract_talent.csv   (team-season contract value +
#                                          performance, 2012-2025)
#   - data/derived/coach_vs_contracts.csv (coach random effect under the
#                                          contract-talent model, wins/season)
#   - docs/figures/contract_2025.png     (2025 talent vs. performance,
#                                          Seattle highlighted)
#   - docs/figures/coach_vs_contracts.png (coach leaderboard, contract model)
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
FIRST_SEASON <- 2012   # see header step 3 for why this is later than R/01's 2003
LAST_SEASON  <- 2025
MIN_SEASONS_COACHED <- 4
TOP_N <- 25

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------
# 1. Team-season performance: wins, point differential/game, head coach of
#    record. Identical logic to R/01 step 1, restricted to this script's
#    window.
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

wins_slope_fit <- lm(wins ~ point_diff_pg, data = team_season)
wins_per_point_diff <- unname(coef(wins_slope_fit)["point_diff_pg"])

# -----------------------------------------------------------------------
# 2. Contract-active lookup: one row per (gsis_id, season) = that player's
#    active contract that season, most recent year_signed wins.
# -----------------------------------------------------------------------
contracts <- load_contracts()

contracts_id <- contracts %>% filter(!is.na(gsis_id), year_signed > 0)

active_by_season <- function(df, key_cols) {
  lapply(FIRST_SEASON:LAST_SEASON, function(yr) {
    df %>%
      filter(year_signed <= yr, yr < year_signed + years) %>%
      group_by(across(all_of(key_cols))) %>%
      slice_max(year_signed, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(season = yr)
  }) %>% bind_rows()
}

contracts_active_id <- active_by_season(contracts_id, "gsis_id") %>%
  select(season, gsis_id, apy_cap_pct, draft_year, contract_player = player)

# Fallback source: contract rows with NO gsis_id recorded by OTC/nflverse
# (~8% of all contract rows). Keyed on cleaned name + coarse position group
# (contracts uses granular positions like LT/RG/C/ED/IDL/CB/S; rosters uses
# grouped OL/DL/DB) so the two tables' position fields actually line up.
normalize_name <- function(x) {
  x %>% toupper() %>%
    gsub("[.'’-]", "", .) %>%
    gsub("\\s+(JR|SR|II|III|IV|V)$", "", .) %>%
    trimws() %>%
    gsub("\\s+", " ", .)
}
pos_group <- function(p) case_when(
  p %in% c("C", "LG", "RG", "LT", "RT") ~ "OL",
  p %in% c("ED", "IDL")                 ~ "DL",
  p %in% c("CB", "S")                   ~ "DB",
  p == "FB"                             ~ "RB",
  TRUE                                  ~ p
)

contracts_noid <- contracts %>%
  filter(is.na(gsis_id), year_signed > 0) %>%
  mutate(name_norm = normalize_name(player), pos_grp = pos_group(position))

contracts_active_noid <- active_by_season(contracts_noid, c("name_norm", "pos_grp")) %>%
  select(season, name_norm, pos_grp, apy_cap_pct_noid = apy_cap_pct, draft_year_noid = draft_year) %>%
  # a handful of season/name/pos_grp keys collide across two real players
  # (rare shared names within the same position group); keep the higher
  # value as the tiebreak and accept the small resulting noise.
  group_by(season, name_norm, pos_grp) %>%
  slice_max(apy_cap_pct_noid, n = 1, with_ties = FALSE) %>%
  ungroup()

# -----------------------------------------------------------------------
# 3. Rosters, joined to contract value: primary join on gsis_id, fallback
#    join on cleaned name + position group for rows the primary join misses.
# -----------------------------------------------------------------------
rosters <- load_rosters(FIRST_SEASON:LAST_SEASON) %>%
  mutate(team = clean_team_abbrs(team)) %>%
  distinct(season, team, gsis_id, .keep_all = TRUE)

roster_contracts <- rosters %>%
  left_join(contracts_active_id, by = c("season", "gsis_id"))

match_rate_primary <- roster_contracts %>%
  group_by(season) %>%
  summarise(n = n(), matched = sum(!is.na(apy_cap_pct)), pct_primary = 100 * matched / n, .groups = "drop")

roster_contracts <- roster_contracts %>%
  mutate(name_norm = normalize_name(full_name), pos_grp = pos_group(position)) %>%
  left_join(contracts_active_noid, by = c("season", "name_norm", "pos_grp")) %>%
  mutate(
    apy_cap_pct = ifelse(is.na(apy_cap_pct), apy_cap_pct_noid, apy_cap_pct),
    draft_year  = ifelse(is.na(draft_year), draft_year_noid, draft_year),
    fallback_matched = is.na(contract_player) & !is.na(apy_cap_pct_noid)
  )

match_rate_final <- roster_contracts %>%
  group_by(season) %>%
  summarise(n = n(), matched = sum(!is.na(apy_cap_pct)), pct_final = 100 * matched / n,
            n_fallback = sum(fallback_matched, na.rm = TRUE), .groups = "drop")

match_rates <- match_rate_primary %>%
  select(season, n, pct_primary) %>%
  left_join(match_rate_final %>% select(season, pct_final, n_fallback), by = "season")

# rookie-scale flag: player's active contract was signed in his draft year
# (misses undrafted rookie deals, which have no draft_year to check against
# -- documented limitation, not fixed here). year_signed wasn't carried
# through the two active_by_season() calls above, so pull it in now.
contracts_active_id_ys <- active_by_season(contracts_id, "gsis_id") %>%
  select(season, gsis_id, year_signed_id = year_signed)
contracts_active_noid_ys <- active_by_season(contracts_noid, c("name_norm", "pos_grp")) %>%
  group_by(season, name_norm, pos_grp) %>% slice_max(year_signed, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(season, name_norm, pos_grp, year_signed_noid = year_signed)

roster_contracts <- roster_contracts %>%
  left_join(contracts_active_id_ys, by = c("season", "gsis_id")) %>%
  left_join(contracts_active_noid_ys, by = c("season", "name_norm", "pos_grp")) %>%
  mutate(
    year_signed_final = ifelse(!is.na(year_signed_id), year_signed_id, year_signed_noid),
    rookie_deal = !is.na(draft_year) & !is.na(year_signed_final) & (year_signed_final == draft_year)
  )

# -----------------------------------------------------------------------
# 4. Team talent: sum of top-25 apy_cap_pct (primary), mean of top-25
#    (robustness), and a rookie-deal-excluded variant (bias check).
# -----------------------------------------------------------------------
matched_players <- roster_contracts %>% filter(!is.na(apy_cap_pct))

n_matched_per_team <- matched_players %>% count(season, team, name = "n_matched")
stopifnot(min(n_matched_per_team$n_matched) >= TOP_N)  # window choice guarantees this

top25 <- matched_players %>%
  group_by(season, team) %>%
  slice_max(apy_cap_pct, n = TOP_N, with_ties = FALSE) %>%
  ungroup()

team_talent_contract <- top25 %>%
  group_by(season, team) %>%
  summarise(
    contract_talent = sum(apy_cap_pct),
    contract_talent_mean = mean(apy_cap_pct),
    rookie_share = sum(apy_cap_pct[rookie_deal]) / sum(apy_cap_pct),
    n_rookie_in_top25 = sum(rookie_deal),
    .groups = "drop"
  )

top25_norookie <- matched_players %>%
  filter(!rookie_deal) %>%
  group_by(season, team) %>%
  slice_max(apy_cap_pct, n = TOP_N, with_ties = FALSE) %>%
  ungroup() %>%
  group_by(season, team) %>%
  summarise(contract_talent_norookie = sum(apy_cap_pct), n_norookie_pool = n(), .groups = "drop")

team_talent_contract <- team_talent_contract %>% left_join(top25_norookie, by = c("season", "team"))

# -----------------------------------------------------------------------
# 5. Primary starting QB per team-season and his contract value. Same
#    dropback-based starter detection as R/01 step 3 (see its header for
#    why games.csv's home_qb_name/away_qb_name were rejected).
# -----------------------------------------------------------------------
qb_stats_raw <- load_player_stats(seq(FIRST_SEASON - 1, LAST_SEASON)) %>%
  filter(season_type == "REG", position == "QB") %>%
  mutate(team = clean_team_abbrs(team), dropbacks = attempts + sacks_suffered)

qb_names <- load_players() %>% filter(!is.na(gsis_id)) %>% distinct(gsis_id, .keep_all = TRUE) %>%
  select(gsis_id, display_name)

starters <- qb_stats_raw %>%
  group_by(season, team, player_id) %>%
  summarise(dropbacks = sum(dropbacks), .groups = "drop") %>%
  group_by(season, team) %>%
  slice_max(dropbacks, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(season >= FIRST_SEASON)

starters <- starters %>%
  left_join(contracts_active_id %>% select(season, gsis_id, qb_apy_cap_pct = apy_cap_pct),
            by = c("season", "player_id" = "gsis_id")) %>%
  left_join(qb_names, by = c("player_id" = "gsis_id")) %>%
  select(season, team, qb_name = display_name, qb_id = player_id, qb_dropbacks = dropbacks, qb_apy_cap_pct)

n_missing_qb_contract <- sum(is.na(starters$qb_apy_cap_pct))

# -----------------------------------------------------------------------
# 6. Assemble team-season table, model.
# -----------------------------------------------------------------------
team_talent <- team_season %>%
  left_join(team_talent_contract, by = c("season", "team")) %>%
  left_join(starters, by = c("season", "team"))

stopifnot(nrow(team_talent) == nrow(team_season), !anyNA(team_talent$contract_talent))

write.csv(team_talent, "data/derived/contract_talent.csv", row.names = FALSE)

model_data <- team_talent %>%
  filter(!is.na(qb_apy_cap_pct)) %>%
  mutate(contract_z = as.numeric(scale(contract_talent)),
         qb_contract_z = as.numeric(scale(qb_apy_cap_pct)))
n_dropped_missing_qb <- nrow(team_talent) - nrow(model_data)

fit_primary <- lmer(point_diff_pg ~ contract_z + qb_contract_z + (1 | head_coach) + (1 | season),
                     data = model_data)

talent_perf_cor <- cor(team_talent$contract_talent, team_talent$point_diff_pg)
cor_by_season <- team_talent %>% group_by(season) %>%
  summarise(r = cor(contract_talent, point_diff_pg), .groups = "drop")

cor_2025 <- cor(team_talent$contract_talent[team_talent$season == 2025],
                team_talent$point_diff_pg[team_talent$season == 2025])

# Robustness: mean-of-top-25 in place of sum-of-top-25.
model_data_mean <- model_data %>% mutate(contract_mean_z = as.numeric(scale(contract_talent_mean)))
fit_mean <- lmer(point_diff_pg ~ contract_mean_z + qb_contract_z + (1 | head_coach) + (1 | season),
                  data = model_data_mean)
coach_ranef_primary <- ranef(fit_primary)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>% rename(effect_primary = `(Intercept)`)
coach_ranef_mean <- ranef(fit_mean)$head_coach %>%
  tibble::rownames_to_column("head_coach") %>% rename(effect_mean = `(Intercept)`)
mean_compare <- inner_join(coach_ranef_primary, coach_ranef_mean, by = "head_coach")
mean_cor <- cor(mean_compare$effect_primary, mean_compare$effect_mean)

# Rookie-deal bias check: r with vs. without rookie-scale players in the
# top-25 pool, full window and 2025-only.
cor_norookie <- cor(team_talent$contract_talent_norookie, team_talent$point_diff_pg, use = "complete.obs")
cor_norookie_2025 <- cor(team_talent$contract_talent_norookie[team_talent$season == 2025],
                          team_talent$point_diff_pg[team_talent$season == 2025], use = "complete.obs")

# Does a team's rookie share predict beating the model (the bias, quantified)?
resid_data <- team_talent %>% mutate(resid = residuals(lm(point_diff_pg ~ contract_talent, data = team_talent)))
rookie_share_bias_cor <- cor(resid_data$rookie_share, resid_data$resid)

# -----------------------------------------------------------------------
# 7. Coach leaderboard table + comparison to R/01's Jimmy Johnson model.
# -----------------------------------------------------------------------
coach_seasons <- model_data %>% count(head_coach, name = "seasons")
coaches_2025 <- model_data %>% filter(season == 2025) %>% pull(head_coach) %>% unique()

coach_table <- coach_ranef_primary %>%
  left_join(coach_seasons, by = "head_coach") %>%
  mutate(wins_per_season = effect_primary * wins_per_point_diff,
         active_2025 = head_coach %in% coaches_2025) %>%
  rename(point_diff_effect = effect_primary) %>%
  arrange(desc(wins_per_season))

write.csv(coach_table, "data/derived/coach_vs_contracts.csv", row.names = FALSE)

leaderboard_pool <- coach_table %>% filter(seasons >= MIN_SEASONS_COACHED)

jj_coach_table <- read.csv("data/derived/coach_vs_roster.csv", stringsAsFactors = FALSE) %>%
  select(head_coach, jj_effect = point_diff_effect)
jj_compare <- inner_join(coach_table %>% select(head_coach, contract_effect = point_diff_effect),
                          jj_coach_table, by = "head_coach")
jj_vs_contract_cor <- cor(jj_compare$contract_effect, jj_compare$jj_effect)

# -----------------------------------------------------------------------
# 8. Chart A: coach leaderboard, contract-value model. Same visual spec as
#    R/01's coach_vs_roster.png.
# -----------------------------------------------------------------------
top_n_board <- 12
board <- bind_rows(
  leaderboard_pool %>% slice_max(wins_per_season, n = top_n_board) %>% mutate(grp = "Outperform contract talent"),
  leaderboard_pool %>% slice_min(wins_per_season, n = top_n_board) %>% mutate(grp = "Underperform contract talent")
) %>%
  mutate(head_coach = factor(head_coach, levels = rev(unique(head_coach[order(wins_per_season)]))))

p_board <- ggplot(board, aes(x = wins_per_season, y = head_coach, fill = active_2025)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = "#045A8D", `FALSE` = "grey65")) +
  labs(
    title = "Coaches who consistently beat their roster's contract value, and coaches who don't",
    subtitle = sprintf("Coach random effect from a mixed model of point differential on team and QB contract value (APY %% of cap), %d-%d. Min %d seasons. Blue = coaching in 2025.",
                        FIRST_SEASON, LAST_SEASON, MIN_SEASONS_COACHED),
    x = "Wins per season above what contract-paid talent predicts",
    y = NULL
  ) +
  theme_coach(grid = "y") +
  theme(axis.text.y = element_text(size = rel(0.85))) +
  labs(caption = paste(strwrap(fig_caption(
    "games.csv (nflverse schedules), nflreadr::load_rosters/load_contracts/load_player_stats, 2012-2025",
    sprintf("Head coaches with >= %d seasons in the window (n=%d of %d coaches). Coach assigned to whoever coached the most games in a season.",
            MIN_SEASONS_COACHED, nrow(leaderboard_pool), nrow(coach_table)),
    "Team talent = sum of top-25 roster players' contract APY as % of the cap in their signing year. QB measure is his own contract value, not in-season play."
  ), width = 130), collapse = "\n"))

save_fig("docs/figures/coach_vs_contracts.png", p_board, w = 12, h = 8.5)

# -----------------------------------------------------------------------
# 9. Chart B: 2025 contract talent vs. performance, Seattle highlighted.
# -----------------------------------------------------------------------
data_2025 <- team_talent %>% filter(season == 2025) %>%
  mutate(contract_z = as.numeric(scale(contract_talent)))
fit_2025 <- lm(point_diff_pg ~ contract_talent, data = data_2025)
sea_resid <- residuals(fit_2025)[data_2025$team == "SEA"]
sea_rank_resid <- rank(-residuals(fit_2025))[data_2025$team == "SEA"]
sea_row <- data_2025 %>% filter(team == "SEA")
sea_talent_rank <- rank(-data_2025$contract_talent, ties.method = "min")[data_2025$team == "SEA"]

r_2025_label <- sprintf("r = %.2f", cor_2025)
title_2025 <- if (sea_resid > 0) {
  sprintf("Seattle's 2025 point differential beat what contract-value talent predicted (ranked %d of 32)",
          sea_talent_rank)
} else {
  sprintf("Seattle's 2025 point differential fell short of what contract-value talent predicted (ranked %d of 32)",
          sea_talent_rank)
}

p_2025 <- ggplot(data_2025, aes(x = contract_z, y = point_diff_pg)) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey55", linewidth = 0.6, linetype = "dashed") +
  geom_nfl_logos(aes(team_abbr = team, alpha = team == "SEA",
                      width = ifelse(team == "SEA", 0.075, 0.05))) +
  geom_point(data = data_2025 %>% filter(team == "SEA"), shape = 21, size = 7,
             stroke = 1.1, colour = ink_title, fill = NA) +
  annotate("text", x = -Inf, y = Inf, label = r_2025_label, hjust = -0.15, vjust = 1.5,
           size = 4, fontface = "bold", colour = ink_body) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.55), guide = "none") +
  labs(
    title = title_2025,
    subtitle = "Point differential per game vs. contract-value team talent (sum of top-25 roster players' APY as % of the cap\nin their signing year, z-scored within 2025), all 32 teams. Dashed line is the league-wide fit; Seattle is the\nfull-opacity, ringed logo.",
    x = "Contract-value talent, z-scored within 2025 (higher = more paid roster talent)",
    y = "Point differential per game"
  ) +
  theme_coach(grid = "y") +
  labs(caption = paste(strwrap(fig_caption(
    "games.csv (nflverse schedules), nflreadr::load_rosters/load_contracts (OverTheCap), 2025",
    "32 teams, 2025 regular season.",
    "Contract value: APY as % of the salary cap in the year each contract was signed (self-adjusts for cap growth). Rookie-scale deals systematically underpay young stars; see the printed console output for that sensitivity check."
  ), width = 150), collapse = "\n"))

save_fig("docs/figures/contract_2025.png", p_2025, w = 12, h = 7.5)

# -----------------------------------------------------------------------
# 10. Print headline numbers.
# -----------------------------------------------------------------------
cat("\n================ WINDOW CHOICE: CONTRACT DATA COVERAGE ================\n")
cat(sprintf("Window used: %d-%d (R/01's draft-capital script used 2003-%d; contract coverage before %d\n",
            FIRST_SEASON, LAST_SEASON, LAST_SEASON, FIRST_SEASON))
cat(sprintf("is thin enough that some team-seasons match fewer than %d players to a contract, which would\n", TOP_N))
cat("silently shrink the top-25 sum for those teams. From 2012 on, every team-season matches at least 29.)\n\n")
print(as.data.frame(match_rates))

cat("\n================ MODEL: point_diff_pg ~ contract_z + qb_contract_z + (1|head_coach) + (1|season) ================\n")
print(summary(fit_primary))
cat(sprintf("\n(%d of %d team-seasons dropped for missing primary-QB contract value; %d starters missing overall.)\n",
            n_dropped_missing_qb, nrow(team_talent), n_missing_qb_contract))

cat(sprintf("\nWins-per-point-differential slope (this window): %.4f wins per point/game\n", wins_per_point_diff))

cat(sprintf("\nCorrelation, contract talent (sum top-25 APY%%cap) vs. point differential per game, full window (%d-%d): r = %.3f (r^2 = %.3f)\n",
            FIRST_SEASON, LAST_SEASON, talent_perf_cor, talent_perf_cor^2))
cat("Per-season r, min/max:\n")
cat(sprintf("  min: %s r = %.3f\n", cor_by_season$season[which.min(cor_by_season$r)], min(cor_by_season$r)))
cat(sprintf("  max: %s r = %.3f\n", cor_by_season$season[which.max(cor_by_season$r)], max(cor_by_season$r)))
print(as.data.frame(cor_by_season))

cat(sprintf("\n2025-only: r = %.3f (r^2 = %.3f)\n", cor_2025, cor_2025^2))

cat(sprintf("\nRobustness, sum-of-top-25 vs. mean-of-top-25: correlation between the two models' coach effects (n=%d shared coaches) = %.3f\n",
            nrow(mean_compare), mean_cor))

cat("\n================ ROOKIE-DEAL BIAS CHECK ================\n")
cat(sprintf("Full-window r WITH rookie-scale players counted in top-25: r = %.3f\n", talent_perf_cor))
cat(sprintf("Full-window r WITHOUT rookie-scale players (excluded before taking top-25): r = %.3f\n", cor_norookie))
cat(sprintf("2025-only r WITH rookie-scale players: r = %.3f | WITHOUT: r = %.3f\n", cor_2025, cor_norookie_2025))
cat(sprintf("\nCorrelation, team's rookie share of its top-25 sum vs. how much it beat the contract-talent fit (residual): r = %.3f\n",
            rookie_share_bias_cor))
if (rookie_share_bias_cor > 0.1) {
  cat("Positive: teams that get more of their top-25 value from cheap rookie-scale players tend to OUTPERFORM what\n")
  cat("contract talent predicts, i.e. rookie-scale underpayment is a real source of understated team talent.\n")
}
sea_rookie_share <- team_talent %>% filter(season == 2025, team == "SEA") %>% pull(rookie_share)
sea_rookie_share_rank <- rank(-team_talent$rookie_share[team_talent$season == 2025], ties.method = "min")[data_2025$team == "SEA"]
cat(sprintf("\nSeattle 2025 rookie share of its top-25 contract value: %.1f%% (rank %d of 32, 1 = most rookie-scale-dependent)\n",
            100 * sea_rookie_share, sea_rookie_share_rank))
cat(sprintf("Seattle 2025 residual vs. the contract-talent fit: %.2f points/game (%s the fit)\n",
            sea_resid, ifelse(sea_resid > 0, "beat", "missed")))

cat("\n================ TOP 10 COACHES: wins/season above contract talent (min ", MIN_SEASONS_COACHED, " seasons) ================\n", sep = "")
print(as.data.frame(coach_table %>% filter(seasons >= MIN_SEASONS_COACHED) %>% slice_max(wins_per_season, n = 10) %>%
        select(head_coach, seasons, point_diff_effect, wins_per_season, active_2025)))

cat("\n================ BOTTOM 10 COACHES: wins/season above contract talent (min ", MIN_SEASONS_COACHED, " seasons) ================\n", sep = "")
print(as.data.frame(coach_table %>% filter(seasons >= MIN_SEASONS_COACHED) %>% slice_min(wins_per_season, n = 10) %>%
        select(head_coach, seasons, point_diff_effect, wins_per_season, active_2025)))

cat(sprintf("\n================ CONTRACT MODEL vs. R/01's DRAFT-CHART (Jimmy Johnson) MODEL ================\n"))
cat(sprintf("Correlation of coach effects, contract model vs. JJ-draft model (n=%d shared coaches, both windows overlap on 2012-2025): r = %.3f\n",
            nrow(jj_compare), jj_vs_contract_cor))

cat("\n================ SEATTLE 2025 ================\n")
mac_effect <- coach_table %>% filter(head_coach == "Mike Macdonald")
sea_qb_rank <- rank(-data_2025$qb_apy_cap_pct, ties.method = "min")[data_2025$team == "SEA"]
sea_qb_percentile <- 100 * (nrow(data_2025) - sea_qb_rank + 1) / nrow(data_2025)
cat(sprintf("Wins: %.1f | Point differential per game: %.2f\n", sea_row$wins, sea_row$point_diff_pg))
cat(sprintf("Contract talent (sum top-25 APY%%cap): %.3f (rank %d of 32 in the NFL)\n", sea_row$contract_talent, sea_talent_rank))
cat(sprintf("QB: %s, contract value %.1f%% of cap (rank %d of 32, %.0fth percentile among primary QBs, 100 = most expensive)\n",
            sea_row$qb_name, 100 * sea_row$qb_apy_cap_pct, sea_qb_rank, sea_qb_percentile))
cat(sprintf("2025 residual vs. league-wide contract-value fit: %.2f points/game (rank %d of 32, positive = overperformed the fit)\n",
            sea_resid, sea_rank_resid))
if (nrow(mac_effect) == 1) {
  cat(sprintf("Mike Macdonald's coach effect (all seasons in window, %d coached): %.3f points/game -> %.2f wins/season above contract talent (rank %d of %d coaches modeled)\n",
              mac_effect$seasons, mac_effect$point_diff_effect, mac_effect$wins_per_season,
              which(coach_table$head_coach == "Mike Macdonald"), nrow(coach_table)))
} else {
  cat("Mike Macdonald not found in coach_table (check name match / seasons in window).\n")
}

cat("\nWrote data/derived/contract_talent.csv, data/derived/coach_vs_contracts.csv,\n")
cat("docs/figures/contract_2025.png, docs/figures/coach_vs_contracts.png\n")
