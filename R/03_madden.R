# =============================================================================
# 03_madden.R
#
# The fun, audience-facing version of the roster-talent question, quoting
# the video collaborator verbatim: "WAR based on Madden ratings or
# something...like how much is a team overachieving based on their player
# talent." Rate each roster by its Madden video-game ratings (not draft
# capital, not real football outcomes -- just what the game thinks of the
# players) and see which teams, and which coaches, beat the video game.
#
# Data hunt (documented in full because this took real effort and hit real
# walls -- see "Known data issues" below):
#
#   1. maddenratings.weebly.com hosts per-edition xlsx archives. Six editions
#      downloaded and inspected (Madden 20-25, covering seasons 2019-2024):
#        madden_nfl_20_-_full_player_ratings.xlsx
#        madden_nfl_21_-_full_player_ratings.xlsx
#        madden_nfl_22_final_roster.xlsx
#        madden_nfl_23_player_ratings.xlsx
#        maddennfl24fullplayerratings.xlsx
#        madden_nfl_25_-_full_player_ratings.xlsx
#      All live under https://maddenratings.weebly.com/uploads/1/4/0/9/14097292/
#      Verdict: Madden 20-24 (seasons 2019-2023) check out -- every team's
#      top-rated player matches the real historical roster. Madden 25 is
#      UNUSABLE: its "Team" values are city-era names (San Diego Chargers,
#      St. Louis Rams, Washington Redskins, Oakland Raiders) and its top
#      rated players are Calvin Johnson, Drew Brees (Saints), Aaron Rodgers
#      (Packers), Rob Gronkowski, Pat McAfee at kicker for the Colts -- this
#      is the 2013 "Madden NFL 25" 25th-anniversary title (2013 season),
#      not the year-numbered 2024-season game. EA used the name "Madden 25"
#      twice in franchise history for two unrelated games, and weebly's
#      /madden-nfl-25.html page is the 2013 one. Confirmed by checking every
#      team's #1 rated player against known history; not a formatting fluke.
#      Also: no Madden 26 page/files exist on the site at all (404).
#      Column schemas differ on every single edition (team format, name
#      split across 1-2 columns, OVR column name) -- there is no shared
#      parser across editions from this source.
#
#   2. GitHub mirror: theedgepredictor/nfl-madden-data. A pre-cleaned,
#      per-season (not per-edition) rebuild of the same underlying data,
#      already using nflverse-style team codes and a consistent schema
#      across years, back to 2001. Spot-checked seasons 2012, 2016-2025:
#      every team's top-rated player matches real history, including a
#      correct, present-tense 2024 file (Lamar Jackson BAL 98, Josh Allen
#      BUF 92, Joe Burrow CIN 93, Micah Parsons DAL 98, etc.) where
#      weebly's own "Madden 25" page is not usable. Its 2025 file is dated
#      2025-08-06 -- genuine Madden 26 LAUNCH ratings, not a live/current
#      snapshot. This is the source actually used below.
#
#   3. A live snapshot was also pulled by hand from leaguestation.com's
#      Madden 26 team-ratings tool (browser automation; that site has no
#      API and isn't curl-downloadable) as a cross-check. Its numbers are
#      visibly inflated relative to source #2's 2025.csv for the same
#      players (e.g. Jaxon Smith-Njigba 95 there vs. 86 in the launch
#      file) because it reflects in-season rating bumps through the date
#      pulled, not launch ratings. Not used in the analysis below --
#      launch ratings are the right, comparable baseline across seasons --
#      but the gap itself is a fun aside: the game rates 2025 Seattle
#      higher in hindsight than it did on day one.
#
# Team code crosswalk: source #2 uses PERMANENT franchise codes (LAR, LAC,
# LV) for every season, while games.csv uses period-accurate codes for the
# seasons in play here (OAK through 2019, LV from 2020). The season window
# below (2017-2025) was chosen specifically to dodge the other two
# relocations (Chargers were still "SD" through 2016, Rams still "STL"
# through 2015) so only one crosswalk rule is needed: LAR -> LA always,
# LV -> OAK for season <= 2019.
#
# Method:
#   1. Team talent per season = mean OVR of the top 25 rated players on
#      that team's Madden launch roster (a "top of the roster" measure,
#      not a full-52 average -- stars carry teams, and it keeps injured/
#      inactive depth players from diluting the number). Z-scored within
#      season because OVR scales drift edition to edition (Madden inflates
#      or compresses the 0-99 scale over time; a raw 82 in 2017 isn't the
#      same roster quality as a raw 82 in 2024).
#   2. Wins ~ talent_z, one point per team-season, simple linear fit.
#      The r tells you how well the video game's ratings predict real
#      wins -- i.e. how much of "winning football" the game actually
#      captures.
#   3. Overachievement = residual (actual wins - predicted wins) per
#      team-season, aggregated to head coach (>= 3 seasons in the sample).
#
# Sources:
#   - data/raw/madden/{season}.csv, cached from
#     https://raw.githubusercontent.com/theedgepredictor/nfl-madden-data/main/data/madden/processed/{season}.csv
#     seasons 2017-2025 (re-downloaded automatically if the cache is empty).
#   - ~/stranger9977/nfl-analysis/data/games.csv (nflverse schedules):
#     wins per team-season and head coach per team-season, REG only.
#
# Out:
#   - data/derived/madden_team_seasons.csv (one row per team-season)
#   - docs/figures/madden_scatter.png (talent z vs wins, all team-seasons)
#   - docs/figures/madden_coaches.png (coach overachievement leaderboard)
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(ggrepel)
})

source("R/lib/theme_coach.R")

accent_over  <- "#2166AC"  # muted blue: overachieved the game's rating
accent_under <- "#B2182B"  # muted red: underachieved it

# -----------------------------------------------------------------------
# 1. Download (cached) per-season Madden files.
# -----------------------------------------------------------------------
seasons <- 2017:2025
raw_dir <- "data/raw/madden"
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

base_url <- "https://raw.githubusercontent.com/theedgepredictor/nfl-madden-data/main/data/madden/processed/"

for (yr in seasons) {
  dest <- file.path(raw_dir, sprintf("%d.csv", yr))
  if (!file.exists(dest) || file.size(dest) == 0) {
    url <- paste0(base_url, yr, ".csv")
    cat(sprintf("downloading %s -> %s\n", url, dest))
    download.file(url, dest, method = "curl", extra = "-sL --max-time 60", quiet = TRUE)
  }
}

missing <- seasons[!file.exists(file.path(raw_dir, sprintf("%d.csv", seasons))) |
                      file.size(file.path(raw_dir, sprintf("%d.csv", seasons))) == 0]
if (length(missing) > 0) {
  stop(sprintf("Missing/empty Madden files for seasons: %s. Check network access to raw.githubusercontent.com.",
               paste(missing, collapse = ", ")))
}

# -----------------------------------------------------------------------
# 2. Load all seasons, standardize team codes to games.csv convention.
# -----------------------------------------------------------------------
madden_raw <- bind_rows(lapply(seasons, function(yr) {
  d <- read_csv(file.path(raw_dir, sprintf("%d.csv", yr)), show_col_types = FALSE)
  d %>%
    filter(!is.na(overallrating)) %>%
    transmute(season = yr, team_raw = team, fullname, position, ovr = overallrating)
}))

madden <- madden_raw %>%
  mutate(team = case_when(
    team_raw == "LAR" ~ "LA",
    team_raw == "LV" & season <= 2019 ~ "OAK",
    TRUE ~ team_raw
  ))

cat(sprintf("Loaded Madden launch ratings: %d players, seasons %d-%d, %d team-seasons (%d team codes -- OAK/LV both count, same Raiders franchise split by the 2020 relocation).\n",
            nrow(madden), min(madden$season), max(madden$season),
            n_distinct(paste(madden$team, madden$season)), n_distinct(madden$team)))

# -----------------------------------------------------------------------
# 3. Team talent per season: mean OVR of the top 25 rated players.
# -----------------------------------------------------------------------
team_talent <- madden %>%
  group_by(season, team) %>%
  arrange(desc(ovr), .by_group = TRUE) %>%
  slice_head(n = 25) %>%
  summarise(n_players = n(), top25_ovr = mean(ovr), .groups = "drop") %>%
  filter(n_players == 25) %>%  # drop any team-season with a thin roster in the source
  group_by(season) %>%
  mutate(talent_z = as.numeric(scale(top25_ovr))) %>%
  ungroup()

cat(sprintf("Team-seasons with a full top-25 roster: %d (of %d team x season combos possible)\n",
            nrow(team_talent), n_distinct(madden$team) * length(seasons)))

# -----------------------------------------------------------------------
# 4. Wins and head coach per team-season, from games.csv (REG only).
# -----------------------------------------------------------------------
games <- read_csv("~/stranger9977/nfl-analysis/data/games.csv", show_col_types = FALSE) %>%
  filter(game_type == "REG", season %in% seasons, !is.na(result))

home_rows <- games %>%
  transmute(season, team = home_team, coach = home_coach,
            win = as.integer(result > 0), tie = as.integer(result == 0))
away_rows <- games %>%
  transmute(season, team = away_team, coach = away_coach,
            win = as.integer(result < 0), tie = as.integer(result == 0))
team_games <- bind_rows(home_rows, away_rows)

team_wins <- team_games %>%
  group_by(season, team) %>%
  summarise(wins = sum(win) + 0.5 * sum(tie), games = n(), .groups = "drop")

# Head coach of record for the team-season: whoever coached the most games.
team_coach <- team_games %>%
  count(season, team, coach) %>%
  group_by(season, team) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(season, team, coach)

# -----------------------------------------------------------------------
# 5. Join, fit wins ~ talent_z.
# -----------------------------------------------------------------------
team_seasons <- team_talent %>%
  inner_join(team_wins, by = c("season", "team")) %>%
  inner_join(team_coach, by = c("season", "team"))

dropped <- anti_join(team_talent, team_wins, by = c("season", "team"))
if (nrow(dropped) > 0) {
  cat(sprintf("Dropped %d team-seasons with Madden data but no games.csv match: %s\n",
              nrow(dropped), paste(paste0(dropped$team, " ", dropped$season), collapse = ", ")))
}

fit <- lm(wins ~ talent_z, data = team_seasons)
r <- cor(team_seasons$wins, team_seasons$talent_z)
team_seasons <- team_seasons %>%
  mutate(predicted_wins = predict(fit),
         residual_wins = wins - predicted_wins)

cat(sprintf("\nSeasons covered: %s (%d team-seasons, 32 franchises)\n",
            paste(range(team_seasons$season), collapse = "-"),
            nrow(team_seasons)))
cat(sprintf("Wins ~ Madden talent (top-25 OVR, z-scored within season): r = %.3f (r^2 = %.3f)\n",
            r, r^2))
cat(sprintf("  fit: wins = %.2f + %.2f * talent_z\n", coef(fit)[1], coef(fit)[2]))

# -----------------------------------------------------------------------
# 6. Coach aggregation (>= 3 seasons in the sample).
# -----------------------------------------------------------------------
coach_agg <- team_seasons %>%
  group_by(coach) %>%
  summarise(
    n_seasons = n(),
    first_season = min(season), last_season = max(season),
    mean_talent_z = mean(talent_z),
    total_wins = sum(wins), total_predicted = sum(predicted_wins),
    total_residual = sum(residual_wins),
    residual_per_season = mean(residual_wins),
    .groups = "drop"
  ) %>%
  filter(n_seasons >= 3) %>%
  arrange(desc(residual_per_season))

cat(sprintf("\nCoaches with >= 3 seasons in the sample: %d\n", nrow(coach_agg)))

cat("\nTop 10 coaches, wins above Madden's rating (per season):\n")
print(coach_agg %>% slice_head(n = 10) %>%
        select(coach, n_seasons, first_season, last_season, residual_per_season, total_residual),
      n = 10)

cat("\nBottom 10 coaches, wins above Madden's rating (per season):\n")
print(coach_agg %>% slice_tail(n = 10) %>%
        select(coach, n_seasons, first_season, last_season, residual_per_season, total_residual),
      n = 10)

# -----------------------------------------------------------------------
# 7. SEATTLE 2025 block.
# -----------------------------------------------------------------------
sea25 <- team_seasons %>% filter(season == 2025) %>%
  mutate(talent_rank = rank(-top25_ovr, ties.method = "min")) %>%
  filter(team == "SEA")

cat("\n--- SEATTLE 2025 BLOCK ---\n")
if (nrow(sea25) == 1) {
  cat(sprintf("Madden 26 launch talent (top-25 OVR): %.2f, rank %d of %d teams, z = %+.2f\n",
              sea25$top25_ovr, sea25$talent_rank, nrow(filter(team_seasons, season == 2025)),
              sea25$talent_z))
  cat(sprintf("Actual wins: %.1f | Madden-predicted wins: %.2f | overachievement: %+.2f wins\n",
              sea25$wins, sea25$predicted_wins, sea25$residual_wins))
  cat(sprintf("Coach of record: %s\n", sea25$coach))
} else {
  cat("No 2025 Seattle row (check games.csv coverage for the 2025 season).\n")
}

# -----------------------------------------------------------------------
# 8. Chart A: talent_z vs wins, all team-seasons.
# -----------------------------------------------------------------------
notable_teams <- team_seasons %>%
  mutate(abs_resid = abs(residual_wins)) %>%
  slice_max(abs_resid, n = 8)

sea_2025_row <- team_seasons %>% filter(season == 2025, team == "SEA")
label_set <- bind_rows(notable_teams, sea_2025_row) %>% distinct(team, season, .keep_all = TRUE)

scatter_title <- sprintf("Madden's ratings explain %d%% of the variance in real NFL wins", round(r^2 * 100))

p_scatter <- ggplot(team_seasons, aes(x = talent_z, y = wins)) +
  geom_point(aes(colour = residual_wins > 0), size = 1.9, alpha = 0.75) +
  geom_point(data = sea_2025_row, shape = 21, size = 3.6, stroke = 1,
             colour = ink_title, fill = NA) +
  geom_smooth(method = "lm", se = TRUE, colour = ink_body, fill = "grey85",
              linewidth = 0.6) +
  geom_text_repel(data = label_set, aes(label = paste0(team, " '", str_sub(season, 3, 4))),
                   size = 2.9, fontface = "bold", colour = ink_title,
                   segment.colour = "grey60", segment.size = 0.3,
                   max.overlaps = Inf, box.padding = 0.4, point.padding = 0.15,
                   min.segment.length = 0.1, seed = 1) +
  scale_colour_manual(values = c(`TRUE` = accent_over, `FALSE` = accent_under)) +
  scale_x_continuous(name = "Madden launch talent (top-25 OVR, z-scored within season)") +
  scale_y_continuous(name = "actual wins", breaks = seq(0, 17, 2)) +
  labs(
    title = scatter_title,
    subtitle = paste0("One point per team-season, ", min(team_seasons$season), "-", max(team_seasons$season),
                       ". Circled point is 2025 Seattle. Blue = won more than the\n",
                       "video game's rating implied; red = won less."),
    caption = fig_caption(
      "Madden launch ratings (theedgepredictor/nfl-madden-data, sourced from maddenratings.weebly.com); nflverse schedules",
      sprintf("%d team-seasons, %d-%d, regular season wins.", nrow(team_seasons),
              min(team_seasons$season), max(team_seasons$season)),
      sprintf("r = %.2f between talent_z and wins.", r)
    )
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/madden_scatter.png", p_scatter, w = 12, h = 7.5)

# -----------------------------------------------------------------------
# 9. Chart B: coach leaderboard.
# -----------------------------------------------------------------------
coach_plot <- coach_agg %>%
  mutate(coach = factor(coach, levels = rev(coach[order(residual_per_season)])),
         beat_it = residual_per_season > 0)

coach_title <- sprintf("%s beats the video game's talent grade most, per season",
                        as.character(coach_agg$coach[1]))

p_coaches <- ggplot(coach_plot, aes(x = residual_per_season, y = coach)) +
  geom_baseline(0) +
  geom_col(aes(fill = beat_it), width = 0.65) +
  geom_text(aes(label = sprintf("%+.2f", residual_per_season),
                hjust = ifelse(residual_per_season >= 0, -0.15, 1.15)),
            size = 2.9, colour = ink_body) +
  scale_fill_manual(values = c(`TRUE` = accent_over, `FALSE` = accent_under)) +
  scale_x_continuous(name = "wins above Madden's talent-implied pace, per season") +
  labs(
    title = coach_title,
    subtitle = paste0("Head coaches with >= 3 seasons in the sample (", min(coach_agg$first_season), "-",
                       max(coach_agg$last_season), "). Positive means the team won more\n",
                       "than its roster's Madden rating predicted, on average."),
    caption = fig_caption(
      "Madden launch ratings (theedgepredictor/nfl-madden-data); nflverse schedules",
      sprintf("%d coaches, minimum 3 seasons.", nrow(coach_agg)),
      "Predicted wins from a single linear fit of wins on talent_z across all team-seasons."
    )
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.75)))

save_fig("docs/figures/madden_coaches.png", p_coaches, w = 10, h = max(6, nrow(coach_agg) * 0.22))

# -----------------------------------------------------------------------
# 10. Write derived CSV.
# -----------------------------------------------------------------------
out <- team_seasons %>%
  select(season, team, coach, n_players, top25_ovr, talent_z, wins,
         predicted_wins, residual_wins)
write_csv(out, "data/derived/madden_team_seasons.csv")
cat(sprintf("\nwrote data/derived/madden_team_seasons.csv (%d rows)\n", nrow(out)))

cat("\nDone.\n")
