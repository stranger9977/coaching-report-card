# =============================================================================
# 64_look_then_strike.R -- "giving them a look, then however they perform
# after that initial look (think Shanahan on their first play in 21 vs 2nd,
# 3rd etc)". Performance by the nth consecutive snap in the same personnel
# group, situation-adjusted, league against the headline callers, each in
# his signature package: Shanahan in 21, McVay in 13 (the 2025-26 pivot),
# Ben Johnson in 12 (his second package every year, 345 snaps by 2025-26).
# Follow-up ask, verbatim: "Adding Benny boy here and other personnel
# packages would be tight...like the Rams in 13...whatever the Bears run".
#
# HONEST LIMIT, on the chart: later streak positions are survivor-flavored.
# A personnel group stays on the field partly because the drive stayed
# alive, so deep-streak snaps lean toward drives that were already working.
# The down-and-distance adjustment absorbs some of that, not all of it.
#
# Out: docs/figures/look_then_strike.png (EPA), look_then_strike_wpa.png (win probability),
#      data/derived/look_then_strike.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & offensive_personnel_basic != ""]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, gm := paste(season, week, off_team)]
d[, new_grp := offensive_personnel_basic != shift(offensive_personnel_basic, fill = "X"), by = gm]
d[, streak_id := cumsum(new_grp), by = gm]
d[, pos := seq_len(.N), by = .(gm, streak_id)]
d[, dd := fifelse(down == 1, "1", fifelse(down == 2 & distance <= 3, "2s", fifelse(down == 2 & distance <= 7, "2m",
         fifelse(down == 2, "2l", fifelse(down == 3 & distance <= 3, "3s", fifelse(down == 3 & distance <= 7, "3m", "3l"))))))]
d <- attach_wpa(d)
d[, adj_epa := expected_points_added - mean(expected_points_added, na.rm = TRUE), by = dd]
d[, adj_wpa := 100 * (wpa - mean(wpa, na.rm = TRUE)), by = dd]   # percentage points of win probability
d[, pos_b := fifelse(pos == 1, "1st snap\nof the look", fifelse(pos == 2, "2nd", fifelse(pos == 3, "3rd", fifelse(pos <= 5, "4th-5th", "6th+"))))]
POSL <- c("1st snap\nof the look", "2nd", "3rd", "4th-5th", "6th+")
d[, pos_b := factor(pos_b, levels = POSL)]
cat("win probability matched on", round(100 * mean(!is.na(d$wpa)), 1), "% of snaps\n")

summarise_lines <- function(col) {
  f <- function(x, who) x[!is.na(get(col)), .(n = .N, adj = mean(get(col)), who = who), by = pos_b]
  t <- rbind(f(d, "league"),
             f(d[off_caller == "Kyle Shanahan" & offensive_personnel_basic == "21"], "Shanahan in 21 personnel"),
             f(d[off_caller == "Sean McVay" & offensive_personnel_basic == "13"], "McVay in 13 personnel"),
             f(d[off_caller == "Ben Johnson" & offensive_personnel_basic == "12"], "Ben Johnson in 12 personnel"))
  t[, who := factor(who, levels = c("league", "McVay in 13 personnel",
                                    "Ben Johnson in 12 personnel", "Shanahan in 21 personnel"))]
  t[order(who, pos_b)]
}
t_epa <- summarise_lines("adj_epa")
t_wpa <- summarise_lines("adj_wpa")
write_csv(as.data.frame(rbind(t_epa[, metric := "epa"], t_wpa[, metric := "wpa"])), "data/derived/look_then_strike.csv")
cat("change vs stay, league adj EPA:", round(mean(d[new_grp == TRUE]$adj_epa, na.rm = TRUE), 3), "vs",
    round(mean(d[new_grp == FALSE]$adj_epa, na.rm = TRUE), 3), "\n")
cat("EPA:\n"); print(t_epa[who != "league"])
cat("WPA (percentage points):\n"); print(t_wpa[who != "league"])

draw <- function(t, metric, file) {
  sh3 <- t[who == "Shanahan in 21 personnel" & pos_b == "3rd"]$adj
  mv  <- t[who == "McVay in 13 personnel"]
  mv_all_ahead <- all(mv$adj > t[who == "league"]$adj)
  is_epa <- metric == "epa"
  fmt <- if (is_epa) "%+.2f" else "%+.1f"
  ahead <- if (mv_all_ahead) "stays ahead at every depth" else "is ahead at most depths"
  subtitle <- if (is_epa) paste0(
    "EPA per play compared with the league average for the same down and distance, by how many consecutive snaps the\n",
    "same personnel group has been on the field. Each caller in his signature package: 21 personnel in San Francisco, 13 in\n",
    "Los Angeles (the 2025-26 pivot), 12 in Detroit and Chicago. The league is flat: showing a look repeatedly neither builds\n",
    sprintf("nor burns value. Shanahan's 21 climbs to %+.2f on the third straight look; the Rams' 13 %s.", sh3, ahead))
  else paste0(
    "Win probability added per play, in percentage points, vs the league average for the same down and distance, by how\n",
    "many consecutive snaps the same personnel group has been on the field. Each caller in his signature package: 21 personnel\n",
    "in San Francisco, 13 in Los Angeles (the 2025-26 pivot), 12 in Detroit and Chicago. The league is flat: showing a look\n",
    sprintf("repeatedly neither builds nor burns value. Shanahan's 21 climbs to %+.1f on the third straight look; the Rams' 13 %s.", sh3, ahead))
  p <- ggplot(t, aes(pos_b, adj, group = who, colour = who)) +
    geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_text(data = t[who == "Shanahan in 21 personnel"],
              aes(label = sprintf(fmt, adj)), vjust = -0.9, size = 3.1,
              fontface = "bold", show.legend = FALSE) +
    geom_text(data = t[who != "league" & n < 60],
              aes(label = sprintf("\n\n%d snaps", n)), vjust = 0.4, size = 2.5,
              colour = "grey50", show.legend = FALSE) +
    scale_colour_manual(values = c("league" = "grey60", "McVay in 13 personnel" = "#2B8CBE",
                                   "Ben Johnson in 12 personnel" = "#1B7837",
                                   "Shanahan in 21 personnel" = "#D55E00"), name = NULL) +
    scale_y_continuous(labels = label_number(style_positive = "plus", accuracy = if (is_epa) 0.1 else 0.5)) +
    labs(title = "Give them the look, then strike: three signature packages against a flat league",
         subtitle = subtitle,
         x = "consecutive snaps with the same personnel group on the field",
         y = if (is_epa) "EPA per play vs the league average, same down and distance"
             else "win probability added vs league average (pct points)",
         caption = fig_caption(
           if (is_epa) "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded"
           else "SumerSports play charting with nflverse win probability, 2022-23 through 2025-26 regular seasons, garbage time excluded",
           paste0("\nNothing is modeled: a play's value is its ", if (is_epa) "points" else "win probability change",
                  " minus the league average for its down-and-distance bucket. Honest limit: deep-streak snaps are\nsurvivor-flavored, a personnel group stays out partly because the drive stayed alive, and the bucket comparison absorbs only part of that. The 12- and\n13-personnel lines run thin past the 2nd straight snap (samples marked); read their late shapes as direction, not gospel. ",
                  if (is_epa) "League-wide, changing\npersonnel vs staying is worth about nothing (-0.01 vs +0.01). "
                  else "Win probability is nflverse's, matched to the\ncharting play by play (98% of snaps); it rewards the same yards more when the game is close. ",
                  "Built by R/64."))) +
    theme_coach(grid = "y") +
    theme(legend.position = "top", legend.justification = "left",
          plot.subtitle = element_text(lineheight = 1.12))
  save_fig(file, p, w = 10.5, h = 6.8)
}
draw(t_epa, "epa", "docs/figures/look_then_strike.png")
draw(t_wpa, "wpa", "docs/figures/look_then_strike_wpa.png")
cat("\nOut: look_then_strike.png, look_then_strike_wpa.png\n")
