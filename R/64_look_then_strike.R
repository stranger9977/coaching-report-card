# =============================================================================
# 64_look_then_strike.R -- "giving them a look, then however they perform
# after that initial look (think Shanahan on their first play in 21 vs 2nd,
# 3rd etc)". Performance by the nth consecutive snap in the same personnel
# group, situation-adjusted, league against the two headline callers.
#
# HONEST LIMIT, on the chart: later streak positions are survivor-flavored.
# A personnel group stays on the field partly because the drive stayed
# alive, so deep-streak snaps lean toward drives that were already working.
# The down-and-distance adjustment absorbs some of that, not all of it.
#
# Out: docs/figures/look_then_strike.png, data/derived/look_then_strike.csv
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
d[, adj := expected_points_added - mean(expected_points_added, na.rm = TRUE), by = dd]
d[, pos_b := fifelse(pos == 1, "1st snap\nof the look", fifelse(pos == 2, "2nd", fifelse(pos == 3, "3rd", fifelse(pos <= 5, "4th-5th", "6th+"))))]
POSL <- c("1st snap\nof the look", "2nd", "3rd", "4th-5th", "6th+")
d[, pos_b := factor(pos_b, levels = POSL)]

lg  <- d[, .(n = .N, adj = mean(adj, na.rm = TRUE), who = "league"), by = pos_b]
sh  <- d[off_caller == "Kyle Shanahan" & offensive_personnel_basic == "21",
         .(n = .N, adj = mean(adj, na.rm = TRUE), who = "Shanahan in 21 personnel"), by = pos_b]
mv  <- d[off_caller == "Sean McVay",
         .(n = .N, adj = mean(adj, na.rm = TRUE), who = "McVay, all personnel"), by = pos_b]
t <- rbind(lg, sh, mv)
write_csv(as.data.frame(t), "data/derived/look_then_strike.csv")
cat("league:\n"); print(lg[order(pos_b)])
cat("change vs stay, league adj:", round(mean(d[new_grp == TRUE]$adj, na.rm = TRUE), 3), "vs",
    round(mean(d[new_grp == FALSE]$adj, na.rm = TRUE), 3), "\n")
cat("Shanahan 21:\n"); print(sh[order(pos_b)])

t[, who := factor(who, levels = c("league", "McVay, all personnel", "Shanahan in 21 personnel"))]

p <- ggplot(t, aes(pos_b, adj, group = who, colour = who)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(data = t[who == "Shanahan in 21 personnel"],
            aes(label = sprintf("%+.2f", adj)), vjust = -0.9, size = 3.1,
            fontface = "bold", show.legend = FALSE) +
  geom_text(data = t[who == "Shanahan in 21 personnel" & pos_b == "6th+"],
            aes(label = sprintf("\n\nonly %d snaps", n)), vjust = 0.4, size = 2.7,
            colour = "grey50", show.legend = FALSE) +
  scale_colour_manual(values = c("league" = "grey60", "McVay, all personnel" = "#2B8CBE",
                                 "Shanahan in 21 personnel" = "#D55E00"), name = NULL) +
  scale_y_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = "Give them the look, then strike: Shanahan's 21 personnel peaks on the 3rd straight snap",
       subtitle = paste0("Points per play compared with the league average for the same down and distance, by how many consecutive snaps the\n",
                         "same personnel group has been on the field. The league is flat: showing a look repeatedly neither builds nor burns value.\n",
                         "Shanahan's 21 personnel climbs to +0.28 on the third straight look before fading, exactly the give-them-a-look shape."),
       x = "consecutive snaps with the same personnel group on the field",
       y = "points per play vs the league average, same down and distance",
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nNothing is modeled: a play's value is its points minus the league average for its down-and-distance bucket. Honest limit: deep-streak snaps are\nsurvivor-flavored, a personnel group stays out partly because the drive stayed alive, and the bucket comparison absorbs only part of that. League-wide, changing personnel vs staying is worth about nothing (-0.01 vs +0.01). Built by R/64.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/look_then_strike.png", p, w = 10.5, h = 6.8)
cat("\nOut: look_then_strike.png\n")
