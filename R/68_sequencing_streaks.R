# =============================================================================
# 68_sequencing_streaks.R -- give them the look, then strike, for ALL of a
# caller's plays, with the survivorship problem handled.
#
# The asks, verbatim: "What about all their plays thou? Idk. Maybe like just
# all sequencing for them too. To see if in general they are better." And:
# "lets make sure we are getting survivorship bias out of there etc."
#
# What changes from R/64:
#   1. STREAKS LIVE INSIDE A DRIVE. R/64 counted consecutive same-personnel
#      snaps across a whole game, so a "3rd straight look" could be the first
#      play after a punt. Here a streak restarts with every drive, which is
#      what the defense actually sees.
#   2. THE SURVIVORSHIP FIX. A personnel group stays on the field partly
#      because the drive stayed alive, so deep-streak snaps lean toward drives
#      that were already working. R/64 adjusted for down and distance only.
#      Here every play is compared with the league average for the same down
#      and distance, the same position in the drive (3rd play, 4th-5th, 6th+)
#      AND the same streak depth. Whatever the league as a whole gets from
#      being deep in a live drive is subtracted out. The league line is
#      therefore zero by construction, and a caller's line is how much better
#      or worse he does than the league at that exact depth.
#   3. TWO VIEWS. All of a caller's plays (the "in general" question) and his
#      signature package (the R/64 question), side by side on the same
#      baseline. EPA and win probability each get a figure.
#
# What this does NOT remove: a caller's own drives staying alive more than
# the league's because he is good. That is not bias, that is the thing being
# measured.
#
# Out: docs/figures/sequencing_streaks_epa.png, sequencing_streaks_wpa.png
#      data/derived/sequencing_streaks.csv
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
d[, new_grp := offensive_personnel_basic != shift(offensive_personnel_basic, fill = "X"), by = drive_id]
d[, streak_id := cumsum(new_grp), by = drive_id]
d[, pos := seq_len(.N), by = .(drive_id, streak_id)]
d[, k := seq_len(.N), by = drive_id]
d[, kb := fifelse(k == 1, "1st play", fifelse(k == 2, "2nd play", fifelse(k == 3, "3rd play",
          fifelse(k <= 5, "4th-5th play", "6th+ play"))))]
d[, dd := fifelse(down == 1, "1", fifelse(down == 2 & distance <= 3, "2s", fifelse(down == 2 & distance <= 7, "2m",
         fifelse(down == 2, "2l", fifelse(down == 3 & distance <= 3, "3s", fifelse(down == 3 & distance <= 7, "3m",
         fifelse(down == 3, "3l", "4th down")))))))]
POSL <- c("1st snap\nof the look", "2nd", "3rd", "4th-5th", "6th+")
d[, pos_b := factor(fifelse(pos == 1, POSL[1], fifelse(pos == 2, "2nd", fifelse(pos == 3, "3rd", fifelse(pos <= 5, "4th-5th", "6th+")))), levels = POSL)]
d <- attach_wpa(d)
d[, wpa100 := 100 * wpa]

# survivorship diagnostic: how much the raw league streak curve was drive position
d <- d[!is.na(expected_points_added)]
raw  <- d[, .(raw = mean(expected_points_added) - mean(d$expected_points_added)), by = pos_b]
d[, adj_dd := expected_points_added - mean(expected_points_added), by = dd]
dd_only <- d[, .(dd_only = mean(adj_dd)), by = pos_b]
d[, adj_epa := expected_points_added - mean(expected_points_added), by = .(dd, kb, pos_b)]
d[!is.na(wpa100), adj_wpa := wpa100 - mean(wpa100), by = .(dd, kb, pos_b)]
cat("league EPA by streak depth, raw vs down-distance only (the survivorship slope R/64 still carried):\n")
print(merge(raw, dd_only, by = "pos_b")[order(pos_b)][, .(pos_b, raw = round(raw, 3), dd_only = round(dd_only, 3))])

CALLERS <- list(
  list(who = "Kyle Shanahan", lab = "Shanahan", pkg = "21"),
  list(who = "Sean McVay",    lab = "McVay",    pkg = "13"),
  list(who = "Ben Johnson",   lab = "Ben Johnson", pkg = "12"))
line <- function(x, col, grp, view) x[!is.na(get(col)), .(n = .N, adj = mean(get(col)), se = sd(get(col)) / sqrt(.N),
                                                          who = grp, view = view), by = pos_b]
build <- function(col) rbindlist(lapply(CALLERS, function(cl) {
  lab2 <- sprintf("%s, %s personnel", cl$lab, cl$pkg)
  rbind(line(d[off_caller == cl$who], col, lab2, "every play he called"),
        line(d[off_caller == cl$who & offensive_personnel_basic == cl$pkg], col, lab2,
             "only his signature package"))}))
t_epa <- build("adj_epa"); t_wpa <- build("adj_wpa")
LV <- c("McVay, 13 personnel", "Ben Johnson, 12 personnel", "Shanahan, 21 personnel")
t_epa[, who := factor(who, levels = LV)]
t_wpa[, who := factor(who, levels = LV)]
write_csv(as.data.frame(rbind(t_epa[, metric := "epa"], t_wpa[, metric := "wpa_pct_points"])), "data/derived/sequencing_streaks.csv")
cat("\nWPA pct points, all plays:\n"); print(dcast(t_wpa[view == "all of his plays"], who ~ pos_b, value.var = "adj")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("WPA pct points, signature:\n"); print(dcast(t_wpa[view != "all of his plays"], who ~ pos_b, value.var = "adj")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\nEPA, all plays:\n"); print(dcast(t_epa[view == "all of his plays"], who ~ pos_b, value.var = "adj")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("EPA, signature package:\n"); print(dcast(t_epa[view != "all of his plays"], who ~ pos_b, value.var = "adj")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("snaps, all plays:\n"); print(dcast(t_epa[view == "all of his plays"], who ~ pos_b, value.var = "n"))
cat("snaps, signature:\n"); print(dcast(t_epa[view != "all of his plays"], who ~ pos_b, value.var = "n"))

draw <- function(t, is_epa, file) {
  t[, view := factor(view, levels = c("every play he called", "only his signature package"))]
  allv <- t[view == levels(t$view)[1]]
  better <- allv[, .(ahead = mean(adj > 0), mean_adj = mean(adj)), by = who]
  fmt <- if (is_epa) "%+.2f" else "%+.1f"
  p <- ggplot(t, aes(pos_b, adj, group = who, colour = who)) +
    geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.5) +
    geom_ribbon(aes(ymin = adj - se, ymax = adj + se, fill = who), alpha = 0.10, colour = NA) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.6) +
    geom_text(data = t[n < 60], aes(label = sprintf("\n\n%d snaps", n)), vjust = 0.4, size = 2.4,
              colour = "grey50", show.legend = FALSE) +
    facet_wrap(~ view) +
    scale_colour_manual(values = c("McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837", "Shanahan, 21 personnel" = "#D55E00"), name = NULL) +
    scale_fill_manual(values = c("McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837", "Shanahan, 21 personnel" = "#D55E00"), guide = "none") +
    scale_y_continuous(labels = label_number(style_positive = "plus", accuracy = if (is_epa) 0.1 else 0.5)) +
    labs(title = if (is_epa) "In general all three are ahead of the league at most streak depths; the third-look spike lives in the package"
                 else "Same picture in win probability: ahead in general, the third-look spike only inside the signature package",
         subtitle = paste0(if (is_epa) "EPA per play" else "Win probability added per play, in percentage points",
                           " minus the league average for the same down and distance, the same position in the drive AND the same streak\n",
                           "depth, by how many consecutive snaps the same personnel group has been on the field inside one drive. The league is zero by\n",
                           "construction: whatever everyone gets from being deep in a live drive is already subtracted. Left: every play he called. Right: only the\npersonnel group named in the legend."),
         x = "consecutive snaps with the same personnel group on the field, same drive",
         y = if (is_epa) "EPA per play vs the league, same situation and streak depth"
             else "win probability added vs the league, same situation (pct points)",
         caption = fig_caption(
           paste0("SumerSports play charting", if (is_epa) "" else " with nflverse win probability", ", 2022-23 through 2025-26 regular seasons, garbage time excluded"),
           paste0("\nShaded band: one standard error. Streaks restart with every drive, so a 3rd straight look is the 3rd straight snap of the same drive. The survivorship problem\n",
                  "(a group stays on the field because the drive is alive) is handled by comparing each play with the league at the same drive position and streak depth.\n",
                  "What remains is the caller's own edge over the league at that depth, including his drives staying alive more because he is good. ",
                  if (is_epa) "Ribbons on the right get\nwide past the 2nd look: thin samples. " else "",
                  "Built by R/68."))) +
    theme_coach(grid = "y") +
    theme(legend.position = "top", legend.justification = "left",
          plot.subtitle = element_text(lineheight = 1.12),
          strip.text = element_text(face = "bold", hjust = 0, size = 11))
  save_fig(file, p, w = 12, h = 7)
  better
}
b1 <- draw(t_epa, TRUE,  "docs/figures/sequencing_streaks_epa.png")
b2 <- draw(t_wpa, FALSE, "docs/figures/sequencing_streaks_wpa.png")
cat("\nshare of streak depths where each caller is above the league, all plays (EPA):\n"); print(b1)
cat("\nOut: sequencing_streaks_epa.png, sequencing_streaks_wpa.png\n")
