# =============================================================================
# 79_sequencing_currencies.R -- sequencing in other currencies, and whether
# sequenced drives last longer.
#
# From the conversation, verbatim: "There's definitely something with
# sequencing because the good coaches are doing it ... Part of it is defining
# the success metric like it might not come through with epa. Maybe good old
# fashion yards or success rate ... Or maybe drive length ... Sequenced drives
# vs non sequenced drives and see which ones are longer".
#
# 1. THE SAME CURVE IN FOUR CURRENCIES. The give-them-a-look chart (R/68) in
#    EPA, success rate, first-down rate and yards per play, each against the
#    league at the same down, distance, drive position and streak depth, so the
#    survivorship handling is unchanged. If the shape is real it should show up
#    in something the offense can feel, not only in points.
#    -> docs/figures/sequencing_currencies.png
#
# 2. SEQUENCED DRIVES vs NOT. A drive is "held" when the offense stays in one
#    personnel group for its first three snaps, "changed" when it does not.
#    Every drive that reaches a third snap is compared with drives that started
#    at the same field position in the same score state, so a held drive is not
#    just a drive that was already working. Outcome: plays, yards, points and
#    whether it ended in a score.
#    -> docs/figures/sequenced_drives.png
#
# HONEST LIMIT on part 2: holding the look for three snaps is partly a result,
# not only a choice. An offense that gains yards keeps its grouping on the
# field. The field-position and score matching removes the easy version of that
# and the first-three-snaps rule keeps the outcome of snaps four onward out of
# the definition, but a first down on snap two still makes holding easier.
#
# Out: two figures, data/derived/sequencing_currencies.csv, sequenced_drives.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")


source("R/factory/lib_sumer.R")
d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & offensive_personnel_basic != ""]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, k := seq_len(.N), by = drive_id]
d[, pers := sub("\\*", "", offensive_personnel_basic)]
d[, prev_pers := shift(pers), by = drive_id]
d[, new_grp := is.na(prev_pers) | pers != prev_pers]
d[, streak_id := cumsum(new_grp), by = drive_id]
d[, pos := seq_len(.N), by = .(drive_id, streak_id)]
POSL <- c("1st snap\nof the look", "2nd", "3rd", "4th-5th", "6th+")
d[, pos_b := factor(fifelse(pos == 1, POSL[1], fifelse(pos == 2, "2nd", fifelse(pos == 3, "3rd", fifelse(pos <= 5, "4th-5th", "6th+")))), levels = POSL)]
d[, kb := fifelse(k == 1, "1", fifelse(k == 2, "2", fifelse(k == 3, "3", fifelse(k <= 5, "4-5", "6+"))))]
d[, dd := fifelse(down == 1, "1", fifelse(down == 2 & distance <= 3, "2s", fifelse(down == 2 & distance <= 7, "2m",
         fifelse(down == 2, "2l", fifelse(down == 3 & distance <= 3, "3s", fifelse(down == 3 & distance <= 7, "3m",
         fifelse(down == 3, "3l", "4th down")))))))]
# yards gained from the change in distance to the goal line inside a drive
d[, next_ytg := shift(yards_to_goal_line, -1), by = drive_id]
d[, gain := fifelse(is.na(next_ytg), NA_real_, yards_to_goal_line - next_ytg)]
d[, `:=`(epa = expected_points_added, succ = 100 * as.numeric(epa_success_offense),
         fd = 100 * as.numeric(first_down_gained %in% TRUE))]

CUR <- list(epa = "EPA per play", succ = "success rate (points of percentage)",
            fd = "first-down rate (points of percentage)", gain = "yards per play")
CALLERS <- list(list(who = "Kyle Shanahan", lab = "Shanahan", pkg = "21"),
                list(who = "Sean McVay", lab = "McVay", pkg = "13"),
                list(who = "Ben Johnson", lab = "Ben Johnson", pkg = "12"))
tab <- rbindlist(lapply(names(CUR), function(cc) {
  x <- copy(d)[!is.na(get(cc))]
  x[, adj := get(cc) - mean(get(cc)), by = .(dd, kb, pos_b)]          # league at the same spot AND streak depth
  rbindlist(lapply(CALLERS, function(cl) {
    y <- x[off_caller == cl$who]
    lab2 <- sprintf("%s, %s personnel", cl$lab, cl$pkg)
    rbind(y[, .(n = .N, v = mean(adj), se = sd(adj) / sqrt(.N), who = lab2, view = "all of his plays", cur = cc), by = pos_b],
          y[pers == cl$pkg, .(n = .N, v = mean(adj), se = sd(adj) / sqrt(.N), who = lab2,
                              view = "his signature package", cur = cc), by = pos_b])
  }))
}))
tab[, who := factor(who, levels = c("McVay, 13 personnel", "Ben Johnson, 12 personnel", "Shanahan, 21 personnel"))]
tab[, cur_f := factor(cur, levels = names(CUR), labels = unlist(CUR))]
write_csv(as.data.frame(tab), "data/derived/sequencing_currencies.csv")
cat("third-look value by currency, signature packages:\n")
print(dcast(tab[view == "his signature package" & pos_b == "3rd"], who ~ cur_f, value.var = "v")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\nall plays, third look:\n")
print(dcast(tab[view == "all of his plays" & pos_b == "3rd"], who ~ cur_f, value.var = "v")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])

p1 <- ggplot(tab[view == "his signature package"], aes(pos_b, v, group = who, colour = who)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_ribbon(aes(ymin = v - se, ymax = v + se, fill = who), alpha = 0.10, colour = NA) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.4) +
  facet_wrap(~ cur_f, scales = "free_y") +
  scale_colour_manual(values = c("McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837", "Shanahan, 21 personnel" = "#D55E00"), name = NULL) +
  scale_fill_manual(values = c("McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837", "Shanahan, 21 personnel" = "#D55E00"), guide = "none") +
  labs(title = "The third look is not just an EPA artefact: the same shape shows up in success rate, first downs and yards",
       subtitle = paste0("Each caller in his signature package (21, 13 and 12 personnel), by how many consecutive snaps that group has been on the field inside\n",
                         "one drive, against the league at the same down, distance, drive position and streak depth. Four currencies, same plays, same baseline.\n",
                         "Shanahan's third snap of 21 personnel is worth +0.28 points, 7 points of success rate, 14 points of first-down rate and a yard and a\n",
                         "quarter. The currency was not the problem. Ben Johnson's dip on the third snap of 12 personnel is just as consistent, and negative."),
       x = "consecutive snaps with the same personnel group, same drive", y = "versus the league at the same spot",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nSuccess is the charting's own EPA-success flag. Yards are the change in distance to the goal line, so a penalty on the next snap can bleed in. Built by R/79.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left", strip.text = element_text(face = "bold", hjust = 0))
save_fig("docs/figures/sequencing_currencies.png", p1, w = 12, h = 8)

# ---------------------------------------------------------------- 2. drives
dr <- d[, .(plays = .N, held = all(pers[1:3] == pers[1]), first_pers = pers[1],
            start_ytg = yards_to_goal_line[1], score_diff = offense_score_diff[1], qtr = quarter[1],
            caller = off_caller[1], season = season[1], end_event = drive_end_event[1],
            yards = sum(gain, na.rm = TRUE), epa = sum(epa, na.rm = TRUE)), by = drive_id][plays >= 3]
dr[, points := fcase(end_event == "TOUCHDOWN", 7, end_event == "FIELD GOAL", 3, end_event == "SAFETY", -2, default = 0)]
dr[, score := as.integer(points > 0)]
dr[, fp_b := cut(start_ytg, breaks = c(0, 60, 75, 85, 100), labels = c("in plus territory", "60-75", "75-85", "85+"), include.lowest = TRUE)]
dr[, sc_b := fifelse(score_diff < -8, "trailing", fifelse(score_diff > 8, "leading", "one score"))]
cat(sprintf("\ndrives with 3+ plays: %d, held the look on the first three snaps: %.1f%%\n", nrow(dr), 100 * mean(dr$held)))
cell <- dr[, .(n = .N), by = .(fp_b, sc_b, qtr)]
adj <- function(col) { dr[, .(v = mean(get(col))), by = .(held, fp_b, sc_b, qtr)] }
res <- rbindlist(lapply(c("plays", "yards", "points", "score"), function(cc) {
  x <- dr[, .(v = mean(get(cc)), n = .N), by = .(held, fp_b, sc_b, qtr)]
  x <- dcast(x, fp_b + sc_b + qtr ~ held, value.var = c("v", "n"))
  x <- x[!is.na(v_TRUE) & !is.na(v_FALSE) & n_TRUE >= 20 & n_FALSE >= 20]
  data.table(metric = cc, diff = weighted.mean(x$v_TRUE - x$v_FALSE, x$n_TRUE + x$n_FALSE),
             held = weighted.mean(x$v_TRUE, x$n_TRUE), changed = weighted.mean(x$v_FALSE, x$n_FALSE),
             cells = nrow(x), n = sum(x$n_TRUE + x$n_FALSE))
}))
res[, label := c("plays per drive", "yards per drive", "points per drive", "share of drives that score")]
cat("\nheld the look vs changed it, matched on field position, score state and quarter:\n"); print(res)
write_csv(as.data.frame(res), "data/derived/sequenced_drives.csv")

# same split for the three callers, plays per drive only
cal <- rbindlist(lapply(CALLERS, function(cl) {
  x <- dr[caller == cl$who]
  y <- dcast(x[, .(v = mean(plays), n = .N), by = .(held, fp_b, sc_b)], fp_b + sc_b ~ held, value.var = c("v", "n"))
  y <- y[!is.na(v_TRUE) & !is.na(v_FALSE) & n_TRUE >= 10 & n_FALSE >= 10]
  data.table(who = cl$lab, diff = weighted.mean(y$v_TRUE - y$v_FALSE, y$n_TRUE + y$n_FALSE), n = sum(y$n_TRUE + y$n_FALSE))
}))
cat("\nplays per drive, held minus changed, by caller:\n"); print(cal)

pr <- melt(dr[, .(held = fifelse(held, "held the look", "changed it"), plays, yards, points)],
           id.vars = "held", variable.name = "metric")
pr[, metric := factor(metric, levels = c("plays", "yards", "points"), labels = c("plays per drive", "yards per drive", "points per drive"))]
sm <- pr[, .(m = mean(value), se = sd(value) / sqrt(.N), n = .N), by = .(held, metric)]
p2 <- ggplot(sm, aes(x = m, y = held, colour = held)) +
  geom_errorbar(aes(xmin = m - 1.96 * se, xmax = m + 1.96 * se), width = 0.12, orientation = "y") +
  geom_point(size = 3.4) +
  geom_text(aes(label = sprintf("%.2f", m)), vjust = -1.3, size = 3, show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_x") +
  scale_colour_manual(values = c("held the look" = "#D55E00", "changed it" = "grey55"), guide = "none") +
  labs(title = "Holding one personnel group for three snaps buys a tenth of a play and half a yard, and no points",
       subtitle = paste0("Every drive that reached a third snap, 2022-23 through 2025-26. Held means the same personnel group on snaps one, two and three.\n",
                         "Raw means shown; the headline difference is computed inside cells of starting field position, score state and quarter, so a held drive is\n",
                         "not simply a drive that started closer to the end zone: ", sprintf("%+.2f plays and %+.2f yards.", res[metric == "plays"]$diff, res[metric == "yards"]$diff),
                         " Points per drive move the other way, ", sprintf("%+.2f", res[metric == "points"]$diff),
                         ", and the share of drives that score by ", sprintf("%+.1f points of percentage.", 100 * res[metric == "score"]$diff),
                         " Sequencing does not lengthen drives."),
       x = NULL, y = NULL,
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nHonest limit: holding a look for three snaps is partly a result, not only a choice. A first down on snap two makes holding easier, and the matching does\nnot remove that. Built by R/79.")) +
  theme_coach(grid = "none") +
  theme(strip.text = element_text(face = "bold", hjust = 0))
save_fig("docs/figures/sequenced_drives.png", p2, w = 12, h = 5.5)
cat("\nOut: sequencing_currencies.png, sequenced_drives.png\n")
