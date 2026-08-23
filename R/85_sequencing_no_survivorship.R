# =============================================================================
# 85_sequencing_no_survivorship.R -- the give-them-a-look finding, tested in a
# way selection cannot touch.
#
# The worry, verbatim: "the Shany narrative kind of died a bit because of
# selection bias, right?"
#
# THE WORRY IS RIGHT IN GENERAL. A personnel group reaches a third straight
# snap only if the drive stayed alive, so third-look snaps are drawn from
# drives that were already working. R/68 handled that by comparing every play
# with the league at the same down, distance, position in the drive and streak
# depth. This script removes the channel a different way, so the finding does
# not rest on one correction.
#
# THE FIX HERE: hold the situation completely fixed. Every play in the test is
# FIRST AND TEN. A first-and-ten is a first-and-ten whether it is the opening
# snap of a drive or the third snap of a look: the offense has just earned a
# fresh set of downs either way, so "the drive was working" no longer separates
# the groups. Then compare, inside cells of field position, score state and
# quarter, what the offense does on a first-and-ten when it has already shown
# this personnel group twice in the drive against when it has just come out in
# it.
#
# A second, stricter version: exact one-to-one matching. Every third-look
# first-and-ten is paired with a first-look first-and-ten by the same caller,
# same yardline band, same score state, same quarter, same season.
#
# If the third-look edge is selection, it dies here. If it survives both, the
# narrative is safe to say out loud.
#
# Out: docs/figures/sequencing_first_down_only.png
#      data/derived/sequencing_no_survivorship.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(85)

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & offensive_personnel_basic != ""]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, pers := sub("\\*", "", offensive_personnel_basic)]
d[, prev_pers := shift(pers), by = drive_id]
d[, new_grp := is.na(prev_pers) | pers != prev_pers]
d[, streak_id := cumsum(new_grp), by = drive_id]
d[, pos := seq_len(.N), by = .(drive_id, streak_id)]
d[, k := seq_len(.N), by = drive_id]
d[, epa := expected_points_added]

# ---------------------------------------------------------------- the fixed situation
fd <- d[down == 1 & distance == 10 & !is.na(epa)]
fd[, look := fifelse(pos == 1, "first snap in this group", fifelse(pos == 2, "second", "third or later"))]
fd[, look := factor(look, levels = c("first snap in this group", "second", "third or later"))]
fd[, fp := cut(yards_to_goal_line, c(0, 50, 70, 85, 100), include.lowest = TRUE)]
fd[, sc := fifelse(offense_score_diff < -8, "trailing", fifelse(offense_score_diff > 8, "leading", "one score"))]
cat(sprintf("first-and-ten snaps, 2022-23 through 2025-26: %d\n", nrow(fd)))
print(fd[, .(snaps = .N, epa = round(mean(epa), 3)), by = look][order(look)])

# league baseline inside each situation cell, then the caller's edge at each look
fd[, adj := epa - mean(epa), by = .(fp, sc, quarter, look)]
CAL <- list(list(w = "Kyle Shanahan", lab = "Shanahan", pkg = "21"),
            list(w = "Sean McVay", lab = "McVay", pkg = "13"),
            list(w = "Ben Johnson", lab = "Ben Johnson", pkg = "12"))
res <- rbindlist(lapply(CAL, function(c1) {
  x <- fd[off_caller == c1$w & pers == c1$pkg]
  x[, .(n = .N, v = mean(adj), se = sd(adj) / sqrt(.N),
        who = sprintf("%s, %s personnel", c1$lab, c1$pkg)), by = look]
}))
setorder(res, who, look)
cat("\nFIRST AND TEN ONLY, signature package, against the league in the same field position, score state and quarter:\n")
print(res[, .(who, look, n, edge = round(v, 3), se = round(se, 3), z = round(v / se, 1))])

# ---------------------------------------------------------------- exact matching
match_test <- function(w, pkg) {
  x <- fd[off_caller == w & pers == pkg]
  x[, cell := paste(season, fp, sc, quarter)]
  third <- x[look == "third or later"]; first <- x[look == "first snap in this group"]
  common <- intersect(third$cell, first$cell)
  if (!length(common)) return(NULL)
  t3 <- third[cell %in% common, .(e3 = mean(epa), n3 = .N), by = cell]
  t1 <- first[cell %in% common, .(e1 = mean(epa), n1 = .N), by = cell]
  m <- merge(t3, t1, by = "cell")
  data.table(who = w, cells = nrow(m), n3 = sum(m$n3), n1 = sum(m$n1),
             diff = weighted.mean(m$e3 - m$e1, pmin(m$n3, m$n1)),
             se = sd(m$e3 - m$e1) / sqrt(nrow(m)))
}
mt <- rbindlist(lapply(CAL, function(c1) match_test(c1$w, c1$pkg)))
cat("\nEXACT MATCHING, third-or-later against first snap of the look, same season, field position, score state and quarter:\n")
print(mt[, .(who, cells, n3, n1, diff = round(diff, 3), se = round(se, 3), z = round(diff / se, 1))])
write_csv(as.data.frame(rbind(res[, .(test = "first and ten only", who, look = as.character(look), n, v, se)],
                              mt[, .(test = "exact matching", who, look = "third minus first", n = n3, v = diff, se)])),
          "data/derived/sequencing_no_survivorship.csv")

# ---------------------------------------------------------------- figure
res[, who := factor(who, levels = c("Shanahan, 21 personnel", "McVay, 13 personnel", "Ben Johnson, 12 personnel"))]
dg <- position_dodge(width = 0.32)
p <- ggplot(res, aes(look, v, group = who, colour = who)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_linerange(aes(ymin = v - se, ymax = v + se), position = dg, linewidth = 3, alpha = 0.18) +
  geom_line(linewidth = 1.2, position = dg) + geom_point(size = 3, position = dg) +
  geom_text(aes(label = sprintf("%+.2f", v)), vjust = -1.2, size = 3.1, fontface = "bold",
            position = dg, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%d", n)), vjust = 2.1, size = 2.5, colour = "grey55",
            position = dg, show.legend = FALSE) +
  scale_colour_manual(values = c("Shanahan, 21 personnel" = "#D55E00", "McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837"), name = NULL) +
  scale_fill_manual(values = c("Shanahan, 21 personnel" = "#D55E00", "McVay, 13 personnel" = "#2B8CBE", "Ben Johnson, 12 personnel" = "#1B7837"), guide = "none") +
  scale_y_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = sprintf("On first and ten only, McVay's third look in 13 personnel holds at %+.2f; Shanahan's 21 fades to %+.2f",
                       res[grepl("McVay", who) & look == "third or later"]$v,
                       res[grepl("Shanahan", who) & look == "third or later"]$v),
       subtitle = paste0("Every snap here is a first and ten, so the offense has just earned a fresh set of downs whether this is the first snap of the personnel\n",
                         "group or the third. That removes the reason third-look snaps looked good in the first place, which is that they come from drives that\n",
                         "were already working. Each play is measured against the league in the same field position, score state and quarter."),
       x = "how many snaps this personnel group has been on the field, this drive (small number is the snap count)",
       y = "EPA per play vs the league in the same spot",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nThe stricter version pairs each third-look first-and-ten with a first-look first-and-ten by the same caller in the same season, field position, score state\nand quarter: Shanahan %+.2f, McVay %+.2f, Ben Johnson %+.2f. Band is one standard error. Built by R/85.",
                 mt[who == "Kyle Shanahan"]$diff, mt[who == "Sean McVay"]$diff, mt[who == "Ben Johnson"]$diff))) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/sequencing_first_down_only.png", p, w = 11, h = 6.5)
cat("\nOut: sequencing_first_down_only.png\n")
