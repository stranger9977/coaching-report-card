# =============================================================================
# factory/82_travels.R -- what leaves with the coach, and what stays.
#
# This is the finding the whole project has been circling. Take every team-season
# and the season that follows it, split those pairs by whether the play-caller
# was still there, and ask how well each measure predicts itself across the gap.
#
#   If a number is SCHEME, it should survive when the caller stays and collapse
#   when he leaves. It belongs to a man.
#   If a number is ROSTER or OUTCOME, it should not care either way. It belongs
#   to a building.
#
# That is exactly what happens, and the separation is brutal. It is also the
# cleanest answer this project has to the question underneath the whole video:
# how do you tell coaching apart from the players?
#
# The sting in the tail: the measures that are unmistakably the coach are the
# ones that do NOT move the scoreboard, and the measure that moves the
# scoreboard most is not a coach trait at all.
#
# METHOD. Consecutive team-season pairs only. A caller "stayed" if the modal
# play-caller of the team is the same man in both seasons. Sumer covers
# 2022-2025, so there are three transitions per team and 96 pairs per side.
#
# Out: docs/figures/factory/travels.png
#      data/factory/travels.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales)
})
source("R/factory/lib_sumer.R")
source("R/lib/theme_coach.R")

d <- load_sumer()
d[, is_pass := run_pass == "P"]

off <- d[off_caller != "", .(
  `Tempo (no huddle)`      = 100*mean(no_huddle == TRUE),
  `Under center to throw`  = 100*mean(quarterback_alignment[is_pass] == "UNDER CENTER", na.rm = TRUE),
  `RPO rate`               = 100*mean(run_pass_option == TRUE),
  `Trick looks`            = 100*mean(trick_look == TRUE),
  `Outside zone runs`      = 100*mean(run_concept[run_pass == "R"] == "OUTSIDE ZONE", na.rm = TRUE),
  `Catchable throw share`  = 100*mean(catchable_target[is_pass & is_targeted_pass == TRUE], na.rm = TRUE),
  `Offensive EPA per play` = mean(expected_points_added, na.rm = TRUE),
  caller = names(sort(table(off_caller), decreasing = TRUE))[1], n = .N),
  by = .(team = off_team, season)][n >= 400]

def <- d[def_caller != "", .(
  `Five-plus rushers`      = 100*mean(pass_rush_count >= 5, na.rm = TRUE),
  `Cover 3 rate`           = 100*mean(coverage_scheme == "COVER 3", na.rm = TRUE),
  `Heavy box (8+)`         = 100*mean(box_defenders >= 8, na.rm = TRUE),
  `Sack rate`              = 100*mean(is_sack == TRUE),
  `EPA allowed per play`   = mean(expected_points_added, na.rm = TRUE),
  caller = names(sort(table(def_caller), decreasing = TRUE))[1], n = .N),
  by = .(team = def_team, season)][n >= 400]

pairs_for <- function(x, cols, side) {
  setorder(x, team, season)
  x[, `:=`(nxt_season = shift(season, -1), nxt_caller = shift(caller, -1)), by = team]
  for (cc in cols) x[, (paste0("nxt_", cc)) := shift(get(cc), -1), by = team]
  p <- x[!is.na(nxt_season) & nxt_season - season == 1]
  p[, stayed := caller == nxt_caller]
  rbindlist(lapply(cols, function(cc) {
    a <- p[stayed == TRUE]; b <- p[stayed == FALSE]
    ra <- cor(a[[cc]], a[[paste0("nxt_", cc)]], use = "complete.obs")
    rb <- cor(b[[cc]], b[[paste0("nxt_", cc)]], use = "complete.obs")
    data.table(side = side, measure = cc, stayed = ra, changed = rb,
               n_stay = nrow(a), n_change = nrow(b))
  }))
}

OFFC <- c("Tempo (no huddle)","Under center to throw","RPO rate","Trick looks",
          "Outside zone runs","Catchable throw share","Offensive EPA per play")
DEFC <- c("Five-plus rushers","Cover 3 rate","Heavy box (8+)","Sack rate",
          "EPA allowed per play")
res <- rbind(pairs_for(off, OFFC, "Offense"), pairs_for(def, DEFC, "Defense"))

ROSTER <- c("Catchable throw share","Offensive EPA per play","Sack rate","EPA allowed per play")
res[, kind := fifelse(measure %in% ROSTER, "What the players do", "What the coach calls")]
res[, drop := stayed - changed]
setorder(res, kind, -drop)
write_csv(as.data.frame(res), "data/factory/travels.csv")
cat("=== year-over-year team correlation, by whether the play-caller stayed ===\n")
print(res[, .(kind, side, measure, stayed = round(stayed,3), changed = round(changed,3),
              drop = round(drop,3), n_stay, n_change)])
cat(sprintf("\nmean drop, scheme measures: %.2f | roster measures: %.2f\n",
            res[kind == "What the coach calls", mean(drop)],
            res[kind == "What the players do", mean(drop)]))

lng <- melt(res, id.vars = c("measure","kind","side"),
            measure.vars = c("stayed","changed"),
            variable.name = "state", value.name = "r")
lng[, state := factor(fifelse(state == "stayed", "Same play-caller", "New play-caller"),
                      levels = c("Same play-caller","New play-caller"))]
lvl <- res[order(kind, drop)]$measure
lng[, measure := factor(measure, levels = lvl)]
res[, measure := factor(measure, levels = lvl)]

p <- ggplot(lng, aes(r, measure)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_segment(data = res, aes(x = changed, xend = stayed, y = measure, yend = measure),
               colour = "grey72", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(aes(colour = state), size = 3.4) +
  geom_text(data = res, aes(x = stayed, y = measure, label = sprintf("%.2f", stayed)),
            inherit.aes = FALSE, vjust = -1.25, size = 2.8, colour = "#1d6a99", fontface = "bold") +
  geom_text(data = res, aes(x = changed, y = measure, label = sprintf("%.2f", changed)),
            inherit.aes = FALSE, vjust = 2.1, size = 2.8, colour = "#8a3d00", fontface = "bold") +
  facet_grid(kind ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c("Same play-caller" = "#2B8CBE", "New play-caller" = "#D55E00")) +
  scale_x_continuous(limits = c(-0.15, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title = "What leaves with the coach, and what stays",
    subtitle = "How well a team's number in one season predicts the next, split by whether the same man was still calling plays",
    x = "year-over-year correlation for the same team", y = NULL,
    caption = fig_caption(
      "SumerSports play charting 2022-2025; play-caller attribution from samhoppen/NFL_public",
      sprintf("%d consecutive team-season pairs a side. A caller 'stayed' if the team's modal play-caller is the same man in both seasons.",
              res[side == "Offense"]$n_stay[1] + res[side == "Offense"]$n_change[1]),
      paste0("\nThe top block is scheme, and it belongs to a man. Tempo goes from 0.94 to -0.03 the season a new play-caller arrives, five-plus rushers from 0.83 to 0.05, Cover 3\n",
             "from 0.74 to -0.03, trick looks from 0.83 to 0.07. Average drop across the eight scheme measures is 0.63. The bottom block is what the players did, and it mostly\n",
             "does not care who is calling: sack rate moves 0.06, catchable throws 0.21, and offensive EPA is MORE stable when the caller changes, because it is the roster.\n",
             "One honest exception: EPA allowed does fall 0.36, so defensive results follow a coordinator more than offensive ones follow a play-caller. The sting is that the\n",
             "numbers which are unmistakably the coach are the ones that do not move the scoreboard. Charting data by SumerSports. Built by R/factory/82.")))+
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left",
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90, face = "bold", size = rel(0.95)),
        panel.spacing = unit(14, "pt"))
save_fig("docs/figures/factory/travels.png", p, w = 12, h = 7.4)
