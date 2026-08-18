# =============================================================================
# factory/81_fingerprints.R -- what each coach IS, in one grid.
#
# The workflow that mined the Sumer charting produced 27 verified coach-level
# traits. A leaderboard per trait is 27 charts nobody will watch. This is the
# useful shape: one row per coach, one column per trait, coloured by where he
# sits in the league. A viewer reads a coach across, or a trait down.
#
# EVERY NUMBER IS A PLAIN RATE, and that is deliberate. The situation-adjusted
# version of these measures correlates with the raw rate at 0.96 to 0.999,
# because down and distance do not predict WHICH run concept a man calls. The
# adjustment is real and it does nothing here, so quoting "above expected" would
# imply a correction that is not doing any work. Situation adjustment stays
# where it earns its keep: run/pass, blitz, shell rotation.
#
# The colour is the percentile among qualified callers, so the grid reads as
# "extreme for this league" rather than "big number". The printed value is the
# actual rate, because that is what gets said on camera.
#
# Out: docs/figures/factory/fingerprints_offense.png
#      docs/figures/factory/fingerprints_defense.png
#      data/factory/fingerprints.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales)
})
source("R/factory/lib_sumer.R")
source("R/lib/theme_coach.R")

d <- load_sumer()
d[, is_pass := run_pass == "P"]
d[, is_run  := run_pass == "R"]

# ---------------------------------------------------------------- offense
off <- d[off_caller != "", .(
  plays   = .N,
  runs    = sum(is_run),
  passes  = sum(is_pass),
  `Tempo`             = 100*mean(no_huddle == TRUE),
  `Under center`      = 100*mean(quarterback_alignment[is_pass] == "UNDER CENTER", na.rm = TRUE),
  `Play action`       = 100*mean(play_action[is_pass] == 1, na.rm = TRUE),
  `RPO`               = 100*mean(run_pass_option == TRUE),
  `Screens`           = 100*mean(screen[is_pass] == 1, na.rm = TRUE),
  `Trick looks`       = 100*mean(trick_look == TRUE),
  `Outside zone`      = 100*mean(run_concept[is_run] == "OUTSIDE ZONE", na.rm = TRUE),
  `Extra blocker`     = 100*mean((lead_run | split_run | cross_lead_run)[is_run], na.rm = TRUE)
), by = .(caller = off_caller)][plays >= 1500 & runs >= 400 & passes >= 600]
OFF_M <- c("Tempo","Under center","Play action","RPO","Screens","Trick looks",
           "Outside zone","Extra blocker")
cat(sprintf("offensive play-callers qualified: %d\n", nrow(off)))
cat("league rates:\n")
for (m in OFF_M) cat(sprintf("  %-15s %.1f%%\n", m, weighted.mean(off[[m]], off$plays)))

# ---------------------------------------------------------------- defense
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
# Rush count and coverage are only meaningful on a dropback: on a run play
# nobody is rushing, so 92.5% of runs chart at three or fewer rushers and
# including them would turn "rush three" into "faced a run".
def <- d[def_caller != "", .(
  plays = .N,
  `Five-plus rushers` = 100*mean(pass_rush_count[is_pass] >= 5, na.rm = TRUE),
  `Rush three`        = 100*mean(pass_rush_count[is_pass] <= 3, na.rm = TRUE),
  `Heavy box`         = 100*mean(box_defenders >= 8, na.rm = TRUE),
  `Man coverage`      = 100*mean(man_zone_coverage[is_pass] == "MAN", na.rm = TRUE),
  `Cover 2`           = 100*mean(coverage_scheme[is_pass] == "COVER 2", na.rm = TRUE),
  `Cover 3`           = 100*mean(coverage_scheme[is_pass] == "COVER 3", na.rm = TRUE),
  `Cover 6`           = 100*mean(coverage_scheme[is_pass] == "COVER 6", na.rm = TRUE),
  `Shell rotation`    = 100*mean(middle_of_field_coverage_look[has_shell] !=
                                 middle_of_field_coverage_played[has_shell], na.rm = TRUE)
), by = .(caller = def_caller)][plays >= 1500]
DEF_M <- c("Five-plus rushers","Rush three","Heavy box","Man coverage",
           "Cover 2","Cover 3","Cover 6","Shell rotation")
cat(sprintf("\ndefensive play-callers qualified: %d\n", nrow(def)))
for (m in DEF_M) cat(sprintf("  %-19s %.1f%%\n", m, weighted.mean(def[[m]], def$plays)))

write_csv(as.data.frame(rbind(
  melt(off, id.vars = "caller", measure.vars = OFF_M)[, side := "Offense"],
  melt(def, id.vars = "caller", measure.vars = DEF_M)[, side := "Defense"])),
  "data/factory/fingerprints.csv")

# ---------------------------------------------------------------- grid
grid_for <- function(tbl, measures, who, title, sub, out, w, h) {
  m <- melt(tbl, id.vars = "caller", measure.vars = measures,
            variable.name = "trait", value.name = "rate")
  m[, pct := frank(rate)/.N, by = trait]           # percentile within trait
  m <- m[caller %in% who]
  m[, caller := factor(caller, levels = rev(who))]
  m[, trait := factor(trait, levels = measures)]
  # league rate for the strip under each column
  lg <- melt(tbl, id.vars = "plays", measure.vars = measures,
             variable.name = "trait", value.name = "rate")[
             , .(lg = weighted.mean(rate, plays)), by = trait]

  ggplot(m, aes(trait, caller)) +
    geom_tile(aes(fill = pct), colour = "white", linewidth = 1.6) +
    geom_text(aes(label = sprintf("%.0f", rate),
                  colour = pct > 0.72 | pct < 0.16),
              size = 3.5, fontface = "bold") +
    geom_text(data = lg, aes(x = trait, y = 0.35, label = sprintf("lg %.0f", lg)),
              inherit.aes = FALSE, size = 2.7, colour = "grey45") +
    scale_fill_gradient2(low = "#2B8CBE", mid = "#f2f4f6", high = "#D55E00",
                         midpoint = 0.5, limits = c(0, 1),
                         breaks = c(0.05, 0.5, 0.95),
                         labels = c("lowest in\nthe league", "average", "highest in\nthe league"),
                         name = NULL) +
    scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "grey25"), guide = "none") +
    scale_x_discrete(position = "top", expand = expansion(0)) +
    scale_y_discrete(expand = expansion(add = c(0.85, 0.5))) +
    labs(title = title, subtitle = sub, x = NULL, y = NULL,
         caption = fig_caption(
           "SumerSports play charting 2022-2025; play-caller attribution from samhoppen/NFL_public",
           sprintf("Cells are the caller's own rate as a percentage; colour is his percentile among the %d qualified callers; 'lg' is the league rate.", nrow(tbl)),
           paste0("\nThese are plain rates, not situation-adjusted ones, and that is on purpose: adjusting for down, distance, score and clock moves every number here by a fraction\n",
                  "of a point, because the situation does not predict WHICH concept a man calls. The adjusted versions live on the model factory tab where they earn their keep.\n",
                  "Charting data by SumerSports. Built by R/factory/81."))) +
    theme_coach(grid = "none") +
    theme(axis.text.x.top = element_text(angle = 30, hjust = 0, size = rel(0.92), face = "bold"),
          plot.subtitle = element_text(margin = margin(b = 46)),
          axis.text.y = element_text(face = "bold", size = rel(1.0)),
          legend.position = "bottom", legend.key.width = unit(34, "pt"),
          legend.key.height = unit(9, "pt"),
          legend.text = element_text(size = rel(0.75), lineheight = 0.95),
          plot.margin = margin(12, 34, 10, 12)) -> p
  save_fig(out, p, w = w, h = h)
}

OFF_WHO <- c("Ben Johnson","Sean McVay","Kyle Shanahan","Andy Reid","Sean Payton",
             "Kevin O'Connell","Mike McDaniel","Liam Coen","Matt LaFleur",
             "Kliff Kingsbury","Arthur Smith","Zac Robinson","Kevin Stefanski","Joe Brady")
OFF_WHO <- OFF_WHO[OFF_WHO %in% off$caller]
grid_for(off, OFF_M, OFF_WHO,
  "Offensive fingerprints: what each play-caller actually calls",
  "Each cell is that caller's own rate. Blue is the low end of the league, orange the high end.",
  "docs/figures/factory/fingerprints_offense.png", 12.2, 7.6)

DEF_WHO <- c("Mike Macdonald","Vic Fangio","Brian Flores","Steve Spagnuolo","Todd Bowles",
             "Jim Schwartz","DeMeco Ryans","Dan Quinn","Sean McDermott","Gus Bradley",
             "Don Martindale","Lou Anarumo","Raheem Morris","Ejiro Evero")
DEF_WHO <- DEF_WHO[DEF_WHO %in% def$caller]
grid_for(def, DEF_M, DEF_WHO,
  "Defensive fingerprints: what each coordinator actually plays",
  "Each cell is that coordinator's own rate. Blue is the low end of the league, orange the high end.",
  "docs/figures/factory/fingerprints_defense.png", 12.2, 7.6)
