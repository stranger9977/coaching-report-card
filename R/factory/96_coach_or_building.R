# =============================================================================
# factory/96_coach_or_building.R -- is it the coach, or is it the building?
#
# GOAL.md called the across-team test "the biggest open hole" and it turned out
# to matter more than anything else in the project.
#
# R5 asks whether a coach's residual repeats from one season to the next. It
# does, strongly. But that test cannot tell a coach from a franchise: the same
# roster, the same coordinators, the same personnel and the same quarterback
# produce the same tendencies whoever is nominally calling it. The only version
# that isolates the man is persistence across a CHANGE OF CLUB.
#
# Splitting the pairs that way is not close:
#
#   target            same team    changed team
#   Run or pass         +0.54          +0.21
#   Play action         +0.64          +0.43
#   Pre-snap motion     +0.81          +0.11
#   Blitz               +0.82          +0.35
#   Man coverage        +0.57          +0.68
#
# So most of what R5 was crediting to coaches is a property of the building.
# Pre-snap motion is the extreme: an almost perfectly repeatable tendency that
# essentially does not travel. Man coverage is the exception that holds up,
# though on eleven pairs.
#
# This does not invalidate the residuals. It reframes what they measure: a
# team's identity in a given season, which the head coach shapes but does not
# solely own. Every claim on the board that reads "this coach does X" should be
# read as "this coach's offence does X".
#
# Out: docs/figures/factory/coach_or_building.png
#      data/factory/persistence_split.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")

rows <- rbindlist(lapply(list.files("data/factory/fits", full.names = TRUE), function(f) {
  o <- readRDS(f); r <- o$residuals
  if (is.null(r$persist_same)) return(NULL)
  data.table(label = o$label, cleared = all(o$grade$pass),
             overall = r$persist, n_pairs = r$n_pairs,
             same = r$persist_same, n_same = r$n_same,
             moved = r$persist_moved, n_moved = r$n_moved)
}))
rows[, drop := same - moved]
setorder(rows, -drop)
write_csv(as.data.frame(rows), "data/factory/persistence_split.csv")
print(rows)

# Fisher z interval on each correlation, so the small moved-team samples carry
# their own uncertainty rather than being read as point estimates.
zci <- function(r, n) {
  if (is.na(r) || n < 6) return(c(NA, NA))
  z <- atanh(r); se <- 1/sqrt(n - 3)
  tanh(c(z - 1.96*se, z + 1.96*se))
}
long <- rbindlist(lapply(seq_len(nrow(rows)), function(i) {
  x <- rows[i]
  rbindlist(list(
    data.table(label = x$label, kind = "Same team", r = x$same, n = x$n_same,
               lo = zci(x$same, x$n_same)[1], hi = zci(x$same, x$n_same)[2]),
    data.table(label = x$label, kind = "Changed team", r = x$moved, n = x$n_moved,
               lo = zci(x$moved, x$n_moved)[1], hi = zci(x$moved, x$n_moved)[2])))
}))
long[, kind := factor(kind, levels = c("Same team", "Changed team"))]
long[, label := factor(label, levels = rev(rows$label))]

p <- ggplot(long, aes(r, label, colour = kind)) +
  geom_vline(xintercept = 0.30, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.7,
                 position = position_dodge(width = 0.55)) +
  geom_point(size = 3.1, position = position_dodge(width = 0.55)) +
  geom_text(aes(label = sprintf("%.2f", r)), position = position_dodge(width = 0.55),
            vjust = -1.1, size = 3, fontface = "bold", show.legend = FALSE) +
  annotate("text", x = 0.315, y = 0.62, hjust = 0, size = 3.1, colour = "grey35",
           fontface = "bold", label = "the 0.30 bar this project uses") +
  scale_colour_manual(values = c("Same team" = "#2B8CBE", "Changed team" = "#D55E00")) +
  scale_x_continuous(limits = c(-0.35, 1.05), breaks = seq(-0.25, 1, 0.25)) +
  labs(
    title = "How much of a coach's tendency is the coach, and how much is the building?",
    subtitle = "Year-over-year persistence of each coach's residual, split by whether he stayed at the same club or moved",
    x = "correlation between this season's residual and last season's", y = NULL,
    caption = fig_caption(
      "Season-grouped out-of-sample residuals, caller-seasons 2015 to 2025 with at least 150 called plays",
      "Bars are 95% Fisher intervals. The changed-team samples are small, 11 to 42 pairs, and are wide accordingly.",
      paste0("\nThis is the test GOAL.md called the biggest open hole, and it lands hard. A tendency that repeats while a coach stays put can belong to the roster, the coordinators or\n",
             "the quarterback just as easily as to him. Only persistence across a change of club isolates the man, and on four of five targets it falls by half or more. Pre-snap\n",
             "motion is the extreme, going from almost perfectly repeatable to nothing at all once a coach changes buildings.\n",
             "It does not make the residuals meaningless. It changes what they are: a description of a TEAM's identity in a season, which the head coach shapes without solely owning.\n",
             "Man coverage is the one that survives, on eleven pairs, so hold it loosely. Built by R/factory/96."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")
save_fig("docs/figures/factory/coach_or_building.png", p, w = 11.5, h = 6.4)
