# =============================================================================
# 43_tip_explainer.R -- what "the look tips run vs pass" means, shown on
# McVay's actual looks.
#
# The ask, verbatim: "How does their formation tip run/pass? Is it 'in 13
# personnel they run it 75% of the time when two tight ends are on the
# right' or what? Need depth there if you can"
#
# So this chart IS that sentence, computed: McVay's ten most-used pre-snap
# looks (personnel x receiver split x QB alignment, the readable version),
# and for each one two numbers:
#   EXPECTED  what the down-and-distance mix on those snaps says the pass
#             rate "should" be (league pass rate per bucket, weighted by
#             when he shows the look; same seven buckets as R/14/R/25)
#   ACTUAL    what he actually calls out of that look
# The gap between them is the tip. No model, no fitting: counting, twice.
#
# The board's leak axis (R/25) runs this same exercise with a finer picture
# (every skill player's alignment) and shrinkage for rare looks; this
# explainer uses the coarse readable look so each row can be said out loud.
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season spans ("2022-23 through 2025-26"), plain language only.
#
# Out: docs/figures/tip_explainer.png
#      data/derived/tip_explainer.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R")]
d[, dd_bucket := fifelse(down == 1, "1st-and-10",
                 fifelse(down == 2 & distance <= 3, "2nd-short",
                 fifelse(down == 2 & distance <= 7, "2nd-medium",
                 fifelse(down == 2, "2nd-long",
                 fifelse(down == 3 & distance <= 3, "3rd-short",
                 fifelse(down == 3 & distance <= 7, "3rd-medium",
                 fifelse(down == 3, "3rd-long", NA_character_)))))))]
d <- d[!is.na(dd_bucket)]
d[, is_pass := as.integer(run_pass == "P")]

# league expectation per bucket (all callers, so "what the situation says")
lg <- d[, .(lg_pass = mean(is_pass)), by = dd_bucket]
cat("league pass rate by bucket:\n"); print(lg[order(dd_bucket)])
d <- merge(d, lg, by = "dd_bucket")

# McVay's readable looks
mc <- d[off_caller == "Sean McVay" &
        offensive_personnel_basic %in% c("10","11","12","13","21","22") &
        formation != "" & quarterback_alignment != ""]
mc[, look := sprintf("%s personnel | %s | %s", offensive_personnel_basic,
                     formation, tolower(quarterback_alignment))]

looks <- mc[, .(n = .N, actual = 100 * mean(is_pass),
                expected = 100 * mean(lg_pass)), by = look][n >= 60][order(-n)]
looks <- looks[1:min(10, .N)]
looks[, gap := actual - expected]
cat("\nMcVay top looks (n >= 60):\n"); print(looks)

write_csv(as.data.frame(looks), "data/derived/tip_explainer.csv")

# concrete sentence for the biggest tipper, in the asked-for format
big <- looks[which.max(abs(gap))]
big_txt <- if (big$gap < 0) sprintf(
  "out of %s: he runs it %.0f%% of the time; the situation alone said %.0f%%",
  big$look, 100 - big$actual, 100 - big$expected) else sprintf(
  "out of %s: he throws it %.0f%% of the time; the situation alone said %.0f%%",
  big$look, big$actual, big$expected)
cat("\nheadline example:", big_txt, "\n")

looks[, look_f := factor(look, levels = rev(looks$look))]
seg_col <- fifelse(looks$gap < 0, "#C0504D", "#2B8CBE")

p <- ggplot(looks) +
  geom_segment(aes(x = expected, xend = actual, y = look_f, yend = look_f),
               colour = "grey70", linewidth = 1.1,
               arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  geom_point(aes(expected, look_f), colour = "grey45", size = 3.4) +
  geom_point(aes(actual, look_f), colour = seg_col, size = 4.2) +
  geom_text(aes(actual, look_f,
                label = sprintf("%+.0f", gap)),
            vjust = -1.1, size = 3, fontface = "bold", colour = seg_col) +
  geom_text(aes(pmin(expected, actual) - 2, look_f,
                label = paste0(comma(n), " snaps")),
            hjust = 1, size = 2.7, colour = "grey55") +
  scale_x_continuous(limits = c(10, 100), breaks = seq(20, 100, 20),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "What \"the look tips run vs pass\" means, on McVay's own looks",
       subtitle = paste0(
         "His ten most-used pre-snap looks, 2022-23 through 2025-26 seasons. Grey dot = the pass rate the down-and-distance mix on those snaps predicts.\n",
         "Colored dot = what he actually calls out of that look (blue = throws it more than expected, red = runs it more). The number is the gap in points.\n",
         "The pattern in plain sight: every shotgun look throws more than expected, every under-center look runs more. The tell is mostly the quarterback's feet.\n",
         "The biggest one, said as a sentence: ", big_txt, "."),
       x = "pass rate", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, non-garbage-time",
         "\nNo model anywhere: expected = league pass rate per down-and-distance bucket, weighted by when he shows the look; actual = counting his calls.",
         paste0("\nLook = personnel group x receiver split x QB alignment, the readable version; the board's leak ranking (R/25) runs the same exercise on the finer picture of\n",
                "every skill player's alignment, with rare looks shrunk toward the caller's baseline so five-snap formations cannot fake a tell. Buckets: 1st-and-10, 2nd and 3rd\n",
                "short/medium/long. Built by R/43."))) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.88), face = "bold"),
        plot.subtitle = element_text(lineheight = 1.15))

save_fig("docs/figures/tip_explainer.png", p, w = 12.5, h = 7.5)
cat("\nOut: docs/figures/tip_explainer.png, data/derived/tip_explainer.csv\n")
