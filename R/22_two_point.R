# =============================================================================
# 22_two_point.R -- does 2-point aggressiveness track 4th-down aggressiveness?
#
# Michael, verbatim: "Do we have anything on 2Pt conversions? Going for 2."
# Nick, verbatim: "Correlates really high with 4th down aggressiveness so can
# probably combine them to an extent. Like no one is aggressive on 4th down
# and conservative on 2pt conversions. But I can test that actually."
#
# This tests Nick's claim instead of assuming it: does a coach's rate of
# going for 2 IN THE SPOTS WHERE THE ANALYTICS SAY GO correlate with his rate
# of going for it on fourth down in the spots R/19 already calls clear-cut?
# And is the quadrant Nick says is empty -- brave on 4th, cowardly on 2pt --
# actually empty?
#
# THE CHART. A 2-point try has a known right answer at specific score
# margins, the same way fourth down has nfl4th. The margins below are the
# standard "go for two" chart most public analytics writers converge on
# (Kevin Cole / rbsdm.com's win-probability-based 2-point chart, matching the
# older Chase Stuart / Football Perspective version): by PRE-TRY score
# margin, posteam perspective, i.e. the score right after the touchdown and
# before the extra-point or two-point try:
#   trailing by 2  -> go (ties the game outright if it converts)
#   trailing by 5  -> go (a made field goal later ties instead of just
#                         cutting the deficit to one score)
#   trailing by 10 -> go (keeps a one-score-plus-two comeback alive)
#   leading by 1   -> go (buys a full field goal of cushion instead of one
#                         point; most public charts call this a go)
# nflverse's own try-play row already carries this pre-try number in
# score_differential (checked by hand: a 6-0 posteam touchdown carries
# score_differential = 6 on the following extra-point row), so no
# reconstruction from the preceding play is needed.
#
# TRAILING BY 8 LATE is the one cell where public charts genuinely disagree
# (tie it now vs. take a one-score lead later), so it is tracked separately
# (score_differential == -8, qtr >= 4) and never folded into "obeys the
# card." League-wide it converts at 31%, close to a coin flip, which is what
# a genuinely contested cell should look like.
#
# 4TH-QUARTER WEIGHTING. The four go-states above hold at any point in the
# game by the underlying win-probability math, but real coaches only actually
# play them that way late: league-wide obey rate in those four states is 99.6%
# in the 4th quarter/OT and just 48.6% in the first three quarters (both
# printed below). Restricting the decision universe to qtr >= 4 only would
# leave a single coach with 10+ qualifying tries in 2018-2025, so it stays a
# reported split rather than the main cut, but it means "obeys the card" is
# an easier bar the later a coach's 2-point tries happen to land, and that
# gets flagged again on the chart.
#
# ERA ADJUSTMENT AND SHRINKAGE reuse R/19's exact method: rate compared to the
# league in the coach's own seasons, then empirical-Bayes shrunk toward zero,
# because 2-point tries are rare (a handful per team-season) and a raw
# per-coach rate on a few dozen decisions is mostly sampling noise.
#
# DATA. nflverse play-by-play, 2018-2025 (same window as R/19). Coach
# attached via nfl-analysis/data/games.csv home_coach/away_coach, same join
# R/19 uses. The fourth-down side of the correlation test reuses R/19's own
# output (data/derived/brave_cowardly.csv) rather than recomputing it.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/two_point.png
#      data/derived/two_point.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
SEASONS <- 2018:2025
CHART_GO   <- c(-2, -5, -10, 1)   # trail 2, trail 5, trail 10, lead 1
MIN_CHART   <- 10   # chart-says-go tries required (Nick's own stated bar)
MIN_OVERALL <- 20   # total post-TD tries required for the overall-rate board
MIN_FOURTH  <- 20   # R/19's own clear-cut fourth-down bar

# ---------------------------------------------------------------- load + filter
pbp_cols <- c("game_id", "season", "posteam", "qtr", "game_seconds_remaining",
              "extra_point_attempt", "two_point_attempt", "extra_point_result",
              "two_point_conv_result", "score_differential")
pbp <- rbindlist(lapply(SEASONS, function(yr) {
  fread(file.path(NFLA, "data", sprintf("play_by_play_%d.csv.gz", yr)),
        select = pbp_cols, showProgress = FALSE)
}))

tries <- pbp[(extra_point_attempt == 1 | two_point_attempt == 1) & !is.na(score_differential)]
stopifnot(nrow(tries[extra_point_attempt == 1 & two_point_attempt == 1]) == 0)
tries[, went_two := as.integer(two_point_attempt == 1)]
tries[, chart_go := score_differential %in% CHART_GO]
tries[, debatable_down8_late := score_differential == -8 & qtr >= 4]

cat(sprintf("post-touchdown tries, %d-%d: %s across %d seasons\n",
            min(SEASONS), max(SEASONS), format(nrow(tries), big.mark = ","), uniqueN(tries$season)))

gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id", "home_team", "home_coach", "away_coach"), showProgress = FALSE)
tries <- merge(tries, gm, by = "game_id", all.x = TRUE)
tries[, coach := ifelse(posteam == home_team, home_coach, away_coach)]
tries <- tries[!is.na(coach) & coach != ""]

# ---------------------------------------------------------------- league rates (the era effect)
cat("\n--- league 2-point rate by season, all post-TD tries ---\n")
lg_overall <- tries[, .(n = .N, two = sum(went_two)), by = season][, rate := 100 * two / n]
print(lg_overall[order(season), .(season, n, rate = round(rate, 1))])

cat("\n--- league rate in chart-says-go states, by season ---\n")
lg_chart <- tries[chart_go == TRUE, .(n = .N, two = sum(went_two)), by = season][, rate := 100 * two / n]
print(lg_chart[order(season), .(season, n, rate = round(rate, 1))])

cat(sprintf("\nchart-says-go states (trail 2, trail 5, trail 10, lead 1): %s tries, %.1f%% obeyed league-wide\n",
            format(nrow(tries[chart_go == TRUE]), big.mark = ","), 100 * mean(tries[chart_go == TRUE]$went_two)))
print(tries[chart_go == TRUE, .(n = .N, obey_rate = round(100 * mean(went_two), 1)), by = score_differential][order(-score_differential)])

cat(sprintf("\n4th-quarter weighting check: obey rate in those same states is %.1f%% in Q4/OT (n=%s) vs %.1f%% in Q1-Q3 (n=%s)\n",
            100 * mean(tries[chart_go == TRUE & qtr >= 4]$went_two), format(nrow(tries[chart_go == TRUE & qtr >= 4]), big.mark = ","),
            100 * mean(tries[chart_go == TRUE & qtr < 4]$went_two), format(nrow(tries[chart_go == TRUE & qtr < 4]), big.mark = ",")))
cat("restricting the universe to qtr >= 4 only leaves too few decisions per coach (max 13, only 1 coach clears 10),\n")
cat("so the main metric below pools all quarters; treat 'obeys the card' as an easier bar for coaches whose\n")
cat("chart-says-go tries skew early in games.\n")

cat(sprintf("\ntrailing by 8 late (qtr>=4), the contested cell, tracked separately: %d tries, %.1f%% went for 2 (close to a coin flip, as a genuinely debated cell should be)\n",
            nrow(tries[debatable_down8_late == TRUE]), 100 * mean(tries[debatable_down8_late == TRUE]$went_two)))

# ---------------------------------------------------------------- era-adjust + shrink (R/19's method)
shrink_rate <- function(try_dt, lg_dt, min_n) {
  cs <- try_dt[, .(n = .N, two = sum(went_two)), by = .(coach, season)]
  cs <- merge(cs, lg_dt[, .(season, lg_rate = rate)], by = "season")
  dc <- cs[, .(n = sum(n), two = sum(two), expected_two = sum(lg_rate / 100 * n)), by = coach]
  spans <- try_dt[, .(first_season = min(season), last_season = max(season)), by = coach]
  dc <- merge(dc, spans, by = "coach")
  dc[, `:=`(rate = 100 * two / n, expected_rate = 100 * expected_two / n)]
  dc[, era_adj := rate - expected_rate]
  dc[, se := 100 * sqrt((rate / 100) * (1 - rate / 100) / n)]
  dc[, `:=`(lo = era_adj - 1.96 * se, hi = era_adj + 1.96 * se)]
  dc <- dc[n >= min_n]
  tau2 <- max(var(dc$era_adj) - mean(dc$se^2), 1e-4)
  dc[, era_shrunk := (tau2 * era_adj) / (tau2 + se^2)]
  setorder(dc, -era_shrunk)
  dc[]
}

dc_overall <- shrink_rate(tries, lg_overall, MIN_OVERALL)
dc_chart   <- shrink_rate(tries[chart_go == TRUE], lg_chart, MIN_CHART)

cat(sprintf("\noverall 2pt-rate board: %d coaches with >= %d total post-TD tries\n", nrow(dc_overall), MIN_OVERALL))
cat(sprintf("chart-says-go board: %d coaches with >= %d chart-says-go tries\n", nrow(dc_chart), MIN_CHART))

cat("\n=== top 10, chart-says-go rate (era-shrunk, most likely to obey the card) ===\n")
print(head(dc_chart[order(-era_shrunk), .(coach, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                     ci = sprintf("[%.1f, %.1f]", lo, hi), era_shrunk = round(era_shrunk, 1))], 10))
cat("\n=== bottom 10, chart-says-go rate (era-shrunk, most likely to kick anyway) ===\n")
print(head(dc_chart[order(era_shrunk), .(coach, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                     ci = sprintf("[%.1f, %.1f]", lo, hi), era_shrunk = round(era_shrunk, 1))], 10))

# ---------------------------------------------------------------- reliability check
# R/19's own method: is there even between-coach signal here before any rank
# gets read as a trait. Random within-coach half split (most coaches' tenures
# here don't span enough seasons for a season split).
reliability_check <- function(try_dt, coaches, min_half_n, seed = 11) {
  set.seed(seed)
  hh <- try_dt[coach %in% coaches][, half := sample(rep_len(1:2, .N)), by = coach]
  sh <- dcast(hh[, .(v = 100 * mean(went_two), n = .N), by = .(coach, half)][n >= min_half_n],
              coach ~ half, value.var = "v")
  setnames(sh, c("coach", "h1", "h2"))
  sh <- sh[!is.na(h1) & !is.na(h2)]
  r_half <- cor(sh$h1, sh$h2)
  list(n = nrow(sh), r_half = r_half, r_sb = 2 * r_half / (1 + r_half))
}
rel_chart   <- reliability_check(tries[chart_go == TRUE], dc_chart$coach, min_half_n = 4)
rel_overall <- reliability_check(tries, dc_overall$coach, min_half_n = 8)

n_sep_chart <- dc_chart[lo > 0 | hi < 0, .N]
cat(sprintf("\nchart-says-go rate reliability: split-half r = %.2f, Spearman-Brown %.2f (n = %d coaches)\n",
            rel_chart$r_half, rel_chart$r_sb, rel_chart$n))
cat(sprintf("tau (between-coach spread after removing sampling noise) = %.2fpp; %d of %d coaches (%.0f%%) separable from their own era average at 95%% confidence\n",
            sqrt(max(var(dc_chart$era_adj) - mean(dc_chart$se^2), 0)), n_sep_chart, nrow(dc_chart), 100 * n_sep_chart / nrow(dc_chart)))
cat("negative split-half means the between-coach spread in the raw rate is fully explained by sampling noise on\n")
cat("10-19 tries per coach. Against this project's own r >= 0.30 trait bar, chart-obedience does NOT clear it,\n")
cat("which is why the EB-shrunk estimate above collapses every coach to ~0 -- there is nothing left to shrink.\n")

n_sep_overall <- dc_overall[lo > 0 | hi < 0, .N]
cat(sprintf("\noverall 2pt-rate reliability: split-half r = %.2f, Spearman-Brown %.2f (n = %d coaches)\n",
            rel_overall$r_half, rel_overall$r_sb, rel_overall$n))
cat(sprintf("tau = %.2fpp; %d of %d coaches (%.0f%%) separable from their own era average at 95%% confidence\n",
            sqrt(max(var(dc_overall$era_adj) - mean(dc_overall$se^2), 0)), n_sep_overall, nrow(dc_overall), 100 * n_sep_overall / nrow(dc_overall)))
cat("same 0.30 bar: short, but the same order of magnitude as R/19's own fourth-down decision-cost ledger\n")
cat("(0.21 to 0.26), i.e. a real if modest signal, not noise.\n")

# ---------------------------------------------------------------- Nick's test
fourth <- as.data.table(read_csv("data/derived/brave_cowardly.csv", show_col_types = FALSE))

cat("\n=== Nick's test, exactly as asked: chart-says-go rate (EB-shrunk) vs 4th-down bravery (EB-shrunk, R/19) ===\n")
merged <- merge(dc_chart[n >= MIN_CHART, .(coach, chart_n = n, chart_rate = rate,
                                            chart_era_adj = era_adj, chart_shrunk = era_shrunk)],
                fourth[decisions >= MIN_FOURTH, .(coach, fourth_n = decisions, fourth_go_rate = go_rate,
                                                   fourth_era_adj = era_adj, fourth_shrunk = era_shrunk)],
                by = "coach")
ct_chart <- cor.test(merged$chart_shrunk, merged$fourth_shrunk)
cat(sprintf("n = %d coaches (>= %d chart-says-go tries, >= %d clear-cut fourth downs)\n", nrow(merged), MIN_CHART, MIN_FOURTH))
cat(sprintf("r = %.2f, 95%% CI [%.2f, %.2f], p = %.3f\n", ct_chart$estimate, ct_chart$conf.int[1], ct_chart$conf.int[2], ct_chart$p.value))
cat("this number is not a real test of the hypothesis: chart-says-go failed its own reliability check above, so\n")
cat("its EB-shrunk estimate is ~0 for all 23 coaches by construction. Correlating a variable with no detectable\n")
cat("signal against anything necessarily returns something close to zero -- a null computed on noise, the same\n")
cat("issue this project's own GOAL.md already flags for the G5 test. It is inconclusive, not a finding against\n")
cat("Nick's hunch.\n")

cat("\n=== robustness check 1: RAW (unshrunk) chart-says-go rate vs 4th-down bravery ===\n")
ct_raw <- cor.test(merged$chart_era_adj, merged$fourth_shrunk)
cat(sprintf("r = %.2f, 95%% CI [%.2f, %.2f], p = %.3f, n = %d -- still null, and the raw point estimate itself is unreliable per the split-half above\n",
            ct_raw$estimate, ct_raw$conf.int[1], ct_raw$conf.int[2], ct_raw$p.value, nrow(merged)))

cat("\n=== robustness check 2, the best-powered version: overall 2pt attempt rate (EB-shrunk, any score margin) vs 4th-down bravery ===\n")
merged_overall <- merge(dc_overall[n >= MIN_OVERALL, .(coach, overall_n = n, overall_rate = rate,
                                                         overall_era_adj = era_adj, overall_shrunk = era_shrunk)],
                        fourth[decisions >= MIN_FOURTH, .(coach, fourth_n = decisions, fourth_go_rate = go_rate,
                                                            fourth_era_adj = era_adj, fourth_shrunk = era_shrunk)],
                        by = "coach")
ct_overall <- cor.test(merged_overall$overall_shrunk, merged_overall$fourth_shrunk)
cat(sprintf("n = %d coaches (>= %d total post-TD tries, >= %d clear-cut fourth downs)\n", nrow(merged_overall), MIN_OVERALL, MIN_FOURTH))
cat(sprintf("r = %.2f, 95%% CI [%.2f, %.2f], p = %.3f\n", ct_overall$estimate, ct_overall$conf.int[1], ct_overall$conf.int[2], ct_overall$p.value))
cat("this is the version with actual, if modest, between-coach signal (reliability check above), and it is the\n")
cat("closer match to what Nick literally said ('aggressive on 2pt conversions' in general, not specifically\n")
cat("obeying a win-probability chart). Weak positive point estimate; the confidence interval rules out anything\n")
cat("close to 'really high' (upper bound 0.35).\n")

# ---------------------------------------------------------------- quadrants
# Built on the overall-rate x 4th-down pairing, the only one of the three
# tests above where both axes carry real between-coach signal.
merged_overall[, quadrant := fifelse(fourth_shrunk >= 0 & overall_shrunk >= 0, "brave both",
                             fifelse(fourth_shrunk < 0 & overall_shrunk < 0, "cowardly both",
                             fifelse(fourth_shrunk >= 0 & overall_shrunk < 0,
                                     "brave on 4th, cowardly on 2pt (Nick says this cell is empty)",
                                     "cowardly on 4th, brave on 2pt")))]
cat("\n--- quadrant counts, overall 2pt rate x 4th-down bravery (sign of each era-shrunk estimate) ---\n")
print(merged_overall[, .N, by = quadrant][order(-N)])

violators <- merged_overall[quadrant %like% "Nick says this cell is empty"]
cat(sprintf("\n%d coach(es) in the cell Nick says is empty (brave on 4th, cowardly on 2pt):\n", nrow(violators)))
if (nrow(violators) > 0) {
  print(violators[, .(coach, fourth_n, fourth_shrunk = round(fourth_shrunk, 1),
                       overall_n, overall_shrunk = round(overall_shrunk, 1))])
} else {
  cat("none. Nick's specific claim holds in this data.\n")
}

flip <- merged_overall[quadrant == "cowardly on 4th, brave on 2pt"]
cat(sprintf("\n%d coach(es) in the mirror cell (cowardly on 4th, brave on 2pt):\n", nrow(flip)))
if (nrow(flip) > 0) {
  print(flip[, .(coach, fourth_n, fourth_shrunk = round(fourth_shrunk, 1),
                  overall_n, overall_shrunk = round(overall_shrunk, 1))])
}

# ---------------------------------------------------------------- write derived CSV
rename_prefixed <- function(dc, prefix) {
  old <- setdiff(names(dc), "coach")
  setnames(dc, old, paste0(prefix, old))
  dc
}
dc_overall_out <- rename_prefixed(copy(dc_overall), "overall_")
dc_chart_out   <- rename_prefixed(copy(dc_chart), "chart_")

out <- merge(dc_overall_out, dc_chart_out, by = "coach", all = TRUE)
setorder(out, -chart_era_shrunk, na.last = TRUE)
write_csv(as.data.frame(out[, .(coach,
             overall_n, overall_rate = round(overall_rate, 1),
             overall_era_adj = round(overall_era_adj, 2), overall_era_shrunk = round(overall_era_shrunk, 2),
             overall_first_season, overall_last_season,
             chart_n, chart_rate = round(chart_rate, 1),
             chart_era_adj = round(chart_era_adj, 2), chart_lo = round(chart_lo, 2), chart_hi = round(chart_hi, 2),
             chart_era_shrunk = round(chart_era_shrunk, 2),
             chart_first_season, chart_last_season)]),
          "data/derived/two_point.csv")
cat("\nwrote data/derived/two_point.csv\n")

# ---------------------------------------------------------------- chart
# Plots the overall 2pt-rate pairing, not the literal chart-says-go pairing:
# it's the only one of the three tests above where the y-axis carries real
# between-coach signal (chart-says-go's own split-half reliability is
# negative -- see console output -- so a chart built on it would just be a
# flat line at y = 0 for all 23 coaches, which isn't a chart, it's a bug
# report). The chart-says-go r is still stated in the caption for the record.
rule_break <- merged_overall[quadrant != "brave both" & quadrant != "cowardly both"]

# All 27 off-diagonal coaches get colored orange below (that's the actual
# finding: with r this weak, "rule breaker" isn't a small exception list),
# but only a legible subset gets a text label: the axis extremes plus a few
# recognizable names sitting in the cell Nick specifically said was empty.
NAMED <- unique(c(
  head(merged_overall[order(-fourth_shrunk)], 2)$coach,
  head(merged_overall[order(fourth_shrunk)], 2)$coach,
  head(merged_overall[order(-overall_shrunk)], 2)$coach,
  head(merged_overall[order(overall_shrunk)], 2)$coach,
  "Josh McDaniels",
  intersect(c("Andy Reid", "John Harbaugh", "Dan Campbell", "Sean McDermott"), violators$coach)
))
lab <- merged_overall[coach %in% NAMED]

p <- ggplot(merged_overall, aes(fourth_shrunk, overall_shrunk)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", size = 2.8, alpha = 0.8) +
  geom_point(data = rule_break, colour = "#D55E00", size = 3.6) +
  geom_text_repel(data = lab, aes(label = coach), size = 3.1, fontface = "bold",
                   colour = "grey20", seed = 4, box.padding = 0.55, min.segment.length = 0, max.overlaps = 25) +
  annotate("text", x = Inf, y = Inf, hjust = 1.05, vjust = 1.4, size = 2.9, fontface = "bold",
           colour = "grey45", label = "brave on 4th,\nbrave on 2pt") +
  annotate("text", x = -Inf, y = -Inf, hjust = -0.08, vjust = -0.6, size = 2.9, fontface = "bold",
           colour = "grey45", label = "cowardly on 4th,\ncowardly on 2pt") +
  annotate("text", x = -Inf, y = Inf, hjust = -0.08, vjust = 1.4, size = 2.9, fontface = "bold",
           colour = "grey45", label = "cowardly on 4th,\nbrave on 2pt") +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.6, size = 2.9, fontface = "bold",
           colour = "#8a3d00", label = "brave on 4th,\ncowardly on 2pt\n(predicted empty)") +
  labs(
    title = sprintf("The two braveries barely correlate: r = %.2f between 4th-down and 2pt aggressiveness (n = %d coaches)",
                     ct_overall$estimate, nrow(merged_overall)),
    subtitle = sprintf("Each dot a head coach, 2018 to 2025. X: era-shrunk go rate on R/19's clear-cut fourth downs. Y: era-shrunk overall 2-point attempt rate (any score margin).\n95%% CI on r: [%.2f, %.2f], p = %.2f. %d coach(es) (orange) sit in an off-diagonal quadrant.",
                       ct_overall$conf.int[1], ct_overall$conf.int[2], ct_overall$p.value, nrow(rule_break)),
    x = "4th-down bravery, era-shrunk go rate on clear-cut fourth downs (R/19)", y = "2pt aggressiveness, era-shrunk overall attempt rate",
    caption = fig_caption(
      "nflverse play-by-play, 2018 to 2025; R/19's fourth-down board (data/derived/brave_cowardly.csv)",
      sprintf("%d coaches with >= %d total post-touchdown tries and >= %d clear-cut fourth downs.",
              nrow(merged_overall), MIN_OVERALL, MIN_FOURTH),
      paste0("This is the best-powered of three cuts tried (see data/derived/two_point.csv and console output). The literal test Nick proposed -- rate of obeying the\n",
             "score-margin go-for-2 chart specifically -- comes back r = -0.01 on n = 23, but that metric's own split-half reliability is negative (2-point tries are too\n",
             "rare, 10-19 per coach, for it to clear this project's r >= 0.30 trait bar), so that number is a null computed on noise, not a real test. This chart plots\n",
             "the version that does carry a modest real signal (Spearman-Brown ~0.28) instead: overall willingness to go for 2 at all. Built by R/22.")
    )
  ) +
  theme_coach(grid = "none") +
  theme(axis.title = element_text(size = rel(0.8)))

save_fig("docs/figures/two_point.png", p, w = 12, h = 7.6)
