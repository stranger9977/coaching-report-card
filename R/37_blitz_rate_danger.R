# =============================================================================
# 37_blitz_rate_danger.R -- blitz rate vs blitz danger, one point per DC.
#
# Michael, verbatim, reacting to R/28's free-rusher finding: "I think you
# should have some way of visualizing this with blitz rate. Might be a
# second chart. But the fact that he does that without blitzing (I think he
# is really low on that category)."
#
# THE ASK IN ONE CHART. x = how often a DC blitzes (raw rate -- the
# intuitive axis Michael asked for). y = how much it costs the offense when
# he does (EPA allowed per blitz, offense-signed, so LOW is dangerous for
# the defense). Point size = how many blitzes that rests on. The corner
# Michael is describing -- rare blitzes that land -- is bottom-left.
#
# RECONCILING WITH WHAT THIS BOARD ALREADY FOUND:
#   R/16  Situation-adjusted: Macdonald blitzes below what his own
#         situations call for in ALL SIX game-context cuts. His raw rate
#         here is the same story on an easier-to-read axis; the caption
#         below carries free_rushers.csv's situation-adjusted blitz rank
#         (built by R/28's sumer_resid()) so the raw picture isn't read
#         alone.
#   R/28  Decomposed pressure into scheme (unblocked_pressure, a free
#         rusher nobody blocked) vs talent (pass_rush_win). Macdonald ranks
#         #2 of 41 on scheme, league-average on talent, while blitzing
#         below expected -- his pressure is manufactured, not sent. This
#         script's enrichment reuses that scheme rank directly so the
#         chart shows WHY the rare-but-dangerous corner works.
#   R/34  Team-level: Seattle's blitzes allowed the LEAST offensive EPA of
#         any defense in the NFL, 2024-2025 pooled (#1 of 32), and a
#         Seattle blitz springs a free rusher far more often than the
#         league's blitzes do. This script is the DC-career-level version
#         of that same reading, built from the same universe as R/28.
#
# DATA. SumerSports play charting via R/factory/lib_sumer.R's load_sumer(),
# 2022-2025, non-garbage-time dropbacks, DC attributed -- identical universe
# to R/28, so this joins to data/derived/free_rushers.csv (scheme rank,
# situation-adjusted blitz rank) by def_play_caller with no re-derivation.
# DCs need 700+ charted dropbacks (R/16's bar); 41 qualify.
#
# METHOD. Blitz rate is the RAW per-DC rate on purpose (Michael's own
# framing, "he is really low on that category" -- a plain rate, not a
# residual). EPA allowed per blitz is a plain mean with a t-test CI on the
# DC's own blitz plays (hundreds per DC, not thousands, so the labeled
# names carry a whisker). The blitz-rate-vs-danger correlation across DCs
# is an unweighted Pearson correlation on the 41 career points, an aside on
# whether blitzing more is self-defeating, not a proof of anything about
# the situation-adjusted decomposition R/16 and R/28 already did.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/blitz_rate_danger.png
#      data/derived/blitz_rate_danger.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
MIN_CAREER <- 700  # matches R/16 and R/28's bar
NAMED_DCS <- c("Mike Macdonald", "Vic Fangio", "Brian Flores", "Steve Spagnuolo")

# =============================================================================
# 1. LOAD, SAME UNIVERSE AS R/28: dropbacks, non-garbage-time, DC-attributed
# =============================================================================

plays <- load_sumer(SEASONS)
d <- plays[is_dropback == TRUE & no_play == FALSE & garbage_time == FALSE & def_caller != ""]
cat(sprintf("Sumer dropbacks %s-%s, non-garbage-time, DC-attributed: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(d), big.mark = ",")))

career_n <- d[, .N, by = def_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_caller]
d2 <- d[def_caller %in% dcs_ok]
cat(sprintf("DCs with >= %d charted dropbacks, %d-%d: %d\n",
            MIN_CAREER, min(SEASONS), max(SEASONS), length(dcs_ok)))

# =============================================================================
# 2. PER-DC: raw blitz rate, EPA allowed per blitz (with CI)
# =============================================================================

blitz_rate <- d2[, .(n_dropbacks = .N, n_blitz = sum(blitz), blitz_rate = mean(blitz)), by = def_caller]

epa_blitz <- d2[blitz == TRUE, {
  tt <- t.test(expected_points_added)
  list(epa_per_blitz = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2])
}, by = def_caller]

dc <- merge(blitz_rate, epa_blitz, by = "def_caller")
setorder(dc, blitz_rate); dc[, rate_rank := .I]                # 1 = fewest blitzes
setorder(dc, epa_per_blitz); dc[, danger_rank := .I]            # 1 = lowest EPA allowed = most dangerous
setorder(dc, def_caller)
n_dcs <- nrow(dc)
cat(sprintf("league raw blitz rate: median %.1f%%, range %.1f%%-%.1f%%\n",
            100 * median(dc$blitz_rate), 100 * min(dc$blitz_rate), 100 * max(dc$blitz_rate)))
cat(sprintf("league EPA allowed per blitz: median %+.3f, range %+.3f to %+.3f\n",
            median(dc$epa_per_blitz), min(dc$epa_per_blitz), max(dc$epa_per_blitz)))

# =============================================================================
# 3. JOIN R/28's scheme rank + situation-adjusted blitz rank (no re-derivation)
# =============================================================================

fr <- fread("data/derived/free_rushers.csv")
fr_cols <- fr[, .(def_caller = def_play_caller, unblocked_rank, unblocked_resid,
                  situ_blitz_rank = blitz_rank, situ_blitz_resid = blitz_resid)]
dc <- merge(dc, fr_cols, by = "def_caller", all.x = TRUE)
cat(sprintf("joined free_rushers.csv scheme rank: %d of %d DCs matched\n",
            sum(!is.na(dc$unblocked_rank)), n_dcs))

# =============================================================================
# 4. THE CORRELATION ASIDE: does blitzing more make your blitzes worse,
# better, or neither?
# =============================================================================

ct <- cor.test(dc$blitz_rate, dc$epa_per_blitz)
cat(sprintf("\n--- BLITZ RATE vs BLITZ DANGER, across %d DCs ---\n", n_dcs))
cat(sprintf("cor(blitz rate, EPA allowed per blitz) = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n",
            ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value))
rate_danger_verdict <- if (ct$p.value > 0.10) {
  "NULL RESULT: no detectable relationship. Blitzing more is not systematically self-defeating or self-reinforcing across DCs -- volume and payoff are close to independent."
} else if (ct$estimate > 0) {
  "blitzing MORE is associated with WORSE (higher) EPA allowed per blitz -- diminishing returns, consistent with a defense tipping its hand the more it blitzes."
} else {
  "blitzing MORE is associated with BETTER (lower) EPA allowed per blitz -- no diminishing returns in this sample."
}
cat("verdict:", rate_danger_verdict, "\n")

# =============================================================================
# 5. QUADRANTS (median splits) + LABELS
# =============================================================================

mx <- median(dc$blitz_rate); my <- median(dc$epa_per_blitz)
dc[, quadrant := fcase(
  blitz_rate < mx & epa_per_blitz < my, "Blitzes rarely, and it lands",
  blitz_rate < mx & epa_per_blitz >= my, "Blitzes rarely, and it doesn't land",
  blitz_rate >= mx & epa_per_blitz < my, "Blitzes often, and it lands",
  blitz_rate >= mx & epa_per_blitz >= my, "Blitzes often, and it doesn't land"
)]
corner_pop <- dc[, .N, by = quadrant]
cat(sprintf("\n--- QUADRANT POPULATIONS (median blitz rate %.1f%%, median EPA/blitz %+.3f) ---\n", 100 * mx, my))
print(corner_pop)

rare_dangerous <- dc[quadrant == "Blitzes rarely, and it lands"][order(epa_per_blitz)]
cat("\nDCs in the rare-but-dangerous corner (Macdonald's corner), most dangerous first:\n")
print(rare_dangerous[, .(def_caller, blitz_rate = round(100 * blitz_rate, 1),
                         epa_per_blitz = round(epa_per_blitz, 3), n_blitz, danger_rank, rate_rank)])

# named DCs + notable outliers (extremes not already named)
present_named <- intersect(NAMED_DCS, dc$def_caller)
missing_named <- setdiff(NAMED_DCS, dc$def_caller)
if (length(missing_named)) cat(sprintf("\nnot in the %d-DC sample at %d+ dropbacks: %s\n",
                                       n_dcs, MIN_CAREER, paste(missing_named, collapse = ", ")))

outlier_ids <- unique(c(
  dc[rate_rank == 1, def_caller], dc[rate_rank == n_dcs, def_caller],
  dc[danger_rank == 1, def_caller], dc[danger_rank == n_dcs, def_caller]
))
outlier_ids <- setdiff(outlier_ids, present_named)
dc[, hl := def_caller %in% present_named]
dc[, ol := def_caller %in% outlier_ids]

cat(sprintf("\nnamed DCs: %s\n", paste(present_named, collapse = ", ")))
cat(sprintf("notable outliers (extremes not already named): %s\n", paste(outlier_ids, collapse = ", ")))

# =============================================================================
# 6. THE MACDONALD ANSWER, DIRECTLY
# =============================================================================

mac <- dc[def_caller == "Mike Macdonald"]
cat(sprintf(paste0(
  "\n=== MACDONALD'S COORDINATES ===\n",
  "Blitz rate:            %.1f%% actual, rank #%d of %d (1 = fewest).\n",
  "  Situation-adjusted (R/28's residual): %+.2fpp vs expected, rank #%d of %d (1 = blitzes most above expected).\n",
  "EPA allowed per blitz: %+.3f [%+.3f, %+.3f] 95%% CI, rank #%d of %d (1 = most dangerous).\n",
  "n blitzes backing that number: %d (of %d dropbacks).\n",
  "Scheme rank (R/28, unblocked-pressure residual): #%d of %d.\n",
  "Quadrant: %s.\n"),
  100 * mac$blitz_rate, mac$rate_rank, n_dcs,
  mac$situ_blitz_resid, mac$situ_blitz_rank, n_dcs,
  mac$epa_per_blitz, mac$lo, mac$hi, mac$danger_rank, n_dcs,
  mac$n_blitz, mac$n_dropbacks,
  mac$unblocked_rank, n_dcs,
  mac$quadrant))

# =============================================================================
# 7. CHART
# =============================================================================

dc[, scheme_rank_inv := (n_dcs + 1) - unblocked_rank]  # so darker = better scheme rank
inv_breaks <- (n_dcs + 1) - c(1, 10, 20, 30, n_dcs)

lab_named <- dc[hl == TRUE]
lab_out   <- dc[ol == TRUE]
lab_all   <- rbind(lab_named, lab_out)

# most named DCs sit in a crowded cluster where ggrepel has room to push labels
# away from each other; Brian Flores sits alone out at ~46% blitz rate with
# nothing nearby to repel against, so give him an explicit nudge off the point
lab_named[, `:=`(nudge_x = 0, nudge_y = 0)]
lab_named[def_caller == "Brian Flores", `:=`(nudge_x = 1.2, nudge_y = -0.06)]

corner_txt <- function(x, y, hj, vj, txt) annotate("text", x = x, y = y, hjust = hj, vjust = vj,
                                                    size = 2.95, fontface = "italic", colour = "grey40", label = txt)
xr <- range(dc$blitz_rate); yr <- range(c(dc$lo, dc$hi))

p <- ggplot(dc, aes(100 * blitz_rate, epa_per_blitz)) +
  geom_vline(xintercept = 100 * mx, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_hline(yintercept = my, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_errorbar(data = lab_all, aes(ymin = lo, ymax = hi), width = 0, colour = "grey50", linewidth = 0.5) +
  geom_point(data = dc[hl == FALSE & ol == FALSE],
             aes(size = n_blitz, fill = scheme_rank_inv), shape = 21, colour = "white", stroke = 0.2, alpha = 0.85) +
  geom_point(data = lab_out, aes(size = n_blitz, fill = scheme_rank_inv), shape = 21, colour = "grey15", stroke = 1.1) +
  geom_point(data = lab_named, aes(size = n_blitz, fill = scheme_rank_inv), shape = 21, colour = "#D55E00", stroke = 1.6) +
  geom_text_repel(data = lab_named, aes(label = sprintf("%s\nscheme #%d, blitz rate #%d", def_caller, unblocked_rank, rate_rank)),
                   nudge_x = lab_named$nudge_x, nudge_y = lab_named$nudge_y,
                   size = 3.0, fontface = "bold", lineheight = 0.95, colour = "#8a3d00",
                   seed = 9, box.padding = 0.8, point.padding = 0.7, min.segment.length = 0, max.overlaps = 30) +
  geom_text_repel(data = lab_out, aes(label = def_caller), size = 2.85, fontface = "bold",
                   colour = "grey20", seed = 6, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  corner_txt(xr[1] * 100, yr[1], 0, 0, "Blitzes rarely,\nand it lands") +
  corner_txt(xr[1] * 100, yr[2], 0, 1, "Blitzes rarely,\nand it doesn't land") +
  corner_txt(xr[2] * 100, yr[1], 1, 0, "Blitzes often,\nand it lands") +
  corner_txt(xr[2] * 100, yr[2], 1, 1, "Blitzes often,\nand it doesn't land") +
  scale_size_continuous(range = c(2, 11), guide = "none") +
  scale_fill_gradient(low = pal_era[["1990s"]], high = pal_era[["2020s"]],
                      breaks = inv_breaks, labels = c("1 (top)", "10", "20", "30", sprintf("%d (bottom)", n_dcs)),
                      name = "Scheme rank\n(unblocked-pressure, R/28)") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), sprintf("%.2f", x))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Macdonald blitzes less than almost anyone in the league, and it is one of the most dangerous blitzes when he does",
    subtitle = sprintf("Each point is one DC's career, %d-%d (%d+ charted dropbacks, n = %d). Point size = number of blitzes. Colour = scheme rank (darker = manufactures\nmore free rushers, R/28). Lower on the y-axis is worse for the offense, i.e. more dangerous for the defense.",
                       min(SEASONS), max(SEASONS), MIN_CAREER, n_dcs),
    x = "blitz rate (raw, share of dropbacks)", y = "EPA allowed per blitz (offense-signed; lower = more dangerous)",
    caption = fig_caption(
      "SumerSports play charting 2022-2025 (blitz = 5+ pass rushers), data/derived/free_rushers.csv (R/28)",
      sprintf("%d DCs, %d+ charted dropbacks each, %s dropbacks total. Whiskers are 95%% t-test CIs, shown on labeled DCs only.",
              n_dcs, MIN_CAREER, format(nrow(d2), big.mark = ",")),
      sprintf(paste0(
        "\nBlitz rate is the RAW per-DC rate (the intuitive axis, as asked); R/16 and R/28's situation-adjusted residual agrees Macdonald blitzes below what his own situations call\n",
        "for in all six game-context cuts (rank #%d of %d on that adjusted scale too). Across all %d DCs, blitz rate and blitz danger correlate at r = %+.2f [%+.2f, %+.2f]\n",
        "(p = %.2f): %s Built by R/37."),
        mac$situ_blitz_rank, n_dcs, n_dcs, ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value,
        sub("^NULL RESULT: ", "", rate_danger_verdict))
    )
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "right",
        legend.title = element_text(size = rel(0.72), colour = ink_body),
        legend.text = element_text(size = rel(0.68), colour = ink_body),
        plot.margin = margin(10, 16, 8, 10))
save_fig("docs/figures/blitz_rate_danger.png", p, w = 13, h = 8)

# =============================================================================
# 8. WRITE OUTPUT
# =============================================================================

out <- dc[, .(def_caller, n_dropbacks, n_blitz, blitz_rate, rate_rank,
             epa_per_blitz, epa_lo = lo, epa_hi = hi, danger_rank,
             unblocked_rank, unblocked_resid, situ_blitz_rank, situ_blitz_resid, quadrant)]
setnames(out, "def_caller", "def_play_caller")
write_csv(as.data.frame(out), "data/derived/blitz_rate_danger.csv")
cat(sprintf("\nwrote data/derived/blitz_rate_danger.csv (%d DCs)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================= SUMMARY =================\n")
cat(sprintf("Universe: %s dropbacks, %d-%d, non-garbage-time, %d DCs with %d+ charted dropbacks.\n",
            format(nrow(d2), big.mark = ","), min(SEASONS), max(SEASONS), n_dcs, MIN_CAREER))
cat(sprintf("Macdonald: blitz rate %.1f%% (rank #%d/%d, 1=fewest); EPA allowed per blitz %+.3f [%+.3f, %+.3f] (rank #%d/%d, 1=most dangerous); scheme rank #%d/%d.\n",
            100 * mac$blitz_rate, mac$rate_rank, n_dcs, mac$epa_per_blitz, mac$lo, mac$hi, mac$danger_rank, n_dcs, mac$unblocked_rank, n_dcs))
cat(sprintf("Rare-but-dangerous corner (blitz rate < median %.1f%%, EPA/blitz < median %+.3f) holds %d of %d DCs: %s.\n",
            100 * mx, my, nrow(rare_dangerous), n_dcs, paste(rare_dangerous$def_caller, collapse = ", ")))
cat(sprintf("Blitz rate vs blitz danger correlation: r = %+.3f [%+.3f, %+.3f], p = %.3f, n = %d -- %s\n",
            ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value, n_dcs, rate_danger_verdict))
cat("=============================================\n")
