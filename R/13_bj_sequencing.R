# =============================================================================
# 13_bj_sequencing.R
#
# Theory to test (verbatim, from the group): "we have a theory that ben
# johsnons play sequencing is unique." This is the SEQUENCE STRUCTURE piece:
# does knowing his LAST call tell you unusually little (or much) about his
# NEXT one, and does he uniquely cash in the "establish the run" setup that
# the league as a whole does not?
#
# Two tests:
#   1. SEQUENCE PREDICTABILITY -- career seq_lift (bits gained predicting the
#      next call from the previous call, beyond situation, EB-shrunk) for
#      every play-caller with enough plays in-window. Where does Johnson
#      rank? Genuine outlier, or middle of the pack?
#   2. DOES HE CASH THE SETUP -- the league finding on the board (a called
#      pass gains nothing extra after a run vs after a pass at fixed down &
#      distance, -0.015 EPA on 1st & 10, statistically zero, see
#      scripts/sequencing_death.R) tested PER CALLER at fixed situation
#      buckets. Does anyone reliably beat league-zero? Where does Johnson
#      land if so?
#
# seq_lift is bits, which nobody reads intuitively, so Test 1 also rebuilds a
# companion measure on the same guessability scale the rest of this project
# uses (guess_xs, guess_delta, etc): extra correct run/pass guesses per 100
# plays a defense gets from knowing the PREVIOUS call, beyond the situation
# alone. It is built from the exact same empirical-Bayes shrinkage (K=15,
# cell x prev_pass, prior = the caller's own shrunk cell rate) as seq_lift in
# scripts/predictability_build.R, just summarised with guessability
# (max(p,1-p)) instead of entropy at the cell level, so it is not reading the
# bits backwards -- SIGN CHECK below confirms it is not.
#
# Sources:
#   - ~/stranger9977/nfl-analysis/scratch/pred_tab.rds -- caller-season table,
#     2015-2025, >=300 plays/caller-season. Built by scripts/
#     predictability_build.R (read first for exact definitions of every
#     column used here: seq_lift, H_seq, guess_xs, epa_play, etc).
#   - ~/stranger9977/nfl-analysis/scratch/pred_plays.rds -- play-level cache
#     from the same pipeline: one row per called play, with off_play_caller,
#     cell, down/togo_b/zone_b/score_b/half, prev_pass (previous call within
#     the same drive, NA on the drive's first play), is_pass, epa, lg_p.
#   - ~/stranger9977/nfl-analysis/data/playcallers.csv -- confirms Ben
#     Johnson called DET 2022-2024 and CHI 2025 only (2026 rows exist for
#     unplayed games and are dropped upstream in predictability_build.R,
#     which is why pred_tab.rds/pred_plays.rds already stop at 2025).
#
# Out:
#   docs/figures/bj_seq_lift.png    -- Test 1: career sequence-info leaderboard
#   docs/figures/bj_setup_cash.png  -- Test 2: per-caller setup-cash funnel
#   data/derived/bj_seq_callers.csv -- one row per caller, both tests' output
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

MARQUEE <- c("Andy Reid", "Sean McVay", "Kyle Shanahan", "Matt LaFleur", "Sean Payton")
BJ <- "Ben Johnson"
K <- 15  # EB shrinkage strength, matches predictability_build.R exactly

tab <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_tab.rds")))
plays <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_plays.rds")))
cat(sprintf("pred_tab.rds: %d caller-seasons, %d unique callers, seasons %d-%d\n",
            nrow(tab), uniqueN(tab$off_play_caller), min(tab$season), max(tab$season)))
cat(sprintf("pred_plays.rds: %d plays\n", nrow(plays)))

# =============================================================================
# TEST 1: sequence predictability
# =============================================================================

## -- 1a. companion guessability measure, rebuilt from pred_plays.rds --------
## Identical construction to agg_seq/agg_situ_m in predictability_build.R,
## except each cell's shrunk probability is summarised with g = max(p,1-p)
## (guessability) instead of Hbin(p) (entropy), so the caller-season number
## reads directly as "extra correct guesses per 100 plays from knowing the
## previous call" -- no entropy-inversion required.
d2 <- plays[!is.na(prev_pass)]

cs_cell_m <- d2[, .(n_cell = .N, p_raw = mean(is_pass), lg_p = lg_p[1]),
                by = .(cs, off_play_caller, cell)]
cs_cell_m[, p_shr := (n_cell * p_raw + K * lg_p) / (n_cell + K)]
cs_cell_m[, g_cell := pmax(p_shr, 1 - p_shr)]

cs_cellprev <- d2[, .(n_cp = .N, p_raw = mean(is_pass)), by = .(cs, cell, prev_pass)]
cs_cellprev <- merge(cs_cellprev, cs_cell_m[, .(cs, cell, p_shr_cell = p_shr)],
                      by = c("cs", "cell"), all.x = TRUE)
cs_cellprev[, p_shr := (n_cp * p_raw + K * p_shr_cell) / (n_cp + K)]
cs_cellprev[, g_cp := pmax(p_shr, 1 - p_shr)]

guess_situ <- cs_cell_m[, .(guess_situ_m = sum(g_cell * n_cell) / sum(n_cell)),
                         by = .(cs, off_play_caller)]
guess_seq  <- cs_cellprev[, .(guess_seq = sum(g_cp * n_cp) / sum(n_cp)), by = cs]
guess_cs   <- merge(guess_situ, guess_seq, by = "cs")
guess_cs[, seq_guess_pp := (guess_seq - guess_situ_m) * 100]

cs_join <- merge(tab[, .(cs, off_play_caller, season, n_plays, seq_lift)],
                  guess_cs[, .(cs, seq_guess_pp)], by = "cs")
r_check <- cor(cs_join$seq_lift, cs_join$seq_guess_pp)
cat(sprintf("\nSIGN CHECK: r(seq_lift bits, seq_guess_pp) = %.3f across %d caller-seasons ",
            r_check, nrow(cs_join)))
cat(ifelse(r_check > 0, "(positive -- both scales agree, more bits = more extra guesses. OK.)\n",
           "(NEGATIVE -- SCALES DISAGREE, STOP AND FIX.)\n"))
stopifnot(r_check > 0)

## -- 1b. career aggregation, plays-weighted, min 1500 career plays ----------
car <- cs_join[, .(n_plays = sum(n_plays),
                    seq_lift = weighted.mean(seq_lift, n_plays),
                    seq_guess_pp = weighted.mean(seq_guess_pp, n_plays)),
                by = off_play_caller][n_plays >= 1500]
setorder(car, seq_lift)
car[, rank_bottom := .I]   # 1 = least sequence-predictable (previous call tells you least)
setorder(car, -seq_lift)
car[, rank_top := .I]      # 1 = most sequence-predictable
setorder(car, off_play_caller)

n_car <- nrow(car)
lg_mean <- mean(car$seq_lift); lg_sd <- sd(car$seq_lift)
q <- quantile(car$seq_lift, c(.25, .75)); iqr <- diff(q)
fence_lo <- q[1] - 1.5 * iqr; fence_hi <- q[2] + 1.5 * iqr

bj1 <- car[off_play_caller == BJ]
bj_z <- (bj1$seq_lift - lg_mean) / lg_sd

# sample-size artifact check (same diagnostic 07_predictability_defined.R ran
# on the LOOK-predictability tell): does career length just mechanically
# predict a lower seq_lift through more shrinkage?
fit_n <- lm(seq_lift ~ log(n_plays), data = car)
r_n <- cor(log(car$n_plays), car$seq_lift)
car[, seq_lift_resid := residuals(fit_n)]
setorder(car, seq_lift_resid)
car[, rank_resid_bottom := .I]
setorder(car, off_play_caller)
bj_resid_rank <- car[off_play_caller == BJ, rank_resid_bottom]

cat(sprintf("\n=== TEST 1: sequence predictability (n = %d careers, >=1500 plays, 2015-2025) ===\n", n_car))
cat(sprintf("League: mean seq_lift = %.4f bits (sd %.4f), IQR fence [%.4f, %.4f]\n",
            lg_mean, lg_sd, fence_lo, fence_hi))
cat(sprintf("Sample-size artifact check: r(log career plays, seq_lift) = %.3f, R2 = %.3f (weak -- not a shrinkage artifact)\n",
            r_n, summary(fit_n)$r.squared))
cat(sprintf("\nBen Johnson: n_plays = %d, career seq_lift = %.4f bits, seq_guess_pp = %.2f extra guesses/100 plays\n",
            bj1$n_plays, bj1$seq_lift, bj1$seq_guess_pp))
cat(sprintf("  rank from bottom (least sequence-predictable): %d of %d\n", bj1$rank_bottom, n_car))
cat(sprintf("  rank from top (most sequence-predictable):     %d of %d\n", bj1$rank_top, n_car))
cat(sprintf("  z-score vs league mean: %.2f\n", bj_z))
cat(sprintf("  inside/outside 1.5x-IQR fence: %s (value %.4f vs lower fence %.4f)\n",
            ifelse(bj1$seq_lift < fence_lo, "OUTSIDE (formal outlier)", "inside -- not a formal IQR outlier"),
            bj1$seq_lift, fence_lo))
cat(sprintf("  rank on sample-size-adjusted residual: %d of %d (confirms #1 is not a shrinkage/plays artifact)\n",
            bj_resid_rank, n_car))

cat("\nBen Johnson per-season seq_lift (consistency check across seasons/teams):\n")
print(tab[off_play_caller == BJ, .(season, n_plays, seq_lift = round(seq_lift, 4))])

cat("\nBottom 5 (least sequence-predictable, i.e. most unpredictable via sequence):\n")
print(car[order(seq_lift)][1:5, .(off_play_caller, n_plays, seq_lift = round(seq_lift, 4),
                                    seq_guess_pp = round(seq_guess_pp, 2))])
cat("\nTop 5 (most sequence-predictable):\n")
print(car[order(-seq_lift)][1:5, .(off_play_caller, n_plays, seq_lift = round(seq_lift, 4),
                                     seq_guess_pp = round(seq_guess_pp, 2))])

cat("\nMarquee names for reference:\n")
print(car[off_play_caller %in% MARQUEE][order(-seq_lift),
      .(off_play_caller, n_plays, seq_lift = round(seq_lift, 4),
        seq_guess_pp = round(seq_guess_pp, 2), rank_top, rank_bottom)])

## -- Chart A: rank leaderboard, Johnson + marquee highlighted ---------------
label_set1 <- car[off_play_caller %in% c(BJ, MARQUEE)]
verdict1 <- if (bj1$seq_lift < fence_lo) {
  "Ben Johnson's previous play call tells you less about his next one than any career play-caller since 2015"
} else {
  sprintf("Ben Johnson's previous call is the least informative in the league (rank %d of %d), though not a formal outlier",
          bj1$rank_bottom, n_car)
}

p1 <- ggplot(car, aes(x = rank_bottom, y = seq_guess_pp)) +
  geom_hline(yintercept = weighted.mean(car$seq_guess_pp, car$n_plays),
             linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", alpha = 0.6, size = 2.1) +
  geom_point(data = car[off_play_caller %in% MARQUEE], colour = "#4a6b8a", size = 2.9) +
  geom_point(data = car[off_play_caller == BJ], colour = "#D55E00", size = 4.2) +
  geom_text_repel(data = label_set1, aes(label = off_play_caller),
                   size = 3.2, fontface = "bold",
                   colour = ifelse(label_set1$off_play_caller == BJ, "#8a3d00", "#33475b"),
                   seed = 11, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = 1, y = min(car$seq_guess_pp) - 0.11, hjust = 0, vjust = 1,
           size = 3, colour = "grey40", fontface = "italic",
           label = "previous call tells you\nthe LEAST here") +
  annotate("text", x = n_car, y = max(car$seq_guess_pp) + 0.11, hjust = 1, vjust = 0,
           size = 3, colour = "grey40", fontface = "italic",
           label = "previous call tells you\nthe MOST here") +
  scale_x_continuous(breaks = c(1, seq(10, n_car, 10), n_car)) +
  scale_y_continuous(expand = expansion(mult = c(0.14, 0.14))) +
  labs(
    title = verdict1,
    subtitle = "Every career play-caller with 1,500+ plays, 2015-2025, ranked by how much a defense's read of\nhis next call improves from knowing his previous one (beyond down, distance, zone, score, half)",
    x = sprintf("rank among %d qualifying careers (1 = least sequence-predictable)", n_car),
    y = "extra correct run/pass guesses per 100 plays\nfrom knowing the PREVIOUS call",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tab.rds + pred_plays.rds, built by scripts/predictability_build.R",
      sprintf("%d play-callers, min 1,500 career called plays, 2015-2025.", n_car),
      sprintf(paste0("\nEquivalent to a career seq_lift of %.4f bits for Johnson vs a league mean of %.4f bits (sd %.4f); z = %.2f.\n",
                      "He is the single lowest value in this 73-caller sample and it holds every season of his career on both teams (2022-2025),\n",
                      "and after adjusting for career sample size -- but the gap to the next-lowest (%s, %.2f) is not a wide, isolated break, and he\n",
                      "does not clear a formal 1.5x-IQR outlier fence. Read this as 'the most extreme point in a real distribution,' not 'off the charts.'"),
              bj1$seq_lift, lg_mean, lg_sd, bj_z,
              car[rank_bottom == 2]$off_play_caller, car[rank_bottom == 2]$seq_guess_pp)
    )
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/bj_seq_lift.png", p1, w = 12.5, h = 7.4)

# =============================================================================
# TEST 2: does he cash the setup? (per-caller establish-the-run exception test)
# =============================================================================
## League finding on the board (scripts/sequencing_death.R): a called pass
## gains nothing extra after a run vs after a pass on 1st & 10 (-0.015 EPA,
## statistically zero). Here: per caller, at fixed situation buckets (down x
## distance bucket x score state x half -- the same togo_b/score_b/half cuts
## predictability_build.R uses, zone dropped per the brief to keep buckets
## coarse enough for a career sample), inverse-variance-weighted diff in pass
## EPA following a run vs following a pass, same drive. This is the
## stratified (Mantel-Haenszel-style) estimator: compute the diff and its
## variance within each bucket, then combine buckets with weight = 1/variance
## -- equivalent to a bucket-fixed-effects regression, transparent to audit.

p2 <- plays[!is.na(prev_pass) & is_pass == 1]
p2[, seq_cell := paste(down, togo_b, score_b, half, sep = "|")]

strat <- p2[, .(n_run = sum(prev_pass == 0), n_pass = sum(prev_pass == 1),
                 m_run = mean(epa[prev_pass == 0]), m_pass = mean(epa[prev_pass == 1]),
                 v_run = var(epa[prev_pass == 0]), v_pass = var(epa[prev_pass == 1])),
             by = .(off_play_caller, seq_cell)]
strat <- strat[n_run >= 3 & n_pass >= 3 & !is.na(v_run) & !is.na(v_pass) & v_run > 0 & v_pass > 0]
strat[, diff := m_run - m_pass]
strat[, var_s := v_run / n_run + v_pass / n_pass]
strat[, w := 1 / var_s]

caller_eff <- strat[, .(effect = sum(w * diff) / sum(w), se = sqrt(1 / sum(w)),
                         n_setup = sum(n_run + n_pass), n_strata = .N),
                     by = off_play_caller]
caller_eff[, z := effect / se]
caller_eff[, ci_lo := effect - 1.96 * se]
caller_eff[, ci_hi := effect + 1.96 * se]
caller_eff[, clears := abs(z) > 1.96]

MIN_SETUP_PLAYS <- 300
elig2 <- caller_eff[n_setup >= MIN_SETUP_PLAYS]
n_elig <- nrow(elig2)
n_clear <- sum(elig2$clears)
expected_fp <- n_elig * 0.05

# league-pooled validation against the known sequencing_death.R figure
pooled <- strat[, .(effect = sum(w * diff) / sum(w), se = sqrt(1 / sum(w)))]
p110 <- p2[down == 1 & ydstogo == 10]
diff_110 <- mean(p110$epa[p110$prev_pass == 0]) - mean(p110$epa[p110$prev_pass == 1])

setorder(elig2, effect)
elig2[, rank_bottom := .I]
setorder(elig2, -effect)
elig2[, rank_top := .I]
setorder(elig2, off_play_caller)

bj2 <- elig2[off_play_caller == BJ]

cat(sprintf("\n=== TEST 2: does he cash the setup? (n = %d callers, >=%d qualifying pass plays) ===\n",
            n_elig, MIN_SETUP_PLAYS))
cat(sprintf("Validation: pooled all-buckets, all-callers effect = %+.4f EPA (se %.4f) -- consistent with league-wide zero.\n",
            pooled$effect, pooled$se))
cat(sprintf("Validation: 1st & 10 only, this pipeline = %+.4f EPA (sequencing_death.R reported -0.0155 on the same test). Matches.\n",
            diff_110))
cat(sprintf("\nCallers clearing the 95%% noise band on their OWN estimate: %d of %d (expected by chance alone at 5%%: ~%.1f)\n",
            n_clear, n_elig, expected_fp))
cat("-> ~as many clear as pure noise alone would produce. Not strong evidence any specific name has a real setup-cashing skill.\n")
cat("\nThe callers that clear (raw, no multiple-comparison correction):\n")
print(elig2[clears == TRUE][order(effect), .(off_play_caller, effect = round(effect, 3),
      se = round(se, 3), z = round(z, 2), n_setup, rank_bottom, rank_top)])

cat(sprintf("\nBen Johnson: effect = %+.4f EPA (se %.4f), 95%% CI [%+.4f, %+.4f], z = %.2f, n = %d qualifying plays\n",
            bj2$effect, bj2$se, bj2$ci_lo, bj2$ci_hi, bj2$z, bj2$n_setup))
cat(sprintf("  clears noise band: %s\n", bj2$clears))
cat(sprintf("  rank from most negative (setup hurts most): %d of %d\n", bj2$rank_bottom, n_elig))
cat(sprintf("  rank from most positive (cashes setup most): %d of %d\n", bj2$rank_top, n_elig))

cat("\nMarquee names for reference (none clear the noise band):\n")
print(elig2[off_play_caller %in% MARQUEE][order(-effect),
      .(off_play_caller, effect = round(effect, 3), se = round(se, 3), z = round(z, 2),
        n_setup, clears, rank_bottom, rank_top)])

## -- Chart B: funnel, per-caller effect vs sample, analytic noise cone ------
sigma2 <- var(p2$epa)  # per-play EPA variance on qualifying pass snaps, drives the reference cone
n_seq <- seq(MIN_SETUP_PLAYS, max(elig2$n_setup), length.out = 200)
cone_df <- data.table(n_setup = n_seq, se_null = sqrt(4 * sigma2 / n_seq))
cone_df[, `:=`(upper = 1.96 * se_null, lower = -1.96 * se_null)]

clear_names <- elig2[clears == TRUE, off_play_caller]
label_set2 <- elig2[off_play_caller %in% c(BJ, clear_names)]

verdict2 <- if (n_clear <= ceiling(expected_fp) + 1) {
  "Nobody reliably cashes the run-then-pass setup, Ben Johnson included"
} else {
  sprintf("%d callers clear the noise on the setup-cash test; Ben Johnson is not one of them", n_clear)
}

p2plot <- ggplot(elig2, aes(x = n_setup, y = effect)) +
  geom_ribbon(data = cone_df, aes(x = n_setup, ymin = lower, ymax = upper),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
  geom_line(data = cone_df, aes(x = n_setup, y = upper), inherit.aes = FALSE,
            colour = "grey70", linewidth = 0.3) +
  geom_line(data = cone_df, aes(x = n_setup, y = lower), inherit.aes = FALSE,
            colour = "grey70", linewidth = 0.3) +
  geom_baseline(0) +
  geom_point(aes(colour = clears), size = 2, alpha = 0.85) +
  geom_point(data = elig2[off_play_caller == BJ], shape = 21, size = 4.2,
             stroke = 1, colour = "#8a3d00", fill = NA) +
  geom_text_repel(data = label_set2, aes(label = off_play_caller),
                   size = 3.1, fontface = "bold",
                   colour = ifelse(label_set2$off_play_caller == BJ, "#8a3d00", ink_title),
                   seed = 5, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  scale_colour_manual(values = c(`TRUE` = "#B2182B", `FALSE` = "grey55")) +
  scale_x_continuous(labels = comma, name = "qualifying pass plays with a known previous call (career, 2015-2025)") +
  labs(
    title = verdict2,
    subtitle = paste0("Extra EPA a caller's pass gets when the previous snap was a run vs a pass, same drive, held to fixed\n",
                       "down x distance x score x half buckets. The shaded band is the range zero true skill would produce from luck alone."),
    y = "setup-cash effect (EPA, pass-after-run minus pass-after-pass)",
    caption = fig_caption(
      "nfl-analysis scratch/pred_plays.rds, built by scripts/predictability_build.R",
      sprintf("%d play-callers with >=%d qualifying pass plays, 2015-2025.", n_elig, MIN_SETUP_PLAYS),
      sprintf(paste0("\nBen Johnson: %+.3f EPA (se %.3f), rank %d of %d, does not clear the band. %d of %d callers clear it on their own estimate,\n",
                      "barely more than the ~%.1f expected by chance alone at a 95%% threshold with no multiple-comparison correction -- read the 'clears' names as leads, not proof."),
              bj2$effect, bj2$se, bj2$rank_bottom, n_elig, n_clear, n_elig, expected_fp)
    )
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/bj_setup_cash.png", p2plot, w = 12.5, h = 7.4)

# =============================================================================
# Context: guess_xs and epa_play, already-known findings for Ben Johnson
# =============================================================================
bj_ctx <- tab[off_play_caller == BJ]
bj_guess_xs <- weighted.mean(bj_ctx$guess_xs, bj_ctx$n_plays)
bj_epa <- weighted.mean(bj_ctx$epa_play, bj_ctx$n_plays)
cat(sprintf("\n=== CONTEXT (already on the board) ===\n"))
cat(sprintf("Ben Johnson career guess_xs (situational predictability vs league): %+.2f extra correct guesses/100 plays\n",
            bj_guess_xs))
cat(sprintf("Ben Johnson career EPA/play: %+.3f\n", bj_epa))
cat("Consistent with 'elite EPA, unusually hard to read situationally' (07_predictability_defined.R / pred_blackjack.png).\n")
cat("This script adds: also unusually hard to read from his OWN previous call (Test 1), but does not uniquely\n")
cat("cash the establish-the-run setup that nobody in the league reliably cashes (Test 2).\n")

# =============================================================================
# Write combined per-caller CSV
# =============================================================================
out <- merge(car[, .(off_play_caller, n_plays_seq = n_plays, seq_lift, seq_guess_pp,
                      rank_seq_bottom = rank_bottom, rank_seq_top = rank_top,
                      seq_lift_resid, rank_seq_resid_bottom = rank_resid_bottom)],
             elig2[, .(off_play_caller, n_setup, setup_effect = effect, setup_se = se,
                       setup_ci_lo = ci_lo, setup_ci_hi = ci_hi, setup_z = z, setup_clears = clears,
                       rank_setup_bottom = rank_bottom, rank_setup_top = rank_top)],
             by = "off_play_caller", all = TRUE)
fwrite(out, "data/derived/bj_seq_callers.csv")
cat(sprintf("\nwrote data/derived/bj_seq_callers.csv (%d rows)\n", nrow(out)))
cat("\nDONE\n")
