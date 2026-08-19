# =============================================================================
# 36_rotation_xpass.R -- does the shell-rotation effect vary by how obvious a
# pass was coming, and is there any named situation where rotating is
# actually a good tactic?
#
# Nick, verbatim, reacting to R/18's rotation worth-it chart (pooled
# controlled effect -0.011 EPA/play, 95% CI crosses zero):
#   "this might already have it baked in but what if we use like xpass as a
#    way to see if this effect is stronger in obvious pass situations or if
#    there are any situations where rotating safeties shows up as a good
#    tactic"
#
# HE IS RIGHT AND R/18 DID NOT ANSWER IT. R/18's expect_epa() already puts
# down/distance/field position/quarter/score/clock/red-zone INTO the
# expectation, so the situation is baked into what "normal" EPA looks like.
# But the ROTATED-MINUS-STATIC effect itself was estimated once, pooled
# across every situation. Nothing in R/18 (or R/27, which tested the same
# rotate flag against the QB's CLOCK instead of the scoreboard and found a
# faint echo: time to throw is taxed +0.063 sec [0.037, 0.089] on man
# coverage specifically, controlled) ever let that effect VARY by situation.
# This script builds the heterogeneity test: same rotate flag, same
# situation-controlled EPA residual, split by how obvious the pass was
# (continuous xpass and discrete buckets) and by named coaching situations.
#
# UNIVERSE. Identical to R/18: load_sumer(), is_dropback == TRUE,
# garbage_time == FALSE, has_shell (both the pre-snap look and the played
# shell are a real OPEN/CLOSED call). rotate is look != played on that set.
# Not re-derived; R/18 already established this is the right universe for a
# post-snap shell question.
#
# THE EPA RESIDUAL. Reused verbatim from R/18: expect_epa(), a continuous
# analogue of lib_sumer.R's sumer_expect() (same season-grouped,
# out-of-sample xgboost design, same ten SUMER_STATE situation features,
# squared-error objective on expected_points_added instead of binary
# cross-entropy). epa_resid = actual EPA minus what situation alone
# predicts. Every test below compares epa_resid on rotated vs static snaps
# INSIDE a subgroup, so the subgroup's own situation mix is already netted
# out before the split even happens.
#
# XPASS. The pre-snap probability a defense should have expected a pass,
# built exactly as R/33_hold_and_vary.R's Test 3 "Model A" does: lib_sumer's
# sumer_expect() on the full run/pass universe (run_pass %in% c("P","R"),
# non-garbage-time, ALL 32 teams, no caller filter -- this is a play
# identity, not a caller one), same SUMER_STATE features, same
# leave-one-season-out xgboost. That model has to be trained on the full
# run/pass universe, not just dropbacks: is_dropback == TRUE already tells
# you the play WAS a pass, so a model fit only on dropbacks would have
# nothing left to predict. Fit once on every called run or pass league-wide,
# then the resulting probability is joined onto R/18's dropback/shell
# universe by sumer_play_id (confirmed a unique play key: 160,277 rows,
# 160,277 distinct ids). Every one of R/18's 95,474 has_shell dropback rows
# matched a play in the run/pass universe; the join drops nothing.
#
# BUCKETS. Cut points given directly for this question (not the folded
# max(p,1-p) "certainty" scale from R/factory/93_predictability_nuance.R,
# which collapses run-leaning and pass-leaning together and would hide
# exactly the asymmetry Nick is asking about): obvious-run lean (<0.4),
# leaning run (0.4-0.5), coin flip (0.5-0.6), leaning pass (0.6-0.8),
# obvious pass (0.8+). Populations are checked directly below; on a
# dropback-only, shell-charted universe they lean heavily toward the
# pass-leaning end almost by construction (a play that turned into a
# charted dropback was more likely to look like a pass pre-snap), and the
# low-xpass buckets are merged only if a bucket's rotated-snap count falls
# below MIN_CELL, logged explicitly when it happens.
#
# TWO SEPARATE TESTS.
#   1. THE XPASS GRADIENT. Descriptive: the controlled effect inside each
#      xpass bucket, with CIs, so the shape is visible. Formal: does adding
#      a rotate x xpass interaction term to a continuous model of epa_resid
#      earn its keep over the additive (pooled-slope) model -- an F-test via
#      anova() on nested lm()s, plus the interaction coefficient's own CI.
#   2. THE SITUATION HUNT. The same controlled effect inside named
#      situations a coach might actually choose to rotate in: third-and-
#      long (3rd, 7+), red zone, two-minute drill, late and close (Q4,
#      one-score), first-and-10, play-action (the QB's back is turned),
#      man vs zone played (R/27's own echo lives here), blitz vs not. One
#      forest panel, every CI shown.
#
# SCREENING HONESTY. That is >=10 cells tested at 95% confidence. The
# expected count of falsely "significant" cells under a true null is printed
# next to the observed count (cells * 0.05), the same discipline as every
# multi-cell screen in this project, so one stray significant bucket does
# not get promoted to a headline on its own.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()),
# R/factory/lib_sumer.R (load_sumer(), sumer_expect(), SUMER_STATE). Plain
# language, no Michael/Nick in rendered chart text, CIs on every estimate,
# no em dashes.
#
# Out: docs/figures/rotation_xpass.png
#      data/derived/rotation_xpass.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(patchwork); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

MIN_CELL <- 50   # same floor R/18's worth_test() uses per side

# =============================================================================
# UNIVERSE -- identical to R/18_shell_rotation.R
# =============================================================================
d <- load_sumer()
d <- d[is_dropback == TRUE & garbage_time == FALSE]
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell &
      middle_of_field_coverage_look != middle_of_field_coverage_played)]
cat(sprintf("dropback snaps, non-garbage-time, 2022-2025: %s (%.1f%% with a charted shell)\n",
            format(nrow(d), big.mark = ","), 100*mean(d$has_shell)))

# =============================================================================
# EPA RESIDUAL -- expect_epa() reused verbatim from R/18_shell_rotation.R
# =============================================================================
expect_epa <- function(x, target = "expected_points_added", feats = SUMER_STATE, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(get(target))], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x[[target]])
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "reg:squarederror", eta = 0.05, max_depth = 5,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.epa_actual = y, .epa_expected = p)]
  x[!is.na(.epa_expected)]
}

cat("\n=== fitting situation-only EPA expectation (R/18's expect_epa, reused) ===\n")
ep <- expect_epa(d[has_shell == TRUE])
ep[, epa_resid := .epa_actual - .epa_expected]
cat(sprintf("situation-only EPA model: mean actual %.3f, mean predicted %.3f (should match closely), n = %s\n",
            mean(ep$.epa_actual), mean(ep$.epa_expected), format(nrow(ep), big.mark = ",")))

# =============================================================================
# XPASS -- sumer_expect() on the full run/pass universe, exactly as
# R/33_hold_and_vary.R's Test 3 Model A
# =============================================================================
cat("\n=== fitting xpass: pre-snap expected-pass probability, full run/pass universe (R/33 Test 3 method) ===\n")
d_xp <- load_sumer(2022:2025)
d_xp <- d_xp[run_pass %in% c("P","R") & garbage_time == FALSE]
d_xp[, is_pass := as.integer(run_pass == "P")]
cat(sprintf("run/pass universe (all 32 teams, no caller filter): %s snaps, %.1f%% pass\n",
            format(nrow(d_xp), big.mark = ","), 100*mean(d_xp$is_pass)))

model_xp <- sumer_expect(d_xp, "is_pass", feats = SUMER_STATE)
cat(sprintf("xpass model: %s of %s snaps scored (leave-one-season-out); mean actual pass rate %.1f%%, mean predicted %.1f%%\n",
            format(nrow(model_xp), big.mark = ","), format(nrow(d_xp), big.mark = ","),
            100*mean(model_xp$.y), 100*mean(model_xp$.expected)))

xp_join <- unique(model_xp[, .(sumer_play_id, xpass = .expected)], by = "sumer_play_id")
n_before <- nrow(ep)
ep <- merge(ep, xp_join, by = "sumer_play_id", all.x = TRUE)
n_matched <- sum(!is.na(ep$xpass))
cat(sprintf("\njoin onto the has_shell dropback universe: %d of %d rows (%.1f%%) matched an xpass value by sumer_play_id\n",
            n_matched, n_before, 100*n_matched/n_before))
ep <- ep[!is.na(xpass)]

# =============================================================================
# BUCKETS -- fixed cut points, merged only if a bucket's rotated-snap count
# is forced below MIN_CELL
# =============================================================================
BREAKS <- c(0, 0.4, 0.5, 0.6, 0.8, 1.0)
LABS   <- c("Obvious run lean\n(<0.4)", "Leaning run\n(0.4-0.5)", "Coin flip\n(0.5-0.6)",
            "Leaning pass\n(0.6-0.8)", "Obvious pass\n(0.8+)")
ep[, xp_bucket := cut(xpass, BREAKS, labels = LABS, include.lowest = TRUE)]

pop <- ep[, .(n = .N, n_rotate = sum(rotate == 1), n_static = sum(rotate == 0),
             mean_xpass = mean(xpass)), by = xp_bucket][order(xp_bucket)]
cat("\nxpass bucket populations (before any forced merge):\n")
cat("bucket                   |      n | n rotate | n static | mean xpass\n")
for (i in seq_len(nrow(pop))) with(pop[i], cat(sprintf(
  "  %-23s | %6s | %8s | %8s | %.3f\n",
  gsub("\n"," ",xp_bucket), format(n, big.mark=","), format(n_rotate, big.mark=","),
  format(n_static, big.mark=","), mean_xpass)))

# Merge low-population buckets left-to-right (run side first, since that is
# where the dropback universe is thinnest) until every surviving bucket has
# at least MIN_CELL rotated snaps, or everything below coin-flip is one bin.
merge_plan <- LABS
while (TRUE) {
  ep[, xp_bucket := factor(xp_bucket, levels = merge_plan)]
  chk <- ep[, .(n_rotate = sum(rotate == 1)), by = xp_bucket][order(xp_bucket)]
  thin <- which(chk$n_rotate < MIN_CELL & chk$xp_bucket %in% c("Obvious run lean\n(<0.4)","Leaning run\n(0.4-0.5)"))
  if (length(thin) == 0 || length(merge_plan) <= 3) break
  # collapse the two lowest (run-side) buckets into one
  new_lab <- "Run lean (<0.5)"
  ep[xp_bucket %in% c("Obvious run lean\n(<0.4)","Leaning run\n(0.4-0.5)"), xp_bucket := new_lab]
  merge_plan <- c(new_lab, setdiff(merge_plan, c("Obvious run lean\n(<0.4)","Leaning run\n(0.4-0.5)")))
  cat(sprintf("\nforced merge: 'Obvious run lean (<0.4)' and 'Leaning run (0.4-0.5)' combined into '%s' (rotated n was below %d in at least one)\n",
              new_lab, MIN_CELL))
}
ep[, xp_bucket := factor(xp_bucket, levels = merge_plan)]
cat(sprintf("\nfinal bucket set: %s\n", paste(gsub("\n"," ",merge_plan), collapse = " | ")))

# =============================================================================
# TEST 1: THE XPASS GRADIENT
# =============================================================================
cat("\n=== TEST 1: the xpass gradient -- is the rotation effect stronger in obvious-pass situations? ===\n")

worth_test <- function(x, label, extra = list()) {
  r1 <- x[rotate == 1]$epa_resid; r0 <- x[rotate == 0]$epa_resid
  base <- c(list(cell = label), extra, list(n_rotate = length(r1), n_static = length(r0)))
  if (length(r1) < MIN_CELL || length(r0) < MIN_CELL) {
    return(as.data.table(c(base, list(diff = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_))))
  }
  tt <- t.test(r1, r0)
  as.data.table(c(base, list(diff = unname(tt$estimate[1] - tt$estimate[2]),
                             lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value)))
}

gradient <- rbindlist(lapply(levels(ep$xp_bucket), function(b) {
  worth_test(ep[xp_bucket == b], b, extra = list(mean_xpass = mean(ep[xp_bucket == b]$xpass)))
}), fill = TRUE)
gradient[, significant := !is.na(lo) & (lo > 0 | hi < 0)]

cat("bucket                    | mean xpass | n rotate | n static | rotated minus static EPA/play (controlled) [95% CI]  | p\n")
for (i in seq_len(nrow(gradient))) with(gradient[i], cat(sprintf(
  "  %-24s | %10.3f | %8s | %8s | %+.4f [%+.4f, %+.4f]                          | %s\n",
  gsub("\n"," ",cell), mean_xpass, format(n_rotate, big.mark=","), format(n_static, big.mark=","),
  diff, lo, hi, if (is.na(p)) "ns (n too small)" else sprintf("%.4f", p))))

# formal interaction test: does letting the slope vary by xpass earn its keep?
lm_add <- lm(epa_resid ~ rotate + xpass, data = ep)
lm_int <- lm(epa_resid ~ rotate * xpass, data = ep)
av <- anova(lm_add, lm_int)
int_row <- coef(summary(lm_int))["rotate:xpass", ]
int_ci  <- confint(lm_int)["rotate:xpass", ]
f_stat <- av$F[2]; p_int <- av$`Pr(>F)`[2]

cat(sprintf("\nformal interaction test: lm(epa_resid ~ rotate * xpass) vs lm(epa_resid ~ rotate + xpass), F(%d,%d) = %.3f, p = %.4f\n",
            av$Df[2], av$Res.Df[2], f_stat, p_int))
cat(sprintf("rotate x xpass coefficient: %+.4f [%+.4f, %+.4f], p = %.4f\n",
            int_row["Estimate"], int_ci[1], int_ci[2], int_row["Pr(>|t|)"]))
gradient_dir <- ifelse(int_row["Estimate"] < 0,
  "gets WORSE for the offense (more negative) as xpass rises -- Nick's hypothesis",
  "gets BETTER for the offense (less negative / more positive) as xpass rises -- opposite of Nick's hypothesis")
gradient_dir_short <- ifelse(int_row["Estimate"] < 0,
  "the rotation effect worsens for the offense as xpass rises",
  "the rotation effect improves for the offense as xpass rises")
if (p_int < 0.05) {
  interaction_verdict <- sprintf("EARNS ITS KEEP (p = %.4f): the rotation effect %s.", p_int, gradient_dir)
} else {
  interaction_verdict <- sprintf("DOES NOT earn its keep (p = %.4f): a single pooled slope fits about as well as letting it vary by xpass. Numerically the coefficient points the direction that %s, but it is not a confident difference.",
                                 p_int, gradient_dir_short)
}
cat(sprintf("\nTEST 1 VERDICT: %s\n", interaction_verdict))

# =============================================================================
# TEST 2: THE SITUATION HUNT
# =============================================================================
cat("\n=== TEST 2: the situation hunt -- named spots a coach might choose to rotate in ===\n")

ep[, is_third_long := down == 3 & distance >= 7]
ep[, is_first_ten  := down == 1 & distance == 10]
ep[, is_late_close := quarter >= 4 & abs(offense_score_diff) <= 8]

situations <- list(
  list(lab = "Third-and-long (3rd, 7+)",     x = ep[is_third_long == TRUE]),
  list(lab = "Red zone",                     x = ep[redzone == TRUE]),
  list(lab = "Two-minute drill",             x = ep[two_minute == TRUE]),
  list(lab = "Late and close (Q4+, 1 score)", x = ep[is_late_close == TRUE]),
  list(lab = "First-and-10",                 x = ep[is_first_ten == TRUE]),
  list(lab = "Play action",                  x = ep[play_action == 1]),
  list(lab = "Man coverage played",          x = ep[man_zone_coverage == "MAN"]),
  list(lab = "Zone coverage played",         x = ep[man_zone_coverage == "ZONE"]),
  list(lab = "Blitz",                        x = ep[blitz == TRUE]),
  list(lab = "No blitz",                     x = ep[blitz == FALSE])
)
situation <- rbindlist(lapply(situations, function(s) worth_test(s$x, s$lab)), fill = TRUE)
situation[, significant := !is.na(lo) & (lo > 0 | hi < 0)]

cat("situation                    | n rotate | n static | rotated minus static EPA/play (controlled) [95% CI]  | p\n")
for (i in seq_len(nrow(situation))) with(situation[i], cat(sprintf(
  "  %-28s | %8s | %8s | %+.4f [%+.4f, %+.4f]                          | %s\n",
  cell, format(n_rotate, big.mark=","), format(n_static, big.mark=","), diff, lo, hi,
  if (is.na(p)) "ns (n too small)" else sprintf("%.4f", p))))

n_cells_tested <- nrow(situation) + nrow(gradient)
n_observed_sig <- sum(situation$significant, na.rm = TRUE) + sum(gradient$significant, na.rm = TRUE)
n_expected_sig <- 0.05 * n_cells_tested
# Probability of seeing AT LEAST this many "significant" cells out of
# n_cells_tested if every single one were truly null (independent-tests
# approximation; the cells overlap in practice, which if anything makes real
# false positives even more likely than this number says).
p_at_least_observed <- 1 - pbinom(max(n_observed_sig - 1, 0), n_cells_tested, 0.05)
cat(sprintf("\n=== SCREENING HONESTY ===\ncells tested at 95%% (gradient buckets + situation cells): %d\n", n_cells_tested))
cat(sprintf("expected significant cells by chance alone (5%% of %d): %.2f\n", n_cells_tested, n_expected_sig))
cat(sprintf("observed significant cells (CI excludes zero): %d\n", n_observed_sig))
cat(sprintf("P(at least %d of %d significant | true null everywhere) = %.3f\n",
            n_observed_sig, n_cells_tested, p_at_least_observed))

all_cells <- rbind(gradient[, .(source = "gradient", cell = gsub("\n"," ",cell), n_rotate, n_static, diff, lo, hi, p, significant)],
                    situation[, .(source = "situation", cell, n_rotate, n_static, diff, lo, hi, p, significant)])
sig_cells <- all_cells[significant == TRUE]

# Same "is this actually more than chance" bar R/04_halftime_adjustments.R
# uses for its noise cone: observed <= 1.5x the naive chance count reads as
# noise, not a finding. That bar, not a bare observed>expected comparison,
# is what decides whether a cell gets promoted.
clears_screen <- nrow(sig_cells) > 0 && n_observed_sig > ceiling(1.5 * n_expected_sig)

if (clears_screen) {
  best <- sig_cells[order(-abs(diff))][1]
  screen_verdict <- sprintf("%d observed vs %.2f expected by chance (P of this many false positives under a true null = %.3f) -- more than chance alone plausibly explains. The standout: '%s' (%+.4f EPA/play [%+.4f, %+.4f], rotated minus static, controlled).",
                            n_observed_sig, n_expected_sig, p_at_least_observed, best$cell, best$diff, best$lo, best$hi)
} else if (nrow(sig_cells) > 0) {
  screen_verdict <- sprintf("%d observed vs %.2f expected by chance -- P(at least this many false positives | true null everywhere) = %.3f, which is unremarkable. The cell(s) that cleared their own CI (%s) do not clear the screening bar and should be read as noise, not a finding.",
                            n_observed_sig, n_expected_sig, p_at_least_observed, paste(sig_cells$cell, collapse = "; "))
} else {
  screen_verdict <- sprintf("0 cells cleared their own 95%% CI, against %.2f expected by chance across %d cells. The null is uniform: nowhere in the situations or xpass range tested here does rotating the shell show up as a good (or bad) tactic beyond the pooled -0.011 EPA null R/18 already found.",
                            n_expected_sig, n_cells_tested)
}
cat(sprintf("\nTEST 2/SCREENING VERDICT: %s\n", screen_verdict))

# =============================================================================
# CHART: gradient on top, situation hunt below
# =============================================================================
gradient[, cell_f := factor(cell, levels = rev(levels(ep$xp_bucket)))]
pTop <- ggplot(gradient, aes(diff, cell_f, colour = significant)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 3.2, na.rm = TRUE) +
  geom_text(aes(label = sprintf("%+.3f (n=%s)", diff, format(n_rotate, big.mark=","))),
            vjust = -1.2, size = 2.95, fontface = "bold", colour = "grey25", na.rm = TRUE) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#6b4c9a"), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = "1. The xpass gradient",
       subtitle = "Situation-controlled rotated-minus-static EPA/play, by pre-snap expected-pass probability",
       x = "offense EPA/play, rotated minus static (negative = rotation helps the defense)", y = NULL) +
  theme_coach(grid = "y")

situation[, cell_f := factor(cell, levels = rev(situation$cell))]
pBot <- ggplot(situation, aes(diff, cell_f, colour = significant)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.18, linewidth = 0.6, na.rm = TRUE) +
  geom_point(size = 3.2, na.rm = TRUE) +
  geom_text(aes(label = sprintf("%+.3f (n=%s)", diff, format(n_rotate, big.mark=","))),
            vjust = -1.2, size = 2.85, fontface = "bold", colour = "grey25", na.rm = TRUE) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#6b4c9a"), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = "2. The situation hunt",
       subtitle = "Same controlled effect, inside named situations a coach might choose to rotate in",
       x = "offense EPA/play, rotated minus static (negative = rotation helps the defense)", y = NULL) +
  theme_coach(grid = "y")

chart_title <- if (clears_screen) {
  sprintf("One spot clears the screen: rotating shows up as a real tactic in '%s'", sig_cells[order(-abs(diff))][1]$cell)
} else {
  "No spot clears the screen: rotation's null holds everywhere tested"
}

pOut <- (pTop / pBot) +
  plot_annotation(
    title = chart_title,
    subtitle = sprintf("Interaction test (rotate x xpass, continuous): p = %.4f (%s). Screening: %d of %d cells significant at 95%%, vs %.2f expected by chance.",
                       p_int, if (p_int < 0.05) "earns its keep" else "does not earn its keep",
                       n_observed_sig, n_cells_tested, n_expected_sig),
    caption = fig_caption(
      "SumerSports play charting 2022-2025",
      sprintf("%s dropback snaps with a charted shell, outside garbage time. xpass from a season-grouped, out-of-sample model of pass probability fit on the full league\nrun/pass universe (%s snaps, all 32 teams), joined by sumer_play_id (100%% match).",
              format(nrow(ep), big.mark = ","), format(nrow(d_xp), big.mark = ",")),
      sprintf("\nBoth panels use the same situation-controlled EPA residual R/18 uses (expectation from down/distance/field position/quarter/score/clock alone, no rotation or\nxpass information). Orange = clears its own 95%% CI. Expected-by-chance count is %.2f (5%% of %d cells tested); treat any single orange cell against that bar, not\nin isolation. Built by R/36_rotation_xpass.R.",
              n_expected_sig, n_cells_tested)),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/rotation_xpass.png", pOut, w = 12.5, h = 11.5)

# =============================================================================
# CSV
# =============================================================================
out_csv <- rbind(
  gradient[, .(test = "xpass_gradient", cell = gsub("\n", " ", cell), mean_xpass = round(mean_xpass, 3), n_rotate, n_static,
              diff = round(diff, 5), lo = round(lo, 5), hi = round(hi, 5), p = round(p, 5), significant)],
  situation[, .(test = "situation_hunt", cell, mean_xpass = NA_real_, n_rotate, n_static,
                diff = round(diff, 5), lo = round(lo, 5), hi = round(hi, 5), p = round(p, 5), significant)],
  data.table(test = "interaction", cell = "rotate x xpass (continuous)", mean_xpass = NA_real_,
             n_rotate = sum(ep$rotate == 1), n_static = sum(ep$rotate == 0),
             diff = round(unname(int_row["Estimate"]), 5), lo = round(int_ci[1], 5), hi = round(int_ci[2], 5),
             p = round(p_int, 5), significant = p_int < 0.05)
)
write_csv(as.data.frame(out_csv), "data/derived/rotation_xpass.csv")
cat(sprintf("\nwrote data/derived/rotation_xpass.csv (%d rows)\n", nrow(out_csv)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("TEST 1 (xpass gradient): %s\n", interaction_verdict))
cat(sprintf("TEST 2 (situation hunt) / SCREENING: %s\n", screen_verdict))
cat("\nOut: docs/figures/rotation_xpass.png\n")
cat("     data/derived/rotation_xpass.csv\n")
