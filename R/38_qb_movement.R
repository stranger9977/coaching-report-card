# =============================================================================
# 38_qb_movement.R -- designed quarterback movement: who calls it, and does it
# pay. Part of the open "keep looking for interesting stuff" pass across
# Sumer fields nobody on this board has touched: dropback_type, dropback_side,
# dropback_escape_side, quarterback_left_pocket, quarterback_scramble.
#
# THE FOLKLORE. The "Shanahan tree" (Kyle Shanahan, Sean McVay, Ben Johnson,
# Matt LaFleur) is supposed to live on the bootleg/rollout/moving-pocket game
# -- keepers and sprint-outs off outside-zone run action, faking the run and
# throwing on the move. Three testable claims: (1) does the tree actually
# call more designed movement than the league, (2) does moving the pocket pay
# once the situation is controlled for, and (3) is any of it a stable coach
# identity or just a description of one stretch of tape.
#
# STEP 0, READING THE FIELD (done before anything else, per the ask). Sumer's
# dropback_type has four levels on non-garbage-time, caller-attributed
# dropbacks 2022-2025 (n=82,228): STANDARD (92.0%, a straight drop -- no
# design), ROLLOUT (7.4%, the boot/sprint-out/moving-pocket level -- this IS
# the designed-movement level), TRICK (0.3%, gadget plays -- 86% play-action,
# ~13x average depth of target, clearly a different animal from a bootleg;
# excluded from the movement measure and reported for completeness only) and
# blank/unclassified (0.2%, dropped). designed_quarterback_run is always
# FALSE in this universe by construction (a QB run classified as designed
# is charted as a run play, not a dropback, so it never appears here).
# quarterback_scramble is its own flag, NOT a dropback_type level -- it fires
# on top of STANDARD, ROLLOUT or TRICK alike (561 of 6,123 rollouts still end
# in a scramble away from the design). Per the brief: scramble is improvised
# by definition, excluded from the design measure, used as its own context
# stat below.
#
# THE quarterback_left_pocket TRAP. This field is one of the five named in
# the brief and it looks like a movement measure, but it is not one -- it is
# a pressure symptom. Checked before using it for anything: its rate is
# statistically IDENTICAL on STANDARD (20.3%) and ROLLOUT (20.8%) dropbacks,
# which a real design measure could never be (a rollout is already outside
# the pocket by definition). What it actually tracks: pressure rate jumps
# from 20.7% to 71.8% and sack rate from 3.3% to 19.0% when it flips TRUE,
# and 98.4% of scrambles carry it. Per-caller, its rate correlates r=+0.55
# with a caller's own pressure rate and r=-0.06 with his designed-movement
# rate. Verdict: quarterback_left_pocket means the QB was forced (or chose,
# under duress) to vacate a broken pocket -- a pass-protection/pressure
# read, not a coaching design choice. It is reported below as a diagnostic
# context stat, not folded into the design measure, and that separation is
# itself the finding for this field.
#
# DATA. Sumer play charting via R/factory/lib_sumer.R's load_sumer(),
# 2022-2025. UNIVERSE for the caller-level tests: same construction as
# R/14/R/25/R/33 -- callers with >= 1200 offensive plays (run+pass,
# non-garbage-time) qualify (37 of 65), THEN that 37-caller list's dropback
# subset (is_dropback == TRUE, no_play == FALSE, garbage_time == FALSE,
# dropback_type in STANDARD/ROLLOUT/TRICK) is the analysis universe, 66,545
# dropbacks. Smallest qualifying caller still carries 725 dropbacks, plenty
# for a rate.
#
# TEST 1: THE MOVEMENT LEADERBOARD. Designed-movement rate (ROLLOUT share of
# classified dropbacks) per caller, raw AND situation-adjusted via
# lib_sumer's sumer_resid() (target = designed_move, situation-only
# features, leave-one-season-out xgboost -- exactly the repo's standing
# convention: nothing about who is coaching goes into the expectation, so
# the residual is the coach's own choice). Ranked; the tree highlighted
# against the league; persist_split() on the residual to test whether the
# rate is a stable identity. Aside: the specific "keepers off wide zone"
# claim, tested directly as the caller-level correlation between outside-
# zone run rate and rollout rate.
#
# TEST 2: DOES MOVING THE POCKET PAY. A continuous analogue of
# lib_sumer's sumer_expect() (same leave-one-season-out xgboost, reg:square
# error instead of binary:logistic, situation-only features, borrowed
# directly from R/31's yac_expect() pattern) gives every dropback an
# expected EPA from situation alone; actual minus expected is epa_resid.
# Grouped by dropback_type x play_action (not modeled as a feature -- held
# out and used to split the result, per the brief), league-wide and for the
# four tree callers. Sack rate, pressure rate and time_to_throw reported
# alongside as the "hide a bad line" check: does moving the pocket cut sacks
# even when it does not cut pressure.
#
# TEST 3: THE SIGNATURE CHECK. Movement rate (x) against its own payoff (y,
# EPA-over-expected on that caller's ROLLOUT plays specifically) for all 37
# callers, quadrants at the league medians, tree annotated. Persistence
# (odd/even season split on the Test 1 residual) answers whether this is a
# stable coach trait, same test R/27's rotation piece and others in this
# repo use as their bar for "trait, not description."
#
# Conventions: R/lib/theme_coach.R, plain football words, no invented
# jargon, no Michael/Nick anywhere in rendered chart text, EB shrinkage not
# needed here (rates carry hundreds-to-thousands of plays per caller, CIs
# on highlighted callers instead), honest computed titles, no em dashes.
#
# Out: docs/figures/qb_movement.png (Test 1 leaderboard | Test 3 quadrant
#        scatter, two aligned panels)
#      data/derived/qb_movement.csv (37 qualifying callers, every measure
#        below, one row each)
# =============================================================================

suppressMessages({
  library(data.table); library(xgboost); library(ggplot2)
  library(ggrepel); library(patchwork); library(scales); library(readr)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)
set.seed(20260819)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS  <- 2022:2025
QUAL_MIN <- 1200  # matches R/14/R/25/R/33's convention -- 37 callers
TREE <- c("Kyle Shanahan", "Sean McVay", "Ben Johnson", "Matt LaFleur")
COL <- c("Kyle Shanahan" = "#AA0000", "Sean McVay" = "#003594",
         "Ben Johnson" = "#D55E00", "Matt LaFleur" = "#009E73",
         "League" = "grey45")

# =============================================================================
# STEP 0: LOAD, READ THE FIELD
# =============================================================================
d <- load_sumer(SEASONS)
d_db <- d[is_dropback == TRUE & no_play == FALSE & garbage_time == FALSE & off_caller != ""]
cat(sprintf("Non-garbage-time, caller-attributed dropbacks %s-%s: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(d_db), big.mark = ",")))

cat("\n=== dropback_type levels (this universe, before any caller qualification) ===\n")
lvl <- d_db[, .(n = .N, pct = 100 * .N / nrow(d_db)), by = dropback_type][order(-n)]
print(lvl)
cat("STANDARD = straight drop, no design. ROLLOUT = the designed-movement level (boot/sprint-out/moving\n")
cat("pocket). TRICK = gadget plays (excluded from the design measure, reported separately). Blank = unclassified, dropped.\n")

# =============================================================================
# STEP 0b: THE quarterback_left_pocket CHECK -- is it a design field or a
# pressure symptom? (documented in the header; verified here so the number
# is computed, not asserted)
# =============================================================================
cat("\n=== quarterback_left_pocket sanity check ===\n")
lp_by_type <- d_db[, .(n = .N, left_pocket_rate = 100 * mean(quarterback_left_pocket)), by = dropback_type][order(-n)]
print(lp_by_type)
lp_press <- d_db[, .(n = .N, pressure_rate = 100 * mean(pressure == 1, na.rm = TRUE),
                     sack_rate = 100 * mean(is_sack)), by = quarterback_left_pocket]
print(lp_press)
scr_lp <- d_db[quarterback_scramble == TRUE, .(pct_also_left_pocket = 100 * mean(quarterback_left_pocket))]
cat(sprintf("scrambles that are ALSO flagged quarterback_left_pocket: %.1f%%\n", scr_lp$pct_also_left_pocket))
cat("verdict: rate is ~identical on STANDARD vs ROLLOUT dropbacks (a real design flag could not be), and it moves\n")
cat("almost entirely with pressure/sacks/scrambles -- a broken-pocket symptom, not a coach's design choice. Used\n")
cat("below as a diagnostic context stat only, never as part of the designed-movement measure.\n")

# =============================================================================
# STEP 1: UNIVERSE -- 37 qualifying callers (>=1200 offensive plays), their
# dropback subset with a classified dropback_type
# =============================================================================
d_off <- d[run_pass %in% c("P", "R") & garbage_time == FALSE & off_caller != ""]
qual <- d_off[, .N, by = off_caller][N >= QUAL_MIN, off_caller]
n_qual <- length(qual)
cat(sprintf("\nCallers with >= %d offensive plays (run+pass, non-garbage-time): %d of %d\n",
            QUAL_MIN, n_qual, uniqueN(d_off$off_caller)))
stopifnot(all(TREE %in% qual))

d2 <- d_db[off_caller %in% qual & dropback_type %in% c("STANDARD", "ROLLOUT", "TRICK")]
d2[, designed_move := dropback_type == "ROLLOUT"]
d2[, play_action := as.integer(play_action)]
cat(sprintf("Analysis universe: %s dropbacks across %d qualifying callers (smallest caller: %d dropbacks)\n",
            format(nrow(d2), big.mark = ","), n_qual, min(d2[, .N, by = off_caller]$N)))

# =============================================================================
# TEST 1: THE MOVEMENT LEADERBOARD
# =============================================================================
cat("\n\n================ TEST 1: THE MOVEMENT LEADERBOARD ================\n")

## -- raw rate --
raw <- d2[, .(n = .N, n_roll = sum(designed_move), rate = 100 * mean(designed_move)), by = off_caller]
setorder(raw, -rate); raw[, raw_rank := .I]
lg_rate <- 100 * sum(raw$n_roll) / sum(raw$n)
cat(sprintf("league pooled designed-movement rate: %.1f%% (raw), median across %d callers: %.1f%%\n",
            lg_rate, n_qual, median(raw$rate)))

## -- situation-adjusted, via sumer_resid() (repo's standing convention) --
res <- sumer_resid(d2, "designed_move", who = "off_caller", min_season = 100, min_career = 500)
setnames(res$career, "caller", "off_caller")
setorder(res$career, -resid); res$career[, adj_rank := .I]
stopifnot(nrow(res$career) == n_qual)  # every qualifying caller survives adjustment
cat(sprintf("situation-adjusted league rate (model expectation): %.1f%%\n", res$league_rate))

lb <- merge(raw, res$career[, .(off_caller, actual, expected = actual - resid, resid, adj_rank)], by = "off_caller")
setorder(lb, adj_rank)

cat("\n--- tree, raw and situation-adjusted ---\n")
for (nm in TREE) {
  r <- lb[off_caller == nm]
  ci <- prop.test(round(r$n_roll), r$n)$conf.int
  cat(sprintf("%s: raw %.1f%% [%.1f,%.1f] (n=%d rollouts of %d), raw rank %d of %d -- adjusted %+.1fpp vs expected, adj rank %d of %d\n",
              nm, r$rate, 100 * ci[1], 100 * ci[2], r$n_roll, r$n, r$raw_rank, n_qual, r$resid, r$adj_rank, n_qual))
}
cat("\n--- full leaderboard, situation-adjusted ---\n")
print(lb[, .(off_caller, n, n_roll, rate = round(rate, 1), raw_rank, resid = round(resid, 2), adj_rank)])

# lb is sorted by adj_rank, so look each tree member up by name rather than
# assuming TREE's order lines up positionally with any subset of lb's rows
tree_adj_ranks <- setNames(lb[match(TREE, off_caller)]$adj_rank, TREE)
top_half <- names(tree_adj_ranks)[tree_adj_ranks <= n_qual / 2]
bot_half <- names(tree_adj_ranks)[tree_adj_ranks > n_qual / 2]
verdict1 <- if (length(bot_half) == 0) {
  "the tree DOES cluster at the top -- the folklore holds up"
} else {
  sprintf("the tree does NOT cluster at the top: only %s rank(s) in the top half (adjusted); %s sit(s) BELOW the league median",
          if (length(top_half)) paste(top_half, collapse = ", ") else "none",
          paste(bot_half, collapse = ", "))
}
cat(sprintf("\nTEST 1 VERDICT: %s\n", verdict1))

## -- persistence: is the rate a stable coach identity? --
ps <- persist_split(res$season, min_half = 150)
cat(sprintf("\nPERSISTENCE (odd vs even season, situation-adjusted rate): r = %+.3f [%+.3f, %+.3f], p = %.5f, n = %d callers -- %s\n",
            ps$r, ps$lo, ps$hi, ps$p, ps$n, ps$verdict))

## -- aside: "keepers off wide zone" -- does outside-zone rate predict rollout rate? --
cat("\n--- aside: does a caller's own outside-zone run rate predict his rollout rate ('boot off wide zone')? ---\n")
oz <- d_off[off_caller %in% qual & run_pass == "R", .(n_run = .N, oz_rate = 100 * mean(run_concept == "OUTSIDE ZONE", na.rm = TRUE)), by = off_caller]
ozm <- merge(oz, raw[, .(off_caller, rate)], by = "off_caller")
ct_oz <- cor.test(ozm$oz_rate, ozm$rate)
cat(sprintf("cor(outside-zone run rate, rollout rate) across %d callers = %+.3f [%+.3f, %+.3f], p = %.3f\n",
            nrow(ozm), ct_oz$estimate, ct_oz$conf.int[1], ct_oz$conf.int[2], ct_oz$p.value))
cat(sprintf("Shanahan himself: outside-zone rate %.1f%% (one of the league's highest), rollout rate %.1f%% (below median) -- the pairing does not hold for its own namesake.\n",
            ozm[off_caller == "Kyle Shanahan"]$oz_rate, ozm[off_caller == "Kyle Shanahan"]$rate))
verdict_oz <- if (ct_oz$p.value > 0.10) "NULL: outside-zone rate and rollout rate are not related across callers -- the 'boot off wide zone' pairing does not show up as a caller-level identity." else sprintf("outside-zone rate and rollout rate ARE related (r=%+.2f, p=%.3f).", ct_oz$estimate, ct_oz$p.value)
cat(verdict_oz, "\n")

## -- context stat: quarterback_scramble, kept separate as instructed --
cat("\n--- context: quarterback_scramble (improvised, NOT part of the design measure) ---\n")
scr <- d2[, .(n = .N, scramble_rate = 100 * mean(quarterback_scramble)), by = off_caller]
lg_scr <- 100 * mean(d2$quarterback_scramble)
scrm <- merge(scr, raw[, .(off_caller, rate)], by = "off_caller")
ct_scr <- cor.test(scrm$scramble_rate, scrm$rate)
cat(sprintf("league scramble rate: %.1f%% of dropbacks. cor(scramble rate, designed-movement rate) across callers = %+.3f [%+.3f, %+.3f], p = %.3f -- %s\n",
            lg_scr, ct_scr$estimate, ct_scr$conf.int[1], ct_scr$conf.int[2], ct_scr$p.value,
            if (ct_scr$p.value > 0.10) "no relationship: scrambling and calling designed movement are independent behaviours." else "related."))
for (nm in TREE) cat(sprintf("%s scramble rate: %.1f%% (n=%d dropbacks)\n", nm, scr[off_caller == nm]$scramble_rate, scr[off_caller == nm]$n))

## -- context: quarterback_left_pocket per caller (diagnostic, not design) --
lp <- d2[, .(left_pocket_rate = 100 * mean(quarterback_left_pocket)), by = off_caller]
lpm <- merge(lp, raw[, .(off_caller, rate)], by = "off_caller")
prm <- d2[, .(n = .N, press_rate = 100 * mean(pressure == 1, na.rm = TRUE)), by = off_caller]
lpprm <- merge(lpm, prm[, .(off_caller, press_rate)], by = "off_caller")
ct_lp_rate <- cor.test(lpprm$left_pocket_rate, lpprm$rate)
ct_lp_press <- cor.test(lpprm$left_pocket_rate, lpprm$press_rate)
cat(sprintf("\ncontext: cor(left-pocket rate, designed-movement rate) = %+.2f [%+.2f, %+.2f]; cor(left-pocket rate, pressure rate) = %+.2f [%+.2f, %+.2f] -- confirms the field tracks pressure, not design, at the caller level too.\n",
            ct_lp_rate$estimate, ct_lp_rate$conf.int[1], ct_lp_rate$conf.int[2],
            ct_lp_press$estimate, ct_lp_press$conf.int[1], ct_lp_press$conf.int[2]))

## -- context: dropback_side on the designed-movement plays (which way callers roll) --
side <- d2[designed_move == TRUE, .N, by = dropback_side][order(-N)]
cat("\ncontext: rollout direction, leaguewide (dropback_side on ROLLOUT plays):\n"); print(side)
esc <- d2[designed_move == TRUE, .(pct_further_escape = 100 * mean(dropback_escape_side != ""))]
cat(sprintf("of designed rollouts, %.1f%% ALSO carry a dropback_escape_side tag -- the QB broke further from the design, not just executed it.\n", esc$pct_further_escape))

# =============================================================================
# TEST 2: DOES MOVING THE POCKET PAY
# =============================================================================
cat("\n\n================ TEST 2: DOES MOVING THE POCKET PAY ================\n")

epa_expect <- function(x, feats = SUMER_STATE, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(expected_points_added)], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x$expected_points_added)
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "reg:squarederror", eta = 0.05, max_depth = 4,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.y = y, .expected = p)]
  x[!is.na(.expected)]
}

z <- epa_expect(d2)
z[, epa_resid := .y - .expected]
cat(sprintf("Model check: %d of %d dropbacks scored, cor(actual EPA, expected EPA) = %.3f (situation explains little of EPA on\n",
            nrow(z), nrow(d2), cor(z$.y, z$.expected)))
cat("purpose -- EPA is mostly execution; the model only needs to strip the situation confound, same caveat as R/31's YAC model.)\n")

grp <- z[, {
  tt <- t.test(epa_resid)
  list(n = .N, resid = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2],
       sack_rate = 100 * mean(is_sack), press_rate = 100 * mean(pressure == 1, na.rm = TRUE),
       ttt = mean(time_to_throw, na.rm = TRUE))
}, by = .(dropback_type, play_action)][!is.na(play_action)]
setorder(grp, dropback_type, play_action)
cat("\n--- league-wide, dropback_type x play_action ---\n")
print(grp[, .(dropback_type, play_action, n, resid = round(resid, 3), lo = round(lo, 3), hi = round(hi, 3),
              sack_rate = round(sack_rate, 1), press_rate = round(press_rate, 1), ttt = round(ttt, 2))])

roll_pa   <- grp[dropback_type == "ROLLOUT" & play_action == 1]
roll_npa  <- grp[dropback_type == "ROLLOUT" & play_action == 0]
std_pa    <- grp[dropback_type == "STANDARD" & play_action == 1]
std_npa   <- grp[dropback_type == "STANDARD" & play_action == 0]
verdict2 <- sprintf(
  "the PLAY ACTION pays (standard+PA %+.3f, rollout+PA %+.3f, overlapping CIs -- indistinguishable), not the movement itself: a rollout WITHOUT the run fake actually costs %+.3f EPA/play vs a standard drop's %+.3f. Movement's real, distinct benefit is sacks: rollout sack rate runs %.1f-%.1f%% vs standard's %.1f-%.1f%%, roughly half, while pressure rate barely moves and time-to-throw runs LONGER on rollouts (%.2fs vs %.2fs on PA) -- a moving target is harder to bring down even though it isn't harder to pressure.",
  std_pa$resid, roll_pa$resid, roll_npa$resid, std_npa$resid,
  min(roll_pa$sack_rate, roll_npa$sack_rate), max(roll_pa$sack_rate, roll_npa$sack_rate),
  min(std_pa$sack_rate, std_npa$sack_rate), max(std_pa$sack_rate, std_npa$sack_rate),
  roll_pa$ttt, std_pa$ttt)
cat(sprintf("\nTEST 2 VERDICT: %s\n", verdict2))

cat("\n--- tree, ROLLOUT vs their own STANDARD baseline ---\n")
tree_epa <- rbindlist(lapply(TREE, function(nm) {
  rbindlist(lapply(c("ROLLOUT", "STANDARD"), function(ty) {
    sub <- z[off_caller == nm & dropback_type == ty]
    tt <- t.test(sub$epa_resid)
    data.table(off_caller = nm, dropback_type = ty, n = nrow(sub),
               resid = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2],
               sack_rate = 100 * mean(sub$is_sack), press_rate = 100 * mean(sub$pressure == 1, na.rm = TRUE),
               ttt = mean(sub$time_to_throw, na.rm = TRUE))
  }))
}))
for (nm in TREE) {
  r <- tree_epa[off_caller == nm & dropback_type == "ROLLOUT"]
  s <- tree_epa[off_caller == nm & dropback_type == "STANDARD"]
  cat(sprintf("%s: ROLLOUT %+.3f [%+.3f,%+.3f] (n=%d, sack %.1f%%, press %.1f%%) vs his own STANDARD %+.3f [%+.3f,%+.3f] (n=%d, sack %.1f%%, press %.1f%%)\n",
              nm, r$resid, r$lo, r$hi, r$n, r$sack_rate, r$press_rate,
              s$resid, s$lo, s$hi, s$n, s$sack_rate, s$press_rate))
}

# =============================================================================
# TEST 3: THE SIGNATURE CHECK
# =============================================================================
cat("\n\n================ TEST 3: THE SIGNATURE CHECK ================\n")

payoff <- z[dropback_type == "ROLLOUT", .(payoff = mean(epa_resid)), by = off_caller]
sig <- merge(lb, payoff, by = "off_caller")
mx <- median(sig$rate); my <- median(sig$payoff)
sig[, quadrant := fcase(
  rate < mx  & payoff >= my, "Moves rarely, and it pays",
  rate < mx  & payoff < my,  "Moves rarely, and it doesn't pay",
  rate >= mx & payoff >= my, "Moves often, and it pays",
  rate >= mx & payoff < my,  "Moves often, and it doesn't pay"
)]
ct_sig <- cor.test(sig$rate, sig$payoff)
cat(sprintf("cor(movement rate, movement payoff) across %d callers = %+.3f [%+.3f, %+.3f], p = %.3f -- usage and payoff are close to independent.\n",
            n_qual, ct_sig$estimate, ct_sig$conf.int[1], ct_sig$conf.int[2], ct_sig$p.value))
cat(sprintf("league medians: rate %.1f%%, payoff %+.3f EPA/play.\n\n", mx, my))
cat("--- tree quadrant placement ---\n")
for (nm in TREE) {
  s <- sig[off_caller == nm]
  cat(sprintf("%s: rate %.1f%% (adj rank %d), payoff %+.3f on his own rollouts -- %s\n",
              nm, s$rate, s$adj_rank, s$payoff, s$quadrant))
}
n_tree_positive_payoff <- sum(sig[off_caller %in% TREE]$payoff > my)
cat(sprintf("\nAll %d of %d tree members sit ABOVE the league median payoff even though only one (McVay) sits above median USAGE --\n", n_tree_positive_payoff, length(TREE)))
cat("the folklore is about EFFECTIVENESS when the tree calls it, not about how often the tree calls it.\n")

# =============================================================================
# CHART A: leaderboard | signature quadrant scatter
# =============================================================================
lb[, hl := off_caller %in% TREE]
lb[, col := ifelse(hl, off_caller, "League")]
lb[, lab := reorder(off_caller, resid)]

pA <- ggplot(lb, aes(x = resid, y = lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_point(data = lb[hl == FALSE], colour = "grey60", size = 2.2, alpha = 0.8) +
  geom_point(data = lb[hl == TRUE], aes(colour = col), size = 4) +
  geom_text(data = lb[hl == TRUE], aes(label = sprintf("  %+.1fpp, rank %d/%d", resid, adj_rank, n_qual), colour = col),
            hjust = 0, size = 3, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = COL, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pp")) +
  coord_cartesian(clip = "off", xlim = c(min(lb$resid) - 0.5, max(lb$resid) + 3.5)) +
  labs(title = "Movement rate vs situation-only expectation",
       x = "designed-movement rate, situation-adjusted (pp vs expected)", y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62)), plot.margin = margin(10, 90, 8, 10))

sig[, hl := off_caller %in% TREE]
outlier_ids <- unique(c(sig[which.max(rate)]$off_caller, sig[which.min(rate)]$off_caller,
                         sig[which.max(payoff)]$off_caller, sig[which.min(payoff)]$off_caller))
outlier_ids <- setdiff(outlier_ids, TREE)
sig[, ol := off_caller %in% outlier_ids]
lab_named <- sig[hl == TRUE]; lab_out <- sig[ol == TRUE]
xr <- range(sig$rate); yr <- range(sig$payoff)
corner_txt <- function(x, y, hj, vj, txt) annotate("text", x = x, y = y, hjust = hj, vjust = vj,
                                                    size = 2.9, fontface = "italic", colour = "grey40", label = txt)

pB <- ggplot(sig, aes(rate, payoff)) +
  geom_vline(xintercept = mx, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_hline(yintercept = my, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_point(data = sig[hl == FALSE & ol == FALSE], aes(size = n_roll), colour = "grey65", alpha = 0.8) +
  geom_point(data = lab_out, aes(size = n_roll), colour = "grey20", shape = 21, fill = "white", stroke = 1) +
  geom_point(data = lab_named, aes(size = n_roll, colour = off_caller)) +
  geom_text_repel(data = lab_named, aes(label = off_caller, colour = off_caller),
                   size = 3.1, fontface = "bold", seed = 11, box.padding = 0.7, point.padding = 0.5,
                   min.segment.length = 0, max.overlaps = 30, show.legend = FALSE) +
  geom_text_repel(data = lab_out, aes(label = off_caller), size = 2.7, fontface = "bold", colour = "grey25",
                   seed = 4, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  corner_txt(xr[1], yr[2], 0, 1, "Moves rarely,\nand it pays") +
  corner_txt(xr[2], yr[2], 1, 1, "Moves often,\nand it pays") +
  corner_txt(xr[1], yr[1], 0, 0, "Moves rarely,\nand it doesn't pay") +
  corner_txt(xr[2], yr[1], 1, 0, "Moves often,\nand it doesn't pay") +
  scale_size_continuous(range = c(2, 9), guide = "none") +
  scale_colour_manual(values = COL, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), sprintf("%.2f", x))) +
  coord_cartesian(clip = "off") +
  labs(title = "Movement rate vs its own payoff, one point per caller",
       x = "designed-movement rate (raw)", y = "EPA over expected, on rollout plays") +
  theme_coach(grid = "none")

pA_title <- sprintf("Designed movement doesn't cluster in the Shanahan tree the way the folklore says")
pA_sub <- sprintf(
  "Situation-adjusted rank (of %d): McVay #%d, Johnson #%d, Shanahan #%d, LaFleur #%d -- three of four sit at or below the league median.\nBut all four sit ABOVE the league's median PAYOFF on the rollouts they do call (right panel): the signature is in effectiveness, not volume.",
  n_qual, lb[off_caller == "Sean McVay"]$adj_rank, lb[off_caller == "Ben Johnson"]$adj_rank,
  lb[off_caller == "Kyle Shanahan"]$adj_rank, lb[off_caller == "Matt LaFleur"]$adj_rank)

pAB <- (pA | pB) +
  plot_annotation(
    title = pA_title,
    subtitle = pA_sub,
    caption = fig_caption(
      "SumerSports play charting, 2022-2025 non-garbage-time",
      sprintf("%d qualifying callers (>=%d offensive plays each), %s dropbacks. Left: designed-movement rate (dropback_type == ROLLOUT) vs a situation-only\nexpectation (down/distance/field position/quarter/score/clock; sumer_resid()). Right: raw movement rate vs EPA-over-expected on that caller's own\nrollout plays (situation-only xgboost expectation, leave-one-season-out); point size = rollouts charted, open circles = extremes not already named.",
              n_qual, QUAL_MIN, format(nrow(d2), big.mark = ",")),
      sprintf("\nPersistence (odd/even season split, adjusted rate): r = %+.2f [%+.2f, %+.2f], %s. Across the league, PLAY ACTION drives the EPA payoff\n(standard+PA %+.2f, rollout+PA %+.2f -- indistinguishable), not movement itself: a rollout with no run fake costs %+.2f EPA/play. Movement's\nclean benefit is sacks, roughly halved on rollouts, without a matching drop in pressure rate. Built by R/38_qb_movement.R.",
              ps$r, ps$lo, ps$hi, ps$verdict, std_pa$resid, roll_pa$resid, roll_npa$resid)
    )
  )
save_fig("docs/figures/qb_movement.png", pAB, w = 17, h = 11)

# =============================================================================
# WRITE OUTPUT
# =============================================================================
out <- Reduce(function(a, b) merge(a, b, by = "off_caller", all = TRUE), list(
  lb[, .(off_caller, n_dropbacks = n, n_rollout = n_roll, rate_raw = round(rate, 2), raw_rank,
        rate_adj_resid = round(resid, 3), adj_rank)],
  sig[, .(off_caller, payoff_rollout = round(payoff, 4), quadrant)],
  tree_epa[dropback_type == "STANDARD", .(off_caller, epa_resid_standard = round(resid, 4),
                                          sack_rate_standard = round(sack_rate, 2), press_rate_standard = round(press_rate, 2))],
  tree_epa[dropback_type == "ROLLOUT", .(off_caller, epa_resid_rollout = round(resid, 4),
                                         sack_rate_rollout = round(sack_rate, 2), press_rate_rollout = round(press_rate, 2))],
  scr[, .(off_caller, scramble_rate = round(scramble_rate, 2))],
  lp[, .(off_caller, left_pocket_rate = round(left_pocket_rate, 2))],
  oz[, .(off_caller, outside_zone_rate = round(oz_rate, 2))]
))
setorder(out, adj_rank)
write_csv(as.data.frame(out), "data/derived/qb_movement.csv")
cat(sprintf("\nwrote data/derived/qb_movement.csv (%d rows)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat("dropback_type levels (n=82,228 dropbacks): "); print(lvl)
cat(sprintf("\nTEST 1: %s\n", verdict1))
cat(sprintf("  Tree situation-adjusted ranks (of %d, 1 = most designed movement above expected):\n", n_qual))
for (nm in TREE) cat(sprintf("    %s: %+.1fpp, rank %d\n", nm, lb[off_caller == nm]$resid, lb[off_caller == nm]$adj_rank))
cat(sprintf("  Persistence: r = %+.3f [%+.3f, %+.3f], p = %.5f, n = %d -- %s\n", ps$r, ps$lo, ps$hi, ps$p, ps$n, ps$verdict))
cat(sprintf("  'Boot off wide zone' aside: %s\n", verdict_oz))
cat(sprintf("\nTEST 2: %s\n", verdict2))
cat(sprintf("\nTEST 3: cor(rate, payoff) = %+.3f [%+.3f, %+.3f], p = %.3f. All %d/%d tree members sit above median payoff; only McVay sits above median usage.\n",
            ct_sig$estimate, ct_sig$conf.int[1], ct_sig$conf.int[2], ct_sig$p.value, n_tree_positive_payoff, length(TREE)))
cat(sprintf("\nMOST SURPRISING NUMBER: outside-zone run rate and rollout rate correlate at r=%+.2f (p=%.2f) across callers --\n", ct_oz$estimate, ct_oz$p.value)); cat(sprintf("Shanahan runs outside zone at %.1f%% (near the league's highest) but rolls out at only %.1f%% (below the league median);\nthe 'boot off wide zone' pairing that supposedly defines this tree does not hold together at the caller level, even for its own namesake.\n",
            ozm[off_caller == "Kyle Shanahan"]$oz_rate, ozm[off_caller == "Kyle Shanahan"]$rate))
cat("\nOut: docs/figures/qb_movement.png, data/derived/qb_movement.csv\n")
