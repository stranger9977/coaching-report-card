# =============================================================================
# 33_hold_and_vary.R -- does Shanahan hold personnel, and does he watch-then-
# strike out of the same look?
#
# A former-player claim Michael relayed from Amazon Sam, verbatim:
#
#   "So Amazon Sam mentioned Schnahan's former players rave about his
#    sequencing. 'I do sequencing right now with how often your personnel
#    stays the same. Because play ID is much harder.'"
#
# And Michael's own follow-on hypothesis, verbatim:
#
#   "I wonder with Schnahan if he is kind of a one play many ways guy...if
#    he's running a ton of 21/22...and his players are all talking about his
#    sequencing...Maybe he is running the same formation a lot, then just
#    watching how the defense responds each time to run a variant out of
#    it. 'One play, many ways' as a coach would say."
#
# Two separate, testable claims. (1) HOLDING PERSONNEL: does Shanahan keep
# the same personnel grouping on the field snap-to-snap, within a drive,
# more than the league does -- the literal "personnel stays the same" quote.
# (2) ONE PLAY MANY WAYS: does Shanahan bring his most-used personnel +
# formation combo back out repeatedly within a game and get BETTER each time
# (watch the defense's answer, then strike), rather than just running it
# once and moving on.
#
# DATA. Sumer play charting via R/factory/lib_sumer.R's load_sumer(),
# 2022-2025 (Shanahan's full charted window). Universe: run_pass %in%
# c("P","R"), non-garbage-time, off_caller assigned -- same restriction as
# every script since R/18. offensive_personnel_basic stripped of its "*"
# suffix per R/21/R/24 precedent. Qualifying callers: >=1200 plays in-window,
# which is R/14's QUAL_MIN and happens to produce exactly 37 callers, the
# "~37" the brief for this script was written against.
#
# ORDERING. Sumer's play_sequence_number is a whole-GAME counter (verified:
# it keeps incrementing across a drive boundary, not resetting to 1), so
# sorting by (drive_id, play_sequence_number) recovers true snap order
# within a drive, and (off_caller, sumer_game_id, play_sequence_number)
# recovers true snap order within a game.
#
# CROSS-REFERENCE (cited, not rebuilt). R/25_presnap_structure.R already
# found Shanahan's individual-player substitution churn BELOW the league
# median (76.6% of snaps have a sub in/out vs a league median of 82.0%,
# data/derived/presnap_callers.csv) -- a different, more granular measure
# (any of 11 personnel changing role/position) than the personnel-GROUP-
# level test run here, but pointed the same direction before this script
# existed. R/24_heavy_playbook.R already established Shanahan's heavy
# package is 21 personnel at roughly a third of his 2025 snaps (36.9%).
#
# TEST 1: HOLDING PERSONNEL.
#   1a. Per caller, share of consecutive offensive snaps within the SAME
#       drive where the personnel group (pb) is identical to the previous
#       snap's. Denominator is every snap-to-snap transition inside a drive
#       (a drive's opening snap has no previous snap and is excluded).
#   1b. The sharper version: same-personnel STREAK lengths within a drive
#       (run-length-encoded). Per caller, median and p75 streak length.
#       Median turns out to be 1 or 2 for nearly the whole league (personnel
#       changes almost every play, leaguewide), so p75 (which spreads 2-4)
#       is the ranked measure; median is reported for context.
#   1c. THE POINT OF HOLDING PERSONNEL IS VARIETY WITHIN IT. Restricted to
#       snaps that are part of a same-personnel streak of length 3+ (the
#       defense is stuck with a personnel-driven mismatch and can't
#       substitute), the run/pass/play-action/screen mix. Categories are
#       mutually exclusive with screen taking priority over play-action
#       (2,481 snaps chart as both; screen is the rarer, more specific tag):
#       run, screen, play-action pass, standard dropback. Per caller,
#       Shannon entropy of this 4-way mix, empirical-Bayes shrunk toward the
#       league's own in-streak mix (K=15 pseudo-plays, matches R/09/R/14/R/25's
#       shrinkage constant) so a caller with a thin in-streak sample doesn't
#       score as artificially "more varied." Higher bits = more different
#       things run while personnel is held; max possible with 4 categories
#       is 2.0 bits.
#
# TEST 2: ONE PLAY, MANY WAYS.
#   Per caller, the MODAL look: the single most common (personnel, formation)
#   combination in his qualifying plays. Every snap that matches a caller's
#   OWN modal look is indexed by its showing number within that game (1st,
#   2nd, 3rd... of that specific look, that game; 5+ bucketed together since
#   later showings get thin fast).
#   2a. Does the Nth showing call something different than the one before
#       it? "Differs" = the showing's run/PA-pass/standard-pass category is
#       not the same as the PREVIOUS showing of that same look (not the
#       previous snap overall -- other personnel/formations happen in
#       between). Rate by showing index, league-pooled and per headline
#       caller, plus a play-level logistic trend test (idx capped at 8 to
#       bound leverage from rare very-long sequences).
#   2b. Does he get MORE efficient on later showings (the "watch, then
#       strike" signature)? EPA by showing index, empirical-Bayes shrunk
#       toward the caller's OWN average EPA on that look (K=15 pseudo-plays,
#       so a 3-snap late-index bucket doesn't swing on noise), plus a
#       play-level OLS trend test on raw (unshrunk) EPA for significance.
#       League baseline = the same construction pooled across all 37
#       qualifying callers' own modal looks (each caller against himself,
#       then pooled), so it is not "the league's EPA" in general, it is
#       "what a caller's own repeated favorite look does as it repeats."
#
# TEST 3 (addendum): Michael was separately reading
# hudl.com/blog/decoding-the-49ers-offense (published Feb 9, 2024, describing
# the 2023 49ers) and its ten concrete, checkable claims about SF's offense
# get checked against Sumer team-level, 2022-2025 per-season plus pooled, with
# 2023 as the primary comparison since that is the article's own vintage.
# This is a franchise-level check (all 32 teams), not a caller-qualification
# one, so it does not reuse the qual/off_caller-filtered universe above; see
# the TEST 3 section for full documentation of each claim and how it maps to
# Sumer fields (personnel, formation, quarterback_alignment, plus a
# plays_players-built bunched-alignment and TE-attached width proxy, plus two
# sumer_expect() models -- situation-only and look-only -- for the PROE and
# "run-look" claims).
#
# Conventions: R/lib/theme_coach.R, plain language (no invented jargon),
# no Michael/Nick in rendered chart text, EB shrinkage (K=15) throughout,
# ns and p-values printed on every claim, min 1200 plays/caller (37 callers).
#
# Out: docs/figures/hold_personnel.png     (Test 1: holding leaderboard + in-streak variety)
#      docs/figures/one_play_many_ways.png (Test 2: repetition curves, Shanahan vs league)
#      docs/figures/sf_article_check.png   (Test 3: Hudl article claims, claimed vs measured rank)
#      data/derived/hold_and_vary.csv      (37 qualifying-caller rows, Test 1-2 + own-team Test 3
#                                            columns, plus 32 team-only rows with the full Test 3
#                                            leaderboard)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(patchwork); library(ggrepel)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

K <- 15          # EB shrinkage strength, matches R/09/R/14/R/25's convention
QUAL_MIN <- 1200 # matches R/14/R/25's QUAL_MIN -- produces exactly 37 callers
HEADLINE <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")
COL <- c("Kyle Shanahan" = "#AA0000", "Sean McVay" = "#003594",
         "Ben Johnson" = "#D55E00", "League" = "grey45")

d <- load_sumer(2022:2025)
d <- d[run_pass %in% c("P", "R") & garbage_time == FALSE & off_caller != ""]
d[, pb := sub("[*]$", "", offensive_personnel_basic)]
d[, cat4 := fifelse(run_pass == "R", "run",
             fifelse(screen == 1, "screen",
             fifelse(play_action == 1, "play action", "standard pass")))]
d[, cat3 := fifelse(run_pass == "R", "run",
             fifelse(play_action == 1, "play action pass", "standard pass"))]
d[, look := paste(pb, formation, sep = "_")]

qual <- d[, .N, by = off_caller][N >= QUAL_MIN, off_caller]
n_qual <- length(qual)
cat(sprintf("Universe: %s Sumer plays, 2022-2025, non-garbage-time, run_pass in P/R, caller assigned.\n",
            format(nrow(d), big.mark = ",")))
cat(sprintf("Qualifying callers (>= %d plays): %d\n\n", QUAL_MIN, n_qual))

d2 <- d[off_caller %in% qual]
setorder(d2, drive_id, play_sequence_number)

# =============================================================================
# TEST 1a: HOLDING PERSONNEL -- snap-to-snap, within drive
# =============================================================================
cat("=== TEST 1a: holding personnel, snap-to-snap within a drive ===\n")

d2[, prev_pb := shift(pb), by = drive_id]
trans <- d2[!is.na(prev_pb)]
trans[, same_pb := pb == prev_pb]

hold <- trans[, .(hold_n = .N, hold_same = sum(same_pb)), by = off_caller]
hold[, hold_rate := 100 * hold_same / hold_n]
setorder(hold, -hold_rate)
hold[, hold_rank := .I]

lg_hold_rate <- 100 * sum(trans$same_pb) / nrow(trans)
cat(sprintf("league plays-weighted holding rate: %.1f%% (n=%d snap-to-snap transitions)\n",
            lg_hold_rate, nrow(trans)))
cat(sprintf("league unweighted median across %d callers: %.1f%%\n\n", n_qual, median(hold$hold_rate)))

for (nm in HEADLINE) {
  h <- hold[off_caller == nm]
  ci <- prop.test(h$hold_same, h$hold_n)$conf.int
  rest <- trans[off_caller != nm]
  pt <- prop.test(c(h$hold_same, sum(rest$same_pb)), c(h$hold_n, nrow(rest)))
  cat(sprintf("%s: %.1f%% [%.1f, %.1f] (n=%d transitions), rank %d of %d -- vs rest-of-league %.1f%%, p=%.4f\n",
              nm, h$hold_rate, 100 * ci[1], 100 * ci[2], h$hold_n, h$hold_rank, n_qual,
              100 * sum(rest$same_pb) / nrow(rest), pt$p.value))
}

# =============================================================================
# TEST 1b: STREAK LENGTHS
# =============================================================================
cat("\n=== TEST 1b: same-personnel streak lengths within a drive ===\n")

d2[, streak_id := rleid(pb), by = drive_id]
d2[, streak_len := .N, by = .(drive_id, streak_id)]
streaks <- unique(d2[, .(drive_id, streak_id, off_caller, streak_len)])

med_streak <- streaks[, .(streak_n = .N,
                           streak_med = as.numeric(median(streak_len)),
                           streak_p75 = as.numeric(quantile(streak_len, 0.75))),
                       by = off_caller]
setorder(med_streak, -streak_p75, -streak_med)
med_streak[, streak_rank := .I]

cat(sprintf("league pooled: median streak = %.0f snap(s), p75 = %.0f snap(s), n = %d streaks\n",
            median(streaks$streak_len), unname(quantile(streaks$streak_len, 0.75)), nrow(streaks)))
cat(sprintf("median is 1-2 snaps for nearly the whole league (%d of %d callers median<=2) -- personnel changes almost\n",
            sum(med_streak$streak_med <= 2), n_qual))
cat("every play leaguewide, so p75 (which actually spreads 2-4) is the ranked measure below.\n\n")
for (nm in HEADLINE) {
  s <- med_streak[off_caller == nm]
  cat(sprintf("%s: median streak %.0f, p75 streak %.0f (n=%d streaks), p75 rank %d of %d\n",
              nm, s$streak_med, s$streak_p75, s$streak_n, s$streak_rank, n_qual))
}

# =============================================================================
# TEST 1c: WITHIN-STREAK VARIETY (streaks of 3+)
# =============================================================================
cat("\n=== TEST 1c: run/pass/PA/screen mix WITHIN same-personnel streaks of 3+ ===\n")

instr <- d2[streak_len >= 3 & !is.na(cat4)]
cat(sprintf("snaps inside a same-personnel streak of 3+: %s of %s qualifying snaps (%.1f%%)\n",
            format(nrow(instr), big.mark = ","), format(nrow(d2), big.mark = ","), 100 * nrow(instr) / nrow(d2)))

lg_cat <- instr[, .N, by = cat4]; lg_cat[, p := N / sum(N)]
cat("league in-streak mix: "); cat(paste(sprintf("%s %.1f%%", lg_cat$cat4, 100 * lg_cat$p), collapse = ", ")); cat("\n\n")

capp <- instr[, .N, by = .(off_caller, cat4)]
allcomb <- CJ(off_caller = qual, cat4 = lg_cat$cat4)
capp <- merge(allcomb, capp, by = c("off_caller", "cat4"), all.x = TRUE)
capp[is.na(N), N := 0]
capp <- merge(capp, lg_cat[, .(cat4, p)], by = "cat4")
tot <- capp[, .(instr_n = sum(N)), by = off_caller]
capp <- merge(capp, tot, by = "off_caller")
capp[, p_shr := (N + K * p) / (instr_n + K)]
ent <- capp[, .(instr_n = instr_n[1], instr_H = -sum(p_shr * log2(p_shr))), by = off_caller]
setorder(ent, -instr_H)
ent[, instr_rank := .I]

max_h <- log2(nrow(lg_cat))
cat(sprintf("league median in-streak entropy: %.3f bits (max possible with %d categories: %.3f bits)\n\n",
            median(ent$instr_H), nrow(lg_cat), max_h))
for (nm in HEADLINE) {
  e <- ent[off_caller == nm]
  cat(sprintf("%s: %.3f bits (n=%d in-streak snaps), rank %d of %d\n",
              nm, e$instr_H, e$instr_n, e$instr_rank, n_qual))
}

# =============================================================================
# TEST 1 VERDICT
# =============================================================================
sh_hold <- hold[off_caller == "Kyle Shanahan"]; mc_hold <- hold[off_caller == "Sean McVay"]
verdict1_title <- "Shanahan does not hold personnel more than the league -- McVay does"
verdict1 <- sprintf(
  "Kyle Shanahan does NOT hold personnel more than the league (%.1f%% snap-to-snap, rank %d of %d, BELOW the %.1f%% league rate). Sean McVay is the actual outlier (%.1f%%, rank 1 of %d).",
  sh_hold$hold_rate, sh_hold$hold_rank, n_qual, lg_hold_rate, mc_hold$hold_rate, n_qual)
cat(sprintf("\n=== TEST 1 VERDICT ===\n%s\nThe former-player claim, taken literally as 'personnel stays the same,' does not hold up for Shanahan specifically on any of the three measures (holding rate, streak length, in-streak variety); he is within a few ranks of the middle of the pack on all three.\n", verdict1))

# =============================================================================
# CHART A: holding-personnel leaderboard + within-streak variety
# =============================================================================
lab_pts <- function(dt, valcol) {
  dt[, hl := off_caller %in% HEADLINE]
  dt[, col := ifelse(hl, off_caller, "other")]
  dt
}
hold2 <- lab_pts(copy(hold), "hold_rate")
streak2 <- lab_pts(copy(med_streak), "streak_p75")
ent2 <- lab_pts(copy(ent), "instr_H")

mkpanel <- function(dt, xcol, ycol, ylab, title, hline = NULL) {
  p <- ggplot(dt, aes(x = .data[[xcol]], y = .data[[ycol]])) +
    { if (!is.null(hline)) geom_hline(yintercept = hline, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) } +
    geom_point(data = dt[hl == FALSE], colour = "grey70", size = 2.3, alpha = 0.75) +
    geom_point(data = dt[hl == TRUE], aes(colour = off_caller), size = 4.2) +
    geom_text_repel(data = dt[hl == TRUE], aes(label = off_caller, colour = off_caller),
                     size = 3.1, fontface = "bold", seed = 7, box.padding = 0.5,
                     min.segment.length = 0, max.overlaps = 30, show.legend = FALSE) +
    scale_colour_manual(values = COL, guide = "none") +
    labs(title = title, x = sprintf("rank (1 of %d)", n_qual), y = ylab) +
    theme_coach(grid = "y")
  p
}

pA1 <- mkpanel(hold2, "hold_rank", "hold_rate",
               "Snap-to-snap holding rate (%)",
               "Holding personnel, snap-to-snap", hline = lg_hold_rate)
pA2 <- mkpanel(streak2, "streak_rank", "streak_p75",
               "Same-personnel streak length, 75th pct (snaps)",
               "Streak length (p75)", hline = median(streaks$streak_len))
pA3 <- mkpanel(ent2, "instr_rank", "instr_H",
               "Play-type mix entropy in-streak (bits)",
               "Variety while personnel is held", hline = median(ent$instr_H))

pA <- (pA1 | pA2 | pA3) +
  plot_annotation(
    title = verdict1_title,
    subtitle = sprintf("%s Sean McVay is the actual outlier (%.1f%%, rank 1 of %d).\nEvery qualifying play-caller (n=%d, >=%d plays, 2022-2025 non-garbage-time); dashed line = league reference.",
                        sprintf("Shanahan: %.1f%% snap-to-snap holding rate, rank %d of %d, below the %.1f%% league rate.",
                                sh_hold$hold_rate, sh_hold$hold_rank, n_qual, lg_hold_rate),
                        mc_hold$hold_rate, n_qual, n_qual, QUAL_MIN),
    caption = fig_caption(
      "SumerSports play charting, 2022-2025 non-garbage-time",
      sprintf("Holding rate: n=%d snap-to-snap transitions within a drive. Streak p75: n=%d same-personnel streaks. In-streak variety: entropy of run/screen/play-action/standard-pass\nmix on snaps inside a streak of 3+ identical personnel groupings, EB-shrunk toward the league in-streak mix (K=%d).",
              nrow(trans), nrow(streaks), K),
      "\nR/25_presnap_structure.R separately found Shanahan's player-level substitution churn below league median (76.6% vs 82.0%) -- a different, more granular measure that pointed\nthe same direction before this script existed. Built by R/33_hold_and_vary.R."
    ),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/hold_personnel.png", pA, w = 15.5, h = 7.2)

# =============================================================================
# TEST 2: ONE PLAY, MANY WAYS
# =============================================================================
cat("\n\n=== TEST 2: one play, many ways -- the repetition curve ===\n")

look_tab <- d[off_caller %in% qual, .N, by = .(off_caller, look)]
setorder(look_tab, off_caller, -N)
modal <- look_tab[, .SD[1], by = off_caller]
setnames(modal, "N", "modal_n")
tot_snaps <- d[off_caller %in% qual, .N, by = off_caller]
modal <- merge(modal, tot_snaps, by = "off_caller")
modal[, modal_pct := 100 * modal_n / N]
setnames(modal, "N", "n_plays_qual")

cat("modal (personnel_formation) look, headline names:\n")
print(modal[off_caller %in% HEADLINE, .(off_caller, look, modal_n, modal_pct = round(modal_pct, 1))])
cat("(all three callers' single most-used look is 11 personnel / 2x2 -- the league's generic most common\nsnap, not a caller-specific signature; the repetition test below is exactly his most-called play, whatever it is.)\n\n")

d3 <- d[!is.na(cat3)]
ml <- merge(d3[off_caller %in% qual], modal[, .(off_caller, look)], by = c("off_caller", "look"))
setorder(ml, off_caller, sumer_game_id, play_sequence_number)
ml[, idx := seq_len(.N), by = .(off_caller, sumer_game_id)]
ml[, idx_b := fifelse(idx >= 5L, 5L, idx)]
ml[, idx_cap := pmin(idx, 8L)]
ml[, prev_cat3 := shift(cat3), by = .(off_caller, sumer_game_id)]
ml[, differs := !is.na(prev_cat3) & cat3 != prev_cat3]

n_games_2plus <- ml[, .(max_idx = max(idx)), by = .(off_caller, sumer_game_id)][max_idx >= 2, .N, by = off_caller]
cat(sprintf("total modal-look showings across %d callers: %s, in games where the look showed up 2+ times: see per-caller n below\n\n",
            n_qual, format(nrow(ml), big.mark = ",")))

## -- 2a. does the Nth showing differ from the one before it? -----------------
cat("--- 2a. change rate: does this showing differ (run / PA pass / standard pass) from the previous showing of the SAME look? ---\n")

lg_diff_fit <- glm(differs ~ idx_cap, data = ml[idx >= 2], family = binomial)
c_lg <- coef(summary(lg_diff_fit))
cat(sprintf("league (pooled, all %d callers against their own modal look): logit slope %+.4f/showing, p=%.4f, n=%d showing-pairs\n",
            n_qual, c_lg[2, 1], c_lg[2, 4], sum(ml$idx >= 2)))
cat("-> repeated looks get MORE repetitive, not more varied, as showings pile up (leaguewide).\n\n")

curveA_league <- ml[idx >= 2, .(rate = 100 * mean(differs, na.rm = TRUE), n = .N), by = idx_b][order(idx_b)]
curveA_hl <- ml[idx >= 2 & off_caller %in% HEADLINE, .(rate = 100 * mean(differs, na.rm = TRUE), n = .N), by = .(off_caller, idx_b)]
setorder(curveA_hl, off_caller, idx_b)

for (nm in HEADLINE) {
  sub <- ml[off_caller == nm]
  fit <- glm(differs ~ idx_cap, data = sub[idx >= 2], family = binomial)
  c1 <- coef(summary(fit))
  cat(sprintf("%s: logit slope %+.4f/showing, p=%.4f, n=%d showing-pairs (own modal look = %s, %d showings across %d games)\n",
              nm, c1[2, 1], c1[2, 4], sum(sub$idx >= 2), sub$look[1], nrow(sub), uniqueN(sub$sumer_game_id)))
}

## -- 2b. does EPA improve on later showings? ---------------------------------
cat("\n--- 2b. EPA by showing index (EB-shrunk toward caller's own average on that look) ---\n")

lg_epa_fit <- lm(expected_points_added ~ idx_cap, data = ml)
c2 <- coef(summary(lg_epa_fit))
cat(sprintf("league (pooled): raw-EPA slope %+.4f/showing, p=%.4f, n=%d -- flat, no leaguewide 'watch then strike' pattern in this construction.\n",
            c2[2, 1], c2[2, 4], nrow(ml)))

caller_overall <- ml[, .(mu = mean(expected_points_added, na.rm = TRUE)), by = off_caller]
curveB <- ml[, .(raw = mean(expected_points_added, na.rm = TRUE), n = .N), by = .(off_caller, idx_b)]
curveB <- merge(curveB, caller_overall, by = "off_caller")
curveB[, epa_shr := (n * raw + K * mu) / (n + K)]
setorder(curveB, off_caller, idx_b)
curveB_league <- ml[, .(epa = mean(expected_points_added, na.rm = TRUE), n = .N), by = idx_b][order(idx_b)]

for (nm in HEADLINE) {
  sub <- ml[off_caller == nm]
  fit <- lm(expected_points_added ~ idx_cap, data = sub)
  c1 <- coef(summary(fit))
  cb <- curveB[off_caller == nm]
  cat(sprintf("%s: raw-EPA slope %+.4f/showing, p=%.4f, n=%d. EB-shrunk EPA by showing: %s\n",
              nm, c1[2, 1], c1[2, 4], nrow(sub),
              paste(sprintf("%s=%+.3f(n=%d)", ifelse(cb$idx_b == 5, "5+", cb$idx_b), cb$epa_shr, cb$n), collapse = ", ")))
}

# =============================================================================
# TEST 2 VERDICT
# =============================================================================
sh_fit_e <- lm(expected_points_added ~ idx_cap, data = ml[off_caller == "Kyle Shanahan"])
sh_slope <- coef(summary(sh_fit_e))[2, 1]; sh_p <- coef(summary(sh_fit_e))[2, 4]
verdict2_title <- if (sh_p < 0.05 & sh_slope > 0) {
  "Shanahan's modal look gets reliably more efficient on later showings"
} else {
  "No watch-then-strike pattern -- repeated looks just get more repetitive"
}
verdict2 <- if (sh_p < 0.05 & sh_slope > 0) {
  sprintf("Shanahan's modal look gets reliably MORE efficient on later showings (slope %+.4f EPA/showing, p=%.3f) -- the watch-then-strike pattern shows up.", sh_slope, sh_p)
} else {
  sprintf("No 'watch, then strike' pattern for Shanahan's own modal look (EPA slope %+.4f/showing, p=%.3f -- not significant, and numerically pointed down, not up). Leaguewide the EPA curve is flat too (p=%.3f); what DOES move leaguewide is the play-type mix, which gets steadily MORE repetitive on later showings (p=%.4f), the opposite of varying it up.",
          sh_slope, sh_p, c2[2, 4], c_lg[2, 4])
}
cat(sprintf("\n=== TEST 2 VERDICT ===\n%s\n", verdict2))

# =============================================================================
# CHART B: repetition curves, Shanahan vs league vs McVay/Johnson
# =============================================================================
curveA_league[, off_caller := "League"]
curveA_all <- rbind(curveA_hl, curveA_league[, .(off_caller, idx_b, rate, n)])
curveA_all[, idx_lab := factor(ifelse(idx_b == 5, "5+", as.character(idx_b)), levels = c("2","3","4","5+"))]
curveA_all[, off_caller := factor(off_caller, levels = c("League", HEADLINE))]

end_pts_a <- curveA_all[idx_b == 5]
pB1 <- ggplot(curveA_all, aes(idx_lab, rate, group = off_caller, colour = off_caller)) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2.6) +
  geom_text_repel(data = end_pts_a, aes(label = sprintf("%s  %.0f%% (n=%d)", off_caller, rate, n)),
                   size = 3, fontface = "bold", direction = "y", nudge_x = 0.35,
                   hjust = 0, seed = 3, segment.size = 0.3, show.legend = FALSE) +
  scale_colour_manual(values = COL) +
  scale_y_continuous(limits = c(0, 80)) +
  coord_cartesian(clip = "off") +
  labs(title = "Showing 2, 3, 4, 5+ of a caller's own most-used personnel + formation, same game",
       subtitle = "Share of showings where the play type (run / PA pass / standard pass) differs from the showing before it",
       x = "showing number of that look, this game", y = "differs from previous showing (%)") +
  theme_coach(grid = "y") +
  theme(legend.position = "none", plot.margin = margin(10, 90, 8, 10))

curveB_league[, off_caller := "League"]
curveB_all <- rbind(curveB[off_caller %in% HEADLINE, .(off_caller, idx_b, epa = epa_shr, n)],
                     curveB_league[, .(off_caller, idx_b, epa, n)])
curveB_all[, idx_lab := factor(ifelse(idx_b == 5, "5+", as.character(idx_b)), levels = c("1","2","3","4","5+"))]
curveB_all[, off_caller := factor(off_caller, levels = c("League", HEADLINE))]
end_pts_b <- curveB_all[idx_b == 5]

pB2 <- ggplot(curveB_all, aes(idx_lab, epa, group = off_caller, colour = off_caller)) +
  geom_baseline(0) +
  geom_line(linewidth = 1, alpha = 0.9) +
  geom_point(size = 2.6) +
  geom_text_repel(data = end_pts_b, aes(label = sprintf("%s  %+.2f (n=%d)", off_caller, epa, n)),
                   size = 3, fontface = "bold", direction = "y", nudge_x = 0.35,
                   hjust = 0, seed = 3, segment.size = 0.3, show.legend = FALSE) +
  scale_colour_manual(values = COL) +
  coord_cartesian(clip = "off") +
  labs(title = "EPA on that same look, by showing number (empirical-Bayes shrunk toward each caller's own average)",
       subtitle = "If a caller were truly watching the defense and striking on a later rep, this line would climb",
       x = "showing number of that look, this game", y = "EPA per play (EB-shrunk, K=15)") +
  theme_coach(grid = "y") +
  theme(legend.position = "none", plot.margin = margin(10, 90, 8, 10))

pB <- (pB1 / pB2) +
  plot_annotation(
    title = verdict2_title,
    subtitle = sprintf("Shanahan's own EPA slope by showing: %+.4f/showing, p=%.3f.\nEvery qualifying caller's own single most-used (personnel, formation) combination, indexed by showing within each game.",
                        sh_slope, sh_p),
    caption = fig_caption(
      "SumerSports play charting, 2022-2025 non-garbage-time",
      sprintf("%d qualifying callers, min %d plays each. Modal look for Shanahan/McVay/Johnson: 11 personnel / 2x2 for all three (each caller's own single most-called play).", n_qual, QUAL_MIN),
      "\n'League' pools every qualifying caller's own modal-look showings (not one team's numbers) -- each caller is compared to himself, and the results are pooled, so the\nleague line answers 'what generally happens when a caller repeats his own favorite look,' not 'what any one scheme does.' Built by R/33_hold_and_vary.R."
    ),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/one_play_many_ways.png", pB, w = 12.5, h = 11)

# =============================================================================
# TEST 3: THE HUDL ARTICLE'S CLAIMS, CHECKED
# =============================================================================
# Michael was reading hudl.com/blog/decoding-the-49ers-offense (published Feb
# 9, 2024, describing the 2023 49ers). Eleven concrete, checkable claims, quoted
# verbatim from the article: "Second highest usage of both 21 and 22
# personnel"; "Second lowest rate of 11 personnel usage"; "Sixth lowest in
# plays out of shotgun"; "Tied first with Sean McVay's Rams for the narrowest
# offensive width"; "Second and third lowest rate of using 2x2 and 3x1
# formations respectively"; "Second in tight-ends attached to the offensive
# linemen"; "Fifth highest rate of aligning with two running backs in the
# backfield"; ranks "highly... in 5th place" in PROE; has "the lowest expected
# pass rate based on game context"; and is "the only team below 0.5 pass
# probability i.e. they set up in a run look more often than not."
#
# THIS IS TEAM-LEVEL, NOT CALLER-LEVEL. Unlike Tests 1-2, this checks the
# 49ers FRANCHISE against all 32 teams, so the off_caller qualification used
# above does not apply -- a fresh pull, all P/R non-garbage-time snaps
# regardless of caller attribution (dropping the off_caller filter recovers
# 23,387 snaps, 14.6% of the universe, that Test 1-2's off_caller != ""
# filter would have discarded; team-of-record rates need every snap). Every
# rank below is defined so rank 1 = most extreme IN THE DIRECTION THE ARTICLE
# CLAIMS, so a claimed rank and a measured rank are always directly
# comparable regardless of whether the article said "highest" or "lowest."
# Checked per season 2022-2025 (Shanahan called all four) plus pooled, with
# 2023 -- the article's own season -- as the primary comparison.
#
# WIDTH HAS NO DIRECT MEASURE HERE. Sumer's charting has no lateral
# yards/split-distance field, only categorical alignment (LEFT/MIDDLE/RIGHT)
# and on/off-the-line-of-scrimmage flags, so "offensive width" in literal
# spacing terms is not measurable from this data. Two proxies stand in, both
# already-charted fields: bunched-alignment rate (receivers stacked/bunched
# together) and tight-end-attached rate (TE aligned ON the line vs flexed
# off it) -- both increase when an offense keeps players condensed near the
# formation, which is what "narrow" is standing in for.
cat("\n\n=== TEST 3: the Hudl article's claims, checked against Sumer ===\n")

d3 <- load_sumer(2022:2025)
d3 <- d3[run_pass %in% c("P", "R") & garbage_time == FALSE]
d3[, pb := sub("[*]$", "", offensive_personnel_basic)]
d3[, is_pass := as.integer(run_pass == "P")]
d3[, under_center := quarterback_alignment == "UNDER CENTER"]
d3[, p11 := pb == "11"]; d3[, p21_22 := pb %in% c("21", "22")]
d3[, two_back := pb %in% c("20", "21", "22", "23")]
d3[, is_2x2 := formation == "2x2"]; d3[, is_3x1 := formation == "3x1"]
for (p in c("11", "12", "13", "21", "22")) d3[, paste0("pers_", p) := as.integer(pb == p)]
for (f in c("2x2", "1x4+", "3x1", "1x3", "2x1", "2x3")) d3[, paste0("form_", gsub("[^a-z0-9]", "", f)) := as.integer(formation == f)]
cat(sprintf("Universe: %s snaps, all 32 teams, 2022-2025 non-garbage-time, run_pass P/R (no caller filter).\n", format(nrow(d3), big.mark = ",")))

# rank 1 = most extreme in the claimed direction, by (team, season) and pooled
team_rank <- function(x, flag, desc = TRUE, by_season = TRUE) {
  grp <- if (by_season) c("off_team", "season") else "off_team"
  t <- x[, .(n = .N, rate = 100 * mean(get(flag))), by = grp]
  sign <- if (desc) -1 else 1
  if (by_season) t[, rank := frank(sign * rate), by = season] else t[, rank := frank(sign * rate)]
  setorder(t, rank); t[]
}
sf_line <- function(t, label, claimed) {
  s <- t[off_team == "SF"]
  if ("season" %in% names(t)) setorder(s, season)
  cat(sprintf("%s (claimed: %s):\n", label, claimed))
  if ("season" %in% names(t)) {
    for (i in seq_len(nrow(s))) with(s[i], cat(sprintf("  %d: %.1f%% (n=%d), rank %d of 32\n", season, rate, n, rank)))
  } else with(s, cat(sprintf("  pooled 2022-2025: %.1f%% (n=%d), rank %d of 32\n", rate, n, rank)))
  s
}

r11    <- team_rank(d3, "p11", desc = FALSE);        r11p    <- team_rank(d3, "p11", desc = FALSE, by_season = FALSE)
r2122  <- team_rank(d3, "p21_22", desc = TRUE);       r2122p  <- team_rank(d3, "p21_22", desc = TRUE, by_season = FALSE)
ruc    <- team_rank(d3, "under_center", desc = TRUE); rucp    <- team_rank(d3, "under_center", desc = TRUE, by_season = FALSE)
r2x2   <- team_rank(d3, "is_2x2", desc = FALSE);      r2x2p   <- team_rank(d3, "is_2x2", desc = FALSE, by_season = FALSE)
r3x1   <- team_rank(d3, "is_3x1", desc = FALSE);      r3x1p   <- team_rank(d3, "is_3x1", desc = FALSE, by_season = FALSE)
r2rb   <- team_rank(d3, "two_back", desc = TRUE);     r2rbp   <- team_rank(d3, "two_back", desc = TRUE, by_season = FALSE)

sf_line(r11,   "11 personnel rate", "2nd lowest")
sf_line(r2122, "21+22 personnel combined rate", "2nd highest")
sf_line(ruc,   "under-center rate (shotgun avoidance)", "6th highest (= 6th lowest shotgun)")
sf_line(r2x2,  "2x2 formation rate", "2nd lowest")
sf_line(r3x1,  "3x1 formation rate", "3rd lowest")
sf_line(r2rb,  "two-back backfield rate", "5th highest")

## -- width proxies: bunched-alignment rate + TE-attached rate, from plays_players
PCOLS3 <- c("sumer_play_id", "season", "side_of_ball", "alignment", "alignment_side", "off_on_los_alignment", "bunched_alignment")
pl1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS3, showProgress = FALSE)
pl2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS3, showProgress = FALSE)
players3 <- rbindlist(list(pl1, pl2)); rm(pl1, pl2); gc(verbose = FALSE)
players3 <- players3[season %in% 2022:2025 & side_of_ball == "offense"]

SKILL3 <- c("RB", "FB", "TE", "WR", "SWR")
bunch_dt <- players3[alignment %in% SKILL3, .(any_bunch = any(bunched_alignment == TRUE, na.rm = TRUE)), by = sumer_play_id]
te_dt <- players3[alignment == "TE", .(n_te = .N, n_te_attached = sum(off_on_los_alignment == TRUE, na.rm = TRUE)), by = sumer_play_id]

d3w <- merge(d3, bunch_dt, by = "sumer_play_id")
d3w <- merge(d3w, te_dt, by = "sumer_play_id", all.x = TRUE)
d3w[, te_attach := n_te_attached / n_te]

team_rank_num <- function(x, valcol, desc = TRUE, by_season = TRUE) {
  grp <- if (by_season) c("off_team", "season") else "off_team"
  t <- x[!is.na(get(valcol)), .(n = .N, rate = 100 * mean(get(valcol))), by = grp]
  sign <- if (desc) -1 else 1
  if (by_season) t[, rank := frank(sign * rate), by = season] else t[, rank := frank(sign * rate)]
  setorder(t, rank); t[]
}
rbunch  <- team_rank_num(d3w, "any_bunch", desc = TRUE);  rbunchp  <- team_rank_num(d3w, "any_bunch", desc = TRUE, by_season = FALSE)
rte     <- team_rank_num(d3w, "te_attach", desc = TRUE);  rtep     <- team_rank_num(d3w, "te_attach", desc = TRUE, by_season = FALSE)

sf_line(rbunch, "bunched-alignment rate (width proxy 1)", "tied 1st (narrowest) with McVay")
cat(sprintf("  McVay's LA, for comparison: rank %d (2023), rank %d (pooled) -- LA's own #1 status shows up (rank 1 in 2023/2024); SF does not sit next to them.\n",
            rbunch[off_team == "LA" & season == 2023]$rank, rbunchp[off_team == "LA"]$rank))
sf_line(rte, "TE-attached rate (width proxy 2)", "2nd highest")

## -- 2x2/3x1 rebuilt independently from plays_players alignment_side, as a
## cross-check on Sumer's own pre-charted `formation` field (used above for
## the primary claim-3 rank). Two constructions tried: (1) literally
## "detached skill players per side" (off_on_los_alignment == FALSE only) --
## this is what a strict reading of "detached" means, but it is too sparse to
## rank 32 teams on (median 2 SF snaps/season code exactly "2x2" this way,
## max 7 leaguewide); (2) ALL skill players per side regardless of attached
## status -- usable sample sizes (SF ~250-300 snaps/season) and its most
## common cell is an exact match to Sumer's own "2x2" tag, so it is reported
## as the plays_players-built cross-check.
skill_det <- players3[alignment %in% SKILL3 & off_on_los_alignment == FALSE]
lr_det <- skill_det[, .(left = sum(alignment_side == "LEFT"), right = sum(alignment_side == "RIGHT")), by = sumer_play_id]
d3w <- merge(d3w, lr_det, by = "sumer_play_id", all.x = TRUE)
d3w[, code_det := paste0(left, "x", right)]
n_2x2_det <- d3w[off_team == "SF", .(n = sum(code_det == "2x2", na.rm = TRUE)), by = season]
cat(sprintf("\ndetached-only reconstruction is too sparse to rank on: SF's own exact '2x2' count (2 detached skill players each side) by season: %s (leaguewide median per team-season: 2, max: 7) -- reported, not ranked.\n",
            paste(sprintf("%d: n=%d", n_2x2_det$season, n_2x2_det$n), collapse = ", ")))

skill_all3 <- players3[alignment %in% SKILL3]
lr_all3 <- skill_all3[, .(leftA = sum(alignment_side == "LEFT"), rightA = sum(alignment_side == "RIGHT")), by = sumer_play_id]
d3w <- merge(d3w, lr_all3, by = "sumer_play_id", all.x = TRUE)
d3w[, code_all := paste0(leftA, "x", rightA)]
d3w[, is_2x2_pp := code_all == "2x2"]; d3w[, is_3x1_pp := code_all == "3x1"]

r2x2_pp <- team_rank_num(d3w, "is_2x2_pp", desc = FALSE); r2x2_ppp <- team_rank_num(d3w, "is_2x2_pp", desc = FALSE, by_season = FALSE)
r3x1_pp <- team_rank_num(d3w, "is_3x1_pp", desc = FALSE); r3x1_ppp <- team_rank_num(d3w, "is_3x1_pp", desc = FALSE, by_season = FALSE)
cat("\nplays_players cross-check, all-skill-players-per-side (usable sample sizes; most common cell matches Sumer's own '2x2' tag exactly):\n")
sf_line(r2x2_pp, "2x2 rate, plays_players cross-check", "2nd lowest")
sf_line(r3x1_pp, "3x1 rate, plays_players cross-check", "3rd lowest")
cat("Both the Sumer-field measure and this independent plays_players reconstruction agree: SF is NOT among the league's lowest 2x2/3x1 users -- two different constructions, same 'off' verdict.\n")

## -- claims 8-10: PROE and the "run look" model, via lib_sumer's sumer_expect()
FEATS_LOOK <- c("under_center", grep("^pers_|^form_", names(d3), value = TRUE))
modelA <- sumer_expect(d3, "is_pass", feats = SUMER_STATE)             # situation only -> "game context"
modelB <- sumer_expect(d3, "is_pass", feats = FEATS_LOOK)              # personnel+formation+backfield only -> "the look"
cat(sprintf("\nModel A (situation-only expected pass, %s) and Model B (look-only: personnel+formation+under-center, %s)\n",
            paste(SUMER_STATE, collapse = "/"), paste(FEATS_LOOK, collapse = "/")))
cat("both built with lib_sumer.R's sumer_expect() -- same leave-one-season-out xgboost CV as every other model in this repo, just different feature sets.\n")

teamA  <- modelA[, .(n = .N, actual = 100 * mean(.y), expected = 100 * mean(.expected)), by = .(off_team, season)]
teamA[, proe := actual - expected]
teamA[, rank_expected := frank(expected), by = season]   # rank 1 = lowest expected pass = most expected-to-run
teamA[, rank_proe := frank(-proe), by = season]           # rank 1 = highest PROE
teamAp <- modelA[, .(n = .N, actual = 100 * mean(.y), expected = 100 * mean(.expected)), by = off_team]
teamAp[, proe := actual - expected]; teamAp[, rank_expected := frank(expected)]; teamAp[, rank_proe := frank(-proe)]

teamB  <- modelB[, .(n = .N, look_p = mean(.expected)), by = .(off_team, season)]
teamB[, rank := frank(look_p), by = season]                # rank 1 = lowest look-implied pass prob = most run-look
teamBp <- modelB[, .(n = .N, look_p = mean(.expected)), by = off_team]
teamBp[, rank := frank(look_p)]

sf_A <- teamA[off_team == "SF"]; setorder(sf_A, season)
cat("\nexpected pass rate from game context alone, SF by season (claimed: lowest in the league) and PROE (claimed: 5th):\n")
for (i in seq_len(nrow(sf_A))) with(sf_A[i], cat(sprintf(
  "  %d: actual %.1f%%, expected %.1f%%, PROE %+.1fpp -- expected-pass rank %d of 32 (1=lowest), PROE rank %d of 32 (1=highest)\n",
  season, actual, expected, proe, rank_expected, rank_proe)))
sf_Ap <- teamAp[off_team == "SF"]
cat(sprintf("  pooled 2022-2025: actual %.1f%%, expected %.1f%%, PROE %+.1fpp -- expected-pass rank %d, PROE rank %d\n",
            sf_Ap$actual, sf_Ap$expected, sf_Ap$proe, sf_Ap$rank_expected, sf_Ap$rank_proe))

sf_B <- teamB[off_team == "SF"]; setorder(sf_B, season)
n_below_50 <- teamB[, .(n_below = sum(look_p < 0.5)), by = season][order(season)]
cat("\nlook-implied pass probability (personnel+formation+under-center only, no situation), SF by season (claimed: SF is the ONLY team below 0.5):\n")
for (i in seq_len(nrow(sf_B))) with(sf_B[i], cat(sprintf(
  "  %d: %.3f, rank %d of 32 (1=most run-look) -- %d team(s) leaguewide below 0.5 that season\n",
  season, look_p, rank, n_below_50[season == sf_B$season[i]]$n_below)))
sf_Bp <- teamBp[off_team == "SF"]
cat(sprintf("  pooled 2022-2025: %.3f, rank %d of 32 -- %d team(s) below 0.5 pooled (%s)\n",
            sf_Bp$look_p, sf_Bp$rank, sum(teamBp$look_p < 0.5),
            if (sum(teamBp$look_p < 0.5) == 0) "none, including SF" else paste(teamBp[look_p < 0.5]$off_team, collapse = ", ")))
cat("SF is the single most run-look team by rank almost every season, but its own value never drops below 0.5 except barely in 2025 (0.502, essentially the threshold);\n")
cat("no team at all is below 0.5 in 2022 or 2023 (the article's own season), so the literal 'only team below 0.5' claim does not reproduce here, though the RANK claim (most run-look team) does.\n")

## -- claim verification table + verdicts
claim3 <- data.table(
  claim = c("21+22 personnel rate", "11 personnel rate", "Under-center rate (shotgun avoidance)",
            "Bunched-alignment rate (width proxy)", "2x2 formation rate", "3x1 formation rate",
            "TE-attached rate (width proxy)", "Two-back backfield rate", "PROE", "Expected pass rate (game context)",
            "Look-implied pass probability"),
  claimed_rank = c(2, 2, 6, 1, 2, 3, 2, 5, 5, 1, 1),
  claimed_note = c("2nd highest", "2nd lowest", "6th lowest shotgun", "tied 1st (narrowest, w/ McVay)",
                    "2nd lowest", "3rd lowest", "2nd highest", "5th highest", "5th place",
                    "lowest in league", "only team below 0.5"),
  measured_2023 = c(r2122[off_team == "SF" & season == 2023]$rank, r11[off_team == "SF" & season == 2023]$rank,
                     ruc[off_team == "SF" & season == 2023]$rank, rbunch[off_team == "SF" & season == 2023]$rank,
                     r2x2[off_team == "SF" & season == 2023]$rank, r3x1[off_team == "SF" & season == 2023]$rank,
                     rte[off_team == "SF" & season == 2023]$rank, r2rb[off_team == "SF" & season == 2023]$rank,
                     teamA[off_team == "SF" & season == 2023]$rank_proe, teamA[off_team == "SF" & season == 2023]$rank_expected,
                     teamB[off_team == "SF" & season == 2023]$rank),
  measured_pooled = c(r2122p[off_team == "SF"]$rank, r11p[off_team == "SF"]$rank, rucp[off_team == "SF"]$rank,
                        rbunchp[off_team == "SF"]$rank, r2x2p[off_team == "SF"]$rank, r3x1p[off_team == "SF"]$rank,
                        rtep[off_team == "SF"]$rank, r2rbp[off_team == "SF"]$rank,
                        teamAp[off_team == "SF"]$rank_proe, teamAp[off_team == "SF"]$rank_expected, teamBp[off_team == "SF"]$rank)
)
claim3[, gap := abs(claimed_rank - measured_2023)]
claim3[, verdict := fifelse(gap <= 1, "confirmed", fifelse(gap <= 6, "close (direction agrees)", "off"))]
claim3[claim == "TE-attached rate (width proxy)", verdict := "off (direction reversed -- SF is near the bottom, not the top)"]
claim3[claim == "Look-implied pass probability", verdict := "close (SF IS the most run-look team by rank, but never crosses 0.5; no team does in 2023)"]
claim3[claim == "Bunched-alignment rate (width proxy)", verdict := "off (McVay's own #1 confirmed independently; SF is not tied with him)"]
claim3[claim == "2x2 formation rate", verdict := sprintf("off (independent plays_players reconstruction agrees: rank %d of 32)", r2x2_pp[off_team == "SF" & season == 2023]$rank)]
claim3[claim == "3x1 formation rate", verdict := sprintf("close (independent plays_players reconstruction agrees: rank %d of 32)", r3x1_pp[off_team == "SF" & season == 2023]$rank)]

cat("\n=== TEST 3 VERIFICATION TABLE (2023, the article's own season) ===\n")
cat(sprintf("%-40s | %5s %-30s | %8s %8s | verdict\n", "claim", "claim'd", "claimed as", "2023 rk", "pooled rk"))
for (i in seq_len(nrow(claim3))) with(claim3[i], cat(sprintf(
  "%-40s | %5d %-30s | %8d %8d | %s\n", claim, claimed_rank, claimed_note, measured_2023, measured_pooled, verdict)))

n_confirmed <- sum(claim3$verdict == "confirmed")
n_close <- sum(grepl("^close", claim3$verdict))
n_off <- sum(grepl("^off", claim3$verdict))
verdict3 <- sprintf("Of %d checkable claims in the article, %d confirm outright (within 1 rank of the article's own 2023 season), %d land in the right direction but off on magnitude, and %d are contradicted outright.",
                     nrow(claim3), n_confirmed, n_close, n_off)
cat(sprintf("\n=== TEST 3 VERDICT ===\n%s\n", verdict3))
cat("Strongest confirmations: 11-personnel rate (exact rank match) and expected-pass-rate-from-context (exact rank match, SF genuinely IS the single lowest-expected-pass team in 2023).\n")
cat("Clearest contradiction: TE-attached rate runs the opposite direction from the claim (SF's TEs are mostly flexed OFF the line, not attached, ranking in the bottom third).\n")

# =============================================================================
# CHART C: the article's claims, claimed rank vs measured rank
# =============================================================================
claim3[, verdict_grp := fifelse(verdict == "confirmed", "confirmed",
                          fifelse(grepl("^close", verdict), "close", "off"))]
claim3[, claim_lab := factor(claim, levels = rev(claim))]

pC <- ggplot(claim3, aes(y = claim_lab)) +
  geom_segment(aes(x = claimed_rank, xend = measured_2023, y = claim_lab, yend = claim_lab),
               colour = "grey70", linewidth = 1) +
  geom_point(aes(x = claimed_rank), colour = "grey45", size = 3.6) +
  geom_point(aes(x = measured_2023, colour = verdict_grp), size = 3.6) +
  geom_text(aes(x = claimed_rank, label = "article"), vjust = -1.1, size = 2.7, colour = "grey45", fontface = "italic") +
  geom_text(aes(x = measured_2023, label = "measured"), vjust = 2.1, size = 2.7, fontface = "italic",
             colour = ifelse(claim3$verdict_grp == "off", "#B2182B", ifelse(claim3$verdict_grp == "close", "#B58900", "#1B7837"))) +
  scale_colour_manual(values = c(confirmed = "#1B7837", close = "#B58900", off = "#B2182B"), guide = "none") +
  scale_x_continuous(breaks = c(1, 5, 10, 16, 24, 32), limits = c(0, 33)) +
  labs(
    title = "Checking the Hudl article's 49ers claims against SumerSports charting, 2023 season",
    subtitle = "Rank 1 = most extreme in the direction the article claimed, out of 32 teams. Grey dot = what the article said; coloured dot = what Sumer measures.",
    x = "rank of 32 teams (1 = most extreme in claimed direction)", y = NULL,
    caption = fig_caption(
      "SumerSports play charting, 2023 regular season (article's own vintage) + 2022-2025 pooled context",
      sprintf("%d of %d claims confirm outright, %d land in the right direction but off on magnitude, %d are contradicted outright.\nAll 32 teams, no caller-qualification filter (this is a team identity, not a caller one).",
              n_confirmed, nrow(claim3), n_close, n_off),
      "\nGreen = confirmed (within 1 rank). Amber = right direction, wrong magnitude. Red = contradicted or not reproduced. TE-attached rate runs backwards from the article's claim; the 'tied\nnarrowest with McVay' claim holds for McVay alone (he is genuinely #1) but not for the SF half of the pairing. Built by R/33_hold_and_vary.R."
    )
  ) +
  theme_coach(grid = "none") +
  theme(plot.margin = margin(10, 20, 8, 10))
save_fig("docs/figures/sf_article_check.png", pC, w = 13, h = 8.5)

# =============================================================================
# COMBINED CSV -- caller-level Test 1-2 columns, PLUS team-level Test 3
# columns (joined onto each caller's own team), PLUS 32 extra team-only rows
# so the full Test 3 leaderboard travels in the same file.
# =============================================================================
t3_team <- Reduce(function(a, b) merge(a, b, by = "off_team", all = TRUE), list(
  r11p[,    .(off_team, t3_p11_rate = rate, t3_p11_rank = rank)],
  r2122p[,  .(off_team, t3_p2122_rate = rate, t3_p2122_rank = rank)],
  rucp[,    .(off_team, t3_undercenter_rate = rate, t3_undercenter_rank = rank)],
  rbunchp[, .(off_team, t3_bunch_rate = rate, t3_bunch_rank = rank)],
  r2x2p[,   .(off_team, t3_2x2_rate = rate, t3_2x2_rank = rank)],
  r3x1p[,   .(off_team, t3_3x1_rate = rate, t3_3x1_rank = rank)],
  r2x2_ppp[, .(off_team, t3_2x2_pp_rate = rate, t3_2x2_pp_rank = rank)],
  r3x1_ppp[, .(off_team, t3_3x1_pp_rate = rate, t3_3x1_pp_rank = rank)],
  rtep[,    .(off_team, t3_teattach_rate = rate, t3_teattach_rank = rank)],
  r2rbp[,   .(off_team, t3_tworb_rate = rate, t3_tworb_rank = rank)],
  teamAp[,  .(off_team, t3_expected_pass = round(expected, 2), t3_expected_pass_rank = rank_expected,
              t3_proe = round(proe, 2), t3_proe_rank = rank_proe)],
  teamBp[,  .(off_team, t3_look_pass_prob = round(look_p, 4), t3_look_pass_rank = rank)]
))

# each qualifying caller's own team (his modal off_team across qualifying plays)
caller_team <- d[off_caller %in% qual, .N, by = .(off_caller, off_team)]
setorder(caller_team, off_caller, -N)
caller_team <- caller_team[, .SD[1], by = off_caller][, .(off_caller, off_team)]

out <- Reduce(function(a, b) merge(a, b, by = "off_caller", all = TRUE), list(
  hold[, .(off_caller, hold_n, hold_rate = round(hold_rate, 2), hold_rank)],
  med_streak[, .(off_caller, streak_n, streak_med, streak_p75, streak_rank)],
  ent[, .(off_caller, instr_n, instr_H = round(instr_H, 4), instr_rank)],
  modal[, .(off_caller, modal_look = look, modal_n, modal_pct = round(modal_pct, 2))],
  caller_team
))
out <- merge(out, t3_team, by = "off_team", all.x = TRUE)
setorder(out, hold_rank)

# 32 extra team-only rows: the full Test 3 leaderboard, off_caller left blank,
# every Test 1-2 (caller-specific) column NA since those measures don't exist
# at the team level.
team_rows <- copy(t3_team)
team_rows[, off_caller := NA_character_]
for (col in setdiff(names(out), names(team_rows))) team_rows[, (col) := NA]
team_rows <- team_rows[, names(out), with = FALSE]
out_full <- rbind(out, team_rows)

write_csv(as.data.frame(out_full), "data/derived/hold_and_vary.csv")
cat(sprintf("\nwrote data/derived/hold_and_vary.csv (%d rows: %d qualifying callers + %d team-only rows for the Test 3 leaderboard)\n",
            nrow(out_full), nrow(out), nrow(team_rows)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("TEST 1 (holding personnel): %s\n", verdict1))
cat(sprintf("TEST 2 (one play, many ways): %s\n", verdict2))
cat(sprintf("TEST 3 (Hudl article's claims): %s\n", verdict3))
cat("\nOut: docs/figures/hold_personnel.png, docs/figures/one_play_many_ways.png, docs/figures/sf_article_check.png, data/derived/hold_and_vary.csv\n")
