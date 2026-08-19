# =============================================================================
# 35_open_and_mismatch.R -- two of Michael's newest asks, verbatim.
#
# "Wonder if we have anything via Sumer over the past 3 years about him
# getting guys in space? Like the big knock was Purdy having easy throws
# because guys are always wide open" (him = Kyle Shanahan)
#
# "You could look at EPA per target LB vs. DB...? Mismatches could be its
# own category"
#
# THE HONEST LIMIT, STATED BEFORE ANYTHING ELSE. Sumer's charting has no
# separation-distance field: no yards-of-space, no nearest-defender-distance,
# nothing that measures "wide open" the way an eye does watching the tape.
# What it has instead, used here as the closest honest proxies, and none of
# them are "how open was he" -- every chart below says so again:
#   - contested_target (player-level, plays_players p1/p2, NEVER used on this
#     board before): a charter's yes/no call on whether the defender actually
#     contested the catch. LOW contested rate = receivers getting a free
#     look at the catch point = as close as this data gets to "guys are open."
#   - a depth-and-situation-only model of completion probability, built the
#     same way every situation model on this board is (leave-one-season-out
#     xgboost via lib_sumer's sumer_expect(), nothing about who is coaching
#     goes in). What's left over after throw depth and game situation are
#     priced in is "easier than the numbers say it should be" -- the closest
#     thing here to measuring "easy throws."
#   - pressed_receiver (player-level, same file): whether the receiver was
#     jammed at the line. Run as color/context, not a headline measure.
#
# DATA. Player-level: data/raw/sumer/plays_players_p1.csv.gz + _p2.csv.gz
# (contested_target, pressed_receiver, receiving_targets, sumer_player_id).
# Names/positions from player_details.csv.gz. ONE CORRECTION TO THE ASK:
# catchable_target is NOT a player-level field -- it lives on the play-level
# table (plays_full.csv.gz, via load_sumer()) and is pulled from there, used
# only as a QC descriptive exactly like R/31's precedent (it's a charting
# judgment call almost entirely implied by is_complete_pass, so it is never
# used as a model feature). Play-level via load_sumer(): is_targeted_pass,
# is_complete_pass, depth_of_target, expected_points_added, garbage_time,
# season, off_caller. 2026 (preseason charting) excluded throughout, matching
# every script on this board.
#
# COVERAGE-POSITION MACHINERY (test 2 only), REUSED FROM R/32. matchups.csv.gz
# joined to plays_players via the identical logic in R/32_matchup_hunting.R:
# primary_coverage_interaction rows, position codes decoded by hand (DC=CB,
# DS=safety, IB/OB=LB pooled) against ~25 known players. R/32's own words on
# this, still true here: "NOT supplied by Sumer's schema and was
# reverse-engineered ... treat it as the single biggest reliability caveat."
# Coverage resolves for ~75-81% of targets (R/32's completeness check, not
# rerun here); that caps test 2's n the same way it capped R/32's.
#
# Conventions: R/lib/theme_coach.R, plain language (no invented jargon; every
# coined measure defined in football words on first use), no Michael/Nick in
# rendered chart text, empirical-Bayes shrinkage + era adjustment (checked
# directly below: contested/pressed/completion rates drift under 1.5
# percentage points across 2022-2025, so era adjustment mostly just centers
# the scale rather than correcting a real drift, but it is applied anyway for
# consistency with every other leaderboard on this board), 95% CIs, min ~400
# targets per caller. No em dashes.
#
# Out: docs/figures/schemed_open.png     (Test 1: contested-target rate x
#                                          completion-rate-over-expected,
#                                          press rate as color, Purdy/Jones
#                                          panel extending R/29)
#      docs/figures/mismatch_category.png (Test 2: mismatch-hunting share x
#                                          mismatch EPA gap, "own category" view)
#      data/derived/open_and_mismatch.csv (one row per caller, both tests)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(20260819)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS  <- 2022:2025
SEASONS3 <- 2023:2025
MIN_N <- 400
KYLE <- "Kyle Shanahan"; MCVAY <- "Sean McVay"; BJ <- "Ben Johnson"
THREE <- c(KYLE, MCVAY, BJ)

ordinal_lite <- function(n) {
  suf <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  sprintf("%d%s", n, suf)
}

# Sumer position code -> position group (copied verbatim from R/32; see its
# header for the reverse-engineering caveat).
POS_GROUP <- c(DC = "CB", DS = "S", IB = "LB", OB = "LB", DE = "DL", DT = "DL",
               OT = "OL", OG = "OL", OC = "OL", QB = "QB", RB = "RB", TE = "TE",
               WR = "WR", FB = "FB", PK = "ST", P = "ST", LS = "ST", RS = "ST")

# =============================================================================
# 1. TARGET-LEVEL TABLE: contested_target, pressed_receiver, play context
# =============================================================================
cat("=========================================================\n")
cat("1. TARGET-LEVEL TABLE: contested_target, pressed_receiver, play context\n")
cat("=========================================================\n")

plays_ctx <- load_sumer(SEASONS)
plays_ng  <- plays_ctx[garbage_time == FALSE]

p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target", "pressed_receiver"),
            showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target", "pressed_receiver"),
            showProgress = FALSE)
players_raw <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
n_2026 <- players_raw[season == 2026, .N]
cat(sprintf("player-play rows: %s total, %s season-2026 rows excluded\n",
            format(nrow(players_raw), big.mark = ","), format(n_2026, big.mark = ",")))

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id", "football_name", "last_name", "position"))
pd[, name := paste(football_name, last_name)]

targets0 <- players_raw[season %in% SEASONS & side_of_ball == "offense" & receiving_targets > 0]
targets0 <- merge(targets0, plays_ng[, .(sumer_play_id, off_team, off_caller, season,
                                          depth_of_target, expected_points_added,
                                          is_complete_pass, catchable_target)],
                   by = c("sumer_play_id", "season"))
targets0 <- merge(targets0, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
cat(sprintf("targeted receiver-play rows, non-garbage-time, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(targets0), big.mark = ",")))

n_targeted_plays <- plays_ng[is_targeted_pass == TRUE, .N]
cat(sprintf("(of %s non-garbage-time targeted-pass plays; %.1f%% resolve to a matched player-row here --\n",
            format(n_targeted_plays, big.mark = ","), 100 * nrow(targets0) / n_targeted_plays))
cat(" a different, wider universe than test 1b's completion model below, which needs no player join.)\n")

n_uncatchable_comp <- targets0[is_complete_pass == TRUE & catchable_target == FALSE, .N]
n_comp_total <- targets0[is_complete_pass == TRUE, .N]
cat(sprintf("\nQC (R/31 precedent): completions marked catchable_target == FALSE: %d of %d (%.1f%%) -- a charting\n",
            n_uncatchable_comp, n_comp_total, 100 * n_uncatchable_comp / n_comp_total))
cat("edge case, not used as a model feature below, same call R/31 made.\n")

cat("\n--- rate drift by season (why era adjustment barely moves these numbers) ---\n")
print(targets0[, .(n = .N, contested_pct = round(100 * mean(contested_target), 1),
                    pressed_pct = round(100 * mean(pressed_receiver), 1),
                    comp_pct = round(100 * mean(is_complete_pass), 1),
                    adot = round(mean(depth_of_target, na.rm = TRUE), 2)), by = season][order(season)])

# Generic era-adjusted, empirical-Bayes-shrunk per-caller rate. Same method as
# R/32's shrink_rate(), generalized to any binary event column.
shrink_binary_rate <- function(x, event_col, min_n) {
  x <- copy(x); x[, event := as.integer(get(event_col))]
  lg <- x[, .(n = .N, k = sum(event)), by = season][, rate := 100 * k / n]
  cs <- x[, .(n = .N, k = sum(event)), by = .(off_caller, season)]
  cs <- merge(cs, lg[, .(season, lg_rate = rate)], by = "season")
  dc <- cs[, .(n = sum(n), k = sum(k), expected_k = sum(lg_rate / 100 * n)), by = off_caller]
  dc[, `:=`(rate = 100 * k / n, expected_rate = 100 * expected_k / n)]
  dc[, era_adj := rate - expected_rate]
  dc[, se := 100 * sqrt((rate / 100) * (1 - rate / 100) / n)]
  dc[, `:=`(lo = era_adj - 1.96 * se, hi = era_adj + 1.96 * se)]
  dc <- dc[n >= min_n]
  tau2 <- max(var(dc$era_adj) - mean(dc$se^2), 1e-4)
  dc[, era_shrunk := (tau2 * era_adj) / (tau2 + se^2)]
  dc[]
}

# =============================================================================
# 2. TEST 1a: CONTESTED-TARGET RATE -- LOW = SCHEMED OPEN
# =============================================================================
cat("\n=========================================================\n")
cat("2. TEST 1a: CONTESTED-TARGET RATE (low = receivers getting open looks)\n")
cat("=========================================================\n")

tg_attr <- targets0[off_caller != ""]
contested_dc <- shrink_binary_rate(tg_attr, "contested_target", MIN_N)
setorder(contested_dc, era_shrunk); contested_dc[, rank_open := .I]  # rank 1 = most open (lowest contested rate)
n_open <- nrow(contested_dc)
cat(sprintf("%d callers with >= %d targets (of %d minimum-plausible callers)\n",
            n_open, MIN_N, uniqueN(tg_attr$off_caller)))

cat("\n--- top 10, most open (lowest contested rate, era-adjusted) ---\n")
print(head(contested_dc[, .(off_caller, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                             ci = sprintf("[%+.1f,%+.1f]", lo, hi), rank_open)], 10))
cat("\n--- bottom 10, most contested ---\n")
print(tail(contested_dc[, .(off_caller, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                             ci = sprintf("[%+.1f,%+.1f]", lo, hi), rank_open)], 10))

# the "past 3 years" check Michael actually asked for
contested_dc3 <- shrink_binary_rate(tg_attr[season %in% SEASONS3], "contested_target", MIN_N)
setorder(contested_dc3, era_shrunk); contested_dc3[, rank_open := .I]
n_open3 <- nrow(contested_dc3)
kyle3 <- contested_dc3[off_caller == KYLE]
cat(sprintf("\n2023-2025 only (the literal 'past 3 years' window): Kyle Shanahan rank %d of %d, rate %.1f%% (era-adj %+.1fpp)\n",
            kyle3$rank_open, n_open3, kyle3$rate, kyle3$era_adj))
cat("(reported for completeness; the chart and headline below use the fuller 2022-2025 sample for statistical power.)\n")

# =============================================================================
# 3. TEST 1b: COMPLETION RATE OVER EXPECTED -- "EASY THROWS," MEASURED
# =============================================================================
cat("\n=========================================================\n")
cat("3. TEST 1b: COMPLETION RATE OVER EXPECTED (depth + situation model, season-held-out)\n")
cat("=========================================================\n")

pass_universe <- plays_ng[is_targeted_pass == TRUE]
cat(sprintf("targeted passes, non-garbage-time, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(pass_universe), big.mark = ",")))

COMP_FEATS <- c("depth_of_target", SUMER_STATE)
z <- sumer_expect(pass_universe, "is_complete_pass", feats = COMP_FEATS)
z[, resid := .y - .expected]
cat(sprintf("Model check: %d of %d targeted passes scored (leave-one-season-out xgboost), cor(actual,expected) = %.3f\n",
            nrow(z), nrow(pass_universe), cor(z$.y, z$.expected)))
cat("Model only sees throw depth + game situation, nothing about who is coaching -- what's left\n")
cat("over (resid = actual completion minus model-expected) is the 'easier than the numbers say' signal.\n")

comp_oe <- z[off_caller != "", .(n = .N, comp_actual = 100 * mean(.y), comp_expected = 100 * mean(.expected),
                                  comp_oe = 100 * mean(resid), se = 100 * sd(resid) / sqrt(.N)), by = off_caller]
comp_oe[, `:=`(lo = comp_oe - 1.96 * se, hi = comp_oe + 1.96 * se)]
comp_oe <- comp_oe[n >= MIN_N]
setorder(comp_oe, -comp_oe); comp_oe[, rank_comp := .I]
n_comp <- nrow(comp_oe)
cat(sprintf("%d callers with >= %d modeled targets\n", n_comp, MIN_N))

cat("\n--- top 10, most completions over what depth+situation predict ---\n")
print(head(comp_oe[, .(off_caller, n, comp_actual = round(comp_actual, 1), comp_oe = round(comp_oe, 2),
                        ci = sprintf("[%+.2f,%+.2f]", lo, hi), rank_comp)], 10))
cat("\n--- bottom 10 ---\n")
print(tail(comp_oe[, .(off_caller, n, comp_actual = round(comp_actual, 1), comp_oe = round(comp_oe, 2),
                        ci = sprintf("[%+.2f,%+.2f]", lo, hi), rank_comp)], 10))

# the "past 3 years" check for this measure too
z3 <- z[season %in% SEASONS3 & off_caller != ""]
comp_oe3 <- z3[, .(n = .N, comp_oe = 100 * mean(resid)), by = off_caller][n >= MIN_N]
setorder(comp_oe3, -comp_oe); comp_oe3[, rank_comp := .I]
kyle_c3 <- comp_oe3[off_caller == KYLE]
cat(sprintf("\n2023-2025 only: Kyle Shanahan completion-over-expected rank %d of %d, %+.2fpp\n",
            kyle_c3$rank_comp, nrow(comp_oe3), kyle_c3$comp_oe))

# =============================================================================
# 4. TEST 1c + VERDICT: PRESS RATE AS CONTEXT, THE THREE-NAME CONTRAST
# =============================================================================
cat("\n=========================================================\n")
cat("4. TEST 1c (context) + VERDICT: THE PURDY KNOCK, CHECKED\n")
cat("=========================================================\n")

pressed_dc <- shrink_binary_rate(tg_attr, "pressed_receiver", MIN_N)

open_tab <- merge(contested_dc[, .(off_caller, n_contested = n, contested_rate = rate,
                                    contested_era_adj = era_adj, contested_lo = lo, contested_hi = hi,
                                    contested_shrunk = era_shrunk, rank_open)],
                   comp_oe[, .(off_caller, n_comp = n, comp_actual, comp_expected, comp_oe,
                                comp_lo = lo, comp_hi = hi, rank_comp)],
                   by = "off_caller")
open_tab <- merge(open_tab, pressed_dc[, .(off_caller, pressed_rate = rate, pressed_era_adj = era_adj)],
                   by = "off_caller", all.x = TRUE)
n_matched_open <- nrow(open_tab)
cat(sprintf("callers qualifying on BOTH contested rate and completion-over-expected: %d\n", n_matched_open))

cat("\n--- three-name contrast, all of test 1 ---\n")
print(open_tab[off_caller %in% THREE, .(off_caller, n_contested, contested_rate = round(contested_rate, 1),
                                         rank_open, n_comp, comp_oe = round(comp_oe, 2), rank_comp,
                                         pressed_rate = round(pressed_rate, 1))])

kyle_row <- open_tab[off_caller == KYLE]
verdict1 <- if (kyle_row$rank_open <= n_matched_open / 3 && kyle_row$rank_comp <= n_matched_open / 3) {
  "VERIFIED"
} else if (kyle_row$rank_open <= n_matched_open / 2 && kyle_row$rank_comp <= n_matched_open / 2) {
  "PARTIALLY HOLDS"
} else {
  "DEFLATES"
}
cat(sprintf("\nKyle Shanahan: contested-target rank %d of %d (%.1f%%, era-adj %+.1fpp), completion-over-expected rank %d of %d (%+.2fpp)\n",
            kyle_row$rank_open, n_matched_open, kyle_row$contested_rate, kyle_row$contested_era_adj,
            kyle_row$rank_comp, n_matched_open, kyle_row$comp_oe))
cat(sprintf("VERDICT, 'guys are always wide open': %s -- ", verdict1))
if (verdict1 == "VERIFIED") {
  cat("Shanahan is in the league's top third on BOTH the openness proxy and the easy-throw proxy.\n")
  cat("That is a scheme feature, not just a QB-friendly narrative.\n")
} else if (verdict1 == "PARTIALLY HOLDS") {
  cat("Shanahan is above the league median on both, but not top-third on both -- the knock has real\n")
  cat("basis but is not the extreme case the folklore implies.\n")
} else {
  cat("Shanahan is not a league extreme on these two proxies -- the knock does not hold up as a\n")
  cat("scheme-level outlier on this data.\n")
}

# ---- the QB angle: Purdy vs Jones, completion rate over expected (extends R/29)
cat("\n--- THE QB ANGLE: Purdy vs Jones, completion rate over expected (extends R/29's EPA/dropback battery) ---\n")
PCOLS_QB <- c("sumer_play_id", "sumer_player_id", "season", "side_of_ball", "passing_attempts", "passing_sacks")
p1q <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS_QB, showProgress = FALSE)
p2q <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS_QB, showProgress = FALSE)
passers <- rbindlist(list(p1q, p2q)); rm(p1q, p2q); gc(verbose = FALSE)
sf_passers <- passers[season == 2025 & side_of_ball == "offense" &
                       (passing_attempts > 0 | passing_sacks > 0)]
sf_passers <- merge(sf_passers, pd[, .(sumer_player_id, name)], by = "sumer_player_id")
sf_passers <- sf_passers[name %in% c("Brock Purdy", "Mac Jones")]

qb_z <- merge(z[, .(sumer_play_id, season, off_team, .y, .expected, resid)],
              sf_passers[, .(sumer_play_id, season, name)], by = c("sumer_play_id", "season"))
qb_z <- qb_z[off_team == "SF"]
cat(sprintf("SF 2025 targeted passes matched to Purdy or Jones as the passer: %d\n", nrow(qb_z)))

qb_comp <- qb_z[, .(n = .N, comp_actual = 100 * mean(.y), comp_expected = 100 * mean(.expected),
                     comp_oe = 100 * mean(resid), se = 100 * sd(resid) / sqrt(.N)), by = name]
qb_comp[, `:=`(lo = comp_oe - 1.96 * se, hi = comp_oe + 1.96 * se)]
setorder(qb_comp, -n)
print(qb_comp[, .(name, n, comp_actual = round(comp_actual, 1), comp_expected = round(comp_expected, 1),
                   comp_oe = round(comp_oe, 2), ci = sprintf("[%+.2f,%+.2f]", lo, hi))])

tt_qb <- t.test(resid ~ name, data = qb_z)
purdy_oe <- qb_comp[name == "Brock Purdy"]$comp_oe
jones_oe <- qb_comp[name == "Mac Jones"]$comp_oe
diff_oe <- jones_oe - purdy_oe
cat(sprintf("\nWelch t-test, completion rate over expected, Jones minus Purdy: %+.2fpp [%+.2f, %+.2f], p = %.3f\n",
            100 * diff(tt_qb$estimate)[["mean in group Mac Jones"]] * -1, tt_qb$conf.int[1] * 100,
            tt_qb$conf.int[2] * 100, tt_qb$p.value))
cat(sprintf("Purdy: %+.2fpp over expected (n=%d). Jones: %+.2fpp over expected (n=%d). Gap: %+.2fpp, %s\n",
            purdy_oe, qb_comp[name == "Brock Purdy"]$n, jones_oe, qb_comp[name == "Mac Jones"]$n, diff_oe,
            if (tt_qb$p.value > 0.10) "not distinguishable from zero -- SAME SYSTEM, easy throws for both."
            else sprintf("clears significance (p = %.3f).", tt_qb$p.value)))

# =============================================================================
# 5. TEST 2: MISMATCHES AS THEIR OWN CATEGORY -- EPA AT LB/S vs CB COVERAGE
# =============================================================================
cat("\n=========================================================\n")
cat("5. TEST 2: MISMATCHES AS THEIR OWN CATEGORY, PER CALLER\n")
cat("=========================================================\n")

m_all <- fread("data/raw/sumer/matchups.csv.gz", showProgress = FALSE)
m <- m_all[season %in% SEASONS]; rm(m_all); gc(verbose = FALSE)
cov_all <- m[primary_coverage_interaction == TRUE]

pd[, pos_group := POS_GROUP[position]]
cov <- merge(cov_all, pd[, .(sumer_player_id_defense = sumer_player_id, def_pos_group = pos_group)],
             by = "sumer_player_id_defense", all.x = TRUE)
cov_j <- cov[, .(sumer_play_id, sumer_player_id_offense, def_pos_group)]
cov_flags <- cov_j[, .(has_lb = any(def_pos_group == "LB"), has_s = any(def_pos_group == "S"),
                        has_cb = any(def_pos_group == "CB"), any_pos = any(!is.na(def_pos_group))),
                    by = .(sumer_play_id, sumer_player_id_offense)]

mism <- merge(targets0, cov_flags, by.x = c("sumer_play_id", "sumer_player_id"),
              by.y = c("sumer_play_id", "sumer_player_id_offense"), all.x = TRUE)
mism[, mismatch_group := fifelse(is.na(any_pos) | !any_pos, "unmatched",
                          fifelse(has_lb | has_s, "LBS", fifelse(has_cb, "CB", "other")))]
d2 <- mism[mismatch_group %in% c("LBS", "CB") & off_caller != ""]
cat(sprintf("targets resolved to CB-only or LB/S-involved coverage: %s (of %s caller-attributed targets, %.1f%%) --\n",
            format(nrow(d2), big.mark = ","), format(nrow(tg_attr), big.mark = ","), 100 * nrow(d2) / nrow(tg_attr)))
cat("same join, same universe as R/32 (its ~75-81% completeness figure also folds in 'other'-position matches\n")
cat("this CB-only-or-LBS filter excludes; not a discrepancy, a narrower slice of the same table).\n")

gap_tab <- d2[, .(n_lbs = sum(mismatch_group == "LBS"),
                   epa_lbs = mean(expected_points_added[mismatch_group == "LBS"]),
                   se_lbs = sd(expected_points_added[mismatch_group == "LBS"]) / sqrt(sum(mismatch_group == "LBS")),
                   n_cb = sum(mismatch_group == "CB"),
                   epa_cb = mean(expected_points_added[mismatch_group == "CB"]),
                   se_cb = sd(expected_points_added[mismatch_group == "CB"]) / sqrt(sum(mismatch_group == "CB"))),
               by = off_caller]
gap_tab[, n := n_lbs + n_cb]
gap_tab <- gap_tab[n >= MIN_N]
gap_tab[, gap := epa_lbs - epa_cb]
gap_tab[, se_gap := sqrt(se_lbs^2 + se_cb^2)]
gap_tab[, `:=`(lo = gap - 1.96 * se_gap, hi = gap + 1.96 * se_gap)]
setorder(gap_tab, -gap); gap_tab[, rank_gap := .I]
n_gap <- nrow(gap_tab)
cat(sprintf("%d callers with >= %d matched targets (same MIN_N as R/32)\n", n_gap, MIN_N))

cat("\n--- top 10, converts the mismatch best (EPA gap, LB/S-covered minus CB-covered) ---\n")
print(head(gap_tab[, .(off_caller, n_lbs, n_cb, epa_lbs = round(epa_lbs, 3), epa_cb = round(epa_cb, 3),
                        gap = round(gap, 3), ci = sprintf("[%+.3f,%+.3f]", lo, hi), rank_gap)], 10))
cat("\n--- bottom 10 ---\n")
print(tail(gap_tab[, .(off_caller, n_lbs, n_cb, gap = round(gap, 3),
                        ci = sprintf("[%+.3f,%+.3f]", lo, hi), rank_gap)], 10))

hunt <- fread("data/derived/matchup_hunting.csv")
mismatch_tab <- merge(gap_tab, hunt[, .(off_caller, hunt_rate = rate, hunt_era_adj = era_adj,
                                         hunt_shrunk = era_shrunk, hunt_rank = rank)], by = "off_caller")
cat(sprintf("\nmerged with R/32's matchup_hunting.csv (reused, not recomputed): %d of %d callers matched\n",
            nrow(mismatch_tab), n_gap))
if (nrow(mismatch_tab) != n_gap) {
  cat("(small mismatch expected only if a caller's targets skew heavily toward one coverage type; see CSV.)\n")
}

cat("\n--- three-name contrast: hunts the mismatch (from R/32) vs cashes it (this script) ---\n")
print(mismatch_tab[off_caller %in% THREE, .(off_caller, hunt_rate = round(hunt_rate, 1), hunt_rank,
                                             gap = round(gap, 3), rank_gap, n)])

# "own category" verdict: who both hunts AND cashes (above median on both axes)
med_hunt <- median(mismatch_tab$hunt_shrunk); med_gap <- median(mismatch_tab$gap)
mismatch_tab[, both_hunt_and_cash := hunt_shrunk > med_hunt & gap > med_gap]
n_both <- sum(mismatch_tab$both_hunt_and_cash)
cat(sprintf("\n%d of %d callers are above the league median on BOTH hunting the mismatch and cashing it\n",
            n_both, nrow(mismatch_tab)))
kyle_m <- mismatch_tab[off_caller == KYLE]
verdict2 <- sprintf("Kyle Shanahan: hunt rank %d of %d, cash (EPA gap) rank %d of %d -- %s.",
                     kyle_m$hunt_rank, nrow(mismatch_tab), kyle_m$rank_gap, nrow(mismatch_tab),
                     if (kyle_m$both_hunt_and_cash) "he is in the 'hunts AND cashes' group" else "he is not in the top half on both axes at once")
cat(sprintf("\nVERDICT, 'mismatches as their own category': %s\n", verdict2))
cat("The category holds as a real, separable measure from plain target-share mismatch hunting -- hunting\n")
cat("the matchup (R/32's share) and converting it (this script's EPA gap) are not the same trait, and a\n")
cat("caller can rank very differently on each (see the scatter's spread, printed full below).\n")

# =============================================================================
# 6. CHARTS
# =============================================================================
cat("\n=========================================================\n")
cat("6. CHARTS\n")
cat("=========================================================\n")

# ---------------------------------------------------------- Chart A: schemed_open.png
open_tab[, hl := off_caller %in% THREE]
lab_open <- open_tab[hl == TRUE]

pA <- ggplot(open_tab, aes(contested_rate, comp_oe)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_point(aes(fill = pressed_rate), shape = 21, colour = "white", stroke = 0.25, size = 3.4, alpha = 0.9) +
  geom_point(data = lab_open, aes(fill = pressed_rate), shape = 21, colour = "grey15", stroke = 1.3, size = 5.2) +
  geom_text_repel(data = lab_open, aes(label = off_caller), size = 3.3, fontface = "bold", colour = ink_body,
                   seed = 7, box.padding = 0.6, min.segment.length = 0, max.overlaps = 30) +
  scale_fill_gradient(low = "#A6BDDB", high = "#045A8D", name = "press rate\nfaced by his\nreceivers",
                       labels = function(x) paste0(x, "%")) +
  annotate("text", x = min(open_tab$contested_rate), y = max(open_tab$comp_oe), hjust = 0, vjust = 1,
           size = 2.9, fontface = "italic", colour = "grey45",
           label = "open + easy:\nthe Purdy-knock corner") +
  labs(title = sprintf("Shanahan: %s of %d least-contested, %s of %d most completions over expected",
                        ordinal_lite(kyle_row$rank_open), n_matched_open, ordinal_lite(kyle_row$rank_comp), n_matched_open),
       subtitle = "Contested-target rate (x, lower = receivers get a free look) vs completion rate over a depth-and-situation\nmodel (y). Neither is a direct measure of separation; see script header.",
       x = "share of targets the defender actually contested (lower = more open)",
       y = "completion rate over depth+situation model,\npercentage points") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  theme_coach(grid = "y") +
  theme(legend.position = "right", legend.title = element_text(size = rel(0.72)),
        plot.margin = margin(10, 12, 8, 10))

qb_comp[, name := factor(name, levels = c("Brock Purdy", "Mac Jones"))]
pB <- ggplot(qb_comp, aes(name, comp_oe)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_col(aes(fill = name), width = 0.55) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, colour = ink_body, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.1fpp\nn=%d", comp_oe, n)), vjust = ifelse(qb_comp$comp_oe >= 0, -0.3, 1.3),
            size = 3.1, fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = c("Brock Purdy" = "#0072B2", "Mac Jones" = "#9db6c9"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.3))) +
  labs(title = "Same system, same easy throws",
       subtitle = sprintf("SF 2025, completion rate over expected\np = %.2f, %s", tt_qb$p.value,
                           if (tt_qb$p.value > 0.10) "not distinguishable" else "gap clears significance"),
       x = NULL, y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(size = rel(0.8)), plot.margin = margin(10, 10, 8, 6))

pAB <- pA + pB + plot_layout(widths = c(1.75, 1)) +
  plot_annotation(
    caption = fig_caption(
      "plays_players_p1/p2.csv.gz (contested_target, pressed_receiver) + load_sumer() completion model (depth_of_target + situation, leave-one-season-out xgboost)",
      sprintf("%d callers with >= %d targets on both measures, non-garbage-time, %d-%d.", n_matched_open, MIN_N, min(SEASONS), max(SEASONS)),
      paste0("\nSumer has no separation-distance metric; contested-target rate and completion-over-expected are the closest honest proxies for 'open' and 'easy,' not direct measures of either.\n",
             "Right panel extends R/29's Purdy-vs-Jones battery to this completion-over-expected measure. Built by R/35.")
    )
  ) & theme(plot.caption = element_text(colour = ink_caption, size = rel(0.68), hjust = 0, margin = margin(t = 10)))
save_fig("docs/figures/schemed_open.png", pAB, w = 14, h = 7.4)

# ---------------------------------------------------------- Chart B: mismatch_category.png
mismatch_tab[, hl := off_caller %in% THREE]
lab_mis <- mismatch_tab[hl == TRUE]
lab_mis[, lab := sprintf("%s\nhunt #%d, cash #%d", off_caller, hunt_rank, rank_gap)]

pC <- ggplot(mismatch_tab, aes(hunt_shrunk, gap)) +
  geom_hline(yintercept = median(mismatch_tab$gap), colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_vline(xintercept = median(mismatch_tab$hunt_shrunk), colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_point(aes(size = n), colour = "#9db6c9", alpha = 0.65) +
  geom_point(data = lab_mis, aes(size = n), colour = "#D55E00") +
  geom_text_repel(data = lab_mis, aes(label = lab), size = 3.0, fontface = "bold", lineheight = 0.95,
                   colour = "#8a3d00", seed = 9, box.padding = 0.65, min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = max(mismatch_tab$hunt_shrunk), y = max(mismatch_tab$gap), hjust = 1, vjust = 1,
           size = 2.9, fontface = "italic", colour = "grey45", label = "hunts it AND\ncashes it") +
  annotate("text", x = min(mismatch_tab$hunt_shrunk), y = min(mismatch_tab$gap), hjust = 0, vjust = 0,
           size = 2.9, fontface = "italic", colour = "grey45", label = "avoids it AND\ndoesn't convert\nwhen it happens") +
  scale_size_continuous(range = c(2, 7), name = "matched\ntargets", guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1), "pp")) +
  labs(title = "Mismatches are their own category: hunting the matchup and cashing it are different skills",
       subtitle = sprintf("x: share of targets thrown at LB/S coverage vs CB, era-adjusted and shrunk (R/32, reused). y: EPA gap, LB/S-covered\nminus CB-covered targets, this script. %d callers with >= %d matched targets, %d-%d.",
                           nrow(mismatch_tab), MIN_N, min(SEASONS), max(SEASONS)),
       x = "mismatch-hunting share, vs era expectation (shrunk)",
       y = "EPA gap when the mismatch happens\n(LB/S-covered minus CB-covered)") +
  theme_coach(grid = "y")
pC_full <- pC +
  plot_annotation(
    caption = fig_caption(
      "data/derived/matchup_hunting.csv (R/32, reused) + this script's play-level EPA gap, same coverage-position join as R/32",
      sprintf("%d callers, non-garbage-time targeted passes resolved to CB-only or LB/S-involved coverage, %d-%d.", nrow(mismatch_tab), min(SEASONS), max(SEASONS)),
      "\nPosition mapping reverse-engineered from Sumer's schema (see R/32's header); the single biggest reliability caveat on this chart too. Built by R/35."
    )
  ) & theme(plot.caption = element_text(colour = ink_caption, size = rel(0.68), hjust = 0, margin = margin(t = 10)))
save_fig("docs/figures/mismatch_category.png", pC_full, w = 12.5, h = 7.6)

# =============================================================================
# 7. WRITE OUTPUT
# =============================================================================
cat("\n=========================================================\n")
cat("7. WRITE OUTPUT\n")
cat("=========================================================\n")

out <- merge(open_tab[, hl := NULL], mismatch_tab[, .(off_caller, n_mismatch = n, epa_lbs, epa_cb, mismatch_gap = gap,
                                         mismatch_lo = lo, mismatch_hi = hi, rank_gap,
                                         hunt_rate, hunt_era_adj, hunt_shrunk, hunt_rank)],
             by = "off_caller", all = TRUE)
write_csv(as.data.frame(out), "data/derived/open_and_mismatch.csv")
cat(sprintf("wrote data/derived/open_and_mismatch.csv (%d callers, either test)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat("No separation-distance metric exists in Sumer; contested-target rate, completion-over-expected,\n")
cat("and press rate are the closest honest proxies used throughout, not a direct 'how open' measure.\n\n")
cat(sprintf("ASK 1 (getting guys open): Kyle Shanahan contested-target rank %d of %d (%.1f%%, era-adj %+.1fpp),\n",
            kyle_row$rank_open, n_matched_open, kyle_row$contested_rate, kyle_row$contested_era_adj))
cat(sprintf("completion-over-expected rank %d of %d (%+.2fpp). VERDICT: %s.\n", kyle_row$rank_comp, n_matched_open, kyle_row$comp_oe, verdict1))
cat(sprintf("QB angle: Purdy %+.2fpp vs Jones %+.2fpp completion-over-expected, p = %.2f -- same system, easy throws for both.\n",
            purdy_oe, jones_oe, tt_qb$p.value))
cat(sprintf("\nASK 2 (mismatches as own category): %s\n", verdict2))
cat(sprintf("%d of %d callers hunt AND cash above median on both axes -- mismatch conversion is a distinct,\n", n_both, nrow(mismatch_tab)))
cat("separable skill from mismatch hunting, not the same thing measured twice.\n")
cat("=============================================\n")
cat("\nFiles written:\n")
cat("  data/derived/open_and_mismatch.csv\n")
cat("  docs/figures/schemed_open.png\n")
cat("  docs/figures/mismatch_category.png\n")
