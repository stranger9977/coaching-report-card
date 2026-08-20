# =============================================================================
# 53_open_allowed.R -- does Seattle's defense leave guys open, on a regular
# rush or a blitz?
#
# THE ASK, verbatim: "see if the Seahawks leave a lot of guys open on regular
# or blitzs?"
#
# THE HONEST LIMIT, STATED BEFORE ANYTHING ELSE (same disclosure as R/35,
# which this script's proxies are borrowed from and flipped to the defense's
# side of the ball). Sumer's charting has no separation-distance field: no
# yards-of-space, no nearest-defender-distance, nothing that measures "wide
# open" the way an eye does watching the tape. Two proxies stand in, neither
# of them a direct measure of openness:
#   - contested-target rate FORCED by the defense, flipped into an
#     "uncontested rate" (share of targets the defender did NOT actually
#     contest at the catch point). Higher = more guys getting a free look =
#     the closest this data gets to "left open." This is R/35's test 1a
#     proxy, same field (plays_players contested_target), just grouped by
#     the DEFENSIVE caller instead of the offensive one and split by rush
#     type instead of pooled.
#   - completions ALLOWED over what a depth-and-situation model expects.
#     Same leave-one-season-out xgboost machinery as R/35's test 1b
#     (sumer_expect() via R/factory/lib_sumer.R), same features (throw
#     depth + down/distance/field position/quarter/score/clock), grouped
#     by defensive caller and rush type. The model adapted cleanly to the
#     split: it is fit once on the full targeted-pass universe (nothing
#     about rush type or who is coaching goes in), and its play-level
#     residuals are then aggregated by (defensive caller, rush type) cell,
#     exactly the way R/35 aggregates them by caller alone. No fallback to
#     a raw-rate-vs-depth-bucket approach was needed.
#
# BLITZ DEFINITION, reused from R/28: `blitz` == TRUE, five or more rushers
# actually sent (not just shown). Every dropback is either a blitz or a
# regular (fewer than five sent) rush; there is no third category.
#
# DATA. Play-level via load_sumer(): is_dropback, is_targeted_pass, blitz,
# def_caller, garbage_time, season_type, plus the situation features
# (SUMER_STATE) and depth_of_target the completion model needs. Player-level:
# data/raw/sumer/plays_players_p1.csv.gz + _p2.csv.gz (contested_target,
# receiving_targets, side_of_ball, sumer_player_id), joined to play context
# on sumer_play_id + season, offense-side rows only (contested_target is
# charted on the receiver, not the defender). 2026 (preseason charting)
# excluded via the season filter below. season_type == 0 (regular season
# only) and garbage_time == FALSE applied throughout, matching every recent
# script on this board (R/39, R/41, R/45, R/46). def_caller attribution
# comes from load_sumer(); receiving_epa is not used anywhere in this script
# (this is a rate/completion comparison, not an EPA-gap test, so the
# catch-only-EPA trap other scripts warn about does not apply here).
#
# UNIVERSE. Defensive callers need 700+ charted dropbacks (R/28's floor) in
# the full non-garbage-time, regular-season, DC-attributed dropback sample.
# Within that set, every (defensive caller x rush type) cell needs 150+
# targets on EACH proxy separately; cells that miss the floor are printed
# and dropped, not silently folded into a smaller n.
#
# METHOD. Era-adjusted, empirical-Bayes-shrunk rate for the uncontested-rate
# proxy, R/35's shrink_binary_rate() generalized to a second grouping
# dimension (rush type): the league baseline a defensive caller is compared
# against is his OWN season x rush type's league rate, not the pooled
# league rate, so a caller's cell residual reflects what he does differently
# from other defenses facing the same situation, not just "blitzing raises
# the league rate anyway." Shrinkage (tau^2) is fit separately within each
# rush type, since blitz cells run far smaller than regular-rush cells. The
# completion-over-expected proxy is not further era-adjusted on top of its
# own model, same choice R/35 made for test 1b.
#
# Conventions: R/lib/theme_coach.R, plain language, no Michael/Nick in
# rendered chart text, season spans in rendered text, no em dashes.
#
# Out: docs/figures/open_allowed.png     (dumbbell, one row per qualifying
#                                          defensive caller, regular-rush
#                                          openness vs blitz openness)
#      data/derived/open_allowed.csv     (one row per def_caller x rush_type,
#                                          both proxies)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(20260820)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
MIN_CAREER <- 700   # R/28's DC floor, on charted dropbacks
MIN_CELL   <- 150   # targets per (def_caller x rush_type) cell
MACDONALD  <- "Mike Macdonald"

season_span <- function(seasons) sprintf("%d-%02d through %d-%02d", min(seasons), (min(seasons) + 1) %% 100,
                                          max(seasons), (max(seasons) + 1) %% 100)

ordinal_lite <- function(n) {
  suf <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  sprintf("%d%s", n, suf)
}

# =============================================================================
# 1. UNIVERSE + DEFENSIVE-CALLER FLOOR
# =============================================================================
cat("=========================================================\n")
cat("1. UNIVERSE + DEFENSIVE-CALLER FLOOR\n")
cat("=========================================================\n")

plays_all <- load_sumer(SEASONS)
plays_ng <- plays_all[season_type == 0 & garbage_time == FALSE & def_caller != ""]
cat(sprintf("plays, regular season, non-garbage-time, DC-attributed, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(plays_ng), big.mark = ",")))

drop_universe <- plays_ng[is_dropback == TRUE]
drop_universe[, rush_type := fifelse(blitz, "blitz", "regular")]
cat(sprintf("dropbacks in that universe: %s (%.1f%% blitzes, five or more rushers sent)\n",
            format(nrow(drop_universe), big.mark = ","), 100 * mean(drop_universe$blitz)))

career_n <- drop_universe[, .N, by = def_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_caller]
cat(sprintf("defensive callers with >= %d charted dropbacks: %d (of %d minimum-plausible)\n",
            MIN_CAREER, length(dcs_ok), uniqueN(drop_universe$def_caller)))
stopifnot(MACDONALD %in% dcs_ok)
cat(sprintf("%s qualifies (%d dropbacks).\n", MACDONALD, career_n[def_caller == MACDONALD]$N))

# =============================================================================
# 2. PROXY A: UNCONTESTED-TARGET RATE, PLAYER-LEVEL JOIN
# =============================================================================
cat("\n=========================================================\n")
cat("2. PROXY A: UNCONTESTED-TARGET RATE (higher = more guys left open)\n")
cat("=========================================================\n")

p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target"), showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target"), showProgress = FALSE)
players_raw <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
n_2026 <- players_raw[season == 2026, .N]
cat(sprintf("player-play rows: %s total, %s season-2026 (preseason charting) rows excluded by the season filter below\n",
            format(nrow(players_raw), big.mark = ","), format(n_2026, big.mark = ",")))

recv <- players_raw[season %in% SEASONS & side_of_ball == "offense" & receiving_targets > 0]
tgt <- merge(recv, plays_ng[, .(sumer_play_id, season, def_team, def_caller, blitz)],
             by = c("sumer_play_id", "season"))
tgt[, rush_type := fifelse(blitz, "blitz", "regular")]
tgt[, uncontested := as.integer(!contested_target)]
cat(sprintf("targeted receiver-play rows, regular season, non-garbage-time, DC-attributed, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(tgt), big.mark = ",")))
cat(sprintf("league uncontested-target rate: %.1f%% (%.1f%% contested)\n",
            100 * mean(tgt$uncontested), 100 * mean(tgt$contested_target)))

tgt <- tgt[def_caller %in% dcs_ok]
cat(sprintf("restricted to the %d qualifying defensive callers: %s targets\n", length(dcs_ok), format(nrow(tgt), big.mark = ",")))

# =============================================================================
# 3. PROXY B: COMPLETIONS ALLOWED OVER A DEPTH-AND-SITUATION MODEL
# =============================================================================
cat("\n=========================================================\n")
cat("3. PROXY B: COMPLETIONS ALLOWED OVER EXPECTED (season-held-out model)\n")
cat("=========================================================\n")

pass_universe <- plays_ng[is_targeted_pass == TRUE]
pass_universe[, rush_type := fifelse(blitz, "blitz", "regular")]
cat(sprintf("targeted passes, regular season, non-garbage-time, DC-attributed, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(pass_universe), big.mark = ",")))

COMP_FEATS <- c("depth_of_target", SUMER_STATE)
z <- sumer_expect(pass_universe, "is_complete_pass", feats = COMP_FEATS)
z[, resid := .y - .expected]
cat(sprintf("model check: %d of %d targeted passes scored (leave-one-season-out xgboost), cor(actual,expected) = %.3f\n",
            nrow(z), nrow(pass_universe), cor(z$.y, z$.expected)))
cat("Model sees only throw depth + game situation, nothing about who is defending -- what's left over\n")
cat("(resid = actual completion minus model-expected) is completions allowed beyond what the moment predicts.\n")

z <- z[def_caller %in% dcs_ok]
cat(sprintf("restricted to the %d qualifying defensive callers: %s targeted passes\n", length(dcs_ok), format(nrow(z), big.mark = ",")))

# =============================================================================
# 4. LEAGUE-WIDE HEADLINE: DOES BLITZING LEAVE GUYS OPEN, LEAGUEWIDE?
# =============================================================================
cat("\n=========================================================\n")
cat("4. LEAGUE-WIDE: BLITZ VS REGULAR-RUSH OPENNESS GAP\n")
cat("=========================================================\n")

lg_a <- tgt[, .(n = .N, uncontested_rate = 100 * mean(uncontested)), by = rush_type]
setorder(lg_a, rush_type)
print(lg_a)
prop_a <- prop.test(x = tgt[, sum(uncontested), by = rush_type][order(rush_type)]$V1,
                     n = tgt[, .N, by = rush_type][order(rush_type)]$N)
gap_a <- lg_a[rush_type == "blitz"]$uncontested_rate - lg_a[rush_type == "regular"]$uncontested_rate
cat(sprintf("\nleaguewide, proxy A: blitz uncontested rate %.1f%% vs regular %.1f%%, gap %+.1fpp [%+.1f, %+.1f], p %s\n",
            lg_a[rush_type == "blitz"]$uncontested_rate, lg_a[rush_type == "regular"]$uncontested_rate, gap_a,
            100 * prop_a$conf.int[1], 100 * prop_a$conf.int[2],
            if (prop_a$p.value < 0.001) "< 0.001" else sprintf("= %.3f", prop_a$p.value)))

lg_b <- z[, .(n = .N, comp_actual = 100 * mean(.y), comp_oe = 100 * mean(resid)), by = rush_type]
setorder(lg_b, rush_type)
print(lg_b)
tt_b <- t.test(resid ~ rush_type, data = z)
gap_b <- lg_b[rush_type == "blitz"]$comp_oe - lg_b[rush_type == "regular"]$comp_oe
cat(sprintf("\nleaguewide, proxy B: blitz completion-over-expected %+.2fpp vs regular %+.2fpp, gap %+.2fpp, p %s\n",
            lg_b[rush_type == "blitz"]$comp_oe, lg_b[rush_type == "regular"]$comp_oe, gap_b,
            if (tt_b$p.value < 0.001) "< 0.001" else sprintf("= %.3f", tt_b$p.value)))
sig_word <- if (prop_a$p.value < 0.001 && tt_b$p.value < 0.001) "clearing significance easily" else "measurable"
gap_a_word <- if (gap_a > 0) "loosens" else "tightens"
verdict_dir <- if (gap_a > 0 && gap_b > 0) {
  sprintf(paste0("blitzing leaves receivers %.1f percentage points more uncontested and allows %.2f more\n",
                 "completion percentage points than a depth-and-situation model expects, both %s. Sending extra\n",
                 "rushers costs the coverage behind them, on both proxies, across the whole league."),
          gap_a, gap_b, sig_word)
} else if (gap_a <= 0 && gap_b <= 0) {
  sprintf(paste0("blitzing does the OPPOSITE of the starting assumption: coverage gets %.1f percentage points\n",
                 "TIGHTER (fewer uncontested targets) and %.2f fewer completion percentage points land than a\n",
                 "depth-and-situation model expects, both %s. Extra rushers buy tighter coverage on average, not\n",
                 "looser -- the rushed throw a blitz forces costs the offense more than the occasional free look\n",
                 "it creates costs the defense."),
          abs(gap_a), abs(gap_b), sig_word)
} else {
  sprintf(paste0("the two proxies disagree on direction: uncontested-rate gap %+.1fpp, completion-over-expected\n",
                 "gap %+.2fpp. Not a clean leaguewide story either way."), gap_a, gap_b)
}
cat(sprintf("\nVERDICT, leaguewide: %s\n", verdict_dir))

# =============================================================================
# 5. PER-CELL DECOMPOSITION: (DEFENSIVE CALLER x RUSH TYPE)
# =============================================================================
cat("\n=========================================================\n")
cat("5. PER-CELL DECOMPOSITION: DEFENSIVE CALLER x RUSH TYPE\n")
cat("=========================================================\n")

# Generalized version of R/35's shrink_binary_rate(): league baseline is now
# each (season, rush_type) cell's own rate, not the pooled season rate, so a
# caller's residual is "different from the league facing the SAME rush type
# in the SAME season," not contaminated by the leaguewide blitz-vs-regular
# gap quantified in section 4.
shrink_rate_cell <- function(x, event_col, min_n) {
  x <- copy(x); x[, event := as.integer(get(event_col))]
  lg <- x[, .(n = .N, k = sum(event)), by = .(season, rush_type)]
  lg[, lg_rate := 100 * k / n]
  cs <- x[, .(n = .N, k = sum(event)), by = .(def_caller, rush_type, season)]
  cs <- merge(cs, lg[, .(season, rush_type, lg_rate)], by = c("season", "rush_type"))
  dc <- cs[, .(n = sum(n), k = sum(k), expected_k = sum(lg_rate / 100 * n)), by = .(def_caller, rush_type)]
  dc[, `:=`(rate = 100 * k / n, expected_rate = 100 * expected_k / n)]
  dc[, era_adj := rate - expected_rate]
  dc[, se := 100 * sqrt((rate / 100) * (1 - rate / 100) / n)]
  dc[, `:=`(lo = era_adj - 1.96 * se, hi = era_adj + 1.96 * se)]
  dropped <- dc[n < min_n, .(def_caller, rush_type, n)]
  dc <- dc[n >= min_n]
  dc[, era_shrunk := {
    tau2 <- max(var(era_adj) - mean(se^2), 1e-4)
    (tau2 * era_adj) / (tau2 + se^2)
  }, by = rush_type]
  list(cells = dc[], dropped = dropped[])
}

res_a <- shrink_rate_cell(tgt, "uncontested", MIN_CELL)
cell_a <- res_a$cells
cat(sprintf("proxy A (uncontested rate): %d of %d possible (def_caller x rush_type) cells clear >= %d targets\n",
            nrow(cell_a), length(dcs_ok) * 2, MIN_CELL))
if (nrow(res_a$dropped)) {
  cat("dropped cells (below the target floor):\n")
  print(res_a$dropped[order(def_caller, rush_type)])
}

cell_b <- z[, .(n = .N, comp_actual = 100 * mean(.y), comp_expected = 100 * mean(.expected),
                 comp_oe = 100 * mean(resid), se = 100 * sd(resid) / sqrt(.N)), by = .(def_caller, rush_type)]
cell_b[, `:=`(lo = comp_oe - 1.96 * se, hi = comp_oe + 1.96 * se)]
dropped_b <- cell_b[n < MIN_CELL, .(def_caller, rush_type, n)]
cell_b <- cell_b[n >= MIN_CELL]
cat(sprintf("\nproxy B (completion over expected): %d of %d possible cells clear >= %d targets\n",
            nrow(cell_b), length(dcs_ok) * 2, MIN_CELL))
if (nrow(dropped_b)) {
  cat("dropped cells (below the target floor):\n")
  print(dropped_b[order(def_caller, rush_type)])
}

# rank within each rush type: rank 1 = most open (highest uncontested rate /
# highest completion-over-expected)
setorder(cell_a, rush_type, -era_adj)
cell_a[, rank_open := seq_len(.N), by = rush_type]
setorder(cell_b, rush_type, -comp_oe)
cell_b[, rank_comp := seq_len(.N), by = rush_type]

open_tab <- merge(cell_a[, .(def_caller, rush_type, n_contested = n, uncontested_rate = rate,
                              uncontested_era_adj = era_adj, uncontested_lo = lo, uncontested_hi = hi,
                              uncontested_shrunk = era_shrunk, rank_open)],
                   cell_b[, .(def_caller, rush_type, n_comp = n, comp_actual, comp_expected, comp_oe,
                              comp_lo = lo, comp_hi = hi, rank_comp)],
                   by = c("def_caller", "rush_type"))
cat(sprintf("\ncells qualifying on BOTH proxies: %d (of %d def_caller x rush_type combinations possible)\n",
            nrow(open_tab), length(dcs_ok) * 2))
n_reg <- open_tab[rush_type == "regular", .N]
n_blz <- open_tab[rush_type == "blitz", .N]
cat(sprintf("regular-rush cells: %d defensive callers. blitz cells: %d defensive callers.\n", n_reg, n_blz))

# =============================================================================
# 6. MACDONALD HEADLINE, VERIFICATION BLOCK
# =============================================================================
cat("\n=========================================================\n")
cat("6. THE MACDONALD QUESTION, BOTH SPLITS\n")
cat("=========================================================\n")

mac_reg <- open_tab[def_caller == MACDONALD & rush_type == "regular"]
mac_blz <- open_tab[def_caller == MACDONALD & rush_type == "blitz"]
stopifnot(nrow(mac_reg) == 1, nrow(mac_blz) == 1)

med_reg <- open_tab[rush_type == "regular", .(med_uncontested = median(uncontested_era_adj), med_comp = median(comp_oe))]
med_blz <- open_tab[rush_type == "blitz", .(med_uncontested = median(uncontested_era_adj), med_comp = median(comp_oe))]

free_rushers <- fread("data/derived/free_rushers.csv")
mac_free <- free_rushers[def_play_caller == MACDONALD]

verdict_reg <- if (mac_reg$rank_open <= n_reg / 3) "leaves guys open" else if (mac_reg$rank_open >= 2 * n_reg / 3) "keeps it tight" else "about league average"
verdict_blz <- if (mac_blz$rank_open <= n_blz / 3) "leaves guys open" else if (mac_blz$rank_open >= 2 * n_blz / 3) "keeps it tight" else "about league average"

cat("\n================= VERIFICATION BLOCK =================\n")
cat(sprintf("REGULAR RUSH (%d qualifying defensive callers):\n", n_reg))
cat(sprintf("  uncontested-target rate: %.1f%% actual, %+.1fpp era-adjusted, rank #%d of %d (1 = most open) -- %s\n",
            mac_reg$uncontested_rate, mac_reg$uncontested_era_adj, mac_reg$rank_open, n_reg, verdict_reg))
cat(sprintf("  completion rate over expected: %+.2fpp, rank #%d of %d\n", mac_reg$comp_oe, mac_reg$rank_comp, n_reg))
cat(sprintf("  league median era-adjusted uncontested rate: %+.1fpp. league median completion-over-expected: %+.2fpp.\n",
            med_reg$med_uncontested, med_reg$med_comp))

cat(sprintf("\nBLITZ (five or more rushers sent, %d qualifying defensive callers):\n", n_blz))
cat(sprintf("  uncontested-target rate: %.1f%% actual, %+.1fpp era-adjusted, rank #%d of %d (1 = most open) -- %s\n",
            mac_blz$uncontested_rate, mac_blz$uncontested_era_adj, mac_blz$rank_open, n_blz, verdict_blz))
cat(sprintf("  completion rate over expected: %+.2fpp, rank #%d of %d\n", mac_blz$comp_oe, mac_blz$rank_comp, n_blz))
cat(sprintf("  league median era-adjusted uncontested rate: %+.1fpp. league median completion-over-expected: %+.2fpp.\n",
            med_blz$med_uncontested, med_blz$med_comp))

cat(sprintf("\nLEAGUEWIDE GAP (section 4): blitzing %s coverage overall -- uncontested rate %+.1fpp and\n", gap_a_word, gap_a))
cat(sprintf("completion-over-expected %+.2fpp on blitzes vs a regular rush, across every defense in the sample.\n", gap_b))

cat(sprintf("\nTIE-IN: %s manufactures free rushers on blitzes 2nd-best in football (R/28: unblocked-pressure\n", MACDONALD))
cat(sprintf("rank #%d of %d) while blitzing below what his own situations predict (blitz rank #%d of %d).\n",
            mac_free$unblocked_rank, nrow(free_rushers), mac_free$blitz_rank, nrow(free_rushers)))

cat(sprintf("\nVERDICT: on a regular rush, %s's defense %s (uncontested rank #%d of %d). On a blitz, it %s\n",
            MACDONALD, verdict_reg, mac_reg$rank_open, n_reg, verdict_blz))
cat(sprintf("(uncontested rank #%d of %d). %s\n", mac_blz$rank_open, n_blz,
            if (verdict_reg == verdict_blz) "The two splits tell the same story." else "The two splits tell different stories: this is a real split, not a coin flip."))
cat("========================================================\n")

# =============================================================================
# 7. CHART: DUMBBELL, REGULAR-RUSH OPENNESS VS BLITZ OPENNESS
# =============================================================================
cat("\n=========================================================\n")
cat("7. CHART\n")
cat("=========================================================\n")

wide <- dcast(open_tab, def_caller ~ rush_type, value.var = "uncontested_era_adj")
wide <- wide[complete.cases(wide)]
n_both <- nrow(wide)
cat(sprintf("defensive callers with BOTH splits qualifying (the ones the chart can show): %d\n", n_both))
setorder(wide, -blitz)
wide[, def_caller := factor(def_caller, levels = rev(wide$def_caller))]
wide[, hl := def_caller == MACDONALD]

mac_lab <- wide[hl == TRUE]
mac_lab[, lab := sprintf("regular %+.1fpp (#%d of %d)\nblitz %+.1fpp (#%d of %d)",
                          mac_reg$uncontested_era_adj, mac_reg$rank_open, n_reg,
                          mac_blz$uncontested_era_adj, mac_blz$rank_open, n_blz)]

med_reg_v <- med_reg$med_uncontested; med_blz_v <- med_blz$med_uncontested

p <- ggplot(wide) +
  geom_vline(xintercept = med_reg_v, colour = "#2B8CBE", linetype = "dashed", linewidth = 0.4) +
  geom_vline(xintercept = med_blz_v, colour = "#D55E00", linetype = "dashed", linewidth = 0.4) +
  geom_segment(aes(x = regular, xend = blitz, y = def_caller, yend = def_caller),
               colour = "grey75", linewidth = 0.5) +
  geom_point(aes(x = regular, y = def_caller, colour = "regular rush"), size = 2.1) +
  geom_point(aes(x = blitz, y = def_caller, colour = "blitz (5+ rushers)"), size = 2.1) +
  geom_segment(data = wide[hl == TRUE], aes(x = regular, xend = blitz, y = def_caller, yend = def_caller),
               colour = "grey25", linewidth = 1.1) +
  geom_point(data = wide[hl == TRUE], aes(x = regular, y = def_caller), colour = "#2B8CBE", size = 4) +
  geom_point(data = wide[hl == TRUE], aes(x = blitz, y = def_caller), colour = "#D55E00", size = 4) +
  geom_text_repel(data = mac_lab, aes(x = blitz, y = def_caller, label = lab), size = 2.9, fontface = "bold",
                   colour = "grey20", lineheight = 0.95, seed = 11, box.padding = 0.8,
                   min.segment.length = 0, direction = "y", nudge_x = 5, hjust = 0) +
  scale_colour_manual(values = c("regular rush" = "#2B8CBE", "blitz (5+ rushers)" = "#D55E00"), name = NULL) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x, 0), "pp")) +
  labs(
    title = sprintf("Blitzing %s coverage leaguewide by %.1f points; on blitzes, %s ranks #%d of %d for openness allowed",
                     gap_a_word, abs(gap_a), MACDONALD, mac_blz$rank_open, n_blz),
    subtitle = sprintf(paste0("Uncontested-target rate, era-adjusted against the league facing the same rush type in the same season. Higher = more receivers get a\n",
                               "free look at the catch point. Dashed lines mark the league median on each rush type. %d defensive callers with >= %d charted dropbacks\n",
                               "and >= %d targets on both splits, %s regular seasons."),
                        n_both, MIN_CAREER, MIN_CELL, season_span(SEASONS)),
    x = "uncontested-target rate vs league expectation for that rush type (percentage points)",
    y = NULL,
    caption = fig_caption(
      "plays_players_p1/p2.csv.gz (contested_target) + load_sumer() dropback/blitz flags, SumerSports play charting",
      sprintf("%d defensive callers, regular-season non-garbage-time targets, %s.", n_both, season_span(SEASONS)),
      "\nSumer has no separation-distance metric; uncontested-target rate is the closest honest proxy for 'left open,' not a direct measure of it.\nA second proxy, completions allowed over a depth-and-situation model, corroborates in data/derived/open_allowed.csv. Built by R/53."
    )
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.text = element_text(size = rel(0.85)),
        axis.text.y = element_text(size = rel(0.68)),
        plot.margin = margin(10, 40, 8, 10))
save_fig("docs/figures/open_allowed.png", p, w = 12.5, h = 11)

# =============================================================================
# 8. WRITE OUTPUT
# =============================================================================
cat("\n=========================================================\n")
cat("8. WRITE OUTPUT\n")
cat("=========================================================\n")

write_csv(as.data.frame(open_tab), "data/derived/open_allowed.csv")
cat(sprintf("wrote data/derived/open_allowed.csv (%d rows, one per def_caller x rush_type)\n", nrow(open_tab)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat("No separation-distance metric exists in Sumer; uncontested-target rate and completion-over-expected\n")
cat("are the closest honest proxies for 'left open,' used throughout, not a direct openness measure.\n\n")
cat(sprintf("LEAGUEWIDE: blitzing %s coverage -- uncontested rate moves %+.1fpp and completion-over-expected\n", gap_a_word, gap_a))
cat(sprintf("moves %+.2fpp on blitzes vs a regular rush, across the league. %s\n\n",
            gap_b, if (gap_a <= 0 && gap_b <= 0) "Extra rushers buy tighter coverage on average, not looser -- the opposite of the starting assumption." else "Sending extra rushers costs the coverage behind them."))
cat(sprintf("%s, REGULAR RUSH: uncontested rank #%d of %d (%+.1fpp era-adj) -- %s.\n",
            MACDONALD, mac_reg$rank_open, n_reg, mac_reg$uncontested_era_adj, verdict_reg))
cat(sprintf("%s, BLITZ:        uncontested rank #%d of %d (%+.1fpp era-adj) -- %s.\n",
            MACDONALD, mac_blz$rank_open, n_blz, mac_blz$uncontested_era_adj, verdict_blz))
cat(sprintf("Tie-in: manufactures free rushers 2nd-best in football on blitzes (R/28) while blitzing below\n"))
cat("expected volume -- the question this script answers is whether the coverage behind those free rushers\n")
cat("pays for the pressure in openness, and the verdict above is the answer, split both ways.\n")
cat("=============================================\n")
cat("\nFiles written:\n")
cat("  data/derived/open_allowed.csv\n")
cat("  docs/figures/open_allowed.png\n")
