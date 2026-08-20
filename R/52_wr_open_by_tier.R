# =============================================================================
# 52_wr_open_by_tier.R -- Michael's ask, verbatim: "can we look at open rate
# for WRs per coach? split by wr1, 2, 3 or diff wide receiver types"
#
# THE HONEST LIMIT, CARRIED FORWARD FROM R/35 UNCHANGED. Sumer's charting has
# no separation-distance field: no yards-of-space, no nearest-defender-
# distance, nothing that measures "open" the way an eye does watching the
# tape. This script reuses R/35's two proxies exactly, split by receiver
# tier instead of pooled across a caller's whole receiving corps:
#   - contested_target rate (player-level, plays_players p1/p2): a charter's
#     yes/no call on whether the defender actually contested the catch. LOW
#     contested rate = receivers getting a free look at the catch point.
#   - completion rate over a depth-and-situation-only model (same leave-one-
#     season-out xgboost as every model on this board via lib_sumer's
#     sumer_expect(), nothing about who is coaching or who is catching goes
#     in). What's left over is "easier than the numbers say it should be."
# Neither proxy is "how open was he." Every chart below says so again.
#
# TIERS. Within each team-season, wide receivers (position == "WR") are
# ranked by season targets: WR1 = most-targeted, WR2 = second, WR3 = third.
# A caller is pooled across every team-season he called plays for, so a
# caller who changed teams still gets one WR1/WR2/WR3 bucket built from all
# of his seasons. Two context buckets, same ranking logic, rank 1 only: TE
# (that team-season's most-targeted tight end) and RB (its most-targeted
# back) -- the board's McCaffrey finding (R/29, R/32) is that a lead back
# can BE the WR1-equivalent target, so it is worth a look here as context,
# not charted as a headline.
#
# DATA. Player-level: data/raw/sumer/plays_players_p1.csv.gz + _p2.csv.gz
# (contested_target, receiving_targets, sumer_player_id), names/positions
# from player_details.csv.gz, joined on sumer_play_id (never nflverse ids,
# per lib_sumer's join notes). Play-level via load_sumer(): is_targeted_pass,
# is_complete_pass, depth_of_target, expected_points_added, garbage_time,
# season_type, off_caller. Regular season only (season_type == 0; this
# repo's standard, R/39/R/45/R/48/R/51), garbage time excluded, 2026
# (preseason charting) excluded by the season window alone.
#
# QUALIFICATION. Caller: >= 1200 offensive plays across the window (this
# board's standard floor, R/31/R/40/R/48). Cell: >= 80 targets in that
# specific (caller, tier) bucket for a measure to be reported -- a tier
# bucket is a much narrower slice than a caller's full receiving corps, so
# this is a tighter floor than R/35's 400-target, caller-wide minimum.
#
# Conventions: R/lib/theme_coach.R, plain language, no Michael/Nick in
# rendered chart text, empirical-Bayes shrinkage + era adjustment (matching
# R/35's method, done here within season x tier so a WR1 target isn't
# compared to the league's TE rate), 95% CIs where sample size allows. No
# em dashes.
#
# Out: docs/figures/wr_open_by_tier.png (chosen proxy, WR1/WR2/WR3 panels,
#        qualified callers, three named callers highlighted, league median)
#      data/derived/wr_open_by_tier.csv (one row per caller x tier, both
#        proxies, ranks within tier, CIs)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(20260819)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
MIN_OFF_PLAYS <- 1200   # caller-level house floor (R/31/R/40/R/48)
MIN_CELL <- 80          # (caller, tier) cell floor
KYLE <- "Kyle Shanahan"; MCVAY <- "Sean McVay"; BJ <- "Ben Johnson"
THREE <- c(KYLE, MCVAY, BJ)
TIER_LEVELS <- c("WR1", "WR2", "WR3", "TE", "RB")

ordinal_lite <- function(n) {
  suf <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  sprintf("%d%s", n, suf)
}

# =============================================================================
# 1. LOAD -- plays, player targets, caller qualification
# =============================================================================
cat("=========================================================\n")
cat("1. LOAD: plays, player-level targets, caller qualification\n")
cat("=========================================================\n")

plays_ctx <- load_sumer(SEASONS)
plays_ctx <- plays_ctx[season_type == 0]   # regular season only, this repo's standard
plays_ng  <- plays_ctx[garbage_time == FALSE]
cat(sprintf("Regular-season, non-garbage-time plays, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(plays_ng), big.mark = ",")))

off_n <- plays_ng[off_caller != "" & run_pass %in% c("P", "R"), .N, by = off_caller]
qualified <- off_n[N >= MIN_OFF_PLAYS]$off_caller
cat(sprintf("Callers with >= %d offensive plays: %d of %d\n", MIN_OFF_PLAYS, length(qualified), nrow(off_n)))
stopifnot(all(THREE %in% qualified))

p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target"),
            showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
                       "receiving_targets", "contested_target"),
            showProgress = FALSE)
players_raw <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id", "football_name", "last_name", "position"))
pd[, name := paste(football_name, last_name)]

targets0 <- players_raw[season %in% SEASONS & side_of_ball == "offense" & receiving_targets > 0]
targets0 <- merge(targets0, plays_ng[, .(sumer_play_id, off_team, off_caller, season,
                                          depth_of_target, expected_points_added, is_complete_pass)],
                   by = c("sumer_play_id", "season"))
targets0 <- merge(targets0, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
cat(sprintf("Targeted receiver-play rows, regular season, non-garbage-time, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(targets0), big.mark = ",")))

# =============================================================================
# 2. TIERS -- rank WR/TE/RB by season targets within each team-season
# =============================================================================
cat("\n=========================================================\n")
cat("2. TIERS: WR1/WR2/WR3 (rank 1-3), TE and RB (rank 1) per team-season\n")
cat("=========================================================\n")

team_season <- targets0[position %in% c("WR", "TE", "RB"),
                         .(targets = sum(receiving_targets)),
                         by = .(sumer_player_id, name, position, off_team, season)]
team_season[, rank_pos := frank(-targets, ties.method = "first"), by = .(off_team, season, position)]

team_season[, tier := NA_character_]
team_season[position == "WR" & rank_pos == 1, tier := "WR1"]
team_season[position == "WR" & rank_pos == 2, tier := "WR2"]
team_season[position == "WR" & rank_pos == 3, tier := "WR3"]
team_season[position == "TE" & rank_pos == 1, tier := "TE"]
team_season[position == "RB" & rank_pos == 1, tier := "RB"]
tiered <- team_season[!is.na(tier)]
cat(sprintf("Team-season tier assignments: %d (%d team-seasons x up to 5 tiers)\n",
            nrow(tiered), uniqueN(team_season[, .(off_team, season)])))
print(tiered[, .N, by = tier][order(factor(tier, levels = TIER_LEVELS))])

targets0 <- merge(targets0, tiered[, .(sumer_player_id, off_team, season, tier)],
                   by = c("sumer_player_id", "off_team", "season"), all.x = TRUE)
tg <- targets0[off_caller != "" & !is.na(tier)]
tg[, tier := factor(tier, levels = TIER_LEVELS)]
cat(sprintf("\nCaller-attributed, tier-tagged target rows: %s (of %s caller-attributed targets overall)\n",
            format(nrow(tg), big.mark = ","), format(nrow(targets0[off_caller != ""]), big.mark = ",")))

# =============================================================================
# 3. PROXY A: CONTESTED-TARGET RATE PER (CALLER, TIER) -- low = more open
# =============================================================================
cat("\n=========================================================\n")
cat("3. PROXY A: contested-target rate per caller x tier (low = more open)\n")
cat("=========================================================\n")

# Same era-adjusted, empirical-Bayes-shrunk method as R/35's shrink_binary_rate(),
# generalized so the league baseline is computed within season x tier (a WR1
# target should not be benchmarked against the league's TE contested rate).
shrink_binary_tiered <- function(x, event_col, min_n) {
  x <- copy(x); x[, event := as.integer(get(event_col))]
  lg <- x[, .(n = .N, k = sum(event)), by = .(season, tier)][, rate := 100 * k / n]
  cs <- x[, .(n = .N, k = sum(event)), by = .(off_caller, tier, season)]
  cs <- merge(cs, lg[, .(season, tier, lg_rate = rate)], by = c("season", "tier"))
  dc <- cs[, .(n = sum(n), k = sum(k), expected_k = sum(lg_rate / 100 * n)), by = .(off_caller, tier)]
  dc[, `:=`(rate = 100 * k / n, expected_rate = 100 * expected_k / n)]
  dc[, era_adj := rate - expected_rate]
  dc[, se := 100 * sqrt((rate / 100) * (1 - rate / 100) / n)]
  dc[, `:=`(lo = era_adj - 1.96 * se, hi = era_adj + 1.96 * se)]
  n_cells_pre <- nrow(dc)
  dc <- dc[n >= min_n]
  dc[, era_shrunk := {
    tau2 <- max(var(era_adj) - mean(se^2), 1e-4)
    (tau2 * era_adj) / (tau2 + se^2)
  }, by = tier]
  attr(dc, "n_dropped") <- n_cells_pre - nrow(dc)
  dc[]
}

contested_dc <- shrink_binary_tiered(tg, "contested_target", MIN_CELL)
contested_pre <- tg[off_caller %in% qualified, .N, by = .(off_caller, tier)]
n_dropped_contested <- sum(contested_pre$N < MIN_CELL)
contested_dc <- contested_dc[off_caller %in% qualified]
setorder(contested_dc, tier, era_shrunk)
contested_dc[, rank_open := seq_len(.N), by = tier]   # rank 1 = most open within that tier
contested_dc[, n_in_tier := .N, by = tier]
cat(sprintf("Cells (caller x tier) dropped for < %d targets: %d\n", MIN_CELL, n_dropped_contested))
cat(sprintf("Qualified-caller cells remaining: %d\n", nrow(contested_dc)))

cat("\n--- league median contested-target rate (era-adjusted, shrunk) by tier ---\n")
print(contested_dc[, .(median_era_shrunk = round(median(era_shrunk), 2), n_callers = .N), by = tier][order(factor(tier, levels = TIER_LEVELS))])

# =============================================================================
# 4. PROXY B: COMPLETION RATE OVER EXPECTED PER (CALLER, TIER)
# =============================================================================
cat("\n=========================================================\n")
cat("4. PROXY B: completion rate over expected per caller x tier\n")
cat("=========================================================\n")

pass_universe <- plays_ng[is_targeted_pass == TRUE]
cat(sprintf("Targeted passes, regular season, non-garbage-time, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(pass_universe), big.mark = ",")))

COMP_FEATS <- c("depth_of_target", SUMER_STATE)
z <- sumer_expect(pass_universe, "is_complete_pass", feats = COMP_FEATS)
z[, resid := .y - .expected]
cat(sprintf("Model check: %d of %d targeted passes scored (leave-one-season-out xgboost), cor(actual,expected) = %.3f\n",
            nrow(z), nrow(pass_universe), cor(z$.y, z$.expected)))
cat("Model only sees throw depth + game situation, nothing about who is coaching or who is catching --\n")
cat("what's left over (actual completion minus model-expected) is attached to the targeted receiver's tier.\n")

z_tier <- merge(z, tg[, .(sumer_play_id, season, sumer_player_id, tier)], by = c("sumer_play_id", "season"))
cat(sprintf("Modeled targeted passes matched to a tier-tagged receiver: %d of %d\n", nrow(z_tier), nrow(z)))

comp_oe <- z_tier[off_caller != "", .(n = .N, comp_actual = 100 * mean(.y), comp_expected = 100 * mean(.expected),
                                       comp_oe = 100 * mean(resid), se = 100 * sd(resid) / sqrt(.N)),
                   by = .(off_caller, tier)]
comp_oe[, `:=`(lo = comp_oe - 1.96 * se, hi = comp_oe + 1.96 * se)]

# dropped-cell count: of qualified-caller x tier cells that exist at all in the raw
# (pre-floor) table, how many failed the >= MIN_CELL floor
comp_oe_pre <- z_tier[off_caller %in% qualified, .N, by = .(off_caller, tier)]
n_dropped_comp <- sum(comp_oe_pre$N < MIN_CELL)

comp_oe <- comp_oe[n >= MIN_CELL & off_caller %in% qualified]
setorder(comp_oe, tier, -comp_oe)
comp_oe[, rank_comp := seq_len(.N), by = tier]
comp_oe[, n_in_tier := .N, by = tier]
cat(sprintf("Cells (caller x tier) dropped for < %d modeled targets: %d\n", MIN_CELL, n_dropped_comp))
cat(sprintf("Qualified-caller cells remaining: %d\n", nrow(comp_oe)))

cat("\n--- league median completion rate over expected by tier ---\n")
print(comp_oe[, .(median_comp_oe = round(median(comp_oe), 2), n_callers = .N), by = tier][order(factor(tier, levels = TIER_LEVELS))])

# =============================================================================
# 5. WHICH PROXY DIFFERENTIATES CALLERS MORE CLEANLY -- signal_share() (lib_sumer)
# =============================================================================
cat("\n=========================================================\n")
cat("5. WHICH PROXY DIFFERENTIATES CALLERS MORE CLEANLY (WR1/WR2/WR3 only)\n")
cat("=========================================================\n")

sig_contested <- contested_dc[tier %in% c("WR1", "WR2", "WR3"), {
  s <- signal_share(era_adj, se); .(share = s$share)
}, by = tier]
sig_comp <- comp_oe[tier %in% c("WR1", "WR2", "WR3"), {
  s <- signal_share(comp_oe, se); .(share = s$share)
}, by = tier]
cat("Share of observed caller-to-caller spread that is real signal, not sampling noise:\n")
cat("  contested-target rate:\n"); print(sig_contested)
cat("  completion rate over expected:\n"); print(sig_comp)

avg_share_contested <- mean(sig_contested$share)
avg_share_comp <- mean(sig_comp$share)
chosen_proxy <- if (avg_share_contested >= avg_share_comp) "contested" else "comp_oe"
cat(sprintf("\nAverage signal share, WR1-3: contested-target = %.2f, completion-over-expected = %.2f\n",
            avg_share_contested, avg_share_comp))
cat(sprintf("CHOSEN FOR THE CHART: %s (higher share of real caller-to-caller spread, less noise).\n",
            if (chosen_proxy == "contested") "contested-target rate" else "completion rate over expected"))

# =============================================================================
# 6. VERIFICATION BLOCK -- three named callers, all tiers, both proxies
# =============================================================================
cat("\n=========================================================\n")
cat("6. VERIFICATION: Shanahan / McVay / Ben Johnson, all tiers, both proxies\n")
cat("=========================================================\n")

for (nm in THREE) {
  cat(sprintf("\n--- %s ---\n", nm))
  co <- contested_dc[off_caller == nm]
  cp <- comp_oe[off_caller == nm]
  for (tr in TIER_LEVELS) {
    a <- co[tier == tr]; b <- cp[tier == tr]
    a_str <- if (nrow(a)) sprintf("contested %.1f%% (era-adj %+.1fpp), rank %s of %d", a$rate, a$era_adj, ordinal_lite(a$rank_open), a$n_in_tier) else "contested: cell dropped (< min targets)"
    b_str <- if (nrow(b)) sprintf("comp-over-expected %+.2fpp, rank %s of %d", b$comp_oe, ordinal_lite(b$rank_comp), b$n_in_tier) else "comp-over-expected: cell dropped (< min targets)"
    cat(sprintf("  %-3s: %s | %s\n", tr, a_str, b_str))
  }
}

cat("\n--- league medians by tier (era-shrunk contested rate; comp-over-expected) ---\n")
med_tab <- merge(
  contested_dc[, .(median_contested_shrunk = round(median(era_shrunk), 2)), by = tier],
  comp_oe[, .(median_comp_oe = round(median(comp_oe), 2)), by = tier],
  by = "tier")
print(med_tab[order(factor(tier, levels = TIER_LEVELS))])

cat(sprintf("\nDropped cells: %d (contested-rate measure), %d (completion-over-expected measure)\n",
            n_dropped_contested, n_dropped_comp))

# =============================================================================
# 7. CHART -- chosen proxy, WR1/WR2/WR3 panels, qualified callers, 3 named
# =============================================================================
cat("\n=========================================================\n")
cat("7. CHART\n")
cat("=========================================================\n")

wr_tab <- if (chosen_proxy == "contested") {
  contested_dc[tier %in% c("WR1", "WR2", "WR3"),
               .(off_caller, tier, value = era_shrunk, n, rank = rank_open, n_in_tier)]
} else {
  comp_oe[tier %in% c("WR1", "WR2", "WR3"),
          .(off_caller, tier, value = comp_oe, n, rank = rank_comp, n_in_tier)]
}
proxy_label <- if (chosen_proxy == "contested") "contested-target rate, era-adjusted and shrunk\n(lower = receivers get a more open look)" else
  "completion rate over a depth-and-situation model,\npercentage points (higher = easier throws than expected)"
proxy_axis_fmt <- if (chosen_proxy == "contested") function(x) paste0(x, "pp") else function(x) paste0(ifelse(x > 0, "+", ""), x, "pp")

# order callers consistently across all three panels by their WR2 value (the
# secondary-receiver tier is the scheme-depth story this ask is really about);
# a caller missing a WR2 cell orders by the mean of whatever tiers he has
order_key <- wr_tab[, .(ord = { v <- value[tier == "WR2"]; if (length(v)) v[1] else mean(value) }), by = off_caller]
wr_tab <- merge(wr_tab, order_key, by = "off_caller", all.x = TRUE)
lvl <- unique(wr_tab[order(ord)]$off_caller)
wr_tab[, off_caller := factor(off_caller, levels = lvl)]
wr_tab[, hl := off_caller %in% THREE]

med_lines <- wr_tab[, .(med = median(value)), by = tier]
lab_wr <- wr_tab[hl == TRUE]

hl_colour <- setNames(c("#7a1a1a", "#8a3d00", "#08306b"), THREE)
lab_wr[, colour := hl_colour[as.character(off_caller)]]

kyle_wr2 <- wr_tab[off_caller == KYLE & tier == "WR2"]
kyle_wr1 <- wr_tab[off_caller == KYLE & tier == "WR1"]
title_txt <- if (nrow(kyle_wr2) && nrow(kyle_wr1)) {
  sprintf("Shanahan: %s of %d most open on WR2, %s of %d on WR1",
          ordinal_lite(kyle_wr2$rank), kyle_wr2$n_in_tier, ordinal_lite(kyle_wr1$rank), kyle_wr1$n_in_tier)
} else "Openness by receiver tier, per play-caller"
if (chosen_proxy == "comp_oe") {
  title_txt <- gsub("most open", "most completions over expected", title_txt)
}

p <- ggplot(wr_tab, aes(value, off_caller)) +
  geom_vline(data = med_lines, aes(xintercept = med), colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_point(colour = "#9db6c9", size = 2.1, alpha = 0.75) +
  geom_point(data = wr_tab[hl == TRUE], aes(colour = as.character(off_caller)), size = 3.4) +
  geom_text_repel(data = lab_wr, aes(label = off_caller, colour = as.character(off_caller)),
                   size = 2.9, fontface = "bold", seed = 11, box.padding = 0.4,
                   min.segment.length = 0, max.overlaps = 40, direction = "y") +
  scale_colour_manual(values = hl_colour, guide = "none") +
  scale_x_continuous(labels = proxy_axis_fmt) +
  facet_wrap(~ tier, ncol = 3) +
  labs(
    title = title_txt,
    subtitle = paste0(
      "Each dot is one play-caller's ", if (chosen_proxy == "contested") "contested-target rate" else "completion rate over a depth-and-situation model",
      " for that receiver tier (", format(sum(off_n[off_caller %in% qualified]$N), big.mark = ","), " qualified-caller offensive plays, ",
      min(SEASONS), "-", max(SEASONS), ").\nDashed line = league median for that tier. Same callers, same order, all three panels, sorted by the WR2 value."),
    x = proxy_label, y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.55), colour = ink_body),
        panel.spacing = unit(1.4, "lines"),
        plot.margin = margin(10, 14, 8, 10))

fig_note <- paste0(
  "Sumer has no separation-distance metric; contested-target rate (who gets contested at the catch point) and ",
  "completion rate over a depth-and-situation model (who completes more than throw depth predicts) are the closest ",
  "honest proxies for 'open,' not direct measures of it (see R/35). Tight end and running back context rows (each ",
  "team-season's most-targeted player at that position) are in the CSV, not charted here to keep this figure readable.")

p_full <- p + labs(caption = paste(strwrap(fig_caption(
  "plays_players_p1/p2.csv.gz (contested_target) + load_sumer() completion model (depth_of_target + situation, leave-one-season-out xgboost)",
  sprintf("%d callers with >= %d offensive plays, >= %d targets in a tier cell, regular season, non-garbage-time, %d-%d.",
          uniqueN(wr_tab$off_caller), MIN_OFF_PLAYS, MIN_CELL, min(SEASONS), max(SEASONS)),
  fig_note
), width = 150), collapse = "\n"))

save_fig("docs/figures/wr_open_by_tier.png", p_full, w = 14, h = 9.5)

# =============================================================================
# 8. WRITE OUTPUT
# =============================================================================
cat("\n=========================================================\n")
cat("8. WRITE OUTPUT\n")
cat("=========================================================\n")

out <- merge(
  contested_dc[, .(off_caller, tier, n_contested = n, contested_rate = rate, contested_era_adj = era_adj,
                    contested_lo = lo, contested_hi = hi, contested_era_shrunk = era_shrunk, rank_open, n_in_tier_contested = n_in_tier)],
  comp_oe[, .(off_caller, tier, n_comp = n, comp_actual, comp_expected, comp_oe, comp_lo = lo, comp_hi = hi,
              rank_comp, n_in_tier_comp = n_in_tier)],
  by = c("off_caller", "tier"), all = TRUE)
setorder(out, tier, off_caller)
write_csv(as.data.frame(out), "data/derived/wr_open_by_tier.csv")
cat(sprintf("wrote data/derived/wr_open_by_tier.csv (%d rows: caller x tier)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat("No separation-distance metric exists in Sumer; contested-target rate and completion-over-expected\n")
cat("remain the closest honest proxies, now split by receiver tier instead of pooled per caller.\n")
cat(sprintf("\nProxy charted: %s (higher share of real signal vs. sampling noise on WR1-3, section 5).\n",
            if (chosen_proxy == "contested") "contested-target rate" else "completion rate over expected"))
cat(sprintf("Cells dropped for < %d targets: %d (contested-rate measure), %d (completion-over-expected measure).\n",
            MIN_CELL, n_dropped_contested, n_dropped_comp))
cat("\nFiles written:\n")
cat("  data/derived/wr_open_by_tier.csv\n")
cat("  docs/figures/wr_open_by_tier.png\n")
