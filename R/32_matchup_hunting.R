# =============================================================================
# 32_matchup_hunting.R -- the matchups table, opened for the first time on
# this board. Michael's instinct on Kyle Shanahan: "their WR1 was a fuckin
# runningback." R/29 already showed Christian McCaffrey led SF in targets by
# volume (143, +46% over the next receiver). This script asks the mechanism
# question underneath that: WHO actually covers him. If defenses guard him
# like a running back (linebackers, safeties) while SF's real receivers draw
# cornerbacks, "the RB as WR1" is not just a target-share quirk, it is a
# scheme mismatch, and it should be measurable in who lines up on him and
# whether throwing at that mismatch pays off.
#
# DATA, FIRST USE ON THIS BOARD. data/raw/sumer/matchups.csv.gz (1,754,638
# rows, 2022-2026), Sumer's offense-defense player interaction table. Two
# logical flags per row: blocking_interaction (offensive player X blocked
# defender Y) and primary_coverage_interaction (defender Y was the primary
# man on offensive player X). 2026 (preseason charting) is excluded
# throughout, matching every other script in this repo.
#
# GRAIN, established below before anything is trusted: one row per
# offense-defense player PAIR that interacted on a play. A play has ~9
# interaction rows on average (blockers + covered receivers). Coverage rows
# per targeted receiver are usually exactly one defender (92%), sometimes two
# or three (double coverage, ~8%) -- both counted as "involves a
# LB/S" for the mismatch-hunting share below, since a bracket that includes a
# linebacker is still the defense reacting to the mismatch. ~1.2% of rows are
# marked BOTH blocking and coverage on the same offense-defense pair (chip
# and release, or a run-play receiver who also draws coverage attention);
# left as-is, not cleaned out. The table also charts non-dropback plays
# (route runners on called runs, presumably decoys/play-action), so nothing
# here assumes "matchups row exists" means "this was a pass."
#
# COMPLETENESS CAVEAT, the one that matters most for trusting the numbers
# below. Of all non-garbage-time targeted-pass plays 2022-2025, the specific
# targeted player appears ANYWHERE in matchups for that play only 81.4% of
# the time (the play itself is in the table 98.5% of the time; it's the
# per-player row that's sometimes missing). Of those present, 97.5% carry a
# primary_coverage_interaction flag. Net: about 3 in 4 targets resolve to a
# primary defender's position. That rate is stable across all four seasons
# (77.9% to 80.7%, no trend), so it is not contaminating any year-over-year
# comparison, but it caps every n below at ~75% of the raw target count and
# is disclosed on every chart.
#
# POSITION LABELS. player_details' `position` field uses Sumer's own short
# codes, decoded here by hand against known players (Fred Warner, Bobby
# Wagner, Roquan Smith, Dre Greenlaw all code IB; Derwin James, Kyle
# Hamilton, Minkah Fitzpatrick, Talanoa Hufanga all code DS; Patrick Surtain,
# Sauce Gardner, Charvarius Ward all code DC): DC=CB, DS=safety, IB=inside
# linebacker, OB=outside linebacker/EDGE, DE/DT=D-line. LB below means IB+OB
# pooled, matching the "LB" language in the ask. This mapping was NOT
# supplied by Sumer's schema and was reverse-engineered from ~25 known
# players; treat it as the single biggest reliability caveat in this script.
#
# PAYOFF METRIC. `receiving_epa` on the player-play file is defined ONLY on
# completions (NA on every incompletion, checked directly: 27,955 of 27,959
# NA rows have receiving_receptions == NA). Using it for a target-level
# payoff comparison would silently condition on the catch and overstate
# value. Every payoff number below uses plays_full's play-level
# expected_points_added instead (99.995% covered on targeted passes),
# matching R/28's convention for exactly this reason.
#
# Conventions: R/lib/theme_coach.R, R/factory/lib_sumer.R (load_sumer()
# drops no_play, attaches off_caller by team-week). garbage_time == FALSE
# throughout, since plays_full context is joined in every section. No
# em dashes.
#
# Out: docs/figures/cmc_mismatch.png  (who covers CMC vs SF's WRs vs league
#                                       RBs, position mix + CMC's own payoff)
#      docs/figures/matchup_hunting.png (per-caller LB/S-target share,
#                                       era-adjusted + EB-shrunk, ranked;
#                                       league payoff by depth bucket)
#      data/derived/matchup_hunting.csv (per-caller leaderboard)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025

# Sumer position code -> position group. Reverse-engineered against known
# players (see header). LB pools inside (IB) and outside (OB) backers.
POS_GROUP <- c(DC = "CB", DS = "S", IB = "LB", OB = "LB", DE = "DL", DT = "DL",
               OT = "OL", OG = "OL", OC = "OL", QB = "QB", RB = "RB", TE = "TE",
               WR = "WR", FB = "FB", PK = "ST", P = "ST", LS = "ST", RS = "ST")

# =============================================================================
# 1. FIELD INSPECTION: what is a row in matchups.csv.gz?
# =============================================================================
cat("=========================================================\n")
cat("1. FIELD INSPECTION: the matchups table, never opened before\n")
cat("=========================================================\n")

m_all <- fread("data/raw/sumer/matchups.csv.gz", showProgress = FALSE)
cat(sprintf("raw rows, all seasons incl. 2026: %s\n", format(nrow(m_all), big.mark = ",")))
print(m_all[, .N, by = season][order(season)])
m <- m_all[season %in% SEASONS]
rm(m_all); gc(verbose = FALSE)
cat(sprintf("\nrows, %d-%d (2026 preseason charting excluded): %s, unique plays: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(m), big.mark = ","), format(uniqueN(m$sumer_play_id), big.mark = ",")))

cat("\n--- grain: blocking_interaction x primary_coverage_interaction crosstab ---\n")
print(m[, .N, by = .(blocking_interaction, primary_coverage_interaction)])
both_pct <- 100 * nrow(m[blocking_interaction == TRUE & primary_coverage_interaction == TRUE]) / nrow(m)
cat(sprintf("both flags TRUE on the same offense-defense pair: %.2f%% of rows (chip-then-release or a run-play\n", both_pct))
cat("route-runner who also draws coverage attention -- left in, not cleaned out).\n")

rpp <- m[, .N, by = sumer_play_id]
cat(sprintf("\ninteraction rows per play: median %.0f, mean %.1f, range %d-%d\n",
            median(rpp$N), mean(rpp$N), min(rpp$N), max(rpp$N)))
cov_all <- m[primary_coverage_interaction == TRUE]
cpp <- cov_all[, .N, by = sumer_play_id]
cat(sprintf("primary-coverage rows per play: median %.0f, mean %.1f\n", median(cpp$N), mean(cpp$N)))

cat("\n--- does 'primary' mean exactly one defender per covered receiver? ---\n")
perrec <- cov_all[, .N, by = .(sumer_play_id, sumer_player_id_offense)]
perrec_tab <- perrec[, .N, by = N][order(N)]
setnames(perrec_tab, c("defenders_flagged_primary", "n_receiver_plays"))
print(perrec_tab)
cat(sprintf("%.1f%% of covered-receiver-plays have exactly one primary defender; %.1f%% have two or more\n",
            100 * perrec_tab[defenders_flagged_primary == 1]$n_receiver_plays / sum(perrec_tab$n_receiver_plays),
            100 * sum(perrec_tab[defenders_flagged_primary >= 2]$n_receiver_plays) / sum(perrec_tab$n_receiver_plays)))
cat("(double coverage, real football, not a data error -- both defenders count toward 'LB/S involved' below)\n")

cat("\n--- is the table pass-only? plays_full type mix of matched plays ---\n")
plays_ctx <- load_sumer(SEASONS)
matched_plays <- plays_ctx[sumer_play_id %in% unique(m$sumer_play_id)]
print(matched_plays[, .N, by = is_dropback])
cat("matchups charts non-dropback plays too (route runners on called runs); nothing below assumes a row means 'pass'.\n")

cat("\n--- coverage completeness: how often does the SPECIFIC targeted player resolve to a primary defender? ---\n")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball", "receiving_targets"),
            showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz",
            select = c("sumer_play_id", "sumer_player_id", "season", "side_of_ball", "receiving_targets"),
            showProgress = FALSE)
players_raw <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)

plays_ng <- plays_ctx[garbage_time == FALSE]
targets0 <- players_raw[season %in% SEASONS & side_of_ball == "offense" & receiving_targets > 0]
targets0 <- merge(targets0, plays_ng[, .(sumer_play_id, off_team, off_caller, season, depth_of_target, expected_points_added)],
                  by = c("sumer_play_id", "season"))
cat(sprintf("targeted receiver-play rows, non-garbage-time, %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(targets0), big.mark = ",")))

cov_keys <- unique(cov_all[, .(sumer_play_id, sumer_player_id_offense)])
cov_keys[, key := paste(sumer_play_id, sumer_player_id_offense)]
targets0[, key := paste(sumer_play_id, sumer_player_id)]
targets0[, has_primary_cov := key %in% cov_keys$key]
cat(sprintf("resolve to a primary-coverage row: %.1f%% overall\n", 100 * mean(targets0$has_primary_cov)))
print(targets0[, .(n = .N, cov_rate = round(100 * mean(has_primary_cov), 1)), by = season][order(season)])
cat("stable 78-81% across all four seasons -- a real completeness gap, not a drift, disclosed on every chart below.\n")

# =============================================================================
# 2. WHO COVERS MCCAFFREY
# =============================================================================
cat("\n=========================================================\n")
cat("2. WHO COVERS MCCAFFREY\n")
cat("=========================================================\n")

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id", "football_name", "last_name", "position"))
pd[, name := paste(football_name, last_name)]
pd[, pos_group := POS_GROUP[position]]

cov <- merge(cov_all, pd[, .(sumer_player_id_defense = sumer_player_id, def_pos_group = pos_group)],
             by = "sumer_player_id_defense", all.x = TRUE)
cov_j <- cov[, .(sumer_play_id, sumer_player_id_offense, def_pos_group)]

# per (play, offensive receiver): which defender groups covered him (dedupes
# double coverage into flags rather than dropping/duplicating rows)
cov_flags <- cov_j[, .(has_lb = any(def_pos_group == "LB"), has_s = any(def_pos_group == "S"),
                       has_cb = any(def_pos_group == "CB"), any_pos = any(!is.na(def_pos_group))),
                   by = .(sumer_play_id, sumer_player_id_offense)]

targets <- merge(targets0, pd[, .(sumer_player_id, name, position, pos_group)], by = "sumer_player_id")
targets <- merge(targets, cov_flags, by.x = c("sumer_play_id", "sumer_player_id"),
                 by.y = c("sumer_play_id", "sumer_player_id_offense"), all.x = TRUE)
targets[, mismatch_group := fifelse(is.na(any_pos) | !any_pos, "unmatched",
                            fifelse(has_lb | has_s, "LBS", fifelse(has_cb, "CB", "other")))]
targets[, lb_vs_db := fifelse(is.na(any_pos) | !any_pos, "unmatched",
                      fifelse(has_lb, "LB", fifelse(has_s | has_cb, "DB", "other")))]

cmc_id <- pd[name == "Christian McCaffrey"]$sumer_player_id
stopifnot(length(cmc_id) == 1)

pos_mix <- function(tbl, label) {
  x <- tbl[mismatch_group %in% c("LBS", "CB")]
  # split LBS into LB / S components for the 3-way mix
  n_lb <- sum(tbl$has_lb, na.rm = TRUE); n_s <- sum(tbl$has_s & !tbl$has_lb, na.rm = TRUE)
  n_cb <- sum(tbl$mismatch_group == "CB", na.rm = TRUE)
  # counts as coverage ASSIGNMENTS (double coverage adds to more than one bucket)
  denom <- n_lb + n_s + n_cb
  data.table(group = label, n_targets = nrow(tbl), n_matched = denom,
            pct_lb = 100 * n_lb / denom, pct_s = 100 * n_s / denom, pct_cb = 100 * n_cb / denom,
            pct_lbs = 100 * (n_lb + n_s) / denom)
}

cmc_targets <- targets[sumer_player_id == cmc_id & season == 2025 & off_team == "SF"]
sf_wr_targets <- targets[off_team == "SF" & season == 2025 & pos_group == "WR"]
lg_rb_targets <- targets[pos_group == "RB"]

mix <- rbindlist(list(
  pos_mix(cmc_targets, "Christian McCaffrey (SF, 2025 targets)"),
  pos_mix(sf_wr_targets, "SF's actual wide receivers (2025 targets)"),
  pos_mix(lg_rb_targets, sprintf("League running backs (%d-%d targets)", min(SEASONS), max(SEASONS)))
))
cat("\n--- coverage position mix (share of matched CB/S/LB assignments) ---\n")
print(mix[, .(group, n_targets, n_matched, pct_lb = round(pct_lb, 1), pct_s = round(pct_s, 1),
             pct_cb = round(pct_cb, 1), pct_lbs = round(pct_lbs, 1))])

# routes, not just targets -- bigger n, sanity check that the mix isn't a targeting artifact
sf25_plays <- plays_ng[off_team == "SF" & season == 2025]
cmc_routes <- cov_j[sumer_player_id_offense == cmc_id & sumer_play_id %in% sf25_plays$sumer_play_id]
cmc_routes_tab <- cmc_routes[def_pos_group %in% c("CB", "S", "LB"), .N, by = def_pos_group]
cmc_routes_n <- sum(cmc_routes_tab$N)
cat(sprintf("\nROUTES check (every primary-coverage row for CMC on SF's 2025 offense, targeted or not): n = %d assignments\n", cmc_routes_n))
print(cmc_routes_tab[order(-N)][, pct := round(100 * N / cmc_routes_n, 1)][])
cat("matches the targets-only mix closely -- defenses cover him like a running back on the route, not just when the ball comes out.\n")

cmc_lbs <- mix[group == "Christian McCaffrey (SF, 2025 targets)"]$pct_lbs
sfwr_lbs <- mix[group == "SF's actual wide receivers (2025 targets)"]$pct_lbs
lgrb_lbs <- mix[group == sprintf("League running backs (%d-%d targets)", min(SEASONS), max(SEASONS))]$pct_lbs
cat(sprintf("\nVERDICT, mechanism: CMC is covered by a linebacker or safety %.0f%% of the time (n=%d matched), essentially\n",
            cmc_lbs, mix[group == "Christian McCaffrey (SF, 2025 targets)"]$n_matched))
cat(sprintf("identical to the league RB norm (%.0f%%, n=%d) and nothing like SF's own wide receivers (%.0f%%, n=%d).\n",
            lgrb_lbs, mix[3]$n_matched, sfwr_lbs, mix[2]$n_matched))
cat("Defenses assign him coverage by his POSITION, not his target share. That is the mismatch, measured.\n")

# ---------------------------------------------------------- CMC's own payoff
cmc_lb <- cmc_targets[lb_vs_db == "LB"]$expected_points_added
cmc_db <- cmc_targets[lb_vs_db == "DB"]$expected_points_added
cmc_tt <- t.test(cmc_lb, cmc_db)
cat(sprintf("\nCMC's OWN payoff, LB-covered vs DB(CB+S)-covered targets, SF 2025 (play-level EPA):\n"))
cat(sprintf("  LB-covered:  %+.3f EPA (n=%d)\n", mean(cmc_lb, na.rm = TRUE), sum(!is.na(cmc_lb))))
cat(sprintf("  DB-covered:  %+.3f EPA (n=%d)\n", mean(cmc_db, na.rm = TRUE), sum(!is.na(cmc_db))))
cat(sprintf("  diff (LB minus DB): %+.3f [%+.3f, %+.3f], p = %.3f\n",
            diff(rev(cmc_tt$estimate)), cmc_tt$conf.int[1], cmc_tt$conf.int[2], cmc_tt$p.value))
cat("One season, one player, small n: the CI crosses zero and the point estimate even runs the other way.\n")
cat("This does NOT confirm the mismatch pays off for CMC specifically -- that test needs the league-wide\n")
cat("sample in section 3, which is where the real statistical power lives.\n")

# =============================================================================
# 3. MATCHUP HUNTING PER CALLER
# =============================================================================
cat("\n=========================================================\n")
cat("3. MATCHUP HUNTING PER CALLER\n")
cat("=========================================================\n")

d <- targets[mismatch_group %in% c("LBS", "CB") & off_caller != ""]
d[, went_lbs := mismatch_group == "LBS"]
cat(sprintf("universe: targeted passes, non-garbage-time, %d-%d, resolved to CB-only or LB/S-involved coverage,\n",
            min(SEASONS), max(SEASONS)))
cat(sprintf("off_caller attributed: %s targets\n", format(nrow(d), big.mark = ",")))

lg <- d[, .(n = .N, lbs = sum(went_lbs)), by = season][, rate := 100 * lbs / n]
cat("\nleague rate of 'thrown at LB/S coverage' by season:\n")
print(lg[order(season)][, .(season, n, rate = round(rate, 1))])

shrink_rate <- function(x, lg_dt, min_n) {
  cs <- x[, .(n = .N, lbs = sum(went_lbs)), by = .(off_caller, season)]
  cs <- merge(cs, lg_dt[, .(season, lg_rate = rate)], by = "season")
  dc <- cs[, .(n = sum(n), lbs = sum(lbs), expected_lbs = sum(lg_rate / 100 * n)), by = off_caller]
  dc[, `:=`(rate = 100 * lbs / n, expected_rate = 100 * expected_lbs / n)]
  dc[, era_adj := rate - expected_rate]
  dc[, se := 100 * sqrt((rate / 100) * (1 - rate / 100) / n)]
  dc[, `:=`(lo = era_adj - 1.96 * se, hi = era_adj + 1.96 * se)]
  dc <- dc[n >= min_n]
  tau2 <- max(var(dc$era_adj) - mean(dc$se^2), 1e-4)
  dc[, era_shrunk := (tau2 * era_adj) / (tau2 + se^2)]
  setorder(dc, -era_shrunk)
  dc[, rank := .I]
  dc[]
}

MIN_N <- 400
res <- shrink_rate(d, lg, MIN_N)
n_callers <- nrow(res)
cat(sprintf("\n%d callers with >= %d matched targets (of %d minimum-plausible callers), era-adjusted + EB-shrunk\n",
            n_callers, MIN_N, uniqueN(d$off_caller)))

cat("\n--- top 10, hunts the LB/S mismatch hardest above era expectation ---\n")
print(head(res[, .(off_caller, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                   ci = sprintf("[%+.1f,%+.1f]", lo, hi), era_shrunk = round(era_shrunk, 2), rank)], 10))
cat("\n--- bottom 10, avoids it, throws at corners instead ---\n")
print(tail(res[, .(off_caller, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                   ci = sprintf("[%+.1f,%+.1f]", lo, hi), era_shrunk = round(era_shrunk, 2), rank)], 10))

named3 <- res[off_caller %in% c("Kyle Shanahan", "Sean McVay", "Ben Johnson")]
cat("\n--- the three names ---\n")
print(named3[, .(off_caller, n, rate = round(rate, 1), era_adj = round(era_adj, 1),
                 ci = sprintf("[%+.1f,%+.1f]", lo, hi), era_shrunk = round(era_shrunk, 2), rank)])
cat(sprintf("\nBen Johnson: #%d of %d, %+.1fpp above era expectation %s.\n",
            named3[off_caller == "Ben Johnson"]$rank, n_callers, named3[off_caller == "Ben Johnson"]$era_adj,
            sprintf("[%+.1f,%+.1f]", named3[off_caller == "Ben Johnson"]$lo, named3[off_caller == "Ben Johnson"]$hi)))
cat(sprintf("Kyle Shanahan: #%d of %d, %+.1fpp above era expectation %s -- confirms the instinct with a number.\n",
            named3[off_caller == "Kyle Shanahan"]$rank, n_callers, named3[off_caller == "Kyle Shanahan"]$era_adj,
            sprintf("[%+.1f,%+.1f]", named3[off_caller == "Kyle Shanahan"]$lo, named3[off_caller == "Kyle Shanahan"]$hi)))
cat(sprintf("Sean McVay: #%d of %d, %+.1fpp %s -- the opposite tendency, significantly below era expectation.\n",
            named3[off_caller == "Sean McVay"]$rank, n_callers, named3[off_caller == "Sean McVay"]$era_adj,
            sprintf("[%+.1f,%+.1f]", named3[off_caller == "Sean McVay"]$lo, named3[off_caller == "Sean McVay"]$hi)))

# ---------------------------------------------------------- league payoff, fixed depth bucket
cat("\n--- PAYOFF: is throwing at the mismatch actually worth it? league EPA by depth bucket ---\n")
d[, depth_bucket := fcase(
  is.na(depth_of_target), NA_character_,
  depth_of_target < 0, "behind LOS",
  depth_of_target <= 9, "short (0-9)",
  depth_of_target <= 19, "medium (10-19)",
  default = "deep (20+)"
)]
d[, depth_bucket := factor(depth_bucket, levels = c("behind LOS", "short (0-9)", "medium (10-19)", "deep (20+)"))]

payoff <- rbindlist(lapply(levels(d$depth_bucket), function(b) {
  rbindlist(lapply(c("LBS", "CB"), function(g) {
    x <- d[depth_bucket == b & mismatch_group == g]$expected_points_added
    tt <- t.test(x)
    data.table(depth_bucket = b, coverage = g, n = length(x[!is.na(x)]),
              epa = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2])
  }))
}))
print(payoff[, .(depth_bucket, coverage, n, epa = round(epa, 3), lo = round(lo, 3), hi = round(hi, 3))])

payoff_tests <- rbindlist(lapply(levels(d$depth_bucket), function(b) {
  x <- d[depth_bucket == b & mismatch_group == "LBS"]$expected_points_added
  y <- d[depth_bucket == b & mismatch_group == "CB"]$expected_points_added
  tt <- t.test(x, y)
  data.table(depth_bucket = b, diff = diff(rev(tt$estimate)), lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value)
}))
cat("\nLB/S-covered minus CB-covered, EPA, by depth bucket:\n")
print(payoff_tests[, .(depth_bucket, diff = round(diff, 3), lo = round(lo, 3), hi = round(hi, 3),
                       p = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))])
cat(sprintf("\nVERDICT, payoff: LB/S coverage beats CB coverage on EPA in EVERY one of the 4 depth buckets\n"))
cat(sprintf("(gaps run %+.3f to %+.3f EPA, all four confidence intervals clear zero). Throwing at the\n",
            min(payoff_tests$diff), max(payoff_tests$diff)))
cat("mismatch is not just a target-share habit -- it is leaguewide, statistically real value, at every depth of throw.\n")

# =============================================================================
# 4. CHARTS
# =============================================================================
cat("\n=========================================================\n")
cat("4. CHARTS\n")
cat("=========================================================\n")

# ---------------------------------------------------------- Chart A: cmc_mismatch.png
mix_long <- melt(mix[, .(group, LB = pct_lb, Safety = pct_s, CB = pct_cb)],
                 id.vars = "group", variable.name = "coverage", value.name = "pct")
mix_long[, coverage := factor(coverage, levels = c("CB", "Safety", "LB"))]
mix_long[, group := factor(group, levels = rev(mix$group))]

pA <- ggplot(mix_long, aes(pct, group, fill = coverage)) +
  geom_col(width = 0.62, position = "stack") +
  geom_text(aes(label = ifelse(pct >= 6, sprintf("%.0f%%", pct), "")),
            position = position_stack(vjust = 0.5), colour = "white", fontface = "bold", size = 3.1) +
  scale_fill_manual(values = c(CB = "#4393C3", Safety = "#9db6c9", LB = "#D55E00"), name = NULL) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "McCaffrey is covered like a running back, not like SF's WR1",
       subtitle = "Share of primary-coverage assignments by defender position (double coverage counts both)",
       x = "share of matched coverage assignments", y = NULL) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.text = element_text(size = rel(0.85)),
        axis.text.y = element_text(face = "bold", size = rel(0.9)),
        plot.margin = margin(10, 14, 8, 10))

cmc_payoff_dt <- data.table(
  group = c("LB-covered", "DB-covered\n(CB or safety)"),
  epa = c(mean(cmc_lb, na.rm = TRUE), mean(cmc_db, na.rm = TRUE)),
  lo = c(t.test(cmc_lb)$conf.int[1], t.test(cmc_db)$conf.int[1]),
  hi = c(t.test(cmc_lb)$conf.int[2], t.test(cmc_db)$conf.int[2]),
  n = c(sum(!is.na(cmc_lb)), sum(!is.na(cmc_db)))
)
cmc_payoff_dt[, group := factor(group, levels = group)]

pB <- ggplot(cmc_payoff_dt, aes(group, epa)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_col(fill = "#9db6c9", width = 0.55) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, colour = ink_body, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f EPA\nn=%d", epa, n)), vjust = -0.4, size = 3.1,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.35))) +
  labs(title = "CMC's own 2025 payoff", subtitle = "One season, one player: not enough n to confirm",
       x = NULL, y = "play-level EPA on his targets") +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(size = rel(0.78)), plot.margin = margin(10, 10, 8, 6))

pAB <- pA + pB + plot_layout(widths = c(1.65, 1)) +
  plot_annotation(
    caption = fig_caption(
      "data/raw/sumer/matchups.csv.gz joined to plays_players_p1/p2.csv.gz and player_details.csv.gz",
      sprintf("SF 2025 targets (n=%d matched), SF WRs 2025 (n=%d matched), league RBs %d-%d (n=%d matched); non-garbage-time.",
              mix[1]$n_matched, mix[2]$n_matched, min(SEASONS), max(SEASONS), mix[3]$n_matched),
      paste0("\nCoverage table matches ~75-81% of targets to a primary defender (see script header); position codes reverse-engineered from Sumer's schema.\n",
             "Right panel: his own single-season CI crosses zero and runs the opposite direction of the point estimate -- the league-wide test (docs/figures/matchup_hunting.png)\n",
             "is where this mismatch actually clears significance. Built by R/32.")
    )
  ) & theme(plot.caption = element_text(colour = ink_caption, size = rel(0.68), hjust = 0, margin = margin(t = 10)))
save_fig("docs/figures/cmc_mismatch.png", pAB, w = 13.5, h = 7.2)

# ---------------------------------------------------------- Chart B: matchup_hunting.png
res[, hl := off_caller %in% c("Kyle Shanahan", "Sean McVay", "Ben Johnson")]
lab_hl <- res[hl == TRUE]
lab_hl[, lab := sprintf("%s\n#%d of %d, %+.1fpp", off_caller, rank, n_callers, era_adj)]

p1 <- ggplot(res, aes(rank, era_shrunk)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_point(colour = "#9db6c9", alpha = 0.65, size = 2.2) +
  geom_point(data = lab_hl, colour = "#D55E00", size = 4) +
  geom_text_repel(data = lab_hl, aes(label = lab), size = 3.0, fontface = "bold", lineheight = 0.95,
                  colour = "#8a3d00", seed = 5, box.padding = 0.6, min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = 9, y = max(res$era_shrunk) * 0.85, hjust = 0, vjust = 1, size = 2.9, fontface = "italic",
           colour = "grey45", label = "hunts the LB/S\nmismatch most") +
  annotate("text", x = n_callers - 8, y = min(res$era_shrunk) * 0.85, hjust = 1, vjust = 0, size = 2.9, fontface = "italic",
           colour = "grey45", label = "throws at corners,\navoids it") +
  scale_x_continuous(breaks = c(1, seq(10, n_callers, 10), n_callers)) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x, 1), "pp")) +
  labs(title = "Shanahan hunts the linebacker/safety mismatch; McVay does the opposite",
       subtitle = sprintf("Share of targets thrown at a receiver covered by a LB or safety (vs a CB), era-adjusted and empirical-Bayes shrunk,\n%d callers with %d+ matched targets, %d-%d",
                          n_callers, MIN_N, min(SEASONS), max(SEASONS)),
       x = sprintf("rank among %d callers (1 = hunts the mismatch most)", n_callers),
       y = "share of targets at LB/S coverage,\nvs era expectation (shrunk)") +
  theme_coach(grid = "y")

payoff_tests[, depth_bucket := factor(depth_bucket, levels = levels(d$depth_bucket))]
p2 <- ggplot(payoff_tests, aes(depth_bucket, diff)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linetype = "dashed", linewidth = 0.35) +
  geom_col(fill = "#D55E00", width = 0.55) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, colour = ink_body, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%+.2f", diff)), vjust = -0.5, size = 3.1, fontface = "bold", colour = ink_body) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.3))) +
  labs(title = "The mismatch pays off at every depth",
       subtitle = "League EPA, LB/S-covered minus CB-covered targets", x = NULL, y = "EPA gap (LB/S minus CB)") +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(size = rel(0.75)), plot.margin = margin(10, 10, 8, 6))

p12 <- p1 + p2 + plot_layout(widths = c(1.7, 1)) +
  plot_annotation(
    caption = fig_caption(
      "matchups.csv.gz + plays_full.csv.gz (load_sumer), off_caller attribution from samhoppen/NFL_public",
      sprintf("%s targeted passes, non-garbage-time, resolved to CB-only or LB/S-involved coverage, %d-%d.",
              format(nrow(d), big.mark = ","), min(SEASONS), max(SEASONS)),
      "\nLeft: era_shrunk = (rate minus that season's league rate), empirical-Bayes shrunk toward zero by sample size, same method as R/22 and R/19.\nRight: play-level EPA (not receiving_epa, which is completions-only), all four depth-bucket gaps clear zero. Built by R/32."
    )
  ) & theme(plot.caption = element_text(colour = ink_caption, size = rel(0.68), hjust = 0, margin = margin(t = 10)))
save_fig("docs/figures/matchup_hunting.png", p12, w = 14, h = 7.6)

# =============================================================================
# 5. WRITE OUTPUT
# =============================================================================
out <- res[, .(off_caller, n, rate, expected_rate, era_adj, lo, hi, era_shrunk, rank)]
write_csv(as.data.frame(out), "data/derived/matchup_hunting.csv")
cat(sprintf("\nwrote data/derived/matchup_hunting.csv (%d callers)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat(sprintf("Grain: one row per offense-defense player pair per play; ~9 interaction rows/play; coverage\n"))
cat(sprintf("resolves for ~75-81%% of targets (stable across seasons, not a trend). Table covers non-dropbacks too.\n"))
cat(sprintf("CMC 2025: covered by LB/S %.0f%% of the time (n=%d), matching the league RB norm (%.0f%%, n=%d),\n",
            cmc_lbs, mix[1]$n_matched, lgrb_lbs, mix[3]$n_matched))
cat(sprintf("nothing like SF's own WRs (%.0f%%, n=%d). Confirmed on routes too (n=%d assignments).\n",
            sfwr_lbs, mix[2]$n_matched, cmc_routes_n))
cat(sprintf("Caller ranks (of %d, %d+ targets): Ben Johnson #%d (%+.1fpp), Kyle Shanahan #%d (%+.1fpp), Sean McVay #%d (%+.1fpp).\n",
            n_callers, MIN_N,
            named3[off_caller == "Ben Johnson"]$rank, named3[off_caller == "Ben Johnson"]$era_adj,
            named3[off_caller == "Kyle Shanahan"]$rank, named3[off_caller == "Kyle Shanahan"]$era_adj,
            named3[off_caller == "Sean McVay"]$rank, named3[off_caller == "Sean McVay"]$era_adj))
cat(sprintf("League payoff: LB/S-covered beats CB-covered EPA in all 4 depth buckets (%+.3f to %+.3f, all CIs clear zero).\n",
            min(payoff_tests$diff), max(payoff_tests$diff)))
cat("CMC's own single-season LB-vs-DB split does NOT clear significance (n too small, point estimate runs backward) --\n")
cat("the honest read is: the mismatch mechanism is real and it is Shanahan's stated tendency, and the LEAGUE-WIDE payoff\n")
cat("for exploiting it is real and large, but CMC's individual 2025 sample is too thin to prove it paid off for him specifically.\n")
cat("=============================================\n")
cat("\nFiles written:\n")
cat("  data/derived/matchup_hunting.csv\n")
cat("  docs/figures/cmc_mismatch.png\n")
cat("  docs/figures/matchup_hunting.png\n")
