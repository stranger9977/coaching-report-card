# =============================================================================
# 16_def_sequencing.R
#
# Nick, verbatim: "im also curious about defensive play call sequencing too"
# ... "in different game situations etc."
#
# Three questions about defensive coordinators that R/factory/84, 85 and 87
# do not already answer (85 covers coverage DISGUISE -- shell rotation and
# blitz bluff/surprise; this is a different question: not whether he shows one
# thing and plays another, but whether his own sequence of real decisions has
# a pattern):
#   1. SEQUENCE LIFT -- does a DC's PREVIOUS blitz call predict his NEXT one,
#      beyond what the situation already explains? "Hot hand" strings blitzes
#      together beyond situation; a "mixer" calls each snap closer to
#      independent.
#   2. SITUATIONAL RANGE -- how much does a DC's blitz tendency (net of
#      situation) move across game contexts (early down, late down, red
#      zone, two-minute, trailing, leading) versus staying flat everywhere?
#   3. PREDICTABILITY PAYOFF, DEFENSIVE SIDE -- R/07 found predictable
#      offensive calling is BETTER for the offense. Same question on defense:
#      is a guessable blitz decision (within situation) associated with
#      better or worse defensive EPA/play allowed, or nothing?
#   4. PERSONNEL SHIFT -- computed directly per DC-season-team from charted
#      snaps (not scraped team pages): how far does a coordinator's own
#      defensive personnel mix move between early-down and late-down snaps?
#
# THE SEATTLE TIE-IN. Mike Macdonald called Baltimore's defense as DC in
# 2022-2023, then became Seattle's HC in 2024 and, per playcallers.csv, calls
# Seattle's defense himself (listed as BOTH head_coach and def_play_caller
# for SEA in every 2024 and 2025 game). Clint Hurtt held SEA's call in
# 2022-2023. Because he changes team and role inside the very window this
# script covers, every test reports his BAL-era and SEA-era numbers
# separately as well as his career number.
#
# METHOD, same house rule as R/factory/85: a raw per-DC rate mostly ranks
# coordinators by the situations they faced, so nothing here is read off a
# raw rate. Tests 1-2 residualize against a season-grouped, out-of-sample
# situation-only model (down, distance, field position, quarter, score
# differential, two/four-minute, red zone, short yardage, goal line) via
# R/factory/lib_sumer.R's sumer_expect(); a DC's residual (actual - expected)
# is his choice, not the schedule's. EB shrinkage follows the shrink() form
# already used in nfl-analysis/scripts/blitz-autopilot.R: k = (within-unit
# variance) / (across-unit variance of true means), posterior = precision-
# weighted blend toward the grand mean. Nothing is claimed until it persists
# (persist_split(), the same odd/even-season test as R/factory/85 and 89).
#
# DATA SOURCE. Primary: SumerSports play-level charting, data/raw/sumer/
# plays_full.csv (193 fields, 2022-2025, licensed -- raw pull stays out of
# the repo, data/raw/sumer/ is gitignored), loaded via R/factory/lib_sumer.R's
# load_sumer() (maps team codes, attaches off/def play-caller by team-week
# from playcallers.csv). Sumer's own `blitz` field is exactly pass_rush_count
# >= 5 (checked: perfect 1:1 correspondence, no exceptions in 85,157
# dropbacks) -- the same "5+ rushers" definition used everywhere else on this
# board. FTN charting (n_pass_rushers >= 5, joined to nflverse pbp dropbacks,
# 2022-2025) is used ONLY as an external season-level cross-check, not
# play-level: lib_sumer.R's own join notes say Sumer's gsis_game_id/
# gsis_play_id are GSIS's internal numeric game key, NOT nflverse's
# old_game_id/play_id, and this was verified empirically here (0 of 53,673
# game-ids matched) before trusting the warning rather than working around
# it. So the check compares each season's marginal blitz rate, not
# play-by-play agreement.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/def_seq_lift.png
#      docs/figures/def_situational.png
#      docs/figures/def_shapeshift.png
#      data/derived/def_seq_callers.csv
#      data/derived/def_shapeshift.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
SEASONS <- 2022:2025
MIN_CAREER <- 700  # matches lib_sumer.R's own min_career default

# =============================================================================
# 1. LOAD, ATTRIBUTE, CROSS-CHECK
# =============================================================================

plays <- load_sumer(SEASONS)
d <- plays[is_dropback == TRUE & no_play == FALSE & def_caller != "" & !is.na(blitz)]
cat(sprintf("Sumer dropbacks %s, DC attributed and blitz-flagged: %s\n",
            paste(range(SEASONS), collapse = "-"), format(nrow(d), big.mark = ",")))

# blitz field sanity check: is it really pass_rush_count >= 5?
chk <- d[!is.na(pass_rush_count)]
match_rate <- mean(chk$blitz == (chk$pass_rush_count >= 5))
cat(sprintf("Sumer blitz field vs pass_rush_count >= 5: %.2f%% match (%s plays)\n",
            100 * match_rate, format(nrow(chk), big.mark = ",")))

# SEA / BAL defensive caller, straight from playcallers.csv
pc <- fread(NFLA_PC, showProgress = FALSE)
cat("\nplaycallers.csv, SEA defensive caller, 2024-2025:\n")
print(pc[team == "SEA" & season %in% 2024:2025, .N, by = .(season, def_play_caller, head_coach)])
cat("playcallers.csv, BAL defensive caller, 2022-2023:\n")
print(pc[team == "BAL" & season %in% 2022:2023, .N, by = .(season, def_play_caller)])

# external season-level cross-check against FTN (marginal rates only; the two
# sources' play ids do not join, see header)
ftn <- rbindlist(lapply(SEASONS, function(y)
  fread(file.path(NFLA, "data", sprintf("ftn_charting_%d.csv.gz", y)),
        select = c("nflverse_game_id", "nflverse_play_id", "season", "n_pass_rushers"),
        showProgress = FALSE)))
setnames(ftn, c("nflverse_game_id", "nflverse_play_id"), c("game_id", "play_id"))
pbp_flag <- rbindlist(lapply(SEASONS, function(y)
  fread(file.path(NFLA, "data", sprintf("play_by_play_%d.csv.gz", y)),
        select = c("game_id", "play_id", "qb_dropback", "qb_kneel", "qb_spike"),
        showProgress = FALSE)))
pbp_flag <- pbp_flag[qb_dropback == 1 & qb_kneel == 0 & qb_spike == 0]
ftn_db <- merge(pbp_flag[, .(game_id, play_id)], ftn, by = c("game_id", "play_id"))
ftn_db[, blitz := n_pass_rushers >= 5]
cmp <- merge(d[, .(sumer_rate = 100 * mean(blitz), sumer_n = .N), by = season],
            ftn_db[!is.na(blitz), .(ftn_rate = 100 * mean(blitz), ftn_n = .N), by = season],
            by = "season")
cmp[, gap := sumer_rate - ftn_rate]
cat("\nSumer vs FTN, season-level blitz rate (marginal, no play-level join):\n")
print(cmp[order(season)])
cat(sprintf("2023-2025 gap averages %.1fpp; 2022 (FTN's first charted season) gap is %.1fpp, the outlier.\n",
            mean(abs(cmp[season >= 2023]$gap)), cmp[season == 2022]$gap))

# =============================================================================
# 2. SITUATION-ONLY MODEL (season-grouped, out-of-sample), residualize blitz
# =============================================================================

career_n <- d[, .N, by = def_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_caller]
d2 <- d[def_caller %in% dcs_ok]
cat(sprintf("\nDCs with >= %d charted dropbacks, %d-%d: %d\n",
            MIN_CAREER, min(SEASONS), max(SEASONS), length(dcs_ok)))

NAMED_DCS <- c("Mike Macdonald", "Steve Spagnuolo", "Vic Fangio", "Brian Flores", "Wade Phillips")
present_named <- intersect(NAMED_DCS, dcs_ok)
missing_named <- setdiff(NAMED_DCS, dcs_ok)
cat(sprintf("famous DCs present at that minimum: %s\n", paste(present_named, collapse = ", ")))
if (length(missing_named)) cat(sprintf("not in the 2022-2025 Sumer window at that minimum: %s\n",
                                       paste(missing_named, collapse = ", ")))

exp_d <- sumer_expect(d2, "blitz")
exp_d[, resid := blitz - .expected]
cat(sprintf("situation-only model: mean actual %.4f vs mean expected %.4f (out-of-sample by season)\n",
            mean(exp_d$blitz), mean(exp_d$.expected)))

shrink_eb <- function(m, n, v) {
  pv <- max(var(m) - mean(v / n), 1e-6)
  k  <- mean(v) / pv
  (n * m + k * weighted.mean(m, n)) / (n + k)
}

# =============================================================================
# TEST 1: SEQUENCE LIFT -- does the DC's own previous blitz call move his
# residual (blitz net of situation) on the next charted dropback?
# =============================================================================

setorder(exp_d, def_caller, sumer_game_id, play_sequence_number)
exp_d[, prev_blitz := shift(blitz), by = .(def_caller, sumer_game_id)]
seqd <- exp_d[!is.na(prev_blitz)]

lift <- seqd[, .(n1 = sum(prev_blitz), m1 = mean(resid[prev_blitz]), v1 = var(resid[prev_blitz]),
                 n0 = sum(!prev_blitz), m0 = mean(resid[!prev_blitz]), v0 = var(resid[!prev_blitz])),
            by = def_caller]
lift[, lift_raw := m1 - m0]
lift[, se := sqrt(v1 / n1 + v0 / n0)]
lift <- lift[is.finite(se) & se > 0]
lift[, n_seq := n1 + n0]

grand_lift <- weighted.mean(lift$lift_raw, 1 / lift$se^2)
tau2_lift <- max(var(lift$lift_raw) - mean(lift$se^2), 1e-6)
lift[, w := tau2_lift / (tau2_lift + se^2)]
lift[, lift_shrunk := grand_lift + w * (lift_raw - grand_lift)]
lift[, se_post := sqrt(w * se^2)]
lift[, `:=`(lo = lift_shrunk - 1.96 * se_post, hi = lift_shrunk + 1.96 * se_post)]
setorder(lift, -lift_shrunk)
lift[, rank := .I]

cat(sprintf("\n--- TEST 1: SEQUENCE LIFT (n = %d DCs, min %d dropbacks, residual = blitz minus situation-expected) ---\n",
            nrow(lift), MIN_CAREER))
cat(sprintf("league median shrunk lift: %+.3f | grand mean (precision-weighted): %+.3f\n",
            median(lift$lift_shrunk), grand_lift))
cat("most hot-hand (top 5):\n")
print(lift[1:5, .(def_caller, rank, n_seq, lift_raw = round(lift_raw, 3), lift_shrunk = round(lift_shrunk, 3))])
cat("most mixing (bottom 5):\n")
print(tail(lift, 5)[, .(def_caller, rank, n_seq, lift_raw = round(lift_raw, 3), lift_shrunk = round(lift_shrunk, 3))])
cat("named DCs:\n")
print(lift[def_caller %in% present_named,
          .(def_caller, rank, n_seq, lift_raw = round(lift_raw, 3), lift_shrunk = round(lift_shrunk, 3))])
verdict1 <- if (grand_lift > 0.01) {
  "league-wide, defenses run slightly hot even net of situation: blitzing last snap makes blitzing this one a bit more likely."
} else "no consistent league-wide sequence pattern once situation is residualized out."
cat("verdict:", verdict1, "\n")

# =============================================================================
# TEST 2: SITUATIONAL RANGE -- how much does the residual (blitz net of
# situation) itself move across game contexts, per DC?
# =============================================================================

exp_d[, def_score_diff := -offense_score_diff]
CUTS <- list(
  early_down = quote(down %in% 1:2), late_down = quote(down %in% 3:4),
  red_zone = quote(redzone == TRUE), two_minute = quote(two_minute == TRUE),
  trailing = quote(def_score_diff <= -9), leading = quote(def_score_diff >= 9)
)
cut_order <- names(CUTS)
cut_labs <- c(early_down = "Early down", late_down = "Late down", red_zone = "Red zone",
             two_minute = "Two-minute", trailing = "Trailing", leading = "Leading")

situ <- rbindlist(lapply(cut_order, function(cn) {
  sub <- exp_d[eval(CUTS[[cn]])]
  agg <- sub[, .(n = .N, resid_m = mean(resid), v = var(resid), rate = mean(blitz),
                league = sub[, mean(blitz)]), by = def_caller][n >= 30]
  agg[, cut := cn]
  agg
}))
situ[, resid_shrunk := shrink_eb(resid_m, n, v), by = cut]
situ[, rate_shrunk := league + resid_shrunk]  # for the intuitive, in-percent chart

wide <- dcast(situ, def_caller ~ cut, value.var = "resid_shrunk")
wide <- wide[complete.cases(wide)]
wide[, range_sd := apply(.SD, 1, sd), .SDcols = cut_order]
wide[, range_mm := apply(.SD, 1, function(x) max(x) - min(x)), .SDcols = cut_order]
setorder(wide, -range_sd)
wide[, range_rank := .I]

cat(sprintf("\n--- TEST 2: SITUATIONAL RANGE (n = %d DCs with all 6 cuts, residual scale) ---\n", nrow(wide)))
cat("changes shape the most (top 5 by SD of residual across cuts):\n")
print(head(wide[, .(def_caller, range_rank, range_sd = round(range_sd, 4))], 5))
cat("calls the same game everywhere (bottom 5):\n")
print(tail(wide[, .(def_caller, range_rank, range_sd = round(range_sd, 4))], 5))
mac_row <- wide[def_caller == "Mike Macdonald"]
cat(sprintf("Macdonald's situational range rank: %d of %d (SD = %.4f)\n",
            mac_row$range_rank, nrow(wide), mac_row$range_sd))
cat("Macdonald's blitz rate by cut (EB-shrunk, in percent):\n")
print(situ[def_caller == "Mike Macdonald",
          .(cut, n, rate = round(100 * rate, 1), league = round(100 * league, 1),
            rate_shrunk = round(100 * rate_shrunk, 1))][order(cut)])

# =============================================================================
# BONUS PERSISTENCE CHECK: is the underlying situation-adjusted blitz
# tendency itself a repeatable trait of the man (house rule from R/factory/85
# and 89: nothing is claimed until it persists)?
# =============================================================================

res <- sumer_resid(d2, "blitz", who = "def_caller", min_season = 200, min_career = MIN_CAREER)
ps <- persist_split(res$season, min_half = 300)
cat(sprintf("\n--- PERSISTENCE CHECK: does over/under-blitzing (vs situation) repeat, odd vs even season? ---\n"))
cat(sprintf("n = %d DCs with both halves, r = %+.3f [%.3f, %.3f], p = %.4f -- %s\n",
            ps$n, ps$r, ps$lo, ps$hi, ps$p, ps$verdict))

# =============================================================================
# TEST 3: PREDICTABILITY PAYOFF, DEFENSIVE SIDE -- within-cell entropy of the
# blitz call (guessability), correlated with defensive EPA/play allowed.
# =============================================================================

d2[, dist_bucket := fcase(distance <= 3, "short", distance <= 7, "medium", default = "long")]
d2[, def_score_diff := -offense_score_diff]
d2[, score_state := fcase(def_score_diff >= 9, "leading", def_score_diff <= -9, "trailing", default = "close")]
d2[, half := fifelse(quarter <= 2, "H1", "H2")]
d2[, cell := paste(down, dist_bucket, score_state, half, sep = "|")]

cellp <- d2[, .(n = .N, p = mean(blitz)), by = .(def_caller, cell)][n >= 10]
cellp[, H := ifelse(p %in% c(0, 1), 0, -(p * log2(p) + (1 - p) * log2(1 - p)))]
dcent <- cellp[, .(entropy = weighted.mean(H, n), n_plays = sum(n)), by = def_caller]
dcent[, guessability := 1 - entropy]

epa_dc <- d2[, .(epa_allowed = mean(expected_points_added, na.rm = TRUE)), by = def_caller]
pred_pay <- merge(dcent, epa_dc, by = "def_caller")
pred_pay <- pred_pay[n_plays >= MIN_CAREER]
ct3 <- cor.test(pred_pay$guessability, pred_pay$epa_allowed)

cat(sprintf("\n--- TEST 3: PREDICTABILITY PAYOFF, DEFENSE (n = %d DCs) ---\n", nrow(pred_pay)))
cat(sprintf("cor(guessability of the blitz call, defensive EPA/play allowed) = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n",
            ct3$estimate, ct3$conf.int[1], ct3$conf.int[2], ct3$p.value))
sign_note <- if (ct3$p.value > 0.10) {
  "NULL RESULT: no detectable relationship. Unlike the offense (R/07: predictable calling correlates with BETTER EPA), a defense's blitz guessability shows no signal either way on this sample."
} else if (ct3$estimate > 0) {
  "guessable defenses allow MORE EPA (worse) -- opposite sign from the offensive finding."
} else {
  "guessable defenses allow LESS EPA (better) -- same direction as the offensive finding."
}
cat("verdict:", sign_note, "\n")
cat("named DCs (guessability, defensive EPA/play allowed):\n")
print(pred_pay[def_caller %in% present_named,
              .(def_caller, guessability = round(guessability, 3),
                epa_allowed = round(epa_allowed, 3))][order(-guessability)])
cat(sprintf("league median guessability: %.3f | median EPA/play allowed: %+.3f\n",
            median(pred_pay$guessability), median(pred_pay$epa_allowed)))

# merge tests 1-3 into one per-DC leaderboard file
caller_tab <- merge(lift[, .(def_caller, n_seq, lift_raw, lift_shrunk, seq_rank = rank)],
                    wide[, .(def_caller, range_sd, range_mm, range_rank)], all = TRUE)
caller_tab <- merge(caller_tab,
                    pred_pay[, .(def_caller, entropy, guessability, epa_allowed)], all = TRUE)
caller_tab <- merge(caller_tab, career_n[, .(def_caller, n_career = N)], all.x = TRUE)
setnames(caller_tab, "def_caller", "def_play_caller")
write_csv(as.data.frame(caller_tab), "data/derived/def_seq_callers.csv")

# =============================================================================
# CHART A: distribution of shrunk sequence lift, famous DCs + Macdonald
# =============================================================================

lab_a <- lift[def_caller %in% present_named]
p1 <- ggplot(lift, aes(rank, lift_shrunk)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_hline(yintercept = grand_lift, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = "#9db6c9", alpha = 0.55) +
  geom_point(colour = "#9db6c9", size = 1.9, alpha = 0.7) +
  geom_point(data = lab_a, colour = "#D55E00", size = 3) +
  geom_text_repel(data = lab_a, aes(label = def_caller), size = 3.2, fontface = "bold",
                  colour = "#8a3d00", seed = 11, box.padding = 0.5,
                  min.segment.length = 0, max.overlaps = 20) +
  annotate("text", x = 2, y = max(lift$hi), hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Hot hand: blitzes\nfollow blitzes") +
  annotate("text", x = nrow(lift) - 1, y = min(lift$lo), hjust = 1, vjust = 0, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Mixes: next call is\nindependent of the last") +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(100 * x), "pp")) +
  labs(
    title = "Defensive coordinators run hot on their own blitz calls, even net of situation",
    subtitle = sprintf("Sequence lift: how much a DC's previous blitz call moves his blitz residual (actual minus a situation-only model)\non the next dropback. EB-shrunk, %d DCs (95%% CI shown).", nrow(lift)),
    x = "rank (1 = most hot-hand)", y = "sequence lift (percentage points)",
    caption = fig_caption(
      "SumerSports play charting 2022-2025 (blitz = 5+ pass rushers), playcallers.csv",
      sprintf("%s charted dropbacks, %d DCs with %d+ career plays.", format(sum(lift$n_seq), big.mark = ","), nrow(lift), MIN_CAREER),
      sprintf("\nResidual is out-of-sample (season-grouped) against a situation-only model, so 'lift' is the DC's own sequencing,\nnot the schedule. Dashed line is the league mean (%+.1fpp): even the median DC runs slightly hot. FTN charting\ncross-check on the season-level rate agrees within 1-2pp in 2023-2025 (its first charted season, 2022, runs\n%.1fpp apart). Built by R/16.",
              100 * grand_lift, cmp[season == 2022]$gap)
    )
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/def_seq_lift.png", p1, w = 12, h = 7)

# =============================================================================
# CHART B: situational shape, Macdonald + most/least range DCs vs league
# =============================================================================

top_range <- wide[range_rank == 1, def_caller]
bot_range <- wide[range_rank == nrow(wide), def_caller]
show_dcs <- unique(c("Mike Macdonald", top_range, bot_range))

situ_show <- situ[def_caller %in% show_dcs]
situ_show[, cut_f := factor(cut, levels = cut_order, labels = cut_labs[cut_order])]
league_line <- unique(situ[, .(cut, league)])
league_line[, cut_f := factor(cut, levels = cut_order, labels = cut_labs[cut_order])]

lab_role <- c("Mike Macdonald" = sprintf("Macdonald (range rank %d/%d)", mac_row$range_rank, nrow(wide)),
             setNames(sprintf("%s (most range)", top_range), top_range),
             setNames(sprintf("%s (least range)", bot_range), bot_range))
situ_show[, role := lab_role[def_caller]]
end_lab <- situ_show[cut_f == "Leading"]

p2 <- ggplot(situ_show, aes(cut_f, 100 * rate_shrunk, group = def_caller, colour = def_caller)) +
  geom_line(data = league_line, aes(cut_f, 100 * league, group = 1), inherit.aes = FALSE,
            linetype = "dashed", colour = ink_baseline, linewidth = 0.5) +
  geom_line(linewidth = 0.9, alpha = 0.85) +
  geom_point(size = 2.6) +
  geom_text_repel(data = end_lab, aes(label = role), size = 3.1, fontface = "bold",
                  nudge_x = 0.5, direction = "y", hjust = 0, seed = 5,
                  segment.size = 0.25, show.legend = FALSE) +
  annotate("text", x = 1, y = max(100 * league_line$league) + 1, hjust = 0, vjust = 0,
           size = 2.9, colour = "grey45", fontface = "italic", label = "dashed = league average") +
  scale_colour_manual(values = c("#D55E00", "#2B8CBE", "#009E73"), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Macdonald barely changes his blitz shape by situation; two others show what the extremes look like",
    subtitle = "EB-shrunk blitz rate by game situation, career, 2022-2025 Sumer charting",
    x = NULL, y = "blitz rate",
    caption = fig_caption(
      "SumerSports play charting 2022-2025, playcallers.csv",
      sprintf("%d DCs with a shrunk residual in all six situational cuts (30+ plays each).", nrow(wide)),
      paste0("\n'Situational range' ranks DCs on the standard deviation of the blitz RESIDUAL (net of the situation-only model) across the six cuts, so the ranking already accounts for how much of each\n",
             sprintf("cut's raw rate the situation itself explains; rates shown here are in percent for readability. Macdonald's range ranks %d of %d, near the flattest end of the league. Built by R/16.",
                     mac_row$range_rank, nrow(wide)))
    )
  ) +
  theme_coach(grid = "y") +
  theme(plot.margin = margin(10, 90, 8, 10))
save_fig("docs/figures/def_situational.png", p2, w = 12, h = 7)

# =============================================================================
# TEST 4: PERSONNEL SHIFT -- total-variation distance between the DC's own
# early-down and late-down defensive personnel mix, per DC-season-team.
# =============================================================================

pers <- d[defensive_personnel_package != "" & !is.na(defensive_personnel_package)]
pers[, down_grp := fifelse(down %in% 1:2, "early", fifelse(down %in% 3:4, "late", NA_character_))]
pers <- pers[!is.na(down_grp)]

cellN <- pers[, .N, by = .(def_caller, season, def_team, down_grp)]
grid_n <- dcast(cellN, def_caller + season + def_team ~ down_grp, value.var = "N", fill = 0)
ok_cells <- grid_n[early >= 150 & late >= 150, .(def_caller, season, def_team)]

mix <- pers[, .(n = .N), by = .(def_caller, season, def_team, down_grp, defensive_personnel_package)]
mix[, pct := n / sum(n), by = .(def_caller, season, def_team, down_grp)]
mw <- dcast(mix, def_caller + season + def_team + defensive_personnel_package ~ down_grp,
           value.var = "pct", fill = 0)
shift_tab <- mw[, .(shapeshift = 0.5 * sum(abs(early - late)), n_personnel = .N),
               by = .(def_caller, season, def_team)]
shift_tab <- merge(shift_tab, ok_cells, by = c("def_caller", "season", "def_team"))
setorder(shift_tab, -shapeshift)
shift_tab[, rank := .I]
setnames(shift_tab, "def_caller", "def_play_caller")
write_csv(as.data.frame(shift_tab), "data/derived/def_shapeshift.csv")

cat(sprintf("\n--- TEST 4: PERSONNEL SHIFT (n = %d DC-season-team cells, 150+ plays each down group, %d-%d) ---\n",
            nrow(shift_tab), min(SEASONS), max(SEASONS)))
cat(sprintf("league median: %.3f | range %.3f to %.3f\n",
            median(shift_tab$shapeshift), min(shift_tab$shapeshift), max(shift_tab$shapeshift)))
cat("shifts personnel the most (top 5):\n")
print(head(shift_tab[, .(def_play_caller, season, def_team, shapeshift = round(shapeshift, 3), rank)], 5))
cat("shifts personnel the least (bottom 5):\n")
print(tail(shift_tab[, .(def_play_caller, season, def_team, shapeshift = round(shapeshift, 3), rank)], 5))
cat("Macdonald's own cells (BAL as DC, SEA as HC/de facto DC):\n")
print(shift_tab[def_play_caller == "Mike Macdonald" | (def_team == "SEA" & season %in% 2024:2025),
                .(def_play_caller, season, def_team, shapeshift = round(shapeshift, 3), rank)][order(season)])

# =============================================================================
# CHART C: personnel shift distribution, Macdonald's cells marked
# =============================================================================

lab_c <- shift_tab[def_play_caller == "Mike Macdonald"]
lab_c[, lab := sprintf("%s %d (%s)", def_team, season, def_play_caller)]

p3 <- ggplot(shift_tab, aes(rank, shapeshift)) +
  geom_hline(yintercept = median(shift_tab$shapeshift), linetype = "dashed",
             colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", size = 1.9, alpha = 0.65) +
  geom_point(data = lab_c, colour = "#D55E00", size = 3.2) +
  geom_text_repel(data = lab_c, aes(label = lab), size = 3.1, fontface = "bold",
                  colour = "#8a3d00", seed = 4, box.padding = 0.6,
                  min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = 3, y = max(shift_tab$shapeshift), hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Reshuffles personnel most\nbetween early and late down") +
  annotate("text", x = nrow(shift_tab) - 2, y = min(shift_tab$shapeshift), hjust = 1, vjust = 0, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Same personnel group\nregardless of down") +
  labs(
    title = "Macdonald's Baltimore defense barely reshuffled personnel by down; Seattle looks average",
    subtitle = "Personnel shift index: distance between a DC's own early-down and late-down defensive personnel mix, per DC-season-team",
    x = "rank (1 = shifts personnel most)", y = "personnel shift index (0 = identical mix, 1 = fully different)",
    caption = fig_caption(
      "SumerSports play charting 2022-2025 (defensive_personnel_package), playcallers.csv",
      sprintf("%d DC-season-team cells, 150+ charted dropbacks in both the early-down\nand late-down group. Six groupings: Base, Nickel, Dime, 0-3DB, 7+DB, Other.", nrow(shift_tab)),
      paste0("\nComputed directly from each coordinator's own charted snaps (not a team aggregate), so a mid-season change in\n",
             "defensive caller does not blend two coordinators together. Index is total variation distance between the two\n",
             "personnel distributions (half the sum of absolute percentage-point gaps). Baltimore under Macdonald sits at the\n",
             "bottom of the league both years (2022 and especially 2023); Seattle under him in 2024-2025 sits close to the\n",
             "league median both years. Built by R/16."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/def_shapeshift.png", p3, w = 12, h = 7)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================= SUMMARY =================\n")
cat(sprintf("1. Sequence lift: league leans hot-hand (median %+.1fpp, grand mean %+.1fpp). Top: %s (%+.1fpp). Bottom: %s (%+.1fpp). Macdonald %+.1fpp, rank %d/%d.\n",
            100 * median(lift$lift_shrunk), 100 * grand_lift,
            lift$def_caller[1], 100 * lift$lift_shrunk[1],
            tail(lift$def_caller, 1), 100 * tail(lift$lift_shrunk, 1),
            100 * lift[def_caller == "Mike Macdonald", lift_shrunk],
            lift[def_caller == "Mike Macdonald", rank], nrow(lift)))
cat(sprintf("2. Situational range: most %s (SD %.4f), least %s (SD %.4f). Macdonald SD %.4f, rank %d/%d (flat end).\n",
            wide$def_caller[1], wide$range_sd[1],
            tail(wide$def_caller, 1), tail(wide$range_sd, 1),
            mac_row$range_sd, mac_row$range_rank, nrow(wide)))
cat(sprintf("   Persistence of the underlying blitz residual, odd vs even season: r = %+.3f, n = %d, verdict: %s\n",
            ps$r, ps$n, ps$verdict))
cat(sprintf("3. Predictability payoff, defense: r = %+.3f [%+.3f, %+.3f], p = %.3f, n = %d -- %s\n",
            ct3$estimate, ct3$conf.int[1], ct3$conf.int[2], ct3$p.value, nrow(pred_pay), sign_note))
cat(sprintf("4. Personnel shift: most %s %d %s (%.3f), least %s %d %s (%.3f). Baltimore/Macdonald 2022 rank %d/%d, 2023 rank %d/%d; Seattle/Macdonald 2024 rank %d/%d, 2025 rank %d/%d.\n",
            shift_tab$def_play_caller[1], shift_tab$season[1], shift_tab$def_team[1], shift_tab$shapeshift[1],
            tail(shift_tab$def_play_caller, 1), tail(shift_tab$season, 1), tail(shift_tab$def_team, 1), tail(shift_tab$shapeshift, 1),
            shift_tab[def_play_caller == "Mike Macdonald" & season == 2022, rank], nrow(shift_tab),
            shift_tab[def_play_caller == "Mike Macdonald" & season == 2023, rank], nrow(shift_tab),
            shift_tab[def_play_caller == "Mike Macdonald" & season == 2024, rank], nrow(shift_tab),
            shift_tab[def_play_caller == "Mike Macdonald" & season == 2025, rank], nrow(shift_tab)))
cat("=============================================\n")
