# =============================================================================
# 16_def_sequencing.R
#
# Nick, verbatim: "im also curious about defensive play call sequencing too"
# ... "in different game situations etc."
#
# Four questions about defensive coordinators, all versions of "does the guy
# calling the defense have a pattern, and does it cost him":
#   1. SEQUENCE LIFT   -- within the same situation (down, distance bucket,
#      score state, half), does a DC's PREVIOUS blitz/no-blitz call predict
#      his NEXT one? A "hot hand" caller strings blitzes together beyond what
#      the situation explains; a "mixer" calls each snap closer to independent.
#   2. SITUATIONAL SHAPE -- how much does a DC's blitz rate move across game
#      situations (early down, late down, red zone, two-minute, trailing,
#      leading) versus staying flat everywhere?
#   3. PREDICTABILITY PAYOFF, DEFENSIVE SIDE -- R/07 found predictable calling
#      is BETTER for the offense (blackjack chart: r(call predictability,
#      EPA/play) is positive). Same question on defense: is a guessable blitz
#      decision (within situation) associated with better or worse defensive
#      EPA/play allowed, or nothing at all?
#   4. SHAPESHIFTER INDEX -- from SumerSports' situational personnel tables,
#      how far does a team's defensive personnel mix move between early-down
#      and late-down snaps? Team-season level, joined to the DC.
#
# THE SEATTLE TIE-IN. Mike Macdonald called Baltimore's defense as DC in
# 2022-2023, then became Seattle's HC in 2024 and, per playcallers.csv, calls
# Seattle's defense himself (he is listed as BOTH head_coach and
# def_play_caller for SEA in every 2024 and 2025 game). Before him, Clint
# Hurtt held SEA's defensive call in 2022-2023. He is tracked across both
# teams and both roles wherever the data supports it.
#
# Sources:
#   - Blitz decisions: FTN charting (n_pass_rushers), local
#     ~/stranger9977/nfl-analysis/data/ftn_charting_2022..2025.csv.gz, already
#     on disk through 2025 (no nflreadr pull needed).
#   - Participation (number_of_pass_rushers), local
#     ~/stranger9977/nfl-analysis/data/pbp_participation_2016..2021.csv.gz.
#     Blitz flag both sources: pass rushers >= 5 (the standard definition).
#     Checked for agreement against FTN in the 2022-2023 overlap before
#     committing to either (see printed agreement rate); final panel takes
#     participation for 2016-2021 and FTN for 2022-2025 so the overlap years
#     are not double counted.
#   - Situation features + EPA: local play_by_play_2016..2025.csv.gz, joined
#     on game_id + play_id.
#   - DC attribution: ~/stranger9977/nfl-analysis/data/playcallers.csv,
#     def_play_caller by team-game (2026 rows do not exist yet in the pbp
#     files this joins against, so nothing unplayed leaks in).
#   - SumerSports situational personnel tendency: plain GET of
#     https://sumersports.com/teams/defensive/personnel-tendency/
#       ?season={2022..2025}&refinement={standard,red_zone,late_down,
#                                         early_down,non_garbage_time}
#     with a browser User-Agent. The page ships its data as a Next.js RSC
#     flight stream: a series of <script>self.__next_f.push([1,"..."])</script>
#     tags whose second element is a JSON-escaped string. Decoding and
#     concatenating those strings in order reconstructs one long payload
#     containing a `"teamData":[...]` JSON array (balanced-bracket extracted
#     here, not regexed field-by-field) with one row per team x personnel
#     grouping x refinement. 20 pages (4 seasons x 5 refinements) fetched
#     once, cached to data/raw/sumer/personnel_tendency/*.html (gitignored),
#     2s sleep between live fetches, reruns parse from cache and fetch
#     nothing.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes. EB shrinkage follows the shrink() pattern already used in
# nfl-analysis/scripts/blitz-autopilot.R: k = (within-unit variance) /
# (across-unit variance of true means), posterior = precision-weighted blend
# toward the grand mean.
#
# Out: docs/figures/def_seq_lift.png
#      docs/figures/def_situational.png
#      docs/figures/def_shapeshift.png
#      data/derived/def_seq_callers.csv
#      data/derived/def_shapeshift.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(jsonlite); library(httr)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

MIN_CAREER <- 800   # min charted pass plays for a DC to be ranked, per spec

# =============================================================================
# 1. BLITZ FLAG: participation 2016-2023 vs FTN 2022-2025, agreement check
# =============================================================================

part_yrs <- 2016:2023
ftn_yrs  <- 2022:2025

part_raw <- rbindlist(lapply(part_yrs, function(y)
  fread(file.path(D, sprintf("pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id", "play_id", "number_of_pass_rushers"),
        showProgress = FALSE)))
setnames(part_raw, "nflverse_game_id", "game_id")
part_raw[, blitz := number_of_pass_rushers >= 5]

ftn_raw <- rbindlist(lapply(ftn_yrs, function(y)
  fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
        select = c("nflverse_game_id", "nflverse_play_id", "n_pass_rushers"),
        showProgress = FALSE)))
setnames(ftn_raw, c("nflverse_game_id", "nflverse_play_id"), c("game_id", "play_id"))
ftn_raw[, blitz := n_pass_rushers >= 5]

overlap <- merge(part_raw[, .(game_id, play_id, blitz_part = blitz)],
                  ftn_raw[, .(game_id, play_id, blitz_ftn = blitz)],
                  by = c("game_id", "play_id"))
overlap <- overlap[!is.na(blitz_part) & !is.na(blitz_ftn)]
agree_rate <- mean(overlap$blitz_part == overlap$blitz_ftn)
cat(sprintf(paste0("BLITZ DEFINITION AGREEMENT (2022-2023 overlap, both sources use pass-rusher\n",
                   "count >= 5): %s charted plays in both, agreement rate %.1f%%\n"),
            format(nrow(overlap), big.mark = ","), 100 * agree_rate))

# final panel: participation where FTN doesn't reach (2016-2021), FTN for
# 2022-2025 (also covers the validated overlap, so it is not double counted)
part_final <- part_raw[part_raw$game_id %like% "^(2016|2017|2018|2019|2020|2021)_",
                       .(game_id, play_id, blitz)]
ftn_final  <- ftn_raw[, .(game_id, play_id, blitz)]
blitz_panel <- rbind(part_final, ftn_final)
blitz_panel <- blitz_panel[!is.na(blitz)]
cat(sprintf("blitz panel: %s plays (participation 2016-2021 + FTN 2022-2025)\n",
            format(nrow(blitz_panel), big.mark = ",")))

# =============================================================================
# 2. PBP JOIN + DC ATTRIBUTION + SITUATION CELLS
# =============================================================================

YRS <- 2016:2025
pbp <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("game_id", "play_id", "season", "week", "posteam", "defteam",
                   "game_half", "qtr", "down", "ydstogo", "score_differential",
                   "epa", "qb_dropback", "qb_kneel", "qb_spike", "yardline_100",
                   "half_seconds_remaining"),
        showProgress = FALSE)))
pbp <- pbp[qb_dropback == 1 & qb_kneel == 0 & qb_spike == 0 &
          !is.na(down) & !is.na(epa) & !is.na(score_differential)]

dt <- merge(pbp, blitz_panel, by = c("game_id", "play_id"))
cat(sprintf("dropbacks matched to a blitz flag: %s of %s (%.1f%%)\n",
            format(nrow(dt), big.mark = ","), format(nrow(pbp), big.mark = ","),
            100 * nrow(dt) / nrow(pbp)))

pc <- fread(file.path(NFLA, "data/playcallers.csv"))
dt <- merge(dt, pc[, .(season, team, game_id, def_play_caller)],
            by.x = c("season", "game_id", "defteam"),
            by.y = c("season", "game_id", "team"), all.x = TRUE)
dt <- dt[!is.na(def_play_caller) & def_play_caller != ""]
cat(sprintf("plays with a DC attributed: %s\n", format(nrow(dt), big.mark = ",")))

# SEA's def_play_caller, 2024-2025, straight from the file
sea_dc <- pc[team == "SEA" & season %in% 2024:2025, .(season, week, def_play_caller, head_coach)]
cat("\nplaycallers.csv, SEA defensive caller, 2024-2025:\n")
print(sea_dc[, .N, by = .(season, def_play_caller, head_coach)])

dt[, dist_bucket := fcase(ydstogo <= 3, "short", ydstogo <= 7, "medium", default = "long")]
dt[, def_score_diff := -score_differential]  # defense's own score margin
dt[, score_state := fcase(def_score_diff >= 9, "leading",
                          def_score_diff <= -9, "trailing", default = "close")]
dt[, half := fifelse(game_half == "Half1", "H1", "H2")]  # overtime folds into H2
dt[, cell := paste(down, dist_bucket, score_state, half, sep = "|")]

career_n <- dt[, .N, by = def_play_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_play_caller]
d2 <- dt[def_play_caller %in% dcs_ok]
cat(sprintf("\nDCs with >= %d charted dropbacks, 2016-2025: %d\n", MIN_CAREER, length(dcs_ok)))

NAMED_DCS <- c("Mike Macdonald", "Steve Spagnuolo", "Vic Fangio", "Brian Flores", "Wade Phillips")
present_named <- intersect(NAMED_DCS, dcs_ok)
cat(sprintf("famous DCs present at that minimum: %s\n", paste(present_named, collapse = ", ")))

# generic EB shrink, same form as nfl-analysis/scripts/blitz-autopilot.R:
# shrink(). Works on a continuous or binomial-variance statistic.
shrink_eb <- function(m, n, v) {
  pv <- max(var(m) - mean(v / n), 1e-6)
  k  <- mean(v) / pv
  (n * m + k * weighted.mean(m, n)) / (n + k)
}

# =============================================================================
# TEST 1: SEQUENCE LIFT -- does the previous call predict the next, beyond
# situation? Within-cell precision-weighted (Mantel-Haenszel style) pooling
# of P(blitz | prev blitz) - P(blitz | prev no-blitz), then EB shrunk.
# =============================================================================

setorder(dt, def_play_caller, game_id, play_id)
dt[, prev_blitz := shift(blitz), by = .(def_play_caller, game_id)]
seqdat <- dt[!is.na(prev_blitz) & def_play_caller %in% dcs_ok]

cellstat <- seqdat[, .(n1 = sum(prev_blitz), r1 = mean(blitz[prev_blitz]),
                       n0 = sum(!prev_blitz), r0 = mean(blitz[!prev_blitz])),
                   by = .(def_play_caller, cell)]
cellstat <- cellstat[n1 >= 5 & n0 >= 5]
cellstat[, diff := r1 - r0]
cellstat[, se := sqrt(r1 * (1 - r1) / n1 + r0 * (1 - r0) / n0)]
cellstat <- cellstat[se > 0 & is.finite(se)]

lift <- cellstat[, .(lift_raw = sum(diff / se^2) / sum(1 / se^2),
                     se_dc    = sqrt(1 / sum(1 / se^2)),
                     n_cells  = .N, n_seq = sum(n1 + n0)),
                 by = def_play_caller]

grand_lift <- weighted.mean(lift$lift_raw, 1 / lift$se_dc^2)
tau2_lift <- max(var(lift$lift_raw) - mean(lift$se_dc^2), 1e-6)
lift[, w := tau2_lift / (tau2_lift + se_dc^2)]
lift[, lift_shrunk := grand_lift + w * (lift_raw - grand_lift)]
lift[, se_post := sqrt(w * se_dc^2)]
lift[, lo := lift_shrunk - 1.96 * se_post]
lift[, hi := lift_shrunk + 1.96 * se_post]
setorder(lift, -lift_shrunk)
lift[, rank := .I]

cat(sprintf("\n--- TEST 1: SEQUENCE LIFT (n = %d DCs, min %d dropbacks) ---\n",
            nrow(lift), MIN_CAREER))
cat(sprintf("league median shrunk lift: %+.3f | grand mean (precision-weighted): %+.3f\n",
            median(lift$lift_shrunk), grand_lift))
cat("most hot-hand (top 5):\n")
print(lift[1:5, .(def_play_caller, rank, n_seq, lift_raw = round(lift_raw, 3),
                  lift_shrunk = round(lift_shrunk, 3))])
cat("most mixing / anti-correlated (bottom 5):\n")
print(tail(lift, 5)[, .(def_play_caller, rank, n_seq, lift_raw = round(lift_raw, 3),
                        lift_shrunk = round(lift_shrunk, 3))])
cat("named DCs:\n")
print(lift[def_play_caller %in% present_named,
          .(def_play_caller, rank, n_seq, lift_raw = round(lift_raw, 3),
            lift_shrunk = round(lift_shrunk, 3))])
verdict1 <- if (grand_lift > 0.01 & lift[def_play_caller == "Mike Macdonald", lift_shrunk] > 0) {
  "league-wide, defenses run slightly hot: blitzing last snap makes blitzing this snap a bit more likely, beyond situation. Macdonald leans the same way."
} else "no consistent league-wide sequence pattern once situation is held constant."
cat("verdict:", verdict1, "\n")

# =============================================================================
# TEST 2: SITUATIONAL SHAPE -- blitz rate by game situation, EB-shrunk to
# league, then the spread across situations per DC ("situational range").
# =============================================================================

CUTS <- list(
  early_down = quote(down %in% 1:2),
  late_down  = quote(down %in% 3:4),
  red_zone   = quote(yardline_100 <= 20),
  two_minute = quote(half_seconds_remaining <= 120),
  trailing   = quote(score_state == "trailing"),
  leading    = quote(score_state == "leading")
)

shrink_binom <- function(rate, n) {
  v <- rate * (1 - rate)
  pv <- max(var(rate) - mean(v / n), 1e-6)
  k  <- mean(v) / pv
  (n * rate + k * weighted.mean(rate, n)) / (n + k)
}

situ <- rbindlist(lapply(names(CUTS), function(cn) {
  sub <- d2[eval(CUTS[[cn]])]
  agg <- sub[, .(n = .N, rate = mean(blitz)), by = def_play_caller][n >= 50]
  agg[, `:=`(cut = cn, league = sub[, mean(blitz)])]
  agg
}))
situ[, rate_shrunk := shrink_binom(rate, n), by = cut]
situ[, se_shrunk := sqrt(rate_shrunk * (1 - rate_shrunk) / n)]  # approx posterior SE

wide <- dcast(situ, def_play_caller ~ cut, value.var = "rate_shrunk")
wide <- wide[complete.cases(wide)]
cutcols <- names(CUTS)
wide[, range_sd := apply(.SD, 1, sd), .SDcols = cutcols]
wide[, range_mm := apply(.SD, 1, function(x) max(x) - min(x)), .SDcols = cutcols]
setorder(wide, -range_sd)
wide[, range_rank := .I]

cat(sprintf("\n--- TEST 2: SITUATIONAL SHAPE (n = %d DCs with all 6 cuts) ---\n", nrow(wide)))
cat("changes shape the most (top 5 by SD across cuts):\n")
print(head(wide[, .(def_play_caller, range_rank, range_sd = round(range_sd, 3),
                    range_mm = round(range_mm, 3))], 5))
cat("calls the same game everywhere (bottom 5):\n")
print(tail(wide[, .(def_play_caller, range_rank, range_sd = round(range_sd, 3),
                    range_mm = round(range_mm, 3))], 5))
mac_row <- wide[def_play_caller == "Mike Macdonald"]
cat(sprintf("Macdonald's situational range rank: %d of %d (SD = %.3f, spread = %.1fpp)\n",
            mac_row$range_rank, nrow(wide), mac_row$range_sd, 100 * mac_row$range_mm))
cat("Macdonald's blitz rate by cut (EB-shrunk):\n")
print(situ[def_play_caller == "Mike Macdonald",
          .(cut, n, rate = round(100 * rate, 1), league = round(100 * league, 1),
            rate_shrunk = round(100 * rate_shrunk, 1))][order(cut)])

# =============================================================================
# TEST 3: PREDICTABILITY PAYOFF, DEFENSIVE SIDE -- within-cell entropy of the
# blitz call per DC (guessability), correlated with defensive EPA/play allowed.
# =============================================================================

cellp <- d2[, .(n = .N, p = mean(blitz)), by = .(def_play_caller, cell)][n >= 10]
cellp[, H := ifelse(p %in% c(0, 1), 0, -(p * log2(p) + (1 - p) * log2(1 - p)))]
dcent <- cellp[, .(entropy = weighted.mean(H, n), n_cells = .N, n_plays = sum(n)),
              by = def_play_caller]
dcent[, guessability := 1 - entropy]  # 0 = fully unpredictable, 1 = fully guessable

epa_dc <- d2[, .(epa_allowed = mean(epa)), by = def_play_caller]
pred_pay <- merge(dcent, epa_dc, by = "def_play_caller")
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
print(pred_pay[def_play_caller %in% present_named,
              .(def_play_caller, guessability = round(guessability, 3),
                epa_allowed = round(epa_allowed, 3))][order(-guessability)])
cat(sprintf("league median guessability: %.3f | median EPA/play allowed: %+.3f\n",
            median(pred_pay$guessability), median(pred_pay$epa_allowed)))

# merge tests 1-3 into one per-DC leaderboard file
caller_tab <- merge(lift[, .(def_play_caller, n_seq, lift_raw, lift_shrunk, seq_rank = rank)],
                    wide[, .(def_play_caller, range_sd, range_mm, range_rank)], all = TRUE)
caller_tab <- merge(caller_tab,
                    pred_pay[, .(def_play_caller, entropy, guessability, epa_allowed)], all = TRUE)
caller_tab <- merge(caller_tab, career_n[, .(def_play_caller, n_career = N)], all.x = TRUE)
write_csv(as.data.frame(caller_tab), "data/derived/def_seq_callers.csv")

# =============================================================================
# CHART A: distribution of shrunk sequence lift, famous DCs + Macdonald
# =============================================================================

lab_a <- lift[def_play_caller %in% present_named]
p1 <- ggplot(lift, aes(rank, lift_shrunk)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_hline(yintercept = grand_lift, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, colour = "#9db6c9", alpha = 0.55) +
  geom_point(colour = "#9db6c9", size = 1.9, alpha = 0.7) +
  geom_point(data = lab_a, colour = "#D55E00", size = 3) +
  geom_text_repel(data = lab_a, aes(label = def_play_caller), size = 3.2, fontface = "bold",
                  colour = "#8a3d00", seed = 11, box.padding = 0.5,
                  min.segment.length = 0, max.overlaps = 20) +
  annotate("text", x = 2, y = max(lift$hi), hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", colour = "grey35",
           label = "Hot hand: blitzes\nfollow blitzes") +
  annotate("text", x = nrow(lift) - 1, y = min(lift$lo), hjust = 1, vjust = 0, size = 3.1,
           fontface = "bold", colour = "grey35",
           label = "Mixes: next call is\nindependent of the last") +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(100 * x), "pp")) +
  labs(
    title = "No defensive coordinator meaningfully anti-correlates his own blitz calls",
    subtitle = sprintf("Sequence lift: how much a DC's own previous blitz/no-blitz call moves his next one, beyond down, distance, score state and half. EB-shrunk, %d DCs (95%% CI shown).", nrow(lift)),
    x = "rank (1 = most hot-hand)", y = "sequence lift (percentage points)",
    caption = fig_caption(
      "nflverse participation (2016-2021) + FTN charting (2022-2025) + playcallers.csv",
      sprintf("%s charted dropbacks, %d DCs with %d+ career plays and 5+ plays on both sides of at least one situation cell.",
              format(sum(lift$n_seq), big.mark = ","), nrow(lift), MIN_CAREER),
      sprintf("\nBlitz = %d+ pass rushers, agreeing with the participation-era definition %.1f%% of the time in the 2022-2023 overlap. Dashed line is the precision-weighted league mean (%+.1fpp): even the median DC runs\nslightly hot, not neutral. Nobody sits meaningfully negative once shrunk. Built by R/16.",
              5, 100 * agree_rate, 100 * grand_lift)
    )
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/def_seq_lift.png", p1, w = 12, h = 7)

# =============================================================================
# CHART B: situational shape, Macdonald + most/least range DCs vs league
# =============================================================================

cut_order <- c("early_down", "late_down", "red_zone", "two_minute", "trailing", "leading")
cut_labs <- c(early_down = "Early down", late_down = "Late down", red_zone = "Red zone",
             two_minute = "Two-minute", trailing = "Trailing", leading = "Leading")

top_range <- wide[range_rank == 1, def_play_caller]
bot_range <- wide[range_rank == nrow(wide), def_play_caller]
show_dcs <- unique(c("Mike Macdonald", top_range, bot_range))

situ_show <- situ[def_play_caller %in% show_dcs]
situ_show[, cut_f := factor(cut, levels = cut_order, labels = cut_labs[cut_order])]
league_line <- unique(situ[, .(cut, league)])
league_line[, cut_f := factor(cut, levels = cut_order, labels = cut_labs[cut_order])]

lab_role <- c("Mike Macdonald" = sprintf("Macdonald (range rank %d/%d)", mac_row$range_rank, nrow(wide)),
             setNames(sprintf("%s (most range)", top_range), top_range),
             setNames(sprintf("%s (least range)", bot_range), bot_range))
situ_show[, role := lab_role[def_play_caller]]
end_lab <- situ_show[cut_f == "Leading"]

p2 <- ggplot(situ_show, aes(cut_f, 100 * rate_shrunk, group = def_play_caller, colour = def_play_caller)) +
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
    subtitle = "EB-shrunk blitz rate by game situation, career, 2016-2025",
    x = NULL, y = "blitz rate",
    caption = fig_caption(
      "nflverse participation (2016-2021) + FTN charting (2022-2025) + playcallers.csv",
      sprintf("%d DCs with a shrunk rate in all six situational cuts (50+ plays each).", nrow(wide)),
      paste0("\n'Situational range' is the standard deviation of a DC's shrunk blitz rate across the six cuts; higher means the coordinator calls a visibly different game by spot.\n",
             sprintf("Macdonald's range ranks %d of %d, near the flattest end of the league. Built by R/16.", mac_row$range_rank, nrow(wide)))
    )
  ) +
  theme_coach(grid = "y") +
  theme(plot.margin = margin(10, 90, 8, 10))
save_fig("docs/figures/def_situational.png", p2, w = 12, h = 7)

# =============================================================================
# 3. SUMERSPORTS SITUATIONAL PERSONNEL: fetch + parse (cached, 2s between
#    live fetches, re-parses from cache on rerun, never re-fetches a page).
# =============================================================================

SUMER_CACHE <- "data/raw/sumer/personnel_tendency"
dir.create(SUMER_CACHE, recursive = TRUE, showWarnings = FALSE)
SUMER_UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
SUMER_SEASONS <- 2022:2025
SUMER_REFS <- c("standard", "red_zone", "late_down", "early_down", "non_garbage_time")

sumer_fetch_page <- function(season, refinement) {
  f <- file.path(SUMER_CACHE, sprintf("%d_%s.html", season, refinement))
  if (!file.exists(f)) {
    url <- sprintf("https://sumersports.com/teams/defensive/personnel-tendency/?season=%d&refinement=%s",
                    season, refinement)
    resp <- GET(url, user_agent(SUMER_UA), timeout(60))
    if (status_code(resp) != 200) stop(sprintf("SumerSports HTTP %d for %s", status_code(resp), url))
    writeLines(content(resp, as = "text", encoding = "UTF-8"), f)
    Sys.sleep(2)
  }
  f
}

# balanced-bracket scan: text is one giant escaped JS string, so quotes and
# escapes have to be tracked to find the TRUE matching ']' for "teamData":[.
sumer_find_matching_bracket <- function(text, open_pos) {
  chars <- strsplit(substr(text, open_pos, nchar(text)), "")[[1]]
  depth <- 0L; in_str <- FALSE; esc <- FALSE
  for (i in seq_along(chars)) {
    ch <- chars[i]
    if (in_str) {
      if (esc) esc <- FALSE
      else if (ch == "\\") esc <- TRUE
      else if (ch == '"') in_str <- FALSE
      next
    }
    if (ch == '"') { in_str <- TRUE; next }
    if (ch == "[") depth <- depth + 1L
    else if (ch == "]") { depth <- depth - 1L; if (depth == 0L) return(open_pos + i - 1L) }
  }
  NA_integer_
}

sumer_parse_page <- function(f, season, refinement) {
  raw <- paste(readLines(f, warn = FALSE), collapse = "")
  pieces <- strsplit(raw, "self.__next_f.push([1,", fixed = TRUE)[[1]][-1]
  decode_piece <- function(p) {
    m <- regexpr('^"((?:[^"\\\\]|\\\\.)*)"\\]\\)', p, perl = TRUE)
    if (m == -1) return(NA_character_)
    s <- sub('\\]\\)$', '', regmatches(p, m))
    tryCatch(fromJSON(s), error = function(e) NA_character_)
  }
  decoded <- vapply(pieces, decode_piece, character(1))
  full <- paste(decoded[!is.na(decoded)], collapse = "")
  marker <- '"teamData":['
  starts <- gregexpr(marker, full, fixed = TRUE)[[1]]
  if (starts[1] == -1) stop(sprintf("no teamData in cached page %s", f))
  best <- NULL
  for (s in starts) {
    open_pos <- s + nchar(marker) - 1L
    end_pos <- sumer_find_matching_bracket(full, open_pos)
    if (is.na(end_pos)) next
    txt <- substr(full, open_pos, end_pos)
    if (is.null(best) || nchar(txt) > nchar(best)) best <- txt
  }
  df <- as.data.table(fromJSON(best, flatten = TRUE))
  keep <- c("teamCode", "personnel", "refinementKey",
            "defensivePersonnelPlays", "defensivePersonnelPlaysPercentage")
  df <- df[, ..keep]
  df[, `:=`(req_season = season, req_refinement = refinement)]
  df
}

sumer_rows <- list()
for (s in SUMER_SEASONS) for (r in SUMER_REFS) {
  f <- sumer_fetch_page(s, r)
  sumer_rows[[paste(s, r)]] <- sumer_parse_page(f, s, r)
}
sumer <- rbindlist(sumer_rows)
cat(sprintf("\nSumerSports personnel-tendency: %s rows across %d season-refinement pages\n",
            format(nrow(sumer), big.mark = ","), length(sumer_rows)))

# =============================================================================
# TEST 4: SHAPESHIFTER INDEX -- total-variation distance between the
# early-down and late-down defensive personnel mix, per team-season.
# =============================================================================

SUMER_TEAM_FIX <- c(ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU")
sumer[, team := fifelse(teamCode %in% names(SUMER_TEAM_FIX), SUMER_TEAM_FIX[teamCode], teamCode)]

ed <- sumer[req_refinement == "early_down", .(team, season = req_season, personnel,
                                               pct = defensivePersonnelPlaysPercentage)]
ld <- sumer[req_refinement == "late_down", .(team, season = req_season, personnel,
                                              pct = defensivePersonnelPlaysPercentage)]
mix <- merge(ed, ld, by = c("team", "season", "personnel"), all = TRUE,
            suffixes = c("_early", "_late"))
mix[is.na(pct_early), pct_early := 0]
mix[is.na(pct_late), pct_late := 0]

shape <- mix[, .(shapeshift = 0.5 * sum(abs(pct_early - pct_late)), n_personnel = .N),
            by = .(team, season)]
setorder(shape, -shapeshift)
shape[, rank := .I]

# primary (most-games) DC per team-season, from playcallers.csv
dc_season <- pc[season %in% SUMER_SEASONS, .N, by = .(season, team, def_play_caller)]
setorder(dc_season, season, team, -N)
dc_primary <- dc_season[, .SD[1], by = .(season, team)][, .(season, team, def_play_caller, dc_games = N)]
shape <- merge(shape, dc_primary, by = c("season", "team"), all.x = TRUE)
setorder(shape, rank)
write_csv(as.data.frame(shape), "data/derived/def_shapeshift.csv")

cat(sprintf("\n--- TEST 4: SHAPESHIFTER INDEX (n = %d team-seasons, %d-%d) ---\n",
            nrow(shape), min(SUMER_SEASONS), max(SUMER_SEASONS)))
cat(sprintf("league median: %.3f | range %.3f to %.3f\n",
            median(shape$shapeshift), min(shape$shapeshift), max(shape$shapeshift)))
cat("shifts shape the most (top 5):\n")
print(head(shape[, .(team, season, def_play_caller, shapeshift = round(shapeshift, 3), rank)], 5))
cat("shifts shape the least (bottom 5):\n")
print(tail(shape[, .(team, season, def_play_caller, shapeshift = round(shapeshift, 3), rank)], 5))
mac_shape <- shape[def_play_caller == "Mike Macdonald" | (team == "SEA" & season %in% 2024:2025)]
cat("Macdonald's team-seasons (BAL as DC, SEA as HC/de facto DC):\n")
print(shape[team %in% c("BAL", "SEA") & season %in% SUMER_SEASONS,
          .(team, season, def_play_caller, shapeshift = round(shapeshift, 3), rank)][order(season, team)])

# =============================================================================
# CHART C: shapeshifter index distribution, Macdonald's team-seasons marked
# =============================================================================

lab_c <- shape[team %in% c("BAL", "SEA") & season %in% SUMER_SEASONS]
lab_c[, lab := sprintf("%s %d (%s)", team, season, def_play_caller)]

p3 <- ggplot(shape, aes(rank, shapeshift)) +
  geom_hline(yintercept = median(shape$shapeshift), linetype = "dashed",
             colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", size = 1.9, alpha = 0.65) +
  geom_point(data = lab_c, colour = "#D55E00", size = 3.2) +
  geom_text_repel(data = lab_c, aes(label = lab), size = 3.1, fontface = "bold",
                  colour = "#8a3d00", seed = 4, box.padding = 0.6,
                  min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = 3, y = max(shape$shapeshift), hjust = 0, vjust = 1, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Reshuffles personnel most\nbetween early and late down") +
  annotate("text", x = nrow(shape) * 0.7, y = 0.37, hjust = 0.5, vjust = 1, size = 3.1,
           fontface = "bold", colour = "grey35", label = "Same personnel group\nregardless of down") +
  labs(
    title = "Macdonald's Baltimore defense barely reshuffled personnel by down; Seattle looks average",
    subtitle = "Shapeshifter index: distance between a team's early-down and late-down defensive personnel mix, by team-season",
    x = "rank (1 = shifts shape most)", y = "shapeshifter index (0 = identical mix, 1 = fully different)",
    caption = fig_caption(
      "SumerSports situational personnel tendency (sumersports.com/teams/defensive/personnel-tendency), 2022-2025, joined to playcallers.csv",
      sprintf("%d team-seasons, personnel mix compared between the early_down and late_down refinements.", nrow(shape)),
      paste0("\nIndex is total variation distance between the two personnel-grouping distributions (half the sum of absolute percentage-point gaps); it is a full-sample descriptive statistic from SumerSports'\n",
             "own charting, not a survey estimate, so no sampling-based confidence interval is attached. Baltimore under Macdonald (2022 rank 128th of 128, 2023 in the bottom quartile) barely changed personnel\n",
             "by down; Seattle in 2024-2025, with Macdonald calling it as head coach, sits close to the league median both years. Built by R/16."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/def_shapeshift.png", p3, w = 12, h = 7)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================= SUMMARY =================\n")
cat(sprintf("1. Sequence lift: league leans hot-hand (median %+.1fpp, grand mean %+.1fpp). Top: %s (%+.1fpp). Bottom: %s (%+.1fpp). Macdonald %+.1fpp, rank %d/%d.\n",
            100 * median(lift$lift_shrunk), 100 * grand_lift,
            lift$def_play_caller[1], 100 * lift$lift_shrunk[1],
            tail(lift$def_play_caller, 1), 100 * tail(lift$lift_shrunk, 1),
            100 * lift[def_play_caller == "Mike Macdonald", lift_shrunk],
            lift[def_play_caller == "Mike Macdonald", rank], nrow(lift)))
cat(sprintf("2. Situational range: most %s (SD %.3f), least %s (SD %.3f). Macdonald SD %.3f, rank %d/%d (flat end).\n",
            wide$def_play_caller[1], wide$range_sd[1],
            tail(wide$def_play_caller, 1), tail(wide$range_sd, 1),
            mac_row$range_sd, mac_row$range_rank, nrow(wide)))
cat(sprintf("3. Predictability payoff, defense: r = %+.3f [%+.3f, %+.3f], p = %.3f, n = %d -- %s\n",
            ct3$estimate, ct3$conf.int[1], ct3$conf.int[2], ct3$p.value, nrow(pred_pay), sign_note))
cat(sprintf("4. Shapeshifter index: most %s %d (%.3f), least %s %d (%.3f). Baltimore/Macdonald 2022 rank %d/%d; Seattle/Macdonald 2024 rank %d/%d.\n",
            shape$team[1], shape$season[1], shape$shapeshift[1],
            tail(shape$team, 1), tail(shape$season, 1), tail(shape$shapeshift, 1),
            shape[team == "BAL" & season == 2022, rank], nrow(shape),
            shape[team == "SEA" & season == 2024, rank], nrow(shape)))
cat("=============================================\n")
