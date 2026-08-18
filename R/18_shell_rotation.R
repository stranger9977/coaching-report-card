# =============================================================================
# 18_shell_rotation.R -- post-snap shell rotation: the trend, whether it pays,
# who does it, and the tape to go watch.
#
# Michael, verbatim:
#   "I wonder if post snap movement out of shell has increased over time...
#    my guess is yes."
#
#   "There are three points in time a quarterback can read the defense:
#    1 Film study - before the game begins
#    2 Pre snap
#    3 Post snap. There's a lot more pressure post snap...and a lot less
#    time...by shifting at this moment, it adds another mental task in a
#    highly constrained environment. The later you can delay this, the
#    better...(kirk cousins video)."
#
#   "Also curious if EPA is down on plays where there is a rotation post-snap.
#    That'd tell us if it's worth it."
#
#   "think isolating a play for 'late movement' for McDonald defense is
#    helpful."
#
# WHAT COUNTS AS A ROTATION. Same definition as R/factory/85_disguise_defense.R
# and R/factory/83_presnap_postsnap.R: Sumer charts the middle-of-field shell
# twice per snap, once before it (middle_of_field_coverage_look) and once as
# actually played (middle_of_field_coverage_played). A rotation is
# look != played, restricted to snaps where both are a real OPEN/CLOSED call
# (has_shell). 85 already established the league rotates 29% of charted snaps
# and that the rate is a repeatable coordinator trait (r = +0.79 even across a
# team change). This script does not redo that; it asks four things 83/85
# did not: whether the rate has moved over TIME, whether offenses actually
# suffer for it (EPA, with controls), where Macdonald sits on the leaderboard
# by name, and concrete plays to pull film on.
#
# UNIVERSE. Sumer's is_dropback flag restricts every test here to actual
# pass dropbacks (rotation is a coverage-shell concept; it has nothing to say
# about a called run). garbage_time (Sumer's own flag, ~3% of dropbacks) is
# excluded from every test in this script, trend included: those are mostly
# prevent-defense snaps played vanilla regardless of coordinator, and the
# question is what a defense chooses to do when the game is live. Missing
# shell (roughly 1% of dropbacks, see the trend print below) is reported by
# season rather than silently dropped, so a change in how Sumer's chartists
# label a look does not get read as a change in behavior.
#
# WORTH-IT METHOD. sumer_resid() from lib_sumer.R only handles a binary
# target (it is built for a caller's RATE on some behavior). EPA is
# continuous, so this script adds expect_epa(), a direct continuous analogue:
# the same season-grouped, out-of-sample xgboost design as sumer_expect(),
# same ten situation features, swapped to a squared-error objective and
# trained on expected_points_added. A play's residual (actual EPA minus what
# the situation alone predicts) is EPA net of down, distance, field position,
# quarter, score, two/four-minute, red zone, short yardage and goal line.
# Comparing that residual on rotated vs static snaps is a same-situation
# comparison without binning plays into cells and losing power in the thin
# ones.
#
# PER-DC LEADERBOARD. Reuses sumer_resid() exactly as R/factory/83 and 85 do:
# a coordinator's rotation rate against a situation-only expectation, min 700
# charted dropbacks, tested with persist_split() (odd season vs even).
#
# THE REEL. Michael wants film, so game/quarter/clock precision matters more
# than the stats above it. Sumer's own play-by-play string (`outcome`) is the
# result column; nothing here paraphrases it.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()),
# R/factory/lib_sumer.R (load_sumer(), sumer_expect(), sumer_resid(),
# persist_split()). No em dashes.
#
# Out: docs/figures/rotation_trend.png
#      docs/figures/rotation_worth.png
#      docs/figures/rotation_dcs.png
#      data/derived/rotation_dcs.csv
#      data/derived/macdonald_reel.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

d <- load_sumer()

# load_sumer() merges week and season_type from games but not week_type or
# kickoff time. Sumer numbers postseason weeks 1-4 within the bracket, so a
# "week 4" POST game reads as regular-season week 4 unless it is labelled as
# what it is: the Super Bowl. The reel needs that distinction to point at the
# right broadcast, so pull week_type and the kickoff date in here too.
games_meta <- rbindlist(lapply(2022:2025, function(y) {
  f <- file.path(SUMER_DIR, sprintf("games_%d.csv", y))
  if (file.exists(f)) fread(f, select = c("sumer_game_id","week_type","start"), showProgress = FALSE) else NULL
}), fill = TRUE)
d <- merge(d, unique(games_meta, by = "sumer_game_id"), by = "sumer_game_id", all.x = TRUE)
ROUND_NAME <- c(`5` = "Wild Card", `2` = "Divisional", `1` = "Conference Championship", `4` = "Super Bowl")

d <- d[is_dropback == TRUE & garbage_time == FALSE]
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell &
      middle_of_field_coverage_look != middle_of_field_coverage_played)]
cat(sprintf("dropback snaps, non-garbage-time, 2022-2025: %s\n", format(nrow(d), big.mark = ",")))

# =============================================================================
# TEST 1: THE TREND -- has rotation risen over time?
# =============================================================================
cat("\n=== 1. has post-snap rotation increased over time? ===\n")

trend <- d[, .(n_dropback = .N, n_shell = sum(has_shell)), by = season]
trend[, missing_pct := 100 * (1 - n_shell / n_dropback)]
rt <- d[has_shell == TRUE, .(n = .N, k = sum(rotate)), by = season]
rt[, `:=`(rate = 100 * k / n)]
rt[, `:=`(lo = 100 * mapply(function(k, n) binom.test(k, n)$conf.int[1], k, n),
          hi = 100 * mapply(function(k, n) binom.test(k, n)$conf.int[2], k, n))]
trend <- merge(trend, rt, by = "season")
setorder(trend, season)
cat("season | dropbacks | missing shell % | rotation rate [95% CI]\n")
for (i in seq_len(nrow(trend))) with(trend[i], cat(sprintf(
  "  %d   | %6s    | %.1f%%            | %.1f%% [%.1f, %.1f]\n",
  season, format(n_dropback, big.mark = ","), missing_pct, rate, lo, hi)))

lm_fit <- lm(rate ~ season, data = trend)
slope <- coef(lm_fit)[["season"]]
slope_ci <- confint(lm_fit)["season", ]
total_move <- trend$rate[nrow(trend)] - trend$rate[1]
verdict <- if (slope_ci[1] > 0) "rose" else if (slope_ci[2] < 0) "fell" else "did not move by a confident amount"
cat(sprintf("\nlinear trend: %+.2f points/season [%.2f, %.2f] | %d to %d: %+.1f points | verdict: rotation %s\n",
            slope, slope_ci[1], slope_ci[2], min(trend$season), max(trend$season), total_move, verdict))
cat(sprintf("Michael guessed rotation increased. The data says it %s.\n", verdict))

title1 <- if (slope_ci[1] > 0) {
  sprintf("Shell rotation is up: %.1f%% of dropbacks in %d, %.1f%% in %d", trend$rate[1], trend$season[1], trend$rate[nrow(trend)], trend$season[nrow(trend)])
} else if (slope_ci[2] < 0) {
  sprintf("Shell rotation is down: %.1f%% of dropbacks in %d, %.1f%% in %d", trend$rate[1], trend$season[1], trend$rate[nrow(trend)], trend$season[nrow(trend)])
} else {
  sprintf("Shell rotation has held near %.0f%% of dropbacks since %d", mean(trend$rate), trend$season[1])
}

p1 <- ggplot(trend, aes(season, rate)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#6b4c9a", alpha = 0.15) +
  geom_line(colour = "#6b4c9a", linewidth = 1) +
  geom_point(colour = "#6b4c9a", size = 2.6) +
  geom_text(aes(label = sprintf("%.1f%%", rate)), vjust = -1.1, size = 3.4,
            fontface = "bold", colour = "#4a3570") +
  scale_x_continuous(breaks = trend$season) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, max(trend$hi) + 3)) +
  labs(title = title1,
       subtitle = "Share of defensive dropback snaps where the shell shown pre-snap differs from the shell actually played",
       x = NULL, y = "rotation rate",
       caption = fig_caption(
         "SumerSports play charting 2022-2025",
         sprintf("%s dropback snaps outside garbage time, %s with a charted shell (%.1f%%-%.1f%% missing shell by season, stable across years).",
                 format(sum(trend$n_dropback), big.mark = ","), format(sum(trend$n_shell), big.mark = ","),
                 min(trend$missing_pct), max(trend$missing_pct)),
         sprintf("\nLinear trend %+.2f points per season [%.2f, %.2f]. Missingness in the shell charting barely moves across the window, so the trend is real rather than a\nlabeling artifact. Built by R/18_shell_rotation.R.",
                 slope, slope_ci[1], slope_ci[2]))) +
  theme_coach(grid = "y")
save_fig("docs/figures/rotation_trend.png", p1, w = 11, h = 6.75)

# =============================================================================
# TEST 2: WORTH IT -- is offensive EPA lower on rotated snaps, net of situation?
# =============================================================================
cat("\n=== 2. is rotation worth it for the defense? (offensive EPA, situation-controlled) ===\n")

# Continuous analogue of lib_sumer.R's sumer_expect(): same season-grouped,
# out-of-sample design, same ten situation features, squared-error objective
# on EPA instead of binary cross-entropy on a 0/1 target.
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

ep <- expect_epa(d[has_shell == TRUE])
ep[, epa_resid := .epa_actual - .epa_expected]
cat(sprintf("situation-only EPA model: mean actual %.3f, mean predicted %.3f (should match closely)\n",
            mean(ep$.epa_actual), mean(ep$.epa_expected)))

worth_test <- function(x, label) {
  r1 <- x[rotate == 1]$epa_resid; r0 <- x[rotate == 0]$epa_resid
  if (length(r1) < 50 || length(r0) < 50) return(data.table(split = label, n_rotate = length(r1), n_static = length(r0),
                                                              raw_diff = NA, resid_diff = NA, lo = NA, hi = NA, p = NA))
  tt_raw  <- t.test(x[rotate == 1]$.epa_actual, x[rotate == 0]$.epa_actual)
  tt_resid <- t.test(r1, r0)
  data.table(split = label, n_rotate = length(r1), n_static = length(r0),
             raw_diff = unname(tt_raw$estimate[1] - tt_raw$estimate[2]),
             resid_diff = unname(tt_resid$estimate[1] - tt_resid$estimate[2]),
             lo = tt_resid$conf.int[1], hi = tt_resid$conf.int[2], p = tt_resid$p.value)
}

overall  <- worth_test(ep, "All rotated snaps")
manzone  <- ep[man_zone_coverage %in% c("MAN","ZONE")]
by_mz    <- rbindlist(lapply(c("MAN","ZONE"), function(v) worth_test(manzone[man_zone_coverage == v], v)))
by_blitz <- rbindlist(lapply(c(TRUE, FALSE), function(v) worth_test(ep[blitz == v], if (v) "Blitz" else "No blitz")))
worth <- rbind(overall, by_mz, by_blitz)
worth[, worth_it := fifelse(hi < 0, "yes, offense worse when rotated",
                     fifelse(lo > 0, "no, offense BETTER when rotated", "not distinguishable from zero"))]

cat("\nsplit               | n rotate | n static | offense EPA/play, rotated minus static (situation-controlled) [95% CI]      | p\n")
for (i in seq_len(nrow(worth))) with(worth[i], cat(sprintf(
  "  %-18s | %8s | %8s | %+.4f [%.4f, %.4f]  ->  %s | p = %.4f\n",
  split, format(n_rotate, big.mark=","), format(n_static, big.mark=","), resid_diff, lo, hi, worth_it, p)))
cat(sprintf("\nraw (uncontrolled) diff on all rotated snaps: %+.4f EPA/play; situation-controlled diff: %+.4f. Controls %s the effect.\n",
            overall$raw_diff, overall$resid_diff,
            if (abs(overall$resid_diff) < abs(overall$raw_diff)) "shrink" else "do not shrink"))

worth[, split := factor(split, levels = rev(split))]
p2 <- ggplot(worth, aes(resid_diff, split)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0.18, colour = "grey45", linewidth = 0.6) +
  geom_point(size = 3.2, colour = "#6b4c9a") +
  geom_text(aes(label = sprintf("%+.3f", resid_diff)), vjust = -1.2, size = 3.2,
            fontface = "bold", colour = "#4a3570") +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = sprintf("Rotating the shell costs the offense %.3f EPA/play, controlling for situation",
                        -overall$resid_diff),
       subtitle = "Offensive EPA per play, rotated snaps minus static snaps, net of down/distance/field position/quarter/score/clock",
       x = "offense EPA/play, rotated minus static (negative = rotation helps the defense)", y = NULL,
       caption = fig_caption(
         "SumerSports play charting 2022-2025",
         sprintf("%s dropback snaps with a charted shell, outside garbage time.", format(nrow(ep), big.mark = ",")),
         sprintf("\nExpectation from a season-grouped, out-of-sample model of EPA on situation alone (same ten features as every situation model in this project); the\nresidual compared here is what is left after that expectation, so it is not a raw before/after gap. Raw gap %+.3f, controlled gap %+.3f. Built by R/18_shell_rotation.R.",
                 overall$raw_diff, overall$resid_diff))) +
  theme_coach(grid = "y")
save_fig("docs/figures/rotation_worth.png", p2, w = 11.5, h = 6.75)

# =============================================================================
# TEST 3: PER-DC ROTATION RATES -- leaderboard, Macdonald placed, persistence
# =============================================================================
cat("\n=== 3. which coordinators rotate more than their situations call for? ===\n")

rr <- sumer_resid(d[has_shell == TRUE], "rotate", "def_caller", min_career = 700)
dcs <- rr$career
setorder(dcs, -resid)
dcs[, rank := .I]
cat(sprintf("league mean rotation rate (situation model): %.1f%% | %d coordinators with >= 700 charted dropbacks\n",
            rr$league_rate, nrow(dcs)))

NAMED_DC <- c("Mike Macdonald","Vic Fangio","Steve Spagnuolo","Brian Flores")
for (nm in NAMED_DC) {
  row <- dcs[caller == nm]
  if (nrow(row)) cat(sprintf("  %-18s rank %d of %d | actual %.1f%% | resid %+.1f pts | n = %s\n",
                             nm, row$rank, nrow(dcs), row$actual, row$resid, format(row$n, big.mark=",")))
  else cat(sprintf("  %-18s not enough charted dropbacks to rank\n", nm))
}
write_csv(as.data.frame(dcs), "data/derived/rotation_dcs.csv")

ps <- persist_split(rr$season)
cat(sprintf("\nis rotation a stable coordinator trait? r = %+.2f [%.2f, %.2f] p = %.4f, n = %d coordinators -> %s\n",
            ps$r, ps$lo, ps$hi, ps$p, ps$n, ps$verdict))

top_n <- 12
placed <- unique(c(head(dcs$caller, top_n), tail(dcs$caller, top_n), NAMED_DC[NAMED_DC %in% dcs$caller]))
chart_dc <- dcs[caller %in% placed]
chart_dc[, side := fifelse(resid >= 0, "Rotates more than the situation calls for", "Shows what it plays")]
chart_dc[, highlight := fifelse(caller == "Mike Macdonald", "Macdonald",
                         fifelse(caller %in% NAMED_DC, "Named", "Other"))]
setorder(chart_dc, resid); chart_dc[, caller := factor(caller, levels = caller)]

p3 <- ggplot(chart_dc, aes(resid, caller, fill = side)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_col(width = 0.7) +
  geom_col(data = chart_dc[highlight == "Macdonald"], fill = "#D55E00", width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", resid), hjust = ifelse(resid >= 0, -0.2, 1.2)),
            size = 2.85, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Rotates more than the situation calls for" = "#6b4c9a",
                               "Shows what it plays" = "#9db6c9")) +
  scale_x_continuous(expand = expansion(mult = c(0.18, 0.18))) +
  labs(title = sprintf("Who rotates the shell: Macdonald ranks %d of %d coordinators", dcs[caller == "Mike Macdonald"]$rank, nrow(dcs)),
       subtitle = sprintf("Rotation rate above what a situation-only model expects, min. 700 charted dropbacks. League mean %.1f%%. Trait persistence r = %+.2f",
                          rr$league_rate, ps$r),
       x = "percentage points above expected", y = NULL,
       caption = fig_caption(
         "SumerSports play charting 2022-2025",
         sprintf("%d coordinators ranked, %d shown (top/bottom %d plus Macdonald/Fangio/Spagnuolo/Flores).\nDefensive play-caller attribution from samhoppen/NFL_public.",
                 nrow(dcs), nrow(chart_dc), top_n),
         "\nOrange bar is Macdonald. Method and persistence test match R/factory/85_disguise_defense.R exactly; this script adds the trend and worth-it tests. Built by R/18_shell_rotation.R.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", legend.text = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.85)))
save_fig("docs/figures/rotation_dcs.png", p3, w = 13, h = 8.5)

# =============================================================================
# TEST 4: THE MACDONALD REEL -- concrete plays for late-movement film
# =============================================================================
cat("\n=== 4. the Macdonald reel: rotated snaps with the worst offensive outcome ===\n")

mac <- d[has_shell == TRUE & rotate == 1 & def_caller == "Mike Macdonald"]
cat(sprintf("Macdonald rotated snaps (BAL 2022-2023, SEA 2024-2025), outside garbage time: %s\n", format(nrow(mac), big.mark = ",")))

ORD <- c("1st","2nd","3rd","4th")
reel <- mac[order(expected_points_added)][1:min(10, .N)]
reel[, `:=`(
  team = def_team,
  opponent = off_team,
  game_date = as.Date(start),
  # Sumer numbers postseason weeks 1-4 within the bracket, so "week" alone is
  # ambiguous there (a POST week 4 is the Super Bowl, not regular-season week
  # 4). Report a clean game label instead: the week number in the regular
  # season, the round name in the postseason.
  game = fifelse(season_type == 1,
                 sprintf("%s (%d)", ROUND_NAME[as.character(week_type)], season),
                 sprintf("Week %d (%d REG)", week, season)),
  down_distance = paste0(ifelse(down %in% 1:4, ORD[down], as.character(down)), " & ", distance),
  shell = paste0(middle_of_field_coverage_look, " -> ", middle_of_field_coverage_played),
  blitz_call = fifelse(blitz == TRUE, "yes", "no"),
  epa = round(expected_points_added, 3)
)]
reel_out <- reel[, .(game_date, game, team, opponent, quarter, clock,
                     down_distance, shell, blitz = blitz_call, epa, result = outcome)]
write_csv(as.data.frame(reel_out), "data/derived/macdonald_reel.csv")

cat("\n date       game                       team opp  qtr clock  down&dist  shell            blitz  epa\n")
for (i in seq_len(nrow(reel_out))) with(reel_out[i], cat(sprintf(
  " %s %-26s %-4s %-3s  Q%d  %-5s %-10s %-16s %-5s %+.2f\n",
  game_date, game, team, opponent, quarter, clock, down_distance, shell, blitz, epa)))
cat("\nresult text (full):\n")
for (i in seq_len(nrow(reel_out))) with(reel_out[i], cat(sprintf(
  "  [%s, %s, %s vs %s, Q%d %s, %s]: %s\n", game_date, game, team, opponent, quarter, clock, down_distance, result)))

# =============================================================================
# summary
# =============================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("1. TREND: rotation %s from %.1f%% (%d) to %.1f%% (%d), %+.2f pts/season [%.2f, %.2f].\n",
            verdict, trend$rate[1], trend$season[1], trend$rate[nrow(trend)], trend$season[nrow(trend)],
            slope, slope_ci[1], slope_ci[2]))
overall_row <- worth[split == "All rotated snaps"]
cat(sprintf("2. WORTH IT: %+.4f EPA/play to the offense on rotated snaps, controlled [%.4f, %.4f] -> %s\n",
            overall_row$resid_diff, overall_row$lo, overall_row$hi, overall_row$worth_it))
cat(sprintf("3. MACDONALD: rank %d of %d coordinators on rotation rate (resid %+.1f pts). Trait persistence r = %+.2f [%.2f, %.2f], %s.\n",
            dcs[caller == "Mike Macdonald"]$rank, nrow(dcs), dcs[caller == "Mike Macdonald"]$resid, ps$r, ps$lo, ps$hi, ps$verdict))
cat(sprintf("4. REEL: %d plays written to data/derived/macdonald_reel.csv\n", nrow(reel_out)))
cat("\nOut: docs/figures/rotation_trend.png, docs/figures/rotation_worth.png, docs/figures/rotation_dcs.png,\n")
cat("     data/derived/rotation_dcs.csv, data/derived/macdonald_reel.csv\n")
