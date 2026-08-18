# =============================================================================
# 19_brave_cowardly.R -- the cowardly coach leaderboard, and the brave one.
#
# Nick, verbatim: "Probably need a cowardly coach leaderboard. And a brave
# coach leaderboard." Michael's reply: "Yes! Will tell my animator to do
# something special for Josh McDaniel" (McDaniels -- he is checked and
# labeled on both boards below if he qualifies, and his numbers are reported
# regardless of whether he does).
#
# This is a rendering job on top of three factory scripts, not a new model.
#
#   COWARDLY = kicked or punted on a fourth down where the nfl4th decision
#   model says going is CLEARLY better. "Clearly better" reuses the exact
#   threshold factory/50_leverage.R already uses to call a verdict knowable:
#   go_boost >= 1.5 win-probability points is "should go," and anything
#   between -1.5 and 1.5 is "close" and thrown out rather than guessed at.
#
#   BRAVE = went for it in those same clear-cut spots. Since the qualifying
#   population is restricted to (went | kicked) == 1, brave and cowardly are
#   two views of the same binary outcome: kicked = 1 - went. A coach's go
#   rate in the clear-cut zone is his BRAVE number; 100 minus it is his
#   COWARDLY number.
#
#   Second signal, per Michael's animator ask and factory/86's actual
#   finding: the go rate in MARGINAL spots (86's "0 to 1, razor thin" band --
#   a real but thin case for going, 0 to 1 win-probability points). 86 found
#   the league-wide go rate collapses exactly there. This script re-cuts that
#   same band per coach, because that is where nerve actually shows, not in
#   the obvious 8-point-edge spots everyone gets right.
#
# ERA ADJUSTMENT AND SHRINKAGE reuse factory/95_decision_value.R's method
# exactly: rate compared to the league in the coach's own seasons (the league
# has gotten more aggressive over time, so a raw pooled rank mostly rewards
# whoever coached recently), then empirical-Bayes shrunk toward zero because
# a raw per-coach rate on a few dozen decisions is mostly sampling noise.
# Reliability is checked the way factory/50 checked hi_obey: a random within-
# coach split-half correlation, not assumed.
#
# THE CATCH, restated from factory/95: the underlying decision-cost ledger
# this leans on has year-over-year persistence of only r = 0.21 to 0.26,
# below this project's own r >= 0.30 bar for calling something a trait
# (GOAL.md, G2). This script's own go/kick rate is checked separately below
# and is not assumed to clear that bar just because it is a different cut of
# related data.
#
# DATA. data/derived/fourth_down_probs.rds (nfl4th win-probability model on
# nflverse play-by-play, 2018-2025) filtered exactly as factory/86 and
# factory/95 filter it: regulation, at least a minute left in the half, win
# probability between 5% and 95%, no kneels. Coach attached via
# nfl-analysis/data/games.csv home_coach/away_coach, same join as 50 and 95.
# Decision cost per game is pulled from factory/95's own output
# (data/factory/decision_value.csv) rather than recomputed, for the CSV's
# context column only; it does not drive either leaderboard's ranking.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# TWO FILES, not one combined figure: each leaderboard needs its coach names
# down the y-axis at readable size, and squeezing both into side-by-side
# panels (as factory/86 does for a 2-band comparison) crushed the labels once
# 12 coach names per side were in play. Stacking them in one tall file would
# have solved that but made neither board screenshot on its own, which the
# ask explicitly required.
#
# Out: docs/figures/cowardly.png
#      docs/figures/brave.png
#      data/derived/brave_cowardly.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
MIN_DECISIONS <- 20   # clear-cut fourth downs required to make either board
MIN_MARGINAL  <- 10   # marginal-band decisions required to print a rate

# ---------------------------------------------------------------- load + filter
fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd <- fd[qtr <= 4 & half_seconds_remaining >= 60 &
         !is.na(go_boost) & !is.na(vegas_wp) &
         vegas_wp > 0.05 & vegas_wp < 0.95 &
         play_type != "" & (is.na(qb_kneel) | qb_kneel != 1)]
fd[, went   := as.integer(play_type %in% c("run","pass") | rush_attempt == 1 | pass_attempt == 1)]
fd[, kicked := as.integer(play_type %in% c("punt","field_goal") | punt_attempt == 1 | field_goal_attempt == 1)]
fd <- fd[went == 1 | kicked == 1]

CUT <- 1.5  # same clear-cut threshold as factory/50_leverage.R
fd[, verdict := fifelse(go_boost >= CUT, "should go",
                fifelse(go_boost <= -CUT, "should kick", "close"))]
clear <- fd[verdict == "should go"]
marg  <- fd[go_boost > 0 & go_boost <= 1]   # factory/86's "0 to 1 razor thin" band

gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
attach_coach <- function(dt) {
  dt <- merge(dt, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
  dt[, coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
  dt[!is.na(coach) & coach != ""]
}
clear <- attach_coach(clear)
marg  <- attach_coach(marg)

cat(sprintf("clear-cut (go_boost >= %.1f) fourth downs: %s across %d coaches, %s to %s\n",
            CUT, format(nrow(clear), big.mark = ","), uniqueN(clear$coach),
            min(clear$season), max(clear$season)))
cat(sprintf("marginal-band (0 to 1) fourth downs: %s across %d coaches\n",
            format(nrow(marg), big.mark = ","), uniqueN(marg$coach)))

# ---------------------------------------------------------------- era-adjust + shrink
# Exactly factory/95's machinery: rate vs the league in the coach's own
# seasons, then empirical-Bayes shrinkage toward zero.
cs <- clear[, .(n = .N, went = sum(went)), by = .(coach, season)]
lg <- clear[, .(lg_n = .N, lg_went = sum(went)), by = season][, lg_rate := 100*lg_went/lg_n]
cat("\n--- league go rate in clear-cut spots, by season (the era effect) ---\n")
print(lg[order(season), .(season, lg_rate = round(lg_rate, 1))])
cs <- merge(cs, lg[, .(season, lg_rate)], by = "season")

dc <- cs[, .(n = sum(n), went = sum(went), expected_went = sum(lg_rate/100 * n)),
         by = coach]
spans <- clear[, .(first_season = min(season), last_season = max(season)), by = coach]
dc <- merge(dc, spans, by = "coach")
dc[, `:=`(go_rate = 100*went/n, expected_rate = 100*expected_went/n)]
dc[, kick_rate := 100 - go_rate]
dc[, era_adj := go_rate - expected_rate]     # + braver than his era, - more cowardly
dc[, se := 100*sqrt((go_rate/100)*(1 - go_rate/100)/n)]
dc[, `:=`(lo = era_adj - 1.96*se, hi = era_adj + 1.96*se)]

dc <- dc[n >= MIN_DECISIONS]
tau2 <- max(var(dc$era_adj) - mean(dc$se^2), 1e-4)
dc[, era_shrunk := (tau2*era_adj)/(tau2 + se^2)]
setorder(dc, era_shrunk)
n_separable <- dc[lo > 0 | hi < 0, .N]
cat(sprintf("\n%d coaches with >= %d clear-cut decisions. shrinkage: tau = %.2fpp, mean se = %.2fpp\n",
            nrow(dc), MIN_DECISIONS, sqrt(tau2), mean(dc$se)))
cat(sprintf("%d of %d (%.0f%%) are separable from their own era's average at 95%% confidence.\n",
            n_separable, nrow(dc), 100*n_separable/nrow(dc)))

# ---------------------------------------------------------------- marginal-band rate
mg <- marg[, .(marg_n = .N, marg_went = sum(went)), by = coach]
mg[, marg_go_rate := 100*marg_went/marg_n]
dc <- merge(dc, mg, by = "coach", all.x = TRUE)

# ---------------------------------------------------------------- reliability check
# factory/50's method: is there even between-coach signal here, before anyone
# reads a rank as a trait. Random within-coach half split, not season split,
# because most tenures here do not span enough seasons for that.
set.seed(11)
hh <- clear[coach %in% dc$coach][, half := sample(rep_len(1:2, .N)), by = coach]
sh <- dcast(hh[, .(v = 100*mean(went), n = .N), by = .(coach, half)][n >= 8],
            coach ~ half, value.var = "v")
setnames(sh, c("coach","h1","h2"))
sh <- sh[!is.na(h1) & !is.na(h2)]
r_half <- cor(sh$h1, sh$h2)
r_sb <- 2*r_half/(1 + r_half)
cat(sprintf("\nclear-cut go-rate reliability: split-half r = %.2f, Spearman-Brown %.2f (n = %d coaches)\n",
            r_half, r_sb, nrow(sh)))
cat(sprintf("rubric's own persistence bar is 0.30, so this leaderboard is %s\n",
            if (r_sb >= 0.30) "a trait by that bar" else
              "a description of behavior in this window, not yet a proven trait -- same caveat factory/95 attaches to its own ledger"))
cat("restating factory/95's own catch: the fourth-down decision-cost ledger this leans on persists year over year at r = 0.21 to 0.26, below the project's r >= 0.30 trait bar.\n")

# ---------------------------------------------------------------- decision cost context
dv <- as.data.table(read_csv("data/factory/decision_value.csv", show_col_types = FALSE))
dc <- merge(dc, dv[, .(coach, decision_cost_per_game = cost_per_game,
                       dv_lo = lo, dv_hi = hi)], by = "coach", all.x = TRUE)
# data.table's merge() resorts by the join key, so re-impose the ranking
# order every downstream print and write depends on.
setorder(dc, era_shrunk)

write_csv(as.data.frame(dc[, .(coach, first_season, last_season, decisions = n,
                               go_rate = round(go_rate,1), kick_rate = round(kick_rate,1),
                               era_adj = round(era_adj,2), se = round(se,2),
                               lo = round(lo,2), hi = round(hi,2),
                               era_shrunk = round(era_shrunk,2),
                               marg_n, marg_go_rate = round(marg_go_rate,1),
                               decision_cost_per_game = round(decision_cost_per_game,3),
                               separable = lo > 0 | hi < 0)]),
          "data/derived/brave_cowardly.csv")

# ---------------------------------------------------------------- McDaniels, explicit
dc[, rank_cowardly := rank(era_shrunk)]
dc[, rank_brave := rank(-era_shrunk)]
mcd <- dc[coach == "Josh McDaniels"]
cat("\n=== Josh McDaniels, explicit ===\n")
if (nrow(mcd) == 0) {
  raw <- clear[coach == "Josh McDaniels"]
  rawm <- marg[coach == "Josh McDaniels"]
  cat(sprintf("does not qualify for either board (needs %d+ clear-cut decisions).\n", MIN_DECISIONS))
  cat(sprintf("raw numbers anyway: %d clear-cut decisions, went %d times (%.1f%% go rate).\n",
              nrow(raw), sum(raw$went), 100*mean(raw$went)))
  if (nrow(rawm) > 0) cat(sprintf("marginal-band: %d decisions, went %d times (%.1f%%).\n",
                                  nrow(rawm), sum(rawm$went), 100*mean(rawm$went)))
} else {
  cat(sprintf("clear-cut spots: %d decisions (%d-%d), go rate %.1f%%, kick rate %.1f%%\n",
              mcd$n, mcd$first_season, mcd$last_season, mcd$go_rate, mcd$kick_rate))
  cat(sprintf("era_adj %+.1fpp [%.1f, %.1f], shrunk %+.1fpp\n",
              mcd$era_adj, mcd$lo, mcd$hi, mcd$era_shrunk))
  cat(sprintf("rank: #%d of %d most cowardly, #%d of %d most brave\n",
              mcd$rank_cowardly, nrow(dc), mcd$rank_brave, nrow(dc)))
  if (!is.na(mcd$marg_n)) cat(sprintf("marginal-band (0-1pp case for going): %d decisions, went %.1f%% of the time\n",
                                       mcd$marg_n, mcd$marg_go_rate))
}

cat("\n=== most cowardly (top 10, most below their own era's go rate) ===\n")
print(head(dc[order(era_shrunk), .(coach, n, go_rate, kick_rate, era_adj = round(era_adj,1),
                  ci = sprintf("[%.1f, %.1f]", lo, hi), era_shrunk = round(era_shrunk,1))], 10))
cat("\n=== most brave (top 10, most above their own era's go rate) ===\n")
print(head(dc[order(-era_shrunk), .(coach, n, go_rate, kick_rate, era_adj = round(era_adj,1),
                                    ci = sprintf("[%.1f, %.1f]", lo, hi), era_shrunk = round(era_shrunk,1))], 10))

# ---------------------------------------------------------------- charts
mk_board <- function(dat, n_show, direction, title, subtitle_extra, path, fill_pos, fill_neg) {
  top <- head(dat, n_show)
  top[, lab := sprintf("%s  (%d dec, %d-%d)", coach, n, first_season, last_season)]
  top[, side := fifelse(era_adj >= 0, "Braver than his era", "More cowardly than his era")]
  setorder(top, era_shrunk)
  if (direction == "brave") setorder(top, -era_shrunk)
  top[, lab := factor(lab, levels = rev(lab))]
  top[, is_mcd := coach == "Josh McDaniels"]
  marg_lab <- top[!is.na(marg_go_rate),
                  .(lab, x = pmax(hi, 0) + 1.2,
                    txt = sprintf("marginal spots: %.0f%% (n=%d)", marg_go_rate, marg_n))]

  # McDaniels may not crack the top N. Say so honestly rather than claiming
  # a ring that is not on the chart.
  mcd_full <- dat[coach == "Josh McDaniels"]
  mcd_rank <- if (nrow(mcd_full)) (if (direction == "cowardly") mcd_full$rank_cowardly else mcd_full$rank_brave) else NA
  mcd_note <- if (any(top$is_mcd)) {
    "Josh McDaniels is ringed."
  } else if (nrow(mcd_full) == 1) {
    sprintf("Josh McDaniels ranks #%d of %d here (shrunk %+.1fpp), outside the top %d shown; see the CSV or console output for his exact numbers.",
            mcd_rank, nrow(dat), mcd_full$era_shrunk, n_show)
  } else {
    "Josh McDaniels does not have enough clear-cut decisions to qualify for this board; see the console output for his raw numbers."
  }

  p <- ggplot(top, aes(era_adj, lab)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.45) +
    geom_col(aes(fill = side), width = 0.64) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.26, linewidth = 0.55, colour = "grey35") +
    geom_point(aes(x = era_shrunk), shape = 18, size = 2.5, colour = "grey15") +
    geom_point(data = top[is_mcd == TRUE], aes(x = era_shrunk), shape = 1, size = 5.2,
               stroke = 1.1, colour = "#8a3d00") +
    { if (nrow(marg_lab) > 0)
      geom_text(data = marg_lab, aes(x = x, y = lab, label = txt), inherit.aes = FALSE,
                hjust = 0, size = 2.7, colour = "grey45") } +
    scale_fill_manual(values = c("Braver than his era" = fill_pos,
                                 "More cowardly than his era" = fill_neg)) +
    scale_x_continuous(expand = expansion(mult = c(0.12, 0.30))) +
    labs(title = title,
         subtitle = subtitle_extra,
         x = "go rate in clear-cut spots, above (+) or below (-) his own era, percentage points", y = NULL,
         caption = fig_caption(
           "nflverse play-by-play; nfl4th decision model, 2018 to 2025",
           sprintf("%s clear-cut fourth downs (go_boost >= %.1f win-probability points, factory/50's threshold).\nHead coaches with %d+ clear-cut decisions.",
                   format(nrow(clear), big.mark = ","), CUT, MIN_DECISIONS),
           paste0(sprintf("\nBars are the raw rate against the coach's own era, whiskers are 95%% intervals, diamonds are the empirical-Bayes shrunk estimate. %d of %d coaches (%.0f%%) cannot be honestly\n",
                          nrow(dc) - n_separable, nrow(dc), 100*(nrow(dc) - n_separable)/nrow(dc)),
                  sprintf("separated from their era's average; treat a bar without daylight between its whisker and zero as noise, not verdict. Split-half reliability of this rate is %.2f (Spearman-\n", r_sb),
                  "Brown), against this project's own r >= 0.30 bar for calling something a trait. The related decision-cost ledger (factory/95) persists year over year at only r = 0.21 to\n",
                  "0.26, the same order of magnitude, so read this as measured behavior in this window rather than a proven permanent trait.\n",
                  sprintf("%s Built by R/19.", mcd_note))
         )) +
    theme_coach(grid = "none") +
    theme(legend.position = "top", legend.title = element_blank(),
          legend.justification = "left", axis.text.y = element_text(size = rel(0.82)))
  save_fig(path, p, w = 11.8, h = 7.4)
}

setorder(dc, era_shrunk)
mk_board(dc, 12, "cowardly",
         "The cowardly coach leaderboard: who kicks when the math clearly says go",
         "Head coaches who go for it least, in fourth-down spots where nfl4th says going is the clearly better win-probability play",
         "docs/figures/cowardly.png", fill_pos = "#9db6c9", fill_neg = "#D55E00")

setorder(dc, -era_shrunk)
mk_board(dc, 12, "brave",
         "The brave coach leaderboard: who goes for it when the math says go",
         "Head coaches who go for it most, in those same clear-cut spots.\nRight-hand number: the same coach's go rate in the marginal 0-to-1-point band, where nerve actually shows",
         "docs/figures/brave.png", fill_pos = "#2B8CBE", fill_neg = "#9db6c9")
