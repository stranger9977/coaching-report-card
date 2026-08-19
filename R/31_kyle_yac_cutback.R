# =============================================================================
# 31_kyle_yac_cutback.R
#
# Michael's ask, verbatim: "what makes Schanahan special this is just the
# output. Besides just 'scheme'. Like we have a lot of ammo for Ben Johnson -
# coin flip guy, when he goes against the mold he's the only one
# successful...and a ton for McVay (13 personal etc). But what do we have
# for Kyle?"
#
# Two Sumer field families nobody on this board has touched yet:
#
#   TEST 1, THE YAC FACTORY. Shanahan's offense is famous for turning short
#   throws into long gains -- wide zone run-fits bleeding into the pass game,
#   receivers catching in space. Tested as: yards after the catch, per
#   caller, against a league model of expected YAC built from throw depth +
#   screen flag + situation only (down/distance/field position/quarter/score
#   diff/two-minute/four-minute/redzone/short-yardage/goal-line -- nothing
#   about who is coaching goes in, so what's left over is the coach's own
#   catch-and-run design). A deep dig naturally gets less YAC than a
#   checkdown; the model knows that, so credit only goes to a caller whose
#   completions run further than the SITUATION predicts. Paired with average
#   depth of target (aDOT), because the folk claim isn't just "gets YAC," it's
#   "throws short ON PURPOSE and manufactures the rest."
#
#   TEST 2, THE RUN THAT NEVER GOES WHERE IT STARTS. Sumer charts both the
#   side a run is DESIGNED to hit (run_gap_intent_side, left/right only) and
#   the side it actually crosses the line of scrimmage (run_gap_outcome_side,
#   left/middle/right) plus the specific gap it hit (run_gap_outcome, A/B/C/D
#   gap or EDGE). The hypothesis: Shanahan's outside-zone game is a one-cut,
#   read-and-react design that's SUPPOSED to bounce away from where it's
#   drawn, and it should be productive when it does; McVay's man/gap game is
#   supposed to stay on script. Tested both ways -- the literal side-flip
#   rate and its EPA payoff, and (because "intent side" is only left/right,
#   coarse enough to miss a zone cut that stays on the called side but
#   changes gap) a secondary look at how spread out the actual landing gap
#   is within a call, using run_gap_outcome's five-way gap detail.
#
# DATA. Sumer play charting via R/factory/lib_sumer.R's load_sumer(),
# 2022-2025 (2026 is unplayed preseason charting, excluded by load_sumer's
# own season filter). Player-level YAC leaderboard joins
# data/raw/sumer/plays_players_p1.csv.gz + _p2.csv.gz (both needed --
# together they cover 2022-2025, not split by season) to the play-level
# model's per-play actual/expected YAC on sumer_play_id + season (same join
# R/29 uses), keyed to the one player per play with receiving_targets == 1
# (verified 1:1, no play has two targeted receivers in a season). Names from
# data/raw/sumer/player_details.csv.gz (football_name + last_name).
#
# UNIVERSE. Non-garbage-time, off_caller attributed (load_sumer() already
# maps team-week to caller). "Qualified" caller = >= 1200 total offensive
# plays (run + pass) over 2022-2025, same bar as R/14's FTN signature piece;
# 37 of 65 callers clear it, and every one of them independently clears
# 400+ qualifying run rows for test 2, so one caller list serves both tests.
#
# CAVEATS BUILT INTO THE TESTS (not discovered after the fact):
#   - depth_of_target and passing_air_yards are the same field (r = 1.00 on
#     completions) -- only depth_of_target is used, passing_air_yards is
#     redundant.
#   - catchable_target is not used as a model feature: it's a charting
#     judgment call almost entirely implied by is_complete_pass (337 of
#     57,197 completions are marked catchable_target == FALSE, a QC edge
#     case, printed below and left in the sample rather than hand-filtered).
#   - The YAC model's own fit is modest (R^2 ~0.15, printed below) -- YAC is
#     mostly receiver improvisation and broken tackles, not situation. That's
#     expected; the model only needs to strip out throw-depth/situation
#     confounds, not explain YAC fully.
#   - The player-level YAC-over-expected leaderboard has no position control,
#     so running backs on checkdowns dominate the very top (a real, separate
#     finding, reported as such) -- Shanahan's core weapons are checked
#     against that pattern explicitly, not read off an unadjusted rank 1.
#   - run_gap_intent_side is left/right only; it cannot see a cut that stays
#     on the called side but changes gap. Test 2 reports the literal
#     side-flip test AND the gap-dispersion secondary look for that reason.
#
# Conventions: R/lib/theme_coach.R, no em dashes, honest computed titles,
# plain football words in every chart axis/title (no invented metric names).
#
# Out: docs/figures/kyle_yac.png      (Test 1: aDOT x YAC-over-expected,
#                                       37 qualified callers, three named)
#      docs/figures/kyle_cutback.png  (Test 2: side-flip rate + its EPA cost,
#                                       Shanahan vs league vs McVay, by
#                                       run-concept family)
#      data/derived/kyle_yac_cutback.csv (one row per qualified caller, both
#                                       tests' measures, ranks, CIs)
# =============================================================================

suppressMessages({
  library(data.table); library(xgboost); library(ggplot2)
  library(ggrepel); library(patchwork); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)
set.seed(20260819)

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

MIN_OFF_PLAYS <- 1200
KYLE  <- "Kyle Shanahan"
MCVAY <- "Sean McVay"
BJ    <- "Ben Johnson"
THREE <- c(KYLE, MCVAY, BJ)

# -----------------------------------------------------------------------
# 1. Load, universe, qualified-caller list (shared by both tests)
# -----------------------------------------------------------------------
d <- load_sumer(2022:2025)
d <- d[garbage_time == FALSE & off_caller != ""]
cat(sprintf("Non-garbage-time, caller-attributed plays 2022-2025: %s\n", format(nrow(d), big.mark = ",")))

off_n <- d[run_pass %in% c("P","R"), .N, by = off_caller]
qualified <- off_n[N >= MIN_OFF_PLAYS]$off_caller
cat(sprintf("Callers with >= %d offensive plays: %d of %d\n", MIN_OFF_PLAYS, length(qualified), nrow(off_n)))
stopifnot(all(THREE %in% qualified))

# =============================================================================
# TEST 1: THE YAC FACTORY
# =============================================================================
cat("\n================ TEST 1: THE YAC FACTORY ================\n")

# ---- 2. Expected-YAC model: leave-one-season-out xgboost regression -------
comp <- d[is_targeted_pass == TRUE & is_complete_pass == TRUE & !is.na(screen)]
n_uncatchable_comp <- comp[catchable_target == FALSE, .N]
cat(sprintf("Completions: %d (%d marked catchable_target == FALSE, a charting QC edge case, kept in)\n",
            nrow(comp), n_uncatchable_comp))
comp[, screen_num := as.integer(screen)]

YAC_FEATS <- c("depth_of_target", "screen_num", SUMER_STATE)

yac_expect <- function(x, feats, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(yards_after_catch)], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x$yards_after_catch)
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

z <- yac_expect(comp, YAC_FEATS)
z[, resid := .y - .expected]
model_r2 <- 1 - var(z$resid) / var(z$.y)
cat(sprintf("Model check: %d of %d completions scored, cor(actual,expected)=%.3f, R^2-ish=%.3f\n",
            nrow(z), nrow(comp), cor(z$.y, z$.expected), model_r2))
cat("(YAC is mostly receiver skill/broken tackles, not situation -- the model only needs to strip\n")
cat(" out throw-depth and situation confounds, not explain YAC fully. R^2 this low is expected.)\n")

# ---- 3. Per-caller: YAC over expected, aDOT, YAC share of passing yards ---
adot_tab <- d[is_targeted_pass == TRUE, .(n_targets = .N, adot = mean(depth_of_target)), by = off_caller]

oe_tab <- z[, .(n_completions = .N, yac_actual = mean(.y), yac_expected = mean(.expected),
                yac_oe = mean(resid), se_oe = sd(resid) / sqrt(.N)), by = off_caller]
oe_tab[, `:=`(yac_oe_lo = yac_oe - 1.96 * se_oe, yac_oe_hi = yac_oe + 1.96 * se_oe)]

share_tab <- comp[, .(yac_sum = sum(yards_after_catch, na.rm = TRUE),
                      pass_yds_sum = sum(passing_yards, na.rm = TRUE)), by = off_caller]
share_tab[, yac_share_pct := 100 * yac_sum / pass_yds_sum]

callers <- merge(adot_tab, oe_tab, by = "off_caller")
callers <- merge(callers, share_tab[, .(off_caller, yac_share_pct)], by = "off_caller")
callers <- merge(off_n[, .(off_caller, n_off_plays = N)], callers, by = "off_caller")
callers <- callers[off_caller %in% qualified]

setorder(callers, adot); callers[, rank_adot := seq_len(.N)]
setorder(callers, -yac_oe); callers[, rank_yac_oe := seq_len(.N)]
setorder(callers, -yac_actual); callers[, rank_yac_raw := seq_len(.N)]
setorder(callers, -yac_share_pct); callers[, rank_yac_share := seq_len(.N)]
setorder(callers, off_caller)
n_q <- nrow(callers)

print_one <- function(nm) {
  r <- callers[off_caller == nm]
  cat(sprintf("\n%s (n_completions=%d, n_targets=%d):\n", nm, r$n_completions, r$n_targets))
  cat(sprintf("  aDOT (avg throw depth, all targets):        %.2f yds  -- rank %d of %d (1 = shortest)\n",
              r$adot, r$rank_adot, n_q))
  cat(sprintf("  YAC per completion (raw):                    %.2f yds -- rank %d of %d\n",
              r$yac_actual, r$rank_yac_raw, n_q))
  cat(sprintf("  YAC over expected (actual minus model):     %+.3f yds/completion [%.3f, %.3f] -- rank %d of %d\n",
              r$yac_oe, r$yac_oe_lo, r$yac_oe_hi, r$rank_yac_oe, n_q))
  cat(sprintf("  YAC share of total passing yards:            %.1f%% -- rank %d of %d\n",
              r$yac_share_pct, r$rank_yac_share, n_q))
}
cat(sprintf("\n--- Three-name contrast, %d qualified callers (>= %d offensive plays, 2022-2025) ---\n", n_q, MIN_OFF_PLAYS))
for (nm in THREE) print_one(nm)

cat("\n--- Full qualified-caller table, sorted by YAC over expected ---\n")
print(callers[order(-yac_oe), .(off_caller, n_completions, adot = round(adot,2),
                                 yac_actual = round(yac_actual,2), yac_oe = round(yac_oe,3),
                                 rank_yac_oe, rank_adot, yac_share_pct = round(yac_share_pct,1))])

# ---- 4. Player-level leaderboard (supporting color) ------------------------
PCOLS <- c("sumer_play_id","sumer_player_id","season","side_of_ball","receiving_targets")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
tgt <- players[receiving_targets == 1 & season %in% 2022:2025, .(sumer_play_id, season, sumer_player_id)]

zp <- merge(z[, .(sumer_play_id, season, .y, .expected)], tgt, by = c("sumer_play_id","season"))
cat(sprintf("\nPlayer-play join: %d of %d modeled completions matched to a targeted receiver\n", nrow(zp), nrow(z)))

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id","football_name","last_name","position"))
pd[, name := paste(football_name, last_name)]

player_tab <- zp[, .(catches = .N, actual_yac = sum(.y), expected_yac = sum(.expected)), by = sumer_player_id]
player_tab[, oe_per_catch := (actual_yac - expected_yac) / catches]
player_tab <- merge(player_tab, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
player_tab <- player_tab[catches >= 50]
setorder(player_tab, -oe_per_catch)
player_tab[, rank := seq_len(.N)]
n_players <- nrow(player_tab)

cat(sprintf("\n--- Player-level YAC-over-expected leaderboard (career 2022-2025, >= 50 catches, n=%d players) ---\n", n_players))
cat("Top 20 overall (NOTE: no position control -- running backs on checkdowns dominate the very top,\n")
cat(" a real pattern in its own right, not a bug):\n")
print(head(player_tab[, .(rank, name, position, catches, oe_per_catch = round(oe_per_catch,2))], 20))
pos_top20 <- table(head(player_tab, 20)$position)
cat("Position mix, top 20:", paste(sprintf("%s=%d", names(pos_top20), pos_top20), collapse = ", "), "\n")

sf_names <- c("Christian McCaffrey","Deebo Samuel","George Kittle","Brandon Aiyuk",
              "Jauan Jennings","Kyle Juszczyk","Trent Taylor","Ricky Pearsall",
              "Jordan Mason","Elijah Mitchell","Jerick McKinnon","Brock Purdy")
cat("\nShanahan's own SF weapons, named and ranked (not filtered to top-of-list -- reported wherever they fall):\n")
print(player_tab[name %in% sf_names][order(rank), .(rank, name, position, catches, oe_per_catch = round(oe_per_catch,2))])
n_sf_top50 <- player_tab[name %in% sf_names & rank <= 50, .N]
cat(sprintf("Of %d named SF skill players tracked, %d land in the top 50 of %d (top %.0f%%).\n",
            sum(sf_names %in% player_tab$name), n_sf_top50, n_players, 100*50/n_players))

fwrite(player_tab, "data/derived/kyle_yac_players.csv")

# ---- 5. CHART A: aDOT x YAC over expected, 37 callers, three highlighted --
kyle_r <- callers[off_caller == KYLE]

title_a <- sprintf(
  "Shanahan's completions gain %+.2f more yards after the catch than his throw depth predicts (rank %d of %d)",
  kyle_r$yac_oe, kyle_r$rank_yac_oe, n_q)

lab_bj  <- callers[off_caller %in% setdiff(THREE, KYLE)]
lab_kyle <- callers[off_caller == KYLE]

p_a <- ggplot(callers, aes(adot, yac_oe)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4, linetype = "dashed") +
  geom_point(colour = "#9db6c9", alpha = 0.65, size = 2.3) +
  geom_point(data = lab_bj, colour = "#D55E00", size = 3.4) +
  geom_point(data = lab_kyle, colour = "#D6604D", size = 4.6) +
  geom_text_repel(data = lab_bj, aes(label = off_caller), size = 3.3, fontface = "bold",
                   colour = "#8a3d00", seed = 11, box.padding = 0.55, min.segment.length = 0, max.overlaps = 30) +
  geom_text_repel(data = lab_kyle, aes(label = off_caller), size = 3.8, fontface = "bold",
                   colour = "#7a1a1a", seed = 11, box.padding = 0.6, min.segment.length = 0) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.5)) +
  labs(
    title = title_a,
    subtitle = paste0(
      "Each point is one play-caller's career (2022-2025, >= 1,200 offensive plays). X is how far downfield he throws, on\n",
      "average (all targets) -- Shanahan's is ordinary, rank ", kyle_r$rank_adot, " of ", n_q, " (1 = shortest). Y is yards after\n",
      "the catch beyond what throw depth, screen use, and the situation predict; above the dashed line runs further than that."
    ),
    x = "average depth of target (yards downfield, all targets)",
    y = "yards after the catch, beyond what depth + situation predict (per completion)",
    caption = paste(strwrap(fig_caption(
      "Sumer play charting 2022-2025 (R/factory/lib_sumer.R)",
      sprintf("%d play-callers with >= %d offensive plays. Expected YAC from a season-holdout model (depth of target, screen flag, down/distance/field position/quarter/score/clock).", n_q, MIN_OFF_PLAYS),
      "Non-garbage-time. Model R^2 is modest by design (YAC is mostly receiver skill) -- it only removes depth/situation confounds, not YAC itself."
    ), width = 130), collapse = "\n")
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/kyle_yac.png", p_a, w = 12, h = 7.4)

# =============================================================================
# TEST 2: THE RUN THAT NEVER GOES WHERE IT STARTS
# =============================================================================
cat("\n\n================ TEST 2: THE RUN THAT NEVER GOES WHERE IT STARTS ================\n")

# ---- 6. Run universe, family classification, bounce flag ------------------
runs <- d[rush_attempt == TRUE & quarterback_scramble == FALSE &
          !is.na(run_gap_intent_side) & !is.na(run_gap_outcome_side) & !is.na(run_gap_outcome) &
          run_gap_intent_side %in% c("LEFT","RIGHT")]
cat(sprintf("Qualifying designed-run rows (rush, not a scramble, gap intent+outcome both charted): %s\n",
            format(nrow(runs), big.mark = ",")))

runs[, family := fifelse(run_concept == "OUTSIDE ZONE", "Outside zone",
                  fifelse(run_concept %in% c("MAN","POWER","COUNTER","PULL LEAD","TRAP"), "Man/gap", "other"))]
runs[, bounce := run_gap_outcome_side != run_gap_intent_side]
runs[, gap_ord := factor(run_gap_outcome, levels = c("A GAP","B GAP","C GAP","D GAP","EDGE"), ordered = TRUE)]

FAMS <- c("Outside zone", "Man/gap")

# ---- 7. Per-caller x family: n, bounce rate, EPA stayed vs bounced --------
wilson_ci <- function(k, n, z = 1.96) {
  if (n == 0) return(c(NA, NA))
  p <- k/n; denom <- 1 + z^2/n
  centre <- (p + z^2/(2*n)) / denom
  half <- (z/denom) * sqrt(p*(1-p)/n + z^2/(4*n^2))
  c(centre - half, centre + half)
}

caller_family_stats <- function(pool) {
  out <- pool[, {
    k <- sum(bounce); n <- .N
    ci <- wilson_ci(k, n)
    epa_stay <- mean(expected_points_added[!bounce], na.rm = TRUE)
    epa_bnc  <- mean(expected_points_added[bounce], na.rm = TRUE)
    n_bnc <- sum(bounce); n_stay <- n - n_bnc
    tt <- if (n_bnc >= 5 && n_stay >= 5) t.test(expected_points_added[!bounce], expected_points_added[bounce]) else NULL
    .(n = n, n_bounce = k, bounce_rate = 100*k/n, bounce_lo = 100*ci[1], bounce_hi = 100*ci[2],
      epa_stayed = epa_stay, epa_bounced = epa_bnc, epa_gap = epa_stay - epa_bnc,
      epa_gap_p = if (!is.null(tt)) tt$p.value else NA_real_)
  }, by = .(off_caller, family)]
  out[]
}

cf <- caller_family_stats(runs[family %in% FAMS])
cf_overall <- runs[, .(n = .N, n_bounce = sum(bounce), bounce_rate = 100*mean(bounce)), by = off_caller]
cf_overall[, bounce_lo_hi := NA]

league_fam <- caller_family_stats(runs[family %in% FAMS][, off_caller := "LEAGUE"])
league_overall <- runs[, .(off_caller = "LEAGUE", n = .N, n_bounce = sum(bounce), bounce_rate = 100*mean(bounce))]

cat(sprintf("\nLeague overall bounce rate (any run, any side-flip): %.1f%% (n=%d)\n",
            league_overall$bounce_rate, league_overall$n))
print(league_fam[, .(family, n, bounce_rate = round(bounce_rate,1), epa_stayed = round(epa_stayed,3),
                     epa_bounced = round(epa_bounced,3), epa_gap = round(epa_gap,3), epa_gap_p = round(epa_gap_p,4))])

report_bounce <- function(nm) {
  r_all <- cf_overall[off_caller == nm]
  cat(sprintf("\n%s -- overall bounce rate (all concepts): %.1f%% (n=%d, n_bounce=%d)\n",
              nm, r_all$bounce_rate, r_all$n, r_all$n_bounce))
  rf <- cf[off_caller == nm]
  print(rf[, .(family, n, bounce_rate = round(bounce_rate,1), bounce_ci = sprintf("[%.1f, %.1f]", bounce_lo, bounce_hi),
              epa_stayed = round(epa_stayed,3), epa_bounced = round(epa_bounced,3),
              epa_gap = round(epa_gap,3), p = round(epa_gap_p,4))])
  # significance test vs league, same family
  for (fm in FAMS) {
    a <- runs[off_caller == nm & family == fm]
    b <- runs[family == fm & off_caller != nm]
    if (nrow(a) >= 10 && nrow(b) >= 10) {
      pt <- prop.test(c(sum(a$bounce), sum(b$bounce)), c(nrow(a), nrow(b)))
      cat(sprintf("  %s vs league (excl. self), bounce rate: %.1f%% vs %.1f%%, prop.test p=%.4f\n",
                  fm, 100*mean(a$bounce), 100*mean(b$bounce), pt$p.value))
    }
  }
}
for (nm in THREE) report_bounce(nm)

cat("\nVERDICT, literal side-flip test: bounce rates sit in a narrow 4-8% band for every group and every\n")
cat("family, bounces are unprofitable everywhere (EPA always worse when the run flips sides than when it\n")
cat("stays), and Shanahan's own overall bounce rate is not elevated versus league -- see numbers above.\n")
cat("The literal 'his zone game is designed to flip sides more, and profitably' claim does NOT hold up.\n")

# ---- 8. Concept mix -- the signature that IS real --------------------------
mix <- runs[, .(n = .N), by = .(off_caller, family)]
mix_wide <- dcast(mix, off_caller ~ family, value.var = "n", fill = 0)
mix_wide[, total := `Outside zone` + `Man/gap` + other]
mix_wide[, `:=`(oz_share = 100*`Outside zone`/total, mangap_share = 100*`Man/gap`/total, other_share = 100*other/total)]
mix_wide <- mix_wide[off_caller %in% qualified]
setorder(mix_wide, -oz_share); mix_wide[, rank_oz := seq_len(.N)]
setorder(mix_wide, -mangap_share); mix_wide[, rank_mangap := seq_len(.N)]
setorder(mix_wide, off_caller)

cat(sprintf("\n--- Run-concept MIX, the real signature (%d qualified callers) ---\n", nrow(mix_wide)))
for (nm in THREE) {
  r <- mix_wide[off_caller == nm]
  cat(sprintf("%s: %.0f%% outside zone (rank %d of %d), %.0f%% man/gap (rank %d of %d), %.0f%% other, n=%d runs\n",
              nm, r$oz_share, r$rank_oz, nrow(mix_wide), r$mangap_share, r$rank_mangap, nrow(mix_wide), r$other_share, r$total))
}
kyle_vs_lg <- prop.test(c(mix_wide[off_caller==KYLE]$`Outside zone`, sum(runs[family=="Outside zone" & off_caller!=KYLE, .N])),
                        c(mix_wide[off_caller==KYLE]$total, sum(runs[off_caller!=KYLE, .N])))
mcvay_vs_lg <- prop.test(c(mix_wide[off_caller==MCVAY]$`Man/gap`, sum(runs[family=="Man/gap" & off_caller!=MCVAY, .N])),
                         c(mix_wide[off_caller==MCVAY]$total, sum(runs[off_caller!=MCVAY, .N])))
cat(sprintf("Shanahan's outside-zone share vs rest of league (excl. self): p=%.2e\n", kyle_vs_lg$p.value))
cat(sprintf("McVay's man/gap share vs rest of league (excl. self): p=%.2e\n", mcvay_vs_lg$p.value))

# ---- 9. Gap-outcome dispersion (secondary look, why side-flip undercounts) -
entropy_fn <- function(x) { p <- prop.table(table(x)); -sum(p*log2(p)) }

disp_league <- runs[family %in% FAMS, .(n = .N, entropy = entropy_fn(run_gap_outcome),
                                         modal_share = 100*max(prop.table(table(run_gap_outcome)))), by = family]
cat("\n--- Gap-outcome dispersion, LEAGUE (how many different gaps a call ends up hitting) ---\n")
print(disp_league[, .(family, n, entropy = round(entropy,3), modal_share_pct = round(modal_share,1))])

disp_caller <- runs[family %in% FAMS & off_caller %in% THREE,
                    .(n = .N, entropy = entropy_fn(run_gap_outcome),
                      modal_share = 100*max(prop.table(table(run_gap_outcome)))), by = .(off_caller, family)]
cat("\n--- Gap-outcome dispersion, three named callers ---\n")
print(disp_caller[, .(off_caller, family, n, entropy = round(entropy,3), modal_share_pct = round(modal_share,1))])

cat("\nVERDICT, gap dispersion: outside zone is structurally more spread across gaps than man/gap league-\n")
cat(sprintf("wide (modal-gap share %.1f%% vs %.1f%%, lower = more spread) -- consistent with zone's one-cut,\n",
            disp_league[family=="Outside zone"]$modal_share, disp_league[family=="Man/gap"]$modal_share))
cat("read-and-react design versus a predetermined hole. Shanahan's OWN outside-zone calls are more spread\n")
cat(sprintf("than the league's outside-zone average (entropy %.3f vs %.3f) and more spread than his own\n",
            disp_caller[off_caller==KYLE & family=="Outside zone"]$entropy, disp_league[family=="Outside zone"]$entropy))
cat("man/gap calls -- the cutback signature is real, it just shows up as WHERE the run lands on his own\n")
cat("side, not as flipping to the other side of the formation.\n")

# ---- 10. CHART B: bounce rate + its EPA cost, Shanahan vs league vs McVay -
chart_groups <- rbindlist(list(
  cf[off_caller %in% c(KYLE, MCVAY)][, group := off_caller],
  league_fam[, group := "League"]
), fill = TRUE)
chart_groups[, group := factor(group, levels = c(KYLE, "League", MCVAY))]
chart_groups[, family := factor(family, levels = FAMS)]

grp_colours <- c("Kyle Shanahan" = "#D6604D", "League" = "grey60", "Sean McVay" = "#4393C3")

p_bounce <- ggplot(chart_groups, aes(family, bounce_rate, fill = group)) +
  geom_col(position = position_dodge(width = 0.68), width = 0.6) +
  geom_errorbar(aes(ymin = bounce_lo, ymax = bounce_hi), position = position_dodge(width = 0.68),
                width = 0.15, colour = ink_body, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f%%", bounce_rate)), position = position_dodge(width = 0.68),
            vjust = -1.6, size = 3.0, fontface = "bold", colour = ink_title) +
  scale_fill_manual(values = grp_colours, name = NULL) +
  scale_y_continuous(labels = function(x) sprintf("%.0f%%", x), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = NULL,
    x = NULL,
    y = "share of runs that cross to the OTHER side (%)"
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", plot.margin = margin(10, 14, 20, 10))

epa_gap_df <- chart_groups[, .(family, group, epa_gap)]
p_epa <- ggplot(epa_gap_df, aes(family, epa_gap, fill = group)) +
  geom_col(position = position_dodge(width = 0.68), width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", epa_gap)), position = position_dodge(width = 0.68),
            vjust = ifelse(epa_gap_df$epa_gap >= 0, -0.6, 1.4), size = 3.0, fontface = "bold", colour = ink_title) +
  scale_fill_manual(values = grp_colours, name = NULL, guide = "none") +
  labs(
    title = NULL,
    x = NULL,
    y = "EPA/play cost of bouncing\n(stayed on side minus flipped)"
  ) +
  theme_coach(grid = "y") +
  theme(plot.margin = margin(20, 14, 8, 10))

combo <- p_bounce / p_epa +
  plot_annotation(
    title = sprintf("Shanahan's zone runs don't flip to the other side more than league average (%.1f%% vs %.1f%%) -- and when any run does, it costs everyone",
                    cf[off_caller==KYLE & family=="Outside zone"]$bounce_rate, league_fam[family=="Outside zone"]$bounce_rate),
    subtitle = paste0(
      "Bounce = the run crosses the line of scrimmage on the opposite side from how it was drawn (Sumer's intent-side vs\n",
      "outcome-side charting), 2022-2025, non-garbage-time. This side-level test is coarse: it can't see a zone run that\n",
      "cuts back within its own called side (see the gap-dispersion finding in the printed output for that signature)."
    ),
    caption = paste(strwrap(fig_caption(
      "Sumer play charting 2022-2025 (R/factory/lib_sumer.R)",
      "Designed runs only (scrambles excluded). League = all qualifying runs leaguewide, not just qualified callers.",
      "Error bars on the top panel are 95% Wilson confidence intervals."
    ), width = 130), collapse = "\n")
  ) &
  theme(plot.title = element_text(face = "bold", size = rel(1.05), colour = ink_title),
        plot.subtitle = element_text(colour = ink_subtitle, size = rel(0.82)),
        plot.caption = element_text(colour = ink_caption, size = rel(0.65), hjust = 0, lineheight = 1.25))

save_fig("docs/figures/kyle_cutback.png", combo, w = 11, h = 9.5)

# =============================================================================
# 11. WRITE CSV -- one row per qualified caller, both tests
# =============================================================================
out <- merge(callers, mix_wide[, .(off_caller, n_runs = total, oz_share, mangap_share, other_share, rank_oz, rank_mangap)],
             by = "off_caller")
out <- merge(out, cf_overall[, .(off_caller, bounce_rate_overall = bounce_rate)], by = "off_caller")
cf_oz <- cf[family == "Outside zone", .(off_caller, bounce_rate_oz = bounce_rate, epa_stayed_oz = epa_stayed,
                                        epa_bounced_oz = epa_bounced, epa_gap_oz = epa_gap)]
cf_mg <- cf[family == "Man/gap", .(off_caller, bounce_rate_mangap = bounce_rate, epa_stayed_mangap = epa_stayed,
                                   epa_bounced_mangap = epa_bounced, epa_gap_mangap = epa_gap)]
out <- merge(out, cf_oz, by = "off_caller", all.x = TRUE)
out <- merge(out, cf_mg, by = "off_caller", all.x = TRUE)
setorder(out, -yac_oe)

fwrite(out, "data/derived/kyle_yac_cutback.csv")
cat(sprintf("\nwrote data/derived/kyle_yac_cutback.csv (%d rows), data/derived/kyle_yac_players.csv (%d rows)\n",
            nrow(out), nrow(player_tab)))

# =============================================================================
# 12. THE KYLE AMMO -- one-line verdicts
# =============================================================================
cat("\n================ THE KYLE AMMO ================\n")
cat(sprintf("1. YAC FACTORY, REAL: Shanahan's completions gain %+.2f more yards after the catch than a\n", kyle_r$yac_oe))
cat(sprintf("   depth+situation model expects (rank %d of %d qualified callers) -- essentially tied with\n", kyle_r$rank_yac_oe, n_q))
cat(sprintf("   Ben Johnson (%+.2f, rank %d) and far clear of McVay (%+.2f, rank %d).\n",
            callers[off_caller==BJ]$yac_oe, callers[off_caller==BJ]$rank_yac_oe,
            callers[off_caller==MCVAY]$yac_oe, callers[off_caller==MCVAY]$rank_yac_oe))
cat(sprintf("2. BUT the 'throws short on purpose' half of the claim is FALSE: his aDOT (%.2f yds, rank %d of\n",
            kyle_r$adot, kyle_r$rank_adot))
cat(sprintf("   %d, 1=shortest) is right around league average -- the YAC is engineered on normal-depth\n", n_q))
cat("   throws, not manufactured by only throwing checkdowns.\n")
cat(sprintf("3. Supporting color: %d of %d SF weapons tracked land in the top 50 of %d player-level YAC-\n",
            n_sf_top50, sum(sf_names %in% player_tab$name), n_players))
cat("   over-expected -- Deebo Samuel and George Kittle both top-20, McCaffrey top-40 -- but the very top\n")
cat("   of that list is running backs on checkdowns leaguewide, a position effect, not an SF-only one.\n")
cat(sprintf("4. CUTBACK, LITERAL VERSION: NULL. Shanahan's side-flip rate on outside zone (%.1f%%) is not\n",
            cf[off_caller==KYLE & family=="Outside zone"]$bounce_rate))
cat(sprintf("   clearly above league (%.1f%%), and bounced runs are worse EPA than on-script runs for\n",
            league_fam[family=="Outside zone"]$bounce_rate))
cat("   Shanahan, McVay, and the league alike. 'Designed to flip sides, and it pays off' does not hold.\n")
cat(sprintf("5. CUTBACK, REAL VERSION: Shanahan calls outside zone on %.0f%% of his designed runs (rank %d of\n",
            mix_wide[off_caller==KYLE]$oz_share, mix_wide[off_caller==KYLE]$rank_oz))
cat(sprintf("   %d), nearly double McVay's %.0f%% (whose plurality call is MAN, not zone, rank %d of %d on\n",
            nrow(mix_wide), mix_wide[off_caller==MCVAY]$oz_share, mix_wide[off_caller==MCVAY]$rank_mangap, nrow(mix_wide)))
cat("   man/gap share instead) -- and his own zone calls land across more different gaps than league-\n")
cat("   average zone runs do. The cutback signature is real, it's just a WITHIN-side gap story, not a\n")
cat("   side-flip story.\n")

cat("\nDONE\n")
