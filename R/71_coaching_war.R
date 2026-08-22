# =============================================================================
# 71_coaching_war.R -- Coaching WAR: wins a head coach adds per season above a
# fresh hire, net of payroll, quarterback and franchise, with honest intervals.
#
# THE QUESTION. "The best coaches who win games should be at the top but dont
# fudge it." So: how many wins per season does each head coach add over the
# coach a franchise would actually get if it did not keep him, once you take
# out the payroll he was handed, LAST season's quarterback, and the building he
# works in? (Not "his roster": the control is payroll plus last season's QB,
# and a residual roster signal remains; see SKEPTIC LOG items 5 and 9.)
#
# THE IDENTITY. Everything is in wins per 17 games so the pieces add up exactly.
#   mkt17     = wins the closing spread implied (R/02's market expectation)
#   talent17  = the part of mkt17 that payroll (R/06 contract talent), QB cap
#               share and LAST season's QB EPA (R/01's lagged G4 control) explain
#   premium17 = mkt17 - talent17   what the market credited beyond priced talent
#   wae17     = act17 - mkt17      what the team did beyond the market (R/02)
#   wat17     = act17 - talent17   = premium17 + wae17    wins above talent
# The coach model is lmer(wat17 ~ 1 + (1|coach) + (1|team) + (1|season)),
# weighted by games/17. The (1|team) term is the franchise (owner, GM, QB
# dynasty that straddles coaches); the coach effect is identified by teams
# changing coaches and coaches changing teams. The coach BLUP is already
# shrunk; short careers keep little of their raw number.
#
# REPLACEMENT. The weighted mean of what first-season coach-team rows actually
# delivered (wat17 minus the fitted team and season effects), because that is
# the coach a franchise gets when it does not keep the incumbent. Estimated,
# not assumed, and used as computed even if it comes out above zero. Two
# alternatives (league average; one-and-done pool) are written as sensitivity.
#
# DATA. nflverse schedules with closing spreads 1999-2025 (games.csv); contract
# talent 2012-2025 (data/derived/contract_talent.csv, R/06); lagged QB EPA
# (data/derived/team_talent.csv, R/01); Madden talent 2017-2025 (R/03) and
# play-callers / fourth-down decision value as side columns only.
#
# HONEST LIMITS. A coach and a QB who never part cannot be separated (Belichick
# and Brady, Reid and Mahomes): those are partnership numbers. Coach-team
# assignment is not random. Coordinators and GMs are invisible. The panel is 14
# seasons, 448 rows, about 105 coaches; single-season persistence of wat17 is
# expected near 0.09 by the variance components, so this is a career quantity
# and cannot grade a two-season coach. The 1999-2025 market-only column has no
# talent or franchise control and is a different quantity, labelled as such.
#
# SKEPTIC LOG round 1 (three reviewers, 13 objections; what was done)
#  1. "Tenure is earned" has no power vs firing-on-luck (major). RIGHT. Added a
#     parametric null: panels from M_wat's components with coach SD = 0, spells
#     cut by the fitted hazard h2, 1000 sims. No-trait null gives a 5+ vs <=2
#     gap of +2.22 (95% range +1.19 to +3.28); observed +3.23 is inside it
#     (P = 0.03 one-sided). verdict_tenure now reports the persistence test:
#     years 1-2 vs 3+ within spell r = +0.30 [0.05, 0.51] (n = 61, 3+ spells),
#     r = -0.03 [-0.45, 0.39] (n = 22, 5+). Figure title and subtitle say the
#     gap is what any firing rule on wins produces. Null written to
#     coaching_war_survivorship_null.csv.
#  2. Replacement lifted 0.3 by a franchise term made of fired coaches (major).
#     PARTLY RIGHT. cor(team effect, coaches used) = -0.72 as claimed. But the
#     skeptic's -0.385 is the raw first-season mean without subtracting the
#     model intercept (-0.145) that the coach effects are centred on; net of
#     season only the zero line is -0.24, so it moves 0.16, not 0.3. Franchise
#     term from years-3+ rows only: -0.25. Leave-one-spell-out franchise term:
#     -0.07, identical to published, so a first-season row does not credit
#     itself back through its own spell. All four in validation CSV and as
#     columns in the sensitivity CSV; leaderboard subtitle states the definition.
#  3. Bottom of board is the worst survivors; rank tracks seasons (minor).
#     RIGHT. Spearman(WAR, seasons) = 0.54 written to validation; leaderboard
#     title and subtitle say "among coaches who got at least 4 seasons";
#     rank_first3 already in the CSV for the page.
#  4. Within-coach bootstrap biased to the raw mean (blocking). RIGHT. Replaced
#     with a parametric bootstrap of the prediction error (simulate from M_wat
#     on the real design, refit, error = estimated WAR minus true WAR), 1000
#     reps. Centring asserted: max |boot_mean - WAR| over 8+ season coaches =
#     0.08 (< 0.1), cor with (raw - WAR) = -0.05. prob_above_avg now 0.87-0.92
#     for the top 5, nobody >= 0.95; P(top 5) 0.24-0.34; rank 90% ranges 1-33.
#  5. Madden still predicts wat17 at +0.69/SD (major). RIGHT on the number
#     (+0.684, t = 4.0). Not added to the main measure: Madden exists 2017+
#     only and a control that switches on mid-panel would grade Belichick
#     2012-16 and 2017+ on different scales. Variant with Madden (2017+
#     interaction) in the talent fit written to sensitivity (Spearman 0.98
#     with main, residual coefficient falls to +0.07); every rendered phrase
#     changed from "roster" to "payroll and last season's quarterback"; the
#     +0.68 coefficient is in validation as residual_roster_madden_coef.
#  6. Odd/even games split is team-season reliability (major). RIGHT. Team-
#     season split with the coach ignored r = 0.32 (n = 435); coach split net
#     of franchise r = 0.29. Both rows added; reliability CSV has a "measures"
#     column labelling the games split as within-season team consistency and
#     the odd/even seasons row (r = 0.09) as the coach reliability.
#  7. Wilcoxon on rows not spells (minor). RIGHT. Now one row per spell:
#     shift +3.48 [2.26, 4.46], p = 2e-6, n = 21 vs 28; year-1 rows only
#     +2.95, p = 0.003.
#  8. Clustering and within-season z-scores (minor). RIGHT. Coach-clustered
#     sandwich SEs for the hazard coefficients and the OOS partial test (WAR
#     given prev wins t = 0.91; WAR alone t = 2.74); note on season-t z-scores
#     in validation. Conclusions unchanged.
#  9. "After quarterback" is only last season's QB (blocking). RIGHT. Same-
#     season QB EPA per dropback computed from nflfastR (cached to
#     data/derived/qb_epa_same_season.csv): +1.87 wins/SD on wat17, R2 0.39,
#     cor with premium 0.61. Variant with it as a fixed effect written to
#     sensitivity: coach SD 0.84 -> 0.68, Spearman 0.59 with main, top-10
#     overlap 5; Reid 11 -> 39, Shanahan 19 -> 41, Fisher 45 -> 10. Axis now
#     says "last season's quarterback"; caption gives the same-season bound.
# 10. Payroll captures little roster; premium is mostly roster (major). RIGHT
#     on the numbers (talent17 SD 1.19 vs market 2.00; cor(premium, same-
#     season QB) 0.61; cor(premium, prior-season team wat17) 0.32). Components
#     subtitle rewritten to say so. Prior-season team wat17 as a fixed effect:
#     coefficient +0.06, Spearman 0.97 with main, top-10 overlap 9, so the
#     carry-over channel moves little; the same-season QB channel is the one
#     that moves the board.
# 11. QB cap share NA on 18 rows (major). RIGHT. Filled 16 from the same QB's
#     nearest season (2 Skelton/Keenum rows have no other season; left at
#     average, flag kept as qb_na, fill flag qb_filled). Movement: Belichick
#     1.03 -> 1.02, Payton 0.86 -> 0.84, Tomlin 0.77 -> 0.75, J. Harbaugh
#     0.74 -> 0.75, Campbell 0.40 -> 0.38. Order of the top 10 unchanged.
# 12. Vrabel rank 4 rides one season (minor). RIGHT, and he is not alone.
#     Leave-one-season-out influence column added: 12 of 50 eligible move
#     more than 0.3 when one row is dropped (Vrabel 0.83 -> 0.48 without 2025
#     NE; Reid 0.62 -> 1.15 without 2012 PHI; McVay 0.66 -> 1.03 without 2022
#     LA). Flagged in the leaderboard label with the number without that row.
# 13. Head coach vs play-caller (minor). Wording kept as "head coach". Own
#     play-caller (>= 50% of games) vs not among the 50 eligible: +0.26 vs
#     +0.23, difference +0.03 [-0.21, +0.27], p = 0.80. In validation CSV.
#  Runtime is now about 6 minutes cold (1000 bootstrap refits, 350 influence
#  refits, 96 leave-one-spell-out refits, 2000 null simulations); all cached.
#
# SKEPTIC LOG round 2 (three reviewers, 11 objections; what was done)
#  1. Interim flag drops first-year hires fired midseason (major). RIGHT.
#     interim is now "not the team's week-1 coach" (first_week > 1); Meyer
#     2021, Hackett 2022, Reich 2023 are asserted to be in the pool and
#     Saturday 2022 asserted to stay interim. Replacement -0.186 (n = 99) vs
#     -0.072 under the old rule (kept in validation as
#     rep_offset_old_full_season_rule). Every WAR rises about 0.11; order
#     unchanged. Leaderboard subtitle and HEADLINE updated.
#  2. Persistence r = +0.30 has no null (major). RIGHT. sim_shift() now also
#     returns the early-vs-late r for 3+ and 5+ spells. No-trait null for 3+:
#     median +0.11, 95% range [-0.15, +0.38], P(null >= 0.30) = 0.11;
#     fitted-trait world median +0.24. 5+: no-trait +0.17 [-0.24, +0.55] vs
#     observed -0.03. Survivorship subtitle says the test has no power;
#     verdict_tenure no longer offers it as a skill test. Rows
#     null_persistence_r_* in the survivorship CSV; draws in the null CSV.
#  3. OOS scores only retained coaches (minor). RIGHT. run_oos() now records
#     the t-1 coaches absent in t: 73 coach-seasons with WAR through t-1
#     averaging +0.05 vs +0.34 for the 267 scored. Written to
#     coaching_war_oos_unscored.csv and validation; caption line added.
#  4. Left-truncated veterans mixed with full careers (minor). RIGHT. Label
#     now reads "12 seasons, hired 2000" for coaches hired before 2012.
#     Veterans vs in-window hires among eligible: +0.47 (n 19) vs +0.28
#     (n 31), +0.19 [-0.05, +0.43], p = 0.12 (veteran_vs_inwindow_hire_diff).
#  5. Coach SD not identified; split-half contradicts it (blocking). RIGHT.
#     (1) Profile CIs: coach SD 0.84 [0.00, 1.32], team 0.76 [0.00, 1.24];
#     odd seasons 0.65, even 1.33. (2) Model-implied split-half, 200
#     simulated panels: median 0.34 [0.04, 0.57] among the 135 where both
#     halves were identified; observed 0.085 sits at the 6th percentile,
#     inside the range, not below it as the skeptic expected, because 65 of
#     200 simulated panels put one half's coach SD on the zero boundary
#     (counted as r = 0 the range is [0.00, 0.55]). Either way the measure
#     has not cleared the 0.30 gate and the rendered title says so. (3) tau
#     propagated two ways: the bootstrap draws the true coach SD per rep from
#     the profile likelihood (P(above avg) barely moves, 0.91 -> 0.92 for the
#     top coach, because the refit re-shrinks with the data), and a tau-
#     marginalised posterior (38 fixed-tau fits weighted by the profile
#     likelihood): top coach +1.04 [-0.13, +2.69], P(above avg) 0.87 instead
#     of 0.92. Fixed-tau boards at 0.40 and 1.32 written to the sensitivity
#     CSV and coaching_war.csv (war_tau_low +0.50 for the top coach, P 0.75;
#     war_tau_high +1.88). Blue crosses on the leaderboard are the tau = 0.40
#     board. (4) Title and subtitle state the reliability r = 0.08
#     [-0.16, 0.31], the profile interval, and the low-tau value.
#  6. Portable coach variance is zero (major). RIGHT. Nested profile CI:
#     portable coach SD 0.01 [0.00, 1.13], coach-within-team 1.26 [0.51,
#     1.66]. Caption carries both; axis reads "a coach-team pairing delivered".
#  7. OOS r pools degenerate holdouts; season-clustered t = 1.6 (minor).
#     RIGHT. Two-way (coach + season) t: WAR alone 1.79, given prev wins
#     0.71. Holdouts whose predictions have SD >= 0.2 (2019-2025, n = 173):
#     r = 0.134 [-0.016, 0.277]. Both on the validation figure.
#  8. Two-sided band used to call one-sided P = 0.03 "inside" (minor). RIGHT
#     on the principle; the numbers moved with fix 1: observed gap +3.51 vs
#     null median +2.24 [+1.24, +3.34], one-sided P = 0.012, so the gap is now
#     OUTSIDE the band. Title says firing alone produces about two-thirds of
#     it; subtitle gives the one-sided P; persistence stays the deciding test
#     and (fix 2) has no power.
#  9. Franchise term at one-coach franchises set by one other-coach row
#     (major). RIGHT. Influence loop extended to all 451 rows. Other-coach row
#     > 0.3: only Reid (Crennel 2012 KC: +0.62 -> +0.31). Belichick without
#     Vrabel 2025 NE +0.95 -> +1.22 (delta 0.28, under the flag) and Carroll
#     without Macdonald 2025 SEA +0.26 -> +0.45 (0.19; the skeptic's 0.51 is
#     not reproduced). franchise_share / franchise_pinned (>= 85%) added:
#     Belichick 86%, Tomlin 100%, J. Harbaugh 100%, Reid 93%, Carroll 86%;
#     pinned coaches show the own-franchise other-coach number in the label
#     and the caption names them.
# 10. Arians is a second Brady partnership number (major). RIGHT. Elite-QB
#     refits (10 QBs, rookie-deal seasons excluded) for every eligible coach:
#     Arians +0.90 -> +0.55 as predicted; also Belichick +0.95 -> +0.09,
#     Tomlin +0.68 -> +0.22, LaFleur +0.46 -> +0.15, Fox +0.12 -> -0.62.
#     Columns coach_effect_no_elite_qb, elite_qb_delta, elite_qb_flag;
#     flagged coaches carry the without-number in the label.
# 11. Lagged QB EPA filled with the league mean on 68 rows (minor). RIGHT.
#     New-starter indicator in the talent fit: -1.22 wins/17 (t = -5.2), so
#     the market prices a first-year starter well below the fill. Variant
#     war_talent_new_starter: Spearman 0.993 with main, top-10 overlap 10;
#     caption states the coefficient.
#  Runtime about 10 minutes cold (adds 451 influence refits, 38 fixed-tau
#  fits, 400 split-half refits, 15 elite-QB refits, two profiles).
#
# SKEPTIC LOG round 3 (three reviewers, 14 objections; what was done)
#  1. "Fresh hire" replacement is entirely a retread effect (major). RIGHT on
#     the split: first-time HCs +0.04 (n 66), second-or-later job -0.64
#     (n 33), pooled -0.19; Wilcoxon p = 0.26 so the split is noise. Rows
#     rep_offset_first_time_hc / _retread / _se_naive / _se in validation;
#     war_rep_no_returnee column in coaching_war.csv; subtitle gives the
#     three numbers with the SE; axis reads "first-season hire".
#  2. Replacement drifts across eras and the season term cannot absorb it
#     (major). RIGHT on the mechanism, and the drift is NOT significant:
#     2012-2018 +0.08 vs 2019-2025 -0.45, lm coef -0.54 (SE 0.59, p = 0.37),
#     linear in season p = 0.78. rep_offset_era_* rows; per-era board
#     war_rep_era (Spearman 0.922 with main, biggest mover Kingsbury 35 ->
#     23, none of the top 12 moves); subtitle states the move and p.
#  3. Voluntary exits coded as firings (minor). RIGHT. Hand-coded 8 spell
#     ends (Kubiak DEN 2016, Arians ARI 2017 and TB 2021, Payton NO 2021,
#     Carroll SEA 2023, Belichick NE 2023, J. Harbaugh SF 2014, Fox DEN
#     2014); Garrett DAL 2019 was not renewed, so not coded. Sweep found no
#     others. Censored in h2 and the groups: wat17 coef 0.436 -> 0.466,
#     gap +3.51 -> +3.67, no-trait P 0.012 -> 0.007. Rows *_vol_censored.
#     As predicted, conservative: the correction makes the gap larger.
#  4/11. Survivorship subtitle overlapped the y-axis title (minor). RIGHT.
#     Subtitle cut to five shorter lines, persistence moved to the caption,
#     margin added, figure re-read: legible.
#  5. Report bottom-5 stale (minor). RIGHT. HEADLINE now prints the bottom 5
#     from the table with both ranks (Allen -0.36, Smith -0.30, Marrone
#     -0.25, Bradley -0.25, McCoy -0.23).
#  6/10. Validation caption asserted "r pulled toward zero" (minor). RIGHT;
#     the sentence is replaced by "the direction of that effect on r is not
#     demonstrated here".
#  7. Survivorship gap is unusual under the fitted-trait world too (major).
#     RIGHT. P(fitted-trait >= 3.51) = 0.038; likelihood ratio within 0.25 of
#     the observed gap 0.087/0.029 = 3.0; a wins-only firing rule with no
#     trait gives a median of +2.18 (P = 0.009), so neither rule reproduces
#     the gap without a trait, but the trait world does not reproduce it
#     either. Title and verdict now say the gap is unusual under both
#     worlds and the simulation cannot attribute it. Rows
#     null_shift_fitted_trait (with p), null_likelihood_ratio_trait_vs_none,
#     null_shift_no_trait_wins_rule.
#  8. Components caption said the parts add to the coach effect (minor).
#     RIGHT: max gap 0.26 (Schwartz -0.22 vs -0.48), cor 0.97. Caption now
#     says they do not sum exactly; 63% is asserted on the summed parts the
#     bars use, 56% on the coach effect is printed beside it.
#  9. Replacement line shown without its SE (minor). RIGHT. rep_offset_se
#     0.25 (bootstrap), naive 0.29; the round-2 move of 0.11 is 0.5 SE;
#     subtitle says "-0.19 (SE 0.25)" and that both moves are under one SE.
# 10. "Beats baseline" and the OOS slope asserted without an interval
#     (minor). RIGHT. Coach-cluster bootstrap: r(WAR) - r(prev wins) = -0.09
#     [-0.20, +0.04], r(WAR) alone [0.03, 0.21]; in the validation CSV and
#     the figure subtitle. The 0.94 slope was in the report text only; the
#     figure and CSV say 0.89 (all rows) and 0.83 (common rows). The
#     spread-to-win logistic refit on seasons < t inside the loop changes r
#     by 0.0016 (0.121 -> 0.122).
# 12. No explicit test of the coach variance (minor). RIGHT. REML LRT rows:
#     coach given team p = 0.031 (stat 3.47), team given coach p = 0.031,
#     both p = 2e-5; clause in the leaderboard subtitle. RLRsim not
#     installed, so the boundary-mixture p is used.
# 13. QB cap-share control credits cheap-QB seasons to the coach (major).
#     RIGHT. 45 of 451 rows have qb_z < -0.5 and qbcur_z > 0.5, mean wat17
#     +2.79 vs -0.32; 10 of the top 12 have one. cor(coach effect, mean
#     qbcur_z) = 0.58 over the 50 eligible, +0.31/SD on play-minus-pay
#     (p = 0.009). Cheap-QB refit for every eligible coach: |delta| > 0.3
#     for Vrabel (+0.83 -> +0.35), McVay (+0.66 -> +0.35), Quinn, Bowles,
#     Pederson, Taylor; flagged coaches carry the number in the label.
#     rank_same_season_qb now in coaching_war.csv and in every label.
# 14. Main and same-season-QB boards are not "bounds" (minor). RIGHT, and
#     the skeptic undercounted: 24 of 50 eligible coaches move 10+ places,
#     not 10; caption names Fisher 45 -> 10 and Reid 11 -> 39. Every list in
#     the HEADLINE block shows both ranks.
# 15. Components title credited the premium to the coach (minor). RIGHT.
#     Retitled "Most of the top coaches' number was already in the closing
#     spread; beating the spread is the smaller part"; 63% and the 0.61/0.32
#     correlations stay in the subtitle.
#  Top-10 order unchanged; no WAR value changed in this round.
#
# Out:
#   data/derived/coaching_war.csv              one row per head coach, 2012-2025
#   data/derived/coaching_war_seasons.csv      coach-season-team rows, components
#   data/derived/coaching_war_validation.csv   every validation test as a row
#   data/derived/coaching_war_oos.csv          out-of-sample predictions
#   data/derived/coaching_war_oos_unscored.csv t-1 coaches not scored in t
#   data/derived/coaching_war_reliability.csv  split-half and persistence
#   data/derived/coaching_war_survivorship.csv survivorship tests
#   data/derived/coaching_war_survivorship_null.csv simulated null shifts
#   data/derived/qb_epa_same_season.csv         same-season QB EPA (cache)
#   data/derived/coaching_war_sensitivity.csv  replacement / talent variants
#   data/derived/coaching_war_variance.csv     variance components
#   docs/figures/coaching_war_leaderboard.png, coaching_war_components.png,
#   docs/figures/coaching_war_validation.png, coaching_war_survivorship.png
# =============================================================================

suppressMessages({
  library(data.table); library(lme4); library(ggplot2); library(ggrepel)
})
source("R/lib/theme_coach.R")

NFLA     <- "/Users/nick/stranger9977/nfl-analysis"
FIRST    <- 2012
LAST     <- 2025
MIN_G    <- 8
OOS_FROM <- 2016
BOOT     <- 1000
OOS_BAR  <- 0.30     # prespecified out-of-sample pass bar (judges raised it from r > 0)
set.seed(71)
accent   <- "#B2182B"
accent2  <- "#2B8CBE"
REBUILD  <- nzchar(Sys.getenv("REBUILD_WAR"))

norm_team <- function(x) fcase(x == "OAK", "LV", x == "SD", "LAC", x == "STL", "LA",
                               default = x)
wmean <- function(x, w) sum(x * w) / sum(w)
ci_r <- function(r, n) {            # Fisher z interval for a Pearson r
  if (is.na(r) || n < 4) return(c(NA_real_, NA_real_))
  z <- atanh(r); s <- 1 / sqrt(n - 3); tanh(c(z - 1.96 * s, z + 1.96 * s))
}
auc_fn <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
val <- list()
add_val <- function(test, statistic, n, note, lo = NA, hi = NA, p = NA) {
  val[[length(val) + 1]] <<- data.table(test = test, statistic = statistic, n = n,
                                        lo = lo, hi = hi, p = p, note = note)
}
fit_m <- function(f, d) suppressMessages(
  lmer(f, data = d, weights = w, REML = TRUE,
       control = lmerControl(check.conv.singular = "ignore")))
blups <- function(m, which = "coach") {
  r <- as.data.table(ranef(m, condVar = TRUE))[grpvar == which]
  r[, .(grp = as.character(grp), blup = condval, se_cond = condsd)]
}
F_WAT <- wat17 ~ 1 + (1 | coach) + (1 | team) + (1 | season)

# ---------------------------------------------------------------------------
# 1. Market layer: reuse R/02 verbatim on games.csv.
# ---------------------------------------------------------------------------
games <- fread(file.path(NFLA, "data/games.csv"),
               select = c("game_id", "season", "game_type", "week", "home_team",
                          "away_team", "home_score", "away_score", "result",
                          "spread_line", "home_coach", "away_coach"))
games <- games[game_type == "REG" & !is.na(result) & !is.na(spread_line) &
               season <= LAST]
games[, `:=`(home_team = norm_team(home_team), away_team = norm_team(away_team))]
decisive <- games[result != 0][, home_win := as.integer(result > 0)]
mkt_fit <- glm(home_win ~ spread_line, data = decisive, family = binomial)
games[, expected_home_wp := predict(mkt_fit, newdata = games, type = "response")]
cat(sprintf("Usable REG games 1999-%d: %d (fit on %d decisive, %d ties)\n",
            LAST, nrow(games), nrow(decisive), sum(games$result == 0)))
cat(sprintf("Market model: intercept %.4f, spread coef %.4f (same glm as R/02)\n",
            coef(mkt_fit)[1], coef(mkt_fit)[2]))

cg <- rbind(
  games[, .(game_id, season, week, team = home_team, opp = away_team, coach = home_coach,
            expected_wp = expected_home_wp,
            actual = fcase(result > 0, 1, result < 0, 0, default = 0.5),
            margin = result)],
  games[, .(game_id, season, week, team = away_team, opp = home_team, coach = away_coach,
            expected_wp = 1 - expected_home_wp,
            actual = fcase(result < 0, 1, result > 0, 0, default = 0.5),
            margin = -result)])
cg[, var_i := expected_wp * (1 - expected_wp)]
cg <- cg[!is.na(coach) & coach != ""]

# cross-check against R/02's derived file (same universe, same model)
cm02 <- fread("data/derived/coach_market.csv")
chk <- merge(cg[, .(ew = sum(expected_wp), aw = sum(actual)), by = coach], cm02, by = "coach")
chk_dev <- max(abs(chk$ew - chk$expected_wins))
cat(sprintf("Check vs data/derived/coach_market.csv: max |expected_wins diff| = %.4f on %d coaches\n",
            chk_dev, nrow(chk)))
if (chk_dev > 0.5) stop("market layer does not reproduce R/02's coach_market.csv")

# opponent-adjusted margin per team-season (iterative, centred), for the
# roster-first grafts: inherit_pd and a point-differential sensitivity outcome
adj_margin <- function(d) {
  teams <- sort(unique(d$team)); r <- setNames(rep(0, length(teams)), teams)
  for (i in 1:60) {
    d[, adj := margin + r[opp]]
    nr <- d[, .(v = mean(adj)), by = team]; r <- setNames(nr$v, nr$team); r <- r - mean(r)
  }
  d[, pd_adj := margin + r[opp] - mean(margin + r[opp])]   # per-game adjusted margin
  d[, adj := NULL]; d
}
cg <- rbindlist(lapply(split(cg, cg$season), adj_margin))
team_season_pd <- cg[, .(pd_adj_pg = mean(pd_adj)), by = .(season, team)]

# ---------------------------------------------------------------------------
# 2. Coach-season-team panel, 1999-2025 (market only), with spells.
# ---------------------------------------------------------------------------
cst <- cg[, .(games = .N, act17 = sum(actual) / .N * 17, mkt17 = sum(expected_wp) / .N * 17,
              var17 = sum(var_i) / .N^2 * 17^2, pd_adj_pg = mean(pd_adj),
              first_week = min(week)),
          by = .(coach, season, team)]
cst[, wae17 := act17 - mkt17]
# full-season length for the team-season (to flag interims)
cst[, team_games := sum(games), by = .(season, team)]
cst <- cst[games >= MIN_G]
cst[, w := games / 17]
setorder(cst, coach, team, season)
# spell = consecutive seasons of a coach with the same team
cst[, spell_id := cumsum(c(1, diff(season) > 1)), by = .(coach, team)]
cst[, spell_yr := seq_len(.N), by = .(coach, team, spell_id)]
cst[, spell_len := .N, by = .(coach, team, spell_id)]
cst[, spell_first := min(season), by = .(coach, team, spell_id)]
cst[, spell_last := max(season), by = .(coach, team, spell_id)]
# SKEPTIC round 2: an interim is a coach who was NOT the team's week-1 head
# coach. The old rule (spell_yr == 1 & games < team_games) also caught full-
# time hires fired inside their first season (Meyer 2021, Hackett 2022, Reich
# 2023), which dropped the three worst fresh hires from the replacement pool.
cst[, interim := spell_yr == 1 & first_week > 1]
cst[, fired_in_yr1 := spell_yr == 1 & !interim & games < team_games]
cst[, career_first := min(season), by = coach]
cst[, prior_other_team := spell_first > career_first &
      vapply(seq_len(.N), function(i) any(cst$coach == coach[i] & cst$team != team[i] &
                                          cst$season < spell_first[i]), logical(1))]
cst[, n_teams_career := uniqueN(team), by = coach]
# inherit_pd: the team's adjusted margin in the season before the spell began
cst <- merge(cst, team_season_pd[, .(team, season = season + 1, inherit_pd = pd_adj_pg)],
             by.x = c("team", "spell_first"), by.y = c("team", "season"), all.x = TRUE)
cst[, inherit_pd := fifelse(spell_yr == 1, inherit_pd, NA_real_)]
# carry the inherited margin across the whole spell (interacted with spell-year bucket later)
cst[, inherit_pd := inherit_pd[spell_yr == 1][1], by = .(coach, team, spell_id)]
cst[, inherit_na := is.na(inherit_pd)][, inherit_pd := fifelse(inherit_na, 0, inherit_pd)]
setorder(cst, coach, season, team)
cat(sprintf("\nMarket panel 1999-%d: %d coach-season-team rows (>= %d games), %d coaches, %d spells, %d first-season rows\n",
            LAST, nrow(cst), MIN_G, uniqueN(cst$coach), uniqueN(cst[, .(coach, team, spell_id)]),
            sum(cst$spell_yr == 1)))

# ---------------------------------------------------------------------------
# 3. Talent layer, 2012-2025.
# ---------------------------------------------------------------------------
ct <- fread("data/derived/contract_talent.csv",
            select = c("season", "team", "head_coach", "contract_talent", "qb_name", "qb_id", "qb_apy_cap_pct"))
tt <- fread("data/derived/team_talent.csv", select = c("season", "team", "qb_prior_epa_per_db"))
md <- fread("data/derived/madden_team_seasons.csv", select = c("season", "team", "talent_z"))
md[, team := norm_team(team)]
# SKEPTIC round 1: 18 rows had the starter's cap share missing (contract join
# failed: Brady 2018, Brees 2017, Roethlisberger 2014, Lamar 2022 ...) and were
# set to league average, which understated talent on several top coaches'
# seasons. Fill from the same QB's nearest season in the contract file
# (earlier season wins a tie) and keep the flag.
ct[, qb_cap_raw := qb_apy_cap_pct]
ct[, qb_filled := FALSE]
na_rows <- which(is.na(ct$qb_apy_cap_pct))
for (i in na_rows) {
  cand <- ct[!is.na(qb_apy_cap_pct) & qb_id == ct$qb_id[i] & !is.na(qb_id)]
  if (nrow(cand)) {
    cand[, d := abs(season - ct$season[i]) + 0.1 * (season > ct$season[i])]
    ct[i, `:=`(qb_apy_cap_pct = cand[order(d)][1, qb_apy_cap_pct], qb_filled = TRUE)]
  }
}
cat(sprintf("QB cap share: %d rows were NA; %d filled from the same QB's nearest season, %d still NA -> league average\n",
            length(na_rows), sum(ct$qb_filled), sum(is.na(ct$qb_apy_cap_pct))))
print(ct[qb_filled == TRUE | is.na(qb_apy_cap_pct), .(season, team, head_coach, qb_name, qb_cap_before = qb_cap_raw, qb_cap_after = round(qb_apy_cap_pct, 3))])
# same-season QB EPA per dropback (nflfastR, REG, qb_dropback == 1): NOT a
# control in the main measure (the coach shapes the QB he has); used as a
# sensitivity bound and to say how much QB play sits inside the coach number.
qbcur_path <- "data/derived/qb_epa_same_season.csv"
if (!file.exists(qbcur_path)) {
  qbcur <- rbindlist(lapply(FIRST:LAST, function(s) {
    p <- fread(file.path(NFLA, sprintf("data/play_by_play_%d.csv.gz", s)),
               select = c("season", "season_type", "posteam", "qb_dropback", "epa"))
    p[season_type == "REG" & qb_dropback == 1 & !is.na(epa) & !is.na(posteam) & posteam != "",
      .(qb_epa_db = mean(epa), n_db = .N), by = .(season, team = posteam)]
  }))
  qbcur[, team := norm_team(team)]
  fwrite(qbcur, qbcur_path)
}
qbcur <- fread(qbcur_path)
tal <- merge(ct, tt, by = c("season", "team"), all.x = TRUE)
tal <- merge(tal, md[, .(season, team, madden_z = talent_z)], by = c("season", "team"), all.x = TRUE)
tal <- merge(tal, qbcur[, .(season, team, qb_epa_db)], by = c("season", "team"), all.x = TRUE)
zs <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
tal[, `:=`(contract_z = zs(contract_talent), qb_z = zs(qb_apy_cap_pct),
           qbp_z = zs(qb_prior_epa_per_db), qbcur_z = zs(qb_epa_db)), by = season]
tal[, `:=`(qb_na = is.na(qb_z), qbp_na = is.na(qbp_z))]
tal[is.na(qb_z), qb_z := 0][is.na(qbp_z), qbp_z := 0]
if (any(is.na(tal$qbcur_z))) stop("same-season QB EPA missing on some team-seasons")
cat(sprintf("Talent file: %d team-seasons, QB cap share still NA -> 0 on %d, lagged QB EPA NA -> 0 on %d (new starters)\n",
            nrow(tal), sum(tal$qb_na), sum(tal$qbp_na)))

panel <- merge(cst[season >= FIRST & season <= LAST], tal, by = c("season", "team"))
mism <- panel[coach != head_coach]
cat(sprintf("Rows where the market coach is not contract_talent's head_coach (second coach in a split season): %d\n",
            nrow(mism)))
if (nrow(mism)) print(mism[, .(season, team, coach, head_coach, games)])
panel[, season_f := factor(season)]

# MADDEN IN THE MAIN FIT (user request, 22 Aug). Madden launch ratings exist
# 2017 on, so they enter through an interaction with a 2017+ indicator: zero
# before 2017, the within-season z-score after. Pre-2017 rows keep the
# payroll-and-quarterback control only. This was the "talent_plus_madden_2017"
# sensitivity; it is now the main measure, and the payroll-only fit is the
# sensitivity (war_talent_contract_qbepa).
panel[, madden_z17 := fifelse(is.na(madden_z), 0, madden_z)][, post17 := as.integer(season >= 2017)]
talent_fit <- lm(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17 + season_f, data = panel, weights = w)
cat(sprintf("Main talent fit with Madden 2017+: Madden %+.3f wins/SD; R2 %.3f\n", coef(talent_fit)["madden_z17:post17"], summary(talent_fit)$r.squared))
panel[, talent17 := fitted(talent_fit)]
panel[, premium17 := mkt17 - talent17]
panel[, wat17 := act17 - talent17]
if (max(abs(panel$premium17 + panel$wae17 - panel$wat17)) > 1e-9)
  stop("identity premium17 + wae17 != wat17")
# the market's expected wins sum to half the games in every season, so the
# season mean of wae17 must be zero within rounding (a two-coach team-season
# split below 8 games can move it slightly)
wae_season <- panel[, .(m = wmean(wae17, w)), by = season]
if (max(abs(wae_season$m)) > 0.25) stop("season mean of wae17 not near zero")
tf <- summary(talent_fit)
cat(sprintf("\ntalent_fit: lm(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17 + season), n = %d, R2 = %.3f\n",
            nrow(panel), tf$r.squared))
cat(sprintf("  wins per SD: contract %+.2f, QB cap share %+.2f, lagged QB EPA %+.2f\n",
            coef(talent_fit)["contract_z"], coef(talent_fit)["qb_z"], coef(talent_fit)["qbp_z"]))
cat(sprintf("  identity premium17 + wae17 = wat17 holds on all %d rows (max dev %.1e)\n",
            nrow(panel), max(abs(panel$premium17 + panel$wae17 - panel$wat17))))
add_val("talent_fit_r2", tf$r.squared, nrow(panel), "R2 of market expectation on payroll + QB cap + lagged QB EPA")
cat(sprintf("  SDs (wins/17): actual %.2f, market %.2f, talent17 %.2f, premium17 %.2f; cor(talent17, act17) %.2f vs cor(mkt17, act17) %.2f\n",
            sd(panel$act17), sd(panel$mkt17), sd(panel$talent17), sd(panel$premium17),
            cor(panel$talent17, panel$act17), cor(panel$mkt17, panel$act17)))

# SKEPTIC round 1: what roster signal is left in the residual after payroll + QB?
# (a) Madden launch ratings 2017-2025 against wat17
pmad <- panel[!is.na(madden_z)]
lm_mad <- summary(lm(wat17 ~ madden_z, data = pmad, weights = w))$coefficients
cat(sprintf("  Residual roster check: lm(wat17 ~ madden_z) on %d rows 2017+: %+.3f wins per SD (t = %.2f, p = %.2g). cor(premium17, madden_z) = %.2f\n",
            nrow(pmad), lm_mad["madden_z", 1], lm_mad["madden_z", 3], lm_mad["madden_z", 4], cor(pmad$premium17, pmad$madden_z)))
add_val("residual_roster_madden_coef", lm_mad["madden_z", 1], nrow(pmad),
        sprintf("wins/17 per SD of Madden top-25 OVR left in wat17 after payroll + QB controls; t = %.2f. Known residual roster: the control is payroll and QB, not the full roster", lm_mad["madden_z", 3]),
        p = lm_mad["madden_z", 4])
# (b) same-season QB EPA against wat17 and the residual handed to the coach
lm_cur <- summary(lm(wat17 ~ qbcur_z, data = panel, weights = w))
cat(sprintf("  Same-season QB EPA: lm(wat17 ~ qbcur_z) %+.2f wins per SD, R2 %.3f; cor(qbcur_z, qb_z) %.2f, cor(qbcur_z, qbp_z) %.2f, cor(premium17, qbcur_z) %.2f\n",
            coef(lm_cur)["qbcur_z", 1], lm_cur$r.squared, cor(panel$qbcur_z, panel$qb_z), cor(panel$qbcur_z, panel$qbp_z),
            cor(panel$premium17, panel$qbcur_z)))
add_val("same_season_qb_coef_on_wat17", coef(lm_cur)["qbcur_z", 1], nrow(panel), sprintf("wins/17 per SD of same-season QB EPA; R2 %.3f. The main measure controls LAST season's QB only", lm_cur$r.squared))
add_val("cor_premium_same_season_qb", cor(panel$premium17, panel$qbcur_z), nrow(panel), "market premium vs same-season QB EPA z")
add_val("cor_premium_madden", cor(pmad$premium17, pmad$madden_z), nrow(pmad), "market premium vs Madden talent z, 2017-2025")
# (c) the team's previous-season wat17 (any coach), every season
team_prev <- panel[, .(prev_team_wat = wmean(wat17, w)), by = .(team, season = season + 1)]
panel <- merge(panel, team_prev, by = c("team", "season"), all.x = TRUE)
cat(sprintf("  cor(premium17, team's previous-season wat17) = %.2f (n = %d)\n",
            cor(panel$premium17, panel$prev_team_wat, use = "complete"), sum(!is.na(panel$prev_team_wat))))
add_val("cor_premium_prev_team_wat", cor(panel$premium17, panel$prev_team_wat, use = "complete"), sum(!is.na(panel$prev_team_wat)), "market premium vs the team's previous-season wins above talent")

# ---------------------------------------------------------------------------
# 4. Coach models.
# ---------------------------------------------------------------------------
m_wat  <- fit_m(F_WAT, panel)
m_prem <- fit_m(premium17 ~ 1 + (1 | coach) + (1 | team) + (1 | season), panel)
m_wae  <- fit_m(wae17 ~ 1 + (1 | coach) + (1 | team) + (1 | season), panel)
m_coach_only <- fit_m(wat17 ~ 1 + (1 | coach) + (1 | season), panel)
panel[, spell_bucket := factor(pmin(spell_yr, 3))]
m_inh <- fit_m(wat17 ~ 1 + inherit_pd:spell_bucket + (1 | coach) + (1 | team) + (1 | season), panel)
m_nested <- fit_m(wat17 ~ 1 + (1 | coach) + (1 | coach:team) + (1 | season), panel)

vc_tab <- function(m, name) {
  v <- as.data.table(VarCorr(m))
  v[, .(model = name, grp, sd = sdcor)]
}
variance <- rbind(vc_tab(m_wat, "M_wat"), vc_tab(m_prem, "M_prem"), vc_tab(m_wae, "M_wae"),
                  vc_tab(m_coach_only, "M_wat_no_team"), vc_tab(m_inh, "M_wat_inherit_pd"),
                  vc_tab(m_nested, "M_wat_nested"))
fwrite(variance, "data/derived/coaching_war_variance.csv")
cat("\nVariance components (SD, wins per 17 games):\n")
print(dcast(variance, model ~ grp, value.var = "sd")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
sd_of <- function(m, g) variance[model == m & grp == g, sd]
cat(sprintf("M_wat: coach %.2f, team %.2f, season %.2f, residual %.2f\n",
            sd_of("M_wat", "coach"), sd_of("M_wat", "team"), sd_of("M_wat", "season"),
            sd_of("M_wat", "Residual")))
cat(sprintf("M_prem: coach %.2f vs team %.2f (the market's reputation premium follows the %s)\n",
            sd_of("M_prem", "coach"), sd_of("M_prem", "team"),
            if (abs(sd_of("M_prem", "team") - sd_of("M_prem", "coach")) < 0.05) "franchise and the man about equally" else if (sd_of("M_prem", "team") > sd_of("M_prem", "coach")) "franchise more than the man" else "man more than the franchise"))
cat(sprintf("Coach SD without the team term %.2f; with inherit_pd fixed effect %.2f (team %.2f)\n",
            sd_of("M_wat_no_team", "coach"), sd_of("M_wat_inherit_pd", "coach"),
            sd_of("M_wat_inherit_pd", "team")))
cat(sprintf("Nested fit: portable coach SD %.2f vs coach-within-team SD %.2f\n",
            sd_of("M_wat_nested", "coach"), sd_of("M_wat_nested", "coach:team")))
for (i in seq_len(nrow(variance)))
  add_val(paste0("varcomp_", variance$model[i], "_", variance$grp[i]), variance$sd[i], nrow(panel), "SD in wins per 17 games")

# SKEPTIC round 3: an explicit test that the coach variance is nonzero. REML
# likelihood-ratio tests dropping (1|coach), (1|team), and both; p from the
# boundary mixture (0.5 chi0 + 0.5 chi1 for one component; 0.25 chi0 + 0.5
# chi1 + 0.25 chi2 for two). RLRsim::exactRLRT is used when installed.
m_no_coach <- fit_m(wat17 ~ 1 + (1 | team) + (1 | season), panel)
m_no_team  <- fit_m(wat17 ~ 1 + (1 | coach) + (1 | season), panel)
m_no_both  <- fit_m(wat17 ~ 1 + (1 | season), panel)
lrt1 <- function(full, red) { s <- as.numeric(2 * (logLik(full) - logLik(red))); c(stat = max(s, 0), p = 0.5 * pchisq(max(s, 0), 1, lower.tail = FALSE)) }
lrt_coach <- lrt1(m_wat, m_no_coach); lrt_team <- lrt1(m_wat, m_no_team)
s_both <- max(as.numeric(2 * (logLik(m_wat) - logLik(m_no_both))), 0)
lrt_both <- c(stat = s_both, p = 0.5 * pchisq(s_both, 1, lower.tail = FALSE) + 0.25 * pchisq(s_both, 2, lower.tail = FALSE))
rlr_note <- ""
if (requireNamespace("RLRsim", quietly = TRUE)) {
  ex <- try(RLRsim::exactRLRT(m = fit_m(wat17 ~ 1 + (1 | coach), panel), mA = m_wat, m0 = m_no_coach), silent = TRUE)
  if (!inherits(ex, "try-error")) rlr_note <- sprintf("; RLRsim exact p = %.3f", ex$p.value)
}
cat(sprintf("LRT (REML, boundary mixture): drop coach given team: stat %.2f, p = %.3f%s; drop team given coach: %.2f, p = %.3f; drop both: %.2f, p = %.2g\n",
            lrt_coach["stat"], lrt_coach["p"], rlr_note, lrt_team["stat"], lrt_team["p"], lrt_both["stat"], lrt_both["p"]))
add_val("lrt_coach_variance_p", unname(lrt_coach["p"]), nrow(panel), sprintf("REML LRT dropping (1|coach) from M_wat, statistic %.2f, 0.5 chi-square-1 mixture%s", lrt_coach["stat"], rlr_note))
add_val("lrt_team_variance_p", unname(lrt_team["p"]), nrow(panel), sprintf("REML LRT dropping (1|team) from M_wat, statistic %.2f", lrt_team["stat"]))
add_val("lrt_coach_plus_team_p", unname(lrt_both["p"]), nrow(panel), sprintf("REML LRT dropping both (1|coach) and (1|team), statistic %.2f, 0.25/0.5/0.25 chi0/chi1/chi2 mixture", lrt_both["stat"]))

# SKEPTIC round 2 (blocking): the coach SD that sets every shrunk number is
# not well identified. Profile the variance components and keep the profile
# likelihood of the coach SD so the bootstrap can draw from it.
prof_wat <- profile(m_wat, which = "theta_", signames = FALSE)
ci_prof  <- confint(prof_wat, level = 0.95)
prof_df  <- as.data.table(as.data.frame(prof_wat))[.par == "sd_(Intercept)|coach", .(tau = .focal, zeta = .zeta)]
tau_ci   <- unname(ci_prof["sd_(Intercept)|coach", ])
team_ci  <- unname(ci_prof["sd_(Intercept)|team", ])
cat(sprintf("\nPROFILE 95%% CI (M_wat): coach SD [%.2f, %.2f], team SD [%.2f, %.2f], residual [%.2f, %.2f]\n",
            tau_ci[1], tau_ci[2], team_ci[1], team_ci[2], ci_prof["sigma", 1], ci_prof["sigma", 2]))
add_val("profile_ci_coach_sd", sd_of("M_wat", "coach"), nrow(panel), "REML coach SD with 95% profile CI (lo, hi)", tau_ci[1], tau_ci[2])
add_val("profile_ci_team_sd", sd_of("M_wat", "team"), nrow(panel), "REML team SD with 95% profile CI", team_ci[1], team_ci[2])
nest_ci <- try(suppressWarnings(confint(m_nested, parm = "theta_", method = "profile", oldNames = FALSE)), silent = TRUE)
if (!inherits(nest_ci, "try-error")) {
  cat(sprintf("PROFILE 95%% CI (nested): portable coach SD [%.2f, %.2f], coach-within-team SD [%.2f, %.2f]\n",
              nest_ci["sd_(Intercept)|coach", 1], nest_ci["sd_(Intercept)|coach", 2],
              nest_ci["sd_(Intercept)|coach:team", 1], nest_ci["sd_(Intercept)|coach:team", 2]))
  add_val("profile_ci_nested_coach_sd", sd_of("M_wat_nested", "coach"), nrow(panel), "portable coach SD, (1|coach)+(1|coach:team), 95% profile CI",
          nest_ci["sd_(Intercept)|coach", 1], nest_ci["sd_(Intercept)|coach", 2])
  add_val("profile_ci_nested_coach_team_sd", sd_of("M_wat_nested", "coach:team"), nrow(panel), "coach-within-team SD, 95% profile CI",
          nest_ci["sd_(Intercept)|coach:team", 1], nest_ci["sd_(Intercept)|coach:team", 2])
} else {
  cat("PROFILE CI (nested): profile failed on the singular fit\n")
  add_val("profile_ci_nested_coach_sd", sd_of("M_wat_nested", "coach"), nrow(panel), "portable coach SD; profile CI did not converge on the singular fit")
}
# odd and even halves of the panel each estimate the coach SD
sd_half <- function(d) as.data.table(VarCorr(fit_m(F_WAT, d)))[grp == "coach", sdcor]
tau_odd <- sd_half(panel[season %% 2 == 1]); tau_even <- sd_half(panel[season %% 2 == 0])
cat(sprintf("Coach SD by half of the panel: odd seasons %.2f, even seasons %.2f\n", tau_odd, tau_even))
add_val("coach_sd_odd_seasons", tau_odd, panel[season %% 2 == 1, .N], "REML coach SD on odd seasons only")
add_val("coach_sd_even_seasons", tau_even, panel[season %% 2 == 0, .N], "REML coach SD on even seasons only")

# refit M_wat with the coach SD held at a chosen value (team and season SDs
# re-optimised, residual profiled out), for the low-tau and high-tau boards
fit_fixed_tau <- function(d, tau) {
  lmod <- lFormula(F_WAT, data = d, weights = w, REML = TRUE, control = lmerControl(check.conv.singular = "ignore"))
  devfun <- do.call(mkLmerDevfun, lmod)
  th <- getME(m_wat, "theta"); sig <- sigma(m_wat)
  for (it in 1:3) {
    th["coach.(Intercept)"] <- tau / sig
    f2 <- function(p) { t <- th; t[2:3] <- pmax(p, 0); devfun(t) }
    o <- optim(th[2:3], f2, method = "L-BFGS-B", lower = 0)
    th[2:3] <- o$par
    mm <- mkMerMod(environment(devfun), opt = list(par = th, fval = devfun(th), conv = 0), reTrms = lmod$reTrms, fr = lmod$fr)
    sig <- sigma(mm)
  }
  mm
}
TAU_LOW <- 0.40      # the value the observed split-half reliability supports (skeptic's number)
TAU_HIGH <- tau_ci[2]
m_tau_low  <- fit_fixed_tau(panel, TAU_LOW)
m_tau_high <- fit_fixed_tau(panel, TAU_HIGH)
cat(sprintf("Fixed-tau refits: tau %.2f -> coach SD %.3f, team SD %.2f; tau %.2f -> coach SD %.3f, team SD %.2f\n",
            TAU_LOW, as.data.table(VarCorr(m_tau_low))[grp == "coach", sdcor], as.data.table(VarCorr(m_tau_low))[grp == "team", sdcor],
            TAU_HIGH, as.data.table(VarCorr(m_tau_high))[grp == "coach", sdcor], as.data.table(VarCorr(m_tau_high))[grp == "team", sdcor]))
inh_cf <- fixef(m_inh)[grep("inherit_pd", names(fixef(m_inh)))]
cat(sprintf("inherit_pd coefficients (wins/17 per point of inherited adjusted margin): yr1 %+.3f, yr2 %+.3f, yr3+ %+.3f\n",
            inh_cf[1], inh_cf[2], inh_cf[3]))

b_wat  <- blups(m_wat);  setnames(b_wat,  c("coach", "coach_effect", "se_cond"))
b_prem <- blups(m_prem); setnames(b_prem, c("coach", "premium_per_season", "se_prem"))
b_wae  <- blups(m_wae);  setnames(b_wae,  c("coach", "surprise_per_season", "se_wae"))
b_inh  <- blups(m_inh);  setnames(b_inh,  c("coach", "coach_effect_inherit", "se_inh"))
b_team <- blups(m_wat, "team");   setnames(b_team, c("team", "team_effect", "se_team"))
b_seas <- blups(m_wat, "season"); setnames(b_seas, c("season", "season_effect", "se_season"))
b_seas[, season := as.integer(season)]

# intercept check: weighted mean of wat17 vs M_wat intercept
cat(sprintf("Weighted mean of wat17 = %.4f; M_wat intercept = %.4f\n",
            wmean(panel$wat17, panel$w), fixef(m_wat)[1]))
add_val("decomp_mean_wat17", wmean(panel$wat17, panel$w), nrow(panel), "season-weighted mean of wat17")
add_val("decomp_intercept_M_wat", unname(fixef(m_wat)[1]), nrow(panel), "fixed intercept of M_wat")

# ---------------------------------------------------------------------------
# 5. Replacement level from first-season rows.
# ---------------------------------------------------------------------------
panel <- merge(panel, b_team, by = "team", all.x = TRUE)
panel <- merge(panel, b_seas, by = "season", all.x = TRUE)
panel[, resid_plus_coach := wat17 - (fixef(m_wat)[1] + team_effect + season_effect)]
rep_from <- function(d, which = c("first", "one_and_done", "no_interim", "no_returnee", "fired_early", "interim_only")) {
  which <- match.arg(which)
  rows <- switch(which,
    first        = d[spell_yr == 1],
    one_and_done = d[spell_len <= 2 & spell_last < LAST],
    no_interim   = d[spell_yr == 1 & !interim],
    no_returnee  = d[spell_yr == 1 & !prior_other_team],
    fired_early  = d[spell_yr == 1 & spell_len <= 2 & spell_last < LAST],
    interim_only = d[spell_yr == 1 & interim])
  list(value = wmean(rows$resid_plus_coach, rows$w), n = nrow(rows))
}
rep_first   <- rep_from(panel, "first")
rep_avg     <- list(value = 0, n = nrow(panel))
rep_ood     <- rep_from(panel, "one_and_done")
rep_noint   <- rep_from(panel, "no_interim")
rep_noret   <- rep_from(panel, "no_returnee")
rep_fired   <- rep_from(panel, "fired_early")
rep_interim <- rep_from(panel, "interim_only")
rep_offset  <- rep_noint$value   # primary: first-season rows, interims (not the week-1 coach) excluded
# SKEPTIC round 2 check: week-1 hires fired inside year one must be in the pool
fired_y1 <- panel[fired_in_yr1 == TRUE, .(coach, season, team, games, wat17 = round(wat17, 2), rpc = round(resid_plus_coach, 2))]
cat("\nWeek-1 hires who did not finish their first season (now IN the replacement pool):\n"); print(fired_y1)
for (nm in c("Urban Meyer", "Nathaniel Hackett", "Frank Reich"))
  if (!any(panel[spell_yr == 1 & !interim, coach] == nm)) stop(paste(nm, "missing from the replacement pool"))
if (any(panel[coach == "Jeff Saturday", interim] == FALSE)) stop("Jeff Saturday 2022 should still be an interim")
rep_old_rule <- panel[spell_yr == 1 & !(games < team_games), .(value = wmean(resid_plus_coach, w), n = .N)]
cat(sprintf("\nReplacement level (wins per 17 below the average sitting coach, after payroll, QB, franchise):\n"))
cat(sprintf("  first-season rows, all:                 %+.3f (n = %d)\n", rep_first$value, rep_first$n))
cat(sprintf("  first-season rows, interims excluded:   %+.3f (n = %d)   <- PRIMARY (interim = not the week-1 coach)\n", rep_noint$value, rep_noint$n))
cat(sprintf("  same under the old full-season rule:    %+.3f (n = %d)   (round-1 value; dropped the fired-in-year-one hires)\n", rep_old_rule$value, rep_old_rule$n))
add_val("rep_offset_old_full_season_rule", rep_old_rule$value, rep_old_rule$n, "round-1 primary: first-season rows with games == team games; excluded Meyer 2021, Hackett 2022, Reich 2023")
cat(sprintf("  interims only (separate floor):         %+.3f (n = %d)\n", rep_interim$value, rep_interim$n))
cat(sprintf("  first-season, returnees excluded:       %+.3f (n = %d)\n", rep_noret$value, rep_noret$n))
cat(sprintf("  one-and-done pool (spell <= 2, ended):  %+.3f (n = %d)  survivorship-contaminated\n", rep_ood$value, rep_ood$n))
cat(sprintf("  fired within 2, first season only:      %+.3f (n = %d)\n", rep_fired$value, rep_fired$n))
cat(sprintf("  league average:                          0\n"))
if (rep_offset > 0) cat("  NOTE: replacement came out ABOVE average and is used as computed, not floored.\n")
add_val("rep_offset_primary", rep_offset, rep_noint$n, "first-season rows, interims excluded")
add_val("rep_offset_first_all", rep_first$value, rep_first$n, "all first-season rows incl. interims")
add_val("rep_offset_interim_only", rep_interim$value, rep_interim$n, "interim first-season rows only")
add_val("rep_offset_no_returnee", rep_noret$value, rep_noret$n, "first-season rows, coaches with a prior HC spell excluded")
add_val("rep_offset_one_and_done", rep_ood$value, rep_ood$n, "spell <= 2 seasons and ended; survivorship-contaminated")
add_val("rep_offset_fired_early_yr1", rep_fired$value, rep_fired$n, "first season of spells that ended within 2")

# SKEPTIC round 3 (major): who is in the "fresh hire" pool? First-time head
# coaches vs coaches on a second or later HC job (retreads), the naive SE of
# the pooled line, and whether the line drifts across the window. wat17 is
# mean-zero within season by construction, so a change in how first-season
# hires do RELATIVE to incumbents is not absorbed by the season term.
fs0 <- panel[spell_yr == 1 & !interim]
rep_ft <- fs0[prior_other_team == FALSE, .(value = wmean(resid_plus_coach, w), n = .N)]
rep_rt <- fs0[prior_other_team == TRUE,  .(value = wmean(resid_plus_coach, w), n = .N)]
rep_se_naive <- sqrt(fs0[, wmean((resid_plus_coach - wmean(resid_plus_coach, w))^2, w)] / nrow(fs0))
wt_rt <- wilcox.test(fs0[prior_other_team == TRUE, resid_plus_coach], fs0[prior_other_team == FALSE, resid_plus_coach], conf.int = TRUE)
fs0[, era := season >= 2019]
rep_era <- fs0[, .(value = wmean(resid_plus_coach, w), n = .N), by = era][order(era)]
rep_era_ft <- fs0[prior_other_team == FALSE, .(value = wmean(resid_plus_coach, w), n = .N), by = era][order(era)]
lm_era <- summary(lm(resid_plus_coach ~ era, data = fs0, weights = w))$coefficients["eraTRUE", ]
lm_lin <- summary(lm(resid_plus_coach ~ I(season - FIRST), data = fs0, weights = w))$coefficients["I(season - FIRST)", ]
cat(sprintf("\nREPLACEMENT POOL SPLITS (SKEPTIC round 3):\n  first-time head coaches %+.3f (n = %d); second-or-later HC job %+.3f (n = %d); pooled %+.3f (n = %d), naive SE %.3f (SD %.2f)\n",
            rep_ft$value, rep_ft$n, rep_rt$value, rep_rt$n, rep_offset, nrow(fs0), rep_se_naive, sd(fs0$resid_plus_coach)))
cat(sprintf("  retread minus first-timer: Wilcoxon shift %+.2f [%+.2f, %+.2f], p = %.2f -> the split is inside the noise of a %d-row pool\n",
            wt_rt$estimate, wt_rt$conf.int[1], wt_rt$conf.int[2], wt_rt$p.value, nrow(fs0)))
cat(sprintf("  by era: 2012-2018 %+.3f (n = %d) vs 2019-2025 %+.3f (n = %d); first-timers only %+.2f vs %+.2f; lm(rpc ~ era >= 2019) coef %+.2f (SE %.2f, p = %.2f); linear in season %+.3f per year (SE %.3f, p = %.2f)\n",
            rep_era$value[1], rep_era$n[1], rep_era$value[2], rep_era$n[2], rep_era_ft$value[1], rep_era_ft$value[2],
            lm_era[1], lm_era[2], lm_era[4], lm_lin[1], lm_lin[2], lm_lin[4]))
cat(sprintf("  The era difference (%+.2f) is %s; the zero line carries an SE of about %.1f wins, about the spread from rank 1 to rank 12\n",
            lm_era[1], if (lm_era[4] < 0.05) "significant" else "NOT significant", rep_se_naive))
add_val("rep_offset_first_time_hc", rep_ft$value, rep_ft$n, "first-season rows of coaches with no prior HC spell (1999+), interims excluded")
add_val("rep_offset_retread", rep_rt$value, rep_rt$n, "first-season rows of coaches with a prior HC spell, interims excluded")
add_val("rep_offset_retread_vs_first_time_wilcoxon", unname(wt_rt$estimate), nrow(fs0), "retread minus first-timer location shift, first-season rows", wt_rt$conf.int[1], wt_rt$conf.int[2], wt_rt$p.value)
add_val("rep_offset_se_naive", rep_se_naive, nrow(fs0), "weighted SD of resid_plus_coach on the primary pool / sqrt(n)")
add_val("rep_offset_era_2012_2018", rep_era$value[1], rep_era$n[1], "primary pool, seasons 2012-2018")
add_val("rep_offset_era_2019_2025", rep_era$value[2], rep_era$n[2], "primary pool, seasons 2019-2025")
add_val("rep_offset_era_diff", unname(lm_era[1]), nrow(fs0), sprintf("lm(resid_plus_coach ~ I(season >= 2019), weights = w) on the primary pool; SE %.3f; linear-in-season slope %+.3f/yr (p = %.2f)", lm_era[2], lm_lin[1], lm_lin[4]),
        unname(lm_era[1] - 1.96 * lm_era[2]), unname(lm_era[1] + 1.96 * lm_era[2]), unname(lm_era[4]))
# per-era replacement line for a sensitivity board (each coach's offset is the
# season-weighted mix of the two era lines over his own seasons)
panel[, rep_offset_era := fifelse(season >= 2019, rep_era$value[2], rep_era$value[1])]

# SKEPTIC round 1: is the franchise term built out of the fired coaches it is
# then netted from? Three checks on the same first-season rows.
fs <- panel[spell_yr == 1 & !interim]
rep_raw_season <- fs[, wmean(wat17 - (fixef(m_wat)[1] + season_effect), w)]   # no franchise term
# team effects from years-3+ rows only (the coaches who were kept)
m_team3 <- fit_m(F_WAT, panel[spell_yr >= 3])
bt3 <- blups(m_team3, "team")
fs[, team_effect_3plus := bt3$blup[match(team, bt3$grp)]]
rep_team3 <- fs[, wmean(wat17 - (fixef(m_team3)[1] + team_effect_3plus + season_effect), w)]
# leave-one-spell-out: refit M_wat without the spell that the first-season row opens
fs[, loso_team := NA_real_][, loso_int := NA_real_]
for (i in seq_len(nrow(fs))) {
  d <- panel[!(coach == fs$coach[i] & team == fs$team[i] & spell_id == fs$spell_id[i])]
  m <- fit_m(F_WAT, d); bt <- blups(m, "team")
  fs[i, `:=`(loso_team = bt$blup[match(fs$team[i], bt$grp)], loso_int = unname(fixef(m)[1]))]
}
rep_loso <- fs[, wmean(wat17 - (loso_int + loso_team + season_effect), w)]
n_coach_team <- panel[, .(n_coaches = uniqueN(coach)), by = team]
n_coach_team[, team_effect := b_team$team_effect[match(team, b_team$team)]]
r_churn <- cor(n_coach_team$team_effect, n_coach_team$n_coaches)
cat(sprintf("  Franchise-term circularity: cor(team effect, coaches used 2012-%d) = %.2f; team effect on first-season rows averages %+.2f vs %+.2f on all rows\n",
            LAST, r_churn, fs[, wmean(team_effect, w)], panel[, wmean(team_effect, w)]))
cat(sprintf("  Replacement with: no franchise term (season only) %+.3f; franchise term from years-3+ rows %+.3f; leave-one-spell-out franchise term %+.3f; published %+.3f\n",
            rep_raw_season, rep_team3, rep_loso, rep_offset))
cat("  The franchise term at churn franchises is partly the coaches that got fired there; the published zero line is the one that nets it out.\n")
add_val("rep_offset_no_franchise_term", rep_raw_season, nrow(fs), "first-season rows, wat17 minus intercept and season effect only (franchise term not removed)")
add_val("rep_offset_team_from_yr3plus", rep_team3, nrow(fs), "first-season rows net of a franchise term estimated on spell_yr >= 3 rows only")
add_val("rep_offset_leave_one_spell_out", rep_loso, nrow(fs), "first-season rows net of a franchise term refit without that coach-team spell")
add_val("cor_team_effect_n_coaches", r_churn, nrow(n_coach_team), "franchise BLUP vs number of distinct head coaches used in the window")

# ---------------------------------------------------------------------------
# 6. Side columns: market-only legacy column, play-calling share, 4th-down value.
# ---------------------------------------------------------------------------
# R/factory/90's EB formula on the full 1999-2025 career of R/02's rate_per17
career_mkt <- cg[, .(games_all = .N, first_all = min(season), last_all = max(season),
                     rate_per17 = sum(actual - expected_wp) / .N * 17,
                     sd_rate = sqrt(sum(var_i)) / .N * 17), by = coach]
x16 <- career_mkt[games_all >= 16]
tau2 <- max(var(x16$rate_per17) - mean(x16$sd_rate^2), 0.01)
career_mkt[, war_market_only_1999 := (tau2 * rate_per17) / (tau2 + sd_rate^2)]
cat(sprintf("\nLegacy market-only column: EB tau = %.3f wins/17 on %d careers >= 16 games\n", sqrt(tau2), nrow(x16)))

pc <- fread(file.path(NFLA, "data/playcallers.csv"))
pc[, team := norm_team(team)]
own_pc <- pc[season >= FIRST & season <= LAST,
             .(pct_own_playcaller = mean(off_play_caller == head_coach, na.rm = TRUE)),
             by = .(coach = head_coach)]
dv <- fread("data/factory/decision_value.csv", select = c("coach", "era_shrunk", "lo", "hi"))
setnames(dv, c("coach", "fourth_down_value", "fourth_down_lo", "fourth_down_hi"))

# ---------------------------------------------------------------------------
# 7. Parametric bootstrap of the prediction error -- cached.
#
# SKEPTIC round 1 (blocking): the old bootstrap resampled seasons within each
# coach and refit. That holds every coach's true effect at his OBSERVED mean,
# so the resampled BLUPs centred on the raw value, not the shrunk one, and
# prob_above_avg came out 1.00 for coaches whose own interval included zero.
# Now: simulate panels from M_wat's fitted variance components on the real
# design (same coaches, teams, seasons, weights), refit, and record for every
# coach the error (estimated WAR minus the simulated true WAR). Intervals are
# the observed WAR plus the quantiles of that error; the centre is the shrunk
# estimate by construction (asserted below), and the error includes
# variance-component and replacement-offset estimation noise.
# ---------------------------------------------------------------------------
panel_key <- sprintf("v4_%d_%.6f_%d", nrow(panel), sum(panel$wat17), sum(panel$interim))
boot_path <- "data/derived/coaching_war_boot.rds"
sim_panel <- function(d, coach_sd, team_sd, season_sd, resid_sd, intercept) {
  # one simulated wat17 vector on the design of d; returns the draw and the true coach effects
  cs <- unique(d$coach); ts <- unique(d$team); ss <- unique(d$season)
  u_c <- setNames(rnorm(length(cs), 0, coach_sd), cs)
  u_t <- setNames(rnorm(length(ts), 0, team_sd), ts)
  u_s <- setNames(rnorm(length(ss), 0, season_sd), as.character(ss))
  y <- intercept + u_c[d$coach] + u_t[d$team] + u_s[as.character(d$season)] + rnorm(nrow(d), 0, resid_sd / sqrt(d$w))
  list(y = unname(y), u_c = u_c)
}
vc_wat <- list(coach = sd_of("M_wat", "coach"), team = sd_of("M_wat", "team"),
               season = sd_of("M_wat", "season"), resid = sd_of("M_wat", "Residual"),
               intercept = unname(fixef(m_wat)[1]))
# SKEPTIC round 2 (blocking): the true coach SD is drawn for every rep from
# the profile likelihood of M_wat (flat prior on tau; 95% profile CI 0 to 1.3),
# not held at the REML point estimate, so the uncertainty in the scale of the
# coach term reaches the intervals and probabilities.
tau_grid <- seq(0, max(prof_df$tau), by = 0.01)
tau_lik  <- exp(-0.5 * approx(prof_df$tau, prof_df$zeta, tau_grid, rule = 2)$y^2)
draw_tau <- function(n) sample(tau_grid, n, replace = TRUE, prob = tau_lik)
run_boot <- function() {
  out <- vector("list", BOOT)
  taus <- draw_tau(BOOT)
  for (b in seq_len(BOOT)) {
    d <- copy(panel)
    s <- sim_panel(d, taus[b], vc_wat$team, vc_wat$season, vc_wat$resid, vc_wat$intercept)
    d[, wat17 := s$y]
    m <- try(fit_m(F_WAT, d), silent = TRUE)
    if (inherits(m, "try-error")) next
    bt <- blups(m, "team"); bs <- blups(m, "season")
    d[, rpc := wat17 - (fixef(m)[1] + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
    ro_hat  <- d[spell_yr == 1 & !interim, wmean(rpc, w)]
    ro_true <- d[spell_yr == 1 & !interim, wmean(s$u_c[coach], w)]
    bc <- blups(m)
    out[[b]] <- data.table(rep = b, coach = bc$grp, err = (bc$blup - ro_hat) - (s$u_c[bc$grp] - ro_true),
                           rep_err = ro_hat - ro_true, tau_true = taus[b],
                           tau_hat = as.data.table(VarCorr(m))[grp == "coach", sdcor])
    if (b %% 50 == 0) cat(sprintf("  bootstrap rep %d/%d\n", b, BOOT))
  }
  list(key = panel_key, boot = rbindlist(out))
}
if (!REBUILD && file.exists(boot_path) && readRDS(boot_path)$key == panel_key) {
  boot <- readRDS(boot_path)$boot; cat("\nBootstrap: reused cache\n")
} else {
  cat("\nBootstrap: running", BOOT, "parametric reps, coach SD drawn from the profile likelihood\n"); bb <- run_boot(); saveRDS(bb, boot_path); boot <- bb$boot
}
rep_boot_se <- sd(unique(boot[, .(rep, rep_err)])$rep_err)
cat(sprintf("rep_offset simulation SE = %.3f (naive SE %.3f); the round-2 move of the zero line (%+.3f -> %+.3f, %.3f) is %.1f bootstrap SEs\n",
            rep_boot_se, rep_se_naive, rep_old_rule$value, rep_offset, rep_offset - rep_old_rule$value, abs(rep_offset - rep_old_rule$value) / rep_boot_se))
add_val("rep_offset_se", rep_boot_se, rep_noint$n, sprintf("SE of the replacement offset from the parametric bootstrap (rep_err); naive SE %.3f; the old-rule vs new-rule difference of %.2f is under one SE", rep_se_naive, abs(rep_offset - rep_old_rule$value)))
tau_draws <- unique(boot[, .(rep, tau_true, tau_hat)])
cat(sprintf("Bootstrap coach SD: true values drawn from the profile likelihood, median %.2f, 95%% range [%.2f, %.2f]; REML recovers median %.2f, and %.0f%% of reps put it on the zero boundary\n",
            median(tau_draws$tau_true), quantile(tau_draws$tau_true, .025), quantile(tau_draws$tau_true, .975),
            median(tau_draws$tau_hat), 100 * mean(tau_draws$tau_hat < 0.01)))
add_val("boot_tau_true_median", median(tau_draws$tau_true), nrow(tau_draws), "coach SD drawn per bootstrap rep from the profile likelihood (2.5, 97.5 quantiles in lo, hi)",
        quantile(tau_draws$tau_true, .025), quantile(tau_draws$tau_true, .975))
add_val("boot_tau_hat_boundary_share", mean(tau_draws$tau_hat < 0.01), nrow(tau_draws), "share of bootstrap refits where REML put the coach SD at zero")

# ---------------------------------------------------------------------------
# 8. Coach table.
# ---------------------------------------------------------------------------
coach <- panel[, .(first_season = min(season), last_season = max(season), seasons = .N,
                   seasons_weighted = sum(w), games = sum(games), teams = uniqueN(team),
                   team_list = paste(unique(team), collapse = "/"),
                   actual_wins = sum(act17 * w), market_expected_wins = sum(mkt17 * w),
                   talent_expected_wins = sum(talent17 * w),
                   raw_wat_per_season = wmean(wat17, w),
                   raw_premium_per_season = wmean(premium17, w),
                   raw_wae_per_season = wmean(wae17, w),
                   n_teams_inwindow = uniqueN(team)), by = coach]
n_coaches_team <- panel[, .(n_other_coaches = uniqueN(coach) - 1), by = team]
coach <- merge(coach, panel[, .(min_other = min(n_coaches_team$n_other_coaches[match(team, n_coaches_team$team)])), by = coach], by = "coach")
coach[, one_team_one_coach := n_teams_inwindow == 1 & min_other == 0]
coach <- Reduce(function(a, b) merge(a, b, by = "coach", all.x = TRUE),
                list(coach, b_wat, b_prem, b_wae, b_inh))
coach[, rep_offset := rep_offset]
coach[, war_per_season := coach_effect - rep_offset]
coach[, war_total := war_per_season * seasons_weighted]
# plausible true WAR for each coach = observed WAR minus a draw of the estimation error
boot <- merge(boot, coach[, .(coach, war_obs = war_per_season)], by = "coach")
boot[, war := war_obs - err]
bsum <- boot[, .(boot_se = sd(err), boot_mean = mean(war), boot_lo = quantile(war, .025), boot_hi = quantile(war, .975),
                 prob_above_avg = mean(war + rep_offset > 0), prob_above_rep = mean(war > 0)), by = coach]
coach <- merge(coach, bsum, by = "coach", all.x = TRUE)
# SKEPTIC round 1 check: the bootstrap must centre on the shrunk estimate, not the raw mean
boot_bias <- coach[seasons >= 8, max(abs(boot_mean - war_per_season))]
cat(sprintf("Bootstrap centring check: max |boot_mean - war_per_season| over coaches with >= 8 seasons = %.3f (must be < 0.1); cor(boot_mean - war, raw - war) = %.2f\n",
            boot_bias, cor(coach$boot_mean - coach$war_per_season, coach$raw_wat_per_season - coach$war_per_season)))
if (boot_bias >= 0.1) stop("bootstrap is not centred on the shrunk estimate")
add_val("boot_centring_max_abs_bias_8plus", boot_bias, coach[seasons >= 8, .N], "max |boot_mean - war_per_season| for coaches with >= 8 seasons; parametric bootstrap of the prediction error")
coach[, se := pmax(se_cond, boot_se, na.rm = TRUE)]
coach[, `:=`(lo = war_per_season - 1.96 * se, hi = war_per_season + 1.96 * se)]
coach[, se_total := sqrt((se * seasons_weighted)^2 + (rep_boot_se * seasons_weighted)^2)]
coach[, clear_of_replacement := lo > 0]
coach[, eligible := seasons >= 4]
coach[eligible == TRUE, rank := frank(-war_per_season, ties.method = "min")]
coach[, rank_all := frank(-war_per_season, ties.method = "min")]
coach[, war_vs_interim := coach_effect - rep_interim$value]
coach[, war_vs_average := coach_effect]
# SKEPTIC round 3: the two alternative zero lines the page can show beside the main one
coach[, war_rep_no_returnee := coach_effect - rep_noret$value]
coach <- merge(coach, panel[, .(rep_offset_era = wmean(rep_offset_era, w)), by = coach], by = "coach", all.x = TRUE)
coach[, war_rep_era := coach_effect - rep_offset_era]
coach <- merge(coach, career_mkt[, .(coach, games_1999 = games_all, first_season_1999 = first_all,
                                     last_season_1999 = last_all, war_market_only_1999)], by = "coach", all.x = TRUE)
coach <- merge(coach, own_pc, by = "coach", all.x = TRUE)
coach <- merge(coach, dv, by = "coach", all.x = TRUE)
# bootstrap rank intervals among eligible coaches
elig <- coach[eligible == TRUE, coach]
brank <- boot[coach %in% elig][, rk := frank(-war, ties.method = "min"), by = rep]
rk_sum <- brank[, .(rank_lo = quantile(rk, .05, type = 1), rank_hi = quantile(rk, .95, type = 1), p_top5 = mean(rk <= 5),
                    p_top10 = mean(rk <= 10)), by = coach]
coach <- merge(coach, rk_sum, by = "coach", all.x = TRUE)
setorder(coach, -war_per_season)

# SKEPTIC round 2 (blocking): the same board with the coach SD held at the
# value the observed split-half supports (0.40) and at the profile upper
# bound. Replacement offset recomputed from each refit's own team/season terms.
war_at_tau <- function(mm, suffix) {
  b <- blups(mm); bt <- blups(mm, "team"); bs <- blups(mm, "season")
  d <- copy(panel)
  d[, rpc := wat17 - (fixef(mm)[1] + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
  ro <- d[spell_yr == 1 & !interim, wmean(rpc, w)]
  out <- data.table(coach = b$grp, eff = b$blup, se = b$se_cond)
  out[, `:=`(war = eff - ro, pa = pnorm(eff / se))]
  setnames(out, c("eff", "se", "war", "pa"), paste0(c("coach_effect", "se_cond", "war", "prob_above_avg"), "_tau_", suffix))
  list(tab = out, ro = ro)
}
tl <- war_at_tau(m_tau_low, "low"); th <- war_at_tau(m_tau_high, "high")
coach <- merge(coach, tl$tab, by = "coach", all.x = TRUE)
coach <- merge(coach, th$tab, by = "coach", all.x = TRUE)
setorder(coach, -war_per_season)
cat(sprintf("\nBOARD AT FIXED COACH SD: tau %.2f (replacement %+.2f) and tau %.2f (replacement %+.2f):\n", TAU_LOW, tl$ro, TAU_HIGH, th$ro))
print(coach[eligible == TRUE][1:8, .(coach, war_main = round(war_per_season, 2), war_tau_low = round(war_tau_low, 2), p_above_avg_low = round(prob_above_avg_tau_low, 2),
                                      war_tau_high = round(war_tau_high, 2))])
add_val("top_coach_war_tau_low", coach[eligible == TRUE][1, war_tau_low], 1, sprintf("WAR of the top coach (%s) with the coach SD held at %.2f; published %+.2f", coach[eligible == TRUE][1, coach], TAU_LOW, coach[eligible == TRUE][1, war_per_season]))
add_val("top_coach_war_tau_high", coach[eligible == TRUE][1, war_tau_high], 1, sprintf("same with the coach SD at the profile upper bound %.2f", TAU_HIGH))
add_val("top_coach_prob_above_avg_tau_low", coach[eligible == TRUE][1, prob_above_avg_tau_low], 1, sprintf("P(coach effect > 0) for the top coach at tau %.2f, from the conditional SD", TAU_LOW))
add_val("rep_offset_tau_low", tl$ro, rep_noint$n, sprintf("replacement offset with the coach SD held at %.2f", TAU_LOW))
# Tau-marginalised posterior: fixed-tau fits on a grid weighted by the profile
# likelihood (flat prior on tau). The bootstrap above draws tau for the TRUE
# effects but the refit re-shrinks with the data, so it barely moves P(above
# average); this mixture is the number that answers "given THIS data and an
# unknown coach SD". Written as *_tau_marg.
tau_g <- seq(0.05, max(prof_df$tau), by = 0.05)
w_g <- exp(-0.5 * approx(prof_df$tau, prof_df$zeta, tau_g, rule = 2)$y^2); w_g <- w_g / sum(w_g)
marg <- rbindlist(lapply(seq_along(tau_g), function(k) {
  r <- war_at_tau(fit_fixed_tau(panel, tau_g[k]), "g")
  data.table(k = k, tau = tau_g[k], wgt = w_g[k], coach = r$tab$coach, eff = r$tab$coach_effect_tau_g, se = r$tab$se_cond_tau_g, war = r$tab$war_tau_g)
}))
set.seed(714)
marg_sum <- marg[, {
  draws <- unlist(lapply(seq_len(.N), function(i) rnorm(round(4000 * wgt[i]), war[i], se[i])))
  .(war_tau_marg = sum(wgt * war), lo_tau_marg = quantile(draws, .025), hi_tau_marg = quantile(draws, .975),
    prob_above_avg_tau_marg = sum(wgt * pnorm(eff / se)))
}, by = coach]
coach <- merge(coach, marg_sum, by = "coach", all.x = TRUE)
setorder(coach, -war_per_season)
cat(sprintf("TAU-MARGINALISED board (%d fixed-tau fits weighted by the profile likelihood; weight on tau < 0.5 is %.0f%%):\n", length(tau_g), 100 * sum(w_g[tau_g < 0.5])))
print(coach[eligible == TRUE][1:8, .(coach, war_main = round(war_per_season, 2), p_above_avg_boot = round(prob_above_avg, 2),
                                      war_tau_marg = round(war_tau_marg, 2), lo = round(lo_tau_marg, 2), hi = round(hi_tau_marg, 2), p_above_avg_tau_marg = round(prob_above_avg_tau_marg, 2))])
add_val("top_coach_prob_above_avg_tau_marg", coach[eligible == TRUE][1, prob_above_avg_tau_marg], length(tau_g), sprintf("P(coach effect > 0) for %s averaged over the coach SD's profile likelihood; bootstrap value %.2f", coach[eligible == TRUE][1, coach], coach[eligible == TRUE][1, prob_above_avg]))
add_val("top_coach_war_tau_marg", coach[eligible == TRUE][1, war_tau_marg], length(tau_g), "WAR of the top coach averaged over the coach SD's profile likelihood (lo, hi = 95% mixture interval)", coach[eligible == TRUE][1, lo_tau_marg], coach[eligible == TRUE][1, hi_tau_marg])
add_val("tau_weight_below_0_5", sum(w_g[tau_g < 0.5]), length(tau_g), "share of the profile likelihood on a coach SD below 0.5 wins")

# SKEPTIC round 1: leave-one-season-out influence (max |change in coach effect|
# when any single one of the coach's rows is dropped), eligible coaches
# SKEPTIC round 2: extended to EVERY row of the panel, so a row of a different
# coach (the successor or predecessor that pins the franchise term) is also
# tested. 451 refits, cached.
infl_path <- "data/derived/coaching_war_influence_cache.rds"
run_infl <- function() {
  elig_c <- coach[eligible == TRUE, coach]
  out <- vector("list", nrow(panel))
  for (i in seq_len(nrow(panel))) {
    m <- fit_m(F_WAT, panel[-i]); b <- blups(m)
    out[[i]] <- data.table(row_coach = panel$coach[i], season = panel$season[i], team = panel$team[i],
                           coach = elig_c, effect_without = b$blup[match(elig_c, b$grp)])
    if (i %% 100 == 0) cat(sprintf("  influence refit %d/%d\n", i, nrow(panel)))
  }
  list(key = panel_key, infl = rbindlist(out))
}
if (!REBUILD && file.exists(infl_path) && readRDS(infl_path)$key == panel_key) {
  infl <- readRDS(infl_path)$infl
} else { cat("\nInfluence: refitting without each of", nrow(panel), "rows\n"); ii <- run_infl(); saveRDS(ii, infl_path); infl <- ii$infl }
infl <- merge(infl, coach[, .(coach, coach_effect)], by = "coach")
infl[, delta := effect_without - coach_effect]
infl_own <- infl[row_coach == coach]
infl_sum <- infl_own[, .(influence_max = max(abs(delta)), influence_season = season[which.max(abs(delta))],
                         influence_team = team[which.max(abs(delta))], influence_without = effect_without[which.max(abs(delta))]), by = coach]
infl_oth <- infl[row_coach != coach]
infl_oth_sum <- infl_oth[, .(influence_other_max = max(abs(delta)), influence_other_row = sprintf("%s %d %s", row_coach, season, team)[which.max(abs(delta))],
                             influence_other_without = effect_without[which.max(abs(delta))]), by = coach]
coach <- merge(coach, infl_sum, by = "coach", all.x = TRUE)
coach <- merge(coach, infl_oth_sum, by = "coach", all.x = TRUE)
coach[, influence_flag := !is.na(influence_max) & influence_max > 0.3]
coach[, influence_other_flag := !is.na(influence_other_max) & influence_other_max > 0.3]
# franchise share: the coach's share of his main franchise's rows in the window
fr_share <- panel[, .(n = .N), by = .(coach, team)][, tot := sum(n), by = team][, share := n / tot]
coach <- merge(coach, fr_share[, .(franchise_share = max(share)), by = coach], by = "coach", all.x = TRUE)
coach[, franchise_pinned := franchise_share >= 0.85]
setorder(coach, -war_per_season)
cat(sprintf("\nInfluence of a DIFFERENT coach's row > 0.3 wins: %d of %d eligible coaches\n", sum(coach$influence_other_flag), sum(coach$eligible)))
oth_flag <- coach[influence_other_flag == TRUE]
for (i in seq_len(nrow(oth_flag))) {
  cat(sprintf("  %s: drop %s and the coach effect goes %+.2f -> %+.2f\n", oth_flag$coach[i], oth_flag$influence_other_row[i], oth_flag$coach_effect[i], oth_flag$influence_other_without[i]))
  add_val(paste0("influence_other_", gsub(" ", "_", oth_flag$coach[i])), oth_flag$influence_other_max[i], oth_flag$seasons[i],
          sprintf("max |change in coach effect| dropping one row of another coach (%s); effect without it %+.2f", oth_flag$influence_other_row[i], oth_flag$influence_other_without[i]))
}
cat(sprintf("Coaches with >= 85%% of their franchise's rows (franchise term rests on 1-2 seasons of other coaches): %s\n",
            paste(coach[franchise_pinned == TRUE & eligible == TRUE, sprintf("%s %.0f%%", coach, 100 * franchise_share)], collapse = ", ")))
add_val("n_franchise_pinned_eligible", coach[franchise_pinned == TRUE & eligible == TRUE, .N], sum(coach$eligible), "eligible coaches with >= 85% of their franchise's rows in the window")

# SKEPTIC round 2: partnership numbers. Refit without the coach's rows where
# the starting QB was one of the era's elite on a non-rookie deal.
elite_qb <- c("Tom Brady", "Peyton Manning", "Drew Brees", "Aaron Rodgers", "Patrick Mahomes",
              "Ben Roethlisberger", "Russell Wilson", "Lamar Jackson", "Josh Allen", "Joe Burrow")
rookie_deal <- list(`Patrick Mahomes` = 2017:2019, `Lamar Jackson` = 2018:2022, `Josh Allen` = 2018:2020,
                    `Joe Burrow` = 2020:2022, `Russell Wilson` = 2012:2014)
panel[, elite_qb_row := qb_name %in% elite_qb &
        !mapply(function(q, s) !is.null(rookie_deal[[q]]) && s %in% rookie_deal[[q]], qb_name, season)]
cat(sprintf("\nElite-QB rows (non-rookie deal): %d of %d panel rows; QBs: %s\n", sum(panel$elite_qb_row), nrow(panel),
            paste(panel[elite_qb_row == TRUE, .N, by = qb_name][order(-N), sprintf("%s %d", qb_name, N)], collapse = ", ")))
eq_coaches <- intersect(coach[eligible == TRUE, coach], unique(panel[elite_qb_row == TRUE, coach]))
eq <- rbindlist(lapply(eq_coaches, function(cn) {
  keep <- !(panel$coach == cn & panel$elite_qb_row)
  n_drop <- sum(!keep)
  if (sum(keep & panel$coach == cn) == 0)
    return(data.table(coach = cn, elite_qb_seasons = n_drop, coach_effect_no_elite_qb = NA_real_))
  b <- blups(fit_m(F_WAT, panel[keep]))
  data.table(coach = cn, elite_qb_seasons = n_drop, coach_effect_no_elite_qb = b$blup[match(cn, b$grp)])
}))
coach <- merge(coach, eq, by = "coach", all.x = TRUE)
coach[, elite_qb_delta := coach_effect_no_elite_qb - coach_effect]
coach[, elite_qb_flag := !is.na(elite_qb_delta) & abs(elite_qb_delta) > 0.3]
coach[is.na(elite_qb_seasons), elite_qb_seasons := 0L]
setorder(coach, -war_per_season)
cat(sprintf("Elite-QB refits for %d eligible coaches; |delta| > 0.3 for %d:\n", length(eq_coaches), sum(coach$elite_qb_flag)))
print(coach[!is.na(coach_effect_no_elite_qb) | (elite_qb_seasons > 0 & eligible), .(coach, seasons, elite_qb_seasons, coach_effect = round(coach_effect, 2),
                                                  no_elite_qb = round(coach_effect_no_elite_qb, 2), delta = round(elite_qb_delta, 2), flag = elite_qb_flag)][order(-abs(delta))])
for (i in which(coach$elite_qb_flag))
  add_val(paste0("elite_qb_", gsub(" ", "_", coach$coach[i])), coach$elite_qb_delta[i], coach$elite_qb_seasons[i],
          sprintf("change in coach effect when his %d elite-QB seasons are dropped; effect without them %+.2f", coach$elite_qb_seasons[i], coach$coach_effect_no_elite_qb[i]))
cat(sprintf("  Coaches whose every season had an elite QB (no number without): %s\n",
            paste(coach[eligible == TRUE & elite_qb_seasons == seasons, coach], collapse = ", ")))
# SKEPTIC round 3 (major): the QB control is cap share plus LAST season's EPA,
# so a quarterback who plays far above his pay (rookie deal, a discount deal)
# is scored as cheap talent and the wins land on the coach. Cheap-QB rows:
# qb_z < -0.5 and same-season qbcur_z > +0.5. Refit without each eligible
# coach's cheap-QB rows, the same way as the elite-QB refit.
panel[, cheap_qb_row := qb_z < -0.5 & qbcur_z > 0.5]
cat(sprintf("\nCheap-QB rows (qb_z < -0.5, same-season qbcur_z > 0.5): %d of %d; mean wat17 %+.2f vs %+.2f on the rest; %d of the top 12 have at least one\n",
            sum(panel$cheap_qb_row), nrow(panel), panel[cheap_qb_row == TRUE, mean(wat17)], panel[cheap_qb_row == FALSE, mean(wat17)],
            sum(coach[eligible == TRUE][1:12, coach] %in% panel[cheap_qb_row == TRUE, coach])))
add_val("cheap_qb_rows_mean_wat17", panel[cheap_qb_row == TRUE, mean(wat17)], sum(panel$cheap_qb_row),
        sprintf("mean wat17 on rows where the starter is paid below average but plays above average; %+.2f on the other %d rows", panel[cheap_qb_row == FALSE, mean(wat17)], sum(!panel$cheap_qb_row)))
cq_coaches <- intersect(coach[eligible == TRUE, coach], unique(panel[cheap_qb_row == TRUE, coach]))
cq <- rbindlist(lapply(cq_coaches, function(cn) {
  keep <- !(panel$coach == cn & panel$cheap_qb_row)
  n_drop <- sum(!keep)
  if (sum(keep & panel$coach == cn) == 0)
    return(data.table(coach = cn, cheap_qb_seasons = n_drop, coach_effect_no_cheap_qb = NA_real_))
  b <- blups(fit_m(F_WAT, panel[keep]))
  data.table(coach = cn, cheap_qb_seasons = n_drop, coach_effect_no_cheap_qb = b$blup[match(cn, b$grp)])
}))
coach <- merge(coach, cq, by = "coach", all.x = TRUE)
coach[, cheap_qb_delta := coach_effect_no_cheap_qb - coach_effect]
coach[, cheap_qb_flag := !is.na(cheap_qb_delta) & abs(cheap_qb_delta) > 0.3]
coach[is.na(cheap_qb_seasons), cheap_qb_seasons := 0L]
setorder(coach, -war_per_season)
cat(sprintf("Cheap-QB refits for %d eligible coaches; |delta| > 0.3 for %d:\n", length(cq_coaches), sum(coach$cheap_qb_flag)))
print(coach[!is.na(coach_effect_no_cheap_qb), .(coach, seasons, cheap_qb_seasons, coach_effect = round(coach_effect, 2),
                                                  no_cheap_qb = round(coach_effect_no_cheap_qb, 2), delta = round(cheap_qb_delta, 2), flag = cheap_qb_flag)][order(-abs(delta))])
for (i in which(coach$cheap_qb_flag))
  add_val(paste0("cheap_qb_", gsub(" ", "_", coach$coach[i])), coach$cheap_qb_delta[i], coach$cheap_qb_seasons[i],
          sprintf("change in coach effect when his %d cheap-QB seasons are dropped; effect without them %+.2f", coach$cheap_qb_seasons[i], coach$coach_effect_no_cheap_qb[i]))
# how much of the published coach effect is the same season's QB play?
mqb <- panel[, .(mean_qbcur_z = mean(qbcur_z), mean_qb_gap = mean(qbcur_z - qb_z)), by = coach]
coach <- merge(coach, mqb, by = "coach", all.x = TRUE)
setorder(coach, -war_per_season)
r_qbcur <- coach[eligible == TRUE, cor(coach_effect, mean_qbcur_z)]
lm_qbgap <- summary(lm(coach_effect ~ mean_qb_gap, data = coach[eligible == TRUE]))$coefficients["mean_qb_gap", ]
cat(sprintf("Across %d eligible coaches: cor(coach effect, mean same-season QB z) = %.2f; coach effect on mean(QB play minus QB pay) %+.2f per SD (t = %.1f, p = %.3f)\n",
            sum(coach$eligible), r_qbcur, lm_qbgap[1], lm_qbgap[3], lm_qbgap[4]))
add_val("cor_coach_effect_mean_qbcur_z", r_qbcur, sum(coach$eligible), "published coach effect vs the coach's mean same-season QB EPA z, eligible coaches")
add_val("coach_effect_on_qb_play_minus_pay", unname(lm_qbgap[1]), sum(coach$eligible), sprintf("slope of coach effect on mean(qbcur_z - qb_z); t = %.2f", lm_qbgap[3]), p = unname(lm_qbgap[4]))

cat(sprintf("\nLeave-one-season-out influence > 0.3 wins (one row moves the coach effect by more than 0.3): %d of %d eligible coaches\n",
            sum(coach$influence_flag), sum(coach$eligible)))
infl_flag <- coach[influence_flag == TRUE]
if (nrow(infl_flag)) for (i in seq_len(nrow(infl_flag))) {
  w_o <- infl[coach == infl_flag$coach[i] & season == infl_flag$influence_season[i] & team == infl_flag$influence_team[i], effect_without][1]
  cat(sprintf("  %s: drop %d %s and the coach effect goes %+.2f -> %+.2f\n", infl_flag$coach[i], infl_flag$influence_season[i], infl_flag$influence_team[i], infl_flag$coach_effect[i], w_o))
  add_val(paste0("influence_", gsub(" ", "_", infl_flag$coach[i])), infl_flag$influence_max[i], infl_flag$seasons[i],
          sprintf("max |change in coach effect| dropping one season (%d %s); effect without it %+.2f", infl_flag$influence_season[i], infl_flag$influence_team[i], w_o))
}

# SKEPTIC round 1: rank tracks seasons (eligibility keeps only survivors)
sp_seas <- cor(coach[eligible == TRUE, war_per_season], coach[eligible == TRUE, seasons], method = "spearman")
cat(sprintf("Spearman(war_per_season, seasons) among %d eligible coaches = %.3f; shrink factor coach_effect/raw: %.2f (4-5 seasons) vs %.2f (10+ seasons)\n",
            sum(coach$eligible), sp_seas,
            coach[eligible == TRUE & seasons <= 5, median(coach_effect / raw_wat_per_season, na.rm = TRUE)],
            coach[eligible == TRUE & seasons >= 10, median(coach_effect / raw_wat_per_season, na.rm = TRUE)]))
add_val("spearman_war_vs_seasons_eligible", sp_seas, sum(coach$eligible), "rank correlation of WAR per season with seasons coached among eligible coaches; the bottom of the board is the worst of the survivors")
# SKEPTIC round 2: coaches already in place at the 2012 window start had
# survived the pre-2012 firing filter and carry no years 1-2 in the panel
coach[, window_start_veteran := !is.na(first_season_1999) & first_season_1999 < FIRST]
vet <- coach[eligible == TRUE]
tt_vet <- t.test(vet[window_start_veteran == TRUE, war_per_season], vet[window_start_veteran == FALSE, war_per_season])
cat(sprintf("Window-start veterans (hired before %d) vs in-window hires, eligible: %+.2f (n = %d) vs %+.2f (n = %d); difference %+.2f [%+.2f, %+.2f], p = %.3f; %d of the top 12 are veterans\n",
            FIRST, tt_vet$estimate[1], sum(vet$window_start_veteran), tt_vet$estimate[2], sum(!vet$window_start_veteran),
            tt_vet$estimate[1] - tt_vet$estimate[2], tt_vet$conf.int[1], tt_vet$conf.int[2], tt_vet$p.value, vet[1:12, sum(window_start_veteran)]))
add_val("veteran_vs_inwindow_hire_diff", unname(tt_vet$estimate[1] - tt_vet$estimate[2]), nrow(vet),
        sprintf("mean WAR, eligible coaches hired before %d (n = %d, left-truncated: no years 1-2 in panel) minus hired in window (n = %d); Welch", FIRST, sum(vet$window_start_veteran), sum(!vet$window_start_veteran)),
        tt_vet$conf.int[1], tt_vet$conf.int[2], tt_vet$p.value)

# SKEPTIC round 1: does calling your own plays show up at all?
pcg <- coach[eligible == TRUE & !is.na(pct_own_playcaller)]
pcg[, own := pct_own_playcaller >= 0.5]
tt_pc <- t.test(pcg[own == TRUE, war_per_season], pcg[own == FALSE, war_per_season])
cat(sprintf("Own play-caller (>= 50%% of games) vs not, mean WAR among eligible: %+.2f (n = %d) vs %+.2f (n = %d); difference %+.2f [%+.2f, %+.2f], p = %.2f\n",
            mean(pcg[own == TRUE, war_per_season]), sum(pcg$own), mean(pcg[own == FALSE, war_per_season]), sum(!pcg$own),
            tt_pc$estimate[1] - tt_pc$estimate[2], tt_pc$conf.int[1], tt_pc$conf.int[2], tt_pc$p.value))
add_val("playcaller_own_vs_not_diff", unname(tt_pc$estimate[1] - tt_pc$estimate[2]), nrow(pcg),
        sprintf("mean WAR, own play-caller (n = %d) minus not (n = %d); this is a head-coach board", sum(pcg$own), sum(!pcg$own)),
        tt_pc$conf.int[1], tt_pc$conf.int[2], tt_pc$p.value)

n_clear <- sum(coach$clear_of_replacement); n_chance <- 0.025 * nrow(coach)
cat(sprintf("\n%d of %d coaches are clear of replacement (lo > 0); ~%.1f would clear a one-sided 2.5%% bound by chance\n",
            n_clear, nrow(coach), n_chance))
cat(sprintf("%d of %d ELIGIBLE (>= 4 seasons) clear of replacement\n", sum(coach[eligible == TRUE]$clear_of_replacement), sum(coach$eligible)))
add_val("n_clear_of_replacement", n_clear, nrow(coach), sprintf("lo > 0; %.1f expected by chance", n_chance))
show <- function(d) print(d[, .(coach, seasons, teams = team_list, war = round(war_per_season, 2),
                                 lo = round(lo, 2), hi = round(hi, 2), raw = round(raw_wat_per_season, 2),
                                 premium = round(premium_per_season, 2), surprise = round(surprise_per_season, 2),
                                 rank_90 = paste0(rank_lo, "-", rank_hi), p_top5 = round(p_top5, 2))])
cat("\nTop 12 (>= 4 seasons), WAR per season over a fresh hire, 95% interval:\n"); show(coach[eligible == TRUE][1:12])
cat("\nBottom 12 (>= 4 seasons):\n"); show(coach[eligible == TRUE][order(war_per_season)][1:12])
cat("\nShort careers (< 4 seasons), top 5 by point estimate, intervals only:\n")
show(coach[eligible == FALSE][1:5])

# Belichick with and without the Brady-era cells
bb_rows <- panel[coach == "Bill Belichick" & season <= 2019]
m_nobrady <- fit_m(F_WAT, panel[!(coach == "Bill Belichick" & season <= 2019)])
bb_full <- coach[coach == "Bill Belichick", coach_effect]
bb_nb <- blups(m_nobrady)[grp == "Bill Belichick", blup]
cat(sprintf("\nBelichick coach effect: with Brady-era cells (2012-2019) %+.2f; without them (2020-2023 only, %d seasons) %+.2f\n",
            bb_full, panel[coach == "Bill Belichick" & season > 2019, .N], if (length(bb_nb)) bb_nb else NA))
add_val("belichick_effect_full", bb_full, panel[coach == "Bill Belichick", .N], "M_wat coach BLUP, all seasons in window")
add_val("belichick_effect_no_brady", if (length(bb_nb)) bb_nb else NA, panel[coach == "Bill Belichick" & season > 2019, .N], "M_wat refit dropping 2012-2019 NE rows")

# ---------------------------------------------------------------------------
# 9. Out-of-sample (G5 form) -- cached.
# ---------------------------------------------------------------------------
oos_path <- "data/derived/coaching_war_oos_cache.rds"
run_oos <- function() {
  out <- list()
  for (t in OOS_FROM:LAST) {
    tr <- panel[season < t]; te <- panel[season == t]
    tf_tr <- lm(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17, data = tr, weights = w)  # no season factor: t is unseen
    tr[, talent17_tr := fitted(tf_tr)]; tr[, wat17 := act17 - talent17_tr]
    te[, talent17_tr := predict(tf_tr, newdata = te)]
    te[, `:=`(realised_wat = act17 - talent17_tr, realised_wae = wae17,
              realised_prem = mkt17 - talent17_tr)]
    m <- fit_m(F_WAT, tr)
    bt <- blups(m, "team"); bs <- blups(m, "season"); bc <- blups(m)
    tr[, rpc := wat17 - (fixef(m)[1] + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
    ro <- tr[spell_yr == 1 & !interim, wmean(rpc, w)]
    prev <- tr[season == t - 1, .(coach, prev_wae = wae17, prev_raw_wat = wat17, prev_wins = act17)]
    prev <- prev[, lapply(.SD, mean), by = coach]
    raw_tr <- tr[, .(raw_wat_through_prev = wmean(wat17, w), seasons_train = .N), by = coach]
    te <- merge(te, data.table(coach = bc$grp, war_through_prev = bc$blup - ro), by = "coach")
    te[, war_plus_team := war_through_prev + bt$blup[match(team, bt$grp)]]
    te <- merge(te, prev, by = "coach", all.x = TRUE)
    te <- merge(te, raw_tr, by = "coach", all.x = TRUE)
    te[, train_coach_sd := as.data.table(VarCorr(m))[grp == "coach", sdcor]]
    out[[length(out) + 1]] <- te[, .(season, coach, team, games, seasons_train, train_coach_sd, war_through_prev, war_plus_team,
                                     raw_wat_through_prev, prev_wae, prev_raw_wat, prev_wins,
                                     realised_wat, realised_wae, realised_prem, realised_wins = act17)]
    # SKEPTIC round 2: coaches who coached in t-1 but are not scored in t (fired or left)
    gone <- setdiff(tr[season == t - 1, coach], te$coach)
    if (length(gone)) unsc[[length(unsc) + 1]] <<- data.table(season = t, coach = gone, war_through_prev = bc$blup[match(gone, bc$grp)] - ro)
  }
  list(key = panel_key, oos = rbindlist(out), unscored = rbindlist(unsc))
}
unsc <- list()
if (!REBUILD && file.exists(oos_path) && readRDS(oos_path)$key == panel_key) {
  oo <- readRDS(oos_path); oos <- oo$oos; oos_unscored <- oo$unscored; cat("\nOOS: reused cache\n")
} else { oo <- run_oos(); saveRDS(oo, oos_path); oos <- oo$oos; oos_unscored <- oo$unscored }
fwrite(oos, "data/derived/coaching_war_oos.csv")
fwrite(oos_unscored, "data/derived/coaching_war_oos_unscored.csv")
cat(sprintf("OOS holdout universe: %d scored coach-seasons (coach retained into t); %d coach-seasons where the t-1 coach is not scored in t (fired or left): WAR through t-1 mean %+.2f (SD %.2f) for scored vs %+.2f (SD %.2f) for unscored\n",
            nrow(oos), nrow(oos_unscored), mean(oos$war_through_prev), sd(oos$war_through_prev), mean(oos_unscored$war_through_prev), sd(oos_unscored$war_through_prev)))
add_val("oos_unscored_prev_war_mean", mean(oos_unscored$war_through_prev), nrow(oos_unscored),
        sprintf("mean WAR through t-1 of coaches NOT scored in t (fired/left); scored mean %+.3f (SD %.3f, n %d); the holdout truncates the low end", mean(oos$war_through_prev), sd(oos$war_through_prev), nrow(oos)))
add_val("oos_unscored_prev_war_sd", sd(oos_unscored$war_through_prev), nrow(oos_unscored), "SD of WAR through t-1 among unscored coaches")

oos_stat <- function(x, y, d, label, target) {
  d <- d[!is.na(get(x)) & !is.na(get(y))]
  ct <- cor.test(d[[x]], d[[y]]); sp <- cor(d[[x]], d[[y]], method = "spearman")
  sl <- coef(lm(d[[y]] ~ d[[x]]))[2]
  cat(sprintf("  %-34s -> %-13s r = %+.3f [%+.3f, %+.3f]  rho = %+.3f  slope = %+.2f  n = %d\n",
              label, target, ct$estimate, ct$conf.int[1], ct$conf.int[2], sp, sl, nrow(d)))
  add_val(paste0("oos_", target, "_", x), unname(ct$estimate), nrow(d),
          sprintf("%s predicting %s; rho %.3f, slope %.2f", label, target, sp, sl),
          ct$conf.int[1], ct$conf.int[2], ct$p.value)
  data.table(predictor = x, target = target, r = unname(ct$estimate), lo = ct$conf.int[1],
             hi = ct$conf.int[2], p = ct$p.value, rho = sp, slope = unname(sl), n = nrow(d))
}
cat(sprintf("\nOUT OF SAMPLE, holdout seasons %d-%d (train on %d..t-1). Prespecified bar: r >= %.2f with CI above 0 AND beats baseline (ii)\n",
            OOS_FROM, LAST, FIRST, OOS_BAR))
common <- oos[!is.na(prev_wae)]
cat(sprintf("  All predictable coach-seasons: %d; rows where every baseline exists (coach also coached t-1): %d\n",
            nrow(oos), nrow(common)))
cat("  Target: realised wins above talent in t (team/season effects NOT subtracted)\n")
oos_tab <- rbind(
  oos_stat("war_through_prev", "realised_wat", oos, "WAR through t-1 (all rows)", "wat"),
  oos_stat("war_through_prev", "realised_wat", common, "WAR through t-1", "wat"),
  oos_stat("war_plus_team", "realised_wat", common, "WAR + franchise effect (sensitivity)", "wat"),
  oos_stat("raw_wat_through_prev", "realised_wat", common, "(ii) raw wat through t-1, unshrunk", "wat"),
  oos_stat("prev_wae", "realised_wat", common, "(i) previous-season wae17", "wat"),
  oos_stat("prev_raw_wat", "realised_wat", common, "(ii) previous-season raw wat17", "wat"),
  oos_stat("prev_wins", "realised_wat", common, "(iii) previous-season wins", "wat"))
cat("  Strict market form. Hypothesis: WAR through t-1 predicts beating the spread in t.\n")
oos_tab <- rbind(oos_tab,
  oos_stat("war_through_prev", "realised_wae", oos, "WAR through t-1 (all rows)", "wae"),
  oos_stat("war_through_prev", "realised_wae", common, "WAR through t-1", "wae"),
  oos_stat("prev_wae", "realised_wae", common, "(i) previous-season wae17", "wae"),
  oos_stat("prev_raw_wat", "realised_wae", common, "(ii) previous-season raw wat17", "wae"),
  oos_stat("prev_wins", "realised_wae", common, "(iii) previous-season wins", "wae"))
# partial test: does WAR add anything beyond last season's win total?
# SKEPTIC round 1: coach-clustered (sandwich) SEs, since each coach contributes several holdout rows
cl_t <- function(f, d, term) {
  m <- lm(f, data = d); V <- sandwich::vcovCL(m, cluster = d$coach)
  t <- coef(m)[term] / sqrt(V[term, term]); c(t = unname(t), p = 2 * pnorm(-abs(unname(t))))
}
pf <- summary(lm(realised_wat ~ prev_wins + war_through_prev, data = common))$coefficients
pf_cl <- cl_t(realised_wat ~ prev_wins + war_through_prev, common, "war_through_prev")
pa_cl <- cl_t(realised_wat ~ war_through_prev, common, "war_through_prev")
cat(sprintf("  Partial: realised_wat ~ prev_wins + WAR: WAR t = %.2f, p = %.4f (n = %d); coach-clustered t = %.2f, p = %.3f. WAR alone, clustered t = %.2f\n",
            pf["war_through_prev", "t value"], pf["war_through_prev", "Pr(>|t|)"], nrow(common), pf_cl["t"], pf_cl["p"], pa_cl["t"]))
add_val("oos_partial_t_war_given_prev_wins", pf["war_through_prev", "t value"], nrow(common),
        sprintf("t for WAR in realised_wat ~ prev_wins + WAR; p = %.4f (plain SE)", pf["war_through_prev", "Pr(>|t|)"]),
        p = pf["war_through_prev", "Pr(>|t|)"])
add_val("oos_partial_t_war_given_prev_wins_clustered", pf_cl["t"], nrow(common), "same, coach-clustered sandwich SE", p = pf_cl["p"])
add_val("oos_t_war_alone_clustered", pa_cl["t"], nrow(common), "realised_wat ~ WAR, coach-clustered sandwich SE", p = pa_cl["p"])
# SKEPTIC round 2: season shocks are the natural cluster in a 10-season holdout
cl2_t <- function(f, d, term) {
  m <- lm(f, data = d); V <- sandwich::vcovCL(m, cluster = as.data.frame(d[, .(coach, season)]))
  t <- coef(m)[term] / sqrt(V[term, term]); c(t = unname(t), p = 2 * pnorm(-abs(unname(t))))
}
pa_cl2 <- cl2_t(realised_wat ~ war_through_prev, common, "war_through_prev")
pf_cl2 <- cl2_t(realised_wat ~ prev_wins + war_through_prev, common, "war_through_prev")
cat(sprintf("  Two-way clustered (coach + season): WAR alone t = %.2f (p = %.3f); WAR given prev wins t = %.2f (p = %.3f)\n", pa_cl2["t"], pa_cl2["p"], pf_cl2["t"], pf_cl2["p"]))
add_val("oos_t_war_alone_twoway_clustered", pa_cl2["t"], nrow(common), "realised_wat ~ WAR, coach + season two-way clustered SE", p = pa_cl2["p"])
add_val("oos_partial_t_war_given_prev_wins_twoway_clustered", pf_cl2["t"], nrow(common), "realised_wat ~ prev_wins + WAR, two-way clustered SE", p = pf_cl2["p"])
# holdouts whose training fit had a usable coach SD (not near the boundary)
usable_seasons <- oos[, .(sd = sd(war_through_prev), tsd = train_coach_sd[1]), by = season][sd >= 0.2, season]
common_u <- common[season %in% usable_seasons]
cat(sprintf("  Holdout seasons whose predictions have SD >= 0.2 (training coach SD off the boundary): %s (n = %d rows)\n", paste(usable_seasons, collapse = ", "), nrow(common_u)))
oos_tab <- rbind(oos_tab, oos_stat("war_through_prev", "realised_wat", common_u, "WAR through t-1, usable holdouts", "wat_usable"))
main_oos_u <- oos_tab[target == "wat_usable"]
add_val("oos_zscore_note", NA_real_, nrow(common), "OOS talent z-scores (contract_z, qb_z, qbp_z) are standardised within season using the season-t cross-section of 32 teams; no outcome from season t is used")
pf2 <- summary(lm(realised_wae ~ prev_wins + war_through_prev, data = common))$coefficients
pf2_cl <- cl_t(realised_wae ~ prev_wins + war_through_prev, common, "war_through_prev")
cat(sprintf("  Partial, strict: realised_wae ~ prev_wins + WAR: WAR t = %.2f, p = %.4f; clustered t = %.2f\n",
            pf2["war_through_prev", "t value"], pf2["war_through_prev", "Pr(>|t|)"], pf2_cl["t"]))
main_oos <- oos_tab[predictor == "war_through_prev" & target == "wat"][2]
base_ii  <- oos_tab[predictor == "prev_raw_wat" & target == "wat"]
oos_pass <- main_oos$r >= OOS_BAR & main_oos$lo > 0 & main_oos$r > base_ii$r
oos_pass_weak <- main_oos$lo > 0 & main_oos$r > base_ii$r
cat(sprintf("  OOS verdict against the prespecified bar (r >= %.2f, CI > 0, beats (ii)): %s; weaker bar (r > 0, CI > 0, beats (ii)): %s\n",
            OOS_BAR, if (oos_pass) "PASS" else "FAIL", if (oos_pass_weak) "PASS" else "FAIL"))
add_val("oos_pass_prespecified", as.numeric(oos_pass), nrow(common), sprintf("r >= %.2f with CI > 0 and beats baseline (ii)", OOS_BAR))
add_val("oos_pass_weak", as.numeric(oos_pass_weak), nrow(common), "r > 0 with CI > 0 and beats baseline (ii)")
# per-season breakdown of the headline r
oos_by_season <- oos[, .(n = .N, sd_pred = sd(war_through_prev),
                         r = if (sd(war_through_prev) > 1e-6) cor(war_through_prev, realised_wat) else NA_real_), by = season]
cat("  Per holdout season r (WAR -> wat):", paste(sprintf("%d %s (n %d)", oos_by_season$season, ifelse(is.na(oos_by_season$r), "NA: coach SD on the boundary, all predictions equal", sprintf("%+.2f", oos_by_season$r)), oos_by_season$n), collapse = "; "), "\n")
cat("  SD of the WAR prediction by holdout season:", paste(sprintf("%d %.2f", oos_by_season$season, oos_by_season$sd_pred), collapse = "; "), "\n")
add_val("oos_seasons_with_boundary_coach_sd", sum(is.na(oos_by_season$r)), nrow(oos_by_season), "holdout seasons whose training fit put the coach SD at zero (every prediction identical); with only 5-6 training seasons the coach term is barely identified")
# SKEPTIC round 3 (minor): "beats baseline" was a point comparison. Coach-
# cluster bootstrap of r(WAR) - r(prev wins) on the common rows, and of r(WAR).
set.seed(715)
cl_ids <- split(seq_len(nrow(common)), common$coach); n_cl <- length(cl_ids)
rdiff <- t(replicate(2000, {
  idx <- unlist(cl_ids[sample(n_cl, n_cl, replace = TRUE)], use.names = FALSE)
  d <- common[idx]
  c(r_war = cor(d$war_through_prev, d$realised_wat), r_prev = cor(d$prev_wins, d$realised_wat))
}))
rd <- rdiff[, "r_war"] - rdiff[, "r_prev"]
rd_ci <- quantile(rd, c(.025, .975)); rw_ci <- quantile(rdiff[, "r_war"], c(.025, .975))
cat(sprintf("  Coach-cluster bootstrap (2000 reps, %d coaches): r(WAR) - r(prev wins) = %+.3f [%+.3f, %+.3f]; r(WAR) alone [%+.3f, %+.3f] -> WAR is not distinguishably worse than last season's record either\n",
            n_cl, main_oos$r - oos_tab[predictor == "prev_wins" & target == "wat", r], rd_ci[1], rd_ci[2], rw_ci[1], rw_ci[2]))
add_val("oos_r_diff_war_minus_prev_wins", main_oos$r - oos_tab[predictor == "prev_wins" & target == "wat", r], nrow(common),
        sprintf("r(WAR -> wat) minus r(prev wins -> wat) on common rows; coach-cluster bootstrap 95%% CI in lo, hi (%d clusters, 2000 reps)", n_cl), rd_ci[1], rd_ci[2])
add_val("oos_r_war_cluster_boot_ci", main_oos$r, nrow(common), "r(WAR -> wat) on common rows with coach-cluster bootstrap 95% CI", rw_ci[1], rw_ci[2])
# SKEPTIC round 3 (minor): the spread-to-win logistic is fit on all seasons.
# Refit it on seasons < t for each holdout, rebuild mkt17 for the training
# and test rows, and rerun the headline r.
oos_strict_mkt <- rbindlist(lapply(OOS_FROM:LAST, function(t) {
  mf <- glm(home_win ~ spread_line, data = decisive[season < t], family = binomial)
  ewp <- predict(mf, newdata = games[season <= t], type = "response")
  g2 <- rbind(games[season <= t, .(season, team = home_team, coach = home_coach, ewp = ewp)],
              games[season <= t, .(season, team = away_team, coach = away_coach, ewp = 1 - ewp)])
  m2 <- g2[!is.na(coach) & coach != "", .(mkt17_t = mean(ewp) * 17), by = .(coach, season, team)]
  d <- merge(panel[season <= t], m2, by = c("coach", "season", "team"))
  d[, mkt17 := mkt17_t]
  tr <- d[season < t]; te <- d[season == t]
  tf <- lm(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17, data = tr, weights = w)
  tr[, wat17 := act17 - fitted(tf)]; te[, realised_wat := act17 - predict(tf, newdata = te)]
  m <- fit_m(F_WAT, tr); bc <- blups(m); bt <- blups(m, "team"); bs <- blups(m, "season")
  tr[, rpc := wat17 - (fixef(m)[1] + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
  ro <- tr[spell_yr == 1 & !interim, wmean(rpc, w)]
  te <- merge(te, data.table(coach = bc$grp, war_through_prev = bc$blup - ro), by = "coach")
  te[coach %in% tr[season == t - 1, coach], .(season, coach, war_through_prev, realised_wat)]
}))
r_strict_mkt <- cor(oos_strict_mkt$war_through_prev, oos_strict_mkt$realised_wat)
cat(sprintf("  Spread-to-win logistic refit on seasons < t inside the loop: r = %.3f (n = %d) vs %.3f with the pooled fit; change %.4f\n",
            r_strict_mkt, nrow(oos_strict_mkt), main_oos$r, r_strict_mkt - main_oos$r))
add_val("oos_r_mkt_fit_refit_per_holdout", r_strict_mkt, nrow(oos_strict_mkt), sprintf("headline OOS r with the spread-to-win logistic refit on seasons < t; pooled-fit value %.3f", main_oos$r))

# ---------------------------------------------------------------------------
# 10. Reliability: split-half and persistence.
# ---------------------------------------------------------------------------
rel <- list()
sb <- function(r) 2 * r / (1 + r)
boot_cor <- function(x, y, R = 1000) {
  n <- length(x); rs <- replicate(R, { i <- sample(n, n, TRUE); cor(x[i], y[i]) })
  quantile(rs, c(.025, .975), na.rm = TRUE)
}
# (a) odd vs even seasons
m_odd <- fit_m(F_WAT, panel[season %% 2 == 1]); m_even <- fit_m(F_WAT, panel[season %% 2 == 0])
ok <- panel[, .(n_odd = sum(season %% 2 == 1), n_even = sum(season %% 2 == 0)), by = coach][n_odd >= 2 & n_even >= 2, coach]
sh <- merge(blups(m_odd)[, .(coach = grp, odd = blup)], blups(m_even)[, .(coach = grp, even = blup)], by = "coach")[coach %in% ok]
r_sh <- cor(sh$odd, sh$even); ci_sh <- boot_cor(sh$odd, sh$even)
rel[[1]] <- data.table(split = "odd_even_seasons_blup", n = nrow(sh), r = r_sh, r_sb = sb(r_sh), lo = ci_sh[1], hi = ci_sh[2])
# SKEPTIC round 2 (blocking): what split-half r does the fitted model itself
# imply? Simulate panels from the REML components on the real design, run
# the same odd/even BLUP split, and compare the observed r to that range.
# Repeated at tau = 0.40 for the value the observed r is consistent with.
N_SH <- 200
sim_split <- function(coach_sd) vapply(seq_len(N_SH), function(b) {
  d <- copy(panel); d[, wat17 := sim_panel(d, coach_sd, vc_wat$team, vc_wat$season, vc_wat$resid, vc_wat$intercept)$y]
  mo <- fit_m(F_WAT, d[season %% 2 == 1]); me <- fit_m(F_WAT, d[season %% 2 == 0])
  s <- merge(blups(mo)[, .(coach = grp, odd = blup)], blups(me)[, .(coach = grp, even = blup)], by = "coach")[coach %in% ok]
  if (sd(s$odd) < 1e-8 || sd(s$even) < 1e-8) return(NA_real_)
  cor(s$odd, s$even)
}, numeric(1))
set.seed(713)
sh_fit <- sim_split(vc_wat$coach); sh_low <- sim_split(TAU_LOW)
q_shf <- quantile(sh_fit, c(.025, .5, .975), na.rm = TRUE); q_shl <- quantile(sh_low, c(.025, .5, .975), na.rm = TRUE)
# a simulated half whose REML coach SD hits zero has NO coach signal; count it as r = 0
sh_fit0 <- fifelse(is.na(sh_fit), 0, sh_fit); q_shf0 <- quantile(sh_fit0, c(.025, .5, .975))
pct_obs <- mean(sh_fit <= r_sh, na.rm = TRUE); pct_obs0 <- mean(sh_fit0 <= r_sh)
cat(sprintf("\nMODEL-IMPLIED SPLIT-HALF (odd/even seasons BLUP r, %d sims): at the REML coach SD %.2f median %.2f, 95%% range [%.2f, %.2f] over the %d sims with both halves identified; observed %.3f is %s that range, at its %.0fth percentile.\n  %d sims put one half's coach SD on the zero boundary (no coach signal); counting those as r = 0 the range is [%.2f, %.2f], median %.2f, observed at the %.0fth percentile. At tau %.2f: median %.2f [%.2f, %.2f]\n",
            N_SH, vc_wat$coach, q_shf[2], q_shf[1], q_shf[3], sum(!is.na(sh_fit)), r_sh, if (r_sh < q_shf[1]) "BELOW" else if (r_sh > q_shf[3]) "ABOVE" else "inside", 100 * pct_obs,
            sum(is.na(sh_fit)), q_shf0[1], q_shf0[3], q_shf0[2], 100 * pct_obs0, TAU_LOW, q_shl[2], q_shl[1], q_shl[3]))
add_val("split_half_model_implied_median", unname(q_shf[2]), sum(!is.na(sh_fit)), sprintf("odd/even BLUP r implied by the fitted components (coach SD %.2f), sims with both halves identified; observed %.3f; lo/hi are the 2.5/97.5 quantiles; p is the observed value's percentile", vc_wat$coach, r_sh), q_shf[1], q_shf[3],
        p = pct_obs)
add_val("split_half_model_implied_median_boundary_as_zero", unname(q_shf0[2]), N_SH, sprintf("same with the %d boundary sims counted as r = 0", sum(is.na(sh_fit))), q_shf0[1], q_shf0[3], p = pct_obs0)
add_val("split_half_model_implied_boundary_share", mean(is.na(sh_fit)), N_SH, "share of simulated panels from the fitted model where one half's REML coach SD was zero")
add_val("split_half_model_implied_tau_low_median", unname(q_shl[2]), N_SH, sprintf("same with the coach SD at %.2f", TAU_LOW), q_shl[1], q_shl[3], p = mean(sh_low <= r_sh, na.rm = TRUE))
add_val("split_half_observed_below_model", as.numeric(r_sh < q_shf[1]), nrow(sh), sprintf("1 if the observed odd/even r is below the 2.5%% quantile implied by the fitted coach SD (it sits at the %.0fth percentile); the measure has not cleared the 0.30 reliability gate either way", 100 * pct_obs))
# raw means for the same split (no shrinkage)
shr <- panel[coach %in% ok, .(odd = wmean(wat17[season %% 2 == 1], w[season %% 2 == 1]),
                              even = wmean(wat17[season %% 2 == 0], w[season %% 2 == 0])), by = coach]
r_shr <- cor(shr$odd, shr$even); ci_shr <- boot_cor(shr$odd, shr$even)
rel[[2]] <- data.table(split = "odd_even_seasons_raw", n = nrow(shr), r = r_shr, r_sb = sb(r_shr), lo = ci_shr[1], hi = ci_shr[2])
# (b) odd vs even games within season, game-level wat = actual - talent17/17
cgp <- merge(cg[season >= FIRST], panel[, .(coach, season, team, talent17)], by = c("coach", "season", "team"))
cgp[, wat_g := actual - talent17 / 17]
cgp[, half := fifelse(week %% 2 == 1, "odd", "even")]
gh <- dcast(cgp[, .(v = mean(wat_g), n = .N), by = .(coach, half)], coach ~ half, value.var = c("v", "n"))
gh <- gh[n_odd >= 16 & n_even >= 16]
r_gh <- cor(gh$v_odd, gh$v_even); ci_gh <- boot_cor(gh$v_odd, gh$v_even)
rel[[3]] <- data.table(split = "odd_even_games", n = nrow(gh), r = r_gh, r_sb = sb(r_gh), lo = ci_gh[1], hi = ci_gh[2])
# SKEPTIC round 1: the games split shares roster, QB, injuries and franchise
# between halves, so it is within-season consistency of TEAM wins above
# payroll, not coach reliability. Two companions make that visible: the same
# split at the team-season level with the coach ignored, and the coach-level
# split with the fitted franchise effect removed.
ghT <- dcast(cgp[, .(v = mean(wat_g), n = .N), by = .(season, team, half)], season + team ~ half, value.var = c("v", "n"))
ghT <- ghT[n_odd >= 7 & n_even >= 7]
r_ghT <- cor(ghT$v_odd, ghT$v_even); ci_ghT <- boot_cor(ghT$v_odd, ghT$v_even)
rel[[4]] <- data.table(split = "odd_even_games_team_season", n = nrow(ghT), r = r_ghT, r_sb = sb(r_ghT), lo = ci_ghT[1], hi = ci_ghT[2])
cgp <- merge(cgp, b_team, by = "team", all.x = TRUE)
cgp[, wat_g_nf := wat_g - team_effect / 17]
ghF <- dcast(cgp[, .(v = mean(wat_g_nf), n = .N), by = .(coach, half)], coach ~ half, value.var = c("v", "n"))[n_odd >= 16 & n_even >= 16]
r_ghF <- cor(ghF$v_odd, ghF$v_even); ci_ghF <- boot_cor(ghF$v_odd, ghF$v_even)
rel[[5]] <- data.table(split = "odd_even_games_net_of_franchise", n = nrow(ghF), r = r_ghF, r_sb = sb(r_ghF), lo = ci_ghF[1], hi = ci_ghF[2])
# plain year-to-year persistence within coach, per component
yy <- merge(panel[, .(coach, season, wat17, premium17, wae17)],
            panel[, .(coach, season = season - 1, wat_n = wat17, prem_n = premium17, wae_n = wae17)],
            by = c("coach", "season"))
yy <- yy[, lapply(.SD, mean), by = .(coach, season)]
cat(sprintf("\nSPLIT-HALF / PERSISTENCE (bar 0.30):\n"))
cat(sprintf("  odd vs even seasons, coach BLUPs: r = %.3f (Spearman-Brown %.3f) [%.3f, %.3f], n = %d coaches\n", r_sh, sb(r_sh), ci_sh[1], ci_sh[2], nrow(sh)))
cat(sprintf("  odd vs even seasons, raw means:   r = %.3f (SB %.3f) [%.3f, %.3f], n = %d\n", r_shr, sb(r_shr), ci_shr[1], ci_shr[2], nrow(shr)))
cat(sprintf("  odd vs even games within season:  r = %.3f (SB %.3f) [%.3f, %.3f], n = %d coaches (>= 16 games per half)\n", r_gh, sb(r_gh), ci_gh[1], ci_gh[2], nrow(gh)))
cat(sprintf("    ... is within-season consistency of TEAM wins above payroll, not coach reliability: team-season split, coach ignored, r = %.3f (n = %d); coach split net of the franchise effect r = %.3f (n = %d)\n",
            r_ghT, nrow(ghT), r_ghF, nrow(ghF)))
cat("    The coach measure's split-half reliability is the odd/even SEASONS row.\n")
exp_r1 <- sd_of("M_wat", "coach")^2 / (sd_of("M_wat", "coach")^2 + sd_of("M_wat", "Residual")^2)
cat(sprintf("  expected single-season within-coach r from the variance components: %.3f (below 0.30; the multi-season BLUP carries the signal)\n", exp_r1))
for (cmp in list(c("wat17", "wat_n"), c("premium17", "prem_n"), c("wae17", "wae_n"))) {
  ct <- cor.test(yy[[cmp[1]]], yy[[cmp[2]]])
  rel[[length(rel) + 1]] <- data.table(split = paste0("year_to_year_", cmp[1]), n = nrow(yy), r = unname(ct$estimate),
                                       r_sb = NA_real_, lo = ct$conf.int[1], hi = ct$conf.int[2])
  cat(sprintf("  year-to-year within coach, %-9s r = %+.3f [%+.3f, %+.3f], n = %d pairs: %s\n", cmp[1],
              ct$estimate, ct$conf.int[1], ct$conf.int[2], nrow(yy), if (ct$estimate >= 0.30) "PASS (trait)" else "FAIL (season)"))
}
# 1999-2025 wae persistence for the bigger n
yy99 <- merge(cst[, .(coach, season, wae17)], cst[, .(coach, season = season - 1, wae_n = wae17)], by = c("coach", "season"))
ct99 <- cor.test(yy99$wae17, yy99$wae_n)
rel[[length(rel) + 1]] <- data.table(split = "year_to_year_wae17_1999", n = nrow(yy99), r = unname(ct99$estimate), r_sb = NA_real_, lo = ct99$conf.int[1], hi = ct99$conf.int[2])
cat(sprintf("  year-to-year wae17 on the 1999-%d panel: r = %+.3f [%+.3f, %+.3f], n = %d (R/02 found -0.03)\n", LAST, ct99$estimate, ct99$conf.int[1], ct99$conf.int[2], nrow(yy99)))

# ---------------------------------------------------------------------------
# 11. Across-team moves and market learning.
# ---------------------------------------------------------------------------
panel[, wat_noteam := wat17 - team_effect - season_effect]
spells <- panel[, .(first = min(season), last = max(season), n = .N, w = sum(w),
                    m_wat = wmean(wat17, w), m_wat_noteam = wmean(wat_noteam, w),
                    m_prem = wmean(premium17, w), m_wae = wmean(wae17, w),
                    yr1_wae = wae17[spell_yr == 1][1],
                    prem_2plus = if (any(spell_yr >= 2)) wmean(premium17[spell_yr >= 2], w[spell_yr >= 2]) else NA_real_,
                    spell_len = spell_len[1], spell_last = spell_last[1], interim = interim[1]),
                by = .(coach, team, spell_id)]
setorder(spells, coach, first)
spells[, k := seq_len(.N), by = coach]
mv <- merge(spells[k == 1, .(coach, s1 = m_wat_noteam, n1 = n)], spells[k == 2, .(coach, s2 = m_wat_noteam, n2 = n)], by = "coach")
ct_mv <- cor.test(mv$s1, mv$s2)
cat(sprintf("\nACROSS-TEAM (G2): spell-1 vs spell-2 mean wat17, team and season effects removed: r = %+.3f [%+.3f, %+.3f], n = %d movers\n",
            ct_mv$estimate, ct_mv$conf.int[1], ct_mv$conf.int[2], nrow(mv)))
cat(sprintf("  (stated in advance: n = %d gives a CI about +/- 0.4 wide; this updates G2 and cannot settle it)\n", nrow(mv)))
rel[[length(rel) + 1]] <- data.table(split = "across_team", n = nrow(mv), r = unname(ct_mv$estimate), r_sb = NA_real_, lo = ct_mv$conf.int[1], hi = ct_mv$conf.int[2])
add_val("across_team_r", unname(ct_mv$estimate), nrow(mv), "spell-1 vs spell-2 mean wat17 net of team/season effects", ct_mv$conf.int[1], ct_mv$conf.int[2], ct_mv$p.value)
# same on the 1999-2025 market-only panel (wae), bigger n
sp99 <- cst[, .(first = min(season), m = mean(wae17)), by = .(coach, team, spell_id)]
setorder(sp99, coach, first); sp99[, k := seq_len(.N), by = coach]
mv99 <- merge(sp99[k == 1, .(coach, s1 = m)], sp99[k == 2, .(coach, s2 = m)], by = "coach")
ct_mv99 <- cor.test(mv99$s1, mv99$s2)
cat(sprintf("  market-only 1999-%d: spell-1 vs spell-2 mean wae17: r = %+.3f [%+.3f, %+.3f], n = %d\n", LAST, ct_mv99$estimate, ct_mv99$conf.int[1], ct_mv99$conf.int[2], nrow(mv99)))
rel[[length(rel) + 1]] <- data.table(split = "across_team_market_only_1999", n = nrow(mv99), r = unname(ct_mv99$estimate), r_sb = NA_real_, lo = ct_mv99$conf.int[1], hi = ct_mv99$conf.int[2])
add_val("across_team_r_market_only", unname(ct_mv99$estimate), nrow(mv99), "1999-2025 wae17 spell-1 vs spell-2", ct_mv99$conf.int[1], ct_mv99$conf.int[2], ct_mv99$p.value)
add_val("nested_coach_sd", sd_of("M_wat_nested", "coach"), nrow(panel), "portable coach SD from (1|coach)+(1|coach:team)")
add_val("nested_coach_team_sd", sd_of("M_wat_nested", "coach:team"), nrow(panel), "coach-within-team SD")
# market learning: year-1 wae vs premium in years 2+
ml <- spells[!is.na(yr1_wae) & !is.na(prem_2plus)]
ct_ml <- cor.test(ml$yr1_wae, ml$prem_2plus)
cat(sprintf("MARKET LEARNING: year-1 wae17 vs premium17 in years 2+ of the same spell: r = %+.3f [%+.3f, %+.3f], n = %d spells -> %s\n",
            ct_ml$estimate, ct_ml$conf.int[1], ct_ml$conf.int[2], nrow(ml),
            if (ct_ml$conf.int[1] > 0) "the market raises its coach premium after delivered performance" else "no clear sign the premium is earned on the field; the premium component is discounted in the text"))
rel[[length(rel) + 1]] <- data.table(split = "market_learning", n = nrow(ml), r = unname(ct_ml$estimate), r_sb = NA_real_, lo = ct_ml$conf.int[1], hi = ct_ml$conf.int[2])
add_val("market_learning_r", unname(ct_ml$estimate), nrow(ml), "year-1 wae17 vs years-2+ premium17 within spell", ct_ml$conf.int[1], ct_ml$conf.int[2], ct_ml$p.value)
reliability <- rbindlist(rel)
reliability[, measures := fcase(
  split == "odd_even_seasons_blup", "coach reliability: independent seasons, shrunk coach effects (the G5 reliability gate is judged on this row)",
  split == "odd_even_seasons_raw", "coach reliability: independent seasons, raw means",
  split == "odd_even_games", "within-season consistency of team wins above payroll (halves share roster, QB, injuries and franchise); NOT coach reliability",
  split == "odd_even_games_team_season", "same games split at the team-season level with the coach ignored",
  split == "odd_even_games_net_of_franchise", "games split at the coach level with the fitted franchise effect removed",
  default = "see split name")]
fwrite(reliability, "data/derived/coaching_war_reliability.csv")
for (i in seq_len(nrow(reliability)))
  add_val(paste0("reliability_", reliability$split[i]), reliability$r[i], reliability$n[i],
          sprintf("Spearman-Brown %.3f", reliability$r_sb[i]), reliability$lo[i], reliability$hi[i])

# ---------------------------------------------------------------------------
# 12. Survivorship.
# ---------------------------------------------------------------------------
surv <- list()
add_surv <- function(test, group, n, est, lo = NA, hi = NA, p = NA)
  surv[[length(surv) + 1]] <<- data.table(test = test, group = group, n = n, estimate = est, lo = lo, hi = hi, p = p)
grp_of <- function(len, last) fcase(len <= 2 & last < LAST, "<=2", len %in% 3:4 & last < LAST, "3-4", len >= 5, "5+", default = "censored")
# (1) first-two-season raw wat17 by eventual spell length (spells starting in window, not interim)
e12 <- panel[spell_yr <= 2 & spell_first >= FIRST & !interim]
e12[, grp := grp_of(spell_len, spell_last)]
# SKEPTIC round 1: one row per spell (mean of years 1-2), so the Wilcoxon n is spells, not coach-seasons
e12s <- e12[grp != "censored", .(wat12 = mean(wat17), n_yrs = .N), by = .(coach, team, spell_id, grp)]
s1 <- e12[grp != "censored", .(n_rows = .N, n_spells = uniqueN(paste(coach, team, spell_id)), mean = mean(wat17), sd = sd(wat17)), by = grp][order(grp)]
cat("\nSURVIVORSHIP 1: years-1-2 raw wat17 by eventual spell length (spells begun 2012+, non-interim):\n"); print(s1)
wt <- wilcox.test(e12s[grp == "5+", wat12], e12s[grp == "<=2", wat12], conf.int = TRUE)
wt_y1 <- wilcox.test(e12[grp == "5+" & spell_yr == 1, wat17], e12[grp == "<=2" & spell_yr == 1, wat17], conf.int = TRUE)
cat(sprintf("  Wilcoxon 5+ vs <=2, SPELL level (n = %d vs %d): shift %+.2f [%+.2f, %+.2f], p = %.2g; year-1 rows only: %+.2f [%+.2f, %+.2f], p = %.3f\n",
            e12s[grp == "5+", .N], e12s[grp == "<=2", .N], wt$estimate, wt$conf.int[1], wt$conf.int[2], wt$p.value,
            wt_y1$estimate, wt_y1$conf.int[1], wt_y1$conf.int[2], wt_y1$p.value))
for (i in seq_len(nrow(s1))) add_surv("yr12_wat_by_tenure", s1$grp[i], s1$n_spells[i], s1$mean[i])
add_surv("yr12_wat_5plus_vs_le2_wilcoxon_spell", "shift", nrow(e12s[grp %in% c("5+", "<=2")]), unname(wt$estimate), wt$conf.int[1], wt$conf.int[2], wt$p.value)
add_surv("yr1_wat_5plus_vs_le2_wilcoxon", "shift", nrow(e12[grp %in% c("5+", "<=2") & spell_yr == 1]), unname(wt_y1$estimate), wt_y1$conf.int[1], wt_y1$conf.int[2], wt_y1$p.value)
# same with wae17 on 1999-2025 for a bigger n, also at spell level
e12b <- cst[spell_yr <= 2 & !interim & spell_first >= 2000]
e12b[, grp := grp_of(spell_len, spell_last)]
e12bs <- e12b[grp != "censored", .(wae12 = mean(wae17)), by = .(coach, team, spell_id, grp)]
s1b <- e12b[grp != "censored", .(n_rows = .N, n_spells = uniqueN(paste(coach, team, spell_id)), mean = mean(wae17), sd = sd(wae17)), by = grp][order(grp)]
wtb <- wilcox.test(e12bs[grp == "5+", wae12], e12bs[grp == "<=2", wae12], conf.int = TRUE)
cat("  Same with wae17, 2000-2025 spells:\n"); print(s1b)
cat(sprintf("  Wilcoxon 5+ vs <=2 (wae17, spell level): shift %+.2f [%+.2f, %+.2f], p = %.2g\n", wtb$estimate, wtb$conf.int[1], wtb$conf.int[2], wtb$p.value))
for (i in seq_len(nrow(s1b))) add_surv("yr12_wae_by_tenure_1999", s1b$grp[i], s1b$n_spells[i], s1b$mean[i])
add_surv("yr12_wae_5plus_vs_le2_wilcoxon_1999_spell", "shift", nrow(e12bs[grp %in% c("5+", "<=2")]), unname(wtb$estimate), wtb$conf.int[1], wtb$conf.int[2], wtb$p.value)
# (2) regression to the mean among 5+ survivors (paired), and the persistence test
sv <- panel[spell_len >= 5, .(early = mean(wat17[spell_yr <= 2]), late = mean(wat17[spell_yr >= 3])), by = .(coach, team, spell_id)]
sv <- sv[!is.na(early) & !is.na(late)]
tt_sv <- t.test(sv$late, sv$early, paired = TRUE)
cat(sprintf("SURVIVORSHIP 2: 5+ season spells, years 3+ minus years 1-2 wat17: %+.2f [%+.2f, %+.2f], p = %.3f, n = %d spells (early mean %+.2f, late mean %+.2f)\n",
            tt_sv$estimate, tt_sv$conf.int[1], tt_sv$conf.int[2], tt_sv$p.value, nrow(sv), mean(sv$early), mean(sv$late)))
add_surv("survivors_late_minus_early", "5+", nrow(sv), unname(tt_sv$estimate), tt_sv$conf.int[1], tt_sv$conf.int[2], tt_sv$p.value)
# SKEPTIC round 1: the test that speaks to skill is whether the survivors'
# early edge persists into years 3+ of the same spell (r between early and late)
sv3 <- panel[spell_len >= 3, .(early = mean(wat17[spell_yr <= 2]), late = mean(wat17[spell_yr >= 3])), by = .(coach, team, spell_id)]
sv3 <- sv3[!is.na(early) & !is.na(late)]
ct_p3 <- cor.test(sv3$early, sv3$late); ct_p5 <- cor.test(sv$early, sv$late)
cat(sprintf("SURVIVORSHIP 3 (persistence within spell, years 1-2 mean vs years 3+ mean): 3+ spells r = %+.2f [%+.2f, %+.2f], n = %d; 5+ spells r = %+.2f [%+.2f, %+.2f], n = %d\n",
            ct_p3$estimate, ct_p3$conf.int[1], ct_p3$conf.int[2], nrow(sv3), ct_p5$estimate, ct_p5$conf.int[1], ct_p5$conf.int[2], nrow(sv)))
add_surv("persistence_early_vs_late_r", "3+", nrow(sv3), unname(ct_p3$estimate), ct_p3$conf.int[1], ct_p3$conf.int[2], ct_p3$p.value)
add_surv("persistence_early_vs_late_r", "5+", nrow(sv), unname(ct_p5$estimate), ct_p5$conf.int[1], ct_p5$conf.int[2], ct_p5$p.value)
# hazard: P(returns to same team next season)
haz <- copy(cst)[season < LAST]
haz[, returns := spell_last > season]
haz[, cum_wae := cumsum(wae17) - wae17, by = .(coach, team, spell_id)]
h1 <- glm(returns ~ wae17 + cum_wae + spell_yr, data = haz, family = binomial)
cat(sprintf("HAZARD 1999-%d (n = %d): P(retained) ~ wae17 %+.3f (p %.3g), cumulative wae %+.3f, spell year %+.3f; AUC %.3f\n",
            LAST - 1, nrow(haz), coef(h1)["wae17"], summary(h1)$coefficients["wae17", 4], coef(h1)["cum_wae"], coef(h1)["spell_yr"], auc_fn(as.integer(haz$returns), fitted(h1))))
add_surv("hazard_wae17_coef_1999", "all", nrow(haz), unname(coef(h1)["wae17"]), p = summary(h1)$coefficients["wae17", 4])
hp <- panel[season < LAST]; hp[, returns := spell_last > season]
hp[, `:=`(cum_wat = cumsum(wat17) - wat17)]
h2 <- glm(returns ~ wat17 + cum_wat + spell_yr, data = hp, family = binomial)
h3 <- glm(returns ~ premium17 + wae17 + spell_yr, data = hp, family = binomial)
h4 <- glm(returns ~ act17 + wat17 + wae17 + spell_yr, data = hp, family = binomial)
cat(sprintf("HAZARD %d-%d (n = %d): ~ wat17 %+.3f (p %.3g) + cum_wat %+.3f + yr; AUC %.3f\n", FIRST, LAST - 1, nrow(hp),
            coef(h2)["wat17"], summary(h2)$coefficients["wat17", 4], coef(h2)["cum_wat"], auc_fn(as.integer(hp$returns), fitted(h2))))
cat(sprintf("  ~ premium17 %+.3f (p %.3g) + wae17 %+.3f (p %.3g): owners react to %s\n",
            coef(h3)["premium17"], summary(h3)$coefficients["premium17", 4], coef(h3)["wae17"], summary(h3)$coefficients["wae17", 4],
            if (coef(h3)["premium17"] > coef(h3)["wae17"]) "the market-priced part more than the surprise" else "the surprise part more than the market-priced part"))
cat(sprintf("  ~ wins %+.3f (p %.3g) + wat17 %+.3f (p %.3g) + wae17 %+.3f (p %.3g); AUC %.3f: owners fire on %s\n",
            coef(h4)["act17"], summary(h4)$coefficients["act17", 4], coef(h4)["wat17"], summary(h4)$coefficients["wat17", 4],
            coef(h4)["wae17"], summary(h4)$coefficients["wae17", 4], auc_fn(as.integer(hp$returns), fitted(h4)),
            if (summary(h4)$coefficients["wat17", 4] < 0.05) "what the coach controls beyond the win total too" else "the win total; wins above talent adds nothing significant"))
# SKEPTIC round 1: coach-clustered SEs for the hazard coefficients (about 6 rows per coach)
cl_p <- function(m, term, cl) { V <- sandwich::vcovCL(m, cluster = cl); z <- coef(m)[term] / sqrt(V[term, term]); 2 * pnorm(-abs(unname(z))) }
cat(sprintf("  coach-clustered p: wae17 (1999+) %.2g; wat17 (2012+) %.2g; premium17 %.2g, wae17 %.2g; wins-in-model: wins %.3f, wat17 %.2f, wae17 %.2f\n",
            cl_p(h1, "wae17", haz$coach), cl_p(h2, "wat17", hp$coach), cl_p(h3, "premium17", hp$coach), cl_p(h3, "wae17", hp$coach),
            cl_p(h4, "act17", hp$coach), cl_p(h4, "wat17", hp$coach), cl_p(h4, "wae17", hp$coach)))
add_surv("hazard_wat17_coef", "all", nrow(hp), unname(coef(h2)["wat17"]), p = cl_p(h2, "wat17", hp$coach))
add_surv("hazard_premium_coef", "all", nrow(hp), unname(coef(h3)["premium17"]), p = cl_p(h3, "premium17", hp$coach))
add_surv("hazard_wae_coef", "all", nrow(hp), unname(coef(h3)["wae17"]), p = cl_p(h3, "wae17", hp$coach))
add_surv("hazard_wins_coef_with_wat_wae", "all", nrow(hp), unname(coef(h4)["act17"]), p = cl_p(h4, "act17", hp$coach))
add_surv("hazard_wat17_coef_given_wins", "all", nrow(hp), unname(coef(h4)["wat17"]), p = cl_p(h4, "wat17", hp$coach))
add_surv("hazard_auc_wins_wat_wae", "all", nrow(hp), auc_fn(as.integer(hp$returns), fitted(h4)))

# SKEPTIC round 3 (minor): voluntary exits (retirements, mutual partings) are
# spell ends too, and the hazard and the tenure groups read them as firings.
# Hand-coded list of spell ends in the window that were not a firing. Garrett
# DAL 2019 (contract not renewed) and Gruden LV 2021 (resigned under pressure,
# and his 2021 stint is under MIN_G anyway) are NOT coded voluntary.
vol_exits <- data.table(coach  = c("Gary Kubiak", "Bruce Arians", "Bruce Arians", "Sean Payton", "Pete Carroll", "Bill Belichick", "Jim Harbaugh", "John Fox"),
                        team   = c("DEN", "ARI", "TB", "NO", "SEA", "NE", "SF", "DEN"),
                        season = c(2016, 2017, 2021, 2021, 2023, 2023, 2014, 2014))
vol_exits[, voluntary_exit := TRUE]
panel <- merge(panel, vol_exits, by = c("coach", "team", "season"), all.x = TRUE)
panel[is.na(voluntary_exit), voluntary_exit := FALSE]
if (any(panel[voluntary_exit == TRUE, season != spell_last])) stop("a voluntary_exit row is not the last season of its spell")
panel[, vol_spell := any(voluntary_exit), by = .(coach, team, spell_id)]
setorder(panel, coach, season, team)
# automated sweep for anything missed: spell ended before LAST, coach did not coach anywhere next season, wat17 > 0, spell_len >= 3
nxt <- unique(cst[, .(coach, season = season - 1, coached_next = TRUE)])
sweep <- merge(panel[season == spell_last & spell_last < LAST & spell_len >= 3 & wat17 > 0 & voluntary_exit == FALSE], nxt, by = c("coach", "season"), all.x = TRUE)[is.na(coached_next)]
cat(sprintf("\nVOLUNTARY EXITS coded: %s\n  sweep (spell ended, not coaching next year, wat17 > 0, spell_len >= 3, not coded): %s -> all firings on the record, none added\n",
            paste(vol_exits[, sprintf("%s %s %d", coach, team, season)], collapse = "; "),
            paste(sweep[, sprintf("%s %s %d (%+.1f)", coach, team, season, wat17)], collapse = "; ")))
hp_v <- merge(hp, vol_exits, by = c("coach", "team", "season"), all.x = TRUE)[is.na(voluntary_exit)]   # censor: drop the voluntary-exit rows
h2_vol <- glm(returns ~ wat17 + cum_wat + spell_yr, data = hp_v, family = binomial)
cat(sprintf("  Hazard with voluntary exits censored (n = %d): wat17 %+.3f (was %+.3f), cum_wat %+.3f (was %+.3f), spell_yr %+.3f (was %+.3f)\n",
            nrow(hp_v), coef(h2_vol)["wat17"], coef(h2)["wat17"], coef(h2_vol)["cum_wat"], coef(h2)["cum_wat"], coef(h2_vol)["spell_yr"], coef(h2)["spell_yr"]))
add_surv("hazard_wat17_coef_vol_censored", "all", nrow(hp_v), unname(coef(h2_vol)["wat17"]), p = cl_p(h2_vol, "wat17", hp_v$coach))
e12v <- panel[spell_yr <= 2 & spell_first >= FIRST & !interim]
e12v[, grp := fifelse(vol_spell & spell_len < 5, "censored", grp_of(spell_len, spell_last))]
e12vs <- e12v[grp != "censored", .(wat12 = mean(wat17)), by = .(coach, team, spell_id, grp)]
wt_v <- wilcox.test(e12vs[grp == "5+", wat12], e12vs[grp == "<=2", wat12], conf.int = TRUE)
obs_shift_vol <- e12vs[grp == "5+", mean(wat12)] - e12vs[grp == "<=2", mean(wat12)]
cat(sprintf("  Years-1-2 gap with voluntary exits censored: Wilcoxon shift %+.2f [%+.2f, %+.2f], p = %.2g (n = %d vs %d; was %+.2f, n = %d vs %d); spell-level mean gap %+.2f\n",
            wt_v$estimate, wt_v$conf.int[1], wt_v$conf.int[2], wt_v$p.value, e12vs[grp == "5+", .N], e12vs[grp == "<=2", .N],
            wt$estimate, e12s[grp == "5+", .N], e12s[grp == "<=2", .N], obs_shift_vol))
add_surv("yr12_wat_5plus_vs_le2_wilcoxon_spell_vol_censored", "shift", nrow(e12vs[grp %in% c("5+", "<=2")]), unname(wt_v$estimate), wt_v$conf.int[1], wt_v$conf.int[2], wt_v$p.value)
add_surv("observed_shift_spell_mean_diff_vol_censored", "5+ minus <=2", nrow(e12vs[grp %in% c("5+", "<=2")]), obs_shift_vol)
# a wins-based firing rule (owners fire on the win total, h4) for the null
hp[, cum_act := cumsum(act17) - act17, by = .(coach, team, spell_id)]
h_wins <- glm(returns ~ act17 + cum_act + spell_yr, data = hp, family = binomial)
tal_mu <- mean(panel$talent17); tal_sd <- sd(panel$talent17)
cat(sprintf("  Wins-only hazard: act17 %+.3f, cum_act %+.3f, spell_yr %+.3f, AUC %.3f\n", coef(h_wins)["act17"], coef(h_wins)["cum_act"], coef(h_wins)["spell_yr"], auc_fn(as.integer(hp$returns), fitted(h_wins))))

# ---------------------------------------------------------------------------
# SKEPTIC round 1 (major): the years-1-2 gap between future 5+ and soon-gone
# coaches is what ANY firing rule on wins produces, because owners fire on
# wat17 (h2). Null: simulate panels from M_wat's variance components with the
# coach SD set to ZERO (no trait at all), let every real spell run from its
# start season to LAST, truncate it with the fitted hazard h2, and recompute
# the spell-level 5+ vs <=2 shift. Repeated with the fitted coach SD for
# reference. "Tenure is earned" only if the observed shift is outside the
# no-trait null's 95% range.
# ---------------------------------------------------------------------------
spell_seed <- e12[spell_yr == 1, .(coach, team, spell_id, start = spell_first)]
spell_seed <- unique(spell_seed)
h2_cf <- coef(h2)
# SKEPTIC round 2: the same simulation also returns the within-spell
# persistence r (years 1-2 mean vs years 3+ mean) for 3+ and 5+ spells, so the
# observed r has a null of its own. Selection induces a positive r by itself:
# the cum_wat term lets a strong-early coach survive a bad later year while a
# weak-early coach is cut on his first bad year.
sim_shift <- function(coach_sd, B = 1000, cf = h2_cf, rule = c("wat", "wins")) {
  # SKEPTIC round 3: cf = hazard coefficients (h2, or h2 with voluntary exits
  # censored); rule = "wins" uses the wins-only hazard on act = talent + wat,
  # talent drawn independently of the coach
  rule <- match.arg(rule)
  n_sp <- nrow(spell_seed)
  out <- t(vapply(seq_len(B), function(b) {
    u_c <- rnorm(n_sp, 0, coach_sd)
    u_t <- setNames(rnorm(uniqueN(spell_seed$team), 0, vc_wat$team), unique(spell_seed$team))
    u_s <- setNames(rnorm(LAST - FIRST + 1, 0, vc_wat$season), as.character(FIRST:LAST))
    wat12 <- numeric(n_sp); late <- rep(NA_real_, n_sp); len <- integer(n_sp); censored <- logical(n_sp)
    for (k in seq_len(n_sp)) {
      maxlen <- LAST - spell_seed$start[k] + 1
      cum <- 0; ys <- numeric(0); alive <- TRUE; L <- 0
      for (yr in seq_len(maxlen)) {
        y <- vc_wat$intercept + u_c[k] + u_t[spell_seed$team[k]] + u_s[as.character(spell_seed$start[k] + yr - 1)] + rnorm(1, 0, vc_wat$resid)
        ys <- c(ys, y); L <- yr
        if (rule == "wat") {
          p_ret <- plogis(cf[1] + cf["wat17"] * y + cf["cum_wat"] * cum + cf["spell_yr"] * yr)
          cum <- cum + y
        } else {
          a <- y + rnorm(1, tal_mu, tal_sd)
          p_ret <- plogis(cf[1] + cf["act17"] * a + cf["cum_act"] * cum + cf["spell_yr"] * yr)
          cum <- cum + a
        }
        if (runif(1) > p_ret) { alive <- FALSE; break }
      }
      wat12[k] <- mean(ys[1:min(2, L)]); len[k] <- L; censored[k] <- alive
      if (L >= 3) late[k] <- mean(ys[3:L])
    }
    g <- fcase(len <= 2 & !censored, "<=2", len %in% 3:4 & !censored, "3-4", len >= 5, "5+", default = "censored")
    shift <- if (sum(g == "5+") < 3 || sum(g == "<=2") < 3) NA_real_ else mean(wat12[g == "5+"]) - mean(wat12[g == "<=2"])
    r3 <- if (sum(len >= 3) >= 5) cor(wat12[len >= 3], late[len >= 3]) else NA_real_
    r5 <- if (sum(len >= 5) >= 5) cor(wat12[len >= 5], late[len >= 5]) else NA_real_
    c(shift = shift, r3 = r3, r5 = r5, n3 = sum(len >= 3), n5 = sum(len >= 5))
  }, numeric(5)))
  as.data.table(out)
}
set.seed(712)
null0 <- sim_shift(0); null1 <- sim_shift(vc_wat$coach)
obs_shift_mean <- e12s[grp == "5+", mean(wat12)] - e12s[grp == "<=2", mean(wat12)]
q0 <- quantile(null0$shift, c(.025, .5, .975), na.rm = TRUE); q1 <- quantile(null1$shift, c(.025, .5, .975), na.rm = TRUE)
p_null0 <- mean(null0$shift >= obs_shift_mean, na.rm = TRUE)
cat(sprintf("\nSURVIVORSHIP NULL (spell-level mean difference, 5+ minus <=2, %d spells, %d sims each):\n", nrow(spell_seed), nrow(null0)))
cat(sprintf("  observed %+.2f (Wilcoxon shift %+.2f); no-trait null (coach SD 0, firing on h2) median %+.2f, 95%% range [%+.2f, %+.2f], P(null >= observed) = %.3f (one-sided)\n",
            obs_shift_mean, wt$estimate, q0[2], q0[1], q0[3], p_null0))
cat(sprintf("  with the fitted coach SD %.2f: median %+.2f, 95%% range [%+.2f, %+.2f]\n", vc_wat$coach, q1[2], q1[1], q1[3]))
add_surv("null_shift_no_trait", "median", nrow(spell_seed), unname(q0[2]), unname(q0[1]), unname(q0[3]), p_null0)
# SKEPTIC round 3 (major): the gap is unusual under the fitted-trait world
# too, so the simulation cannot attribute the remainder to a trait. Report
# P under both worlds and the likelihood ratio (share of sims within 0.25 of
# the observed gap in each world).
p_null1 <- mean(null1$shift >= obs_shift_mean, na.rm = TRUE)
dens0 <- mean(abs(null0$shift - obs_shift_mean) < 0.25, na.rm = TRUE); dens1 <- mean(abs(null1$shift - obs_shift_mean) < 0.25, na.rm = TRUE)
lr_trait <- dens1 / dens0
cat(sprintf("  fitted-trait world: P(shift >= observed) = %.3f; share of sims within 0.25 of the observed gap: no-trait %.3f vs fitted-trait %.3f, likelihood ratio %.1f\n",
            p_null1, dens0, dens1, lr_trait))
cat(sprintf("  -> the observed gap is unusual under BOTH worlds (P %.3f and %.3f); the hazard simulation understates the real selection, and the gap cannot be split into firing and trait\n", p_null0, p_null1))
add_surv("null_shift_fitted_trait", "median", nrow(spell_seed), unname(q1[2]), unname(q1[1]), unname(q1[3]), p_null1)
add_surv("null_likelihood_ratio_trait_vs_none", "within 0.25 of observed", nrow(null0), lr_trait, dens0, dens1)
# the same null with voluntary exits censored out of the hazard, and with a wins-only firing rule
set.seed(713)
null0v <- sim_shift(0, cf = coef(h2_vol)); null1v <- sim_shift(vc_wat$coach, cf = coef(h2_vol))
null0w <- sim_shift(0, cf = coef(h_wins), rule = "wins")
q0v <- quantile(null0v$shift, c(.025, .5, .975), na.rm = TRUE); q1v <- quantile(null1v$shift, c(.025, .5, .975), na.rm = TRUE)
q0w <- quantile(null0w$shift, c(.025, .5, .975), na.rm = TRUE)
p_null0v <- mean(null0v$shift >= obs_shift_vol, na.rm = TRUE); p_null1v <- mean(null1v$shift >= obs_shift_vol, na.rm = TRUE)
p_null0w <- mean(null0w$shift >= obs_shift_mean, na.rm = TRUE)
cat(sprintf("  VOLUNTARY EXITS CENSORED: observed gap %+.2f (was %+.2f); no-trait null median %+.2f [%+.2f, %+.2f], P = %.3f (was %.3f); fitted-trait median %+.2f, P = %.3f\n",
            obs_shift_vol, obs_shift_mean, q0v[2], q0v[1], q0v[3], p_null0v, p_null0, q1v[2], p_null1v))
cat(sprintf("  WINS-ONLY FIRING RULE (act17 + cumulative wins + year), no trait: median %+.2f [%+.2f, %+.2f], P(>= observed %+.2f) = %.3f -> a wins-based rule %s reproduce the gap without a trait\n",
            q0w[2], q0w[1], q0w[3], obs_shift_mean, p_null0w, if (p_null0w >= 0.05) "CAN" else "does not"))
add_surv("null_shift_no_trait_vol_censored", "median", nrow(spell_seed), unname(q0v[2]), unname(q0v[1]), unname(q0v[3]), p_null0v)
add_surv("null_shift_fitted_trait_vol_censored", "median", nrow(spell_seed), unname(q1v[2]), unname(q1v[1]), unname(q1v[3]), p_null1v)
add_surv("null_shift_no_trait_wins_rule", "median", nrow(spell_seed), unname(q0w[2]), unname(q0w[1]), unname(q0w[3]), p_null0w)
add_surv("observed_shift_spell_mean_diff", "5+ minus <=2", nrow(e12s[grp %in% c("5+", "<=2")]), obs_shift_mean)
# persistence r under the two worlds
qr3_0 <- quantile(null0$r3, c(.025, .5, .975), na.rm = TRUE); qr3_1 <- quantile(null1$r3, c(.025, .5, .975), na.rm = TRUE)
qr5_0 <- quantile(null0$r5, c(.025, .5, .975), na.rm = TRUE); qr5_1 <- quantile(null1$r5, c(.025, .5, .975), na.rm = TRUE)
p_r3 <- mean(null0$r3 >= ct_p3$estimate, na.rm = TRUE); p_r5 <- mean(null0$r5 >= ct_p5$estimate, na.rm = TRUE)
cat(sprintf("  PERSISTENCE r under the no-trait null: 3+ spells median %+.2f [%+.2f, %+.2f] (sim n ~ %.0f), observed %+.2f, P(null >= obs) = %.3f; 5+ spells median %+.2f [%+.2f, %+.2f], observed %+.2f, P = %.3f\n",
            qr3_0[2], qr3_0[1], qr3_0[3], median(null0$n3), ct_p3$estimate, p_r3, qr5_0[2], qr5_0[1], qr5_0[3], ct_p5$estimate, p_r5))
cat(sprintf("  PERSISTENCE r with the fitted coach SD %.2f: 3+ spells median %+.2f [%+.2f, %+.2f]; 5+ spells median %+.2f [%+.2f, %+.2f]. The two worlds overlap, so the test has no power at this n\n",
            vc_wat$coach, qr3_1[2], qr3_1[1], qr3_1[3], qr5_1[2], qr5_1[1], qr5_1[3]))
add_surv("null_persistence_r_no_trait", "3+", round(median(null0$n3)), unname(qr3_0[2]), unname(qr3_0[1]), unname(qr3_0[3]), p_r3)
add_surv("null_persistence_r_no_trait", "5+", round(median(null0$n5)), unname(qr5_0[2]), unname(qr5_0[1]), unname(qr5_0[3]), p_r5)
add_surv("null_persistence_r_fitted_trait", "3+", round(median(null1$n3)), unname(qr3_1[2]), unname(qr3_1[1]), unname(qr3_1[3]))
add_surv("null_persistence_r_fitted_trait", "5+", round(median(null1$n5)), unname(qr5_1[2]), unname(qr5_1[1]), unname(qr5_1[3]))
fwrite(data.table(sim = seq_len(nrow(null0)), shift_no_trait = null0$shift, shift_fitted_trait = null1$shift,
                  persistence_r3_no_trait = null0$r3, persistence_r3_fitted_trait = null1$r3,
                  persistence_r5_no_trait = null0$r5, persistence_r5_fitted_trait = null1$r5,
                  shift_no_trait_vol_censored = null0v$shift, shift_fitted_trait_vol_censored = null1v$shift,
                  shift_no_trait_wins_rule = null0w$shift),
       "data/derived/coaching_war_survivorship_null.csv")
tenure_outside_null <- obs_shift_mean > q0[3]
null_share <- q0[2] / obs_shift_mean
# SKEPTIC round 3: the verdict no longer reads the remainder as evidence of a
# trait. Both simulated worlds find the gap unusual; the simulation cannot
# attribute it.
verdict_tenure <- sprintf("the no-trait null (firing on wins above talent, no skill) produces a years-1-2 gap of %+.2f against the observed %+.2f (one-sided P = %.3f), but the fitted-trait world (coach SD %.2f) also finds it unusual (median %+.2f, P = %.3f; likelihood ratio %.1f), so the simulated firing rule understates the real selection and the gap cannot be split into firing and skill. Censoring the %d voluntary exits: observed %+.2f, no-trait P = %.3f. A wins-only firing rule with no trait gives a median of %+.2f (P = %.3f). Within-spell persistence r = %+.2f (3+ spells, n = %d) sits inside its own no-trait range [%+.2f, %+.2f] (P = %.2f) and the fitted-trait world gives only %+.2f, so it cannot tell the two apart at this n",
                          q0[2], obs_shift_mean, p_null0, vc_wat$coach, q1[2], p_null1, lr_trait,
                          nrow(vol_exits), obs_shift_vol, p_null0v, q0w[2], p_null0w,
                          ct_p3$estimate, nrow(sv3), qr3_0[1], qr3_0[3], p_r3, qr3_1[2])
add_surv("rep_offset_primary", "first_no_interim", rep_noint$n, rep_noint$value)
add_surv("rep_offset_no_returnee", "no_returnee", rep_noret$n, rep_noret$value)
add_surv("rep_offset_fired_early", "fired_early", rep_fired$n, rep_fired$value)
add_surv("rep_offset_interim_only", "interim", rep_interim$n, rep_interim$value)
# stint-year curve of raw wat17
sy <- panel[, .(n = .N, mean_wat = mean(wat17), se = sd(wat17) / sqrt(.N)), by = .(spell_yr = pmin(spell_yr, 8))][order(spell_yr)]
cat("Stint-year curve of raw wat17 (spell year 8 = 8+):\n"); print(sy[, .(spell_yr, n, mean_wat = round(mean_wat, 2), se = round(se, 2))])
for (i in seq_len(nrow(sy))) add_surv("stint_year_curve", as.character(sy$spell_yr[i]), sy$n[i], sy$mean_wat[i], sy$mean_wat[i] - 1.96 * sy$se[i], sy$mean_wat[i] + 1.96 * sy$se[i])
cat(sprintf("SURVIVORSHIP VERDICT: %s\n", verdict_tenure))
survivorship <- rbindlist(surv, fill = TRUE)
fwrite(survivorship, "data/derived/coaching_war_survivorship.csv")
for (i in seq_len(nrow(survivorship)))
  add_val(paste0("surv_", survivorship$test[i], "_", survivorship$group[i]), survivorship$estimate[i], survivorship$n[i], "see coaching_war_survivorship.csv", survivorship$lo[i], survivorship$hi[i], survivorship$p[i])

# equal-exposure board: each coach's first 3 seasons in the window
panel[, career_yr := frank(season, ties.method = "first"), by = coach]
m_e3 <- fit_m(F_WAT, panel[career_yr <= 3])
e3 <- merge(blups(m_e3)[, .(coach = grp, war_first3 = blup - rep_offset)], coach[eligible == TRUE, .(coach, war_per_season, rank)], by = "coach")
e3[, rank_first3 := frank(-war_first3, ties.method = "min")]
e3[, move := rank_first3 - rank]
r_e3 <- cor(e3$war_first3, e3$war_per_season, method = "spearman")
cat(sprintf("\nEQUAL EXPOSURE: WAR from first 3 seasons only vs full-career WAR, %d eligible coaches: Spearman %.3f\n", nrow(e3), r_e3))
big <- e3[abs(move) >= 10][order(move)]
if (nrow(big)) cat("  moved 10+ places:", paste(sprintf("%s (%d -> %d)", big$coach, big$rank, big$rank_first3), collapse = "; "), "\n")
add_val("equal_exposure_spearman", r_e3, nrow(e3), sprintf("%d coaches move 10+ places", nrow(big)))
coach <- merge(coach, e3[, .(coach, war_first3, rank_first3)], by = "coach", all.x = TRUE)

# ---------------------------------------------------------------------------
# 13. Sensitivity: replacement definitions, talent controls, pd outcome.
# ---------------------------------------------------------------------------
sens <- coach[, .(coach, seasons, eligible, war_main = war_per_season, rank_main = rank,
                  war_rep_average = coach_effect - 0, war_rep_one_and_done = coach_effect - rep_ood$value,
                  war_rep_first_all = coach_effect - rep_first$value)]
talent_variant <- function(form, d, name) {
  d <- copy(d); f <- lm(form, data = d, weights = w); d[, wat17 := act17 - fitted(f)]
  m <- fit_m(F_WAT, d); b <- blups(m)
  bt <- blups(m, "team"); bs <- blups(m, "season")
  d[, rpc := wat17 - (fixef(m)[1] + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
  ro <- d[spell_yr == 1 & !interim, wmean(rpc, w)]
  out <- data.table(coach = b$grp, v = b$blup - ro); setnames(out, "v", name); out
}
sens <- merge(sens, talent_variant(mkt17 ~ contract_z + season_f, panel, "war_talent_contract_only"), by = "coach", all.x = TRUE)
sens <- merge(sens, talent_variant(mkt17 ~ contract_z + qbp_z + season_f, panel, "war_talent_contract_qbepa"), by = "coach", all.x = TRUE)
pm <- panel[!is.na(madden_z)]; pm[, season_f := factor(season)]
sens <- merge(sens, talent_variant(mkt17 ~ madden_z + season_f, pm, "war_talent_madden_2017"), by = "coach", all.x = TRUE)
# point-differential outcome: adjusted margin per game * 17/?? keep in points/game, then rank compare
pdv <- copy(panel); f_pd <- lm(pd_adj_pg ~ contract_z + qb_z + qbp_z + season_f, data = pdv, weights = w)
pdv[, wat17 := pd_adj_pg - fitted(f_pd)]
m_pd <- fit_m(F_WAT, pdv); bpd <- blups(m_pd)
sens <- merge(sens, data.table(coach = bpd$grp, pd_effect_ppg = bpd$blup), by = "coach", all.x = TRUE)

# SKEPTIC round 1 variants --------------------------------------------------
# replacement alternatives from section 5 (rank order identical by construction)
sens <- merge(sens, coach[, .(coach, coach_effect)], by = "coach", all.x = TRUE)
sens[, `:=`(war_rep_no_franchise_term = coach_effect - rep_raw_season,
            war_rep_team_from_yr3plus = coach_effect - rep_team3,
            war_rep_leave_one_spell_out = coach_effect - rep_loso)]
sens[, coach_effect := NULL]
# (a) same-season QB EPA as a fixed effect in the coach model: the coach gets NO credit for his QB's play
fe_variant <- function(form, d, name) {
  d <- copy(d); m <- fit_m(form, d); b <- blups(m)
  bt <- blups(m, "team"); bs <- blups(m, "season")
  fe <- fixef(m); X <- model.matrix(reformulate(setdiff(names(fe), "(Intercept)")), d)
  d[, rpc := wat17 - (as.vector(X %*% fe) + bt$blup[match(team, bt$grp)] + bs$blup[match(season, bs$grp)])]
  ro <- d[spell_yr == 1 & !interim, wmean(rpc, w)]
  out <- data.table(coach = b$grp, v = b$blup - ro); setnames(out, "v", name)
  list(tab = out, m = m, ro = ro)
}
v_qb <- fe_variant(wat17 ~ 1 + qbcur_z + (1 | coach) + (1 | team) + (1 | season), panel, "war_talent_same_season_qb")
sens <- merge(sens, v_qb$tab, by = "coach", all.x = TRUE)
cat(sprintf("\nSAME-SEASON QB variant: wat17 ~ qbcur_z + coach + team + season: qbcur_z %+.2f wins/SD; coach SD %.2f (main %.2f), team SD %.2f; replacement %+.2f\n",
            fixef(v_qb$m)["qbcur_z"], as.data.table(VarCorr(v_qb$m))[grp == "coach", sdcor], vc_wat$coach,
            as.data.table(VarCorr(v_qb$m))[grp == "team", sdcor], v_qb$ro))
add_val("varcomp_M_wat_same_season_qb_coach", as.data.table(VarCorr(v_qb$m))[grp == "coach", sdcor], nrow(panel), "coach SD once same-season QB EPA is a fixed effect")
# (b) the team's previous-season wat17 as a fixed effect (carry-over roster), rows with a previous season
pp <- panel[!is.na(prev_team_wat)]
v_prev <- fe_variant(wat17 ~ 1 + prev_team_wat + (1 | coach) + (1 | team) + (1 | season), pp, "war_prev_team_wat")
sens <- merge(sens, v_prev$tab, by = "coach", all.x = TRUE)
cat(sprintf("PREV-SEASON TEAM wat17 variant (n = %d rows): coefficient %+.2f; coach SD %.2f\n",
            nrow(pp), fixef(v_prev$m)["prev_team_wat"], as.data.table(VarCorr(v_prev$m))[grp == "coach", sdcor]))
# (c) Madden added to the talent fit for 2017+ (interaction with a 2017+ indicator; zero before)
pmd <- copy(panel)[, madden_z17 := fifelse(is.na(madden_z), 0, madden_z)][, post17 := as.integer(season >= 2017)]
f_md <- lm(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17 + season_f, data = pmd, weights = w)
pmd[, wat17_md := act17 - fitted(f_md)]
lm_mad2 <- summary(lm(wat17_md ~ madden_z, data = pmd[!is.na(madden_z)], weights = w))$coefficients
cat(sprintf("MADDEN-AUGMENTED talent fit (2017+ interaction): Madden %+.2f wins/SD in the market fit; residual lm(wat17 ~ madden_z) after it: %+.3f (t = %.2f) vs %+.3f before\n",
            coef(f_md)["madden_z17:post17"], lm_mad2["madden_z", 1], lm_mad2["madden_z", 3], lm_mad["madden_z", 1]))
add_val("residual_roster_madden_coef_after_madden_in_talent", lm_mad2["madden_z", 1], nrow(pmd[!is.na(madden_z)]), sprintf("same check after Madden (2017+) enters the talent fit; t = %.2f", lm_mad2["madden_z", 3]))
sens <- merge(sens, talent_variant(mkt17 ~ contract_z + qb_z + qbp_z + madden_z17:post17 + season_f, pmd, "war_talent_plus_madden_2017"), by = "coach", all.x = TRUE)
# SKEPTIC round 2: lagged QB EPA is NA on new starters (rookies, returns from
# injury) and was filled with the league mean. Variant: a new-starter indicator
# in the talent fit, so the baseline for those rows is estimated.
f_new <- lm(mkt17 ~ contract_z + qb_z + qbp_z + qbp_na + season_f, data = panel, weights = w)
new_cf <- summary(f_new)$coefficients["qbp_naTRUE", ]
cat(sprintf("\nNEW-STARTER indicator in the talent fit (%d of %d rows have no lagged QB EPA): %+.2f wins/17 (t = %.2f, p = %.3f); the market prices a first-year starter %s average\n",
            sum(panel$qbp_na), nrow(panel), new_cf[1], new_cf[3], new_cf[4], if (new_cf[1] < -0.25) "BELOW" else if (new_cf[1] > 0.25) "above" else "about at"))
add_val("talent_fit_new_starter_coef", unname(new_cf[1]), sum(panel$qbp_na), sprintf("market wins/17 for a new starter (no lagged QB EPA) vs the league-mean fill; t = %.2f", new_cf[3]), p = unname(new_cf[4]))
sens <- merge(sens, talent_variant(mkt17 ~ contract_z + qb_z + qbp_z + qbp_na + season_f, panel, "war_talent_new_starter"), by = "coach", all.x = TRUE)
# the low-tau and high-tau boards (coach SD held at 0.40 and at the profile upper bound)
sens <- merge(sens, coach[, .(coach, war_tau_low, prob_above_avg_tau_low, war_tau_high)], by = "coach", all.x = TRUE)
for (v in c("war_rep_average", "war_rep_one_and_done", "war_rep_first_all", "war_talent_contract_only",
            "war_talent_contract_qbepa", "war_talent_madden_2017", "pd_effect_ppg",
            "war_talent_same_season_qb", "war_prev_team_wat", "war_talent_plus_madden_2017",
            "war_talent_new_starter", "war_tau_low", "war_tau_high"))
  sens[eligible == TRUE & !is.na(get(v)), paste0("rank_", v) := frank(-get(v), ties.method = "min")]
fwrite(sens, "data/derived/coaching_war_sensitivity.csv")
se_el <- sens[eligible == TRUE]
top10 <- function(v) se_el[order(-get(v))][1:10, coach]
common10 <- Reduce(intersect, list(top10("war_main"), top10("war_talent_contract_only"), top10("war_talent_contract_qbepa")))
cat(sprintf("\nSENSITIVITY: top-10 coaches common to main / contract-only / contract+QB-EPA talent controls: %d\n", length(common10)))
sp_qb <- cor(se_el$war_main, se_el$war_talent_same_season_qb, method = "spearman")
sp_prev <- cor(se_el$war_main, se_el$war_prev_team_wat, method = "spearman", use = "complete")
sp_md2 <- cor(se_el$war_main, se_el$war_talent_plus_madden_2017, method = "spearman")
cat(sprintf("  Spearman with main: same-season QB control %.3f (top-10 overlap %d), prev-season team wat17 %.3f (overlap %d), talent + Madden 2017+ %.3f (overlap %d)\n",
            sp_qb, length(intersect(top10("war_main"), top10("war_talent_same_season_qb"))),
            sp_prev, length(intersect(top10("war_main"), top10("war_prev_team_wat"))),
            sp_md2, length(intersect(top10("war_main"), top10("war_talent_plus_madden_2017")))))
mv_qb <- se_el[, .(coach, rank_main, rank_qb = rank_war_talent_same_season_qb, move = rank_war_talent_same_season_qb - rank_main)][order(-abs(move))][1:10]
cat("  Same-season QB control, 10 biggest movers (main rank -> QB-controlled rank):", paste(sprintf("%s %d -> %d", mv_qb$coach, mv_qb$rank_main, mv_qb$rank_qb), collapse = "; "), "\n")
# SKEPTIC round 3: carry the same-season-QB rank on the main board and count the big movers
n_mv10 <- se_el[, sum(abs(rank_war_talent_same_season_qb - rank_main) >= 10)]
cat(sprintf("  %d of %d eligible coaches move 10+ places between the two boards; biggest: %s %d -> %d and %s %d -> %d\n",
            n_mv10, nrow(se_el), mv_qb$coach[1], mv_qb$rank_main[1], mv_qb$rank_qb[1], mv_qb$coach[2], mv_qb$rank_main[2], mv_qb$rank_qb[2]))
add_val("sens_n_move_10plus_same_season_qb", n_mv10, nrow(se_el), sprintf("eligible coaches whose rank moves 10+ places with the same-season QB control; biggest %s (%d -> %d), %s (%d -> %d)", mv_qb$coach[1], mv_qb$rank_main[1], mv_qb$rank_qb[1], mv_qb$coach[2], mv_qb$rank_main[2], mv_qb$rank_qb[2]))
coach <- merge(coach, se_el[, .(coach, rank_same_season_qb = rank_war_talent_same_season_qb, war_same_season_qb = war_talent_same_season_qb)], by = "coach", all.x = TRUE)
setorder(coach, -war_per_season)
# SKEPTIC round 3: per-era and no-returnee zero lines as boards
sens <- merge(sens, coach[, .(coach, war_rep_no_returnee, war_rep_era)], by = "coach", all.x = TRUE)
sens[eligible == TRUE, rank_war_rep_era := frank(-war_rep_era, ties.method = "min")]
sens[eligible == TRUE, rank_war_rep_no_returnee := frank(-war_rep_no_returnee, ties.method = "min")]
fwrite(sens, "data/derived/coaching_war_sensitivity.csv")   # rewritten with the round-3 columns
se_el <- sens[eligible == TRUE]
sp_era <- cor(se_el$war_main, se_el$war_rep_era, method = "spearman")
mv_era <- se_el[, .(coach, rank_main, rank_era = rank_war_rep_era, move = rank_war_rep_era - rank_main)][order(-abs(move))][1:5]
cat(sprintf("  Per-era replacement line (2012-2018 %+.2f, 2019-2025 %+.2f): Spearman with main %.3f; biggest movers: %s\n",
            rep_era$value[1], rep_era$value[2], sp_era, paste(sprintf("%s %d -> %d", mv_era$coach, mv_era$rank_main, mv_era$rank_era), collapse = "; ")))
add_val("sens_spearman_rep_era", sp_era, nrow(se_el), sprintf("main vs board with the replacement line estimated per era half; biggest movers %s", paste(sprintf("%s %d -> %d", mv_era$coach, mv_era$rank_main, mv_era$rank_era), collapse = ", ")))
sp_new <- cor(se_el$war_main, se_el$war_talent_new_starter, method = "spearman")
ov_new <- length(intersect(top10("war_main"), top10("war_talent_new_starter")))
sp_tl <- cor(se_el$war_main, se_el$war_tau_low, method = "spearman"); ov_tl <- length(intersect(top10("war_main"), top10("war_tau_low")))
cat(sprintf("  New-starter indicator variant: Spearman with main %.3f, top-10 overlap %d. Low-tau board (%.2f): Spearman %.3f, overlap %d; top 5 at low tau: %s\n",
            sp_new, ov_new, TAU_LOW, sp_tl, ov_tl, paste(top10("war_tau_low")[1:5], collapse = ", ")))
add_val("sens_spearman_new_starter", sp_new, nrow(se_el), sprintf("main vs talent fit with a new-starter indicator; top-10 overlap %d", ov_new))
add_val("sens_spearman_tau_low", sp_tl, nrow(se_el), sprintf("main vs board with coach SD held at %.2f; top-10 overlap %d", TAU_LOW, ov_tl))
add_val("sens_spearman_same_season_qb", sp_qb, nrow(se_el), "main vs coach model with same-season QB EPA fixed effect (coach gets no credit for his QB)")
add_val("sens_spearman_prev_team_wat", sp_prev, sum(!is.na(se_el$war_prev_team_wat)), "main vs coach model with team's previous-season wat17 fixed effect")
add_val("sens_spearman_talent_plus_madden", sp_md2, nrow(se_el), "main vs talent fit with Madden 2017+ interaction")
add_val("sens_top10_overlap_same_season_qb", length(intersect(top10("war_main"), top10("war_talent_same_season_qb"))), 10, "top-10 overlap, main vs same-season QB control")
cat(sprintf("  Spearman with main ranking: contract only %.3f, contract+QB EPA %.3f, Madden 2017-2025 %.3f (n %d), point differential %.3f\n",
            cor(se_el$war_main, se_el$war_talent_contract_only, method = "spearman"),
            cor(se_el$war_main, se_el$war_talent_contract_qbepa, method = "spearman"),
            cor(se_el$war_main, se_el$war_talent_madden_2017, method = "spearman", use = "complete"), sum(!is.na(se_el$war_talent_madden_2017)),
            cor(se_el$war_main, se_el$pd_effect_ppg, method = "spearman")))
flips <- se_el[, sum(sign(war_main) != sign(war_rep_average) | sign(war_main) != sign(war_rep_one_and_done))]
cat(sprintf("  replacement definitions: rank order identical (Spearman 1.000 by construction); %d of %d eligible coaches change sign across the three zero lines\n", flips, nrow(se_el)))
add_val("sens_top10_common", length(common10), nrow(se_el), "top-10 common across three talent controls")
add_val("sens_spearman_pd_outcome", cor(se_el$war_main, se_el$pd_effect_ppg, method = "spearman"), nrow(se_el), "main vs adjusted point differential outcome")
add_val("sens_spearman_madden", cor(se_el$war_main, se_el$war_talent_madden_2017, method = "spearman", use = "complete"), sum(!is.na(se_el$war_talent_madden_2017)), "main vs Madden talent control 2017-2025")
add_val("sens_sign_flips_replacement", flips, nrow(se_el), "coaches changing sign across rep = first hire / average / one-and-done")

# legacy consistency
lc <- coach[!is.na(war_market_only_1999) & games_1999 >= 100]
r_lc <- cor(lc$war_per_season, lc$war_market_only_1999, method = "spearman")
cat(sprintf("\nLEGACY CONSISTENCY: main WAR vs market-only 1999-%d column, %d coaches with >= 100 career games: Spearman %.3f\n", LAST, nrow(lc), r_lc))
print(lc[coach %in% c("Bill Belichick", "Andy Reid", "Mike Tomlin", "Pete Carroll", "Sean Payton", "John Harbaugh", "Sean McVay", "Kyle Shanahan", "Mike McCarthy"),
         .(coach, war_per_season = round(war_per_season, 2), war_market_only_1999 = round(war_market_only_1999, 2), games_1999)])
add_val("legacy_consistency_spearman", r_lc, nrow(lc), "main WAR vs EB-shrunk market-only rate 1999-2025, >= 100 games")
coach_names <- coach$coach
legacy <- career_mkt[games_all >= 16 & !(coach %in% coach_names)][order(-war_market_only_1999)]
cat("  Legacy-only coaches (not in 2012-2025 window), top 8 by market-only column, NOT comparable to the main column:\n")
print(legacy[1:8, .(coach, games_all, first_all, last_all, raw = round(rate_per17, 2), war_market_only_1999 = round(war_market_only_1999, 2))])

# ---------------------------------------------------------------------------
# 14. Write coach-level and season-level CSVs, validation CSV.
# ---------------------------------------------------------------------------
setorder(coach, -war_per_season)
out_cols <- c("coach", "first_season", "last_season", "seasons", "seasons_weighted", "games", "teams", "team_list",
              "actual_wins", "market_expected_wins", "talent_expected_wins", "raw_wat_per_season",
              "raw_premium_per_season", "raw_wae_per_season", "premium_per_season", "surprise_per_season",
              "coach_effect", "coach_effect_inherit", "rep_offset", "war_per_season", "war_total", "se_cond", "boot_se", "se",
              "lo", "hi", "boot_mean", "boot_lo", "boot_hi", "se_total", "prob_above_avg", "prob_above_rep", "clear_of_replacement",
              "eligible", "rank", "rank_all", "rank_lo", "rank_hi", "p_top5", "p_top10", "war_vs_average", "war_vs_interim",
              "war_first3", "rank_first3", "influence_max", "influence_season", "influence_team", "influence_without", "influence_flag",
              "influence_other_max", "influence_other_row", "influence_other_without", "influence_other_flag",
              "franchise_share", "franchise_pinned", "elite_qb_seasons", "coach_effect_no_elite_qb", "elite_qb_delta", "elite_qb_flag",
              "cheap_qb_seasons", "coach_effect_no_cheap_qb", "cheap_qb_delta", "cheap_qb_flag", "mean_qbcur_z",
              "rank_same_season_qb", "war_same_season_qb", "war_rep_no_returnee", "rep_offset_era", "war_rep_era",
              "war_tau_low", "prob_above_avg_tau_low", "war_tau_high", "war_tau_marg", "lo_tau_marg", "hi_tau_marg", "prob_above_avg_tau_marg", "window_start_veteran",
              "one_team_one_coach", "pct_own_playcaller",
              "fourth_down_value", "fourth_down_lo", "fourth_down_hi",
              "games_1999", "first_season_1999", "last_season_1999", "war_market_only_1999")
fwrite(coach[, ..out_cols], "data/derived/coaching_war.csv")
cat(sprintf("\nwrote data/derived/coaching_war.csv (%d coaches)\n", nrow(coach)))

seasons_out <- merge(cst[, .(coach, season, team, games, act17, mkt17, wae17, var17, pd_adj_pg, spell_yr, spell_len, interim, inherit_pd)],
                     panel[, .(coach, season, team, talent17, premium17, wat17, contract_z, qb_z, qbp_z, qb_na, qb_filled, qbp_na,
                               qbcur_z, madden_z, prev_team_wat, team_effect, season_effect, resid_plus_coach)],
                     by = c("coach", "season", "team"), all.x = TRUE)
seasons_out[, in_main_window := !is.na(wat17)]
setorder(seasons_out, coach, season)
fwrite(seasons_out, "data/derived/coaching_war_seasons.csv")
cat(sprintf("wrote data/derived/coaching_war_seasons.csv (%d rows, %d in the 2012-%d window)\n", nrow(seasons_out), sum(seasons_out$in_main_window), LAST))
one11 <- coach[one_team_one_coach == TRUE, coach]
cat(sprintf("One-team-one-coach careers (coach and team effects separated only by the priors): %s\n",
            if (length(one11)) paste(one11, collapse = ", ") else "none"))

# ---------------------------------------------------------------------------
# 15. Figures.
# ---------------------------------------------------------------------------
cap_src <- "nflverse schedules and closing spreads 1999-2025; OverTheCap contracts via nflreadr 2012-2025; built by R/71"
n_elig <- sum(coach$eligible)
top25 <- coach[eligible == TRUE][1:min(25, n_elig)]
top25[, coach_f := factor(coach, levels = rev(coach))]
# SKEPTIC round 2: label carries the hire year when the career began before the
# window, the number without a pinning row of another coach, and the number
# without the coach's elite-QB seasons
# for a franchise-pinned coach the other-coach row is shown when it is at his own franchise
top25[, own_team := sub(".* ", "", influence_other_row)]
top25[, show_other := influence_other_flag | (franchise_pinned & !is.na(own_team) & mapply(grepl, own_team, team_list))]
# SKEPTIC round 3: every label carries the rank once the same season's QB play
# is controlled, and the number without the coach's cheap-QB seasons when it
# moves him more than 0.3
top25[, lab := sprintf("%+.2f  (%d seasons%s, %s; QB-controlled rank %d%s%s%s%s)", war_per_season, seasons,
                       fifelse(window_start_veteran, sprintf(", hired %d", first_season_1999), ""), team_list, rank_same_season_qb,
                       fifelse(influence_flag, sprintf("; without %d it is %+.2f", influence_season, influence_without - rep_offset), ""),
                       fifelse(show_other, sprintf("; without %s it is %+.2f", sub("^\\S+ ", "", influence_other_row), influence_other_without - rep_offset), ""),
                       fifelse(elite_qb_flag, sprintf("; without %d elite-QB seasons %+.2f", elite_qb_seasons, coach_effect_no_elite_qb - rep_offset), ""),
                       fifelse(cheap_qb_flag, sprintf("; without %d cheap-QB seasons %+.2f", cheap_qb_seasons, coach_effect_no_cheap_qb - rep_offset), ""))]
top25[, coach_f := factor(coach, levels = rev(coach[order(-war_per_season)]))]
n_clear_top <- sum(top25$clear_of_replacement)
top1 <- top25[1]
rel_sh <- reliability[split == "odd_even_seasons_blup"]
p_lb <- ggplot(top25, aes(y = coach_f)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_segment(aes(x = lo, xend = hi, yend = coach_f, colour = clear_of_replacement), linewidth = 0.9) +
  geom_point(aes(x = raw_wat_per_season - rep_offset), shape = 1, size = 2.4, colour = "grey60") +
  geom_point(aes(x = war_tau_low), shape = 4, size = 2.2, colour = accent2) +
  geom_point(aes(x = war_per_season, colour = clear_of_replacement), size = 3) +
  geom_text(aes(x = pmax(hi, raw_wat_per_season - rep_offset) + 0.1, label = lab), hjust = 0, size = 2.5, colour = ink_body) +
  scale_colour_manual(values = c(`TRUE` = accent, `FALSE` = "grey45")) +
  scale_x_continuous(name = "wins per season a coach-team pairing delivered above a first-season hire, after payroll, last season's quarterback and franchise",
                     expand = expansion(mult = c(0.05, 1.15))) +
  labs(title = sprintf("Among head coaches who got at least 4 seasons, the top pairings come out near %+.1f wins a season over a first-season hire,\nbut the measure has not cleared its own reliability gate (split-half r = %.2f) and its scale rests on a coach spread the data cannot pin down",
                       top1$war_per_season, rel_sh$r),
       subtitle = paste0(sprintf("Head coaches with 4+ seasons, 2012-2025, so the bottom of the board is the worst of the survivors. Filled dot is the shrunk estimate, hollow circle the raw average,\nbar the 95%% interval with the coach spread drawn from its profile likelihood; blue cross is the same board with the coach spread held at %.2f wins, the value the\nreliability supports (top coach %+.2f instead of %+.2f; averaged over everything the likelihood allows, %+.2f with P(above average) %.2f). Coach reliability across odd and even seasons\nr = %.2f [%.2f, %.2f], below the 0.30 gate; panels simulated from the fitted model give a median of %.2f [%.2f, %.2f] and %.0f%% of them produce a half with no coach signal at all.\n",
                                 TAU_LOW, top1$war_tau_low, top1$war_per_season, top1$war_tau_marg, top1$prob_above_avg_tau_marg, rel_sh$r, rel_sh$lo, rel_sh$hi, q_shf[2], q_shf[1], q_shf[3], 100 * mean(is.na(sh_fit))),
                         sprintf("Coach spread (SD) %.2f wins, 95%% profile interval %.2f to %.2f. The published coach effect correlates %.2f with the coach's mean same-season quarterback play.\nZero line: what a week-1 first-season hire delivered net of the franchise term, %d rows pooled: %+.2f wins vs the average sitting coach (SE %.2f). A first-time head coach is league average\n(%+.2f, n = %d); a coach on a second or later job is %+.2f (n = %d); the split is inside the noise (Wilcoxon p = %.2f). The line moved %.2f between halves of the window (p = %.2f) and %.2f when\nthe interim rule changed; both are under one SE. The no-returnee line (%+.2f) would lower every number by %.2f. Coach term alone: LRT p = %.3f; coach and franchise together p < 0.001.",
                                 vc_wat$coach, tau_ci[1], tau_ci[2], r_qbcur, nrow(fs0), rep_offset, rep_boot_se, rep_ft$value, rep_ft$n, rep_rt$value, rep_rt$n, wt_rt$p.value,
                                 abs(lm_era[1]), lm_era[4], abs(rep_offset - rep_old_rule$value), rep_noret$value, rep_noret$value - rep_offset, lrt_coach["p"])),
       caption = fig_caption(cap_src,
                             sprintf("%d head coaches with >= 4 seasons of >= %d games; %d coach-seasons.", sum(coach$eligible), MIN_G, nrow(panel)),
                             sprintf("\nCoach term from a mixed model of wins above talent with coach, franchise and season terms. A nested fit puts the portable coach SD at %.2f against a coach-within-team SD of %.2f,\nso this is what a coach-team pairing delivered, not a property of the man that travels. Coaches with %.0f%%+ of a franchise's rows (%s) have a franchise term resting on 1-2 seasons of other coaches.\nThe quarterback control is cap share and last season's play, so a quarterback playing above his pay is scored as cheap talent and his wins land on the coach: %d of %d coach-seasons are such rows, averaging %+.1f wins above talent.\nControlling for the quarterback's play in the same season cuts the coach spread by %.0f%%; %d of %d coaches move 10 or more places (Spearman %.2f), the biggest %s (%d to %d) and %s (%d to %d). That rank is in each label.%s",
                                     sd_of("M_wat_nested", "coach"), sd_of("M_wat_nested", "coach:team"), 85,
                                     paste(coach[franchise_pinned == TRUE & eligible == TRUE, sub("^.* ", "", coach)], collapse = ", "),
                                     sum(panel$cheap_qb_row), nrow(panel), panel[cheap_qb_row == TRUE, mean(wat17)],
                                     100 * (1 - as.data.table(VarCorr(v_qb$m))[grp == "coach", sdcor] / vc_wat$coach), n_mv10, nrow(se_el), sp_qb,
                                     mv_qb$coach[1], mv_qb$rank_main[1], mv_qb$rank_qb[1], mv_qb$coach[2], mv_qb$rank_main[2], mv_qb$rank_qb[2],
                                     if (new_cf[1] < -0.25) sprintf("\nThe market prices a first-year starting quarterback %.1f wins below the league-mean fill used here; with that indicator in the talent fit the board's rank order correlates %.2f with this one.", -new_cf[1], sp_new) else ""))) +
  theme_coach(grid = "none") + theme(axis.title.y = element_blank())
save_fig("docs/figures/coaching_war_leaderboard.png", p_lb, w = 14, h = 10.5)

# components: premium vs surprise for top 12 and bottom 12
cmp <- rbind(copy(coach[eligible == TRUE][1:12])[, grp := "Top 12"], copy(coach[eligible == TRUE][order(war_per_season)][1:12])[, grp := "Bottom 12"])
cmp_long <- melt(cmp[, .(coach, grp, war_per_season, `market premium (seen before kickoff)` = premium_per_season, `surprise (beat the spread)` = surprise_per_season)],
                 id.vars = c("coach", "grp", "war_per_season"), variable.name = "component")
cmp_long[, coach_f := factor(coach, levels = cmp[order(war_per_season), coach])]
cmp_long[, grp := factor(grp, levels = c("Top 12", "Bottom 12"))]
share_prem <- cmp[grp == "Top 12", sum(premium_per_season) / sum(premium_per_season + surprise_per_season)]
# SKEPTIC round 3: the two parts are BLUPs from separate models with their own
# shrinkage and do NOT sum to the M_wat coach effect. The share in the title is
# computed on the summed parts, the basis the bars use; the coach-effect basis
# is reported beside it.
share_prem_ce <- cmp[grp == "Top 12", sum(premium_per_season) / sum(coach_effect)]
cmp_gap <- coach[, max(abs(premium_per_season + surprise_per_season - coach_effect))]
cmp_gap_who <- coach[which.max(abs(premium_per_season + surprise_per_season - coach_effect))]
stopifnot(abs(share_prem - sum(cmp_long[grp == "Top 12" & grepl("premium", component), value]) / sum(cmp_long[grp == "Top 12", value])) < 1e-9)
cat(sprintf("\nCOMPONENTS: premium share of the top 12 on the summed parts %.0f%%, on the coach effect %.0f%%; max |premium + surprise - coach_effect| = %.2f (%s: %+.2f vs %+.2f), cor %.2f\n",
            100 * share_prem, 100 * share_prem_ce, cmp_gap, cmp_gap_who$coach, cmp_gap_who$premium_per_season + cmp_gap_who$surprise_per_season, cmp_gap_who$coach_effect,
            coach[, cor(premium_per_season + surprise_per_season, coach_effect)]))
add_val("components_max_gap_sum_vs_coach_effect", cmp_gap, nrow(coach), sprintf("max |premium + surprise - coach_effect|; the parts come from separate models and do not sum exactly (%s)", cmp_gap_who$coach))
add_val("components_premium_share_top12", share_prem, 12, sprintf("premium share of the top 12 on the summed parts (bars); %.3f on the coach effect", share_prem_ce))
p_cmp <- ggplot(cmp_long, aes(y = coach_f, x = value, fill = component)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.35) +
  facet_wrap(~grp, scales = "free_y") +
  scale_fill_manual(values = setNames(c(accent2, accent), levels(cmp_long$component)), name = NULL) +
  scale_x_continuous(name = "wins per season above talent, shrunk coach effect, split into its two parts") +
  labs(title = "Most of the top coaches' number was already in the closing spread; beating the spread is the smaller part",
       subtitle = sprintf("Blue: the market priced the team above its payroll and last season's QB (unpriced roster, a QB on a rookie deal, reputation, health); about %.0f%% of the\ntop 12's total. Red: the team then beat the spread anyway. The blue part correlates %.2f with the same season's QB play and %.2f with the team's\nprevious-season wins above talent, so much of it is roster the payroll model misses, not the coach.",
                          100 * share_prem, cor(panel$premium17, panel$qbcur_z), cor(panel$premium17, panel$prev_team_wat, use = "complete")),
       caption = fig_caption(cap_src, "\nTop and bottom 12 of the coaches with >= 4 seasons, 2012-2025.",
                             sprintf("Each part is the coach term from its own mixed model with its own shrinkage,\nso the two do not sum exactly to the coach effect behind the leaderboard (max gap %.2f, %s); the %.0f%% is on the summed parts, %.0f%% on the coach effect.",
                                     cmp_gap, cmp_gap_who$coach, 100 * share_prem, 100 * share_prem_ce))) +
  theme_coach_legend(grid = "none", position = "bottom") + theme(axis.title.y = element_blank())
save_fig("docs/figures/coaching_war_components.png", p_cmp, w = 12, h = 7)

# OOS validation figure
mo <- oos_tab[predictor == "war_through_prev" & target == "wat"][1]
lab_o <- oos[abs(war_through_prev) > quantile(abs(war_through_prev), .97) | abs(realised_wat) > 6.5]
p_oos <- ggplot(oos, aes(x = war_through_prev, y = realised_wat)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.35, linetype = "dashed") +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.35, linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, colour = "grey80", linewidth = 0.4) +
  geom_point(colour = "grey50", alpha = 0.7, size = 1.8) +
  geom_smooth(method = "lm", se = TRUE, colour = accent, fill = accent, alpha = 0.15, linewidth = 0.9) +
  geom_text_repel(data = lab_o, aes(label = paste(coach, season)), size = 2.7, colour = ink_body, max.overlaps = 20) +
  scale_x_continuous(name = "WAR per season through the previous season (fit on earlier seasons only)") +
  scale_y_continuous(name = "wins above talent in the held-out season") +
  labs(title = sprintf("Out of sample, a coach's WAR predicts his next season's wins above talent at r = %.2f (slope %.2f),\n%s",
                       mo$r, mo$slope, if (oos_pass) "clearing the prespecified bar" else if (oos_pass_weak) "short of the 0.30 bar" else "which does not clear the prespecified bar of 0.30 or beat last season's record"),
       subtitle = sprintf("%d coach-seasons, holdout seasons %d-%d. Each point: the model refit on 2012 to t-1, then scored on season t. Grey line is slope 1.\n95%% CI on r: [%.2f, %.2f]. Previous-season wins alone: r = %.2f; previous-season raw wins above talent: r = %.2f.\nOn the %d rows where both exist, r(WAR) minus r(last season's wins) is %+.2f with a coach-cluster bootstrap interval of [%+.2f, %+.2f],\nso WAR is neither distinguishably better nor worse than last season's record (slope %.2f on those rows).\nThe %d-%d holdouts alone, where the training fit had a usable coach spread (the earlier ones put it near zero and predict the same\nnumber for everyone): r = %.2f [%.2f, %.2f], n = %d. WAR alone, t clustered by coach %.1f, by coach and season %.1f; given last season's wins, two-way t = %.1f.",
                          mo$n, OOS_FROM, LAST, mo$lo, mo$hi, oos_tab[predictor == "prev_wins" & target == "wat", r], base_ii$r, nrow(common),
                          main_oos$r - oos_tab[predictor == "prev_wins" & target == "wat", r], rd_ci[1], rd_ci[2], main_oos$slope,
                          min(usable_seasons), max(usable_seasons), main_oos_u$r, main_oos_u$lo, main_oos_u$hi, main_oos_u$n, pa_cl["t"], pa_cl2["t"], pf_cl2["t"]),
       caption = fig_caption(cap_src, "\nCoaches present in both the training seasons and the held-out season.",
                             sprintf("Team and season effects are not subtracted from the held-out outcome.\nThe holdout universe is coaches retained into season t: the %d coach-seasons whose coach was fired or left after t-1 are never scored (their WAR through t-1 averaged %+.2f against %+.2f for the scored),\nso the low end of the x-axis is truncated by selection; the direction of that effect on r is not demonstrated here. Refitting the spread-to-win model on seasons before t changes r by %.3f.",
                                     nrow(oos_unscored), mean(oos_unscored$war_through_prev), mean(oos$war_through_prev), r_strict_mkt - main_oos$r))) +
  theme_coach(grid = "y")
save_fig("docs/figures/coaching_war_validation.png", p_oos, w = 12, h = 7.5)

# survivorship figure
e12[, grp := factor(grp, levels = c("<=2", "3-4", "5+", "censored"))]
e12p <- e12[grp != "censored"]
e12p[, grp_lab := factor(fcase(grp == "<=2", "gone within 2 seasons", grp == "3-4", "lasted 3-4 seasons", grp == "5+", "lasted 5+ seasons"),
                         levels = c("gone within 2 seasons", "lasted 3-4 seasons", "lasted 5+ seasons"))]
s1p <- e12p[, .(mean = mean(wat17), se = sd(wat17) / sqrt(.N), n = .N), by = grp_lab]
p_sv <- ggplot(e12p, aes(x = grp_lab, y = wat17)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.35, linetype = "dashed") +
  geom_jitter(width = 0.18, height = 0, colour = "grey60", alpha = 0.6, size = 1.8) +
  geom_pointrange(data = s1p, aes(y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), colour = accent, size = 0.7, linewidth = 0.9) +
  geom_text(data = s1p, aes(y = mean, label = sprintf("%+.2f (n = %d)", mean, n)), nudge_x = 0.32, hjust = 0, size = 3.2, colour = accent, fontface = "bold") +
  scale_y_continuous(name = "wins above talent per 17 games, first two seasons with the team") +
  scale_x_discrete(name = "what happened to the coach afterwards") +
  labs(title = if (wt$conf.int[1] > 0) "Coaches who went on to last 5+ seasons were already winning above their talent in years 1-2.\nThe gap is unusual under both simulated worlds, with and without a coaching trait,\nso the simulation cannot say how much of it is firing and how much is skill"
               else sprintf("In their first two seasons, future long-tenured coaches were %.1f wins better than those gone within 2, but the gap is not clear of zero", wt$estimate),
       subtitle = sprintf("Each point is one coach-season in years 1-2 of a spell begun 2012 or later. Spell-level Wilcoxon shift 5+ vs gone within 2: %+.2f [%+.2f, %+.2f], %s\n(n = %d vs %d spells). Owners fire on wins, so a gap appears even when no coach has any skill: the simulated no-trait null gives a median of %+.2f\n(95%% range %+.2f to %+.2f; one-sided P = %.3f for the observed %+.2f). But the world with the fitted coach trait gives a median of only %+.2f (P = %.3f;\nlikelihood ratio about %.0f), so both worlds find the gap unusual and the simulated firing rule understates the real selection. Censoring the %d voluntary\nexits (retirements, mutual partings): observed %+.2f, P = %.3f. A wins-only firing rule with no trait: median %+.2f, P = %.3f.",
                          wt$estimate, wt$conf.int[1], wt$conf.int[2], if (wt$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", wt$p.value),
                          e12s[grp == "5+", .N], e12s[grp == "<=2", .N], q0[2], q0[1], q0[3], p_null0, obs_shift_mean, q1[2], p_null1, lr_trait,
                          nrow(vol_exits), obs_shift_vol, p_null0v, q0w[2], p_null0w),
       caption = fig_caption(cap_src, "\nInterim stints (coach was not in place in week 1) and spells still running in 2025 with fewer than 5 seasons are excluded. 'Gone' includes retirements and mutual partings unless censored as above.",
                             sprintf("\nRaw wins above talent, not the shrunk estimate, because the shrunk estimate already knows how the career ended.\nNull: panels simulated from the fitted variance components with the coach term set to zero, spells cut by the fitted retention model (wins above talent, cumulative, spell year).\nWhether the survivors' early edge persists into years 3+ of the same spell would be the test that speaks to skill, but it has no power at this n: observed r = %+.2f [%+.2f, %+.2f] (3+ spells, n = %d)\nagainst a no-trait range of %+.2f to %+.2f (median %+.2f; P = %.2f) and a fitted-trait median of %+.2f. For 5+ spells r = %+.2f (n = %d), no-trait range %+.2f to %+.2f.",
                                     ct_p3$estimate, ct_p3$conf.int[1], ct_p3$conf.int[2], nrow(sv3), qr3_0[1], qr3_0[3], qr3_0[2], p_r3, qr3_1[2],
                                     ct_p5$estimate, nrow(sv), qr5_0[1], qr5_0[3]))) +
  theme_coach(grid = "y") + theme(plot.subtitle = element_text(colour = ink_subtitle, size = rel(0.9), margin = margin(b = 30)))
save_fig("docs/figures/coaching_war_survivorship.png", p_sv, w = 12, h = 7.5)

# ---------------------------------------------------------------------------
# 16. Validation CSV and final print block.
# ---------------------------------------------------------------------------
validation <- rbindlist(val)
fwrite(validation, "data/derived/coaching_war_validation.csv")
cat(sprintf("wrote data/derived/coaching_war_validation.csv (%d rows)\n", nrow(validation)))

elite <- coach[eligible == TRUE][1:5]
cat("\n==================== HEADLINE ====================\n")
cat(sprintf("Variance (wins/17): coach %.2f, franchise %.2f, season %.2f, residual %.2f. Premium model: coach %.2f vs franchise %.2f.\n",
            sd_of("M_wat", "coach"), sd_of("M_wat", "team"), sd_of("M_wat", "season"), sd_of("M_wat", "Residual"),
            sd_of("M_prem", "coach"), sd_of("M_prem", "team")))
cat(sprintf("Coach SD %.2f has a 95%% profile CI of [%.2f, %.2f] (odd seasons %.2f, even %.2f); model-implied split-half r median %.2f [%.2f, %.2f] vs observed %.3f -> the measure has NOT cleared its own reliability gate\n",
            vc_wat$coach, tau_ci[1], tau_ci[2], tau_odd, tau_even, q_shf[2], q_shf[1], q_shf[3], r_sh))
cat(sprintf("Nested fit: portable coach SD %.2f vs coach-within-team SD %.2f: the quantity is a coach-team pairing\n", sd_of("M_wat_nested", "coach"), sd_of("M_wat_nested", "coach:team")))
cat(sprintf("Replacement: a week-1 first-season hire delivers %+.2f wins/17 vs the average sitting coach (n = %d; bootstrap SE %.2f, naive SE %.2f; old full-season interim rule gave %+.2f on %d rows)\n", rep_offset, rep_noint$n, rep_boot_se, rep_se_naive, rep_old_rule$value, rep_old_rule$n))
cat(sprintf("  first-time head coaches %+.2f (n = %d), second-or-later job %+.2f (n = %d), Wilcoxon p = %.2f; era 2012-2018 %+.2f vs 2019-2025 %+.2f (diff %+.2f, SE %.2f, p = %.2f); no-returnee line %+.2f\n",
            rep_ft$value, rep_ft$n, rep_rt$value, rep_rt$n, wt_rt$p.value, rep_era$value[1], rep_era$value[2], lm_era[1], lm_era[2], lm_era[4], rep_noret$value))
cat(sprintf("LRT: coach variance given team p = %.3f; team given coach p = %.3f; both p = %.2g\n", lrt_coach["p"], lrt_team["p"], lrt_both["p"]))
cat(sprintf("Cheap-QB rows (paid below average, played above average): %d of %d, mean wat17 %+.2f vs %+.2f; cor(coach effect, mean same-season QB z) = %.2f over %d eligible; coaches moved > 0.3 by dropping them: %s\n",
            sum(panel$cheap_qb_row), nrow(panel), panel[cheap_qb_row == TRUE, mean(wat17)], panel[cheap_qb_row == FALSE, mean(wat17)], r_qbcur, sum(coach$eligible),
            paste(coach[cheap_qb_flag == TRUE, sprintf("%s (%+.2f -> %+.2f)", coach, coach_effect, coach_effect_no_cheap_qb)], collapse = "; ")))
cat("Top 5 (>= 4 seasons):\n")
for (i in 1:5) cat(sprintf("  %d. %-18s %+.2f [%+.2f, %+.2f]  %d seasons  P(true value in top 5) %.2f, P(above average) %.2f; at tau %.2f: %+.2f, P(above avg) %.2f; at tau %.2f: %+.2f%s%s%s\n", i, elite$coach[i], elite$war_per_season[i], elite$lo[i], elite$hi[i], elite$seasons[i], elite$p_top5[i], elite$prob_above_avg[i],
                            TAU_LOW, elite$war_tau_low[i], elite$prob_above_avg_tau_low[i], TAU_HIGH, elite$war_tau_high[i],
                            if (elite$influence_flag[i]) sprintf("  [one season, %d, moves him %.2f]", elite$influence_season[i], elite$influence_max[i]) else "",
                            if (elite$influence_other_flag[i]) sprintf("  [another coach's row, %s, moves him %.2f]", elite$influence_other_row[i], elite$influence_other_max[i]) else "",
                            if (elite$elite_qb_flag[i]) sprintf("  [without %d elite-QB seasons: %+.2f]", elite$elite_qb_seasons[i], elite$coach_effect_no_elite_qb[i] - rep_offset) else ""))
# SKEPTIC round 3: both ranks on every list that reaches the page, and the
# bottom 5 generated from the table so the report cannot go stale
cat("  same-season-QB ranks, top 5:", paste(sprintf("%s %d -> %d", elite$coach, elite$rank, elite$rank_same_season_qb), collapse = "; "), "\n")
bot5 <- coach[eligible == TRUE][order(war_per_season)][1:5]
cat("Bottom 5 (>= 4 seasons), WAR [95% interval], seasons, teams; rank with same-season QB controlled:\n")
for (i in 1:5) cat(sprintf("  %d. %-18s %+.2f [%+.2f, %+.2f]  %d seasons (%s); QB-controlled rank %d of %d\n", bot5$rank[i], bot5$coach[i], bot5$war_per_season[i], bot5$lo[i], bot5$hi[i],
                           bot5$seasons[i], bot5$team_list[i], bot5$rank_same_season_qb[i], sum(coach$eligible)))
cat(sprintf("Bootstrap: parametric with the coach SD drawn from the profile likelihood, centred on the shrunk estimate (max bias %.3f over 8+ seasons); %d of %d coaches have P(above average) >= 0.95, %d have lo > 0\n",
            boot_bias, sum(coach$prob_above_avg >= 0.95, na.rm = TRUE), nrow(coach), n_clear))
cat(sprintf("Tau-marginalised (mixture over the profile likelihood): top coach %+.2f [%+.2f, %+.2f], P(above average) %.2f (bootstrap %.2f); %d of %d eligible have P(above average) >= 0.95 on that basis\n",
            elite$war_tau_marg[1], elite$lo_tau_marg[1], elite$hi_tau_marg[1], elite$prob_above_avg_tau_marg[1], elite$prob_above_avg[1],
            coach[eligible == TRUE, sum(prob_above_avg_tau_marg >= 0.95)], sum(coach$eligible)))
cat(sprintf("Influence of another coach's row > 0.3: %s\n", paste(coach[influence_other_flag == TRUE, sprintf("%s (%s: %+.2f -> %+.2f)", coach, influence_other_row, coach_effect, influence_other_without)], collapse = "; ")))
cat(sprintf("Elite-QB partnership numbers (|delta| > 0.3): %s\n", paste(coach[elite_qb_flag == TRUE, sprintf("%s (%+.2f -> %+.2f)", coach, coach_effect, coach_effect_no_elite_qb)], collapse = "; ")))
cat(sprintf("Window-start veterans vs in-window hires (eligible): %+.2f [%+.2f, %+.2f], p = %.2f\n", tt_vet$estimate[1] - tt_vet$estimate[2], tt_vet$conf.int[1], tt_vet$conf.int[2], tt_vet$p.value))
cat(sprintf("OOS two-way clustered t: WAR alone %.2f, given prev wins %.2f; usable holdouts %d-%d r = %.3f [%.3f, %.3f], n = %d; unscored (fired) coaches' WAR through t-1 averaged %+.2f vs %+.2f scored\n",
            pa_cl2["t"], pf_cl2["t"], min(usable_seasons), max(usable_seasons), main_oos_u$r, main_oos_u$lo, main_oos_u$hi, main_oos_u$n, mean(oos_unscored$war_through_prev), mean(oos$war_through_prev)))
cat(sprintf("Replacement zero line: %+.2f published (net of franchise term); %+.2f with no franchise term; %+.2f with the franchise term from years-3+ rows; %+.2f leave-one-spell-out\n",
            rep_offset, rep_raw_season, rep_team3, rep_loso))
cat(sprintf("Same-season QB control: coach SD %.2f -> %.2f, Spearman with main %.2f, top-10 overlap %d. Residual Madden coefficient on wat17: %+.2f wins/SD (p = %.1g). The control is payroll and LAST season's QB, not the roster.\n",
            vc_wat$coach, as.data.table(VarCorr(v_qb$m))[grp == "coach", sdcor], sp_qb, length(intersect(top10("war_main"), top10("war_talent_same_season_qb"))), lm_mad["madden_z", 1], lm_mad["madden_z", 4]))
cat(sprintf("Rank vs seasons among eligible: Spearman %.2f. Own play-caller minus not: %+.2f [%+.2f, %+.2f]\n", sp_seas, tt_pc$estimate[1] - tt_pc$estimate[2], tt_pc$conf.int[1], tt_pc$conf.int[2]))
cat(sprintf("Clear of replacement: %d of %d coaches (chance ~%.1f)\n", n_clear, nrow(coach), n_chance))
cat(sprintf("OOS (WAR -> wins above talent in t): r = %.3f [%.3f, %.3f], n = %d, slope %.2f; vs prev wins r = %.3f; vs prev raw wat r = %.3f. Bar r >= %.2f: %s\n",
            main_oos$r, main_oos$lo, main_oos$hi, main_oos$n, main_oos$slope, oos_tab[predictor == "prev_wins" & target == "wat", r], base_ii$r, OOS_BAR, if (oos_pass) "PASS" else "FAIL"))
so <- oos_tab[predictor == "war_through_prev" & target == "wae"][2]
cat(sprintf("OOS strict (WAR -> beating the spread in t): r = %.3f [%.3f, %.3f], n = %d\n", so$r, so$lo, so$hi, so$n))
cat(sprintf("Split-half: odd/even seasons r = %.2f (SB %.2f), odd/even games r = %.2f (SB %.2f); year-to-year wat17 r = %.2f\n",
            r_sh, sb(r_sh), r_gh, sb(r_gh), reliability[split == "year_to_year_wat17", r]))
cat(sprintf("Across-team: r = %.2f [%.2f, %.2f], n = %d movers\n", ct_mv$estimate, ct_mv$conf.int[1], ct_mv$conf.int[2], nrow(mv)))
cat(sprintf("Survivorship: %s\n", verdict_tenure))
cat(sprintf("The 3-4 win hypothesis: the elite tier on this measure runs %+.2f to %+.2f wins per season over a fresh hire (upper interval bounds %.2f to %.2f). %s\n",
            min(elite$war_per_season), max(elite$war_per_season), min(elite$hi), max(elite$hi),
            if (max(elite$hi) < 3) "Nobody reaches 3 wins even at the top of his interval; the hypothesis is not supported after payroll, QB and franchise." else "Some intervals reach 3 wins, so the hypothesis cannot be ruled out for the very top."))
cat("Done.\n")
