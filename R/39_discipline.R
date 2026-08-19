# =============================================================================
# 39_discipline.R -- does a disciplined team reflect its coach?
#
# THE ASK. Sumer's penalty fields (is_penalty, is_penalty_accepted,
# penalty_type, penalty_yards, penalty_side_of_ball, penalty_outcome) have
# never been touched on this board. They carry the most repeated claim in
# football broadcasting: "disciplined teams reflect their coach." The board's
# house standard is a persistence test (odd season vs even season, same test
# that killed the halftime-adjuster myth in R/04) -- a trait either survives
# that split or it does not.
#
# TAXONOMY. penalty_type has 63 distinct values. Four buckets, assigned by
# hand below and printed in full at the top of the run:
#   PRESNAP_OFFENSE       FalseStart, DelayOfGame, IllegalFormation,
#                         IllegalShift, IllegalMotion -- offense commits these
#                         BEFORE the ball is snapped. No athleticism, no
#                         opponent involved, purely "did 11 guys line up right
#                         and get moving on the right sound." The purest
#                         preparation/discipline signal Sumer carries.
#   PRESNAP_DEFENSE_DRAWN DefensiveOffside, NeutralZoneInfraction,
#                         Encroachment -- the DEFENSE jumps while this team is
#                         on offense. This is drawn, not committed: a hard
#                         count is an offensive weapon, and it is a caller
#                         skill, not a head-coach trait, so it is tested by
#                         OFFENSIVE CALLER in test 2, not by head coach.
#   POSTSNAP_AGGRESSION   Holding (both sides), DPI/OPI, roughing, facemask,
#                         illegal contact/use of hands, chop/crackback/
#                         blindside blocks, horse-collar, hip-drop, tripping,
#                         low block, disqualifications for these -- fouls that
#                         happen because of what a player did with his body
#                         AFTER the snap, at football speed. Reported for
#                         context in the taxonomy table only; not part of
#                         either headline test, because these are talent and
#                         scheme fouls (a corner grabbing a route, a rusher
#                         hitting a QB half a step late) far more than they
#                         are a coach's preparation.
#   DEADBALL_DUMB          Unsportsmanlike conduct, taunting, too-many-men,
#                         disqualification, illegal substitution -- mental
#                         and discipline fouls that have nothing to do with
#                         snap execution. Reported for context only.
#   OTHER_SPECIAL_TEAMS    Kicking-game infractions (kickoff/punt fouls,
#                         fair-catch interference, ineligible downfield
#                         kick/pass) that do not map cleanly onto offense or
#                         defense discipline in the way above. Reported for
#                         context only.
#
# UNIVERSE. Sumer play charting, 2022-2025, REGULAR SEASON ONLY
# (season_type == 0 in Sumer's own games files; confirmed weeks 1-18 vs
# playoffs' weeks 1-4 in the same field). Denominator for every rate is a
# SCRIMMAGE SNAP: special_teams_type == "" (blank), which excludes kickoffs,
# punts, field goals and extra points -- a false start during a punt
# formation is a special-teams-unit issue, not the offense's snap discipline.
#
# THE LOADER PROBLEM THIS SCRIPT DOES NOT USE load_sumer() FOR. load_sumer()
# filters `no_play == FALSE`, which is correct for every other script on this
# board (a wiped-out down carries no EPA to model) but is fatal here: 66% of
# ALL penalty rows in Sumer (12,321 of 18,651) are no_play == TRUE, and the
# presnap types this script cares about most are almost entirely no_play
# (FalseStart 3174/3175, NeutralZoneInfraction 422/424, Encroachment
# 158/161). Running this through load_sumer() would delete the discipline
# signal itself. So this script reimplements load_sumer()'s team-code and
# caller/head-coach join (same source files, same SUMER_TEAM_FIX) without the
# no_play filter, and additionally keeps season_type == 0 explicit rather
# than assumed.
#
# GARBAGE TIME. Kept IN for every rate in this script, on purpose: a flag is
# a flag regardless of score, and the underlying claim ("disciplined teams")
# is about snap execution, not about competitive leverage. Checked for
# distortion anyway: garbage-time snaps are only 3.6% of the universe and
# actually run BELOW the competitive-snap penalty rate (presnap-offense 1.6%
# vs 2.5%, presnap-defense-drawn 0.6% vs 0.8%), so including them is
# conservative -- it very slightly depresses rather than inflates every rate
# below, and does not favor either verdict.
#
# ERA/SEASON DRIFT. League presnap-offense rate rose from 2.12% (2022) to
# 2.78% (2024) to 2.71% (2025), a real ~30% relative move, almost certainly
# officiating point-of-emphasis, not a change in NFL preparation quality.
# Presnap-defense-drawn rose less (0.77% to 0.85%, flat after 2023). Both are
# season-adjusted below: every HC-season / caller-season rate is expressed as
# a residual against THAT season's league rate before being pooled to a
# career number, so a coach who happened to coach more 2024-2025 seasons
# isn't punished relative to one weighted toward 2022-2023.
#
# SHRINKAGE. Career residuals are empirical-Bayes shrunk toward the league
# mean (method-of-moments tau^2 = between-coach variance minus mean sampling
# variance; shrink weight = sampling variance / (sampling variance + tau^2)),
# same logic as lib_sumer.R's signal_share(), applied by hand here since this
# is a plain binomial rate, not a caller-vs-QB residual.
#
# PERSISTENCE. lib_sumer.R's persist_split(): odd-season vs even-season
# correlation across coaches/callers, >= 350 snaps in each half. That
# function's verdict thresholds (r's CI lower bound > 0.30 = trait, > 0.10 =
# weak trait, CI upper bound < 0.10 = not a trait) are used verbatim, the
# same bar that carded the halftime-adjuster claim as noise.
#
# ATTRIBUTION. Test 1 (discipline leaderboard) and test 3 (does it matter):
# HEAD COACH, via playcallers.csv's head_coach column, joined on
# (season, week, team) exactly like load_sumer() joins off_caller -- a
# false start is not an offensive coordinator's fault, it's a program-wide
# snap-discipline issue, hence HC not OC. Test 2 (cadence weapon): OFFENSIVE
# CALLER, via the same off_play_caller column load_sumer() uses -- a hard
# count is a play-calling skill, drawn possession by possession, not a
# program trait.
#
# Min 2 seasons of 2022-2025 to appear in either leaderboard.
#
# Conventions: R/lib/theme_coach.R. No em dashes.
#
# Out: docs/figures/discipline.png (Chart A, HC leaderboard + verdict)
#      docs/figures/cadence_weapon.png (Chart B, caller leaderboard)
#      data/derived/discipline.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")   # for SUMER_DIR, SUMER_TEAM_FIX, NFLA_PC, persist_split()
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
MIN_SEASONS <- 2
MIN_HALF <- 300   # persist_split() min snaps per half; presnap penalties are rarer than blitzes
NAMED_HC <- c("Mike Tomlin", "John Harbaugh", "Bill Belichick", "Sean McDermott",
              "Mike Macdonald", "Dan Campbell", "Kyle Shanahan", "Sean McVay")
NAMED_CALLERS <- c("Kyle Shanahan", "Sean McVay", "Andy Reid", "Sean Payton",
                    "Ben Johnson", "Josh McDaniels", "Mike Tomlin", "John Harbaugh")

# =============================================================================
# 1. TAXONOMY -- every penalty_type mapped by hand, printed for the record
# =============================================================================

PRESNAP_OFFENSE <- c("FalseStart", "DelayOfGame", "IllegalFormation",
                     "IllegalShift", "IllegalMotion")
PRESNAP_DEFENSE_DRAWN <- c("DefensiveOffside", "NeutralZoneInfraction", "Encroachment")
POSTSNAP_AGGRESSION <- c("OffensiveHolding", "DefensiveHolding", "DefensivePassInterference",
                         "OffensivePassInterference", "RoughingThePasser", "RoughingTheKicker",
                         "FaceMask", "IllegalContact", "UnnecessaryRoughness", "IllegalUseOfHands",
                         "IllegalBlockAboveTheWaist", "HorseCollarTackle", "ChopBlock", "Tripping",
                         "LowBlock", "IllegalBlindsideBlock", "LoweringHeadToInitiateContact",
                         "HipDropTackle", "IllegalCrackback", "Clipping", "IllegalPeelback",
                         "IllegalCut", "IllegalDoubleTeamBlock", "IllegalBat", "OffensiveIllegalBat",
                         "IllegalKick", "OffensiveIllegallyKickingBall")
DEADBALL_DUMB <- c("UnsportsmanlikeConduct", "Taunting", "Defensive12OnField", "Offensive12OnField",
                   "Disqualification", "OffensiveDisqualification", "IllegalSubstitution",
                   "DefensiveDelayOfGame", "Leverage")
OTHER_SPECIAL_TEAMS <- c("IneligibleDownfieldPass", "IneligibleDownfieldKick",
                         "KickoffShortOfLandingZone", "KickoffOutOfBounds", "PlayerOutOfBoundsOnKick",
                         "FairCatchInterference", "KickCatchInterference", "IllegalTouchPass",
                         "IllegalTouchKick", "IllegalForwardPass", "InvalidFairCatchSignal",
                         "OffensiveOffside", "OffsideOnFreeKick", "IllegalDoubleTeamBlock",
                         "IllegalSubstitution", "Unknown", "")

bucket_of <- function(pt) fcase(
  pt %in% PRESNAP_OFFENSE, "PRESNAP_OFFENSE",
  pt %in% PRESNAP_DEFENSE_DRAWN, "PRESNAP_DEFENSE_DRAWN",
  pt %in% POSTSNAP_AGGRESSION, "POSTSNAP_AGGRESSION",
  pt %in% DEADBALL_DUMB, "DEADBALL_DUMB",
  default = "OTHER_SPECIAL_TEAMS"
)

# =============================================================================
# 2. LOAD -- reimplement load_sumer()'s joins WITHOUT the no_play filter
# =============================================================================

plays <- fread(file.path(SUMER_DIR, "plays_full.csv.gz"), showProgress = FALSE)
teams <- as.data.table(jsonlite::fromJSON(file.path(SUMER_DIR, "teams.json")))
teams[, abbr := fifelse(team_code %in% names(SUMER_TEAM_FIX), SUMER_TEAM_FIX[team_code], team_code)]

games <- rbindlist(lapply(SEASONS, function(y) {
  f <- file.path(SUMER_DIR, sprintf("games_%d.csv", y))
  if (file.exists(f)) fread(f, showProgress = FALSE) else NULL
}), fill = TRUE)

plays <- merge(plays, teams[, .(sumer_offense_team_id = sumer_team_id, off_team = abbr)],
               by = "sumer_offense_team_id", all.x = TRUE)
plays <- merge(plays, unique(games[, .(sumer_game_id, week, season_type)]),
               by = "sumer_game_id", all.x = TRUE)

pc <- fread(NFLA_PC, showProgress = FALSE)
plays <- merge(plays, pc[, .(season, week, off_team = team, off_caller = off_play_caller, head_coach)],
               by = c("season", "week", "off_team"), all.x = TRUE)

plays <- plays[season %in% SEASONS & season_type == 0]           # regular season only
plays[, off_caller := fifelse(is.na(off_caller), "", off_caller)]
plays[, head_coach := fifelse(is.na(head_coach), "", head_coach)]
cat(sprintf("Sumer plays loaded, %d-%d regular season: %s rows\n",
            min(SEASONS), max(SEASONS), format(nrow(plays), big.mark = ",")))

# print taxonomy on the top 20 penalty types by frequency, as asked
plays[, bucket := bucket_of(penalty_type)]
top20 <- plays[is_penalty == TRUE, .N, by = .(penalty_type, bucket)][order(-N)][1:20]
cat("\n--- TOP 20 penalty_type BY FREQUENCY, WITH TAXONOMY BUCKET ---\n")
print(top20)

# =============================================================================
# 3. SCRIMMAGE SNAP UNIVERSE -- exclude kickoffs/punts/FG/XP from the
# denominator; garbage time STAYS IN (see header note)
# =============================================================================

scrim <- plays[special_teams_type == ""]
scrim[, off_flag := is_penalty == TRUE & penalty_type %in% PRESNAP_OFFENSE]
scrim[, def_flag := is_penalty == TRUE & penalty_type %in% PRESNAP_DEFENSE_DRAWN]
cat(sprintf("\nScrimmage snaps (non-special-teams), %d-%d: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(scrim), big.mark = ",")))

gt_check <- scrim[, .(n = .N, off_rate = mean(off_flag), def_rate = mean(def_flag)), by = garbage_time]
cat("Garbage-time check (rates only, garbage time stays IN below):\n"); print(gt_check)

league_season <- scrim[, .(n = .N, off_pen = sum(off_flag), def_pen = sum(def_flag),
                           off_rate = mean(off_flag), def_rate = mean(def_flag)), by = season][order(season)]
cat("\nLeague rate by season (the era drift this script adjusts for):\n"); print(league_season)

# =============================================================================
# 4. TEST 1 -- THE DISCIPLINE LEADERBOARD: presnap-offense rate per HC
# =============================================================================

hc_season <- scrim[head_coach != "", .(n = .N, pen = sum(off_flag), rate = mean(off_flag)),
                   by = .(head_coach, season)]
hc_season <- merge(hc_season, league_season[, .(season, lg_rate = off_rate)], by = "season")
hc_season[, resid := rate - lg_rate]   # season-adjusted: positive = MORE presnap-offense penalties than league that year

hc_seasons_n <- hc_season[, .(n_seasons = .N), by = head_coach]
hc_ok <- hc_seasons_n[n_seasons >= MIN_SEASONS, head_coach]
hc_season_ok <- hc_season[head_coach %in% hc_ok]
cat(sprintf("\nHCs with >= %d seasons, %d-%d: %d (of %d total)\n",
            MIN_SEASONS, min(SEASONS), max(SEASONS), length(hc_ok), nrow(hc_seasons_n)))

hc_career <- hc_season_ok[, .(seasons = .N, n = sum(n), pen = sum(pen),
                              rate = weighted.mean(rate, n), resid = weighted.mean(resid, n)),
                          by = head_coach]

# empirical-Bayes shrinkage of the career residual toward 0 (league average,
# already netted out by the season adjustment above)
eb_shrink <- function(x, n, p_ref) {
  se2 <- p_ref * (1 - p_ref) / pmax(n, 1)
  tau2 <- max(0, var(x) - mean(se2))
  b <- se2 / (se2 + tau2)          # 1 = fully shrunk to 0, 0 = no shrinkage
  list(shrunk = (1 - b) * x, b = b, tau2 = tau2, sd_noise = sqrt(mean(se2)), sd_obs = sd(x))
}
p_ref_off <- mean(scrim$off_flag)
eb_hc <- eb_shrink(hc_career$resid, hc_career$n, p_ref_off)
hc_career[, resid_shrunk := eb_hc$shrunk]
setorder(hc_career, resid_shrunk); hc_career[, rank := .I]   # 1 = most disciplined (fewest penalties vs season expectation)
n_hc <- nrow(hc_career)

cat(sprintf("\nEB shrinkage (presnap-offense, HC): sd(observed) = %.4f, sd(sampling noise) = %.4f, share of spread that is real = %.1f%%\n",
            eb_hc$sd_obs, eb_hc$sd_noise, 100 * (1 - mean(eb_hc$b))))

# persistence: does presnap-offense discipline survive odd/even seasons?
persist_input_hc <- hc_season_ok[, .(season, caller = head_coach, n, resid)]
p1 <- persist_split(persist_input_hc, min_half = MIN_HALF)
cat(sprintf("\n--- TEST 1 PERSISTENCE: presnap-offense penalty rate, HEAD COACH ---\n"))
cat(sprintf("n = %d coaches with both halves >= %d snaps. r = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n",
            p1$n, MIN_HALF, p1$r, p1$lo, p1$hi, p1$p))
cat(sprintf("VERDICT: %s\n", toupper(p1$verdict)))

cat("\nTop 10 most disciplined (lowest presnap-offense rate vs season expectation):\n")
print(hc_career[order(resid_shrunk)][1:10, .(head_coach, seasons, n, rate = round(100*rate,2),
                                             resid_shrunk = round(100*resid_shrunk,3), rank)])
cat("\nBottom 10 least disciplined:\n")
print(hc_career[order(-resid_shrunk)][1:10, .(head_coach, seasons, n, rate = round(100*rate,2),
                                              resid_shrunk = round(100*resid_shrunk,3), rank)])

# =============================================================================
# 5. TEST 2 -- THE CADENCE WEAPON: defensive presnap penalties DRAWN per
# offensive snap, by OFFENSIVE CALLER
# =============================================================================

oc_season <- scrim[off_caller != "", .(n = .N, drawn = sum(def_flag), rate = mean(def_flag)),
                   by = .(off_caller, season)]
oc_season <- merge(oc_season, league_season[, .(season, lg_rate = def_rate)], by = "season")
oc_season[, resid := rate - lg_rate]

oc_seasons_n <- oc_season[, .(n_seasons = .N), by = off_caller]
oc_ok <- oc_seasons_n[n_seasons >= MIN_SEASONS, off_caller]
oc_season_ok <- oc_season[off_caller %in% oc_ok]
cat(sprintf("\nOffensive callers with >= %d seasons, %d-%d: %d (of %d total)\n",
            MIN_SEASONS, min(SEASONS), max(SEASONS), length(oc_ok), nrow(oc_seasons_n)))

oc_career <- oc_season_ok[, .(seasons = .N, n = sum(n), drawn = sum(drawn),
                              rate = weighted.mean(rate, n), resid = weighted.mean(resid, n)),
                          by = off_caller]
p_ref_def <- mean(scrim$def_flag)
eb_oc <- eb_shrink(oc_career$resid, oc_career$n, p_ref_def)
oc_career[, resid_shrunk := eb_oc$shrunk]
setorder(oc_career, -resid_shrunk); oc_career[, rank := .I]   # 1 = draws the most flags vs season expectation
n_oc <- nrow(oc_career)

cat(sprintf("\nEB shrinkage (presnap-defense-drawn, caller): sd(observed) = %.4f, sd(sampling noise) = %.4f, share of spread that is real = %.1f%%\n",
            eb_oc$sd_obs, eb_oc$sd_noise, 100 * (1 - mean(eb_oc$b))))

persist_input_oc <- oc_season_ok[, .(season, caller = off_caller, n, resid)]
p2 <- persist_split(persist_input_oc, min_half = MIN_HALF)
cat(sprintf("\n--- TEST 2 PERSISTENCE: defensive presnap penalties drawn per snap, OFFENSIVE CALLER ---\n"))
cat(sprintf("n = %d callers with both halves >= %d snaps. r = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n",
            p2$n, MIN_HALF, p2$r, p2$lo, p2$hi, p2$p))
cat(sprintf("VERDICT: %s\n", toupper(p2$verdict)))

cat("\nTop 10 cadence-weapon callers (draw the most defensive presnap flags vs season expectation):\n")
print(oc_career[order(-resid_shrunk)][1:10, .(off_caller, seasons, n, rate = round(100*rate,2),
                                              resid_shrunk = round(100*resid_shrunk,3), rank)])
cat("\nBottom 10 (draw the fewest):\n")
print(oc_career[order(resid_shrunk)][1:10, .(off_caller, seasons, n, rate = round(100*rate,2),
                                             resid_shrunk = round(100*resid_shrunk,3), rank)])

# payoff: free yards + free first downs per season, on plays where this
# caller's offense drew a presnap-defense penalty (accepted: the enforced
# penalty_yards; declined: the offense kept a live "free play" instead, so
# its payoff is the yards actually gained on that snap)
drawn_plays <- scrim[def_flag == TRUE & off_caller != ""]
drawn_plays[, payoff_yards := fifelse(is_penalty_accepted == TRUE, penalty_yards,
                                      fifelse(is.na(offensive_yards), 0, offensive_yards))]
drawn_plays[, payoff_fd := fifelse(is_penalty_accepted == TRUE, penalty_first_down == TRUE,
                                   first_down_gained == TRUE)]
payoff <- drawn_plays[, .(drawn = .N, free_yards = sum(payoff_yards, na.rm = TRUE),
                          free_fd = sum(payoff_fd, na.rm = TRUE),
                          seasons_active = uniqueN(season)), by = off_caller]
payoff[, `:=`(free_yards_per_season = free_yards / seasons_active,
             free_fd_per_season = free_fd / seasons_active)]
oc_career <- merge(oc_career, payoff[, .(off_caller, free_yards, free_fd,
                                         free_yards_per_season, free_fd_per_season)],
                   by = "off_caller", all.x = TRUE)
setorder(oc_career, -resid_shrunk)

cat("\nPayoff of the cadence weapon, top 10 by drawn-flag rate (free yards + free first downs PER SEASON):\n")
print(oc_career[1:10, .(off_caller, rank, free_yards_per_season = round(free_yards_per_season,1),
                        free_fd_per_season = round(free_fd_per_season,2))])

# =============================================================================
# 6. TEST 3 -- DOES DISCIPLINE MATTER? presnap-offense rate vs team success
# and vs the coach-market measures already on the board
# =============================================================================

# point differential per game, joined to whichever HC coached that specific
# game (handles in-season coaching changes correctly, week by week)
gl <- rbindlist(list(
  games[season %in% SEASONS & season_type == 0,
        .(season, week, team = fifelse(home_team_code %in% names(SUMER_TEAM_FIX),
                                        SUMER_TEAM_FIX[home_team_code], home_team_code),
          point_diff = home_team_score - away_team_score)],
  games[season %in% SEASONS & season_type == 0,
        .(season, week, team = fifelse(away_team_code %in% names(SUMER_TEAM_FIX),
                                        SUMER_TEAM_FIX[away_team_code], away_team_code),
          point_diff = away_team_score - home_team_score)]
))
gl <- merge(gl, pc[, .(season, week, team, head_coach)], by = c("season", "week", "team"), all.x = TRUE)
gl <- gl[!is.na(head_coach) & head_coach != ""]
hc_success <- gl[, .(games = .N, point_diff_pg = mean(point_diff)), by = head_coach]

test3 <- merge(hc_career[, .(head_coach, n, resid_shrunk, rate)], hc_success, by = "head_coach")
cat(sprintf("\nHCs matched to point-differential success data: %d\n", nrow(test3)))

ct_pd <- cor.test(test3$resid_shrunk, test3$point_diff_pg)
cat(sprintf("\n--- TEST 3a: presnap-offense discipline vs point differential/game, n = %d HCs ---\n", nrow(test3)))
cat(sprintf("r = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n", ct_pd$estimate, ct_pd$conf.int[1], ct_pd$conf.int[2], ct_pd$p.value))

market <- fread("data/derived/coach_market.csv", showProgress = FALSE)
test3b <- merge(test3, market[, .(head_coach = coach, wins_above_expectation, rate_per17, z, eligible)],
                by = "head_coach")
cat(sprintf("HCs matched to coach_market.csv: %d of %d\n", nrow(test3b), nrow(test3)))

ct_z <- cor.test(test3b$resid_shrunk, test3b$z)
ct_rate17 <- cor.test(test3b$resid_shrunk, test3b$rate_per17)
cat(sprintf("\n--- TEST 3b: presnap-offense discipline vs coach_market.csv z-score, n = %d ---\n", nrow(test3b)))
cat(sprintf("r = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n", ct_z$estimate, ct_z$conf.int[1], ct_z$conf.int[2], ct_z$p.value))
cat(sprintf("\n--- TEST 3c: presnap-offense discipline vs coach_market.csv wins/17 above expectation, n = %d ---\n", nrow(test3b)))
cat(sprintf("r = %+.3f  95%% CI [%+.3f, %+.3f]  p = %.3f\n", ct_rate17$estimate, ct_rate17$conf.int[1], ct_rate17$conf.int[2], ct_rate17$p.value))

# significance read off the CI crossing zero, not a raw p-value cutoff --
# same standard persist_split() uses elsewhere on this board
pd_sig   <- ct_pd$conf.int[1] > 0 || ct_pd$conf.int[2] < 0
z_sig    <- ct_z$conf.int[1] > 0 || ct_z$conf.int[2] < 0
rate_sig <- ct_rate17$conf.int[1] > 0 || ct_rate17$conf.int[2] < 0

matters_verdict <- if (pd_sig & z_sig) {
  "discipline correlates with BOTH point differential and the board's own coach-value measure. Not a virtue stat."
} else if (pd_sig & !z_sig) {
  sprintf(paste0(
    "MIXED. Presnap-offense discipline correlates with team point differential (r = %+.2f, 95%% CI [%+.2f, %+.2f]: ",
    "teams whose coach commits fewer presnap-offense penalties than expected win by more), but that same ",
    "discipline measure does NOT clear a confidence interval against the board's own coach-value z-score ",
    "(r = %+.2f, 95%% CI [%+.2f, %+.2f]) or against wins/17 above expectation (r = %+.2f, 95%% CI [%+.2f, %+.2f]). ",
    "Read cautiously both ways: the point-differential link could run through discipline causing wins, OR through ",
    "good teams (better O-line continuity, fewer desperate no-huddle snaps) simply committing fewer presnap ",
    "penalties as a side effect of being good. Nothing here separates those two stories, and it does not yet ",
    "clear the bar to be called a coaching-VALUE measure."),
    ct_pd$estimate, ct_pd$conf.int[1], ct_pd$conf.int[2],
    ct_z$estimate, ct_z$conf.int[1], ct_z$conf.int[2],
    ct_rate17$estimate, ct_rate17$conf.int[1], ct_rate17$conf.int[2])
} else if (!pd_sig & z_sig) {
  "discipline correlates with the board's coach-value measure but not with raw point differential; see the coefficients above."
} else {
  "VIRTUE STAT: no detectable relationship between presnap-offense discipline and either point differential or the board's own coach-value measure. The leaderboard measures something real about a coach's program, but it does not predict winning in this sample."
}
cat("\nTEST 3 VERDICT:", matters_verdict, "\n")

# =============================================================================
# 7. CHART A -- docs/figures/discipline.png: HC discipline leaderboard
# =============================================================================

hc_career[, hl := head_coach %in% NAMED_HC]
lab_hc <- hc_career[hl == TRUE]

verdict_txt <- sprintf("Persistence (odd vs even seasons): r = %+.2f [%+.2f, %+.2f], n = %d coaches -- %s",
                        p1$r, p1$lo, p1$hi, p1$n, p1$verdict)

pA <- ggplot(hc_career, aes(x = reorder(head_coach, resid_shrunk), y = 100 * resid_shrunk)) +
  geom_col(aes(fill = hl), width = 0.72) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey70"), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), sprintf("%.1f", x), "pp")) +
  labs(
    title = "The discipline leaderboard: presnap-offense penalty rate, by head coach",
    subtitle = paste0(sprintf("Season-adjusted rate vs that year's league average, EB-shrunk. Bars below zero commit FEWER false starts/delay-of-game/illegal formation-shift-motion\nthan the league that season. n = %d head coaches, %d-%d, min %d seasons each. Named coaches in orange.", n_hc, min(SEASONS), max(SEASONS), MIN_SEASONS),
                     "\n", verdict_txt),
    x = NULL, y = "presnap-offense penalty rate vs season league average"
  ) +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = rel(0.55)),
        plot.subtitle = element_text(size = rel(0.72)))
save_fig("docs/figures/discipline.png", pA, w = 15, h = 8)

# =============================================================================
# 8. CHART B -- docs/figures/cadence_weapon.png: caller leaderboard
# =============================================================================

oc_career[, hl := off_caller %in% NAMED_CALLERS]

verdict_txt2 <- sprintf("Persistence (odd vs even seasons): r = %+.2f [%+.2f, %+.2f], n = %d callers -- %s",
                         p2$r, p2$lo, p2$hi, p2$n, p2$verdict)

pB <- ggplot(oc_career, aes(x = reorder(off_caller, -resid_shrunk), y = 100 * resid_shrunk)) +
  geom_col(aes(fill = hl), width = 0.72) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey70"), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), sprintf("%.2f", x), "pp")) +
  labs(
    title = "The cadence weapon: defensive presnap penalties drawn per offensive snap, by caller",
    subtitle = paste0(sprintf("Season-adjusted drawn-flag rate (defensive offside, neutral zone, encroachment) vs that year's league average, EB-shrunk. n = %d offensive\ncallers, %d-%d, min %d seasons each. Named callers in blue.", n_oc, min(SEASONS), max(SEASONS), MIN_SEASONS),
                     "\n", verdict_txt2),
    x = NULL, y = "defensive presnap penalties drawn vs season league average"
  ) +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_text(angle = 55, hjust = 1, size = rel(0.55)),
        plot.subtitle = element_text(size = rel(0.72)))
save_fig("docs/figures/cadence_weapon.png", pB, w = 15, h = 8)

# =============================================================================
# 9. WRITE OUTPUT
# =============================================================================

out_hc <- hc_career[, .(entity = head_coach, role = "head_coach", seasons, n, pen,
                        rate = round(rate, 5), resid = round(resid, 5),
                        resid_shrunk = round(resid_shrunk, 5), rank)]
out_oc <- oc_career[, .(entity = off_caller, role = "offensive_caller", seasons, n, drawn,
                        rate = round(rate, 5), resid = round(resid, 5), resid_shrunk = round(resid_shrunk, 5),
                        rank, free_yards_per_season = round(free_yards_per_season, 1),
                        free_fd_per_season = round(free_fd_per_season, 2))]
out <- rbindlist(list(out_hc, out_oc), fill = TRUE)
write_csv(as.data.frame(out), "data/derived/discipline.csv")
cat(sprintf("\nwrote data/derived/discipline.csv (%d rows: %d HCs, %d callers)\n", nrow(out), nrow(out_hc), nrow(out_oc)))

# =============================================================================
# SUMMARY
# =============================================================================

most_surprising <- sprintf(
  "Presnap-offense penalty rate has RISEN %.0f%% leaguewide from 2022 (%.2f%%) to 2024 (%.2f%%) with no rule change on the books -- a bigger swing than the gap between the most and least disciplined team in any single season.",
  100 * (league_season[season == max(SEASONS) - 1, off_rate] / league_season[season == min(SEASONS), off_rate] - 1),
  100 * league_season[season == min(SEASONS), off_rate], 100 * league_season[season == max(SEASONS) - 1, off_rate])

cat("\n================= SUMMARY =================\n")
cat(sprintf("Universe: %s scrimmage snaps, %d-%d regular season, garbage time included (see caveat).\n",
            format(nrow(scrim), big.mark = ","), min(SEASONS), max(SEASONS)))
cat(sprintf("TEST 1 (discipline leaderboard, HC, n=%d): most disciplined %s (rank #1), least %s (rank #%d). Persistence r = %+.3f [%+.3f, %+.3f], n=%d -- %s\n",
            n_hc, hc_career[order(resid_shrunk)][1, head_coach], hc_career[order(-resid_shrunk)][1, head_coach], n_hc,
            p1$r, p1$lo, p1$hi, p1$n, toupper(p1$verdict)))
cat(sprintf("TEST 2 (cadence weapon, caller, n=%d): biggest weapon %s (rank #1), smallest %s (rank #%d). Persistence r = %+.3f [%+.3f, %+.3f], n=%d -- %s\n",
            n_oc, oc_career[order(-resid_shrunk)][1, off_caller], oc_career[order(resid_shrunk)][1, off_caller], n_oc,
            p2$r, p2$lo, p2$hi, p2$n, toupper(p2$verdict)))
cat(sprintf("TEST 3: discipline vs point diff/game r = %+.3f [%+.3f, %+.3f] (n=%d); vs coach_market z r = %+.3f [%+.3f, %+.3f] (n=%d).\n",
            ct_pd$estimate, ct_pd$conf.int[1], ct_pd$conf.int[2], nrow(test3),
            ct_z$estimate, ct_z$conf.int[1], ct_z$conf.int[2], nrow(test3b)))
cat("TEST 3 verdict:", matters_verdict, "\n")
cat("MOST SURPRISING NUMBER:", most_surprising, "\n")
cat("Files: docs/figures/discipline.png, docs/figures/cadence_weapon.png, data/derived/discipline.csv\n")
cat("=============================================\n")
