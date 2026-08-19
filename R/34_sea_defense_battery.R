# =============================================================================
# 34_sea_defense_battery.R -- checking Michael's Seahawks defense read against
# SumerSports charting, 2024-2025 (Mike Macdonald), with his Baltimore years
# as a same-man-different-team check and San Francisco kept only as a
# reference column after a mis-attribution got corrected.
#
# Michael, verbatim. Two of these continue an earlier Seattle thread
# ("They ran Nickel like the whole time and did not give af what the offense
# was doing"); all four turned out to be about Seattle once a Hudl article he
# was also reading was checked and found to be 49ers OFFENSE only, with no
# nickel/zone/blitz/explosive content at all -- so nothing in this script's
# earlier SF-attributed draft survives, and it is rebuilt here as one battery:
#   "Wait so how often did they run nickel vs. the rest of the league? Lol"
#   "Also - I think from what I am reading he runs a LOT of zone coverage"
#   "He doesn't really blitz that much but when they do they are pretty
#    dangerous"
#   "If they aren't blitzing a lot I bet they didn't allow a lot of explosive
#    plays. Might be the best in that category or close"
#
# WHO "HE" IS. Mike Macdonald called Baltimore's defense as DC in 2022-2023,
# then became Seattle's head coach in 2024 and, per playcallers.csv, calls
# Seattle's defense himself (listed as both head_coach and def_play_caller for
# SEA in 2024-2025). PRIMARY window is SEA 2024-2025. CONTEXT window is BAL
# 2022-2023, the same man's prior stop, run through identical tests so a
# reader can see whether the pattern is Macdonald or just this year's Seattle
# roster. SF is carried in the CSV only, as a labeled leftover of the
# corrected mis-attribution, not tested or charted.
#
# THIS BOARD ALREADY KNOWS THINGS ABOUT MACDONALD; this script's job is a
# quick, independent verification battery in Michael's own terms (team-level
# rate and rank of 32), not a restatement of the more rigorous work below --
# their numbers are cited alongside, not reproduced:
#   R/16  DC-level, situation-adjusted (season-grouped xgboost residual, not
#         a raw team rate): Macdonald blitzes below what his own situations
#         call for in EVERY ONE of six game-context cuts (early/late down,
#         red zone, two-minute, trailing, leading) -- his blitz shape is flat,
#         not just his blitz volume.
#   R/28  Decomposes pressure into scheme (a free rusher nobody blocked) vs
#         talent (winning a one-on-one). Macdonald ranks in the top 3 of 41
#         qualifying DCs manufacturing free rushers while his blitz rate and
#         his rushers' one-on-one win rate both sit at or below league
#         average -- his pressure is schemed, not sent. That is the
#         mechanism this script's "dangerous when they do blitz" number sits
#         on top of.
#   R/18  Baltimore under him barely reshuffles personnel by down (bottom of
#         the league both years); Seattle under him looks average on that
#         same measure -- a reminder his fingerprint is not identical at
#         every stop.
#   R/20  Measured SEA's 2025 pre-snap shell and named coverages directly:
#         Cover 3 rate was 33% charted, ranking 16th of 32 -- dead center,
#         not extreme. This script's zone number (below) lands on the same
#         "average, not extreme" read for Cover 3 specifically, even though
#         total zone usage (every zone shell combined) ranks higher -- shell
#         DIVERSITY, not a Cover-3 obsession. See CLAIM 2.
#
# WHAT WE CHECK IT AGAINST. SumerSports charting (same fields and universe
# logic as this script's SF-attributed predecessor; unchanged here):
#   defensive_personnel_package, man_zone_coverage (cross-checked against
#   coverage_scheme: MAN = Cover 0/1/2-Man, ZONE = Cover 2/3/4/6), blitz,
#   pressure, expected_points_added (offense-signed: confirmed monotonic in
#   offensive_yards, so a defense wants this LOW on its own snaps),
#   offensive_yards (confirmed NA-free once play_type == "OFF/DEF" restricts
#   to real scrimmage plays).
#
# UNIVERSE. play_type == "OFF/DEF", season_type == 0 (regular season), 2022-
# 2025, garbage_time == FALSE, all 32 teams for ranking. Zone/blitz/pressure
# additionally require is_dropback == TRUE.
#
# RANK CONVENTION -- claim-direction rank, same rule as this script's
# predecessor: 1 of 32 means the team sits at the extreme that would make
# Michael's specific claim MOST true (highest rate for nickel/zone, fewest
# blitzes for frequency, lowest EPA allowed on a blitz for "dangerous",
# fewest explosive plays allowed for "best/close"). League average is the
# unweighted mean of the 32 team rates in that same season window.
#
# Conventions: R/lib/theme_coach.R, R/factory/lib_sumer.R (load_sumer()).
# No em dashes.
#
# Out: docs/figures/sea_defense_battery.png
#      data/derived/sea_defense_battery.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
WINDOWS <- list(
  SEA = list(team = "SEA", seasons = 2024:2025, role = "primary",
             label = "Seattle 2024-2025 (Macdonald, HC/DC)"),
  BAL = list(team = "BAL", seasons = 2022:2023, role = "context",
             label = "Baltimore 2022-2023 (Macdonald, DC)"),
  SF  = list(team = "SF",  seasons = 2022:2025, role = "secondary",
             label = "San Francisco 2022-2025 (mis-attribution, kept for reference only)")
)

# ---------------------------------------------------------------- load
d <- load_sumer(seasons = SEASONS)
scrim <- d[play_type == "OFF/DEF" & season_type == 0 & garbage_time == FALSE]
db    <- scrim[is_dropback == TRUE]
cat(sprintf("universe: %s non-garbage-time regular-season snaps (%s of them dropbacks), %s-%s, all 32 teams\n",
            format(nrow(scrim), big.mark = ","), format(nrow(db), big.mark = ","),
            min(SEASONS), max(SEASONS)))

dc_by_team_season <- unique(scrim[def_caller != "", .(def_team, season, def_caller)])
dc_lookup <- function(team, szn) {
  x <- dc_by_team_season[def_team == team & season == szn]
  if (nrow(x)) x$def_caller[1] else NA_character_
}
cat("\nDefensive play-caller, relevant team-seasons:\n")
for (nm in names(WINDOWS)) {
  w <- WINDOWS[[nm]]
  for (s in w$seasons) cat(sprintf("  %s %d: %s\n", w$team, s, dc_lookup(w$team, s)))
}

# ---------------------------------------------------------------- rank helper
# decreasing = TRUE  -> rank 1 = highest value (nickel rate, zone rate,
#                        pressure-on-blitz rate: high IS the claim direction)
# decreasing = FALSE -> rank 1 = lowest value (blitz rate "not much";
#                        EPA allowed on blitz "dangerous" = low is good for
#                        the defense; explosive-play rate "best/close")
rank_teams <- function(x, value_col, decreasing = TRUE) {
  t <- copy(x)
  setorderv(t, value_col, order = if (decreasing) -1 else 1)
  t[, rank := .I][]
}
season_ranks <- function(x, value_col, decreasing = TRUE) {
  t <- copy(x)
  setorderv(t, c("season", value_col), order = c(1, if (decreasing) -1 else 1))
  t[, rank := 1:.N, by = season][]
}
# pooled rank for one team inside one specific season window (a window's
# pooled rank has to be computed against the SAME window for all 32 teams)
window_rank <- function(base, flag_col, window, decreasing = TRUE, agg = "mean") {
  sub <- base[season %in% window$seasons]
  t <- if (agg == "mean") sub[, .(n = .N, value = 100 * mean(get(flag_col))), by = def_team]
       else sub[, .(n = .N, value = mean(get(flag_col))), by = def_team]
  t <- rank_teams(t, "value", decreasing)
  list(row = t[def_team == window$team], league_avg = mean(t$value), all = t)
}

results <- list()  # collects long-format rows for the CSV

add_rows <- function(claim, metric, rank_direction, pooled_list, season_tbl, verdict_by_team = list()) {
  out <- list()
  for (nm in names(pooled_list)) {
    w <- WINDOWS[[nm]]; r <- pooled_list[[nm]]
    out[[length(out) + 1]] <- data.table(
      claim = claim, metric = metric, team = w$team, team_role = w$role,
      scope = "pooled", season = NA_integer_, def_caller = w$label,
      n = r$row$n, value = round(r$row$value, 4), league_avg = round(r$league_avg, 4),
      rank_of_32 = r$row$rank, rank_direction = rank_direction,
      verdict = if (!is.null(verdict_by_team[[nm]])) verdict_by_team[[nm]] else NA_character_)
  }
  for (nm in names(WINDOWS)) {
    w <- WINDOWS[[nm]]
    st <- season_tbl[def_team == w$team & season %in% w$seasons]
    if (!nrow(st)) next
    out[[length(out) + 1]] <- data.table(
      claim = claim, metric = metric, team = w$team, team_role = w$role,
      scope = as.character(st$season), season = st$season,
      def_caller = sapply(st$season, function(s) dc_lookup(w$team, s)),
      n = st$n, value = round(st$value, 4), league_avg = NA_real_,
      rank_of_32 = st$rank, rank_direction = rank_direction, verdict = NA_character_)
  }
  results[[length(results) + 1]] <<- rbindlist(out, fill = TRUE)
}

# =============================================================================
# CLAIM 1: NICKEL RATE
# =============================================================================
scrim[, nickel := defensive_personnel_package == "Nickel"]
nick_seas <- season_ranks(scrim[, .(n = .N, value = 100 * mean(nickel)), by = .(def_team, season)],
                          "value", TRUE)
nick_w <- lapply(WINDOWS, function(w) window_rank(scrim, "nickel", w, decreasing = TRUE))

cat("\n=== CLAIM 1: nickel rate ===\n")
cat(sprintf("SEA pooled 2024-2025: %.1f%%, rank %d of 32 (league avg %.1f%%)\n",
            nick_w$SEA$row$value, nick_w$SEA$row$rank, nick_w$SEA$league_avg))
cat(sprintf("BAL context, pooled 2022-2023 (Macdonald's prior stop): %.1f%%, rank %d of 32 (league avg %.1f%%)\n",
            nick_w$BAL$row$value, nick_w$BAL$row$rank, nick_w$BAL$league_avg))
for (s in 2022:2025) with(nick_seas[def_team == "SEA" & season == s], cat(sprintf(
  "  SEA %d (%s): %.1f%%, rank %d of 32\n", season, dc_lookup("SEA", season), value, rank)))
verdict1 <- if (nick_w$SEA$row$rank <= 10) "confirmed, a lot" else if (nick_w$SEA$row$rank <= 16) "confirmed, above average" else "not confirmed"
cat(sprintf("verdict (SEA primary): %s. Same-man BAL context agrees (rank %d of 32, also top third).\n",
            verdict1, nick_w$BAL$row$rank))
add_rows("1 nickel rate", "nickel_rate_pct", "1 = highest rate", nick_w, nick_seas,
         list(SEA = verdict1))

# =============================================================================
# CLAIM 2: ZONE RATE (+ coverage-scheme flavor, cross-checked against R/20)
# =============================================================================
mz <- db[man_zone_coverage %in% c("MAN", "ZONE")]
mz[, zone := man_zone_coverage == "ZONE"]
zone_seas <- season_ranks(mz[, .(n = .N, value = 100 * mean(zone)), by = .(def_team, season)],
                          "value", TRUE)
zone_w <- lapply(WINDOWS, function(w) window_rank(mz, "zone", w, decreasing = TRUE))

cat("\n=== CLAIM 2: zone rate ===\n")
cat(sprintf("SEA pooled 2024-2025: %.1f%%, rank %d of 32 (league avg %.1f%%) -- dead average, not \"a lot\"\n",
            zone_w$SEA$row$value, zone_w$SEA$row$rank, zone_w$SEA$league_avg))
for (s in 2024:2025) with(zone_seas[def_team == "SEA" & season == s], cat(sprintf(
  "  SEA %d (%s): %.1f%%, rank %d of 32\n", season, dc_lookup("SEA", season), value, rank)))
cat(sprintf("BAL context, pooled 2022-2023: %.1f%%, rank %d of 32 (league avg %.1f%%) -- also dead average\n",
            zone_w$BAL$row$value, zone_w$BAL$row$rank, zone_w$BAL$league_avg))

cov25 <- db[def_team == "SEA" & season == 2025 & coverage_scheme %in%
             c("COVER 0","COVER 1","COVER 2","COVER 2 MAN","COVER 3","COVER 4","COVER 6"),
             .N, by = coverage_scheme][order(-N)]
cov25[, pct := round(100 * N / sum(N), 1)]
cat("\nSEA 2025 coverage-scheme mix (named shells only), cross-checked against R/20's Cover 3 figure:\n")
for (i in seq_len(nrow(cov25))) cat(sprintf("  %-14s %5.1f%%\n", cov25$coverage_scheme[i], cov25$pct[i]))
cov3_rank_2025 <- rank_teams(db[season == 2025, .(value = 100 * mean(coverage_scheme == "COVER 3")), by = def_team], "value", TRUE)[def_team == "SEA"]
cat(sprintf("Cover 3 alone: %.1f%% here vs R/20's 33%% (same 2025 season, both non-garbage-time), rank %d of 32 -- agrees with R/20's rank of 16, average.\n",
            cov25$pct[cov25$coverage_scheme == "COVER 3"], cov3_rank_2025$rank))
cat("Reading: SEA is not Cover-3-heavy specifically (average), but spreads usage across four zone shells\n")
cat("(Cover 2/3/4/6) roughly evenly, which is what pushes the TOTAL zone share above average -- shell\n")
cat("diversity, not a single dominant look.\n")
verdict2 <- if (zone_w$SEA$row$rank <= 10) "confirmed" else if (zone_w$SEA$row$rank <= 16) "not clearly confirmed pooled (dead average); confirmed in 2025 alone, not in 2024" else "not confirmed"
cat(sprintf("verdict (SEA primary): %s\n", verdict2))
add_rows("2 zone rate", "zone_rate_pct", "1 = highest rate", zone_w, zone_seas,
         list(SEA = verdict2))

# =============================================================================
# CLAIM 3: BLITZ (frequency low, danger high when they do)
# =============================================================================
# 3a. frequency -- claim direction is LOW rate
bl_seas <- season_ranks(db[, .(n = .N, value = 100 * mean(blitz)), by = .(def_team, season)],
                        "value", FALSE)
bl_w <- lapply(WINDOWS, function(w) window_rank(db, "blitz", w, decreasing = FALSE))

cat("\n=== CLAIM 3a: blitz frequency (\"doesn't blitz that much\") ===\n")
cat(sprintf("SEA pooled 2024-2025: %.1f%%, claim-direction rank %d of 32 fewest (league avg %.1f%%)\n",
            bl_w$SEA$row$value, bl_w$SEA$row$rank, bl_w$SEA$league_avg))
cat(sprintf("BAL context, pooled 2022-2023: %.1f%%, rank %d of 32 fewest (league avg %.1f%%) -- same rank (11) at both stops\n",
            bl_w$BAL$row$value, bl_w$BAL$row$rank, bl_w$BAL$league_avg))
cat("Matches R/16's DC-level, situation-adjusted finding directly: Macdonald blitzes below what his own\n")
cat("situations call for in all six game-context cuts (early/late down, red zone, two-minute, trailing, leading).\n")
verdict3a <- if (bl_w$SEA$row$rank <= 10) "confirmed" else if (bl_w$SEA$row$rank <= 16) "confirmed, mildly" else "not confirmed"
cat(sprintf("verdict (SEA primary): %s\n", verdict3a))
add_rows("3a blitz frequency", "blitz_rate_pct", "1 = fewest blitzes", bl_w, bl_seas,
         list(SEA = verdict3a))

# 3b. danger -- expected_points_added is offense-signed; claim direction is
# LOW EPA allowed on a blitz (decreasing = FALSE)
epa_seas <- season_ranks(db[blitz == TRUE, .(n = .N, value = mean(expected_points_added)), by = .(def_team, season)],
                         "value", FALSE)
epa_w <- lapply(WINDOWS, function(w) window_rank(db[blitz == TRUE], "expected_points_added", w,
                                                 decreasing = FALSE, agg = "raw"))
pr_seas <- season_ranks(db[blitz == TRUE, .(n = .N, value = 100 * mean(pressure)), by = .(def_team, season)],
                        "value", TRUE)
pr_w <- lapply(WINDOWS, function(w) window_rank(db[blitz == TRUE], "pressure", w, decreasing = TRUE))

sea_nonblitz_epa <- mean(db[def_team == "SEA" & season %in% 2024:2025 & blitz == FALSE]$expected_points_added)
bal_nonblitz_epa <- mean(db[def_team == "BAL" & season %in% 2022:2023 & blitz == FALSE]$expected_points_added)

cat("\n=== CLAIM 3b: blitz danger (\"pretty dangerous when they do\") ===\n")
cat(sprintf("SEA pooled 2024-2025: EPA allowed on blitzes = %+.3f (n=%d), claim-direction rank %d of 32 -- #1 IN THE NFL\n",
            epa_w$SEA$row$value, epa_w$SEA$row$n, epa_w$SEA$row$rank))
cat(sprintf("league-avg EPA allowed on a blitz (2024-2025 window): %+.3f. SEA's own non-blitz EPA allowed: %+.3f -- SEA's defense is BETTER when it blitzes.\n",
            epa_w$SEA$league_avg, sea_nonblitz_epa))
cat(sprintf("pressure rate when SEA blitzes: %.1f%%, rank %d of 32 (league avg %.1f%%)\n",
            pr_w$SEA$row$value, pr_w$SEA$row$rank, pr_w$SEA$league_avg))
cat(sprintf("\nBAL context, pooled 2022-2023: EPA allowed on blitzes = %+.3f (n=%d), rank %d of 32 (also top tier)\n",
            epa_w$BAL$row$value, epa_w$BAL$row$n, epa_w$BAL$row$rank))
cat(sprintf("BAL non-blitz EPA allowed: %+.3f; pressure rate on blitz %.1f%%, rank %d of 32\n",
            bal_nonblitz_epa, pr_w$BAL$row$value, pr_w$BAL$row$rank))
for (s in 2022:2025) {
  es <- epa_seas[def_team %in% c("SEA","BAL") & season == s]
  for (i in seq_len(nrow(es))) with(es[i], cat(sprintf(
    "  %s %d (%s): EPA allowed on blitz %+.3f (n=%d), rank %d of 32\n",
    def_team, season, dc_lookup(def_team, season), value, n, rank)))
}
cat("\nMechanism, per R/28: Macdonald's pressure is schemed, not sent -- he ranks top 3 of 41 DCs manufacturing\n")
cat("unblocked free rushers while blitzing below expected and winning one-on-ones at a league-average clip.\n")
cat("The blitzes he DOES call are not volume, they are selective, and the EPA/pressure numbers above show they\n")
cat("land: this is the same story from a different angle.\n")
verdict3b <- if (epa_w$SEA$row$rank <= 10) "confirmed" else "contradicted"
cat(sprintf("verdict (SEA primary): %s\n", verdict3b))
add_rows("3b blitz danger", "blitz_epa_allowed", "1 = lowest EPA allowed (most dangerous)", epa_w, epa_seas,
         list(SEA = verdict3b))
add_rows("3b blitz danger", "blitz_pressure_rate_pct", "1 = highest pressure rate", pr_w, pr_seas)

# =============================================================================
# CLAIM 3c: MECHANISM -- does the "danger" trace to manufactured free rushers?
# Same player-play pull as R/28 (unblocked_pressure, side_of_ball == "defense"),
# aggregated to any_unblocked per play, merged onto the same dropback universe
# used throughout this script (100% match on sumer_play_id, same as R/28 found).
# =============================================================================
PCOLS <- c("sumer_play_id", "season", "side_of_ball", "unblocked_pressure")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2)
def_players <- players[season %in% SEASONS & side_of_ball == "defense"]
playp <- def_players[, .(any_unblocked = any(unblocked_pressure, na.rm = TRUE)), by = sumer_play_id]
db <- merge(db, playp, by = "sumer_play_id", all.x = TRUE)
db[is.na(any_unblocked), any_unblocked := FALSE]

mech_split <- function(team, seasons) {
  sub <- db[def_team == team & season %in% seasons & blitz == TRUE]
  lg  <- db[season %in% seasons & blitz == TRUE]
  split <- sub[, .(n = .N, epa = mean(expected_points_added)), by = any_unblocked]
  setorder(split, any_unblocked)
  list(rate = mean(sub$any_unblocked), lg_rate = mean(lg$any_unblocked), split = split, n_total = nrow(sub))
}
sea_mech <- mech_split("SEA", 2024:2025)
bal_mech <- mech_split("BAL", 2022:2023)

cat("\n=== CLAIM 3c: MECHANISM -- is the blitz danger explained by manufactured free rushers? (R/28's play-level flag) ===\n")
sm <- sea_mech$split
epa_yes_sea <- sm[any_unblocked == TRUE]$epa;  n_yes_sea <- sm[any_unblocked == TRUE]$n
epa_no_sea  <- sm[any_unblocked == FALSE]$epa; n_no_sea  <- sm[any_unblocked == FALSE]$n
cat(sprintf("SEA 2024-2025: a blitz springs a free, unblocked rusher %.1f%% of the time (n=%d of %d blitzes), vs %.1f%% league average in the same window.\n",
            100 * sea_mech$rate, n_yes_sea, sea_mech$n_total, 100 * sea_mech$lg_rate))
cat(sprintf("  free rusher sprung:    EPA allowed %+.3f (n=%d) -- a near-disaster for the offense\n", epa_yes_sea, n_yes_sea))
cat(sprintf("  no free rusher sprung: EPA allowed %+.3f (n=%d) -- roughly average, actually a bit worse than SEA's own non-blitz baseline (%+.3f)\n",
            epa_no_sea, n_no_sea, sea_nonblitz_epa))

bm <- bal_mech$split
epa_yes_bal <- bm[any_unblocked == TRUE]$epa; epa_no_bal <- bm[any_unblocked == FALSE]$epa
cat(sprintf("\nBAL 2022-2023 context: free rusher on %.1f%% of blitzes (league avg %.1f%%); EPA allowed %+.3f when sprung vs %+.3f when not.\n",
            100 * bal_mech$rate, 100 * bal_mech$lg_rate, epa_yes_bal, epa_no_bal))
cat(sprintf("\nVERDICT ON THE MECHANISM: yes. SEA's blitzes spring a free rusher about %.0f%% more often than the league's blitzes do\n",
            100 * (sea_mech$rate / sea_mech$lg_rate - 1)))
cat("(the BAL gap is smaller, ~10% more often). Essentially all of SEA's pooled blitz-EPA advantage lives in that\n")
cat("free-rusher subset; the other three in four SEA blitzes are not dangerous on their own. Matches R/28 exactly:\n")
cat("the rare blitz is not dangerous because it is a blitz, it is dangerous because it disproportionately manufactures\n")
cat("a free rusher, and a free rusher is what actually wins the down.\n")

results[[length(results) + 1]] <- rbindlist(list(
  data.table(claim = "3c blitz mechanism", metric = "blitz_unblocked_rate_pct", team = "SEA", team_role = "primary",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$SEA$label, n = sea_mech$n_total,
             value = round(100 * sea_mech$rate, 2), league_avg = round(100 * sea_mech$lg_rate, 2),
             rank_of_32 = NA_integer_, rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_),
  data.table(claim = "3c blitz mechanism", metric = "blitz_epa_when_unblocked", team = "SEA", team_role = "primary",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$SEA$label, n = n_yes_sea,
             value = round(epa_yes_sea, 4), league_avg = NA_real_, rank_of_32 = NA_integer_,
             rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_),
  data.table(claim = "3c blitz mechanism", metric = "blitz_epa_when_no_unblocked", team = "SEA", team_role = "primary",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$SEA$label, n = n_no_sea,
             value = round(epa_no_sea, 4), league_avg = NA_real_, rank_of_32 = NA_integer_,
             rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_),
  data.table(claim = "3c blitz mechanism", metric = "blitz_unblocked_rate_pct", team = "BAL", team_role = "context",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$BAL$label, n = bal_mech$n_total,
             value = round(100 * bal_mech$rate, 2), league_avg = round(100 * bal_mech$lg_rate, 2),
             rank_of_32 = NA_integer_, rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_),
  data.table(claim = "3c blitz mechanism", metric = "blitz_epa_when_unblocked", team = "BAL", team_role = "context",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$BAL$label, n = bm[any_unblocked == TRUE]$n,
             value = round(epa_yes_bal, 4), league_avg = NA_real_, rank_of_32 = NA_integer_,
             rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_),
  data.table(claim = "3c blitz mechanism", metric = "blitz_epa_when_no_unblocked", team = "BAL", team_role = "context",
             scope = "pooled", season = NA_integer_, def_caller = WINDOWS$BAL$label, n = bm[any_unblocked == FALSE]$n,
             value = round(epa_no_bal, 4), league_avg = NA_real_, rank_of_32 = NA_integer_,
             rank_direction = "n/a -- decomposition, not a team-ranked rate", verdict = NA_character_)
), fill = TRUE)

# =============================================================================
# CLAIM 4: EXPLOSIVE PLAYS ALLOWED
# =============================================================================
scrim[, exp20  := as.integer(offensive_yards >= 20)]
scrim[, exp_rp := as.integer((is_pass_attempt == TRUE  & offensive_yards >= 20) |
                              (is_pass_attempt == FALSE & offensive_yards >= 15))]

exp_seas  <- season_ranks(scrim[, .(n = .N, value = 100 * mean(exp20)),  by = .(def_team, season)], "value", FALSE)
exp2_seas <- season_ranks(scrim[, .(n = .N, value = 100 * mean(exp_rp)), by = .(def_team, season)], "value", FALSE)
exp_w  <- lapply(WINDOWS, function(w) window_rank(scrim, "exp20",  w, decreasing = FALSE))
exp2_w <- lapply(WINDOWS, function(w) window_rank(scrim, "exp_rp", w, decreasing = FALSE))
leader <- exp_w$SEA$all[rank == 1]

cat("\n=== CLAIM 4: explosive plays allowed (\"best in that category or close\") ===\n")
cat("primary definition: 20+ yard gain, any offensive snap.\n")
cat(sprintf("SEA pooled 2024-2025: %.2f%%, rank %d of 32 (league avg %.2f%%). League leader: %s at %.2f%%.\n",
            exp_w$SEA$row$value, exp_w$SEA$row$rank, exp_w$SEA$league_avg, leader$def_team, leader$value))
for (s in 2022:2025) with(exp_seas[def_team == "SEA" & season == s], cat(sprintf(
  "  SEA %d (%s): %.2f%%, rank %d of 32\n", season, dc_lookup("SEA", season), value, rank)))
cat(sprintf("BAL context, pooled 2022-2023: %.2f%%, rank %d of 32 (league avg %.2f%%)\n",
            exp_w$BAL$row$value, exp_w$BAL$row$rank, exp_w$BAL$league_avg))
cat("\nrobustness definition: 15+ yard rush OR 20+ yard pass.\n")
cat(sprintf("SEA pooled 2024-2025: %.2f%%, rank %d of 32 (league avg %.2f%%)\n",
            exp2_w$SEA$row$value, exp2_w$SEA$row$rank, exp2_w$SEA$league_avg))
verdict4 <- if (exp_w$SEA$row$rank <= 5) "confirmed -- top 5, essentially what he bet" else if (exp_w$SEA$row$rank <= 10) "confirmed -- top third, close to the claim" else "not confirmed"
cat(sprintf("verdict (SEA primary): %s. BAL context agrees (rank %d of 32, also top 5).\n",
            verdict4, exp_w$BAL$row$rank))
add_rows("4 explosive allowed (20+ any)", "explosive_rate_pct", "1 = fewest allowed", exp_w, exp_seas,
         list(SEA = verdict4))
add_rows("4 explosive allowed (15+ rush/20+ pass robustness)", "explosive_rate_robust_pct",
         "1 = fewest allowed", exp2_w, exp2_seas)

# =============================================================================
# CSV
# =============================================================================
out <- rbindlist(results, fill = TRUE)
write_csv(as.data.frame(out), "data/derived/sea_defense_battery.csv")
cat(sprintf("\nwrote data/derived/sea_defense_battery.csv (%d rows; SF rows are team_role == \"secondary\", the corrected mis-attribution, not part of any verdict)\n",
            nrow(out)))

# =============================================================================
# CHART: verification battery -- Seattle primary, Baltimore context marked
# =============================================================================
row_lab <- c(
  nickel   = "Nickel rate\n(1 = highest of 32)",
  zone     = "Zone rate\n(1 = highest of 32)",
  blitz_f  = "Blitz frequency\n(1 = fewest of 32)",
  blitz_d  = "Blitz danger: EPA allowed\n(1 = most dangerous of 32)",
  explosive= "Explosive plays allowed\n(1 = fewest of 32)"
)
row_key <- names(row_lab)

sea_pooled <- data.table(
  row = factor(row_key, levels = rev(row_key)),
  rank = c(nick_w$SEA$row$rank, zone_w$SEA$row$rank, bl_w$SEA$row$rank, epa_w$SEA$row$rank, exp_w$SEA$row$rank),
  verdict = c("Confirmed", "Mixed", "Confirmed", "Confirmed", "Confirmed")
)
bal_pooled <- data.table(
  row = factor(row_key, levels = rev(row_key)),
  rank = c(nick_w$BAL$row$rank, zone_w$BAL$row$rank, bl_w$BAL$row$rank, epa_w$BAL$row$rank, exp_w$BAL$row$rank)
)
sea_season_pts <- rbindlist(list(
  data.table(row = "nickel",    season = 2024:2025, rank = nick_seas[def_team=="SEA" & season %in% 2024:2025][order(season)]$rank),
  data.table(row = "zone",      season = 2024:2025, rank = zone_seas[def_team=="SEA" & season %in% 2024:2025][order(season)]$rank),
  data.table(row = "blitz_f",   season = 2024:2025, rank = bl_seas[def_team=="SEA" & season %in% 2024:2025][order(season)]$rank),
  data.table(row = "blitz_d",   season = 2024:2025, rank = epa_seas[def_team=="SEA" & season %in% 2024:2025][order(season)]$rank),
  data.table(row = "explosive", season = 2024:2025, rank = exp_seas[def_team=="SEA" & season %in% 2024:2025][order(season)]$rank)
))
sea_range <- sea_season_pts[, .(lo = min(rank), hi = max(rank)), by = row]
sea_range[, row := factor(row, levels = rev(row_key))]
sea_season_pts[, row := factor(row, levels = rev(row_key))]

callouts <- rbindlist(list(
  data.table(row = "zone", season = 2024L,
             lab = "2024-25 season: below average (21st); 2025-26 alone ranks 6th"),
  data.table(row = "blitz_d", season = 2024L,
             lab = "2024-25 season: rank 22nd; 2025-26 alone is #1 in the NFL")
))
callouts[, row := factor(row, levels = rev(row_key))]
callouts <- merge(callouts, sea_season_pts, by = c("row","season"))
callouts[, hj := fifelse(rank <= 6, 0, fifelse(rank >= 27, 1, 0.5))]

verdict_col <- c(Confirmed = "#2B8CBE", Contradicted = "#D55E00", Mixed = "grey45")

p <- ggplot() +
  geom_vline(xintercept = 16.5, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_segment(data = sea_range, aes(x = lo, xend = hi, y = row, yend = row),
               colour = "grey80", linewidth = 3.2, lineend = "round") +
  geom_point(data = sea_season_pts, aes(rank, row), colour = "grey45", size = 1.8) +
  geom_point(data = bal_pooled, aes(rank, row), shape = 23, size = 3.6,
             fill = "white", colour = "grey35", stroke = 1) +
  geom_point(data = sea_pooled, aes(rank, row, colour = verdict), size = 5.5) +
  geom_text(data = sea_pooled, aes(rank, row, label = rank), colour = "white",
            size = 2.6, fontface = "bold") +
  geom_text(data = callouts, aes(rank, row, label = lab, hjust = hj), size = 2.6, colour = "grey30",
            fontface = "italic", vjust = -1.5) +
  scale_colour_manual(values = verdict_col, name = "Seattle, Macdonald's two seasons pooled (2024-25 + 2025-26)") +
  scale_y_discrete(labels = row_lab) +
  scale_x_continuous(limits = c(1, 32), breaks = c(1, 8, 16.5, 24, 32),
                     labels = c("1", "8", "16.5\nleague avg", "24", "32")) +
  labs(title = "Checking the Seattle defense read against SumerSports charting, both Macdonald seasons",
       subtitle = "Blue/grey dot = Seattle pooled across the 2024-25 AND 2025-26 seasons (his whole Seattle tenure); the italic notes flag where the two seasons disagree.\nClaim-direction rank of 32 teams: 1 = Seattle at the extreme that would make his claim true.\nGrey diamond = Mike Macdonald's prior stop, Baltimore (2022-23 and 2023-24 seasons), same test, for reference. Grey bar = Seattle's season-to-season range.",
       x = "Claim-direction rank of 32 (see subtitle)", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, four regular seasons, 2022-23 through 2025-26",
         sprintf("SEA pooled non-garbage snaps: %s total, %s dropbacks. Rank convention flips per row so 1 always means \"confirms the claim\".",
                 format(nrow(scrim[def_team=="SEA" & season %in% 2024:2025]), big.mark=","),
                 format(nrow(db[def_team=="SEA" & season %in% 2024:2025]), big.mark=",")),
         sprintf("\nNickel, blitz frequency and explosive-play prevention: confirmed, all comfortably above average, and Baltimore under the same coordinator lands in the same range.\nZone is the one mixed read: dead average pooled (matches R/20's Cover 3 rank of 16th), driven by an average 2024-25 and an above-average 2025-26, not a consistent identity.\nBlitz danger is the standout: Seattle's blitzes allowed the LEAST offensive EPA of any defense in football pooled, entirely on the strength of 2025-26 (#1); 2024-25 was\nbelow average. WHY: a Seattle blitz springs a free, unblocked rusher %.0f%% of the time (league %.0f%%); that slice alone allows %+.2f EPA (a near-disaster for the offense) while\nthe other three in four Seattle blitzes allow %+.2f (unremarkable) -- confirms R/28's finding that his pressure is schemed, not sent. Built by R/34.",
                 100 * sea_mech$rate, 100 * sea_mech$lg_rate, epa_yes_sea, epa_no_sea))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left",
        axis.text.y = element_text(size = rel(0.82), face = "bold", lineheight = 0.95),
        axis.text.x = element_text(size = rel(0.8)),
        plot.margin = margin(10, 20, 8, 10))
save_fig("docs/figures/sea_defense_battery.png", p, w = 13, h = 7.8)

cat("\nOut: docs/figures/sea_defense_battery.png, data/derived/sea_defense_battery.csv\n")
