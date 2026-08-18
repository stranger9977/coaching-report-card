# =============================================================================
# factory/lib_sumer.R -- shared plumbing for the Sumer charting data.
#
# Every Sumer analysis needs the same three things, and every one of them is
# easy to get subtly wrong, so they live here once:
#
#   load_sumer()    read the full pull, map Sumer team ids to nflverse codes,
#                   attach the offensive and defensive play-caller by team-week
#   sumer_resid()   a caller's rate on some behaviour, measured against a
#                   situation-only model of that behaviour rather than against
#                   the raw league average
#   persist_split() the odd-against-even season test that decides whether a
#                   number is a trait or a description
#
# THE RULE THIS ENFORCES. Defenses and offenses behave completely differently by
# down, distance, score and clock, so a raw per-caller rate mostly ranks people
# by the situations they happened to face. Everything published from this data
# goes through sumer_resid() and then has to survive persist_split().
#
# JOIN NOTES. Sumer's gsis_game_id is the GSIS game key, which is NOT the same
# id as nflverse's old_game_id, and gsis_play_id is not nflverse's play_id
# either. So nothing here joins to nflverse at play level. It does not need to:
# Sumer carries its own situation fields and its own EPA, and play-caller
# attribution only needs season, week and team.
# =============================================================================

suppressMessages({
  library(data.table); library(jsonlite); library(xgboost)
})

SUMER_DIR <- "data/raw/sumer"
NFLA_PC   <- "/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv"

# Sumer club codes that nflverse spells differently
SUMER_TEAM_FIX <- c(ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU")

load_sumer <- function(seasons = 2022:2025, file = file.path(SUMER_DIR, "plays_full.csv")) {
  plays <- fread(file, showProgress = FALSE)
  teams <- as.data.table(fromJSON(file.path(SUMER_DIR, "teams.json")))
  teams[, abbr := fifelse(team_code %in% names(SUMER_TEAM_FIX),
                          SUMER_TEAM_FIX[team_code], team_code)]

  games <- rbindlist(lapply(seasons, function(y) {
    f <- file.path(SUMER_DIR, sprintf("games_%d.csv", y))
    if (file.exists(f)) fread(f, showProgress = FALSE) else NULL
  }), fill = TRUE)

  plays <- merge(plays, teams[, .(sumer_offense_team_id = sumer_team_id, off_team = abbr)],
                 by = "sumer_offense_team_id", all.x = TRUE)
  plays <- merge(plays, teams[, .(sumer_defense_team_id = sumer_team_id, def_team = abbr)],
                 by = "sumer_defense_team_id", all.x = TRUE)
  plays <- merge(plays, unique(games[, .(sumer_game_id, week, season_type)]),
                 by = "sumer_game_id", all.x = TRUE)

  pc <- fread(NFLA_PC, showProgress = FALSE)
  plays <- merge(plays, pc[, .(season, week, off_team = team, off_caller = off_play_caller)],
                 by = c("season","week","off_team"), all.x = TRUE)
  plays <- merge(plays, pc[, .(season, week, def_team = team, def_caller = def_play_caller)],
                 by = c("season","week","def_team"), all.x = TRUE)

  plays <- plays[season %in% seasons]
  # kneels, spikes and wiped plays are not calls
  plays <- plays[no_play == FALSE]
  plays[, off_caller := fifelse(is.na(off_caller), "", off_caller)]
  plays[, def_caller := fifelse(is.na(def_caller), "", def_caller)]
  plays[]
}

# The situation features every model in here uses. Sumer's own names.
SUMER_STATE <- c("down","distance","field_position","quarter","offense_score_diff",
                 "two_minute","four_minute","redzone","short_yardage","goal_line")

.sumer_prep <- function(x, feats) {
  x <- copy(x)
  for (f in feats) if (is.logical(x[[f]])) set(x, j = f, value = as.integer(x[[f]]))
  x[, (feats) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = feats]
  x
}

# Season-grouped situation-only model of `target`, returning the play-level
# expectation. Nothing about who is coaching goes in, so what is left over is
# the coach's choice.
sumer_expect <- function(x, target, feats = SUMER_STATE, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(get(target))], feats)
  x <- x[complete.cases(x[, ..feats])]
  if (!nrow(x)) return(x)
  y <- as.numeric(x[[target]])
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "binary:logistic", eta = 0.06, max_depth = 5,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.y = y, .expected = p)]
  x[!is.na(.expected)]
}

# Per caller-season and per career: actual rate, expected rate, residual.
# `who` is "off_caller" or "def_caller".
sumer_resid <- function(x, target, who = "off_caller", feats = SUMER_STATE,
                        min_season = 200, min_career = 700) {
  z <- sumer_expect(x[get(who) != ""], target, feats)
  if (!nrow(z)) return(NULL)
  season <- z[, .(n = .N, actual = 100*mean(.y), expected = 100*mean(.expected),
                  epa = mean(expected_points_added, na.rm = TRUE)),
              by = .(caller = get(who), season)][n >= min_season]
  season[, resid := actual - expected]
  career <- season[, .(seasons = .N, n = sum(n),
                       actual = weighted.mean(actual, n),
                       resid  = weighted.mean(resid, n),
                       epa    = weighted.mean(epa, n)), by = caller][n >= min_career]
  list(season = season, career = career[order(-resid)], plays = nrow(z),
       league_rate = 100*mean(z$.y))
}

# The test that decides whether anything gets published. Splits a caller's
# seasons odd against even, rebuilds the residual inside each half, correlates.
persist_split <- function(season_tbl, min_half = 350) {
  a <- season_tbl[season %% 2 == 1, .(n = sum(n), r = weighted.mean(resid, n)), by = caller][n >= min_half]
  b <- season_tbl[season %% 2 == 0, .(n = sum(n), r = weighted.mean(resid, n)), by = caller][n >= min_half]
  m <- merge(a, b, by = "caller", suffixes = c("_odd","_even"))
  if (nrow(m) < 8) return(list(n = nrow(m), r = NA_real_, lo = NA_real_, hi = NA_real_,
                               p = NA_real_, verdict = "too few callers", m = m))
  ct <- cor.test(m$r_odd, m$r_even)
  list(n = nrow(m), r = unname(ct$estimate), lo = ct$conf.int[1], hi = ct$conf.int[2],
       p = ct$p.value,
       verdict = if (ct$conf.int[1] > 0.30) "strong trait"
                 else if (ct$conf.int[1] > 0.10) "trait"
                 else if (ct$conf.int[2] < 0.10) "not a trait"
                 else "unresolved",
       m = m)
}

# How much of an observed spread is real rather than sampling noise.
signal_share <- function(stat, se) {
  tau2 <- var(stat) - mean(se^2)
  list(sd_obs = sd(stat), sd_noise = sqrt(mean(se^2)),
       share = if (tau2 > 0) tau2/var(stat) else 0)
}
