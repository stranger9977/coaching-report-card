# =============================================================================
# 81_card_lines_all_coaches.R -- the report card's lines, recomputed with a
# one-season minimum so first-year head coaches are graded on the season they
# actually coached.
#
# The problem this fixes: five of the 2026 head coaches showed Inc on nearly
# every line, not because the data is missing but because the career files the
# card was reading are built for careers. decision_value.csv needs enough
# fourth downs to shrink a career, discipline.csv keeps only coaches with two
# or more seasons as a head coach, talent_adjusted.csv needs 3,000 plays, and
# the two-point chart line needs 10 chart situations. A coach with one season
# fails all four while his season sits in the raw data.
#
# So each line is rebuilt here from source, per coach-season, then averaged
# over whatever seasons a coach has:
#
#   Fourth downs   nfl4th decision model in data/derived/fourth_down_probs.rds
#                  (2018-2025). Cost of a choice = the best of go, field goal
#                  and punt in win probability minus what he chose, in
#                  competitive game states, per game. Same definition as
#                  R/factory/95, without the career minimum.
#   Penalties      offensive pre-snap and holding penalties per play from
#                  nflverse play-by-play, team-season, against the league that
#                  season. Same idea as R/39, without the two-season minimum.
#   Offense        team offense EPA per play, residual after the same talent
#   Defense        controls the WAR model uses (payroll, quarterback pay, his
#                  play last season, Madden from 2017), per season.
#   Two-point      chart-following rate where the coach has 10 or more chart
#                  situations; otherwise his overall two-point rate against
#                  his era, which every coach has.
#
# Thin seasons stay thin: the count of seasons behind every line is written to
# the CSV so the page can say so.
#
# Out: data/derived/card_lines.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(lme4) })
NFLA <- "/Users/nick/stranger9977/nfl-analysis/data"
YRS <- 2012:2025

pc <- fread(file.path(NFLA, "playcallers.csv"), select = c("season", "week", "team", "head_coach"))
pc[, head_coach := trimws(head_coach)]
pc2 <- fread(file.path(NFLA, "playcallers.csv"), select = c("season", "week", "team", "head_coach", "off_play_caller", "def_play_caller"))
pc2[, `:=`(head_coach = trimws(head_coach), off_play_caller = trimws(off_play_caller), def_play_caller = trimws(def_play_caller))]
pc2 <- pc2[season %in% YRS]
hc <- unique(pc[season %in% YRS, .(season, team, coach = head_coach)])           # coach of record per team-season
hc <- hc[, .SD[1], by = .(season, team)]
ACTIVE <- unique(pc[season == 2026 & week == 1]$head_coach)

# ---------------------------------------------------------------- 1. fourth downs
fd <- setDT(readRDS("data/derived/fourth_down_probs.rds"))
fd[, went := as.integer(play_type %in% c("run", "pass") | rush_attempt == 1 | pass_attempt == 1)]
fd[, kicked := as.integer(play_type %in% c("punt", "field_goal") | punt_attempt == 1 | field_goal_attempt == 1)]
d4 <- fd[!is.na(go_boost) & (went == 1 | kicked == 1) & season_type == "REG" &
         vegas_wp > 0.05 & vegas_wp < 0.95 & qtr <= 4]
d4[, chosen_wp := fifelse(went == 1, go_wp, fifelse(play_type == "field_goal" | field_goal_attempt == 1, fg_wp, punt_wp))]
d4[, best_wp := pmax(go_wp, fg_wp, punt_wp, na.rm = TRUE)]
d4[, cost := pmax(best_wp - chosen_wp, 0)]
d4 <- merge(d4[!is.na(cost), .(season, team = posteam, game_id, cost)], hc, by = c("season", "team"))
g4 <- d4[, .(cost = sum(cost), games = uniqueN(game_id)), by = .(coach, season)]
g4[, cost_pg := cost / games]
lg4 <- g4[, .(lg = weighted.mean(cost_pg, games)), by = season]
g4 <- merge(g4, lg4, by = "season")
fourth <- g4[, .(fourth = -weighted.mean(cost_pg - lg, games), n_fourth = .N), by = coach]  # lower cost is better
cat(sprintf("fourth downs: %d coaches, %d of the 2026 field\n", nrow(fourth), sum(fourth$coach %in% ACTIVE)))

# ---------------------------------------------------------------- 2. penalties
pen <- rbindlist(lapply(YRS, function(y)
  fread(file.path(NFLA, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("season", "posteam", "penalty", "penalty_team", "penalty_type", "play_type", "season_type"),
        showProgress = FALSE)))
pen <- pen[season_type == "REG" & !is.na(posteam) & play_type %in% c("pass", "run", "punt", "field_goal", "no_play")]
PRE <- c("False Start", "Delay of Game", "Illegal Formation", "Illegal Shift", "Illegal Motion",
         "Illegal Substitution", "Offensive Holding", "Illegal Use of Hands", "Unnecessary Roughness",
         "Unsportsmanlike Conduct", "Neutral Zone Infraction", "Encroachment", "Offside on Free Kick", "Defensive Offside")
pen[, own_pen := as.integer(penalty == 1 & penalty_team == posteam & penalty_type %in% PRE)]
tp <- pen[, .(plays = .N, pens = sum(own_pen, na.rm = TRUE)), by = .(season, team = posteam)]
tp[, rate := pens / plays]
tp <- merge(tp, hc, by = c("season", "team"))
tp <- merge(tp, tp[, .(lg = weighted.mean(rate, plays)), by = season], by = "season")
penalties <- tp[, .(penalties = -weighted.mean(rate - lg, plays), n_pen = .N), by = coach]   # fewer is better
cat(sprintf("penalties: %d coaches, %d of the 2026 field\n", nrow(penalties), sum(penalties$coach %in% ACTIVE)))

# ---------------------------------------------------------------- 3. offense and defense above talent
ep <- rbindlist(lapply(YRS, function(y)
  fread(file.path(NFLA, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("season", "posteam", "defteam", "epa", "play_type", "season_type"), showProgress = FALSE)))
ep <- ep[season_type == "REG" & play_type %in% c("pass", "run") & !is.na(epa)]
o <- ep[, .(off_plays = .N, off_epa = mean(epa)), by = .(season, team = posteam)]
dd <- ep[, .(def_plays = .N, def_epa = -mean(epa)), by = .(season, team = defteam)]
ts <- merge(o, dd, by = c("season", "team"))
pan <- fread("data/derived/coaching_war_seasons.csv")[, .(season, team, contract_z, qb_z, qbp_z, madden_z, games,
                                                            qb_na, qbp_na)]
ts <- merge(ts, unique(pan, by = c("season", "team")), by = c("season", "team"))
ts[, madden_z17 := fifelse(is.na(madden_z), 0, madden_z)][, post17 := as.integer(season >= 2017)]
ts[, season_f := factor(season)]
ts <- ts[!is.na(contract_z) & !is.na(qb_z) & !is.na(qbp_z)]
# qbp_na marks a starter with no charted play from the season before: a rookie
# or a first-year starter. Without it the fit expects last season's production
# from a quarterback who has none, and charges the coach for the gap.
ts[, `:=`(new_qb = as.integer(qbp_na %in% c(TRUE, "TRUE")), qb_unk = as.integer(qb_na %in% c(TRUE, "TRUE")))]
cat(sprintf("team-seasons with a first-year starter: %d of %d\n", sum(ts$new_qb), nrow(ts)))
fo <- lm(off_epa ~ contract_z + qb_z + qbp_z + new_qb + qb_unk + madden_z17:post17 + season_f, ts, weights = off_plays)
fdf <- lm(def_epa ~ contract_z + qb_z + qbp_z + new_qb + qb_unk + madden_z17:post17 + season_f, ts, weights = def_plays)
cat(sprintf("first-year-starter effect on offense: %+.4f EPA per play (t = %.1f)\n",
            coef(fo)["new_qb"], summary(fo)$coefficients["new_qb", 3]))
ts[, `:=`(off_adj = off_epa - fitted(fo), def_adj = def_epa - fitted(fdf))]
ts <- merge(ts, hc, by = c("season", "team"))
od <- ts[, .(offense = weighted.mean(off_adj, off_plays), defense = weighted.mean(def_adj, def_plays), n_od = .N), by = coach]
cat(sprintf("offense/defense above talent: %d coaches, %d of the 2026 field\n", nrow(od), sum(od$coach %in% ACTIVE)))

# ------------------------------------------- 3b. play-calling above talent
# the same residuals, attributed to whoever called the plays rather than to the
# head coach, so a coordinator's record is measured the way a head coach's is
pcall <- unique(pc2[, .(season, team, off_play_caller, def_play_caller, head_coach)])
tsc <- merge(ts[, .(season, team, off_adj, def_adj, off_plays, def_plays)], pcall, by = c("season", "team"))
co <- tsc[off_play_caller != "", .(caller = off_play_caller, season, adj = off_adj, plays = off_plays,
                                   side = "offense", as_hc = as.integer(off_play_caller == head_coach))]
cd <- tsc[def_play_caller != "", .(caller = def_play_caller, season, adj = def_adj, plays = def_plays,
                                   side = "defense", as_hc = as.integer(def_play_caller == head_coach))]
cal <- rbind(co, cd)
caller_adj <- cal[, .(caller_adj = weighted.mean(adj, plays), plays = sum(plays), seasons = .N,
                      seasons_as_coord = sum(as_hc == 0)), by = .(caller, side)][plays >= 1000]
caller_adj <- caller_adj[order(-plays), .SD[1], by = caller]      # his main side of the ball
write_csv(as.data.frame(caller_adj), "data/derived/caller_above_talent.csv")
cat(sprintf("play-calling above talent: %d callers with 1,000+ plays, %d of the 2026 field\n",
            nrow(caller_adj), sum(caller_adj$caller %in% ACTIVE)))

# ---------------------------------------------------------------- 4. two point
tw <- fread("data/derived/two_point.csv")
two <- tw[, .(coach, two_pt = fifelse(!is.na(chart_n) & chart_n >= 10, chart_era_shrunk, overall_era_shrunk),
              two_basis = fifelse(!is.na(chart_n) & chart_n >= 10, "chart situations", "overall rate vs era"))]
cat(sprintf("two point: %d coaches, %d of the 2026 field (%d on the chart definition)\n",
            nrow(two), sum(two$coach %in% ACTIVE), two[coach %in% ACTIVE & two_basis == "chart situations", .N]))

# per-season two-point rate: attempts per touchdown, against the league that season
tw2 <- rbindlist(lapply(YRS, function(y)
  fread(file.path(NFLA, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("season", "posteam", "two_point_attempt", "touchdown", "td_team", "season_type"), showProgress = FALSE)))
tw2 <- tw2[season_type == "REG" & !is.na(posteam)]
t2 <- tw2[, .(att = sum(two_point_attempt, na.rm = TRUE), tds = sum(touchdown == 1 & td_team == posteam, na.rm = TRUE)),
          by = .(season, team = posteam)]
t2[, rate2 := att / pmax(tds, 1)]
t2 <- merge(t2, hc, by = c("season", "team"))
t2 <- merge(t2, t2[, .(lg2 = weighted.mean(rate2, tds)), by = season], by = "season")
t2[, two_s := rate2 - lg2]

# ---------------------------------------------------------------- per-season file
# the same lines at coach-season level, for the out-of-sample test in R/82
ps4 <- g4[, .(coach, season, fourth_s = -(cost_pg - lg))]
psp <- tp[, .(coach, season, penalties_s = -(rate - lg), plays_pen = plays)]
pso <- ts[, .(coach, season, offense_s = off_adj, defense_s = def_adj, off_plays, def_plays)]
psw <- fread("data/derived/coaching_war_seasons.csv")[in_main_window == TRUE,
        .(coach, season, wat17, act17, wae17, games)]
ps <- Reduce(function(a, b) merge(a, b, by = c("coach", "season"), all = TRUE),
             list(psw, ps4, psp, pso, t2[, .(coach, season, two_s, tds)]))
ps <- ps[!is.na(act17)]
write_csv(as.data.frame(ps), "data/derived/card_lines_seasons.csv")
cat(sprintf("\nper-season file: %d coach-seasons, %d coaches, seasons %d-%d\n",
            nrow(ps), uniqueN(ps$coach), min(ps$season), max(ps$season)))

out <- Reduce(function(a, b) merge(a, b, by = "coach", all = TRUE), list(fourth, penalties, od, two))
write_csv(as.data.frame(out), "data/derived/card_lines.csv")
cat("\ncoverage for the 2026 field:\n")
print(out[coach %in% ACTIVE, .(coach, fourth = round(fourth, 4), n_fourth, penalties = round(penalties, 5), n_pen,
                               offense = round(offense, 4), defense = round(defense, 4), n_od,
                               two_pt = round(two_pt, 2), two_basis)][order(coach)])
cat("\nOut: data/derived/card_lines.csv\n")
