# =============================================================================
# 83_recency_and_job_change.R -- stale credit and the coaches their teams
# moved on from.
#
# The ask: "harbaugh and other coaches joining a new team are kind of getting
# credit from their old record so we need to handle that gracefully like they
# should be penalized because their team moved on from them".
#
# Two separate problems, handled separately because they need different
# evidence.
#
# 1. STALE CREDIT. Every line on the card is a career average, so John
#    Harbaugh's grade is mostly Baltimore between 2012 and 2024 and Mike
#    McCarthy's is mostly Green Bay. This weights each season by how recent it
#    is, with a half-life set by the data rather than by taste: the half-life
#    that maximises the correlation between a coach's weighted history and his
#    next season is picked from a grid, and the whole grid is printed so the
#    choice can be argued with.
#
# 2. THE TEAM MOVED ON. Whether that carries information beyond the record is
#    testable: take every coach who got a second job, split by how the first
#    one ended, and compare what he did in the new job. Firing is not the same
#    as leaving, and neither is the same as retiring into a job elsewhere.
#
# Out: data/derived/recency_halflife.csv, data/derived/job_change.csv,
#      docs/figures/job_change.png
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")
NFLA <- "/Users/nick/stranger9977/nfl-analysis/data"

ps <- fread("data/derived/card_lines_seasons.csv")
setorder(ps, coach, season)

# ---------------------------------------------------------------- 1. half-life
# for a grid of half-lives, build each coach's weighted history through t-1 and
# correlate it with what he actually did in season t
grid <- c(1, 1.5, 2, 3, 4, 6, 8, 100)
score <- rbindlist(lapply(grid, function(h) {
  x <- copy(ps)
  x[, prior_wat := {
    v <- wat17; out <- rep(NA_real_, .N)
    for (i in seq_len(.N)) if (i > 1) {
      w <- 0.5 ^ ((season[i] - season[1:(i - 1)]) / h)
      ok <- !is.na(v[1:(i - 1)])
      if (any(ok)) out[i] <- sum(v[1:(i - 1)][ok] * w[ok]) / sum(w[ok])
    }
    out
  }, by = coach]
  d <- x[!is.na(prior_wat) & !is.na(wat17)]
  data.table(half_life = h, r = cor(d$prior_wat, d$wat17), n = nrow(d))
}))
cat("how much a coach's past predicts his next season, by how fast the past is discounted:\n")
print(score[, .(half_life, r = round(r, 4), n)])
best <- score[which.max(r)]$half_life
cat(sprintf("best half-life: %.1f seasons (a season five years back counts %.0f%% of last season)\n",
            best, 100 * 0.5 ^ (5 / best)))
write_csv(as.data.frame(score), "data/derived/recency_halflife.csv")

# ---------------------------------------------------------------- 2. the team moved on
pc <- fread(file.path(NFLA, "playcallers.csv"), select = c("season", "week", "team", "head_coach"))
pc[, head_coach := trimws(head_coach)]
hc <- unique(pc[, .(season, team, coach = head_coach)])[, .SD[1], by = .(season, team)]
setorder(hc, coach, season)
hc[, spell := cumsum(c(1, diff(season) != 1 | head(team, -1) != tail(team, -1))), by = coach]
sp <- hc[, .(team = team[1], first = min(season), last = max(season), seasons = .N), by = .(coach, spell)]
setorder(sp, coach, first)
sp[, next_first := shift(first, -1), by = coach]
sp[, next_team := shift(team, -1), by = coach]
# the team's own next coach: if the club kept coaching without him, it moved on
nxt <- merge(sp[, .(coach, spell, team, last)], hc[, .(team, season, coach_after = coach)],
             by.x = c("team", "last"), by.y = c("team", "season"), all.x = TRUE)
after <- hc[, .(team, season, coach_after = coach)]
sp <- merge(sp, after, by.x = c("team", "last"), by.y = c("team", "season"), all.x = TRUE)
sp2 <- merge(sp, after[, .(team, season2 = season, replacement = coach_after)],
             by.x = c("team"), by.y = c("team"), allow.cartesian = TRUE)
sp2 <- sp2[season2 == last + 1]
sp2[, moved_on := as.integer(replacement != coach)]
sp <- merge(sp, unique(sp2[, .(coach, spell, replacement, moved_on)]), by = c("coach", "spell"), all.x = TRUE)
sp[is.na(moved_on), moved_on := NA_integer_]                    # spell still running in 2026
sp[, ended := last < 2026]
cat(sprintf("\n%d head-coaching spells, %d ended with the club hiring someone else\n",
            nrow(sp), sum(sp$moved_on == 1, na.rm = TRUE)))

# does being moved on predict the next job?
first3 <- ps[, .SD[order(season)], by = coach]
sp[, key := paste(coach, first)]
nx <- merge(sp[!is.na(next_first), .(coach, prev_end = last, prev_moved_on = moved_on, next_first)],
            ps[, .(coach, season, wat17, act17)], by.x = c("coach", "next_first"), by.y = c("coach", "season"))
nx <- nx[!is.na(prev_moved_on)]
cat(sprintf("\ncoaches who got another job: %d, of whom %d had been moved on from\n", nrow(nx), sum(nx$prev_moved_on == 1)))
cmp <- nx[, .(n = .N, wat = mean(wat17, na.rm = TRUE), wins = mean(act17)), by = prev_moved_on]
print(cmp)
if (nrow(cmp) == 2 && all(cmp$n >= 5)) {
  tt <- t.test(wat17 ~ prev_moved_on, nx)
  cat(sprintf("difference in wins above talent in the first season of the new job: %+.2f (p = %.3f)\n",
              diff(rev(cmp$wat)), tt$p.value))
}
# how big a penalty does the evidence support? first-year rows of a new spell,
# retreads against first-timers, on the same replacement pool R/71 uses
fs <- merge(sp[, .(coach, first, prev = shift(last, 1, type = "lag")), by = coach][, .(coach, first, retread = !is.na(prev))],
            ps[, .(coach, season, wat17, act17)], by.x = c("coach", "first"), by.y = c("coach", "season"))
fs <- fs[!is.na(wat17)]
gap <- fs[, .(n = .N, wat = mean(wat17), wins = mean(act17)), by = retread]
cat("\nfirst season of a spell, retreads vs first-time head coaches:\n"); print(gap)
tt2 <- t.test(wat17 ~ retread, fs)
delta <- gap[retread == TRUE]$wat - gap[retread == FALSE]$wat
sd_wat <- sd(ps$wat17, na.rm = TRUE)
cat(sprintf("retread minus first-timer: %+.2f wins above talent (p = %.2f); that is %.2f standard deviations of a season\n",
            delta, tt2$p.value, delta / sd_wat))
write_csv(as.data.frame(data.table(estimate_wins = delta, p = tt2$p.value, sd_wat = sd_wat,
                                   per_sd = delta / sd_wat, n_retread = gap[retread == TRUE]$n,
                                   n_first = gap[retread == FALSE]$n)), "data/derived/retread_gap.csv")
write_csv(as.data.frame(sp), "data/derived/job_change.csv")

# ---------------------------------------------------------------- who this hits in 2026
ACT <- unique(pc[season == 2026 & week == 1, .(team26 = team, coach = trimws(head_coach))])
cur <- sp[last >= 2025 | first == 2026]
prev <- sp[ended == TRUE, .SD[which.max(last)], by = coach]
a <- merge(ACT, prev[, .(coach, prev_team = team, prev_last = last, prev_moved_on = moved_on, prev_seasons = seasons)],
           by = "coach", all.x = TRUE)
a <- merge(a, hc[season == 2025, .(coach, team25 = team)], by = "coach", all.x = TRUE)
a[, new_team := is.na(team25) | team25 != team26]
a[, status := fifelse(!new_team, "same team as last season",
              fifelse(is.na(prev_moved_on), "first head-coaching job",
              fifelse(prev_moved_on == 1, "his last club moved on from him", "left his last club on his own terms")))]
cat("\n2026 head coaches by how they got here:\n"); print(a[, .N, by = status])
print(a[new_team == TRUE, .(coach, team26, prev_team, prev_last, prev_seasons, status)][order(status, coach)])
write_csv(as.data.frame(a), "data/derived/job_change_2026.csv")
cat("\nOut: recency_halflife.csv, job_change.csv, job_change_2026.csv\n")
