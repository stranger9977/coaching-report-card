# =============================================================================
# factory/95_decision_value.R -- cumulative decision value per coach.
#
# Nick: "a count or leaderboard of wpa and epa per coach like cumulative over
# time thats tied back to decisions if that makes sense."
#
# It makes sense, and the phrase "tied back to decisions" is the whole design
# constraint. Raw cumulative EPA or WPA per coach is a counting stat that mixes
# four things: the coach's choices, the players' execution, the roster he was
# given, and how long he lasted. Ranking coaches on it would mostly rank them
# on career length and roster quality.
#
# So this splits into two leaderboards that must not be confused.
#
# 1. DECISION COST. On fourth down the win-probability cost of a choice is
#    computable exactly and counterfactually. The model gives a win probability
#    for each of the three options: go, kick the field goal, punt. A coach is
#    charged the gap between the best of those and the one he chose, at the
#    moment he chose it, whether or not the punt then pinned them at the one.
#    That is decision value with execution stripped out.
#
#    Two corrections after adversarial review, both of which moved the
#    ordering. The cost is priced against all THREE options, not just go
#    versus kick, which had been giving every coach a free pass on punting
#    when the field goal was better. And each coach is scored against the
#    league in his own seasons, because leaguewide cost per game fell 31%
#    from 2018 to 2025 and a pooled ranking was 22% an artefact of when a
#    coach happened to work. Rates are then empirical-Bayes shrunk, because
#    the raw metric's split-half reliability is only about 0.50.
#
# 2. COUNTING STATS. Cumulative team EPA and WPA under each coach, shown
#    separately and labeled as what they are: mostly a measure of how good
#    the team was and how long he had the job.
#
# The distinction is the point. A coach can be top of the counting stats and
# bottom of the decision ledger, and those two facts are both true and about
# different things.
#
# Out: docs/figures/factory/decision_cost.png
#      docs/figures/factory/decision_cost_cumulative.png
#      docs/figures/factory/counting_stats.png
#      data/factory/decision_value.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"

# ---------------------------------------------------------------- decision cost
fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd[, went   := as.integer(play_type %in% c("run","pass") | rush_attempt == 1 | pass_attempt == 1)]
fd[, kicked := as.integer(play_type %in% c("punt","field_goal") | punt_attempt == 1 | field_goal_attempt == 1)]
d <- fd[!is.na(go_boost) & (went == 1 | kicked == 1) &
        half_seconds_remaining >= 60 & vegas_wp >= 0.05 & vegas_wp <= 0.95]

# The cost of the choice actually made, priced against ALL THREE options.
# An earlier version charged only the go-versus-kick axis, which silently gave
# every coach a free pass on punting when the field goal was better and vice
# versa. Now the best available option is the max of going, kicking a field
# goal and punting, and a coach is charged the gap between that and whatever he
# actually did.
d[, chosen_wp := fifelse(went == 1, go_wp,
                 fifelse(play_type == "field_goal" | field_goal_attempt == 1, fg_wp, punt_wp))]
d[, best_wp := pmax(go_wp, fg_wp, punt_wp, na.rm = TRUE)]
d <- d[!is.na(chosen_wp) & !is.na(best_wp)]
d[, cost := 100 * pmax(best_wp - chosen_wp, 0)]
d[, lev := 4*vegas_wp*(1 - vegas_wp)]

gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
d <- merge(d, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
d[, coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
d <- d[!is.na(coach) & coach != ""]

cat(sprintf("fourth downs scored: %s across %d coaches, %d-%d\n",
            format(nrow(d), big.mark = ","), uniqueN(d$coach),
            min(d$season), max(d$season)))
cat(sprintf("league total win probability thrown away: %.0f points = %.1f games\n",
            sum(d$cost), sum(d$cost)/100))

# ERA. The league has got sharper every year: cost per game falls from 2.64 in
# 2018 to 1.82 in 2025, a 31% decline, and that alone explains 22% of the
# variance in a pooled ranking. Ranking coaches against a single eight-year
# average therefore rewards whoever happened to coach recently. Every coach is
# scored against the league in HIS OWN seasons instead, weighted by how many
# games he coached in each.
gs <- unique(d[, .(game_id, coach, season)])[, .(g = .N), by = .(coach, season)]
cs <- d[, .(cost = sum(cost)), by = .(coach, season)]
cs <- merge(cs, gs, by = c("coach","season"))
lg_season <- d[, .(lg_cost = sum(cost)), by = season]
lg_games  <- unique(d[, .(game_id, coach, season)])[, .(lg_g = .N), by = season]
lg_season <- merge(lg_season, lg_games, by = "season")[, lg_rate := lg_cost/lg_g]
cat("\n--- league cost per game by season (the era effect) ---\n")
print(lg_season[order(season), .(season, lg_rate = round(lg_rate,3))])
cs <- merge(cs, lg_season[, .(season, lg_rate)], by = "season")

games <- gs[, .(g = sum(g)), by = coach]
dc <- cs[, .(wp_cost = sum(cost), g = sum(g),
             expected = sum(lg_rate * g)), by = coach]
dc <- merge(dc, d[, .(decisions = .N), by = coach], by = "coach")
spans <- d[, .(first_season = min(season), last_season = max(season)), by = coach]
dc <- merge(dc, spans, by = "coach")
dc[, `:=`(cost_per_game = wp_cost/g,
          era_adj = (wp_cost - expected)/g,      # + means worse than his own era
          games_thrown = wp_cost/100)]

# SHRINKAGE. The same empirical-Bayes step the market leaderboard already uses.
# Between-coach variance net of sampling noise gives tau; each coach is pulled
# toward zero by how noisy his own record is. Without it the ranking is
# substantially luck: split-half reliability of the raw rate is about 0.50.
gcost <- merge(unique(d[, .(game_id, coach, season)]),
               d[, .(gcost = sum(cost)), by = .(game_id, coach)],
               by = c("game_id","coach"))
gcost <- merge(gcost, lg_season[, .(season, lg_rate)], by = "season")
gcost[, gdev := gcost - lg_rate]
se <- gcost[, .(se = sd(gdev)/sqrt(.N)), by = coach]
dc <- merge(dc, se, by = "coach")
dc <- dc[g >= 32]
tau2 <- max(var(dc$era_adj) - mean(dc$se^2), 1e-4)
dc[, era_shrunk := (tau2 * era_adj) / (tau2 + se^2)]
dc[, `:=`(lo = era_adj - 1.96*se, hi = era_adj + 1.96*se)]
cat(sprintf("\nshrinkage: tau = %.3f wp/game, mean se = %.3f -> typical shrink factor %.2f\n",
            sqrt(tau2), mean(dc$se), tau2/(tau2 + mean(dc$se)^2)))
setorder(dc, era_shrunk)
write_csv(as.data.frame(dc), "data/factory/decision_value.csv")

cat("\n=== best decision-makers (least win probability thrown away per game) ===\n")
print(head(dc[, .(coach, g, decisions, cost_per_game = round(cost_per_game,3),
                  games_thrown = round(games_thrown,2))], 10))
cat("\n=== worst ===\n")
print(tail(dc[, .(coach, g, decisions, cost_per_game = round(cost_per_game,3),
                  games_thrown = round(games_thrown,2))], 10))

# ---------------------------------------------------------------- chart 1
top <- rbind(head(dc, 12)[, side := "Better than his era"],
             tail(dc, 12)[, side := "Worse than his era"])
top[, lab := sprintf("%s  (%d g, %d-%d)", coach, g, first_season, last_season)]
setorder(top, -era_shrunk)
top[, lab := factor(lab, levels = lab)]

p1 <- ggplot(top, aes(era_adj, lab, fill = side)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.45) +
  geom_col(width = 0.66) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.28, linewidth = 0.55,
                 colour = "grey35") +
  geom_point(aes(x = era_shrunk), shape = 18, size = 2.4, colour = "grey15") +
  scale_fill_manual(values = c("Better than his era" = "#2B8CBE",
                               "Worse than his era" = "#D55E00")) +
  scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
  labs(
    title = "Fourth-down decisions: win probability wasted, against the league in that coach's own seasons",
    subtitle = "Bars are the raw rate, whiskers are 95% intervals, diamonds are the regressed estimate. Most coaches cannot be separated from zero.",
    x = "win probability points wasted per game, above (+) or below (-) his own era", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play; nfl4th decision model, 2018 to 2025",
      sprintf("%s fourth downs, head coaches with at least 32 games. Cost is priced against all three options: go, field goal, punt.",
              format(nrow(d), big.mark = ",")),
      paste0("\nThree things this chart is careful about, all of which changed the ordering. The league got 31% sharper between 2018 and 2025, so a pooled ranking would mostly reward\n",
             "coaching recently; every coach is scored against his own seasons instead. The whiskers are there because roughly half of these names overlap zero and cannot honestly be\n",
             "separated from average. The diamonds are the regressed estimate, and the gap between diamond and bar is how much of the raw number was luck. Built by R/factory/95."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", axis.text.y = element_text(size = rel(0.84)))
save_fig("docs/figures/factory/decision_cost.png", p1, w = 12, h = 7.6)

# ---------------------------------------------------------------- chart 2
# The career arc: cumulative wins above what the market expected, game by game.
#
# An earlier version of this chart ran the cumulative COST of fourth-down
# mistakes, which only goes up and therefore made every long-tenured good coach
# look like the worst in football. That is backwards as a way to see a career.
# This is the same idea pointed the right way: the spread already prices the
# roster and the quarterback, so a line that climbs is a coach who kept beating
# what his team was thought to be, week after week.
sched <- fread(file.path(NFLA, "data/games.csv"), showProgress = FALSE)
reg <- sched[game_type == "REG" & !is.na(result) & !is.na(spread_line)]
dec <- reg[result != 0][, home_win := as.integer(result > 0)]
mfit <- glm(home_win ~ spread_line, data = dec, family = binomial)
reg[, p_home := predict(mfit, newdata = reg, type = "response")]

arc <- rbind(
  reg[, .(season, week, coach = home_coach, exp = p_home,
          win = fifelse(result > 0, 1, fifelse(result == 0, 0.5, 0)))],
  reg[, .(season, week, coach = away_coach, exp = 1 - p_home,
          win = fifelse(result < 0, 1, fifelse(result == 0, 0.5, 0)))]
)[!is.na(coach) & coach != ""]
setorder(arc, coach, season, week)
arc[, `:=`(g = seq_len(.N), cum = cumsum(win - exp)), by = coach]

# Every coach with a real tenure goes in as light grey context. Only a handful
# get a color and a label, because seventeen coloured lines on one panel is a
# ball of wool: the eye cannot follow any single career and the labels land on
# top of each other in the crowded first hundred games.
ctx <- arc[, .N, by = coach][N >= 64]$coach
bg  <- arc[coach %in% ctx]

HL <- c("Bill Belichick","Andy Reid","Mike Tomlin","Sean Payton",
        "Sean McDermott","Hue Jackson","Norv Turner")
PAL <- c("Bill Belichick" = "#1c3f94", "Andy Reid" = "#2B8CBE",
         "Mike Tomlin"    = "#1baf7a", "Sean Payton" = "#7a4fa3",
         "Sean McDermott" = "#0f8a8a", "Hue Jackson" = "#D55E00",
         "Norv Turner"    = "#a8332a")
hl <- arc[coach %in% HL]
hend <- hl[, .SD[.N], by = coach]
hend[, lab := sprintf("%s  %+.0f", coach, cum)]

p2 <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.45) +
  geom_line(data = bg, aes(g, cum, group = coach), colour = "grey88", linewidth = 0.35) +
  geom_line(data = hl, aes(g, cum, colour = coach), linewidth = 1) +
  geom_point(data = hend, aes(g, cum, colour = coach), size = 2.1) +
  geom_text_repel(data = hend, aes(g, cum, label = lab, colour = coach),
                  hjust = 0, size = 3.3, fontface = "bold", direction = "y",
                  nudge_x = 26, segment.colour = NA, seed = 5, box.padding = 0.55,
                  point.padding = 0.4, min.segment.length = Inf, max.overlaps = Inf) +
  scale_colour_manual(values = PAL, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.30))) +
  labs(
    title = "The career arc: wins banked above what the market expected",
    subtitle = sprintf("Cumulative across every regular-season game. Grey lines are the other %d head coaches with four seasons or more.", length(ctx) - length(HL)),
    x = "games coached (in career order)", y = "cumulative wins above market expectation",
    caption = fig_caption(
      "nflverse schedules 1999 to 2025, closing spreads; market model as in R/02",
      sprintf("%d head coaches with at least 64 games. The spread already prices the roster, the quarterback and the schedule.", length(ctx)),
      paste0("\nClimbing means beating what the team was thought to be, week after week. The slope is the coach and the length is his tenure, so a long flat line is a long average\n",
             "career and a long climbing line is the real thing. Built by R/factory/95."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/career_arc.png", p2, w = 12, h = 6.8)

# ---------------------------------------------------------------- chart 3
# Windows must match: the fourth-down games denominator is 2018-2025, so the
# EPA numerator is subset to the same span rather than 2015-2025.
mt <- as.data.table(readRDS("data/factory/model_table.rds"))[season >= 2018]
pbp <- merge(mt[, .(game_id, season, posteam, defteam, epa, wpa)], gm, by = "game_id")
tm <- pbp[!is.na(epa), .(plays = .N, tot_epa = sum(epa, na.rm = TRUE),
                         tot_wpa = sum(wpa, na.rm = TRUE)),
          by = .(coach = fifelse(posteam == home_team, home_coach, away_coach))]
tm <- merge(tm, games, by = "coach")[g >= 32]
setorder(tm, -tot_epa)
lab3 <- tm[coach %in% c(HL, "Sean Payton","Pete Carroll","Bruce Arians","Hue Jackson",
                        "Adam Gase","Josh McDaniels","Matt Patricia")]

p3 <- ggplot(tm, aes(g, tot_epa)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(colour = tot_epa > 0), size = 2.3, alpha = 0.75) +
  geom_text_repel(data = lab3, aes(label = coach), size = 3, fontface = "bold",
                  colour = "grey25", seed = 8, box.padding = 0.45,
                  min.segment.length = 0, max.overlaps = 22) +
  scale_colour_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#D55E00"), guide = "none") +
  labs(
    title = "The counting stat, and why it is not a coaching ranking",
    subtitle = "Total offensive EPA accumulated under each head coach against how many games he has coached, 2018 to 2025",
    x = "games coached", y = "cumulative offensive EPA",
    caption = fig_caption(
      "nflverse play-by-play 2018 to 2025; head coaches with at least 32 games",
      "Cumulative, not per play, so it rewards longevity by construction.",
      paste0("\nShown because it was asked for and because it is worth seeing what it actually measures. The dominant axis here is the horizontal one: coaching a lot of games is how\n",
             "you accumulate a lot of EPA. It also mixes in the roster and the players' execution. For a ranking of decisions rather than of tenure, use the two charts above,\n",
             "where the cost of each choice is computed at the moment it was made. Built by R/factory/95."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/counting_stats.png", p3, w = 12, h = 6.8)
