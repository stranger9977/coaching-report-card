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
#    computable exactly and counterfactually. nfl4th's `go_boost` is the win
#    probability gained by going instead of kicking. A coach who kicks when
#    go_boost is +4 threw away four points of win probability at the moment he
#    decided, whether or not the punt team then pinned them at the one. That
#    is decision value with execution stripped out, which is the thing being
#    asked for. Summed over a career it is "win probability points thrown
#    away", and divided by 100 it is roughly games thrown away.
#
# 2. COUNTING STATS. Cumulative team EPA and WPA under each coach, shown
#    separately and labelled as what they are: mostly a measure of how good
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

# The cost of the choice actually made. Zero when the coach picked the better
# side; otherwise the full size of the edge he passed up.
d[, cost := fifelse(go_boost > 0 & kicked == 1,  go_boost,
            fifelse(go_boost < 0 & went   == 1, -go_boost, 0))]
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

games <- unique(d[, .(game_id, coach)])[, .(g = .N), by = coach]
dc <- d[, .(decisions = .N,
            wp_cost = sum(cost),
            wp_cost_hi = sum(cost[lev >= quantile(d$lev, 0.75)]),
            wp_cost_post = sum(cost[season_type == "POST"]),
            n_post = sum(season_type == "POST")), by = coach]
dc <- merge(dc, games, by = "coach")
dc[, `:=`(cost_per_game = wp_cost/g, games_thrown = wp_cost/100)]
dc <- dc[g >= 32]
setorder(dc, cost_per_game)
write_csv(as.data.frame(dc), "data/factory/decision_value.csv")

cat("\n=== best decision-makers (least win probability thrown away per game) ===\n")
print(head(dc[, .(coach, g, decisions, cost_per_game = round(cost_per_game,3),
                  games_thrown = round(games_thrown,2))], 10))
cat("\n=== worst ===\n")
print(tail(dc[, .(coach, g, decisions, cost_per_game = round(cost_per_game,3),
                  games_thrown = round(games_thrown,2))], 10))

# ---------------------------------------------------------------- chart 1
top <- rbind(head(dc, 12)[, side := "Wastes the least"],
             tail(dc, 12)[, side := "Wastes the most"])
top[, lab := sprintf("%s  (%d g)", coach, g)]
setorder(top, -cost_per_game)
top[, lab := factor(lab, levels = lab)]
lg_rate <- sum(d$cost)/nrow(unique(d[, .(game_id, coach)]))

p1 <- ggplot(top, aes(cost_per_game, lab, fill = side)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = lg_rate, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", cost_per_game)), hjust = -0.2,
            size = 3.1, fontface = "bold", colour = "grey25") +
  annotate("text", x = lg_rate + 0.03, y = 1.2, hjust = 0, size = 3.1,
           colour = "grey35", fontface = "bold", label = sprintf("league %.2f", lg_rate)) +
  scale_fill_manual(values = c("Wastes the least" = "#2B8CBE", "Wastes the most" = "#D55E00")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Win probability thrown away on fourth down, per game",
    subtitle = "What each coach's fourth-down choices cost at the moment he made them, regardless of whether the play then worked",
    x = "win probability points wasted per game", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play; decision model from the nfl4th package",
      sprintf("%s fourth downs, 2018 to 2025, head coaches with at least 32 games. Regulation, a minute or more left in the half, win probability between 5%% and 95%%.",
              format(nrow(d), big.mark = ",")),
      paste0("\nThis is decision value with execution stripped out. If going for it was worth four points of win probability and the coach punted, he is charged four points at the\n",
             "moment he decided, whether or not the punt then pinned them at the one. It is the counterfactual cost of the choice, not the result of the play.\n",
             sprintf("Leaguewide that is %.0f points of win probability, about %.0f games, thrown away over eight seasons. Built by R/factory/95.", sum(d$cost), sum(d$cost)/100)))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", axis.text.y = element_text(size = rel(0.88)))
save_fig("docs/figures/factory/decision_cost.png", p1, w = 12, h = 7.6)

# ---------------------------------------------------------------- chart 2
# The running total: a debt curve per coach.
setorder(d, coach, season, week)
NAMED <- c("Bill Belichick","Andy Reid","Mike Tomlin","John Harbaugh","Sean McVay",
           "Kyle Shanahan","Mike Macdonald","Dan Campbell","Matt LaFleur",
           "Sean McDermott","Nick Sirianni","Brandon Staley")
cum <- d[coach %in% NAMED]
setorder(cum, coach, season, week)
cum[, dec_no := seq_len(.N), by = coach]
cum[, running := cumsum(cost), by = coach]
ends <- cum[, .SD[.N], by = coach]

p2 <- ggplot(cum, aes(dec_no, running, group = coach, colour = coach)) +
  geom_line(linewidth = 0.9) +
  geom_text_repel(data = ends, aes(label = coach), hjust = 0, size = 3.1,
                  fontface = "bold", direction = "y", nudge_x = 12,
                  segment.colour = NA, seed = 4, max.overlaps = 20) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.22))) +
  labs(
    title = "The running tab: win probability each coach has thrown away on fourth down",
    subtitle = "Cumulative cost across every clear and close fourth down of his career, in order. A flatter line is a better decision-maker.",
    x = "fourth downs faced (in career order)", y = "cumulative win probability wasted",
    caption = fig_caption(
      "nflverse play-by-play; nfl4th decision model, 2018 to 2025",
      "Selected head coaches. The slope is what matters; the length of a line is just how long the coach has been on the job.",
      paste0("\nEvery fourth down adds the cost of the choice made, so the line can only go up. A coach who always picks the better side has a flat line. The steepness is the rate at\n",
             "which he gives win probability away, and it is the only part of this chart that is about him rather than about tenure. Built by R/factory/95."))
  ) +
  theme_coach(grid = "y") + theme(legend.position = "none")
save_fig("docs/figures/factory/decision_cost_cumulative.png", p2, w = 12, h = 6.8)

# ---------------------------------------------------------------- chart 3
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
pbp <- merge(mt[, .(game_id, season, posteam, defteam, epa, wpa)], gm, by = "game_id")
tm <- pbp[!is.na(epa), .(plays = .N, tot_epa = sum(epa, na.rm = TRUE),
                         tot_wpa = sum(wpa, na.rm = TRUE)),
          by = .(coach = fifelse(posteam == home_team, home_coach, away_coach))]
tm <- merge(tm, games, by = "coach")[g >= 32]
setorder(tm, -tot_epa)
lab3 <- tm[coach %in% c(NAMED, "Sean Payton","Pete Carroll","Bruce Arians","Hue Jackson",
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
    subtitle = "Total offensive EPA accumulated under each head coach against how many games he has coached, 2015 to 2025",
    x = "games coached", y = "cumulative offensive EPA",
    caption = fig_caption(
      "nflverse play-by-play 2015 to 2025; head coaches with at least 32 games",
      "Cumulative, not per play, so it rewards longevity by construction.",
      paste0("\nShown because it was asked for and because it is worth seeing what it actually measures. The dominant axis here is the horizontal one: coaching a lot of games is how\n",
             "you accumulate a lot of EPA. It also mixes in the roster and the players' execution. For a ranking of decisions rather than of tenure, use the two charts above,\n",
             "where the cost of each choice is computed at the moment it was made. Built by R/factory/95."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/counting_stats.png", p3, w = 12, h = 6.8)
