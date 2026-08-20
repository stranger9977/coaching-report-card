# =============================================================================
# 59_worst_fourth.R -- two asks, two charts:
#   1. "What were the worst 4th down decisions for 2025? And in the playoffs?"
#      -> docs/figures/worst_fourth.png (a clock-stamped ledger)
#   2. "For the coach bravery, how much do coaches vary? ...an easier way to
#      visualize" -> docs/figures/bravery_simple.png (one axis, one dot each)
#
# Decision cost = the win-probability points the model said the WRONG choice
# left on the table (the nfl4th go/kick recommendation R/19 uses): a punt or
# kick where going was worth X points costs X; a go where kicking was better
# costs the reverse. Cost prices the CHOICE, not the outcome.
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(nflreadr)
})
source("R/lib/theme_coach.R")

fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd <- fd[season == 2025 & !is.na(go_boost) & down == 4 & play_type %in% c('punt','field_goal','run','pass')]
fd[, coach := fifelse(posteam == home_team, home_coach, away_coach)]
fd[, went := play_type %in% c("run", "pass")]
fd[, cost := fifelse(went, pmax(0, -go_boost), pmax(0, go_boost))]
fd <- fd[vegas_wp > 0.05 & vegas_wp < 0.95]

worst <- fd[order(-cost)][1:12]
worst[, po := season_type == "POST"]
cat("=== worst 12 decisions of 2025-26 (cost in WP points) ===\n")
print(worst[, .(coach, posteam, defteam, week, po, qtr, time, ydstogo, play_type, cost = round(cost, 1))])
cat("\nworst playoff decision:\n")
print(fd[season_type == "POST"][order(-cost)][1:3, .(coach, posteam, defteam, week, qtr, time, ydstogo, play_type, cost = round(cost, 1))])

worst[, row := .I]
worst[, y := -row]
worst[, game := sprintf("%s%s vs %s, wk %d", fifelse(po, "PLAYOFFS: ", ""), posteam, defteam, week)]
worst[, situ := sprintf("Q%d %s, 4th & %d", qtr, time, ydstogo)]
worst[, did := fifelse(play_type == "punt", "punted",
              fifelse(play_type %in% c("field_goal"), "kicked the field goal",
              fifelse(went, "went for it", play_type)))]
worst[, cost_lab := sprintf("-%.1f win points", cost)]

p1 <- ggplot(worst, aes(y = y)) +
  geom_text(aes(x = 0.00, label = game), hjust = 0, size = 2.9,
            fontface = fifelse(worst$po, "bold", "plain"),
            colour = fifelse(worst$po, "#C0504D", "grey35")) +
  geom_text(aes(x = 0.30, label = situ), hjust = 0, size = 2.9, colour = "grey45") +
  geom_text(aes(x = 0.47, label = did), hjust = 0, size = 3, fontface = "bold", colour = "#1d6a99") +
  geom_text(aes(x = 0.66, label = coach), hjust = 0, size = 2.9, colour = "grey35") +
  geom_text(aes(x = 0.86, label = cost_lab), hjust = 0, size = 3, fontface = "bold", colour = "#D55E00") +
  annotate("text", x = c(0.00, 0.30, 0.47, 0.66, 0.86), y = 0,
           label = c("GAME", "SPOT", "THE CHOICE", "HEAD COACH", "WHAT IT COST"),
           hjust = 0, size = 2.6, fontface = "bold", colour = "grey55") +
  scale_x_continuous(limits = c(0, 1.02)) +
  scale_y_continuous(limits = c(min(worst$y) - 1, 1)) +
  labs(title = "The worst fourth-down decisions of 2025-26, priced by the choice",
       subtitle = "Cost = the win-probability points the recommendation model said the wrong choice left on the table, whichever direction the sin ran.\nPlayoff rows in red. Cost prices the decision at the moment it was made, not how the play turned out.",
       x = NULL, y = NULL,
       caption = fig_caption(
         "nflverse play data with the public fourth-down recommendation model behind this board's bravery charts",
         "\n2025-26 regular season and playoffs, game within reach (win probability between 5% and 95%). Built by R/59.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/worst_fourth.png", p1, w = 11.5, h = 5.6)

# ---------------------------------------------------------------- 2. bravery, one axis
bc <- fread("data/derived/brave_cowardly.csv")
bc <- bc[decisions >= 20]
setorder(bc, -era_shrunk)
bc[, nm := factor(coach, levels = rev(bc$coach))]
hi <- bc[1]; lo <- bc[.N]
cat(sprintf("\nbravery spread: %s %+.0f to %s %+.0f, %d coaches\n", hi$coach, hi$era_shrunk, lo$coach, lo$era_shrunk, nrow(bc)))

p2 <- ggplot(bc, aes(era_shrunk, nm)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = era_shrunk, y = nm, yend = nm,
                   colour = era_shrunk > 0), linewidth = 1.6, show.legend = FALSE) +
  geom_point(aes(colour = era_shrunk > 0), size = 2.6, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#2B8CBE", `FALSE` = "#C0504D")) +
  annotate("text", x = max(bc$era_shrunk) - 1, y = nrow(bc) - 9, hjust = 1,
           label = "goes for it MORE than the league\non the same clear-cut chances", size = 3, colour = "#1d6a99", fontface = "italic", lineheight = 1) +
  annotate("text", x = min(bc$era_shrunk), y = 1.5, hjust = 0,
           label = "kicks it away more", size = 3, colour = "#a04340", fontface = "italic") +
  scale_x_continuous(labels = function(v) sprintf("%+.0f", v)) +
  labs(title = "How much do coaches vary on clear go-for-it chances? Enormously",
       subtitle = sprintf(paste0("One number per head coach: how much more or less often he goes for it on clear-cut fourth downs than the league\n",
                                 "in his own seasons, in percentage points, small samples pulled toward zero. %s (%+.0f) and %s (%+.0f)\n",
                                 "are %.0f points apart on the SAME class of decision."),
                          hi$coach, hi$era_shrunk, lo$coach, lo$era_shrunk, hi$era_shrunk - lo$era_shrunk),
       x = "go rate on clear-cut fourth downs, points above or below the league", y = NULL,
       caption = fig_caption(
         "nflverse play data, 2018-19 through 2025-26 seasons; the same clear-cut universe as the brave and cowardly boards",
         "\nClear-cut = the recommendation model says going is worth at least 1.5 win-probability points, game within reach, 20+ such decisions per coach.\nGoing for it more repeats coach to coach across seasons (r = 0.76), one of the most stable traits on this board. Built by R/59.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.6), colour = "grey45"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/bravery_simple.png", p2, w = 10, h = 9.5)
cat("\nOut: worst_fourth.png, bravery_simple.png\n")
