# =============================================================================
# 74_war_clean.R -- the Coaching WAR leaderboard, clean. One dot, one interval,
# one number per coach. The annotated version (R/71) stays as the reference;
# this is the one a reader should see first. Numbers are R/71's, not refit.
# Out: docs/figures/coaching_war_leaderboard_clean.png
# =============================================================================
suppressMessages({ library(data.table); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")
w <- fread("data/derived/coaching_war.csv")[eligible == TRUE & !is.na(rank)][order(rank)][1:25]
w[, coach_f := factor(coach, levels = rev(coach))]
w[, lab := sprintf("%+.2f   [%+.2f to %+.2f]   %d seasons", war_per_season, lo, hi, seasons)]
p <- ggplot(w, aes(y = coach_f)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "22") +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, colour = "grey60", orientation = "y") +
  geom_point(aes(x = war_per_season), colour = "#2B8CBE", size = 3.2) +
  geom_text(aes(x = hi + 0.08, label = lab), hjust = 0, size = 3, colour = "grey30") +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.05, 0.55))) +
  labs(title = "Coaching WAR, top 25: the familiar names lead, and every interval crosses zero",
       subtitle = paste0("Wins per season a head coach delivered above a first-season hire, after payroll, Madden roster ratings (2017 on),\n",
                         "last season's quarterback and the franchise he works in. 2012-2025, head coaches with 4+ seasons. Bar is the 95% interval. Dotted line is replacement level."),
       x = "WAR per season", y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025; Madden launch ratings 2017-2025",
         "\nThe annotated version, with every sensitivity in the label, is coaching_war_leaderboard.png. Built by R/74 from R/71's table.")) +
  theme_coach(grid = "none")
save_fig("docs/figures/coaching_war_leaderboard_clean.png", p, w = 11, h = 9.5)
cat("Out: coaching_war_leaderboard_clean.png\n")
