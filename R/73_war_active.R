# =============================================================================
# 73_war_active.R -- the Coaching WAR board for the 32 active head coaches.
#
# The ask, verbatim: "can we see the war leaderboard for active head coaches".
# Active = the week-1 head coach of each team in the 2026 playcaller file.
# Numbers come straight from R/71's data/derived/coaching_war.csv; nothing is
# refit. Coaches with fewer than 4 seasons in the 2012-2025 window carry a
# number but no rank on the main board (the shrinkage leaves mostly prior),
# so they are drawn lighter and say so. Coaches with no head-coaching season
# in the window are listed, not plotted.
#
# Out: docs/figures/coaching_war_active.png, data/derived/coaching_war_active.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")

w <- fread("data/derived/coaching_war.csv")
nm <- fread("data/derived/coaching_war_no_market.csv")[, .(coach, war_no_market, rank_no_market)]
cons <- fread("data/derived/coaching_war_consensus.csv")[, .(coach, rank_cons)]
w <- merge(w, nm, by = "coach", all.x = TRUE); w <- merge(w, cons, by = "coach", all.x = TRUE)
pc <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
act <- unique(pc[season == 2026 & week == 1, .(team, coach = trimws(head_coach))])
stopifnot(nrow(act) == 32)
a <- merge(act, w, by = "coach", all.x = TRUE)
a[, war_show := war_no_market]
a[, status := fifelse(is.na(war_show), "no head-coaching season in the window",
              fifelse(seasons >= 4, "4+ seasons, ranked on the main board", "under 4 seasons, mostly prior"))]
setorder(a, -war_show, na.last = TRUE)
a[!is.na(war_show), active_rank := seq_len(.N)]
cat("active head coaches, 2026:\n")
print(a[, .(team, coach, seasons, war_no_market = round(war_show, 2), rank_cons, rank_main = rank, status)])
write_csv(as.data.frame(a[, .(team, coach, seasons, first_season, last_season, war_market = war_per_season, war_no_market = war_show, rank_cons, rank_main = rank, active_rank, status)]),
          "data/derived/coaching_war_active.csv")

p <- a[!is.na(war_show)]
p[, lab := sprintf("%+.2f  (%s, %d season%s%s)", war_show, team, seasons, fifelse(seasons == 1, "", "s"),
                   fifelse(seasons >= 4, "", "; number is mostly the prior"))]
p[, coach_f := factor(coach, levels = rev(coach))]
missing <- a[is.na(war_show)]
pl <- ggplot(p, aes(y = coach_f)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "22") +
  geom_errorbar(aes(xmin = war_show - 0.55, xmax = war_show + 0.55, alpha = seasons >= 4), width = 0, colour = "grey55", orientation = "y") +
  geom_point(aes(x = war_show, alpha = seasons >= 4, colour = seasons >= 4), size = 3) +
  geom_text(aes(x = war_show + 0.62, label = lab, alpha = seasons >= 4), hjust = 0, size = 2.9, colour = "grey30") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "#2B8CBE", `FALSE` = "grey60"), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.05, 0.6))) +
  labs(title = "Active head coaches, market-free: the Harbaughs lead, and the top of the board is thin on evidence",
       subtitle = paste0("The 2026 week-1 head coaches on the market-free measure: point differential per game above payroll, Madden roster ratings and the\n",
                         "quarterback's pay and play, converted to wins, 2012-2025. No betting line anywhere, so nobody is paid for his own reputation.\n",
                         "No head-coaching season in the window, not shown: ", paste(sprintf("%s (%s)", missing$coach[1:3], missing$team[1:3]), collapse = ", "), ",\n",
                         paste(sprintf("%s (%s)", missing$coach[-(1:3)], missing$team[-(1:3)]), collapse = ", "), "."),
       x = "WAR per season", y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025; Madden ratings 2017-2025; 2026 head coaches from the playcaller file",
         "\nBar is a nominal interval of one win either side; the model's own uncertainty is on the report card tab. Numbers from R/78. Built by R/73.")) +
  theme_coach(grid = "none") +
  theme(plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/coaching_war_active.png", pl, w = 12, h = 2.4 + nrow(p) * 0.3)
cat("\nOut: coaching_war_active.png\n")
