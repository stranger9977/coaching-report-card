# =============================================================================
# 75_war_stints.R -- Coaching WAR broken down by coach-team stint.
#
# The ask, verbatim: "it seems like you are giving vrabel and payton more
# seasons for old teams. break it down by team seasons".
#
# R/71's own nested fit says the coach effect is a coach-team pairing (SD
# 1.26 within team, 0.01 portable), so this is the honest unit. For every
# stint in the 2012-2025 window: seasons, raw wins above talent per 17 games
# (actual wins minus payroll-and-quarterback expectation, from R/71's season
# file), and the same number shrunk toward zero with the model's own
# variance ratio (residual SD 2.61 vs pairing SD 1.26, so k = 4.3 seasons of
# prior). Franchise and season terms are NOT subtracted, because inside one
# stint the coach and the building cannot be told apart; that is the point.
#
# Out: docs/figures/coaching_war_stints_active.png
#      data/derived/coaching_war_stints.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")

s <- fread("data/derived/coaching_war_seasons.csv")[in_main_window == TRUE & !is.na(wat17) & interim == FALSE]
v <- fread("data/derived/coaching_war_variance.csv")
k <- (v[model == "M_wat_nested" & grp == "Residual"]$sd / v[model == "M_wat_nested" & grp == "coach:team"]$sd)^2
cat(sprintf("shrinkage prior: %.1f seasons\n", k))
s[, spell := rleid(team), by = coach]
st <- s[, .(seasons = .N, first = min(season), last = max(season), wins = sum(act17) / .N,
            raw = mean(wat17), se = sd(wat17) / sqrt(.N)), by = .(coach, team, spell)]
st[, shrunk := raw * seasons / (seasons + k)]
st[, se_shrunk := v[model == "M_wat_nested" & grp == "Residual"]$sd / sqrt(seasons + k)]
setorder(st, -shrunk)
write_csv(as.data.frame(st[, .(coach, team, first, last, seasons, wins_per17 = wins, raw_wat_per_season = raw, shrunk_wat_per_season = shrunk, se = se_shrunk)]),
          "data/derived/coaching_war_stints.csv")
cat("top 15 stints, shrunk wins above talent per season:\n")
print(st[1:15, .(coach, team, first, last, seasons, raw = round(raw, 2), shrunk = round(shrunk, 2))])

w <- fread("data/derived/coaching_war.csv")
pc <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
act <- unique(pc[season == 2026 & week == 1, .(team_now = team, coach = trimws(head_coach))])
a <- merge(st, act, by = "coach")
a <- merge(a, w[, .(coach, war_main = war_per_season)], by = "coach", all.x = TRUE)
setorder(a, -war_main, coach, first)
a[, coach_f := factor(coach, levels = rev(unique(coach)))]
a[, lab := sprintf("%+.2f  %s %d-%d, %d season%s, raw %+.2f", shrunk, team, first, last, seasons, fifelse(seasons == 1, "", "s"), raw)]
a[, now := team == team_now]
cat("\nactive coaches with more than one stint:\n")
print(a[coach %in% a[, .N, by = coach][N > 1]$coach, .(coach, team, first, last, seasons, raw = round(raw, 2), shrunk = round(shrunk, 2), current = now)])

p <- ggplot(a, aes(y = coach_f)) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "22") +
  geom_errorbar(aes(xmin = shrunk - 1.96 * se_shrunk, xmax = shrunk + 1.96 * se_shrunk, group = spell),
                width = 0, colour = "grey70", orientation = "y", position = position_dodge(width = 0.7)) +
  geom_point(aes(x = shrunk, colour = now, group = spell), size = 3, position = position_dodge(width = 0.7)) +
  geom_text(aes(x = shrunk + 1.96 * se_shrunk + 0.1, label = lab, group = spell, colour = now), hjust = 0, size = 2.7,
            position = position_dodge(width = 0.7), show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#2B8CBE", `FALSE` = "#D55E00"), labels = c(`TRUE` = "current team", `FALSE` = "earlier team"), name = NULL) +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.05, 0.7))) +
  labs(title = "Active head coaches by stint: what each coach-team pairing won above its payroll, roster ratings and quarterback",
       subtitle = paste0("One row per coach, one dot per team he has led in the window. Wins above talent per 17 games, shrunk toward zero with the model's own\n",
                         "prior (about 4 seasons' worth), with 95% intervals. Orange is an earlier team, blue the current one. Ordered by the main board.\n",
                         "The franchise term is not subtracted here: inside one stint the coach and the building cannot be told apart."),
       x = "wins above talent per season, this stint (shrunk)", y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025; Madden ratings 2017-2025; 2026 head coaches from the playcaller file",
         "\nRaw value in each label is the unshrunk average. Coaches with no head-coaching season in the window are not shown. Built by R/75 from R/71's season file.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/coaching_war_stints_active.png", p, w = 12, h = 3 + uniqueN(a$coach) * 0.36)
cat("\nOut: coaching_war_stints_active.png\n")
