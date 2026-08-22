# =============================================================================
# 77_war_three_boards.R -- the three WAR boards side by side.
#
# The ask: "rebuild the tab that way": the market-anchored board, the board
# with same-season quarterback play removed (the Josh Allen objection), and
# the market-free board (adjusted point differential), same 50 coaches, so
# the agreement between them is the finding. Numbers are R/71's.
#
# Out: docs/figures/coaching_war_three_boards.png, data/derived/coaching_war_three_boards.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")

sn <- fread("data/derived/coaching_war_sensitivity.csv")[eligible == TRUE & !is.na(rank_main)]
b <- rbind(
  sn[, .(coach, board = "Market-anchored (the main board)", v = war_main, r = rank_main)],
  sn[, .(coach, board = "Same-season QB play removed", v = war_talent_same_season_qb, r = rank_war_talent_same_season_qb)],
  sn[, .(coach, board = "No market: adjusted point differential", v = pd_effect_ppg, r = rank_pd_effect_ppg)])
b[, board := factor(board, levels = unique(board))]
cons <- b[, .(top10_all = all(r <= 10), top10_n = sum(r <= 10), mean_rank = mean(r)), by = coach]
b <- merge(b, cons, by = "coach")
write_csv(as.data.frame(dcast(b, coach + top10_n + mean_rank ~ board, value.var = c("v", "r"))[order(mean_rank)]), "data/derived/coaching_war_three_boards.csv")
cat("top-10 on all three:", paste(cons[top10_all == TRUE]$coach, collapse = ", "), "\n")
cat("top-10 on two:", paste(cons[top10_n == 2]$coach, collapse = ", "), "\n")
show <- b[r <= 15]
show[, lab := sprintf("%s  %s", coach, fifelse(board == levels(board)[3], sprintf("%+.2f pts/g", v), sprintf("%+.2f", v)))]
show[, coach_f := factor(paste(board, r), levels = paste(rep(levels(board), each = 15), rep(15:1, 3)))]
p <- ggplot(show, aes(x = v, y = reorder(paste(board, sprintf("%02d", r)), -r))) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_segment(aes(x = 0, xend = v, yend = reorder(paste(board, sprintf("%02d", r)), -r), colour = factor(top10_n)), linewidth = 3.2, alpha = 0.9) +
  geom_text(aes(label = lab, x = pmax(v, 0) + 0.02), hjust = 0, size = 2.9, colour = "grey25") +
  facet_wrap(~ board, scales = "free") +
  scale_colour_manual(values = c(`3` = "#1B7837", `2` = "#2B8CBE", `1` = "grey70", `0` = "grey85"),
                      labels = c(`3` = "top 10 on all three boards", `2` = "top 10 on two", `1` = "top 10 on one", `0` = "not top 10"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.9))) +
  labs(title = sprintf("Three ways to score the same 50 coaches: %s are top-10 on all of them",
                       paste(sub("^\\S+ ", "", cons[top10_all == TRUE]$coach), collapse = ", ")),
       subtitle = paste0("Top 15 on each board, 2012-2025, head coaches with 4+ seasons. Left: wins above a first-season hire after payroll, Madden ratings, last season's\n",
                         "quarterback and franchise, anchored to the betting market. Middle: the same with the quarterback's play in the SAME season also removed, so a coach\n",
                         "gets nothing for his quarterback. Right: no market at all, point differential per game above the same talent controls. Colour marks agreement."),
       x = NULL, y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025; Madden launch ratings 2017-2025",
         "\nThe middle board cannot credit a coach for developing his quarterback; the left board cannot avoid crediting him for a quarterback who outplays his pay. Built by R/77 from R/71.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_blank(), legend.position = "top", legend.justification = "left",
        strip.text = element_text(face = "bold", hjust = 0, size = 11))
save_fig("docs/figures/coaching_war_three_boards.png", p, w = 14, h = 7.5)
cat("Out: coaching_war_three_boards.png\n")
