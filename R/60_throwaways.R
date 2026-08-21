# =============================================================================
# 60_throwaways.R -- "Have you ever looked into throwaways? Like if they are
# worth it vs. extending the play?" Plus the Caleb hook: "Caleb kind of
# defied all qb logic by tanking his sack rate last year."
#
# Design: price the MENU of ways a pressured dropback can end, in EPA, then
# show which menu items Caleb moved his pressured snaps between across his
# two seasons. Extend-and-throw = ball out at 3.5 seconds or later.
#
# HONEST LIMIT, said on both charts: this prices how pressured snaps
# actually ended. An extend attempt that fails becomes a sack, so the
# extend-and-throw row only contains the survivors; it is the observed
# scoreboard of each ending, not a counterfactual of what choosing to
# extend is worth before you know it works.
#
# Out: docs/figures/pressure_menu.png, docs/figures/caleb_menu.png
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
db <- d[season_type == 0 & garbage_time == FALSE & is_dropback == TRUE]
pr <- db[pressure == TRUE]
pr[, res := fifelse(is_sack == TRUE, "take the sack",
           fifelse(throwaway == TRUE, "throw it away",
           fifelse(quarterback_scramble == TRUE, "escape and run",
           fifelse(!is.na(time_to_throw) & time_to_throw >= 3.5, "extend, then throw",
           "get the ball out quick"))))]

t <- pr[, .(n = .N, epa = mean(expected_points_added, na.rm = TRUE)), by = res]
setorder(t, epa)
t[, nm := factor(res, levels = res)]
cat("menu:\n"); print(t[, .(res, n, epa = round(epa, 2))])

p1 <- ggplot(t, aes(epa, nm)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = epa, y = nm, yend = nm),
               colour = fifelse(t$epa < 0, "#e3b3b1", "#a8cbe0"), linewidth = 7, lineend = "round") +
  geom_text(aes(label = sprintf("%+.2f", epa), x = epa + fifelse(epa < 0, -0.09, 0.09),
                hjust = fifelse(epa < 0, 1, 0)),
            size = 3.6, fontface = "bold", colour = fifelse(t$epa < 0, "#C0504D", "#2B8CBE")) +
  geom_text(aes(x = fifelse(epa < 0, 0.05, -0.05), hjust = fifelse(epa < 0, 0, 1),
                label = sprintf("%s of pressured snaps", percent(n / sum(t$n), accuracy = 1))),
            size = 2.9, colour = "grey50") +
  scale_x_continuous(limits = c(-2.35, 0.95)) +
  labs(title = "What a pressured dropback is worth, by how the quarterback ends it",
       subtitle = paste0("Points per play for the offense on every pressured dropback, split by the ending. The throwaway answer: it beats the sack\n",
                         "by a full point, and everything else beats the throwaway. The whole game under pressure is avoiding the bottom row."),
       x = "points per play for the offense", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded, pressured dropbacks only",
         sprintf("\n%s pressured dropbacks. Extend, then throw = the ball out at 3.5 seconds or later.", comma(nrow(pr))),
         "\nHonest limit: this prices how snaps actually ENDED. An extend attempt that fails becomes a sack, so the extend row holds the survivors;\nit is the scoreboard of each ending, not the value of choosing to extend before you know it works. Built by R/60.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.95), face = "bold", colour = "grey25"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/pressure_menu.png", p1, w = 10.5, h = 5.8)

# ---------------------------------------------------------------- Caleb
cw <- pr[off_team == "CHI" & season %in% 2024:2025]
ct <- cw[, .(share = 100 * .N / nrow(cw[season == .BY$season])), by = .(season, res)]
ct[, nm := factor(res, levels = t$res)]
ct[, yr := factor(season, levels = c(2024, 2025), labels = c("2024-25 (before)", "2025-26 (Ben Johnson)"))]
cat("\nCaleb mix:\n"); print(dcast(ct, res ~ yr, value.var = "share"))

p2 <- ggplot(ct, aes(share, nm, fill = yr)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f%%", share)),
            position = position_dodge(width = 0.72), hjust = -0.15,
            size = 3.1, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("grey65", "#D55E00"), name = NULL) +
  scale_x_continuous(labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Where Caleb's sacks went: he learned to extend without dying",
       subtitle = paste0("How Chicago's pressured dropbacks END, his season before Ben Johnson against his first with him. Rows ordered worst\n",
                         "ending to best. The sack share collapsed 33% to 11%, and almost all of it moved to extending and throwing (18% to 34%),\n",
                         "not to throwaways. He stopped converting pressure into the worst item on the menu."),
       x = "share of pressured dropbacks ending this way", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, Chicago pressured dropbacks, garbage time excluded",
         sprintf("\n%d pressured dropbacks in 2024-25, %d in 2025-26.", nrow(cw[season == 2024]), nrow(cw[season == 2025])),
         "\nThe menu prices are on the companion chart: a sack costs -1.9 points, a throwaway -0.9, extending and throwing +0.0, a scramble +0.4. Built by R/60.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left",
        axis.text.y = element_text(size = rel(0.92), face = "bold", colour = "grey25"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/caleb_menu.png", p2, w = 10.5, h = 6.4)
cat("\nOut: pressure_menu.png, caleb_menu.png\n")
