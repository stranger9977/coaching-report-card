# =============================================================================
# factory/60_narratives.R -- the coach stories, straight off Michael's outline.
#
# Two of his outline questions turn out to have clean, surprising answers.
#
# "What Sean McVay did: Led NFL in 11 Personnel... Led NFL in 13 Personnel"
#   Both true, but never in the same season, and that is the story. He was the
#   most extreme 11-personnel caller in football in 2022 and 2023, then in 2025
#   became the most extreme 13-personnel caller in football. Nobody else in the
#   charting era moves like that. It is his "Adaptability" section, and it also
#   explains the year-to-year swings in his pre-snap tell from R/09.
#
# "Is play action more effective out of 13 personnel? Would assume better?"
#   No. It is worth +0.12 EPA out of 11 personnel, +0.09 out of 12, and
#   nothing at all out of 13. Heavy personnel telegraphs it: callers run play
#   action on more than half their 13-personnel dropbacks, so the defense stops
#   believing it.
#
# Out: docs/figures/factory/mcvay_reinvention.png
#      docs/figures/factory/pa_by_personnel.png
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

mt <- as.data.table(readRDS("data/factory/model_table.rds"))
d <- mt[!is.na(offense_personnel) & offense_personnel != ""]
pers <- function(x) {
  rb <- suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", x))); rb[is.na(rb)] <- 1L
  te <- suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", x))); te[is.na(te)] <- 1L
  paste0(rb, te)
}
d[, p := pers(offense_personnel)]

# ---------------------------------------------------------------- McVay
cs <- d[!is.na(off_play_caller) & off_play_caller != "",
        .(n = .N, p11 = 100*mean(p == "11"), p13 = 100*mean(p == "13"),
          epa = mean(epa, na.rm = TRUE)), by = .(off_play_caller, season)][n >= 300]
cs[, `:=`(rank11 = frank(-p11), rank13 = frank(-p13), callers = .N), by = season]
mv <- cs[off_play_caller == "Sean McVay"][order(season)]
cat("=== McVay by season ===\n")
print(mv[, .(season, p11 = round(p11,1), rank11, p13 = round(p13,1), rank13,
             of = callers, epa = round(epa,4))])

lg <- d[, .(lg11 = 100*mean(p == "11"), lg13 = 100*mean(p == "13")), by = season]
long <- rbind(
  mv[, .(season, grp = "Sean McVay, 11 personnel", v = p11)],
  mv[, .(season, grp = "Sean McVay, 13 personnel", v = p13)],
  lg[, .(season, grp = "League, 11 personnel", v = lg11)],
  lg[, .(season, grp = "League, 13 personnel", v = lg13)]
)
pal <- c("Sean McVay, 11 personnel" = "#2B8CBE", "Sean McVay, 13 personnel" = "#D55E00",
         "League, 11 personnel" = "#9db6c9", "League, 13 personnel" = "#E8A33D")
ends <- long[season == max(season)]

p1 <- ggplot(long, aes(season, v, colour = grp)) +
  geom_line(aes(linewidth = grepl("McVay", grp))) +
  geom_point(data = long[grepl("McVay", grp)], size = 2.2) +
  geom_text_repel(data = ends, aes(label = grp), hjust = 0, direction = "y",
                  nudge_x = 0.12, size = 3.2, fontface = "bold", seed = 3,
                  segment.colour = NA, xlim = c(2025.1, NA)) +
  scale_colour_manual(values = pal) +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.7), guide = "none") +
  scale_x_continuous(breaks = 2022:2025, expand = expansion(mult = c(0.03, 0.42))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Sean McVay tore up his own offense and rebuilt it, in one off-season",
    subtitle = "Share of plays from 11 personnel (1 back, 1 tight end) and 13 personnel (1 back, 3 tight ends)",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse participation personnel groupings, 2022 to 2025",
      "Play-callers with at least 300 charted plays in a season.",
      paste0("\nMichael's outline says McVay led the league in both 11 and 13 personnel. Both are true and they never happened in the same year, which is the interesting part.\n",
             "He ran 11 personnel on 92% and 95% of plays in 2022 and 2023, the most extreme in football both years. In 2025 he dropped to 61% and put 29% of his plays in\n",
             "13 personnel, again the most extreme in football, against a league average of 5%. His offense went from -0.09 EPA per play in 2022 to +0.12 in 2025. Built by R/factory/60."))
  ) +
  theme_coach(grid = "y") + theme(legend.position = "none",
                                  plot.margin = margin(10, 20, 8, 10))
save_fig("docs/figures/factory/mcvay_reinvention.png", p1, w = 12, h = 6.6)

# ---------------------------------------------------------------- play action
pa <- d[qb_dropback == 1 & !is.na(y_pa) & p %in% c("11","12","13")]
tab <- pa[, .(dropbacks = .N, pa_rate = 100*mean(y_pa),
              with_pa = mean(epa[y_pa == 1], na.rm = TRUE),
              no_pa   = mean(epa[y_pa == 0], na.rm = TRUE),
              se_w = sd(epa[y_pa == 1], na.rm = TRUE)/sqrt(sum(y_pa == 1)),
              se_n = sd(epa[y_pa == 0], na.rm = TRUE)/sqrt(sum(y_pa == 0))), by = p][order(p)]
tab[, `:=`(edge = with_pa - no_pa, se = sqrt(se_w^2 + se_n^2))]
cat("\n=== play action edge by personnel ===\n"); print(tab)

lab <- c("11" = "11 personnel\n1 back, 1 tight end", "12" = "12 personnel\n1 back, 2 tight ends",
         "13" = "13 personnel\n1 back, 3 tight ends")
tab[, plab := lab[p]]
p2 <- ggplot(tab, aes(plab, edge)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(aes(fill = edge > 0.02), width = 0.55) +
  geom_errorbar(aes(ymin = edge - 1.96*se, ymax = edge + 1.96*se), width = 0.12,
                linewidth = 0.7, colour = "grey30") +
  geom_text(aes(label = sprintf("%+.3f", edge), vjust = ifelse(edge > 0, -1.6, 2.4)),
            size = 4, fontface = "bold", colour = "grey25") +
  geom_text(aes(y = -0.045, label = sprintf("play action on %.0f%% of dropbacks", pa_rate)),
            size = 3.2, colour = "grey40") +
  scale_fill_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#D55E00"), guide = "none") +
  scale_y_continuous(limits = c(-0.06, 0.17)) +
  labs(
    title = "Play action stops working out of heavy personnel, because everyone can see it coming",
    subtitle = "EPA per dropback with play action minus without, by personnel grouping, 2022 to 2025",
    x = NULL, y = "play-action edge, EPA per dropback",
    caption = fig_caption(
      "nflverse play-by-play, FTN charting and participation personnel, 2022 to 2025",
      sprintf("%s charted dropbacks from these three groupings. Bars are 95%% intervals.", format(sum(tab$dropbacks), big.mark = ",")),
      paste0("\nMichael's outline asks whether play action is more effective out of 13 personnel and assumes it would be. It is the opposite. The fake is worth +0.12 EPA out of\n",
             "11 personnel, +0.09 out of 12, and nothing at all out of 13. The reason is in the grey line: callers run play action on more than half their 13-personnel dropbacks,\n",
             "so the look stops being information. It is the same lesson as the pre-snap tell work, arriving from a different direction. Built by R/factory/60."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/pa_by_personnel.png", p2, w = 11.5, h = 6.4)
