# =============================================================================
# factory/86_fourth_down_nerve.R -- the two fourth-down questions Michael asked.
#
#   "I bet on like 4th and 5 or something teams are wayyyy less likely to go for
#    it if it's close? Those more iffy 'go for it situations' I bet we see a
#    huge falloff"
#
#   "Is there anything to suggest that teams 4th down conversion rates are worse
#    in high leverage situations?"
#
# The first is about NERVE: does the go rate collapse as the decision gets less
# obvious, and does it collapse further when the game is tight? The second is
# about EXECUTION: given they went, do they convert less often when it matters?
#
# Those are different failures and they matter differently. If the go rate falls
# but conversion holds, coaches are leaving points on the field out of caution
# and the caution is unearned. If conversion also falls, going really is harder
# under pressure. The two panels answer them separately.
#
# The book is nfl4th's go_boost: win probability gained by going rather than
# taking the better of punt and field goal. Everything is sliced by how big that
# edge is, because "should go by 8 points" and "should go by 1 point" are
# completely different asks of a coach's nerve. Comparing within a go_boost
# bucket is what makes the close-game comparison fair.
#
# Out: docs/figures/factory/fourth_down_nerve.png
#      data/factory/fourth_down_nerve.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))

# same filters the 4th-down bots use, and the same ones R/08 used
fd <- fd[qtr <= 4 & half_seconds_remaining >= 60 &
         !is.na(go_boost) & !is.na(vegas_wp) &
         vegas_wp > 0.05 & vegas_wp < 0.95 &
         play_type != "" & (is.na(qb_kneel) | qb_kneel != 1)]
fd[, went := as.integer(play_type %in% c("pass","run"))]
fd[, lev  := 4*vegas_wp*(1 - vegas_wp)]
hi <- quantile(fd$lev, 0.75, na.rm = TRUE)
fd[, moment := fifelse(lev >= hi, "Close game", "Everything else")]
cat(sprintf("fourth downs after filters: %s | %s to %s | high-leverage cut %.3f\n",
            format(nrow(fd), big.mark = ","), min(fd$season), max(fd$season), hi))

# ---------------------------------------------------------------- 1. nerve
# Only the spots where the book says GO. How marginal the call is drives
# everything, so bucket by it.
go <- fd[go_boost > 0]
go[, band := cut(go_boost, c(0, 1, 2, 4, 8, 100),
                 labels = c("0 to 1\nrazor thin", "1 to 2\nslim",
                            "2 to 4\nclear", "4 to 8\nobvious", "8+\nno argument"))]
nerve <- go[, .(n = .N, rate = 100*mean(went)), by = .(band, moment)]
nerve_all <- go[, .(n = .N, rate = 100*mean(went)), by = band]
cat("\n=== go rate by how strong the case is ===\n")
print(dcast(nerve, band ~ moment, value.var = c("n","rate")))
cat("\noverall:\n"); print(nerve_all[order(band)])

gap <- merge(nerve[moment == "Close game", .(band, close = rate)],
             nerve[moment == "Everything else", .(band, rest = rate)], by = "band")
gap[, diff := close - rest]
cat("\nclose game minus everything else, percentage points:\n"); print(gap)

# Michael's 4th and 5 specifically
cat("\n=== by distance, in spots where the book says go ===\n")
dist <- go[ydstogo <= 10, .(n = .N, rate = 100*mean(went),
                            boost = mean(go_boost)), by = ydstogo][order(ydstogo)]
print(dist)
write_csv(as.data.frame(nerve), "data/factory/fourth_down_nerve.csv")

nerve[, moment := factor(moment, levels = c("Everything else","Close game"))]
pA <- ggplot(nerve, aes(band, rate, fill = moment)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%.0f%%", rate), vjust = -0.55),
            position = position_dodge(width = 0.72), size = 3.1,
            fontface = "bold", colour = "grey25") +
  geom_text(data = nerve_all, aes(x = band, y = -6, label = sprintf("n = %s", format(n, big.mark = ","))),
            inherit.aes = FALSE, size = 2.7, colour = "grey45") +
  scale_fill_manual(values = c("Everything else" = "#9db6c9", "Close game" = "#D55E00")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-9, 100)) +
  labs(title = "The nerve question: how often do they actually go?",
       subtitle = "Win probability the model says going is worth, against how often coaches went",
       x = "how strong the case for going is (win probability points)", y = "went for it") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")

# ---------------------------------------------------------------- 2. execution
conv <- fd[went == 1 & !is.na(fourth_down_converted)]
cv <- conv[, .(n = .N, rate = 100*mean(fourth_down_converted)), by = moment]
cat("\n=== conversion rate given they went ===\n"); print(cv)
ct <- prop.test(c(conv[moment == "Close game", sum(fourth_down_converted)],
                  conv[moment == "Everything else", sum(fourth_down_converted)]),
                c(nrow(conv[moment == "Close game"]), nrow(conv[moment == "Everything else"])))
cat(sprintf("difference %.1f points, p = %.3f, 95%% CI [%.1f, %.1f]\n",
            100*diff(rev(ct$estimate)), ct$p.value, 100*ct$conf.int[1], 100*ct$conf.int[2]))

# by distance, because close games might simply feature different distances
# Buckets, not single yards: split by moment there are only a few hundred
# close-game attempts past 3 yards and a per-yard line would be noise.
conv[, dband := cut(ydstogo, c(0, 1, 3, 6, 99),
                    labels = c("1", "2-3", "4-6", "7+"))]
cvd <- conv[, .(n = .N, rate = 100*mean(fourth_down_converted)),
            by = .(dband, moment)][n >= 30]
cat("\nconversion by distance and moment:\n")
print(dcast(cvd, dband ~ moment, value.var = c("n","rate")))

cvd[, moment := factor(moment, levels = c("Everything else","Close game"))]
pB <- ggplot(cvd, aes(dband, rate, colour = moment, group = moment)) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = n)) +
  geom_text(aes(label = sprintf("%.0f%%", rate)), vjust = -1.3, size = 2.9,
            fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("Everything else" = "#7f97a8", "Close game" = "#D55E00")) +
  scale_size_area(max_size = 5, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.16))) +
  annotate("text", x = 2.3, y = 72,
           label = sprintf("Overall %.1f%% in close games against\n%.1f%% elsewhere. If anything it goes UP.\n(difference %+.1f points, p = %.2f)",
                           cv[moment == "Close game"]$rate,
                           cv[moment == "Everything else"]$rate,
                           cv[moment == "Close game"]$rate - cv[moment == "Everything else"]$rate,
                           ct$p.value),
           size = 3, colour = "grey30", hjust = 0, lineheight = 1.05) +
  labs(title = "The execution question: do they convert less when it matters?",
       subtitle = "Fourth-down conversion rate by distance, on the plays where they went",
       x = "yards to go", y = "converted") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")

THIN <- gap[band == "0 to 1\nrazor thin"]
NOARG <- nerve_all[band == "8+\nno argument"]$rate
RAZOR <- nerve_all[band == "0 to 1\nrazor thin"]$rate
p <- (pA | pB) + plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = "Coaches lose their nerve exactly where the call gets close, and it is not because the play gets harder",
    subtitle = "Fourth downs where the nfl4th model says go, 2018 to 2025, regulation only, win probability between 5% and 95%",
    caption = fig_caption(
      "nfl4th decision model on nflverse play-by-play 2018-2025",
      sprintf("%s fourth downs after filters, %s of them with a positive case for going. Close game is the top quartile of leverage.",
              format(nrow(fd), big.mark = ","), format(nrow(go), big.mark = ",")),
      paste0(sprintf("\nThe shape is the point, and the cause is not what it looks like. The go rate falls off a cliff as the case gets thinner, from %.0f%% when the model says go by\n", NOARG),
             sprintf("eight-plus points to %.0f%% when it says go by under one, and in close games the thin band drops another %.1f points. But conversion does NOT fall: teams convert\n",
                     RAZOR, -THIN$diff),
             sprintf("%.1f%% in close games against %.1f%% elsewhere, a gap of %.1f points with p = %.2f. So the caution is not earned by the plays being harder when it matters. It is\n",
                     cv[moment == "Close game"]$rate, cv[moment == "Everything else"]$rate,
                     cv[moment == "Close game"]$rate - cv[moment == "Everything else"]$rate, ct$p.value),
             "the coach, not the down. Built by R/factory/86.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/fourth_down_nerve.png", p, w = 13.4, h = 6.8)
