# =============================================================================
# 54_stunt_rate.R -- how much does Macdonald stunt on passing downs?
#
# The ask, verbatim: "How much did they stunt on passing downs?" (with the
# sim-pressure comparison answered by R/49: his sim volume is league-average,
# 25th of 48 on passing downs; the caption carries that.)
#
# A stunt = two or more rushers crossing or looping so the blockers have to
# pass off moving targets; Sumer flags it per dropback. Passing downs =
# 2nd-and-8-plus, or 3rd/4th-and-5-plus.
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
#
# Out: docs/figures/stunt_rate.png
#      data/derived/stunt_rate.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & is_dropback == TRUE &
       !is.na(def_caller) & !is.na(stunt)]
d[, passing_down := (down == 2 & distance >= 8) | (down >= 3 & distance >= 5)]

t <- d[passing_down == TRUE, .(n_pd = .N, stunt_pd = 100 * mean(stunt)), by = def_caller][n_pd >= 250]
a <- d[, .(n_all = .N, stunt_all = 100 * mean(stunt)), by = def_caller]
t <- merge(t, a, by = "def_caller")
t[, rank_pd := frank(-stunt_pd, ties.method = "min")]
setorder(t, -stunt_pd)
write_csv(as.data.frame(t), "data/derived/stunt_rate.csv")

mac <- t[def_caller == "Mike Macdonald"]
cat(sprintf("Macdonald: passing-down stunt %.1f%% (rank %d of %d); all downs %.1f%%\n",
            mac$stunt_pd, mac$rank_pd, nrow(t), mac$stunt_all))
cat(sprintf("league medians: passing downs %.1f%%, all downs %.1f%%\n",
            median(t$stunt_pd), median(t$stunt_all)))
print(t[1:3, .(def_caller, stunt_pd = round(stunt_pd, 1))])

t[, nm := factor(def_caller, levels = rev(t$def_caller))]
mac <- t[def_caller == "Mike Macdonald"]
top1 <- t[1]

p <- ggplot(t, aes(stunt_pd, nm)) +
  geom_segment(aes(x = 0, xend = stunt_pd, y = nm, yend = nm), colour = "grey88", linewidth = 1.5) +
  geom_vline(xintercept = median(t$stunt_pd), linetype = "dotted", colour = "grey60") +
  annotate("text", x = median(t$stunt_pd) + 0.4, y = 2.2, label = "league middle",
           hjust = 0, size = 2.9, colour = "grey55") +
  geom_point(aes(x = stunt_all), colour = "grey65", size = 1.9, shape = 21, fill = "white", stroke = 0.8) +
  geom_point(colour = "grey55", size = 2.4) +
  geom_segment(data = mac, aes(x = 0, xend = stunt_pd, y = nm, yend = nm),
               colour = "#f3c7a8", linewidth = 1.5) +
  geom_point(data = mac, aes(x = stunt_all), colour = "#e59c6b", size = 2.6, shape = 21, fill = "white", stroke = 1) +
  geom_point(data = mac, colour = "#D55E00", size = 4.2) +
  geom_text(data = mac, aes(label = sprintf("  Macdonald: %.0f%%, %s of %d", stunt_pd, scales::ordinal(rank_pd), nrow(t))),
            colour = "#D55E00", fontface = "bold", size = 3.3, hjust = 0) +
  geom_text(data = top1, aes(label = sprintf("  %s: %.0f%%, most", def_caller, stunt_pd)),
            colour = "grey40", size = 2.9, hjust = 0) +
  scale_x_continuous(labels = function(v) paste0(v, "%"),
                     expand = expansion(mult = c(0.01, 0.24))) +
  labs(title = "Macdonald stunts on 42% of passing-down dropbacks, 7th-most in football",
       subtitle = paste0("Solid dot = share of passing-down dropbacks with a stunt (rushers crossing or looping so blockers must pass off moving targets).\n",
                         "Open dot = the same rate on all downs. Everyone stunts more on passing downs; his jump (+14 points) beats the league's (+11)."),
       x = "share of dropbacks with a stunt", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\n%d defensive callers with 250+ passing-down dropbacks. Passing down = 2nd-and-8-plus, or 3rd/4th-and-5-plus.", nrow(t)),
         paste0("\nThe stunt is the missing piece of the free-rusher mechanism: it gets a rusher home without adding one. The other lever, sim pressure (four rushers, one of\n",
                "them a linebacker or defensive back), he calls at league-average volume on passing downs (25th of 48) but converts at double the league rate (R/49).\n",
                "The full picture: heavy stunting, average-volume-but-lethal sims, and almost no actual blitzing. Built by R/54."))) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/stunt_rate.png", p, w = 9.5, h = 8.8)
cat("\nOut: docs/figures/stunt_rate.png, data/derived/stunt_rate.csv\n")
