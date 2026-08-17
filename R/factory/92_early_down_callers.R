# =============================================================================
# factory/92_early_down_callers.R -- who runs too much on early downs?
#
# Michael, 17 Aug, looking at the early-down run rate chart: "What coaches are
# the best/worst at this? Playcallers."
#
# Raw early-down run rate is the wrong leaderboard, because a caller with the
# lead is supposed to run more. So the measure is his actual run rate on first
# and second down minus the rate the situation-only model expects given the
# down, distance, field position, score and clock he was handed. Positive means
# he runs more than the game state calls for.
#
# The model never sees who the caller is, so it cannot learn his tendency and
# hand it back. It is fit on all callers at once, so a very high-volume caller
# nudges the league baseline he is measured against, which is worth a sentence
# on the chart but not worth correcting for at 94 callers.
#
# Out: docs/figures/factory/early_down_callers.png
#      data/factory/early_down_callers.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

d <- as.data.table(readRDS("data/factory/y_pass_preds.rds"))
d <- d[!is.na(off_play_caller) & off_play_caller != "" & down %in% 1:2]

cl <- d[, .(n = .N,
            act_run = 100*(1 - mean(y)),
            exp_run = 100*(1 - mean(p)),
            epa     = mean(epa, na.rm = TRUE),
            first   = min(season), last = max(season)),
        by = .(caller = off_play_caller)][n >= 800]
cl[, over := act_run - exp_run]
setorder(cl, -over)
write_csv(as.data.frame(cl), "data/factory/early_down_callers.csv")

r_epa <- cor(cl$over, cl$epa)
cat(sprintf("%s early-down plays, %d callers with 800 or more\n",
            format(nrow(d), big.mark = ","), nrow(cl)))
cat(sprintf("correlation between running more than expected and offensive EPA: %+.3f\n", r_epa))
cat("\n--- runs the most above what the situation calls for ---\n")
print(head(cl[, .(caller, n, act_run = round(act_run,1), exp_run = round(exp_run,1),
                  over = round(over,1), epa = round(epa,3))], 10))
cat("\n--- and the least ---\n")
print(tail(cl[, .(caller, n, act_run = round(act_run,1), exp_run = round(exp_run,1),
                  over = round(over,1), epa = round(epa,3))], 10))

# how much is a caller's extra running worth, in his own EPA terms
gap <- d[, .(pass_epa = mean(epa[y == 1], na.rm = TRUE),
             run_epa  = mean(epa[y == 0], na.rm = TRUE)), by = .(caller = off_play_caller)]
cl <- merge(cl, gap, by = "caller")
cl[, own_gap := pass_epa - run_epa]
cat(sprintf("\nleaguewide early-down passing advantage: %+.3f EPA per play\n",
            mean(d$epa[d$y == 1], na.rm = TRUE) - mean(d$epa[d$y == 0], na.rm = TRUE)))

# ---------------------------------------------------------------- leaderboard
top <- rbind(head(cl[order(-over)], 12), tail(cl[order(-over)], 12))
top[, side := rep(c("Runs more than the situation calls for",
                    "Passes more than the situation calls for"), each = 12)]
setorder(top, over)
top[, caller := factor(caller, levels = caller)]

pA <- ggplot(top, aes(over, caller, fill = side)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", over),
                hjust = ifelse(over >= 0, -0.22, 1.22)),
            size = 2.95, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Runs more than the situation calls for" = "#D55E00",
                               "Passes more than the situation calls for" = "#2B8CBE")) +
  scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
  labs(title = "Twelve at each end",
       subtitle = "First and second down run rate minus the rate the game state expects",
       x = "percentage points of run rate above expected", y = NULL) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", legend.text = element_text(size = rel(0.82)),
        axis.text.y = element_text(size = rel(0.85)))

NAMED <- c("Andy Reid","Sean McVay","Kyle Shanahan","Ben Johnson","Matt LaFleur",
           "Arthur Smith","Luke Getsy","Dowell Loggains","Zac Taylor","Sean Payton",
           "Mike McDaniel","Kevin O'Connell","Todd Monken","Eric Bieniemy",
           "Joe Lombardi","Mike McCarthy","Nathaniel Hackett","Liam Coen")
lab <- cl[caller %in% NAMED]
pB <- ggplot(cl, aes(over, epa)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.35) +
  geom_smooth(aes(weight = n), method = "lm", formula = y ~ x, se = TRUE,
              colour = "#D55E00", fill = "#D55E00", alpha = 0.1, linewidth = 0.8) +
  geom_point(aes(size = n), colour = "#9db6c9", alpha = 0.75) +
  geom_point(data = lab, aes(size = n), colour = "#2B8CBE") +
  geom_text_repel(data = lab, aes(label = caller), size = 2.9, fontface = "bold",
                  colour = "#1d6a99", seed = 3, box.padding = 0.42,
                  min.segment.length = 0, max.overlaps = 22) +
  scale_size_area(max_size = 6, guide = "none") +
  labs(title = "And whether it costs them",
       subtitle = sprintf("Offensive EPA per play against the same measure. Correlation %+.2f.", r_epa),
       x = "run rate above expected on early downs", y = "EPA per play, all downs") +
  theme_coach(grid = "y")

p <- (pA | pB) + plot_layout(widths = c(1, 1.15)) +
  plot_annotation(
    title = "Andy Reid throws on early downs more than any caller in the league, and has the best offense to show for it",
    subtitle = "Play-callers 2015 to 2025, measured against a model that knows the down, distance, field position, score and clock but not who is calling",
    caption = fig_caption(
      "nflverse play-by-play 2015-2025; play-caller attribution from samhoppen/NFL_public",
      sprintf("%s first and second down plays, %d callers with at least 800. Leaguewide a pass is worth %+.3f EPA more than a run on early downs.",
              format(nrow(d), big.mark = ","), nrow(cl),
              mean(d$epa[d$y == 1], na.rm = TRUE) - mean(d$epa[d$y == 0], na.rm = TRUE)),
      paste0("\nRaw run rate would put a caller who spent three years with a lead at the top, so this is run rate above what his own situations called for. Reid is 8.4 points\n",
             sprintf("below expected and Arthur Smith is 6.2 above. Running more than the situation asks for tracks worse offense at %+.2f, which is real but loose: it is a tendency\n", r_epa),
             "across the league, not a verdict on any one caller. The model is fit on every caller at once, so a very high-volume caller shifts the baseline he is scored\n",
             "against by a small amount. Built by R/factory/92.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/early_down_callers.png", p, w = 13.6, h = 7.4)
