# =============================================================================
# 07_predictability_defined.R
#
# Michael read the predictability cards on 13 Aug and pushed on the word three
# separate ways in one afternoon:
#   "Why is predictable play calling doing better?"
#   "This is saying Sean Mcvay is more predictable?"
#   "I feel like Kubiak was labeled as highly predictable by the public but not
#    here?"
#   "I feel like McVay is lauded for being unpredictable in the sense of lots of
#    movement, ya? But what you're saying is he is highly predictable based on
#    run/pass?"
# Nick conceded on text: "Predictable might be a bad way to frame it... Gotta
# define predictable better."
#
# He is right. Two different measures were both being called "predictable":
#   CALL predictability  -- given down, distance, field zone, score and half,
#                           how often does an optimal guesser call run/pass
#                           right, versus the league in those same spots?
#                           (guess_xs, extra correct guesses per 100 plays)
#   LOOK predictability  -- does the formation and personnel on the field add
#                           run/pass information BEYOND the situation?
#                           (presnap_tip, in bits)
# A caller can be high on one and low on the other. Conflating them is what
# made the McVay answer sound wrong.
#
# The other correction in here: LOOK predictability is badly contaminated by
# sample size. Its empirical-Bayes shrinkage pulls small-sample callers toward
# zero, so callers with more career plays measure as more telegraphed almost
# mechanically: r(log(plays), tip) = 0.60, R^2 = 0.36. McVay has both the
# largest sample and the highest raw tip. So the raw ranking is partly an
# artifact and has to be residualised before anyone reads a name off it.
#
# Sources: ~/stranger9977/nfl-analysis/scratch/pred_tip_disguise.rds (career,
# 77 callers, formation window 2016-2023, built by scripts/pred_tip_disguise_
# build.R) and scratch/pred_tab.rds (caller-season 2015-2025, built by
# scripts/predictability_build.R).
#
# Out: docs/figures/pred_two_axes.png
#      docs/figures/pred_blackjack.png
#      docs/figures/pred_stability.png
#      data/derived/predictability_defined.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

tip <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_tip_disguise.rds")))
tab <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_tab.rds")))[n_plays >= 300]

# --- the sample-size artifact, measured then removed ------------------------
fit <- lm(presnap_tip ~ log(n_plays_car), data = tip)
tip[, tip_resid := residuals(fit)]
r_conf <- cor(log(tip$n_plays_car), tip$presnap_tip)
cat(sprintf("\nLOOK predictability vs sample size: r = %.3f, R2 = %.3f\n",
            r_conf, summary(fit)$r.squared))

pct <- function(x) 100 * frank(x) / length(x)
tip[, `:=`(pct_call = pct(guess_xs), pct_look_raw = pct(presnap_tip),
           pct_look_adj = pct(tip_resid))]

NAMED <- c("Sean McVay", "Gary Kubiak", "Klint Kubiak", "Andy Reid",
           "Kyle Shanahan", "Ben Johnson", "Josh McDaniels", "Sean Payton")
cat("\n--- where the names Michael raised actually sit (percentiles) ---\n")
print(tip[off_play_caller %in% NAMED,
          .(off_play_caller, n = n_plays_car,
            call_pct = round(pct_call), look_raw_pct = round(pct_look_raw),
            look_adj_pct = round(pct_look_adj), epa = round(epa_play, 3))][order(-call_pct)])

write_csv(as.data.frame(tip), "data/derived/predictability_defined.csv")

# --- chart 1: they are two different axes -----------------------------------
lab1 <- tip[off_play_caller %in% NAMED]
mid_x <- median(tip$guess_xs); mid_y <- median(tip$tip_resid)

p1 <- ggplot(tip, aes(guess_xs, tip_resid)) +
  geom_hline(yintercept = mid_y, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = mid_x, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", alpha = 0.55, size = 1.9) +
  geom_point(data = lab1, colour = "#D55E00", size = 2.6) +
  geom_text_repel(data = lab1, aes(label = off_play_caller), size = 3.2,
                  fontface = "bold", colour = "#8a3d00", seed = 3,
                  box.padding = 0.5, min.segment.length = 0, max.overlaps = 20) +
  annotate("text", x = max(tip$guess_xs), y = max(tip$tip_resid), hjust = 1, vjust = 1,
           size = 3.2, fontface = "bold", colour = "grey35",
           label = "Calls the obvious play AND\nthe formation gives it away") +
  annotate("text", x = min(tip$guess_xs), y = min(tip$tip_resid), hjust = 0, vjust = 0,
           size = 3.2, fontface = "bold", colour = "grey35",
           label = "Calls against the situation AND\nshows you nothing pre-snap") +
  labs(
    title = "Two different things were both being called 'predictable'",
    subtitle = "Every play-caller with enough charted snaps, 2016 to 2023. Horizontal is the call, vertical is the look.",
    x = "CALL predictability  →  extra run/pass guesses a situational guesser nails per 100 plays, vs league",
    y = "LOOK predictability  →  formation tell,\nafter removing the sample-size artifact",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tip_disguise.rds, built from nflverse participation + play-by-play",
      sprintf("%d play-callers, formation-charted window 2016 to 2023.", nrow(tip)),
      paste0("\nCall predictability answers 'does he do what the down and distance suggest'. Look predictability answers 'does the formation add anything beyond that'.\n",
             "The vertical axis is residualised because the raw tell tracks sample size almost mechanically (r = 0.60): more career plays means less shrinkage means a higher measured tell.\n",
             "Built by R/07."))
  ) +
  theme_coach(grid = "none") +
  theme(axis.title = element_text(size = rel(0.8)))

save_fig("docs/figures/pred_two_axes.png", p1, w = 12, h = 7.4)

# --- chart 2: the blackjack chart -------------------------------------------
# Nick's explanation to Michael, and it survives contact with the data:
#   "It's kind of like hitting on 14 when the dealer shows a 10. Just do what
#    the book says. Ben Johnson is having a lot of success being unpredictable
#    in smart spots tho."
r_be <- cor(tip$guess_xs, tip$epa_play)
cat(sprintf("\nr(call predictability, offensive EPA/play) = %.3f across %d callers\n",
            r_be, nrow(tip)))

lab2 <- tip[off_play_caller %in% c("Ben Johnson", "Andy Reid", "Sean McVay",
                                   "Adam Gase", "Nathaniel Hackett", "Chip Kelly",
                                   "Luke Getsy", "Joe Brady", "Todd Monken")]
p2 <- ggplot(tip, aes(guess_xs, epa_play)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey45",
              linewidth = 0.6, linetype = "dashed", formula = y ~ x) +
  geom_point(colour = "#9db6c9", alpha = 0.55, size = 1.9) +
  geom_point(data = lab2, colour = "#D55E00", size = 2.6) +
  geom_text_repel(data = lab2, aes(label = off_play_caller), size = 3.2,
                  fontface = "bold", colour = "#8a3d00", seed = 9,
                  box.padding = 0.55, min.segment.length = 0, max.overlaps = 20) +
  labs(
    title = "The unpredictable callers are mostly just bad. Ben Johnson is the exception.",
    subtitle = "Call predictability against offensive EPA per play, career, 2016 to 2023",
    x = "→ more predictable (does what the situation calls for)",
    y = "offensive EPA per play",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tip_disguise.rds",
      sprintf("%d career play-callers. Correlation is %+.2f.", nrow(tip), r_be),
      paste0("\nNick's way of putting it: it is like hitting on 14 when the dealer shows a 10. Mostly you should just do what the book says, and the coaches who deviate for its own sake\n",
             "(Gase, Hackett, Getsy, Kelly) are the ones losing. The edge is in knowing the handful of spots worth deviating in, which is what Ben Johnson does. Built by R/07."))
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/pred_blackjack.png", p2, w = 12, h = 6.8)

# --- chart 3: is it even a stable trait? ------------------------------------
# Michael: "So teams aren't that similar year to year in a way?"
setorder(tab, off_play_caller, season)
tab[, `:=`(prev_gx = shift(guess_xs), prev_epa = shift(epa_play),
           prev_season = shift(season)), by = off_play_caller]
pr <- tab[!is.na(prev_gx) & season - prev_season == 1]
r_gx  <- cor(pr$guess_xs, pr$prev_gx)
r_epa <- cor(pr$epa_play, pr$prev_epa, use = "complete.obs")
cat(sprintf("\nyear-over-year: predictability r = %.3f, offensive EPA r = %.3f (n = %d pairs)\n",
            r_gx, r_epa, nrow(pr)))

stab <- rbind(
  data.frame(what = "How predictable he calls it", x = pr$prev_gx, y = pr$guess_xs),
  data.frame(what = "How well the offense plays", x = pr$prev_epa, y = pr$epa_play)
) %>% filter(!is.na(x), !is.na(y))
labs_df <- data.frame(
  what = c("How predictable he calls it", "How well the offense plays"),
  r = c(r_gx, r_epa)) %>% mutate(lab = sprintf("r = %.2f", r))

p3 <- ggplot(stab, aes(x, y)) +
  geom_point(colour = "#9db6c9", alpha = 0.5, size = 1.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "#D55E00",
              linewidth = 0.8, formula = y ~ x) +
  geom_text(data = labs_df, aes(x = -Inf, y = Inf, label = lab), inherit.aes = FALSE,
            hjust = -0.25, vjust = 1.6, size = 4.6, fontface = "bold", colour = "#8a3d00") +
  facet_wrap(~what, scales = "free") +
  labs(
    title = "A coach's play-calling tendencies carry over year to year, a little more than his results do",
    subtitle = "Each point is one play-caller in consecutive seasons: last year on the horizontal, this year on the vertical",
    x = "last season", y = "this season",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tab.rds, caller-seasons 2015 to 2025 with at least 300 called plays",
      sprintf("%d consecutive-season pairs.", nrow(pr)),
      paste0("\nAnswers Michael's question about whether teams look similar year to year. Both persist, and tendency persists a bit more strongly than performance does.\n",
             "That is what makes predictability worth measuring at all: it behaves like a trait of the coach rather than only a description of last season. Built by R/07."))
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/pred_stability.png", p3, w = 12, h = 6.2)
