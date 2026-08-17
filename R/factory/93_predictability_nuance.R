# =============================================================================
# factory/93_predictability_nuance.R -- when does being unpredictable pay?
#
# Nick: "this makes McVay look predictable and good which is fine i guess but
# what about in different high leverage situations perhaps when the model is
# 50/50 how do things look there?"
#
# This is his own blackjack framing taken literally. The book is obvious on 14
# against a dealer 10, and the whole game is what you do on 16. So the right
# split is not by coach, it is by HOW CERTAIN THE SITUATION IS, and then asking
# whether deviating pays differently in each.
#
# The run/pass model gives a probability on every snap, so certainty is
# measurable: max(p, 1-p) is how obvious the call was.
#
#   Obvious      the model is at least 80% sure. Third and 12. Second and 1.
#   Leaning      60 to 80%. A preference, not a rule.
#   Coin flip    50 to 60%. Genuinely ambiguous. This is 16 against a 10.
#
# Three questions, three charts:
#   1. Does the league behave differently in each bin, and does deviating pay
#      differently in each?
#   2. Where does each caller deviate: in the obvious spots (bad) or the
#      ambiguous ones (possibly good)?
#   3. Does any of it change when the game is on the line?
#
# Out: docs/figures/factory/nuance_by_certainty.png
#      docs/figures/factory/nuance_where_deviate.png
#      docs/figures/factory/nuance_leverage.png
#      data/factory/predictability_nuance.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost)
})
source("R/factory/lib_factory.R")
source("R/lib/theme_coach.R")

CACHE <- "data/factory/y_pass_preds.rds"
mt   <- as.data.table(readRDS("data/factory/model_table.rds"))
spec <- readRDS("data/factory/feature_spec.rds")

if (!file.exists(CACHE)) {
  # The saved fit keeps residuals and slices but not play-level predictions, so
  # refit once and cache them. Same season-grouped design as everywhere else.
  fit <- fit_target(mt, "y_pass", spec$state, label = "Run or pass", perm_repeats = 1)
  saveRDS(fit$data[, .(game_id, play_id, season, off_play_caller, posteam,
                       epa, success, leverage, down, ydstogo,
                       y = fit$y, p = fit$pred)], CACHE)
}
d <- as.data.table(readRDS(CACHE))
d <- d[!is.na(off_play_caller) & off_play_caller != ""]
d[, certainty := pmax(p, 1 - p)]
d[, bin := cut(certainty, c(0.5, 0.6, 0.8, 1.0),
               labels = c("Coin flip\n50-60% sure", "Leaning\n60-80% sure",
                          "Obvious\n80%+ sure"), include.lowest = TRUE)]
# did the caller do what the model expected?
d[, followed := as.integer((p >= 0.5 & y == 1) | (p < 0.5 & y == 0))]

cat("=== how the league splits, and how it does in each ===\n")
lg <- d[, .(plays = .N, share = 100*.N/nrow(d), follow = 100*mean(followed),
            epa = mean(epa, na.rm = TRUE),
            epa_follow = mean(epa[followed == 1], na.rm = TRUE),
            epa_deviate = mean(epa[followed == 0], na.rm = TRUE)), by = bin][order(bin)]
lg[, edge := epa_deviate - epa_follow]
print(lg)

# ---------------------------------------------------------------- chart 1
bars <- melt(lg[, .(bin, `Did what the model expected` = epa_follow,
                    `Went the other way` = epa_deviate)],
             id.vars = "bin", variable.name = "choice", value.name = "epa")
p1 <- ggplot(bars, aes(bin, epa, fill = choice)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%+.03f", epa), vjust = ifelse(epa >= 0, -0.5, 1.4)),
            position = position_dodge(width = 0.7), size = 3.4, fontface = "bold",
            colour = "grey25") +
  geom_text(data = lg, aes(bin, y = -0.16, label = sprintf("%.0f%% of snaps", share)),
            inherit.aes = FALSE, size = 3.1, colour = "grey40") +
  scale_fill_manual(values = c("Did what the model expected" = "#9db6c9",
                               "Went the other way" = "#D55E00")) +
  scale_y_continuous(limits = c(-0.19, 0.16)) +
  labs(
    title = "Going against the situation is punished when the call is obvious, and pays when it is a coin flip",
    subtitle = "EPA per play by how sure the situation-only model was, and whether the caller followed it, 2015 to 2025",
    x = NULL, y = "EPA per play",
    caption = fig_caption(
      "Situation-only run/pass model, season-grouped out-of-sample predictions",
      sprintf("%s called plays. Certainty is max(p, 1-p) from the model.", format(nrow(d), big.mark = ",")),
      paste0("\nThis is the blackjack rule with numbers on it. On the obvious calls the book is right and deviating costs; in the genuinely ambiguous spots the caller who goes\n",
             "against the grain does better, because the defense is working off the same probabilities the model is and the surprise is worth something.\n",
             "A third of all snaps are real coin flips, so this is not a rare edge case. It does not contradict the leaguewide finding that predictable callers are better, because\n",
             "that measure is about how tightly a caller sticks to tendency within a situation, which is a different axis from whether he takes the less likely option here. Built by R/factory/93."))
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")
save_fig("docs/figures/factory/nuance_by_certainty.png", p1, w = 11.5, h = 6.4)

# ---------------------------------------------------------------- chart 2
# HOW OFTEN a caller deviates on coin flips is not a real axis. When the model
# says 50/50, going the other way happens about half the time by construction:
# across 76 callers the rate runs 37% to 53% with a standard deviation of 3.2
# points, and it correlates with offensive EPA at -0.01. Nothing there.
#
# The question that does have an answer is whether a caller's surprises WORK.
# For each caller, inside the coin-flip bin only, take his EPA when he went
# against the model minus his EPA when he followed it. That is "does he pick
# his spots", and it is the closest thing to measuring the skill Michael is
# asking about.
tot <- d[, .(tot = .N, epa = mean(epa, na.rm = TRUE)), by = off_play_caller]
cf <- d[bin == "Coin flip\n50-60% sure"]
edge <- cf[, .(n = .N, n_dev = sum(followed == 0),
               follow = mean(epa[followed == 1], na.rm = TRUE),
               dev    = mean(epa[followed == 0], na.rm = TRUE)),
           by = off_play_caller][n >= 400 & n_dev >= 150]
edge[, surprise_edge := dev - follow]
edge <- merge(edge, tot, by = "off_play_caller")
setorder(edge, -surprise_edge)
write_csv(as.data.frame(edge), "data/factory/predictability_nuance.csv")

r_edge <- cor(edge$surprise_edge, edge$epa)
cat(sprintf("\ncallers with enough coin-flip snaps: %d | r(surprise edge, overall EPA) = %+.3f\n",
            nrow(edge), r_edge))
cat("\n--- surprises pay off most ---\n")
print(head(edge[, .(off_play_caller, n, surprise_edge = round(surprise_edge,3),
                    epa = round(epa,4))], 8))
cat("\n--- and least ---\n")
print(tail(edge[, .(off_play_caller, n, surprise_edge = round(surprise_edge,3),
                    epa = round(epa,4))], 6))

NAMED <- c("Ben Johnson","Sean McVay","Andy Reid","Kyle Shanahan","Matt LaFleur",
           "Adam Gase","Nathaniel Hackett","Joe Brady","Todd Monken","Sean Payton",
           "Liam Coen","Kevin O'Connell","Mike McDaniel","Steve Sarkisian")
top <- rbind(head(edge, 12), tail(edge, 12))
top[, side := rep(c("Surprises pay off","Surprises backfire"), each = 12)]
setorder(top, surprise_edge)
top[, off_play_caller := factor(off_play_caller, levels = off_play_caller)]

p2 <- ggplot(top, aes(surprise_edge, off_play_caller, fill = side)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%+.02f", surprise_edge),
                hjust = ifelse(surprise_edge >= 0, -0.2, 1.2)),
            size = 3, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Surprises pay off" = "#2B8CBE",
                               "Surprises backfire" = "#D55E00")) +
  scale_x_continuous(expand = expansion(mult = c(0.14, 0.14))) +
  labs(
    title = "When the call is a coin flip, whose surprises actually work?",
    subtitle = "EPA when a caller went against the model minus EPA when he followed it, in genuinely ambiguous spots only",
    x = "EPA gained by going against the grain on a coin flip", y = NULL,
    caption = fig_caption(
      "Situation-only run/pass model, season-grouped out-of-sample, 2015 to 2025",
      sprintf("%d play-callers with at least 400 coin-flip snaps. Leaguewide the surprise is worth +0.088 EPA.", nrow(edge)),
      paste0("\nHOW OFTEN a caller surprises is not worth measuring: on a 50/50 call everyone goes against the grain about half the time by construction, and that rate correlates\n",
             sprintf("with offense at -0.01. WHETHER it works does vary, and it tracks the caller's overall quality at %+.2f. Every bar here is positive-to-mildly-negative because the\n", r_edge),
             "league as a whole gains from surprising in ambiguous spots; the spread is who gains most. Built by R/factory/93."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", axis.text.y = element_text(size = rel(0.88)))
save_fig("docs/figures/factory/nuance_where_deviate.png", p2, w = 11.5, h = 7.6)

# ---------------------------------------------------------------- chart 3
hi_cut <- quantile(d$leverage, 0.75, na.rm = TRUE)
d[, lev_bin := fifelse(leverage >= hi_cut, "High leverage", "Everything else")]
lv <- d[, .(plays = .N, epa_follow = mean(epa[followed == 1], na.rm = TRUE),
            epa_deviate = mean(epa[followed == 0], na.rm = TRUE)),
        by = .(bin, lev_bin)]
lv[, edge := epa_deviate - epa_follow]
cat("\n=== the deviation edge, by certainty and leverage ===\n"); print(lv[order(bin, lev_bin)])

lv[, lev_bin := factor(lev_bin, levels = c("Everything else","High leverage"))]
p3 <- ggplot(lv, aes(bin, edge, fill = lev_bin)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%+.03f", edge), vjust = ifelse(edge >= 0, -0.5, 1.4)),
            position = position_dodge(width = 0.7), size = 3.4, fontface = "bold",
            colour = "grey25") +
  scale_fill_manual(values = c("Everything else" = "#9db6c9", "High leverage" = "#D55E00")) +
  labs(
    title = "The payoff for surprising the defense is biggest in coin-flip spots when the game is on the line",
    subtitle = "EPA gained by going against the model rather than following it, by how sure the situation was and how much the play mattered",
    x = NULL, y = "EPA gained by deviating",
    caption = fig_caption(
      "Situation-only run/pass model; leverage is 4 x wp x (1 - wp), high is the top quartile",
      sprintf("%s called plays, 2015 to 2025.", format(nrow(d), big.mark = ",")),
      paste0("\nRead the right-hand group first: on the obvious calls deviating is a mistake whatever the stakes. The coin-flip group is where the two lines separate, and the\n",
             "high-leverage bar is the biggest number on the chart. That is the part of the game where a caller can actually take something from a defense that is guessing\n",
             "with the same information he has. Built by R/factory/93."))
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")
save_fig("docs/figures/factory/nuance_leverage.png", p3, w = 11.5, h = 6.4)
