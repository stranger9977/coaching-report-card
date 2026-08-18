# =============================================================================
# factory/84_leaks.R -- where play-calling can obviously be improved.
#
# Michael: "Here's another topic for the narrative structure - where can
# playcalling be obviously improved? For example... running on early downs"
#
# The interesting answer is not that the early-down run is a mistake, which
# everyone has said. It is the SCALE GAP. Fourth down is the leak the internet
# argues about and it comes up about half a time per team per game. Running on
# early downs in situations where passing clearly wins comes up SIXTEEN times a
# game. Thirty times the volume, a fraction of the attention.
#
# HOW THE EARLY-DOWN LEAK IS DEFINED, and why it is not the usual sloppy
# version. The lazy claim compares all passes to all runs, which is confounded
# by situation: teams run when they are ahead and near the goal line. Here every
# early-down play is put in a cell of down x distance x field position x score
# margin, and the pass-minus-run gap is computed WITHIN the cell, using only
# cells with at least 25 of each. So the comparison is always like for like.
#
# WHAT THIS DOES NOT CLAIM. It does not price the leak in points. Converting
# those runs to passes at the observed average gap assumes the marginal run
# becomes an average pass, and it would not: the runs a coach gives up first are
# his most predictable ones, and defenses would adjust. The direction and the
# frequency are solid. The dollar value is not, and the chart says so rather
# than inventing a number.
#
# Out: docs/figures/factory/leaks.png
#      data/factory/leaks.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

mt <- as.data.table(readRDS("data/factory/model_table.rds"))
fd <- fread("data/derived/fourth_down_decisions.csv", showProgress = FALSE)
tg_all <- 2*uniqueN(mt$game_id)
tg_fd  <- 2*uniqueN(fd$game_id)

# ---------------------------------------------------------------- matched cells
d <- mt[!is.na(epa) & down %in% 1:2 & !is.na(y_pass)]
d[, dist_b := cut(ydstogo, c(0,2,4,7,10,15,99),
                  labels = c("1-2","3-4","5-7","8-10","11-15","16+"))]
d[, cell := paste(down, dist_b, cut(yardline_100, c(0,10,20,40,60,80,90,100)),
                  cut(score_differential, c(-99,-9,-4,0,4,9,99)))]
cl <- d[, .(n = .N, npass = sum(y_pass), nrun = sum(1 - y_pass),
            pe = mean(epa[y_pass == 1]), re = mean(epa[y_pass == 0])),
        by = .(cell, down, dist_b)][npass >= 25 & nrun >= 25]
cl[, gap := pe - re]
strong <- cl[gap > 0.15]
cat(sprintf("matched cells %d covering %s early-down plays | pass beats run in %.0f%% of cells\n",
            nrow(cl), format(sum(cl$n), big.mark = ","), 100*weighted.mean(cl$gap > 0, cl$n)))

L_EARLY <- sum(strong$nrun)/tg_all
L_FOURTH <- sum(fd[verdict == "should go"]$went == 0)/tg_fd
G_EARLY <- weighted.mean(strong$gap, strong$nrun)

leaks <- data.table(
  leak = c("Running on early downs\nwhen passing clearly wins",
           "Declining a fourth down\nthe math says to take"),
  per_game = c(L_EARLY, L_FOURTH),
  detail = c(sprintf("%s runs called in situations where an identical-situation pass\nwas worth %+.2f EPA more. Passing wins in %.0f%% of matched cells.",
                     format(sum(strong$nrun), big.mark = ","), G_EARLY,
                     100*weighted.mean(cl$gap > 0, cl$n)),
             sprintf("%s of %s clear go-for-it spots declined (%.0f%%).\nPriced properly by nfl4th against all three options.",
                     format(sum(fd[verdict == "should go"]$went == 0), big.mark = ","),
                     format(nrow(fd[verdict == "should go"]), big.mark = ","),
                     100*mean(fd[verdict == "should go"]$went == 0))))
write_csv(as.data.frame(leaks), "data/factory/leaks.csv")
cat(sprintf("\nper team-game: early-down runs %.1f | declined fourth downs %.2f | ratio %.0fx\n",
            L_EARLY, L_FOURTH, L_EARLY/L_FOURTH))

leaks[, leak := factor(leak, levels = rev(leak))]
pA <- ggplot(leaks, aes(per_game, leak, fill = leak)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.1f a game", per_game)), hjust = -0.15,
            size = 4.2, fontface = "bold", colour = "grey20") +
  geom_text(aes(x = 0.15, label = detail), hjust = 0, vjust = 2.7,
            size = 2.7, colour = "grey40", lineheight = 1.05) +
  scale_fill_manual(values = c("#9db6c9", "#D55E00"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.22))) +
  scale_y_discrete(expand = expansion(add = c(0.85, 0.4))) +
  labs(title = sprintf("One of these comes up %.0f times more often than the other", L_EARLY/L_FOURTH),
       subtitle = "Chances per team per game to make an obviously better call",
       x = "opportunities per team-game", y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.95), lineheight = 0.95))

# ---------------------------------------------------------------- where it lives
by_dd <- cl[, .(runs = sum(nrun), gap = weighted.mean(gap, nrun)),
            by = .(down, dist_b)][runs >= 2000]
by_dd[, lab := sprintf("%s & %s   (%s runs)", c("1st","2nd")[down], dist_b,
                       format(runs, big.mark = ","))]
setorder(by_dd, gap)
by_dd[, lab := factor(lab, levels = lab)]
cat("\n--- where the early-down leak concentrates ---\n")
print(by_dd[order(-gap), .(lab, runs, gap = round(gap, 3))])

pB <- ggplot(by_dd, aes(gap, lab)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(aes(fill = gap > 0.15), width = 0.68) +
  geom_text(aes(label = sprintf("%+.2f", gap), hjust = ifelse(gap >= 0, -0.2, 1.2)),
            size = 2.9, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#9db6c9"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.18))) +
  labs(title = "And where it lives",
       subtitle = "Pass minus run EPA within identical situations, by down and distance",
       x = "EPA advantage of passing, matched situations", y = NULL) +
  theme_coach(grid = "none")

p <- (pA / pB) + plot_layout(heights = c(1, 1.5)) +
  plot_annotation(
    title = "The play-calling leak everyone argues about is the small one",
    subtitle = "Early downs 2015 to 2025; fourth downs 2018 to 2025 where the nfl4th model says go by at least 1.5 win-probability points",
    caption = fig_caption(
      "nflverse play-by-play; nfl4th decision model",
      sprintf("%s early-down plays across %d matched cells of down, distance, field position and score. %s team-games.",
              format(sum(cl$n), big.mark = ","), nrow(cl), format(tg_all, big.mark = ",")),
      paste0("\nThe usual version of this claim compares all passes with all runs, which is confounded, because teams run when they are ahead and near the goal line. Every\n",
             "comparison here is inside a cell of down, distance, field position and score margin, using only cells with at least 25 of each, so it is always like for like.\n",
             "What this does NOT do is price the leak in points. Turning those runs into passes at the observed gap would assume the marginal run becomes an average pass,\n",
             "and it would not: the first runs a coach gives up are his most predictable ones, and defenses adjust. The frequency and the direction are solid; the point value\n",
             "is not, so no point value is quoted. Built by R/factory/84.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/leaks.png", p, w = 12.4, h = 9.4)
