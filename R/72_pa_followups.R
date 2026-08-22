# =============================================================================
# 72_pa_followups.R -- three follow-ups on the play-action charts (R/63).
#
# The asks, verbatim: "Why do you think this ticks up in the middle in a
# significant way" (the fake's payoff by prior runs peaks at 5-6 runs);
# "Maybe for this stuff we use all years"; "I wonder where Ben Johnson ranks
# on PA for early downs - also it's hilarious if PA is still more effective
# on third and long lol".
#
# ALL YEARS: not possible for play action. The only play-action flags in any
# public source are SumerSports and FTN charting, both 2022-23 onward.
# nflfastR has no fake flag. So the window stays 2022-23 through 2025-26 and
# the honest fix is to put the uncertainty on the chart instead.
#
# 1. The mid-game bump, with 95% intervals per bin and a test of whether the
#    curve is anything but flat -> docs/figures/pa_bite_runs_ci.png
# 2. Down and distance with intervals, so the 3rd-and-long "+0.08 on 107
#    fakes" is shown for what it is -> docs/figures/pa_when_bite_ci.png
# 3. Early-down play action by caller: rate and payoff, Ben Johnson marked
#    -> docs/figures/pa_early_callers.png, data/derived/pa_early_callers.csv
#
# Conventions: plain language, no Michael/Nick in rendered text, season spans,
# no em dashes in rendered text.
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R")]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, gm := paste(season, week, off_team)]
d[, prior_runs := cumsum(run_pass == "R") - (run_pass == "R"), by = gm]
db <- d[is_dropback == TRUE & !is.na(play_action) & !is.na(expected_points_added)]
db[, epa := expected_points_added]

lift_tab <- function(x, by) {
  t <- x[, .(n_pa = sum(play_action), n_no = sum(!play_action),
             pa = mean(epa[play_action == TRUE]), no = mean(epa[play_action == FALSE]),
             se = sqrt(var(epa[play_action == TRUE]) / sum(play_action) + var(epa[play_action == FALSE]) / sum(!play_action))),
         by = by]
  t[, lift := pa - no]
  t
}

# ---------------------------------------------------------------- 1. the bump
db[, run_b := cut(pmin(prior_runs, 12), breaks = c(-1, 0, 2, 4, 6, 8, 10, 12),
                  labels = c("0 runs yet", "1-2", "3-4", "5-6", "7-8", "9-10", "11+"))]
t1 <- lift_tab(db, "run_b")[order(run_b)]
cat("PA lift by prior runs, with SE:\n"); print(t1[, .(run_b, n_pa, lift = round(lift, 3), se = round(se, 3), z = round(lift / se, 1))])
# is the curve anything but flat? interaction of fake x bin in a linear model
m0 <- lm(epa ~ play_action + run_b, db); m1 <- lm(epa ~ play_action * run_b, db)
av <- anova(m0, m1); p_flat <- av$`Pr(>F)`[2]
cat(sprintf("test that the fake's lift differs across bins: F = %.2f, p = %.3f\n", av$F[2], p_flat))
# the 5-6 bin against its neighbours
bump <- t1[run_b == "5-6"]; nb <- t1[run_b %in% c("3-4", "7-8")]
cat(sprintf("5-6 bin lift %.3f (SE %.3f); neighbours %.3f and %.3f; bump vs neighbour mean z = %.1f\n",
            bump$lift, bump$se, nb$lift[1], nb$lift[2], (bump$lift - mean(nb$lift)) / sqrt(bump$se^2 + mean(nb$se^2) / 2)))
# what sits in the 5-6 bin: quarter mix and score state
db[, q := pmin(quarter, 4)]
cat("share of fakes by quarter within each bin:\n")
print(dcast(db[play_action == TRUE, .N, by = .(run_b, q)][, share := round(N / sum(N), 2), by = run_b], run_b ~ q, value.var = "share"))

p1 <- ggplot(t1, aes(run_b, lift, group = 1)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_ribbon(aes(ymin = lift - 1.96 * se, ymax = lift + 1.96 * se), fill = "#D55E00", alpha = 0.12) +
  geom_line(colour = "#D55E00", linewidth = 1.2) + geom_point(colour = "#D55E00", size = 3) +
  geom_text(aes(label = sprintf("%+.2f", lift)), vjust = -1.1, size = 3.1, colour = "#D55E00", fontface = "bold") +
  geom_text(aes(label = sprintf("%s fakes", comma(n_pa))), vjust = 2.2, size = 2.6, colour = "grey50") +
  scale_y_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = sprintf("The bump at 5-6 runs is inside the noise: the fake's lift is flat across run counts (p = %.2f)", p_flat),
       subtitle = paste0("What a play fake adds in points per dropback over a dropback without one, by how many times the offense had already run the ball\n",
                         "in that game. Band is the 95% interval for each bin. The middle bins ride higher on fewer fakes and wider intervals; a test that the lift\n",
                         "differs at all across bins does not reject flat. The fake pays about the same from the first snap to the twelfth run."),
       x = "runs by the offense earlier in the game", y = "what the fake adds (points per dropback)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nPlay-action charting exists only from 2022-23 in any public source (SumerSports and FTN), so the window cannot be widened; the intervals are the honest fix. Built by R/72.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/pa_bite_runs_ci.png", p1, w = 11, h = 6.5)

# ---------------------------------------------------------------- 2. down and distance with intervals
db[, dd := fifelse(down == 1 & distance == 10, "1st and 10",
          fifelse(down == 2 & distance <= 3, "2nd and short", fifelse(down == 2 & distance <= 7, "2nd and medium",
          fifelse(down == 2, "2nd and long", fifelse(down == 3 & distance <= 3, "3rd and short",
          fifelse(down == 3 & distance <= 7, "3rd and medium", fifelse(down == 3, "3rd and long", NA_character_)))))))]
ordr <- c("1st and 10", "2nd and short", "2nd and medium", "2nd and long", "3rd and short", "3rd and medium", "3rd and long")
t3 <- lift_tab(db[!is.na(dd)], "dd")
t3[, dd := factor(dd, levels = rev(ordr))]
cat("\nPA lift by down and distance, with SE:\n"); print(t3[order(dd), .(dd, n_pa, lift = round(lift, 3), se = round(se, 3), z = round(lift / se, 1))])
tl <- t3[dd == "3rd and long"]
p3 <- ggplot(t3, aes(y = dd)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_errorbarh(aes(xmin = lift - 1.96 * se, xmax = lift + 1.96 * se), height = 0.25, colour = "grey60") +
  geom_point(aes(x = lift, colour = abs(lift / se) > 2), size = 3.2) +
  geom_text(aes(x = lift + 1.96 * se + 0.02, label = sprintf("%+.2f  (%s fakes)", lift, comma(n_pa))), hjust = 0, size = 3, colour = "grey35") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey55"), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.05, 0.45))) +
  labs(title = sprintf("The fake pays on early downs; on 3rd and long the +%.2f sits on %d fakes and an interval that spans zero", tl$lift, tl$n_pa),
       subtitle = paste0("What a play fake adds in points per dropback over a dropback without one, by down and distance, with 95% intervals.\n",
                         "Orange: clears zero. 1st and 10 and 3rd and short carry it. Callers barely fake on 3rd and long, so whether anyone still bites there\n",
                         "is unanswerable at this sample: the interval runs from ", sprintf("%+.2f to %+.2f.", tl$lift - 1.96 * tl$se, tl$lift + 1.96 * tl$se)),
       x = "what the fake adds (points per dropback)", y = NULL,
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nSame-down-and-distance comparison; sacks and scrambles stay in the fake column. Built by R/72.")) +
  theme_coach(grid = "none")
save_fig("docs/figures/pa_when_bite_ci.png", p3, w = 11, h = 6)

# ---------------------------------------------------------------- 3. early-down play action by caller
ed <- db[down %in% 1:2 & off_caller != ""]
cal <- ed[, .(dropbacks = .N, fakes = sum(play_action), rate = mean(play_action),
              pa = mean(epa[play_action == TRUE]), no = mean(epa[play_action == FALSE]),
              se = sqrt(var(epa[play_action == TRUE]) / sum(play_action) + var(epa[play_action == FALSE]) / sum(!play_action))),
        by = off_caller][dropbacks >= 600]
cal[, lift := pa - no]
cal[, `:=`(rank_rate = frank(-rate), rank_pa = frank(-pa), rank_lift = frank(-lift), n = .N)]
lg <- ed[, .(rate = mean(play_action), pa = mean(epa[play_action == TRUE]), no = mean(epa[play_action == FALSE]))]
bj <- cal[off_caller == "Ben Johnson"]
cat(sprintf("\nearly-down play action, %d callers with 600+ early-down dropbacks. League rate %.1f%%, PA EPA %+.3f, non-PA %+.3f\n",
            nrow(cal), 100 * lg$rate, lg$pa, lg$no))
cat(sprintf("Ben Johnson: rate %.1f%% (rank %d), PA EPA %+.3f (rank %d), lift %+.3f (rank %d), %d fakes\n",
            100 * bj$rate, bj$rank_rate, bj$pa, bj$rank_pa, bj$lift, bj$rank_lift, bj$fakes))
print(cal[order(-rate)][, .(off_caller, fakes, rate = round(rate, 3), pa = round(pa, 3), lift = round(lift, 3), rank_rate, rank_pa, rank_lift)][1:12])
write_csv(as.data.frame(cal[order(-rate)]), "data/derived/pa_early_callers.csv")

lab_c <- function(x) sub("Kevin O'Connell", "O'Connell", sub("^\\S+ ", "", x))
cal[, lab := fifelse(off_caller %in% c("Ben Johnson", "Kyle Shanahan", "Sean McVay", "Andy Reid", "Matt LaFleur", "Sean Payton", "Kevin O'Connell") |
                     rank_rate <= 3 | rank_pa <= 3 | rank_rate >= n - 1, lab_c(off_caller), "")]
p4 <- ggplot(cal, aes(rate, pa)) +
  geom_hline(yintercept = lg$pa, colour = "grey80", linetype = "22") +
  geom_vline(xintercept = lg$rate, colour = "grey80", linetype = "22") +
  geom_point(aes(colour = off_caller == "Ben Johnson", size = fakes)) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70", max.overlaps = 30) +
  scale_colour_manual(values = c(`TRUE` = "#1B7837", `FALSE` = "grey55"), guide = "none") +
  scale_size_continuous(range = c(1.8, 5), guide = "none") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_number(style_positive = "plus", accuracy = 0.05)) +
  labs(title = sprintf("Early-down play action: Ben Johnson fakes on %.0f%% of dropbacks (%s of %d) and earns %+.2f a fake (%s)",
                       100 * bj$rate, scales::ordinal(bj$rank_rate), nrow(cal), bj$pa, scales::ordinal(bj$rank_pa)),
       subtitle = paste0("Every caller with 600+ early-down dropbacks, 2022-23 through 2025-26. Across: how often he fakes on 1st and 2nd down. Up: points per\n",
                         "dropback when he does. Dotted lines are the league (", sprintf("%.0f%% and %+.2f", 100 * lg$rate, lg$pa), "). Dot size is the number of fakes. ",
                         "Johnson is the green dot."),
       x = "play-action rate on early-down dropbacks", y = "points per play-action dropback, early downs",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nLift over his own non-fake dropbacks, for the record: Johnson %+.2f (%s of %d); league %+.2f. Built by R/72.",
                 bj$lift, scales::ordinal(bj$rank_lift), nrow(cal), lg$pa - lg$no))) +
  theme_coach(grid = "y")
save_fig("docs/figures/pa_early_callers.png", p4, w = 11, h = 7)
cat("\nOut: pa_bite_runs_ci.png, pa_when_bite_ci.png, pa_early_callers.png\n")
