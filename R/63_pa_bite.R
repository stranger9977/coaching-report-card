# =============================================================================
# 63_pa_bite.R -- three play-action questions from one morning, one chart each:
#   1. The 538-style bite question: does the fake work better the more you
#      have run it? (their study used tracking on linebacker movement; no
#      source here has tracking, so this is the PAYOFF version: what the
#      fake actually earns by how many runs preceded it)
#      -> docs/figures/pa_bite_runs.png
#   2. Does play action work better with an elite running game?
#      -> docs/figures/pa_elite_rb.png
#   3. When do defenders stop biting? The fake's payoff by down and distance.
#      -> docs/figures/pa_when_bite.png
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(ggrepel)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R")]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, gm := paste(season, week, off_team)]
d[, prior_runs := cumsum(run_pass == "R") - (run_pass == "R"), by = gm]
db <- d[is_dropback == TRUE & !is.na(play_action)]

# ---------------------------------------------------------------- 1. by prior runs
db[, run_ct := pmin(prior_runs, 12)]
db[, run_b := cut(run_ct, breaks = c(-1, 0, 2, 4, 6, 8, 10, 12),
                  labels = c("0 runs yet", "1-2", "3-4", "5-6", "7-8", "9-10", "11+"))]
t1 <- db[, .(n = .N, pa_epa = mean(expected_points_added[play_action == TRUE], na.rm = TRUE),
             no_epa = mean(expected_points_added[play_action == FALSE], na.rm = TRUE),
             n_pa = sum(play_action)), by = run_b][order(run_b)]
t1[, lift := pa_epa - no_epa]
cat("PA lift by prior runs in game:\n"); print(t1[, .(run_b, n_pa, pa_epa = round(pa_epa,3), no_epa = round(no_epa,3), lift = round(lift,3))])

t1m <- melt(t1, id.vars = c("run_b", "n", "n_pa"), measure.vars = c("pa_epa", "no_epa"))
t1m[, kind := fifelse(variable == "pa_epa", "with a play fake", "no fake")]

p1 <- ggplot(t1m, aes(run_b, value, group = kind, colour = kind)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("with a play fake" = "#D55E00", "no fake" = "grey55"), name = NULL) +
  scale_y_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = "The fake pays from the first snap: play action by how many runs came before it",
       subtitle = paste0("Points per dropback with and without a play fake, by how many times the offense had run the ball earlier in that game.\n",
                         "The famous tracking study asked whether linebackers bite harder by the ninth run; the payoff version answers: the gap\n",
                         "is there before the FIRST run and never grows. Defenders bite on the fake itself, not on the memory of runs."),
       x = "runs by the offense earlier in the game", y = "points per dropback",
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nNo source on this board has player tracking, so this is the payoff version of the bite question, not measured linebacker steps.\nIt agrees with this board's earlier sequencing finding: play action works cold, and no caller's fake improves with setup runs. Built by R/63.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/pa_bite_runs.png", p1, w = 10.5, h = 6.4)

# ---------------------------------------------------------------- 2. elite run game
ts <- d[run_pass == "R", .(rush_epa = mean(expected_points_added, na.rm = TRUE), n_rush = .N), by = .(season, off_team)]
pa <- db[, .(pa_lift = mean(expected_points_added[play_action == TRUE], na.rm = TRUE) -
             mean(expected_points_added[play_action == FALSE], na.rm = TRUE),
             n_pa = sum(play_action)), by = .(season, off_team)]
t2 <- merge(ts, pa, by = c("season", "off_team"))[n_pa >= 80]
r2 <- cor(t2$rush_epa, t2$pa_lift)
cat(sprintf("\nteam-seasons: %d | cor(rushing quality, PA lift) = %.2f\n", nrow(t2), r2))
lab2 <- t2[order(-rush_epa)][c(1:3)]
lab2[, lab := sprintf("%s %d", off_team, season)]

p2 <- ggplot(t2, aes(rush_epa, pa_lift)) +
  geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.4) +
  geom_point(colour = "grey60", size = 2.4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "#D55E00", linewidth = 1) +
  geom_point(data = lab2, colour = "#2B8CBE", size = 3.2) +
  geom_text_repel(data = lab2, aes(label = lab), size = 2.9, colour = "#1d6a99",
                  fontface = "bold", seed = 3, min.segment.length = 0) +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  scale_y_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = sprintf("Does an elite run game make the fake better? Barely: r = %.2f", r2),
       subtitle = paste0("Each dot is a team-season: how good its actual runs were (across) against how much its play fakes added over its\n",
                         "no-fake dropbacks (up). The three best running teams of the window are marked. The fake does not need a feared\n",
                         "run game any more than it needs setup runs; the relationship is close to nothing."),
       x = "points per rush (the run game's actual quality)",
       y = "what the play fake added, points per dropback",
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\n%d team-seasons with 80+ play-action dropbacks. A flat line here matches the public research: fakes work on their own. Built by R/63.", nrow(t2)))) +
  theme_coach(grid = "none")
save_fig("docs/figures/pa_elite_rb.png", p2, w = 10.5, h = 7)

# ---------------------------------------------------------------- 3. when the bite dies
db[, dd := fifelse(down == 1, "1st and 10",
          fifelse(down == 2 & distance <= 3, "2nd and short",
          fifelse(down == 2 & distance <= 7, "2nd and medium",
          fifelse(down == 2, "2nd and long",
          fifelse(down == 3 & distance <= 3, "3rd and short",
          fifelse(down == 3 & distance <= 7, "3rd and medium",
          fifelse(down == 3, "3rd and long", NA_character_)))))))]
t3 <- db[!is.na(dd), .(n_pa = sum(play_action),
                       lift = mean(expected_points_added[play_action == TRUE], na.rm = TRUE) -
                              mean(expected_points_added[play_action == FALSE], na.rm = TRUE)), by = dd]
ordr <- c("1st and 10", "2nd and short", "2nd and medium", "2nd and long", "3rd and short", "3rd and medium", "3rd and long")
t3[, dd := factor(dd, levels = rev(ordr))]
setorder(t3, dd)
cat("\nPA lift by down and distance:\n"); print(t3[order(-dd)])

p3 <- ggplot(t3, aes(lift, dd)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = lift, y = dd, yend = dd),
               colour = fifelse(t3$lift > 0, "#a8cbe0", "#e3b3b1"), linewidth = 6, lineend = "round") +
  geom_text(aes(label = sprintf("%+.2f", lift), x = lift + fifelse(lift > 0, 0.02, -0.02),
                hjust = fifelse(lift > 0, 0, 1)),
            size = 3.3, fontface = "bold", colour = fifelse(t3$lift > 0, "#2B8CBE", "#C0504D")) +
  geom_text(aes(x = fifelse(lift > 0, -0.012, 0.012), hjust = fifelse(lift > 0, 1, 0),
                label = comma(n_pa)), size = 2.7, colour = "grey55") +
  scale_x_continuous(labels = label_number(style_positive = "plus"),
                     expand = expansion(mult = c(0.12, 0.14))) +
  labs(title = "Where the bite dies: the fake's payoff by down and distance",
       subtitle = paste0("What a play fake adds over a no-fake dropback in the same down and distance, in points per play. The fake pays most\n",
                         "where a run is believable: 3rd and short (+0.17) and 1st and 10 (+0.12). In pass-obvious spots the answer is subtler than the\n",
                         "guess: callers nearly stop faking at all (107 fakes on 3rd and long in four seasons), so the bite there is untestable."),
       x = "what the play fake adds, points per dropback (fake counts labeled left)", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nSame-down-and-distance comparison, so the fake is not credited for being called in better spots. Built by R/63.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.95), face = "bold", colour = "grey25"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/pa_when_bite.png", p3, w = 10.5, h = 6.4)
cat("\nOut: pa_bite_runs.png, pa_elite_rb.png, pa_when_bite.png\n")
