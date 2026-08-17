# =============================================================================
# factory/96_examples.R -- "What is a coin flip high leverage play?"
#
# Michael, 17 Aug, looking at nuance_leverage.png: "What is a coin flip high
# leverage play?" and "Can you provide some examples of each spot here."
#
# Two dials, measured separately, and the chart he was looking at crosses them.
#
#   LEVERAGE  how much the play can swing the game. leverage = 4 * wp * (1-wp),
#             which is 1.00 at a 50/50 game and 0 at a decided one. "High" is
#             the top quarter of all snaps, and that quarter turns out to be
#             every play with the win probability between 36% and 64%.
#
#   CERTAINTY how obvious the call was, from the situation-only run/pass model.
#             It knows down, distance, field position, score and clock, and
#             nothing about who is playing. max(p, 1-p) is how sure it is.
#             Coin flip 50-60%, leaning 60-80%, obvious 80%+.
#
# Chart 1 draws both dials so the definition is visible rather than described.
# Chart 2 is twelve real plays, one that followed the model and one that went
# against it in each of the six cells, so he can pull tape.
#
# Out: docs/figures/factory/coinflip_anatomy.png
#      docs/figures/factory/coinflip_examples.png
#      data/factory/coinflip_examples.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(nflreadr)
})
source("R/lib/theme_coach.R")

d  <- as.data.table(readRDS("data/factory/y_pass_preds.rds"))
d  <- d[!is.na(off_play_caller) & off_play_caller != ""]
d[, certainty := pmax(p, 1 - p)]
d[, bin := cut(certainty, c(0.5, 0.6, 0.8, 1.0),
               labels = c("Coin flip", "Leaning", "Obvious"), include.lowest = TRUE)]
d[, followed := as.integer((p >= 0.5 & y == 1) | (p < 0.5 & y == 0))]

hi_cut <- quantile(d$leverage, 0.75, na.rm = TRUE)
d[, lev_bin := fifelse(leverage >= hi_cut, "High leverage", "Everything else")]

# leverage >= hi_cut solves to a symmetric window on win probability
wp_lo <- (1 - sqrt(1 - hi_cut)) / 2
wp_hi <- (1 + sqrt(1 - hi_cut)) / 2
cat(sprintf("high leverage = top quartile = leverage >= %.3f = win probability between %.1f%% and %.1f%%\n",
            hi_cut, 100*wp_lo, 100*wp_hi))

# ---------------------------------------------------------------- what the cell contains
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
# vegas_wp, not wp: leverage in 00_features is built from the spread-aware win
# probability, so the number shown next to a play has to be the same one that
# put it in the high-leverage bucket.
d <- merge(d, mt[, .(game_id, play_id, qtr, yardline_100, score_differential,
                     half_seconds_remaining, wp = vegas_wp, defteam, week, season_type)],
           by = c("game_id","play_id"), all.x = TRUE)

cf_hi <- d[bin == "Coin flip" & lev_bin == "High leverage"]
cat(sprintf("\ncoin flip + high leverage: %s plays (%.1f%% of all snaps), pass rate %.3f\n",
            format(nrow(cf_hi), big.mark = ","), 100*nrow(cf_hi)/nrow(d), mean(cf_hi$y)))
cat("median play in that cell:\n")
cat(sprintf("  down %.0f, %.0f to go, %.0f yards from the end zone, score margin %+.0f, quarter %.0f\n",
            median(cf_hi$down), median(cf_hi$ydstogo), median(cf_hi$yardline_100),
            median(cf_hi$score_differential), median(cf_hi$qtr)))
cat("most common down and distance:\n")
print(head(cf_hi[, .N, by = .(down, dist = cut(ydstogo, c(0,2,5,10,15,99),
      labels = c("1-2","3-5","6-10","11-15","16+")))][order(-N)], 6))

# ---------------------------------------------------------------- chart 1: the two dials
# left panel: leverage against win probability, with the high-leverage window
lev_curve <- data.table(wp = seq(0, 1, 0.005))[, leverage := 4*wp*(1-wp)]
pA <- ggplot(lev_curve, aes(wp, leverage)) +
  annotate("rect", xmin = wp_lo, xmax = wp_hi, ymin = 0, ymax = 1.02,
           fill = "#D55E00", alpha = 0.13) +
  geom_hline(yintercept = hi_cut, linetype = "22", colour = "#D55E00", linewidth = 0.5) +
  geom_line(linewidth = 1.1, colour = "#2B8CBE") +
  annotate("text", x = 0.5, y = 0.52, label = "HIGH LEVERAGE\ntop quarter of all snaps",
           size = 3.3, fontface = "bold", colour = "#8a3d00", lineheight = 1.05) +
  annotate("text", x = 0.5, y = 0.30,
           label = sprintf("win probability\n%.0f%% to %.0f%%", 100*wp_lo, 100*wp_hi),
           size = 3, colour = "#8a3d00", lineheight = 1.05) +
  annotate("text", x = 0.02, y = 0.78, label = "game already decided,\nthe call matters less", size = 3,
           colour = "grey45", hjust = 0, lineheight = 1.05) +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(limits = c(0, 1.02), expand = expansion(mult = c(0.01, 0.02))) +
  labs(title = "Dial one: does the play matter?",
       subtitle = "Leverage = 4 x win probability x (1 - win probability)",
       x = "win probability before the snap", y = "leverage") +
  theme_coach(grid = "y")

# right panel: what the model thinks on 1st and 2nd down by distance
grid <- d[down %in% 1:3 & ydstogo <= 15,
          .(pass_prob = mean(p), n = .N), by = .(down, ydstogo)][n >= 300]
grid[, down_lab := factor(paste0(c("1st","2nd","3rd")[down], " down"),
                          levels = c("1st down","2nd down","3rd down"))]
pB <- ggplot(grid, aes(ydstogo, pass_prob, colour = down_lab)) +
  annotate("rect", xmin = 0.4, xmax = 15.6, ymin = 0.40, ymax = 0.60,
           fill = "#D55E00", alpha = 0.13) +
  annotate("text", x = 15.4, y = 0.50, label = "COIN FLIP\nthe situation tells\nthe defense nothing",
           size = 3.2, fontface = "bold", colour = "#8a3d00", hjust = 1, lineheight = 1.05) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 1.7) +
  scale_colour_manual(values = c("1st down" = "#2B8CBE", "2nd down" = "#7A9E3F",
                                 "3rd down" = "#6b4c9a")) +
  scale_x_continuous(breaks = seq(1, 15, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.1, 1)) +
  labs(title = "Dial two: is the call obvious?",
       subtitle = "Average pass probability from the situation-only model",
       x = "yards to go", y = "model's pass probability", colour = NULL) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left")

suppressMessages(library(patchwork))
pAB <- (pA | pB) +
  plot_annotation(
    title = "A coin-flip high-leverage play needs both dials at once, a live game and a situation that gives nothing away",
    subtitle = sprintf("%s of all called plays, 2015 to 2025. The most common one is 1st and 10 near midfield in a one-score game.",
                       percent(nrow(cf_hi)/nrow(d), accuracy = 0.1)),
    caption = fig_caption(
      "nflverse play-by-play 2015-2025; run/pass model is situation only, season-grouped out-of-sample",
      sprintf("%s called plays. High leverage is the top quartile of leverage. Coin flip is max(p, 1-p) under 60%%.",
              format(nrow(d), big.mark = ",")),
      paste0("\nThe left dial is about the scoreboard; it says nothing about the play call. The right dial is about down, distance and field position; it says nothing about the\n",
             "stakes. Third and 12 is an obvious pass whether the game is tied or over. A tied game on 1st and 10 at the 45 is both live and unreadable.\n",
             "Note what the left dial does NOT mean. Leverage peaks when a game is even and games are most even at the start, so the top quartile is 36% first quarter and 14%\n",
             "fourth. It marks a CLOSE game, not a LATE one, which is why several of the examples below are early and tied. Built by R/factory/96.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/coinflip_anatomy.png", pAB, w = 13, h = 6.6)

# ---------------------------------------------------------------- chart 2: real plays
# Rule, stated on the chart: 2024 and 2025 only, one play that followed the
# model and one that went against it in each of the six cells. The play taken
# is the one nearest the middle of its cell on down, distance, field position,
# score and win probability, so it represents the spot rather than the tail of
# it. Picking the biggest EPA swing instead returned twelve pick-sixes and
# strip-sacks, which is a chart about turnovers, not about play calling.
pbp <- as.data.table(load_pbp(2024:2025))[, .(game_id, play_id, desc, time,
                                              posteam_score, defteam_score)]
ex <- merge(d[season >= 2024], pbp, by = c("game_id","play_id"), all.x = TRUE)
ex <- ex[!is.na(desc) & !is.na(epa)]

med <- ex[, .(m_dn = median(down), m_yt = median(ydstogo), m_yl = median(yardline_100),
              m_sd = median(score_differential), m_wp = median(wp)),
          by = .(bin, lev_bin, followed)]
ex <- merge(ex, med, by = c("bin","lev_bin","followed"))
# scaled by roughly one meaningful unit of each, so no single axis dominates
ex[, typ := ((down - m_dn))^2 + ((ydstogo - m_yt)/3)^2 + ((yardline_100 - m_yl)/10)^2 +
            ((score_differential - m_sd)/7)^2 + ((wp - m_wp)/0.15)^2]
# exact ties on situation are common; break them toward the play with something
# on it so there is tape worth watching
setorder(ex, typ, -epa)
pick <- ex[, .SD[1], by = .(bin, lev_bin, followed)]

ord <- function(n) c("1st","2nd","3rd","4th")[n]
pick[, cell := paste0(bin, " / ", ifelse(lev_bin == "High leverage", "high leverage", "normal"))]
pick[, matchup := sprintf("%d wk %d", season, week)]
# score with the team holding the ball first, so it reads with the logos
pick[, score := sprintf("%d-%d", posteam_score, defteam_score)]
pick[, situation := sprintf("Q%d %s | %s and %d at the %s %d | %s | win prob %.0f%%",
      qtr, time, ord(down), ydstogo,
      fifelse(yardline_100 > 50, "own", "opp"),
      fifelse(yardline_100 > 50, 100 - yardline_100, yardline_100),
      score, 100*wp)]
pick[, model_said := sprintf("%.0f%% pass", 100*p)]
pick[, call := fifelse(y == 1, "PASS", "RUN")]
pick[, verdict := fifelse(followed == 1, "followed the model", "went the other way")]
pick[, result := sprintf("%+.2f EPA", epa)]
pick[, short := {
  s <- gsub("^\\([^)]*\\) ", "", desc)
  s <- gsub("\\s+", " ", s)
  substr(s, 1, 96)
}]

setorder(pick, bin, lev_bin, -followed)
write_csv(as.data.frame(pick[, .(cell, posteam, defteam, matchup, situation, score,
                                 model_said, call, verdict, result, desc)]),
          "data/factory/coinflip_examples.csv")
cat("\n--- twelve example plays ---\n")
print(pick[, .(cell, verdict, posteam, defteam, matchup, score, model_said, call, result)])

# render as a table graphic so it sits with the other figures
pick[, row := .I]
pick[, y := -row]
cell_lab <- pick[, .(y = mean(y)), by = cell]

lvl <- c("Coin flip / high leverage", "Coin flip / normal",
         "Leaning / high leverage", "Leaning / normal",
         "Obvious / high leverage", "Obvious / normal")
pick[, band := match(cell, lvl) %% 2]

suppressMessages(library(nflplotR))
p2 <- ggplot(pick) +
  geom_rect(aes(xmin = 0, xmax = 100, ymin = y - 0.5, ymax = y + 0.5,
                fill = factor(band)), alpha = 0.5) +
  # ball carrier first, then the defense, so the score reads left to right
  geom_nfl_logos(aes(x = 3.2, y = y, team_abbr = posteam), width = 0.019) +
  geom_text(aes(x = 5.9, y = y, label = "v"), size = 2.6, colour = "grey55") +
  geom_nfl_logos(aes(x = 8.6, y = y, team_abbr = defteam), width = 0.019) +
  geom_text(aes(x = 11.6, y = y, label = matchup), hjust = 0, size = 2.75,
            colour = "grey40") +
  geom_text(aes(x = 21.5, y = y, label = situation), hjust = 0, size = 2.9,
            colour = "grey30") +
  geom_text(aes(x = 64, y = y, label = model_said), hjust = 0, size = 2.9,
            colour = "grey30") +
  geom_text(aes(x = 72, y = y, label = call, colour = verdict), hjust = 0,
            size = 3.05, fontface = "bold") +
  geom_text(aes(x = 78.5, y = y, label = verdict, colour = verdict), hjust = 0,
            size = 2.85) +
  geom_text(aes(x = 94, y = y, label = result), hjust = 0, size = 2.9,
            fontface = "bold", colour = "grey15") +
  geom_text(data = cell_lab, aes(x = -1, y = y, label = cell), hjust = 1,
            size = 3.15, fontface = "bold", colour = "#1e2126", lineheight = 0.95) +
  annotate("text", x = c(1.6, 21.5, 64, 72, 94), y = 0,
           label = c("game", "situation", "model", "call", "result"),
           hjust = 0, size = 2.9, fontface = "bold", colour = "grey45") +
  scale_fill_manual(values = c("0" = "#eef2f5", "1" = "#ffffff"), guide = "none") +
  scale_colour_manual(values = c("followed the model" = "#2B7A3F",
                                 "went the other way" = "#D55E00"), guide = "none") +
  scale_x_continuous(limits = c(-13, 102), expand = expansion(0)) +
  scale_y_continuous(limits = c(-12.8, 0.9), expand = expansion(mult = c(0.01, 0.01))) +
  labs(
    title = "Two real plays from every spot on the chart",
    subtitle = "One where the caller did what the situation implied and one where he went the other way, in each of the six buckets",
    caption = fig_caption(
      "nflverse play-by-play, 2024 and 2025 regular season and playoffs",
      "Model probability is out-of-sample from the situation-only run/pass model. Win probability is the spread-aware one that defines leverage.",
      paste0("\nHow these were chosen: inside each of the twelve boxes, the play sitting nearest the middle of that box on down, distance, field position, score and win\n",
             "probability, so each one represents its spot rather than the tail of it. Green is a call the situation implied, orange is a call against it. Built by R/factory/96."))
  ) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0,
                                  margin = margin(b = 4)),
        plot.subtitle = element_text(size = 11, colour = "grey35", hjust = 0,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 7.6, colour = "grey45", hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot",
        plot.margin = margin(14, 14, 10, 14))
save_fig("docs/figures/factory/coinflip_examples.png", p2, w = 14, h = 5.9)
