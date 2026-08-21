# =============================================================================
# 61_fourth_v2_bears_pa.R -- two charts:
#   1. The worst-4th-down ledger REBUILT on the peer test, after film review
#      showed the win-probability cost crowns high-leverage moments rather
#      than indefensible choices. New measure: in this exact spot (distance,
#      field zone, score state, quarter), what share of the league's other
#      coaches went for it? -> docs/figures/worst_fourth_v2.png
#   2. "What was the Bears PA rate and rank before Ben Johnson?"
#      -> docs/figures/bears_pa.png
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")

# ---------------------------------------------------------------- 1. peer-test ledger
fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd <- fd[down == 4 & play_type %in% c("punt", "field_goal", "run", "pass") & !is.na(go_boost)]
fd[, went := play_type %in% c("run", "pass")]
fd[posteam == sub(" .*", "", yrdln), yl := 100 - as.integer(sub(".* ", "", yrdln))]
fd[posteam != sub(" .*", "", yrdln), yl := as.integer(sub(".* ", "", yrdln))]
fd[, dist_b := fifelse(ydstogo == 1, "and 1", fifelse(ydstogo == 2, "and 2", fifelse(ydstogo <= 5, "and 3 to 5", "and 6 plus")))]
fd[, zone := fifelse(yl >= 80, "own deep", fifelse(yl >= 60, "own side", fifelse(yl >= 45, "midfield", fifelse(yl >= 38, "plus territory", "kick range"))))]
fd[, st := fifelse(score_differential <= -9, "down big", fifelse(score_differential < 0, "down 1 score", fifelse(score_differential == 0, "tied", fifelse(score_differential <= 8, "up 1 score", "up big"))))]
fd[, late := qtr >= 4]
fd[, bucket := paste(dist_b, zone, st, late)]
peers <- fd[, .(peer_go = mean(went), n_bucket = .N), by = bucket]
fd <- merge(fd, peers, by = "bucket")

k <- fd[season == 2025 & went == FALSE & go_boost > 1 & n_bucket >= 40 &
        vegas_wp > 0.05 & vegas_wp < 0.95]
k[, coach := fifelse(posteam == home_team, home_coach, away_coach)]
setorder(k, -peer_go, -go_boost)
worst <- k[1:10]
cat("=== the peer-test worst 10 of 2025-26 ===\n")
print(worst[, .(coach, posteam, defteam, week, qtr, time, ydstogo, zone, st, play_type,
                peers = round(100 * peer_go), boost = round(go_boost, 1))])

worst[, row := .I]
worst[, y := -row]
worst[, game := fifelse(week == 22, sprintf("SUPER BOWL: %s vs %s", posteam, defteam), fifelse(week > 18, sprintf("PLAYOFFS: %s vs %s", posteam, defteam), sprintf("%s vs %s, wk %d", posteam, defteam, week)))]
worst[, situ := sprintf("Q%d %s, 4th %s, %s", qtr, time, dist_b, zone)]
worst[, score_lab := sprintf("%d-%d", posteam_score, defteam_score)]
worst[, did := fifelse(play_type == "punt", "punted", "kicked the field goal")]
worst[, peer_lab := sprintf("%d%% of coaches go there", round(100 * peer_go))]

p1 <- ggplot(worst, aes(y = y)) +
  geom_text(aes(x = 0.00, label = game), hjust = 0, size = 2.9, colour = "grey35") +
  geom_text(aes(x = 0.17, label = situ), hjust = 0, size = 2.85, colour = "grey45") +
  geom_text(aes(x = 0.43, label = score_lab), hjust = 0, size = 2.9, colour = "grey45") +
  geom_text(aes(x = 0.52, label = did), hjust = 0, size = 3, fontface = "bold", colour = "#1d6a99") +
  geom_text(aes(x = 0.66, label = coach), hjust = 0, size = 2.9, colour = "grey35") +
  geom_text(aes(x = 0.84, label = peer_lab), hjust = 0, size = 3, fontface = "bold", colour = "#D55E00") +
  annotate("text", x = c(0.00, 0.17, 0.43, 0.52, 0.66, 0.84), y = 0,
           label = c("GAME", "THE SPOT", "SCORE", "THE CHOICE", "HEAD COACH", "THE PEER TEST"),
           hjust = 0, size = 2.6, fontface = "bold", colour = "grey55") +
  scale_x_continuous(limits = c(0, 1.06)) +
  scale_y_continuous(limits = c(min(worst$y) - 1, 1)) +
  labs(title = "The worst fourth-down calls of 2025-26, judged by what every other coach does",
       subtitle = paste0("The first version of this list ranked by win-probability cost and crowned high-leverage moments instead of indefensible choices.\n",
                         "This one asks a fairer question: in this exact spot (distance, field zone, score state, quarter), how often did the REST of the\n",
                         "league go for it? A kick where nine of ten coaches go is a sin regardless of the clock."),
       x = NULL, y = NULL,
       caption = fig_caption(
         "nflverse play data; the recommendation model is Ben Baldwin's nfl4th, the engine behind his 4th-down bot; peer rates from all 4th downs, 2018-19 through 2025-26",
         "\nKept: kicks in buckets with 40+ league decisions where the recommendation model also favors going. Game within reach (win probability 5% to 95%).\nThe old No. 1, a 4th-and-1 punt from a team's own 19 protecting a lead late, drops off entirely: zero percent of coaches go there. Built by R/61.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/worst_fourth_v2.png", p1, w = 12.5, h = 5.2)

# ---------------------------------------------------------------- 2. Bears PA
source("R/factory/lib_sumer.R")
d <- load_sumer(seasons = 2022:2025)
db <- d[season_type == 0 & garbage_time == FALSE & is_dropback == TRUE & !is.na(play_action)]
t <- db[, .(pa = 100 * mean(play_action)), by = .(season, off_team)]
t[, rank := frank(-pa, ties.method = "min"), by = season]
chi <- t[off_team == "CHI"][order(season)]
med <- t[, .(med = median(pa)), by = season][order(season)]
cat("\nBears PA by season:\n"); print(chi)
chi[, yr := factor(season, labels = c("2022-23", "2023-24", "2024-25", "2025-26"))]
med[, yr := factor(season, labels = c("2022-23", "2023-24", "2024-25", "2025-26"))]
chi[, era := fifelse(season == 2025, "Ben Johnson", "before")]

p2 <- ggplot(chi, aes(yr, pa, group = 1)) +
  geom_line(data = med, aes(yr, med, group = 1), colour = "grey70", linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = 4.18, y = med[season == 2025]$med, label = "league\nmiddle", size = 2.8, colour = "grey55", hjust = 0, lineheight = 0.95) +
  geom_line(colour = "#f3c7a8", linewidth = 1.4) +
  geom_point(aes(colour = era), size = 5, show.legend = FALSE) +
  scale_colour_manual(values = c(`Ben Johnson` = "#D55E00", before = "grey55")) +
  geom_text(aes(label = sprintf("%.0f%%\n%s of 32", pa, scales::ordinal(rank)),
                vjust = fifelse(season == 2024, 1.35, -0.5)),
            size = 3.3, fontface = "bold", colour = fifelse(chi$season == 2025, "#D55E00", "grey35"),
            lineheight = 0.95) +
  annotate("text", x = 3.5, y = 30.5, label = "Ben Johnson arrives", size = 3.1,
           colour = "#D55E00", fontface = "italic", hjust = 0.5) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), limits = c(12, 40)) +
  coord_cartesian(clip = "off") +
  labs(title = "The Bears' play-fake rate: 30th of 32 the season before Ben Johnson, 2nd with him",
       subtitle = "Share of dropbacks with a play fake by season. The year before him the Bears faked less than all but two teams;\nhis first year they fake more than all but one.",
       x = NULL, y = "share of dropbacks with a play fake",
       caption = fig_caption(
         "SumerSports play charting, garbage time excluded",
         "\nHis league-leading fake habit travelled: Detroit's offense under him was 2nd of 35 callers in early-down play action. Built by R/61.")) +
  theme_coach(grid = "y") +
  theme(plot.margin = margin(10, 46, 8, 10))
save_fig("docs/figures/bears_pa.png", p2, w = 9.5, h = 6.2)
cat("\nOut: worst_fourth_v2.png, bears_pa.png\n")
