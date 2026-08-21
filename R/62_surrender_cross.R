# =============================================================================
# 62_surrender_cross.R -- the comedy metric against the sober one: Jon Bois's
# Surrender Index for every 2025-26 punt, crossed with this board's peer test.
#
# Surrender Index (via the public Surrender-Index bot code): field-position
# score x yards-to-go multiplier x score multiplier x clock multiplier.
#   field position: own side max(1, 1.1^(yards_from_own_goal - 40));
#                   opponent side 1.2^(yards_from_own_goal - 50) * 1.1^10
#   yards to go: >=10 -> 0.2, 7-9 -> 0.4, 4-6 -> 0.6, 2-3 -> 0.8, <2 -> 1.0
#   score: winning 1, tied 2, losing by 8 or less 4, losing by more 3
#   clock: if trailing in the second half, (seconds since halftime x 0.001)^3 + 1
#
# Peer test (R/61): share of the league's other coaches who went for it in
# the same spot (distance x field zone x score state x quarter), 2018-2025.
#
# Out: docs/figures/surrender_cross.png, data/derived/surrender_cross.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel)
})
source("R/lib/theme_coach.R")

fd <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd <- fd[down == 4 & play_type %in% c("punt", "field_goal", "run", "pass") & !is.na(go_boost)]
fd[, went := play_type %in% c("run", "pass")]
fd[posteam == sub(" .*", "", yrdln), yl := 100 - as.integer(sub(".* ", "", yrdln))]
fd[posteam != sub(" .*", "", yrdln), yl := as.integer(sub(".* ", "", yrdln))]
fd[, yfog := 100 - yl]   # yards from own goal line
fd[, dist_b := fifelse(ydstogo == 1, "1", fifelse(ydstogo == 2, "2", fifelse(ydstogo <= 5, "3-5", "6+")))]
fd[, zone := fifelse(yl >= 80, "own deep", fifelse(yl >= 60, "own side", fifelse(yl >= 45, "midfield", fifelse(yl >= 38, "plus", "kick"))))]
fd[, st := fifelse(score_differential <= -9, "down big", fifelse(score_differential < 0, "down 1 score", fifelse(score_differential == 0, "tied", fifelse(score_differential <= 8, "up 1 score", "up big"))))]
fd[, bucket := paste(dist_b, zone, st, qtr >= 4)]
peers <- fd[, .(peer_go = mean(went), n_bucket = .N), by = bucket]
fd <- merge(fd, peers, by = "bucket")

# surrender index for 2025-26 punts
p25 <- fd[season == 2025 & play_type == "punt"]
p25[, fp_score := fifelse(yfog <= 50, pmax(1, 1.1^(yfog - 40)), 1.2^(yfog - 50) * 1.1^10)]
p25[, ytg_mult := fifelse(ydstogo >= 10, 0.2, fifelse(ydstogo >= 7, 0.4, fifelse(ydstogo >= 4, 0.6, fifelse(ydstogo >= 2, 0.8, 1.0))))]
p25[, sc_mult := fifelse(score_differential > 0, 1, fifelse(score_differential == 0, 2, fifelse(score_differential >= -8, 4, 3)))]
p25[, clock_s := as.integer(sub(":.*", "", time)) * 60 + as.integer(sub(".*:", "", time))]
p25[, since_half := pmax(0, (pmin(qtr, 4) - 3) * 900 + (900 - clock_s))]
p25[, ck_mult := fifelse(qtr >= 3 & score_differential < 0, (since_half * 0.001)^3 + 1, 1)]
p25[, si := fp_score * ytg_mult * sc_mult * ck_mult]
p25[, coach := fifelse(posteam == home_team, home_coach, away_coach)]
p25[, game := fifelse(week == 22, sprintf("SUPER BOWL %s-%s", posteam, defteam),
             fifelse(week > 18, sprintf("playoffs %s-%s", posteam, defteam),
             sprintf("%s-%s wk%d", posteam, defteam, week)))]
setorder(p25, -si)
write_csv(as.data.frame(p25[, .(game, coach, qtr, time, ydstogo, yfog, score_differential, si = round(si, 1), peer_go = round(100 * peer_go))]),
          "data/derived/surrender_cross.csv")
cat("top 5 by surrender index:\n")
print(p25[1:5, .(game, coach, qtr, time, ydstogo, si = round(si), peers = round(100 * peer_go))])
cat(sprintf("\ncorrelation, surrender index vs peer test (all %d punts): r = %.2f\n",
            nrow(p25), cor(p25$si, p25$peer_go)))
cat(sprintf("agree-corner punts (peer_go >= 50%% & SI >= 100): %d\n", nrow(p25[peer_go >= 0.5 & si >= 100])))

lab_pts <- rbind(p25[order(-si)][1:3], p25[order(-peer_go)][1:3], p25[si >= 50 & peer_go >= 0.6])
lab_pts <- unique(lab_pts, by = c("game_id", "play_id"))
lab_pts[, lab := sprintf("%s (%s)", game, sub(".* ", "", coach))]

p <- ggplot(p25, aes(100 * peer_go, si)) +
  geom_point(colour = "grey60", size = 2.2, alpha = 0.7) +
  geom_point(data = lab_pts, colour = "#D55E00", size = 3.2) +
  geom_text_repel(data = lab_pts, aes(label = lab), size = 2.8, colour = "#a04300",
                  fontface = "bold", seed = 3, box.padding = 0.5, min.segment.length = 0,
                  max.overlaps = 20) +
  scale_y_log10(labels = comma) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  labs(title = "The comedy metric against the sober one: every 2025-26 punt",
       subtitle = paste0("Across: the peer test (share of other coaches who went for it in that exact spot). Up: Jon Bois's Surrender Index, the internet's\n",
                         "deliberately arbitrary punt-cowardice score, on a stretched scale. Top-right = punts both metrics despise. The comedy formula\n",
                         "cubes the late-game clock, so its tallest scores are desperation punts nobody disputes; the peer test is immune to that."),
       x = "share of other coaches who went for it in the same spot",
       y = "Surrender Index (cowardice score, stretched scale)",
       caption = fig_caption(
         "nflverse play data; Surrender Index computed with the public Surrender-Index bot formula; peer spots bucketed by distance, field zone, score state, quarter",
         "\nAll 2025-26 punts, regular season and playoffs. The two metrics agree on the headliners and disagree on purpose everywhere else:\nthe Surrender Index only sees punts and worships the clock; the peer test sees every kick and ignores the clock beyond the quarter. Built by R/62.")) +
  theme_coach(grid = "none")
save_fig("docs/figures/surrender_cross.png", p, w = 11.5, h = 7.8)
cat("\nOut: surrender_cross.png\n")
