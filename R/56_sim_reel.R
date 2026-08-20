# =============================================================================
# 56_sim_reel.R -- the sim-pressure film card: every charted Seattle sim from
# the Super Bowl and the San Francisco games, clock-stamped for clipping.
#
# The ask, verbatim: "You have a SIM pressure from the Super Bowl or Niners
# game? Like one that was clicked in the Sumur data as SIM"
#
# Sim = four rushers, at least one a linebacker or defensive back, no blitz
# (the same classification as R/49; Sumer charts who rushed, not the label).
#
# Out: docs/figures/sim_reel.png
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2025)
d <- d[def_team == "SEA" & is_dropback == TRUE & (season_type == 1 | off_team == "SF")]
pp <- rbindlist(list(fread("data/raw/sumer/plays_players_p1.csv.gz"),
                     fread("data/raw/sumer/plays_players_p2.csv.gz")), fill = TRUE)
pd <- fread("data/raw/sumer/player_details.csv.gz")
pd[, player_name := paste(football_name, last_name)]
ru <- pp[role == "PASS RUSH" & sumer_play_id %in% d$sumer_play_id]
ru <- merge(ru, pd[, .(sumer_player_id, position, player_name)], by = "sumer_player_id", all.x = TRUE)
agg <- ru[, .(n_rush = .N,
              n_2nd = sum(position %in% c("IB","OB","DC","DS"), na.rm = TRUE),
              n_unk = sum(is.na(position) | !position %in% c("DE","DT","IB","OB","DC","DS")),
              second_rushers = paste(player_name[position %in% c("IB","OB","DC","DS")], collapse = " + ")),
          by = sumer_play_id]
d2 <- merge(d, agg, by = "sumer_play_id")
sims <- d2[schemed_blitz == TRUE & blitz == FALSE]
sims[, second_lvl := fifelse(n_2nd >= 1 & n_unk == 0, second_rushers, "")]
sims[, game := fifelse(season_type == 1, "SUPER BOWL vs New England", "Week 1 vs San Francisco")]
setorder(sims, -season_type, quarter, -clock)
sims[, row_id := .I]
cat(sprintf("sims found: %d\n", nrow(sims)))
print(sims[, .(game, quarter, clock, down, distance, second_rushers, epa = round(expected_points_added, 2))])

tab <- sims[, .(row_id, game,
                q = paste0("Q", quarter, "  ", clock),
                dd = sprintf("%d & %d", down, distance),
                who = fifelse(second_lvl != "", paste(second_lvl, "rushes"), "front four rushes, the show backs out"),
                result = sprintf("%+.1f pts for the offense%s", expected_points_added,
                                 fifelse(expected_points_added < 0, "  (defense wins)", "")))]
tab[, y := -row_id - fifelse(game == "Week 1 vs San Francisco", 1.2, 0)]

hdr <- tab[, .(y = max(y) + 0.9), by = game]

p <- ggplot(tab, aes(y = y)) +
  geom_text(data = hdr, aes(x = 0, y = y, label = game), hjust = 0, size = 3.5,
            fontface = "bold", colour = "#D55E00") +
  geom_text(aes(x = 0.02, label = q), hjust = 0, size = 3, colour = "grey45") +
  geom_text(aes(x = 0.16, label = dd), hjust = 0, size = 3, colour = "grey45") +
  geom_text(aes(x = 0.25, label = who), hjust = 0, size = 3.15, fontface = "bold", colour = "#1d6a99") +
  geom_text(aes(x = 0.66, label = result), hjust = 0, size = 3, colour = "grey30") +
  scale_x_continuous(limits = c(0, 1.02)) +
  scale_y_continuous(limits = c(min(tab$y) - 1, 1.6)) +
  labs(title = "Every Seattle sim pressure Sumer charted: showed a blitz, sent four or fewer",
       subtitle = paste0("Sim here = the charted show-versus-send: the defense showed five or more rushers before the snap and sent four or fewer.\n",
                         "Where a linebacker or defensive back was among the four, he is named. All five Super Bowl sims won, the late 4th-and-4 stop the biggest."),
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play and player charting, 2025-26 season",
         "\nThis is the charting's own show-versus-send flag, the closest thing in the data to a clicked \"sim\". The who-rushed version (a second-level player\namong four rushers) overlaps on most rows and is the definition behind the sim-pressure rate chart. None sprang a formally charted\nunblocked rusher; the damage came through hurried throws. Built by R/56.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/sim_reel.png", p, w = 10.5, h = 4.6)
cat("\nOut: docs/figures/sim_reel.png\n")
