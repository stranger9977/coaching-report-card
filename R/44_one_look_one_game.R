# =============================================================================
# 44_one_look_one_game.R -- the single-game case study of "same look,
# different plays."
#
# The ask, verbatim: "For number 3...can you give me like a single game
# where they run the same formation several times, but run different plays
# out of it"
#
# THE SHOWCASE. Scanned every McVay game 2022-25 for (game x look) with the
# most distinct plays from one look (look = personnel | receiver split | QB
# alignment, the same readable look as R/43; play = run concept + side for
# runs, protection action + depth for passes). The winner: 2022 week 13 at
# Seattle, the look "11 personnel | 2x2 | under center", shown 28 times,
# 15 DIFFERENT plays called out of it. Film-ready alternates printed too,
# including a current-era one (2025 week 9 vs New Orleans: his 13-personnel
# look, 20 snaps, 11 different plays).
#
# Output is a film card: every snap of the showcase look in that game, in
# order, with quarter, clock, down and distance, the play, and the result,
# so the game can be pulled and clipped directly.
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season spans in rendered text, plain language.
#
# Out: docs/figures/one_look_one_game.png
#      data/derived/one_look_one_game.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & off_caller == "Sean McVay" & run_pass %in% c("P", "R")]
d <- d[offensive_personnel_basic != "" & formation != "" & quarterback_alignment != ""]
d[, look := sprintf("%s personnel | %s | %s", offensive_personnel_basic,
                    formation, tolower(quarterback_alignment))]

# readable, film-ready play label
d[, depth_b := fifelse(is.na(depth_of_target), "throw",
              fifelse(depth_of_target < 0, "behind the line",
              fifelse(depth_of_target < 10, "short",
              fifelse(depth_of_target < 20, "intermediate", "deep"))))]
d[, play_lbl := fifelse(
    run_pass == "R",
    trimws(paste(tolower(run_concept),
                 tolower(fifelse(is.na(run_gap_intent_side) | run_gap_intent_side == "",
                                 "", run_gap_intent_side)))),
    trimws(paste(fifelse(play_action == TRUE, "play-action", "dropback"),
                 fifelse(screen == TRUE, "screen", depth_b))))]

# the league of candidates, printed for the record
cand <- d[, .(snaps = .N, plays = uniqueN(play_lbl)),
          by = .(season, week, def_team, look)][snaps >= 8][order(-plays, -snaps)]
cat("top single-game same-look candidates:\n"); print(cand[1:8])

SHOW <- list(season = 2022L, week = 13L, look = "11 personnel | 2x2 | under center")
g <- d[season == SHOW$season & week == SHOW$week & look == SHOW$look]
setorder(g, quarter, -clock)
g[, snap_no := .I]
g[, dd := sprintf("%s & %s", down, distance)]
g[, result := paste0(fifelse(expected_points_added >= 0, "+", ""),
                     sprintf("%.1f", expected_points_added),
                     fifelse(is_touchdown == TRUE, "  TD",
                     fifelse(rushing_first_down == TRUE | passing_first_down == TRUE, "  first down", "")))]
cat(sprintf("\nshowcase: 2022 wk13 at SEA, %d snaps, %d distinct plays\n",
            nrow(g), uniqueN(g$play_lbl)))

write_csv(as.data.frame(g[, .(snap_no, quarter, clock, down, distance, play_lbl,
                              run_pass, epa = expected_points_added)]),
          "data/derived/one_look_one_game.csv")

# ---------------------------------------------------------------- film card
tab <- g[, .(snap_no, q = paste0("Q", quarter, "  ", clock), dd, play_lbl,
             result, run_pass)]
tab[, y := -snap_no]
n_plays <- uniqueN(g$play_lbl)

p <- ggplot(tab, aes(y = y)) +
  geom_text(aes(x = 0.00, label = q), hjust = 0, size = 2.85, colour = "grey45") +
  geom_text(aes(x = 0.16, label = dd), hjust = 0, size = 2.85, colour = "grey45") +
  geom_text(aes(x = 0.26, label = play_lbl,
                colour = fifelse(run_pass == "R", "run", "pass")),
            hjust = 0, size = 3.05, fontface = "bold") +
  geom_text(aes(x = 0.72, label = result), hjust = 0, size = 2.85, colour = "grey30") +
  annotate("text", x = c(0.00, 0.16, 0.26, 0.72), y = 0,
           label = c("QUARTER, CLOCK", "DOWN & DIST", "THE PLAY (runs red, passes blue)", "RESULT (points added)"),
           hjust = 0, size = 2.6, fontface = "bold", colour = "grey55") +
  scale_colour_manual(values = c(run = "#C0504D", pass = "#2B8CBE"), guide = "none") +
  scale_x_continuous(limits = c(0, 0.95)) +
  scale_y_continuous(limits = c(min(tab$y) - 1, 1)) +
  labs(title = "One game, one look, fifteen different plays",
       subtitle = paste0(
         "Rams at Seattle, 2022-23 season, week 13. McVay showed the exact same look 28 times: 11 personnel, receivers 2x2, quarterback under center.\n",
         "Out of that one picture he called 15 different plays. This is what \"same formation, different plays\" looks like on a single film reel."),
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play charting",
         "\nLook = personnel group x receiver split x QB alignment. A play = run concept plus side for runs; protection action plus throw depth for passes.",
         paste0("\nThat labeling UNDERCOUNTS variety: two different deep concepts read as one row here. Result is points added on the play, with first downs and touchdowns marked.\n",
                "Current-era alternate for the same exercise: 2025-26 week 9 against New Orleans, his 13-personnel | 2x2 | under-center look, 20 snaps, 11 different plays.\n",
                "Another: the 2024-25 week 14 shootout at Buffalo, 11 personnel | 1x3 | under center, 16 snaps, 12 different plays. Built by R/44."))) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(lineheight = 1.15))

save_fig("docs/figures/one_look_one_game.png", p, w = 11, h = 12)
cat("\nOut: docs/figures/one_look_one_game.png, data/derived/one_look_one_game.csv\n")
