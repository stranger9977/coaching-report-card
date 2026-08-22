# =============================================================================
# 65_niners_reel.R -- the film-ready run of consecutive Niners plays.
#
# The ask, verbatim, pointing at R/64's look-then-strike chart: "Also for
# this...do you have like a run of Niners plays in a row I can use". So: one
# game, one unbroken run of snaps in Shanahan's signature 21 personnel, with
# a different play coming out of the same grouping snap after snap.
#
# THE SHOWCASE. Scanned every Shanahan game 2022-2025 for the longest
# consecutive-snap 21-personnel runs (R/64's streak logic exactly: offensive
# snaps in game order, streak broken the moment the personnel group changes).
# The winner: 2025 week 17 against Chicago, NINE straight snaps of 21
# personnel with EIGHT different plays called, +3.3 points added over the
# run. A bonus for the video: the opponent is Ben Johnson's Bears, the other
# caller on the R/64 chart. Runner-up, printed with the candidates: 2025
# week 5 against the Rams, 7 straight for +6.0.
#
# Output is a film card: every snap of the run, in order, with quarter,
# clock, down and distance, drive breaks marked, the play, and the result,
# so the game can be pulled and clipped directly.
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season spans in rendered text, plain language.
#
# Out: docs/figures/niners_reel.png
#      data/derived/niners_reel.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & off_caller == "Kyle Shanahan" & run_pass %in% c("P", "R") &
       offensive_personnel_basic != ""]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, quarter, -clock_s)
d[, gm := paste(season, week)]
d[, new_grp := offensive_personnel_basic != shift(offensive_personnel_basic, fill = "X"), by = gm]
d[, streak_id := cumsum(new_grp), by = gm]

# readable, film-ready play label, exactly as R/44
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
cand <- d[offensive_personnel_basic == "21",
          .(snaps = .N, plays = uniqueN(play_lbl),
            epa = round(sum(expected_points_added, na.rm = TRUE), 1)),
          by = .(season, week, def_team, streak_id)][snaps >= 5][order(-snaps, -plays)]
cat("top consecutive-21-personnel candidates:\n"); print(cand[1:8])

g <- d[season == 2025 & week == 17 & def_team == "CHI" & offensive_personnel_basic == "21"]
g <- g[streak_id == cand[season == 2025 & week == 17 & def_team == "CHI"][1]$streak_id]
setorder(g, quarter, -clock_s)
g[, snap_no := .I]
g[, dd := sprintf("%s & %s", down, distance)]
g[, drv := fifelse(game_drive_number != shift(game_drive_number, fill = -1),
                   sprintf("drive %d", game_drive_number), "")]
g[, result := paste0(fifelse(expected_points_added >= 0, "+", ""),
                     sprintf("%.1f", expected_points_added),
                     fifelse(is_touchdown == TRUE, "  TD",
                     fifelse(rushing_first_down == TRUE | passing_first_down == TRUE, "  first down", "")))]
cat(sprintf("\nshowcase: 2025 wk17 vs CHI, %d consecutive 21-personnel snaps, %d distinct plays, drives %s\n",
            nrow(g), uniqueN(g$play_lbl), paste(unique(g$game_drive_number), collapse = ", ")))

write_csv(as.data.frame(g[, .(snap_no, quarter, clock, down, distance, drive = game_drive_number,
                              play_lbl, run_pass, epa = expected_points_added)]),
          "data/derived/niners_reel.csv")

# ---------------------------------------------------------------- film card
tab <- g[, .(snap_no, q = paste0("Q", quarter, "  ", clock), dd, drv, play_lbl,
             result, run_pass)]
tab[, y := -snap_no]
n_plays <- uniqueN(g$play_lbl)
stopifnot(nrow(tab) == 9, n_plays == 8)  # hardcoded in the rendered text below

p <- ggplot(tab, aes(y = y)) +
  geom_text(aes(x = 0.00, label = q), hjust = 0, size = 2.85, colour = "grey45") +
  geom_text(aes(x = 0.15, label = dd), hjust = 0, size = 2.85, colour = "grey45") +
  geom_text(aes(x = 0.24, label = drv), hjust = 0, size = 2.6, colour = "grey55",
            fontface = "italic") +
  geom_text(aes(x = 0.34, label = play_lbl,
                colour = fifelse(run_pass == "R", "run", "pass")),
            hjust = 0, size = 3.05, fontface = "bold") +
  geom_text(aes(x = 0.76, label = result), hjust = 0, size = 2.85, colour = "grey30") +
  annotate("text", x = c(0.00, 0.15, 0.24, 0.34, 0.76), y = 0,
           label = c("QUARTER, CLOCK", "DOWN & DIST", "DRIVE",
                     "THE PLAY (runs red, passes blue)", "RESULT (points added)"),
           hjust = 0, size = 2.6, fontface = "bold", colour = "grey55") +
  scale_colour_manual(values = c(run = "#C0504D", pass = "#2B8CBE"), guide = "none") +
  scale_x_continuous(limits = c(0, 0.98)) +
  scale_y_continuous(limits = c(min(tab$y) - 1, 1)) +
  labs(title = "Nine straight snaps of 21 personnel, eight different plays",
       subtitle = paste0(
         "San Francisco against Chicago, week 17 of the 2025-26 season. The same personnel grouping, two backs and one tight end,\n",
         "stays on the field for nine consecutive offensive snaps, and eight different plays come out of it, worth +3.3 points in total.\n",
         "The give-them-a-look pattern from the streak chart, caught on film: show the grouping, then vary everything else."),
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play charting",
         "\nConsecutive offensive snaps with the same personnel group on the field, the exact streak definition behind the look-then-strike chart; a drive\nbreak is marked where the run spans possessions. A play = the run scheme and its side for runs; play action or not, plus throw depth, for passes.",
         "\nThat labeling UNDERCOUNTS variety: two different deep concepts read as one row here. Result is points added on the play, with first downs and\ntouchdowns marked. Runner-up run, if a second example helps: week 5 of the same season against the Rams, 7 straight in 21 personnel for +6.0. Built by R/65.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        plot.subtitle = element_text(lineheight = 1.15))

save_fig("docs/figures/niners_reel.png", p, w = 11, h = 3.2 + nrow(tab) * 0.31)
cat("\nOut: docs/figures/niners_reel.png, data/derived/niners_reel.csv\n")
