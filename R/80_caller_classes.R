# =============================================================================
# 80_caller_classes.R -- the simple play-caller board Michael asked for, split
# by class year.
#
# The asks, verbatim: "I think a very very easy way to look at OCs and DCs is
# just ones with 5 Yrs+ avg EPA ... I call out the flaws ... but it's something
# everyone will still look at and enjoy", and "break it down by freshman,
# sophomores and upper classman".
#
# Window: 2012-2025 (nflverse play-by-play), because the charting window is only
# four seasons and nobody would reach five.
#
# Classes are seasons of play-calling experience in the window, not
# the coach's age or his time in the league:
#   Freshmen      1 or 2 seasons calling plays
#   Sophomores    3 or 4
#   Upperclassmen 5 or more   <- Michael's 5+ group
#
# Two boards, offense and defense, EPA per play, one row per caller, coloured
# by class. Defensive EPA is flipped so that up is good on both.
#
# THE FLAWS, on the chart, because they are the whole caveat:
#   1. This is not adjusted for anything. A caller with a franchise
#      quarterback outranks a caller without one, every time.
#   2. Upperclassmen are survivors. A caller only reaches five seasons if the
#      results were good, so the class averages are not comparable: freshmen
#      include everyone, upperclassmen include only the ones who lasted.
#   3. Play-caller attribution is by season and team, so a coordinator who
#      shares the call is credited with all of it.
#
# Out: docs/figures/caller_classes.png, data/derived/caller_classes.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel) })
source("R/lib/theme_coach.R")
NFLA <- "/Users/nick/stranger9977/nfl-analysis/data"
YRS <- 2012:2025

pbp <- rbindlist(lapply(YRS, function(y)
  fread(file.path(NFLA, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("season", "week", "posteam", "defteam", "play_type", "epa", "season_type"), showProgress = FALSE)))
pbp <- pbp[season_type == "REG" & play_type %in% c("pass", "run") & !is.na(epa) & !is.na(posteam)]
pc <- fread(file.path(NFLA, "playcallers.csv"), select = c("season", "week", "team", "off_play_caller", "def_play_caller"))
pc[, `:=`(off_play_caller = trimws(off_play_caller), def_play_caller = trimws(def_play_caller))]
o <- merge(pbp, pc[, .(season, week, team, off_play_caller)], by.x = c("season", "week", "posteam"), by.y = c("season", "week", "team"))
dfn <- merge(pbp, pc[, .(season, week, team, def_play_caller)], by.x = c("season", "week", "defteam"), by.y = c("season", "week", "team"))
off <- o[off_play_caller != "", .(caller = off_play_caller, season, epa)]
def <- dfn[def_play_caller != "", .(caller = def_play_caller, season, epa = -epa)]
cat(sprintf("plays: %d offensive rows, %d defensive rows, %d seasons\n", nrow(off), nrow(def), uniqueN(off$season)))

board <- function(x, side) {
  s <- x[, .(plays = .N, epa = mean(epa)), by = .(caller, season)]
  c <- s[, .(seasons = .N, plays = sum(plays), epa = weighted.mean(epa, plays),
             first = min(season), last = max(season)), by = caller][plays >= 1500]
  c[, class := fifelse(seasons >= 5, "Upperclassmen (5+ seasons)",
              fifelse(seasons >= 3, "Sophomores (3-4)", "Freshmen (1-2)"))]
  c[, side := side][]
}
b <- rbind(board(off, "Offense: EPA per play"), board(def, "Defense: EPA per play allowed, flipped"))
b[, class := factor(class, levels = c("Freshmen (1-2)", "Sophomores (3-4)", "Upperclassmen (5+ seasons)"))]
setorder(b, side, -epa)
b[, rank_in_side := seq_len(.N), by = side]
b[, rank_in_class := seq_len(.N), by = .(side, class)]
write_csv(as.data.frame(b), "data/derived/caller_classes.csv")

cat("class sizes and averages:\n")
print(b[, .(callers = .N, mean_epa = round(mean(epa), 4), median_seasons = as.numeric(median(seasons))), by = .(side, class)][order(side, class)])
cat("\nUpperclassmen, offense (Michael's 5+ board):\n")
print(b[side == "Offense: EPA per play" & class == "Upperclassmen (5+ seasons)", .(caller, seasons, plays, epa = round(epa, 4))][order(-epa)])
cat("\nUpperclassmen, defense:\n")
print(b[side != "Offense: EPA per play" & class == "Upperclassmen (5+ seasons)", .(caller, seasons, plays, epa = round(epa, 4))][order(-epa)])
cat("\nTop 5 of each class, offense:\n")
print(b[side == "Offense: EPA per play" & rank_in_class <= 5, .(class, caller, seasons, epa = round(epa, 4))][order(class, -epa)])

lab_keep <- b[rank_in_class <= 3 | caller %in% c("Ben Johnson", "Kyle Shanahan", "Sean McVay", "Andy Reid", "Mike Macdonald", "Vic Fangio")]
p <- ggplot(b, aes(x = epa, y = reorder(caller, epa), colour = class)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(aes(size = plays), alpha = 0.9) +
  geom_text_repel(data = lab_keep, aes(label = caller), size = 2.9, segment.colour = "grey75", max.overlaps = 40,
                  direction = "y", nudge_x = 0.012, hjust = 0, show.legend = FALSE) +
  facet_grid(class ~ side, scales = "free", space = "free_y") +
  scale_colour_manual(values = c("Freshmen (1-2)" = "grey60", "Sophomores (3-4)" = "#2B8CBE",
                                 "Upperclassmen (5+ seasons)" = "#1B7837"), name = NULL) +
  scale_size_continuous(range = c(1.2, 3.6), guide = "none") +
  scale_x_continuous(labels = label_number(style_positive = "plus", accuracy = 0.05)) +
  labs(title = "Play-callers by class: freshmen, sophomores and upperclassmen, EPA per play, nothing adjusted",
       subtitle = paste0("Every caller with 1,500+ plays, 2012-2025. Class is how many seasons he has called plays in the window, so ",
                         b[class == "Upperclassmen (5+ seasons)" & side == "Offense: EPA per play", .N], " offensive\n",
                         "and ", b[class == "Upperclassmen (5+ seasons)" & side != "Offense: EPA per play", .N],
                         " defensive callers make the 5-plus group. Defense is flipped so up is good on both sides. Dot size is snaps."),
       x = "EPA per play", y = NULL,
       caption = fig_caption("nflverse play-by-play 2012-2025 regular seasons; play-caller attribution from the nflverse playcallers file",
         paste0("\nThree flaws worth saying out loud. Nothing here is adjusted, so a caller with a franchise quarterback outranks one without, every time. Upperclassmen are\n",
                "survivors: a caller reaches five seasons only if the results were good, so the classes are not comparable. And the call is credited to one name per team-season.\n",
                "Built by R/80."))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left", axis.text.y = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0, size = 10))
save_fig("docs/figures/caller_classes.png", p, w = 13, h = 10)
cat("\nOut: caller_classes.png, data/derived/caller_classes.csv\n")
