# =============================================================================
# 67_mcvay_personnel_history.R -- the McVay 11/13 personnel chart, pushed back
# to the start of the participation data.
#
# The asks, verbatim, on R/factory/60's chart: "can we go further back?
# Curious how much they ran it in their Super Bowl winning season" and "They
# were a league leading 90% according to Robert Mays the year they lost the
# Super Bowl."
#
# nflverse participation charting starts in 2016, which catches his last
# season calling plays in Washington, then every Rams season from 2017. Marks
# his two Super Bowl seasons: 2018 (lost to New England) and 2021 (beat
# Cincinnati).
#
# Same definitions as R/factory/60: called plays (pass or rush), play-callers
# with at least 300 charted plays in a season, personnel read as RB count +
# TE count from the participation string.
#
# Out: docs/figures/mcvay_reinvention_career.png
#      data/derived/mcvay_personnel_career.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
D <- "/Users/nick/stranger9977/nfl-analysis/data"
YRS <- 2016:2025

pbp <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("game_id", "play_id", "season", "week", "season_type", "posteam", "play_type", "epa"),
        showProgress = FALSE)))
pbp <- pbp[season_type == "REG" & play_type %in% c("pass", "run") & !is.na(posteam)]
part <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id", "play_id", "offense_personnel"), showProgress = FALSE)))
setnames(part, "nflverse_game_id", "game_id")
pc <- fread(file.path(D, "playcallers.csv"), select = c("season", "week", "team", "off_play_caller"))
pc[, off_play_caller := trimws(off_play_caller)]

d <- merge(pbp, part, by = c("game_id", "play_id"))
d <- merge(d, pc, by.x = c("season", "week", "posteam"), by.y = c("season", "week", "team"), all.x = TRUE)
d <- d[!is.na(offense_personnel) & offense_personnel != "" & !is.na(off_play_caller) & off_play_caller != ""]
pers <- function(x) {
  rb <- suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", x))); rb[is.na(rb)] <- 1L
  te <- suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", x))); te[is.na(te)] <- 1L
  paste0(rb, te)
}
d[, p := pers(offense_personnel)]

cs <- d[, .(n = .N, p11 = 100 * mean(p == "11"), p13 = 100 * mean(p == "13"),
            epa = mean(epa, na.rm = TRUE)), by = .(off_play_caller, season)][n >= 300]
cs[, `:=`(rank11 = frank(-p11), rank13 = frank(-p13), callers = .N), by = season]
mv <- cs[off_play_caller == "Sean McVay"][order(season)]
cat("=== McVay by season (rank among callers with 300+ plays) ===\n")
print(mv[, .(season, plays = n, p11 = round(p11, 1), rank11, p13 = round(p13, 1), rank13, of = callers, epa = round(epa, 3))])
lg <- d[, .(lg11 = 100 * mean(p == "11"), lg13 = 100 * mean(p == "13")), by = season][order(season)]
cat("league:\n"); print(lg[, .(season, lg11 = round(lg11, 1), lg13 = round(lg13, 1))])
write_csv(as.data.frame(merge(mv, lg, by = "season")), "data/derived/mcvay_personnel_career.csv")

long <- rbind(
  mv[, .(season, grp = "Sean McVay, 11 personnel", v = p11)],
  mv[, .(season, grp = "Sean McVay, 13 personnel", v = p13)],
  lg[season >= min(mv$season), .(season, grp = "League, 11 personnel", v = lg11)],
  lg[season >= min(mv$season), .(season, grp = "League, 13 personnel", v = lg13)]
)
pal <- c("Sean McVay, 11 personnel" = "#2B8CBE", "Sean McVay, 13 personnel" = "#D55E00",
         "League, 11 personnel" = "#9db6c9", "League, 13 personnel" = "#E8A33D")
ends <- long[season == max(season)]
sb <- data.table(season = c(2016, 2018, 2021),
                 lab = c("calling plays in Washington", "lost the Super Bowl", "won the Super Bowl"))
sb <- merge(sb, mv[, .(season, v = p11, r = rank11)], by = "season")
sb[, lab := sprintf("%s\n%.0f%% 11 personnel, %s in the league", lab, v,
                    fifelse(r == 1, "1st", fifelse(r == 2, "2nd", fifelse(r == 3, "3rd", paste0(r, "th")))))]
sb[season == 2016, lab := sub("\n.*", "", lab)]

p1 <- ggplot(long, aes(season, v, colour = grp)) +
  geom_vline(data = sb, aes(xintercept = season), colour = "grey85", linewidth = 0.6, linetype = "22") +
  geom_line(aes(linewidth = grepl("McVay", grp))) +
  geom_point(data = long[grepl("McVay", grp)], size = 2.2) +
  geom_text(data = sb, aes(x = season, y = fifelse(season == 2016, v + 6, 103), label = lab), inherit.aes = FALSE,
            size = 3, colour = "grey35", lineheight = 0.95, vjust = 0) +
  geom_text_repel(data = ends, aes(label = grp), hjust = 0, direction = "y",
                  nudge_x = 0.2, size = 3.2, fontface = "bold", seed = 3,
                  segment.colour = NA, xlim = c(2025.2, NA)) +
  scale_colour_manual(values = pal) +
  scale_linewidth_manual(values = c("TRUE" = 1.2, "FALSE" = 0.7), guide = "none") +
  scale_x_continuous(breaks = min(mv$season):2025, expand = expansion(mult = c(0.03, 0.3))) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 112), breaks = seq(0, 100, 25)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "A decade of 11 personnel, then McVay tore up his own offense in one off-season",
    subtitle = "Share of plays from 11 personnel (1 back, 1 tight end) and 13 personnel (1 back, 3 tight ends), every season he has called plays in the charting era",
    x = NULL, y = NULL,
    caption = fig_caption(
      sprintf("nflverse participation personnel groupings, %d to 2025", min(mv$season)),
      "Play-callers with at least 300 charted plays in a season.",
      paste0(sprintf("\nHe led the league in 11 personnel in %s of his %s seasons as a caller, including both Super Bowl years (93%% in 2018, 86%% in 2021),",
                     sum(mv$rank11 == 1), nrow(mv)),
             sprintf("\nand never ran it under %.0f%% until 2025, when he dropped to %.0f%% and put %.0f%% of his plays in 13 personnel, ",
                     min(mv[season < 2025]$p11), mv[season == 2025]$p11, mv[season == 2025]$p13),
             sprintf("the most extreme in football against a league average of %.0f%%.\n2016 is his last season as Washington's coordinator; 2017 on is the Rams. Built by R/67.", lg[season == 2025]$lg13)))
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "none", plot.margin = margin(10, 30, 10, 10))
save_fig("docs/figures/mcvay_reinvention_career.png", p1, w = 12, h = 6.75)
cat("\nOut: docs/figures/mcvay_reinvention_career.png\n")
