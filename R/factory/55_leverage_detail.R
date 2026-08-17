# =============================================================================
# factory/55_leverage_detail.R -- the detailed high-leverage breakdown.
#
# Nick: "flesh out high leverage stuff big swings in epa and win prob added
# playoffs late game late half late down all that is super interesting i want
# it to be very detailed there for each coach but also high level league wide
# historical break downs."
#
# Six situations, defined once here and used everywhere so the numbers on the
# page always mean the same thing:
#
#   All plays        every called play, the baseline
#   Late down        third and fourth down, where a drive lives or dies
#   Late half        inside two minutes of either half
#   Late game        fourth quarter inside four minutes
#   High leverage    top quartile of 4 x wp x (1 - wp)
#   Playoffs         postseason
#   Big swings       plays where |WPA| lands in the top 5%
#
# ONE OF THESE IS NOT LIKE THE OTHERS, and it would have produced a false
# headline if left alone. "Big swings" is defined by |WPA|, which is measured
# AFTER the play, so it selects on outcome rather than on situation. It is
# therefore dominated by plays that worked: +0.268 EPA and a 61.7% success
# rate against -0.008 and 43.2% overall. That is not offenses being good in
# big moments, it is the definition of a big moment including the touchdown.
# It is kept because it answers a real question, which snaps actually decide
# games, but it is excluded from every performance comparison because
# comparing it to the others would be circular.
#
# Two things are deliberately kept apart. The LEAGUE-WIDE historical view asks
# how the sport has changed. The PER-COACH view asks who is good in which
# moment. The second is much noisier than the first and is labeled as such:
# a coach's playoff sample is tiny, and the chart says so rather than letting
# a rank imply precision it does not have.
#
# Out: docs/figures/factory/leverage_history.png
#      docs/figures/factory/leverage_situations.png
#      docs/figures/factory/coach_situations.png
#      data/factory/leverage_detail.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
d <- merge(mt, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
d[, coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
d <- d[!is.na(coach) & coach != "" & !is.na(epa)]

# ---------------------------------------------------------------- situations
swing_cut <- quantile(abs(d$wpa), 0.95, na.rm = TRUE)
d[, `:=`(
  s_late_down = down >= 3,
  s_late_half = half_seconds_remaining <= 120,
  s_late_game = qtr == 4 & game_seconds_remaining <= 240,
  s_high_lev  = leverage >= quantile(d$leverage, 0.75, na.rm = TRUE),
  s_playoff   = season_type == "POST",
  s_big_swing = abs(wpa) >= swing_cut
)]
cat(sprintf("big-swing threshold: |WPA| >= %.3f (top 5%% of plays)\n", swing_cut))

SITS <- c("All plays" = NA, "Late down" = "s_late_down", "Late half" = "s_late_half",
          "Late game" = "s_late_game", "High leverage" = "s_high_lev",
          "Playoffs" = "s_playoff", "Big swings" = "s_big_swing")

sit_rows <- function(dd) rbindlist(lapply(names(SITS), function(nm) {
  col <- SITS[[nm]]
  x <- if (is.na(col)) dd else dd[get(col) == TRUE]
  if (!nrow(x)) return(NULL)
  data.table(situation = nm, plays = nrow(x),
             epa = mean(x$epa, na.rm = TRUE),
             wpa = mean(x$wpa, na.rm = TRUE),
             success = mean(x$success, na.rm = TRUE))
}))

# ---------------------------------------------------------------- league history
hist <- rbindlist(lapply(sort(unique(d$season)), function(s) {
  r <- sit_rows(d[season == s]); r[, season := s][]
}))
write_csv(as.data.frame(hist), "data/factory/leverage_detail.csv")
cat("\n=== league-wide, by situation, pooled ===\n")
print(sit_rows(d)[, .(situation, plays = format(plays, big.mark=","),
                      epa = round(epa,4), wpa = round(wpa,5), success = round(success,3))])

hist2 <- hist[situation %in% c("All plays","Late down","Late game","High leverage","Big swings")]
p1 <- ggplot(hist2, aes(season, success, colour = situation)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  geom_text_repel(data = hist2[season == max(season)], aes(label = situation),
                  hjust = 0, nudge_x = 0.15, direction = "y", size = 3.1,
                  fontface = "bold", segment.colour = NA, seed = 2) +
  scale_colour_manual(values = c("All plays" = "#7f97a8", "Late down" = "#2B8CBE",
                                 "Late game" = "#D55E00", "High leverage" = "#1c7a43",
                                 "Big swings" = "#8a3d00")) +
  scale_x_continuous(breaks = seq(2015, 2025, 2), expand = expansion(mult = c(0.02, 0.24))) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The penalty for pressure has not shrunk in a decade",
    subtitle = "Success rate by situation, 2015 to 2025",
    x = NULL, y = "success rate",
    caption = fig_caption(
      "nflverse play-by-play 2015 to 2025, all called plays",
      "Late down is third and fourth. Late game is the last four minutes of the fourth quarter. High leverage is the top quartile of 4 x wp x (1 - wp). Big swings are the top 5% of plays by absolute win-probability change.",
      paste0("\nThe gap between the grey line and the late-down, late-game and high-leverage lines is the price of pressure, and it has stayed roughly constant since 2015.\n",
             "The big-swing line sits far above everything because it is selected on outcome: a play only lands in the top 5% of win-probability change if something happened,\n",
             "so that series describes which snaps decide games rather than how well anyone played. Built by R/factory/55."))
  ) +
  theme_coach(grid = "y") + theme(legend.position = "none",
                                  plot.margin = margin(10, 30, 8, 10))
save_fig("docs/figures/factory/leverage_history.png", p1, w = 12, h = 6.4)

# ---------------------------------------------------------------- situation bars
pooled <- sit_rows(d)[situation != "Big swings"]   # outcome-selected, see header
pooled[, situation := factor(situation, levels = rev(setdiff(names(SITS), "Big swings")))]
p2 <- ggplot(pooled, aes(epa, situation)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(aes(fill = epa > 0), width = 0.66) +
  geom_text(aes(label = sprintf("%+.3f   n=%s", epa, format(plays, big.mark = ",")),
                hjust = ifelse(epa > 0, -0.06, 1.06)),
            size = 3.2, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#D55E00"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.22, 0.22))) +
  labs(
    title = "Offense collapses when the game is actually on the line",
    subtitle = "EPA per play by situation, pooled 2015 to 2025",
    x = "EPA per play", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play 2015 to 2025, all called plays",
      "Situations overlap by design: a play can be late-game and high-leverage at once. Big-swing plays are excluded here because they are selected on outcome.",
      paste0("\nThe ordering is the point. Offenses are mildly positive on an average snap and sharply negative on third and fourth down and in the last four minutes of a game.\n",
             "Some of that is the situation selecting for hard downs, and some is that defenses can play simpler when the field shrinks and the clock is short.\n",
             "Playoffs run positive because the teams that get there are better, which is a selection effect rather than a statement about January. Built by R/factory/55."))
  ) +
  theme_coach(grid = "none")
save_fig("docs/figures/factory/leverage_situations.png", p2, w = 11.5, h = 6)

# ---------------------------------------------------------------- per coach
NAMED <- c("Bill Belichick","Andy Reid","Mike Tomlin","Sean Payton","John Harbaugh",
           "Sean McVay","Kyle Shanahan","Mike Macdonald","Dan Campbell","Matt LaFleur",
           "Sean McDermott","Nick Sirianni","Pete Carroll","Kevin Stefanski",
           "Josh McDaniels","Matt Patricia","Brandon Staley","Adam Gase")
gc <- unique(d[, .(game_id, coach)])[, .(g = .N), by = coach]
per <- rbindlist(lapply(NAMED, function(cn) {
  x <- d[coach == cn]; if (!nrow(x)) return(NULL)
  r <- sit_rows(x); r[, coach := cn][]
}))
per <- merge(per, gc, by = "coach")
lg <- sit_rows(d)[, .(situation, lg_epa = epa)]
per <- merge(per, lg, by = "situation")
per[, edge := epa - lg_epa]
per[, situation := factor(situation, levels = names(SITS))]
per <- per[situation != "Big swings"]   # outcome-selected, not comparable
ord <- per[situation == "High leverage"][order(edge)]$coach
per[, coach := factor(coach, levels = ord)]

p3 <- ggplot(per[plays >= 50 & situation != "Big swings"], aes(situation, coach, fill = pmax(pmin(edge, 0.25), -0.25))) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%+.02f", edge)), size = 2.8, fontface = "bold",
            colour = "grey15") +
  scale_fill_gradient2(low = "#D55E00", mid = "#f4f2ef", high = "#2B8CBE",
                       midpoint = 0, limits = c(-0.25, 0.25), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(
    title = "Which coaches hold up when it matters, and which do not",
    subtitle = "Offensive EPA per play above or below the league in each situation. Blue is better than league, orange is worse.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse play-by-play 2015 to 2025; head coaches, offense only, cells with at least 50 plays",
      "Rows are ordered by the high-leverage column. Situations overlap.",
      paste0("\nRead the left columns and the right columns differently. 'All plays' rests on tens of thousands of snaps per coach and is close to a settled number. 'Playoffs' and\n",
             "'Big swings' rest on a few hundred at most, so a good cell there is weak evidence on its own and a coach can move several places on one January afternoon.\n",
             "The useful signal is a coach whose whole row leans the same way. Built by R/factory/55."))
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(size = rel(0.82), face = "bold", angle = 20, hjust = 0),
        axis.text.y = element_text(size = rel(0.88)))
save_fig("docs/figures/factory/coach_situations.png", p3, w = 11, h = 8.4)

cat("\n=== per-coach high-leverage edge, best and worst ===\n")
hl <- per[situation == "High leverage"][order(-edge)]
print(head(hl[, .(coach, plays, epa = round(epa,4), edge = round(edge,4))], 6))
print(tail(hl[, .(coach, plays, epa = round(epa,4), edge = round(edge,4))], 6))
