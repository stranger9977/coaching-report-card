# =============================================================================
# factory/90_report_card.R -- the greats and the WOATs.
#
# Nick: "i want kind of like a breakdown for all the coaching greats modern and
# legacy and all the coaching woats."
#
# THE RANKING PROBLEM, and how it is handled. Raw wins-above-market is
# unusable as a leaderboard: Liam Coen leads every coach in history at +3.85
# per 17 games on exactly one season, and Bill Belichick sits at +0.55 on 449
# games. One of those is a career and one is a coin landing heads four times.
#
# So every rate here is empirical-Bayes shrunk toward the league mean, using
# each coach's own standard error. A short career gets pulled hard toward zero
# and a long one barely moves. The shrunk number is what a bookmaker should
# believe about a coach, which is the right quantity for "who is actually
# great".
#
# Five dimensions, each scored where the data exists:
#   Beats the market   wins above the closing spread, shrunk    1999-2025
#   Fourth down        share of clear-cut calls got right       2018-2025
#   Offence            team EPA per play on offence             1999-2025
#   Defence            team EPA per play allowed                1999-2025
#   Talent squeeze     wins above what the roster's cost implies 2012-2025
#
# The dimensions are deliberately not averaged into one number. A single
# ranking would hide the interesting part, which is that the greats are great
# in different ways: Belichick's edge is defence and the market, Reid's is
# offence, and neither is good at fourth down.
#
# Out: docs/figures/factory/greats_woats.png
#      docs/figures/factory/report_card_grid.png
#      data/factory/report_card.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
cm <- as.data.table(read_csv("data/derived/coach_market.csv", show_col_types = FALSE))

# ---------------------------------------------------------------- shrinkage
# EB: posterior mean = (tau^2 * x) / (tau^2 + se^2). tau is the spread of TRUE
# coaching ability, estimated as the observed spread net of average noise.
x <- cm[games >= 16]
tau2 <- max(var(x$rate_per17) - mean(x$sd_rate_per17^2), 0.01)
cm[, shrunk := (tau2 * rate_per17) / (tau2 + sd_rate_per17^2)]
cat(sprintf("spread of true ability tau = %.3f wins/17; mean noise se = %.3f\n",
            sqrt(tau2), mean(x$sd_rate_per17)))
cat("\n--- what shrinkage does to the extremes ---\n")
print(cm[order(-rate_per17)][1:5, .(coach, games, raw = round(rate_per17,2), shrunk = round(shrunk,2))])
print(cm[order(rate_per17)][1:5, .(coach, games, raw = round(rate_per17,2), shrunk = round(shrunk,2))])

# ---------------------------------------------------------------- team-side EPA
gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
pbp <- merge(mt[, .(game_id, season, posteam, defteam, epa)], gm, by = "game_id")
off <- pbp[!is.na(epa), .(off_plays = .N, off_epa = mean(epa)),
           by = .(coach = fifelse(posteam == home_team, home_coach, away_coach))]
def <- pbp[!is.na(epa), .(def_plays = .N, def_epa = mean(epa)),
           by = .(coach = fifelse(defteam == home_team, home_coach, away_coach))]

# ---------------------------------------------------------------- fourth down
fd <- as.data.table(read_csv("data/derived/fourth_down_decisions.csv", show_col_types = FALSE))
fdc <- fd[, .(fd_n = .N, fd_obey = 100*mean(correct)), by = coach][fd_n >= 40]

# ---------------------------------------------------------------- talent squeeze
ct <- tryCatch(as.data.table(read_csv("data/derived/coach_vs_contracts.csv", show_col_types = FALSE)),
               error = function(e) NULL)

rc <- Reduce(function(a,b) merge(a,b,by="coach",all.x=TRUE),
             list(cm[, .(coach, games, first_season, last_season,
                         market_raw = rate_per17, market = shrunk, outside_cone)],
                  off[off_plays >= 500], def[def_plays >= 500], fdc))
rc <- rc[games >= 32]
if (!is.null(ct) && "coach" %in% names(ct)) {
  tc <- names(ct)[grepl("resid|above|squeeze", names(ct))][1]
  if (!is.na(tc)) rc <- merge(rc, ct[, .(coach, talent = get(tc))], by = "coach", all.x = TRUE)
}
write_csv(as.data.frame(rc), "data/factory/report_card.csv")
cat(sprintf("\nhead coaches with 32+ games: %d\n", nrow(rc)))

# ---------------------------------------------------------------- chart 1
setorder(rc, -market)
N <- 14
top <- rbind(head(rc, N)[, side := "The greats"], tail(rc, N)[, side := "The WOATs"])
top[, era := fifelse(last_season >= 2020, "still coaching recently", "legacy")]
top[, lab := sprintf("%s  (%d-%d, %d g)", coach, first_season, last_season, games)]
setorder(top, market)
top[, lab := factor(lab, levels = lab)]

p1 <- ggplot(top, aes(market, lab, fill = side)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.2f", market),
                hjust = ifelse(market >= 0, -0.22, 1.22)),
            size = 3.1, fontface = "bold", colour = "grey25") +
  geom_point(data = top[outside_cone == TRUE], aes(x = market), shape = 8,
             size = 1.9, colour = "grey20", show.legend = FALSE) +
  scale_fill_manual(values = c("The greats" = "#2B8CBE", "The WOATs" = "#D55E00")) +
  scale_x_continuous(expand = expansion(mult = c(0.14, 0.14))) +
  labs(
    title = "The coaching report card: wins above what the market expected, per 17 games",
    subtitle = "Regressed for career length, so one lucky season cannot top the list. Stars mark careers too good or bad to be luck.",
    x = "wins above market expectation, per 17 games (regressed)", y = NULL,
    caption = fig_caption(
      "nflverse schedules 1999 to 2025, closing spreads; market model as in R/02",
      sprintf("%d head coaches with at least 32 games. Empirical-Bayes shrunk toward the league mean using each coach's own standard error.", nrow(rc)),
      paste0("\nThe raw numbers are unusable as a ranking: Liam Coen leads all of history at +3.85 on one season, Belichick sits at +0.55 on 449 games. Shrinking each coach toward\n",
             "the mean by how noisy his own record is fixes that, and it is what a bookmaker should believe about him. The spread already prices the roster and the quarterback,\n",
             "so this is what the coach added on top of what his team was thought to be. Built by R/factory/90."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", axis.text.y = element_text(size = rel(0.86)))
save_fig("docs/figures/factory/greats_woats.png", p1, w = 12, h = 8)

# ---------------------------------------------------------------- chart 2
NAMED <- c("Bill Belichick","Andy Reid","Mike Tomlin","Pete Carroll","Sean Payton",
           "John Harbaugh","Sean McVay","Kyle Shanahan","Mike Macdonald","Dan Campbell",
           "Sean McDermott","Matt LaFleur","Nick Sirianni","Ben Johnson",
           "Adam Gase","Matt Patricia","Hue Jackson","Nathaniel Hackett","Urban Meyer",
           "Josh McDaniels","Jim Harbaugh","Mike Shanahan","Tony Dungy","Bill Cowher")
z <- function(v) { s <- sd(v, na.rm = TRUE); if (is.na(s) || s == 0) return(rep(0, length(v))); (v - mean(v, na.rm = TRUE))/s }
grid <- rc[, .(coach, games,
               `Beats the market` = z(market),
               `Offence`          = z(off_epa),
               `Defence`          = z(-def_epa),
               `Fourth down`      = z(fd_obey))]
g <- melt(grid[coach %in% NAMED], id.vars = c("coach","games"),
          variable.name = "dim", value.name = "score")
ord <- grid[coach %in% NAMED][order(`Beats the market`)]$coach
g[, coach := factor(coach, levels = ord)]
g[, score := pmax(pmin(score, 2.5), -2.5)]

p2 <- ggplot(g[!is.na(score)], aes(dim, coach, fill = score)) +
  geom_tile(colour = "white", linewidth = 1.1) +
  geom_text(aes(label = sprintf("%+.1f", score)), size = 3, fontface = "bold",
            colour = "grey15") +
  scale_fill_gradient2(low = "#D55E00", mid = "#f4f2ef", high = "#2B8CBE",
                       midpoint = 0, limits = c(-2.5, 2.5), guide = "none") +
  scale_x_discrete(position = "top") +
  labs(
    title = "Nobody is great at everything, and the WOATs are bad at different things too",
    subtitle = "Standard deviations above or below the average head coach on each dimension. Blue is good, orange is bad.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse play-by-play and schedules; nfl4th decision model; head coaches with at least 32 games",
      paste0("Market is regressed for career length. Offence and defence are team EPA per play under that head coach.\n",
             "Play-by-play only reaches back to 2015 and the fourth-down model to 2018, so pre-2015 coaches show market only."),
      paste0("\nThe dimensions are kept apart on purpose. Averaging them into one grade would bury the interesting part: Belichick's edge is defence and beating the market and he is\n",
             "poor at fourth down, Reid is the mirror image, and Macdonald and Campbell are strong exactly where the old guard is weak. Built by R/factory/90."))
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(size = rel(0.9), face = "bold"),
        axis.text.y = element_text(size = rel(0.9)))
save_fig("docs/figures/factory/report_card_grid.png", p2, w = 10.5, h = 9)

cat("\n=== the greats ===\n")
print(head(rc[order(-market), .(coach, games, market = round(market,2),
                                off = round(off_epa,3), def = round(def_epa,3),
                                fd = round(fd_obey,1))], 12))
cat("\n=== the WOATs ===\n")
print(head(rc[order(market), .(coach, games, market = round(market,2),
                               off = round(off_epa,3), def = round(def_epa,3),
                               fd = round(fd_obey,1))], 12))
