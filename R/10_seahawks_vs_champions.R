# =============================================================================
# 10_seahawks_vs_champions.R
#
# Michael, 13 Aug:
#   "Did the Seahawks outperform more than any other Super Bowl winning team
#    (look at various metrics)?"
#
# The premise checks out: Seattle beat New England in Super Bowl LX to close the
# 2025 season. So the question is well posed, and it is the right one for the
# spine of the video, because it puts his Seahawks hunch on a scale with every
# other champion of the modern era instead of leaving it as a feeling.
#
# The market measure is the one that reaches all 27 champions (1999-2025),
# because closing spreads exist for every game. It is also the strictest test:
# the spread already prices in the roster, the quarterback and the schedule, so
# beating it is what is left over after the market has accounted for talent.
# Method matches R/02 exactly, an empirical logistic P(home win) ~ closing
# spread fit on decisive regular-season games, summed to expected wins.
#
# The other two talent meters do not reach back that far. Madden ratings start
# at 2017 (R/03) and the contract-value meter at 2012 (R/06), so those get a
# shorter comparison, clearly labelled.
#
# Source: ~/stranger9977/nfl-analysis/data/games.csv (nflverse schedules,
# 1999-2025, with closing spread_line and result), plus the two derived talent
# tables this repo already builds.
#
# Out: docs/figures/champions_market.png
#      data/derived/champions.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
g <- fread(file.path(NFLA, "data/games.csv"), showProgress = FALSE)

# --- who won each Super Bowl -------------------------------------------------
sb <- g[game_type == "SB" & !is.na(result)]
sb[, champion := fifelse(result > 0, home_team, away_team)]
champs <- sb[, .(season, champion)]
cat(sprintf("Super Bowls covered: %d (%d to %d)\n", nrow(champs),
            min(champs$season), max(champs$season)))

# --- market model, identical to R/02 ----------------------------------------
reg <- g[game_type == "REG" & !is.na(result) & !is.na(spread_line)]
dec <- reg[result != 0][, home_win := as.integer(result > 0)]
fit <- glm(home_win ~ spread_line, data = dec, family = binomial)
cat(sprintf("market model on %s decisive games; 50-50 line at spread %.2f\n",
            format(nrow(dec), big.mark = ","), -coef(fit)[1]/coef(fit)[2]))

reg[, p_home := predict(fit, newdata = reg, type = "response")]
long <- rbind(
  reg[, .(season, team = home_team, exp_wp = p_home,
          win = fifelse(result > 0, 1, fifelse(result == 0, 0.5, 0)))],
  reg[, .(season, team = away_team, exp_wp = 1 - p_home,
          win = fifelse(result < 0, 1, fifelse(result == 0, 0.5, 0)))]
)
ts <- long[, .(games = .N, wins = sum(win), exp_wins = sum(exp_wp)), by = .(season, team)]
ts[, `:=`(wae = wins - exp_wins, wae17 = (wins - exp_wins) * 17 / games)]

ch <- merge(champs, ts, by.x = c("season","champion"), by.y = c("season","team"))
setorder(ch, -wae17)
ch[, rank := .I]
write_csv(as.data.frame(ch), "data/derived/champions.csv")

cat("\n--- every Super Bowl winner, by regular-season wins above the market ---\n")
print(ch[, .(rank, season, champion, wins = round(wins,1),
             exp_wins = round(exp_wins,2), wae17 = round(wae17,2))])

sea <- ch[champion == "SEA" & season == 2025]
cat(sprintf("\n2025 Seattle: %.0f wins vs %.2f expected = %+.2f, rank %d of %d champions\n",
            sea$wins, sea$exp_wins, sea$wae17, sea$rank, nrow(ch)))

# league-wide context: where does that sit among ALL team-seasons?
allpct <- 100 * mean(ts$wae17 < sea$wae17)
cat(sprintf("that is the %.0fth percentile of all %s team-seasons since 1999\n",
            allpct, format(nrow(ts), big.mark = ",")))

# --- chart -------------------------------------------------------------------
ch[, lab := paste0(season, " ", champion)]
ch[, lab := factor(lab, levels = rev(lab))]
ch[, is_sea := champion == "SEA" & season == 2025]

p <- ggplot(ch, aes(wae17, lab, fill = is_sea)) +
  geom_col(width = 0.72) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_text(aes(label = sprintf("%+.1f", wae17),
                hjust = ifelse(wae17 >= 0, -0.25, 1.25)),
            size = 3.1, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#9db6c9")) +
  scale_x_continuous(expand = expansion(mult = c(0.10, 0.10))) +
  labs(
    title = "Only three champions since 1999 beat the market by more than the 2025 Seahawks",
    subtitle = "Regular-season wins above what the closing point spread expected, per 17 games, every Super Bowl winner since 1999",
    x = "wins above market expectation, per 17 games", y = NULL,
    caption = fig_caption(
      "nflverse schedules 1999 to 2025, closing spreads; market model as in R/02",
      sprintf("All %d Super Bowl winners. Regular season only.", nrow(ch)),
      paste0("\nAnswers Michael's question directly. The spread already prices in the roster, the quarterback and the schedule, so this is what a team did beyond what the market thought it was.\n",
             sprintf("2025 Seattle won %.0f games against %.1f expected, which ranks %d of %d champions and sits in the %.0fth percentile of every team-season since 1999.\n",
                     sea$wins, sea$exp_wins, sea$rank, nrow(ch), allpct),
             "Ahead of them: the 2003 and 2001 Patriots, and last season's Eagles by six hundredths of a win. The early Belichick teams are the only real separation. Built by R/10."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.85)))

save_fig("docs/figures/champions_market.png", p, w = 11.5, h = 8.4)
