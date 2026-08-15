# =============================================================================
# factory/80_macdonald.R -- the defensive half of Michael's spine.
#
# His outline: "Setup Defense vs. Offense / McVay vs. MacDonald" and
# "MacDonald (defense): The overachievers (how much have his teams
# overachieved defensively?) Ravens / Seahawks. What exactly is he doing that
# makes him so successful?"
#
# The answer to the first part is a shape nobody seems to have named. At both
# franchises his defence was ordinary in year one and second in the league in
# year two. Same two-year clock, different rosters, different conference.
#
# The reason that is worth a chart rather than a sentence: there is no
# league-wide year-two effect to hide behind. Across 96 coordinator stints
# with two seasons at the same club, year two is no better than year one
# (paired t-test p = 0.32, only 43% improve). So this is not the normal shape
# of a coordinator settling in. It is his.
#
# Honest limit, stated on the chart: n = 2. Two stints is two data points, and
# the claim is only that both land in the top tenth of all stints, which is
# unlikely twice but not proof of a method.
#
# Out: docs/figures/factory/macdonald_clock.png
#      docs/figures/factory/defense_leaders.png
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

mt <- as.data.table(readRDS("data/factory/model_table.rds"))
def <- mt[!is.na(def_caller_opp) & def_caller_opp != "",
          .(plays = .N, epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE),
            team = names(sort(table(defteam), decreasing = TRUE))[1]),
          by = .(dc = def_caller_opp, season)][plays >= 300]
def[, `:=`(rank = frank(epa), of = .N), by = season]
setorder(def, dc, team, season)
def[, yr := seq_len(.N), by = .(dc, team)]

pairs <- def[, .(y1 = epa[yr == 1][1], y2 = epa[yr == 2][1]), by = .(dc, team)][!is.na(y1) & !is.na(y2)]
pairs[, jump := y1 - y2]
tt <- t.test(pairs$y2, pairs$y1, paired = TRUE)
cat(sprintf("league: %d two-year stints, %.0f%% improve, paired p = %.3f\n",
            nrow(pairs), 100*mean(pairs$jump > 0), tt$p.value))
mm <- pairs[grepl("Macdonald", dc)]
print(mm)

# ---------------------------------------------------------------- chart 1
mac <- def[grepl("Macdonald", dc)][order(season)]
mac[, stint := paste0(team, " (", ifelse(team == "BAL", "2022-23", "2024-25"), ")")]
mac[, yrlab := paste0("Year ", yr)]
lgmean <- def[, .(lg = mean(epa)), by = season]
mac <- merge(mac, lgmean, by = "season")

p1 <- ggplot(mac, aes(yrlab, epa, group = stint, colour = stint)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.4) +
  geom_text(aes(label = sprintf("%.3f  (%s in the league)", epa,
                                scales::ordinal(as.integer(rank))),
                vjust = ifelse(team == "BAL", -1.5, 2.4)),
            size = 3.3, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = c("#D55E00", "#2B8CBE")) +
  scale_y_continuous(limits = c(-0.18, 0.03)) +
  labs(
    title = "Mike Macdonald has done the same thing twice: ordinary in year one, second in the league in year two",
    subtitle = "EPA per play allowed by his defences, at Baltimore and then at Seattle. Lower is better.",
    x = NULL, y = "EPA per play allowed",
    caption = fig_caption(
      "nflverse play-by-play; defensive play-caller attribution from samhoppen/NFL_public",
      "Defensive play-callers with at least 300 plays in a season, 2015 to 2025.",
      paste0("\nThe pattern is not the normal shape of a coordinator settling in. Across 96 stints where a coordinator had two seasons at the same club, year two is no better than\n",
             "year one (43% improve, paired t-test p = 0.32). Macdonald's improvement was 0.127 EPA per play at Baltimore and 0.101 at Seattle, the 94th and 91st percentile of all\n",
             "stints, with different rosters in different conferences. The honest limit is that two stints is two data points. Built by R/factory/80."))
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")
save_fig("docs/figures/factory/macdonald_clock.png", p1, w = 11.5, h = 6.4)

# ---------------------------------------------------------------- chart 2
car <- def[, .(seasons = .N, plays = sum(plays),
               epa = weighted.mean(epa, plays)), by = dc][plays >= 1500]
setorder(car, epa)
top <- head(car, 16)
top[, dc := factor(dc, levels = rev(dc))]
top[, hl := grepl("Macdonald", dc)]

p2 <- ggplot(top, aes(epa, dc, fill = hl)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", epa)), hjust = -0.25,
            size = 3.2, fontface = "bold", colour = "white") +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#7f97a8")) +
  scale_x_continuous(expand = expansion(mult = c(0.06, 0))) +
  labs(
    title = "Career EPA per play allowed: Macdonald is second, on the fewest snaps of anyone near him",
    subtitle = "Defensive play-callers with at least 1,500 charted plays, 2015 to 2025. Lower is better.",
    x = "EPA per play allowed", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play; defensive play-caller attribution from samhoppen/NFL_public",
      sprintf("%d defensive play-callers clear the 1,500-play bar.", nrow(car)),
      paste0("\nMacdonald has 4,640 plays against Wade Phillips's 5,598 and Jim Schwartz's 8,403, so his average is over a shorter and more recent window and is less settled.\n",
             "Answers the outline's 'how much have his teams overachieved defensively'. Built by R/factory/80."))
  ) +
  theme_coach(grid = "none") + theme(legend.position = "none")
save_fig("docs/figures/factory/defense_leaders.png", p2, w = 11.5, h = 7)
