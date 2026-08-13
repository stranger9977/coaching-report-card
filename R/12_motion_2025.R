# =============================================================================
# 12_motion_2025.R
#
# The second stale chart on the board, refreshed for the same reason as R/11.
# "Motion does not scare man coverage" ran on 2022-23 because that was all the
# FTN charting we had. There are now four seasons. After the blitz card's
# headline died on refresh, every carried-over finding on two seasons of
# charting has to be re-checked rather than assumed.
#
# The original claim, verbatim from nfl-analysis: the celebrated motion-beats-
# man edge is small and fragile (+0.03 EPA diff-in-diff, 95% CI -0.04 to +0.11)
# and the deterrence corollary is flatly false, defenses do not check out of
# man when motion is on.
#
# This reruns both tests on 2022-2025 with the identical method: a 2x2 of EPA
# by motion and coverage, a bootstrapped diff-in-diff, and a linear probability
# model of man coverage on motion with situation and team controls.
#
# Source: ~/stranger9977/nfl-analysis, FTN charting and participation 2022-2025
# (2025 added 2026-08-13). Method from scripts/motion-man-tax.R.
#
# Out: docs/figures/motion_2025.png
#      data/derived/motion_2025.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
YRS <- 2022:2025

pbp <- as.data.table(readRDS(file.path(D, "pbp_slim.rds")))[season %in% YRS]
ftn <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
        select = c("nflverse_game_id","nflverse_play_id","is_motion"), showProgress = FALSE)))
part <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id","play_id","defense_man_zone_type"), showProgress = FALSE)))
setnames(ftn, c("nflverse_game_id","nflverse_play_id"), c("game_id","play_id"))
setnames(part, "nflverse_game_id", "game_id")

d <- merge(merge(pbp, ftn, by = c("game_id","play_id")), part, by = c("game_id","play_id"))
d <- d[season_type == "REG" & qb_dropback == 1 & play_type != "no_play" &
       qb_kneel == 0 & qb_spike == 0 & !is.na(epa) & !is.na(xpass) &
       vegas_wp >= .05 & vegas_wp <= .95 &
       defense_man_zone_type %in% c("MAN_COVERAGE","ZONE_COVERAGE")]
d[, `:=`(motion = as.integer(is_motion),
         man = as.integer(defense_man_zone_type == "MAN_COVERAGE"))]
cat(sprintf("dropbacks 2022-2025: %s (the old chart had %s)\n",
            format(nrow(d), big.mark = ","),
            format(nrow(d[season <= 2023]), big.mark = ",")))

dd_stat <- function(x) {
  m <- x[, .(epa = mean(epa)), by = .(motion, man)]
  (m[motion == 1 & man == 1, epa] - m[motion == 0 & man == 1, epa]) -
    (m[motion == 1 & man == 0, epa] - m[motion == 0 & man == 0, epa])
}
report <- function(x, tag) {
  set.seed(7)
  bs <- replicate(500, dd_stat(x[sample(.N, .N, replace = TRUE)]))
  ci <- quantile(bs, c(.025, .975))
  mr <- x[, .(man_rate = 100*mean(man)), by = motion]
  cat(sprintf("\n%s  n=%s\n  EPA diff-in-diff %+.3f (95%% CI %+.3f to %+.3f)\n",
              tag, format(nrow(x), big.mark=","), dd_stat(x), ci[1], ci[2]))
  cat(sprintf("  man rate without motion %.1f%%, with motion %.1f%%, gap %+.1f pp\n",
              mr[motion==0, man_rate], mr[motion==1, man_rate],
              mr[motion==1, man_rate] - mr[motion==0, man_rate]))
  list(did = dd_stat(x), lo = ci[1], hi = ci[2])
}
o <- report(d[season <= 2023], "2022-23 (what the board says)")
n <- report(d, "2022-25 (refreshed)")

m_lpm <- lm(man ~ motion + factor(down) + log1p(ydstogo) + yardline_100 + xpass +
              factor(season) + factor(posteam) + factor(defteam), data = d)
co <- summary(m_lpm)$coefficients["motion", ]
cat(sprintf("\nsituation-adjusted man-rate change from motion: %+.1f pp (p = %.3f)\n",
            100*co[1], co[4]))

cells <- d[, .(n = .N, epa = mean(epa), se = sd(epa)/sqrt(.N)),
           by = .(motion, cov = fifelse(man == 1, "Against man coverage", "Against zone coverage"))]
cells[, `:=`(lo = epa - 1.96*se, hi = epa + 1.96*se,
             mot = fifelse(motion == 1, "With motion", "No motion"))]
write_csv(as.data.frame(cells), "data/derived/motion_2025.csv")
print(cells[order(cov, motion), .(cov, mot, n, epa = round(epa,3))])

p <- ggplot(cells, aes(epa, cov, colour = mot)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.35) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.14, linewidth = 0.7,
                 position = position_dodge(width = 0.45)) +
  geom_point(size = 3.2, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = c("No motion" = "#9db6c9", "With motion" = "#D55E00")) +
  labs(
    title = sprintf("Four seasons on, motion still is not a man-coverage tax (%+.3f EPA, CI %+.3f to %+.3f)",
                    n$did, n$lo, n$hi),
    subtitle = "Offensive EPA per dropback with and without pre-snap motion, against man and against zone, 2022 to 2025",
    x = "EPA per dropback (bars are 95% confidence intervals)", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play, FTN charting and participation, 2022 to 2025 (2025 added 2026-08-13)",
      sprintf("%s regular-season dropbacks in competitive game states, man or zone tagged.", format(nrow(d), big.mark = ",")),
      paste0("\nThe claim is that motion hurts man coverage specifically, so the test is whether motion's benefit is bigger against man than against zone. On two seasons that gap was\n",
             sprintf("%+.3f EPA. On four it is %+.3f, with the interval still straddling zero. Unlike the blitz card, this one survives the refresh unchanged.\n", o$did, n$did),
             sprintf("The deterrence corollary shifts, though. Raw man rates are identical with and without motion, 37.9%% either way, but adjusting for situation and both teams motion is worth\n%+.1f points less man coverage, and with double the sample that is now statistically detectable (p = %.3f) where before it was not. Real, and far too small to be the hidden half of motion's value. Built by R/12.", 100*co[1], co[4])))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")

save_fig("docs/figures/motion_2025.png", p, w = 12, h = 5.4)
