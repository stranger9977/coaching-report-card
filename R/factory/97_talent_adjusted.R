# =============================================================================
# factory/97_talent_adjusted.R -- team and quarterback talent, actually applied.
#
# Nick: "i feel like we arent controlling for qb talent or team talent enough
# in some of these views."
#
# He is right, and the gap was specific. The market leaderboard is fine, because
# the closing spread prices the roster by construction. The decision-cost ledger
# is fine, because it prices a choice rather than an outcome. But two views were
# raw team performance with a coach's name on top and no adjustment at all:
#
#   - the Offense and Defense columns of the report-card grid
#   - the per-coach situation grid (EPA by late down, late game, high leverage)
#
# Both were effectively asking "whose team was good", which is mostly a
# question about the roster.
#
# WHAT IS CONTROLLED, and why each one. All are known before the season or are
# lagged, so none of them is contaminated by the outcome being explained.
#   ctrl_index          composite QB quality: lagged EPA and CPOE, contract
#                       share, draft slot
#   ctrl_qb_apy_pct     what the team pays its quarterback, a market read on
#                       him that does not depend on how he then played
#   ctrl_team_contract  cap allocation across the roster
#   ctrl_team_madden    Madden launch ratings, an independent talent read
#
# Each is tested on its own and as an index, per the original spec, because
# they disagree: draft slot correlates about 0.00 with how a quarterback
# actually plays, while salary correlates 0.39.
#
# The adjusted number is the residual from regressing coach-season performance
# on talent. Positive means the unit did better than its talent implied.
#
# Out: docs/figures/factory/talent_adjusted.png
#      docs/figures/factory/talent_controls_compare.png
#      data/factory/talent_adjusted.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
ctrl <- readRDS("data/factory/controls.rds")
gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
d <- merge(mt, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
d[, off_coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
d[, def_coach := ifelse(defteam == home_team_gm, home_coach, away_coach)]

# ---------------------------------------------------------------- coach-seasons
off <- d[!is.na(epa) & !is.na(off_coach) & off_coach != "",
         .(plays = .N, epa = mean(epa),
           hi_epa = mean(epa[leverage >= quantile(d$leverage, .75, na.rm = TRUE)], na.rm = TRUE),
           team = names(sort(table(posteam), decreasing = TRUE))[1]),
         by = .(coach = off_coach, season)][plays >= 300]
def <- d[!is.na(epa) & !is.na(def_coach) & def_coach != "",
         .(plays = .N, epa_allowed = mean(epa),
           team = names(sort(table(defteam), decreasing = TRUE))[1]),
         by = .(coach = def_coach, season)][plays >= 300]

# primary QB per coach-season, then his controls
qb1 <- d[qb_dropback == 1 & !is.na(passer_player_id) & passer_player_id != "" &
         !is.na(off_coach), .N, by = .(coach = off_coach, season, qb_id = passer_player_id)]
setorder(qb1, coach, season, -N)
qb1 <- qb1[, .SD[1], by = .(coach, season)]
qb1 <- merge(qb1, as.data.table(ctrl$qb)[, .(qb_id, season, ctrl_index,
                                             ctrl_qb_prior_epa, ctrl_qb_apy_pct)],
             by = c("qb_id","season"), all.x = TRUE)
off <- merge(off, qb1[, .(coach, season, ctrl_index, ctrl_qb_prior_epa, ctrl_qb_apy_pct)],
             by = c("coach","season"), all.x = TRUE)

tm <- as.data.table(ctrl$team)
off <- merge(off, tm, by.x = c("team","season"), by.y = c("team","season"), all.x = TRUE)
def <- merge(def, tm, by.x = c("team","season"), by.y = c("team","season"), all.x = TRUE)

# ---------------------------------------------------------------- how much does talent explain?
CTRLS <- list(
  "QB index"        = "ctrl_index",
  "QB prior EPA"    = "ctrl_qb_prior_epa",
  "QB salary share" = "ctrl_qb_apy_pct",
  "Team cap talent" = "ctrl_team_contract",
  "Team Madden"     = "ctrl_team_madden"
)
r2_rows <- rbindlist(lapply(names(CTRLS), function(nm) {
  v <- CTRLS[[nm]]
  x <- off[!is.na(get(v)) & !is.na(epa)]
  if (nrow(x) < 40) return(NULL)
  data.table(control = nm, n = nrow(x),
             r2 = summary(lm(epa ~ get(v), data = x))$r.squared)
}))
full <- off[complete.cases(off[, .(epa, ctrl_index, ctrl_team_contract)])]
r2_rows <- rbind(r2_rows, data.table(control = "All together",
                                     n = nrow(full),
                                     r2 = summary(lm(epa ~ ctrl_index + ctrl_team_contract,
                                                     data = full))$r.squared))
cat("\n--- share of a coach-season's offensive EPA explained by talent alone ---\n")
print(r2_rows[order(-r2), .(control, n, r2 = round(r2, 3))])

setorder(r2_rows, r2)
r2_rows[, control := factor(control, levels = control)]
p0 <- ggplot(r2_rows, aes(r2, control)) +
  geom_col(aes(fill = control == "All together"), width = 0.66) +
  geom_text(aes(label = sprintf("%.1f%%  (n=%d)", 100*r2, n)), hjust = -0.08,
            size = 3.2, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#9db6c9"), guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "How much of a coach-season's offense is just the talent he was handed",
    subtitle = "Share of variance in offensive EPA per play explained by each talent measure on its own, and by the best pair together",
    x = "variance explained", y = NULL,
    caption = fig_caption(
      "Coach-seasons 2015 to 2025 with at least 300 plays; controls from R/factory/40",
      "Quarterback measures are lagged or pre-season, so none of them is contaminated by the season being explained.",
      paste0("\nTested individually and combined because they disagree with each other: where a quarterback was drafted correlates about 0.00 with how he actually plays, while what\n",
             "he is paid correlates 0.39. Even together they leave most of the variance unexplained, which is the room a coach could be working in, but a fifth to a quarter of what\n",
             "looks like coaching is visibly the roster. Built by R/factory/97."))
  ) +
  theme_coach(grid = "none")
save_fig("docs/figures/factory/talent_controls_compare.png", p0, w = 11, h = 5.4)

# ---------------------------------------------------------------- adjust
off_m <- off[complete.cases(off[, .(epa, ctrl_index, ctrl_team_contract)])]
off_m[, epa_adj := residuals(lm(epa ~ ctrl_index + ctrl_team_contract, data = off_m))]
def_m <- def[complete.cases(def[, .(epa_allowed, ctrl_team_contract)])]
def_m[, def_adj := -residuals(lm(epa_allowed ~ ctrl_team_contract, data = def_m))]

oc <- off_m[, .(seasons = .N, plays = sum(plays),
                raw = weighted.mean(epa, plays),
                adj = weighted.mean(epa_adj, plays)), by = coach][plays >= 3000]
dc2 <- def_m[, .(seasons = .N, plays = sum(plays),
                 raw_def = weighted.mean(epa_allowed, plays),
                 adj_def = weighted.mean(def_adj, plays)), by = coach][plays >= 3000]
both <- merge(oc, dc2, by = "coach", all = TRUE)
write_csv(as.data.frame(both), "data/factory/talent_adjusted.csv")

cat("\n--- biggest movers once talent is removed (offense) ---\n")
oc[, `:=`(rank_raw = frank(-raw), rank_adj = frank(-adj))]
oc[, shift := rank_raw - rank_adj]
print(head(oc[order(-abs(shift)), .(coach, raw = round(raw,4), adj = round(adj,4),
                                    rank_raw, rank_adj, shift)], 10))

NAMED <- c("Andy Reid","Sean McVay","Kyle Shanahan","Bill Belichick","Mike Tomlin",
           "Matt LaFleur","Sean McDermott","John Harbaugh","Dan Campbell","Nick Sirianni",
           "Sean Payton","Pete Carroll","Mike Macdonald","Kevin Stefanski","Josh McDaniels",
           "Adam Gase","Matt Patricia","Brandon Staley")
lab <- oc[coach %in% NAMED]
p1 <- ggplot(oc, aes(raw, adj)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = mean(oc$raw), linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#9db6c9", alpha = 0.7, size = 2.2) +
  geom_point(data = lab, colour = "#D55E00", size = 2.8) +
  geom_text_repel(data = lab, aes(label = coach), size = 3.05, fontface = "bold",
                  colour = "#8a3d00", seed = 5, box.padding = 0.5,
                  min.segment.length = 0, max.overlaps = 24) +
  labs(
    title = "Offense before and after removing the quarterback and the payroll",
    subtitle = "Raw offensive EPA per play against the same figure with QB quality and team cap talent regressed out",
    x = "raw EPA per play", y = "EPA per play above what the talent implied",
    caption = fig_caption(
      "Coach-seasons 2015 to 2025; QB index and team cap talent from R/factory/40",
      sprintf("%d head coaches with at least 3,000 offensive plays.", nrow(oc)),
      paste0("\nCoaches well above the diagonal did more with less. Those below it had the roster to explain most of their offense. This is the adjustment that was missing from the\n",
             "report-card grid, where the offense and defense columns were raw team performance with a coach's name attached. Built by R/factory/97."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/talent_adjusted.png", p1, w = 11.5, h = 7)
