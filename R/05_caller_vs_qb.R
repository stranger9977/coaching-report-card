# =============================================================================
# 05_caller_vs_qb.R
#
# Purpose: is play-calling predictability a trait of the COACH or just a
# byproduct of whichever quarterback he happens to have? Two questions:
#   (1) STABILITY -- does a caller's predictability this season predict his
#       predictability next season, and does that hold up even when his QB
#       changes? If predictability were really just "what this particular
#       QB can execute," it should collapse when the QB leaves.
#   (2) SURVIVAL -- does the link between predictability and offensive EPA
#       survive controlling for QB quality, or is "predictable" just a proxy
#       for "has a bad quarterback"?
#
# Predictability = within-situation run/pass entropy vs. the league's entropy
# in the same situation cells (down x distance x field zone x score x half),
# empirical-Bayes shrunk toward the league rate (K=15 pseudo-plays). Negative
# H_vs_lg = MORE predictable than the league in the same spots.
#
# Sources:
#   - scratch/pred_tab.rds, scratch/pred_plays.rds
#     ~/stranger9977/nfl-analysis/scratch/, built by
#     ~/stranger9977/nfl-analysis/scripts/predictability_build.R from local
#     nflverse pbp (2015-2025). pred_tab.rds is one row per caller-season
#     (>=300 called plays) with H_vs_lg and offensive EPA/play already
#     computed. pred_plays.rds is the underlying called-play universe
#     (game_id/play_id keyed) used here to attach QB identity.
#   - ~/stranger9977/nfl-analysis/data/play_by_play_20{15..25}.csv.gz, local
#     nflverse pbp, joined on game_id+play_id to pull passer_player_name,
#     cpoe, and qb_epa onto the caller's dropback plays.
#
# Provenance note: the original chart (docs/figures/caller-vs-qb.png,
# scripts/caller-vs-qb.R in nfl-analysis) read two pre-built tables,
# stability.rds and tab_qb.rds, from a Claude scratchpad directory that no
# longer exists. This script rebuilds those two tables from the surviving
# upstream caches instead of the dead scratchpad files, using the same
# definitions: QB quality on a caller-season = the mean cpoe (or qb_epa) of
# the caller's own dropback plays; the "QB changed" flag for the stability
# comparison uses each caller-season's PRIMARY quarterback (whoever threw
# the most of that caller's dropbacks that season).
#
# Out:
#   docs/figures/caller_qb_stability.png  -- predictability persists season
#     to season, with or without the same QB (dumbbell w/ 95% CIs)
#   docs/figures/caller_qb_control.png    -- predictability-offense link
#     survives controlling for the QB (3-bar, raw vs. two QB controls)
#   data/derived/caller_qb_season.csv     -- caller-season table w/ QB fields
#   data/derived/caller_qb_stability.csv  -- consecutive caller-season pairs
# =============================================================================

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

source("R/lib/theme_coach.R")

NFL_ROOT <- path.expand("~/stranger9977/nfl-analysis")
SCRATCH  <- file.path(NFL_ROOT, "scratch")
DATA_DIR <- file.path(NFL_ROOT, "data")
SEASONS  <- 2015:2025

# ---------------------------------------------------------------------------
# 1. Caller-season predictability + EPA table (already built upstream)
# ---------------------------------------------------------------------------
tab <- as.data.table(readRDS(file.path(SCRATCH, "pred_tab.rds")))
cat(sprintf("pred_tab.rds: %d caller-seasons, %d-%d\n",
            nrow(tab), min(tab$season), max(tab$season)))

# ---------------------------------------------------------------------------
# 2. Attach QB identity + quality to each dropback in the analysis universe
# ---------------------------------------------------------------------------
plays <- as.data.table(readRDS(file.path(SCRATCH, "pred_plays.rds")))
db <- plays[is_pass == 1, .(game_id, play_id, season, cs, off_play_caller)]

pbp_qb <- rbindlist(lapply(SEASONS, function(yr) {
  f <- file.path(DATA_DIR, sprintf("play_by_play_%d.csv.gz", yr))
  fread(cmd = sprintf("zcat < %s", f),
        select = c("game_id", "play_id", "passer_player_name", "cpoe", "qb_epa"))
}))
setnames(pbp_qb, "passer_player_name", "passer")
pbp_qb <- pbp_qb[!is.na(passer) & passer != ""]

db <- merge(db, pbp_qb, by = c("game_id", "play_id"), all.x = TRUE)
n_unmatched <- db[is.na(passer), .N]
cat(sprintf("dropbacks w/o a passer match: %d of %d (%.2f%%)\n",
            n_unmatched, nrow(db), 100 * n_unmatched / nrow(db)))
db <- db[!is.na(passer)]

# ---------------------------------------------------------------------------
# 3. Collapse to caller-season: QB quality (mean over the caller's own
#    dropbacks) + primary QB (whoever threw the most of them)
# ---------------------------------------------------------------------------
qb_quality <- db[, .(qb_cpoe = mean(cpoe, na.rm = TRUE),
                      qb_epa  = mean(qb_epa, na.rm = TRUE),
                      n_db    = .N), by = cs]

primary_qb <- db[, .N, by = .(cs, passer)][order(cs, -N)][, .SD[1], by = cs]
setnames(primary_qb, c("passer", "N"), c("primary_qb", "n_primary"))

tab_qb <- merge(tab, qb_quality, by = "cs", all.x = TRUE)
tab_qb <- merge(tab_qb, primary_qb[, .(cs, primary_qb, n_primary)], by = "cs", all.x = TRUE)

n_no_qb <- tab_qb[is.na(qb_cpoe), .N]
cat(sprintf("caller-seasons w/o any QB match: %d of %d\n", n_no_qb, nrow(tab_qb)))

fwrite(tab_qb, "data/derived/caller_qb_season.csv")

# ---------------------------------------------------------------------------
# 4. Stats helpers (match the original script's conventions)
# ---------------------------------------------------------------------------
fz <- function(r, n) {                       # Fisher-z 95% CI
  z <- atanh(r); se <- 1 / sqrt(n - 3)
  tanh(z + c(-1.96, 1.96) * se)
}
pcor <- function(x, y, z) {                  # partial correlation of x,y | z
  rxy <- cor(x, y); rxz <- cor(x, z); ryz <- cor(y, z)
  (rxy - rxz * ryz) / sqrt((1 - rxz^2) * (1 - ryz^2))
}

# ---------------------------------------------------------------------------
# 5. SURVIVAL: does predictability -> EPA survive controlling for the QB?
# ---------------------------------------------------------------------------
s <- tab_qb[!is.na(qb_cpoe) & !is.na(qb_epa) & !is.na(epa_play)]
r_raw    <- cor(s$H_vs_lg, s$epa_play)
r_part   <- pcor(s$H_vs_lg, s$epa_play, s$qb_cpoe)
r_part_e <- pcor(s$H_vs_lg, s$epa_play, s$qb_epa)
n_control <- nrow(s)

# ---------------------------------------------------------------------------
# 6. STABILITY: consecutive caller-season pairs, split by whether the
#    caller's primary QB changed
# ---------------------------------------------------------------------------
setorder(tab_qb, off_play_caller, season)
nxt_qb <- tab_qb[, .(off_play_caller, season = season - 1, primary_qb)]
setnames(nxt_qb, "primary_qb", "primary_qb_next")

st <- tab_qb[, .(off_play_caller, season, H_vs_lg, primary_qb)]
st <- merge(st, tab_qb[, .(off_play_caller, season = season - 1, H_next = H_vs_lg)],
            by = c("off_play_caller", "season"))
st <- merge(st, nxt_qb, by = c("off_play_caller", "season"))
st <- st[!is.na(primary_qb) & !is.na(primary_qb_next)]
st[, qb_changed := primary_qb != primary_qb_next]

fwrite(st, "data/derived/caller_qb_stability.csv")

r_auto  <- cor(st$H_vs_lg, st$H_next)
n_auto  <- nrow(st)
r_same  <- cor(st[qb_changed == FALSE]$H_vs_lg, st[qb_changed == FALSE]$H_next)
n_same  <- st[qb_changed == FALSE, .N]
r_chg   <- cor(st[qb_changed == TRUE]$H_vs_lg,  st[qb_changed == TRUE]$H_next)
n_chg   <- st[qb_changed == TRUE, .N]
ci_same <- fz(r_same, n_same)
ci_chg  <- fz(r_chg, n_chg)
z_diff  <- (atanh(r_same) - atanh(r_chg)) / sqrt(1 / (n_same - 3) + 1 / (n_chg - 3))
p_diff  <- 2 * (1 - pnorm(abs(z_diff)))

# ---------------------------------------------------------------------------
# 7. Reproduced numbers vs. the original chart's published targets
# ---------------------------------------------------------------------------
cat("\n=== STABILITY (reproduced vs. target) ===\n")
cat(sprintf("overall r          = %.3f (n=%d)   [target r=0.48]\n", r_auto, n_auto))
cat(sprintf("same QB       r    = %.3f [%.2f, %.2f]  n=%d   [target r=0.54 [0.41,0.66] n=125]\n",
            r_same, ci_same[1], ci_same[2], n_same))
cat(sprintf("after QB change r  = %.3f [%.2f, %.2f]  n=%d   [target r=0.34 [0.15,0.50] n=103]\n",
            r_chg, ci_chg[1], ci_chg[2], n_chg))
cat(sprintf("gap p-value        = %.3f   [target p=0.058]\n", p_diff))

cat("\n=== SURVIVAL (reproduced vs. target) ===\n")
cat(sprintf("raw r                       = %.3f  (n=%d)   [target r=-0.37]\n", r_raw, n_control))
cat(sprintf("partial r | QB cpoe         = %.3f              [target r=-0.26]\n", r_part))
cat(sprintf("partial r | QB total EPA*   = %.3f              [target r=-0.09, *over-controls]\n", r_part_e))

# ---------------------------------------------------------------------------
# 8. FIGURE 1: predictability persists season to season, with or without
#    the same QB. One message, dumbbell/paired bars with CIs, no scatter.
# ---------------------------------------------------------------------------
stab_bars <- data.table(
  grp = factor(c("Same QB\nnext season", "After a QB\nchange"),
               levels = c("After a QB\nchange", "Same QB\nnext season")),
  r   = c(r_same, r_chg),
  lo  = c(ci_same[1], ci_chg[1]),
  hi  = c(ci_same[2], ci_chg[2]),
  n   = c(n_same, n_chg)
)

p_stability <- ggplot(stab_bars, aes(x = grp, y = r)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.5, fill = "#2B8CBE") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.12, linewidth = 0.7,
                colour = ink_title) +
  geom_text(aes(y = 0.71,
                label = sprintf("r = %.2f  [%.2f, %.2f]\nn = %d pairs", r, lo, hi, n)),
            hjust = 0, colour = ink_title, fontface = "bold", size = 4.6, lineheight = 0.95) +
  coord_flip(ylim = c(0, 0.85), clip = "off") +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  labs(
    title = "A coach's play-calling predictability carries into next season, QB or no QB",
    subtitle = "Correlation between this season's and next season's predictability, by whether the caller's QB changed.\nThe two confidence intervals overlap, so the gap between them is not statistically distinguishable (p = 0.06).",
    x = NULL,
    y = "correlation with next season's predictability (r)",
    caption = paste(strwrap(fig_caption(
      "nflverse play-by-play, 2015-2025",
      "Caller-seasons with >=300 called plays, consecutive-season pairs with a known primary QB both years.",
      "Predictability = situation-shrunk run/pass entropy vs. league; primary QB = most dropbacks that caller-season."
    ), width = 130), collapse = "\n")
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(face = "bold", size = rel(1.05)))

save_fig("docs/figures/caller_qb_stability.png", p_stability, w = 11, h = 6.5)

# ---------------------------------------------------------------------------
# 9. FIGURE 2: the predictability -> offense link survives controlling for
#    the QB. One message, 3 bars, values printed large.
# ---------------------------------------------------------------------------
# H_vs_lg is ENTROPY vs league: high = hard to guess. cor(H_vs_lg, epa) is
# negative, i.e. more predictable = better offense. For display we flip to the
# predictability scale (predictability = -entropy; cor(-x, y) = -cor(x, y)) so
# the bars read in the same direction as the board's language.
ctrl_bars <- data.table(
  lab = factor(c("Raw correlation", "Minus QB accuracy\n(cpoe)", "Minus QB total EPA*\n(over-controls)"),
               levels = c("Raw correlation", "Minus QB accuracy\n(cpoe)", "Minus QB total EPA*\n(over-controls)")),
  r   = -c(r_raw, r_part, r_part_e)
)

p_control <- ggplot(ctrl_bars, aes(x = lab, y = r)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.6, fill = "#045A8D") +
  geom_text(aes(y = r + 0.025, label = sprintf("+%.2f", r)),
            colour = ink_title, fontface = "bold", size = 6.5) +
  scale_y_continuous(limits = c(-0.03, max(ctrl_bars$r) + 0.06),
                      breaks = seq(0, 0.4, 0.1)) +
  labs(
    title = "Predictable play-calling still tracks better offense after removing the QB",
    subtitle = "Correlation between a caller's predictability and offensive EPA/play, before and after controlling for QB quality.\nThe link shrinks under either control but stays positive; the third bar over-controls (it partly removes the caller's own play design).",
    x = NULL,
    y = "correlation of predictability with offensive EPA/play",
    caption = paste(strwrap(fig_caption(
      "nflverse play-by-play, 2015-2025",
      sprintf("Caller-seasons with >=300 called plays and a matched QB (n=%d).", n_control),
      "QB accuracy = mean cpoe on the caller's dropbacks; QB total EPA = mean qb_epa on the same dropbacks."
    ), width = 130), collapse = "\n")
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(face = "bold", size = rel(0.95)))

save_fig("docs/figures/caller_qb_control.png", p_control, w = 10, h = 6.5)

cat("\nDONE\n")
