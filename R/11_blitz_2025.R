# =============================================================================
# 11_blitz_2025.R
#
# Michael, 13 Aug, looking at the blitz card:
#   "So Josh Allen blows against the blitz?"
#   "And Kyle Murray is solid?"
#   "Also I noticed a lot of them were 2022-2023 data, did you not have 2025?"
#
# Three separate things to sort out and they turn out to be one thing.
#
# 1. HIS READING IS CORRECT. On that chart, far right means blitzing this
#    quarterback pays. Allen was the furthest right in the league. So yes, the
#    chart said blitzing Josh Allen is profitable, which is a strange claim
#    about a quarterback with his reputation under pressure.
#
# 2. THAT NUMBER WAS UNSTABLE. The break-even is
#       pb* = (p4*P + (1-p4)*F_N - B_N) / (P - B_N)
#    and the denominator is a quarterback's EPA under pressure minus his EPA on
#    a blitz that never got home. When those two are close the ratio explodes.
#    Allen's printed value was +58 percentage points, so far outside the field
#    that the original chart squished him to +30 just to keep the axis usable.
#    A point that has to be capped to be drawn is a point to re-examine, not a
#    finding.
#
# 3. THE FIX IS ALSO THE ANSWER TO HIS THIRD QUESTION. That chart ran on two
#    seasons because two were all the FTN charting we had. There are now four,
#    2022 through 2025. Doubling the sample is exactly what an unstable
#    per-quarterback ratio needs.
#
# So this rebuilds the whole thing on 2022-2025 and reports what happens to
# Allen and Murray, whatever that turns out to be.
#
# Source: ~/stranger9977/nfl-analysis, FTN charting 2022-2025 (2025 downloaded
# 2026-08-13), participation 2022-2025, pbp_slim.rds. Method copied from
# scripts/blitz-autopilot.R.
#
# Out: docs/figures/blitz_2025.png
#      data/derived/blitz_qb_2025.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
YRS <- 2022:2025

ftn <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
        select = c("nflverse_game_id","nflverse_play_id","n_blitzers","is_screen_pass"),
        showProgress = FALSE)))
part <- rbindlist(lapply(YRS, function(y)
  fread(file.path(D, sprintf("pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id","play_id","was_pressure"), showProgress = FALSE)))
pbp <- as.data.table(readRDS(file.path(D, "pbp_slim.rds")))
pbp <- pbp[season %in% YRS & qb_dropback == 1 & qb_spike == 0,
           .(game_id, play_id, season, epa, passer_player_name)]

setnames(ftn, c("nflverse_game_id","nflverse_play_id"), c("game_id","play_id"))
setnames(part, "nflverse_game_id", "game_id")
dt <- merge(merge(pbp, ftn, by = c("game_id","play_id")), part, by = c("game_id","play_id"))
dt <- dt[is_screen_pass == FALSE & !is.na(was_pressure) & !is.na(epa) & passer_player_name != ""]
dt[, `:=`(blitz = n_blitzers > 0, pressure = was_pressure == 1)]
cat(sprintf("plays %d-%d: %s (the old chart had %s from 2022-23)\n",
            min(YRS), max(YRS), format(nrow(dt), big.mark = ","),
            format(nrow(dt[season <= 2023]), big.mark = ",")))

build <- function(x, min_n = 400) {
  qb <- x[, .(n = .N, blitz_rate = mean(blitz),
              p4 = mean(pressure[!blitz]), pb = mean(pressure[blitz]),
              F_N = mean(epa[!blitz & !pressure]), n_FN = sum(!blitz & !pressure),
              B_N = mean(epa[blitz & !pressure]),  n_BN = sum(blitz & !pressure),
              P_epa = mean(epa[pressure]), n_P = sum(pressure),
              v_play = var(epa)), by = passer_player_name][n >= min_n]
  shrink <- function(m, n, v) {
    pv <- max(var(m) - mean(v/n), 0.002); k <- mean(v)/pv
    (n*m + k*weighted.mean(m, n))/(n + k)
  }
  qb[, `:=`(F_Ns = shrink(F_N, n_FN, v_play),
            B_Ns = shrink(B_N, n_BN, v_play),
            P_s  = shrink(P_epa, n_P, v_play))]
  qb[, denom := P_s - B_Ns]
  qb[, pb_star := (p4*P_s + (1-p4)*F_Ns - B_Ns)/denom]
  qb[, surplus := 100*((pb - p4) - (pb_star - p4))]
  qb[]
}

old <- build(dt[season <= 2023])
new <- build(dt)
write_csv(as.data.frame(new), "data/derived/blitz_qb_2025.csv")

cat("\n--- the instability, shown ---\n")
cat("The break-even divides by (EPA under pressure - EPA on an unpressured blitz).\n")
cat("The closer that is to zero, the more the answer explodes.\n\n")
show <- c("J.Allen","K.Murray","P.Mahomes","J.Hurts","L.Jackson","J.Burrow")
cat("2022-23 (what Michael saw):\n")
print(old[passer_player_name %in% show,
          .(passer_player_name, n, denom = round(denom,3), surplus = round(surplus,1))][order(-surplus)])
cat("\n2022-25 (double the sample):\n")
print(new[passer_player_name %in% show,
          .(passer_player_name, n, denom = round(denom,3), surplus = round(surplus,1))][order(-surplus)])

cmp <- merge(old[, .(passer_player_name, old = surplus)],
             new[, .(passer_player_name, new = surplus, n)], by = "passer_player_name")
cat(sprintf("\nQBs in both: %d. Correlation of the payoff estimate between windows: %.2f\n",
            nrow(cmp), cor(cmp$old, cmp$new)))
cat(sprintf("range 2022-23: %.0f to %.0f pp | range 2022-25: %.0f to %.0f pp\n",
            min(old$surplus), max(old$surplus), min(new$surplus), max(new$surplus)))

r <- cor(new$blitz_rate, new$surplus); pv <- summary(lm(blitz_rate ~ surplus, new))$coefficients[2,4]
cat(sprintf("\ncoordinators still blitz on autopilot? r(blitz rate, payoff) = %.2f (p = %.2f)\n", r, pv))

# --- chart -------------------------------------------------------------------
new[, clears := surplus > 0]
lab <- new[passer_player_name %in% c("J.Allen","K.Murray","P.Mahomes","J.Hurts","L.Jackson",
                                     "J.Burrow","G.Smith","T.Tagovailoa","B.Purdy","J.Herbert",
                                     "C.Stroud","J.Goff","D.Prescott")]

p <- ggplot(new, aes(surplus, 100*blitz_rate)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_smooth(method = "lm", se = FALSE, colour = "grey55",
              linewidth = 0.6, linetype = "dashed", formula = y ~ x) +
  geom_point(aes(colour = clears), size = 2.5, alpha = 0.85) +
  geom_text_repel(data = lab, aes(label = passer_player_name, colour = clears),
                  size = 3.1, fontface = "bold", seed = 7,
                  box.padding = 0.5, min.segment.length = 0, max.overlaps = 25,
                  show.legend = FALSE) +
  annotate("text", x = -1.2, y = max(100*new$blitz_rate) + 0.6, hjust = 1, size = 3.2,
           fontface = "bold", colour = "#8a3d00",
           label = "Blitzing this QB never pays") +
  annotate("text", x = 1.2, y = max(100*new$blitz_rate) + 0.6, hjust = 0, size = 3.2,
           fontface = "bold", colour = "#1c5b80",
           label = "Blitzing this QB pays") +
  annotate("text", x = max(new$surplus), y = min(100*new$blitz_rate), hjust = 1, size = 3.1,
           colour = "grey35",
           label = sprintf("blitz rate vs payoff: r = %.2f (p = %.2f)", r, pv)) +
  scale_colour_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#D55E00"), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pp")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "With two more seasons of data, the autopilot finding does not survive",
    subtitle = "How often defenses blitz each quarterback, against whether blitzing him actually pays, 2022 to 2025",
    x = "Blitz payoff: pressure bump achieved minus the bump needed to break even",
    y = "how often defenses blitzed him",
    caption = fig_caption(
      "nflverse play-by-play, FTN charting and participation, 2022 to 2025 (2025 added 2026-08-13)",
      sprintf("%s dropbacks, screens and spikes excluded; quarterbacks with at least 400.", format(nrow(dt), big.mark = ",")),
      paste0("\nTwo things changed. Michael read the old chart right, it really did say blitzing Josh Allen was the most profitable in football at +58 points, so far out that the chart squished\n",
             "him to fit. That came from dividing by the gap between his EPA under pressure and his EPA on a blitz that never got home, which for him was nearly zero. On four seasons he\n",
             "falls to +14, an ordinary target, and the whole league range narrows from -7..+58 to -16..+17. The two-season estimates correlate with the four-season ones at only 0.33.\n",
             "And the headline claim goes with it: on 2022-23 blitz rate versus payoff was a flat line (r = 0.06, p = 0.71), which is where 'autopilot' came from. On 2022-25 it is\n",
             "r = 0.32, p = 0.02. Coordinators do blitz the quarterbacks it pays to blitz, just weakly. Built by R/11."))
  ) +
  theme_coach(grid = "y") + coord_cartesian(clip = "off")

save_fig("docs/figures/blitz_2025.png", p, w = 12, h = 7)
