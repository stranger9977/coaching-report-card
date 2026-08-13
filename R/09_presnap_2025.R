# =============================================================================
# 09_presnap_2025.R
#
# Two of Michael's 13 Aug notes, answered together.
#
# FRESHNESS. His words: "Also I noticed a lot of them were 2022-2023 data, did
# you not have 2025?" He was right and the cause was our own stale cache, not
# the league. nflverse participation (the formation feed) now publishes 2024
# and 2025, and FTN charting now publishes 2025; the local copies in
# nfl-analysis stopped at 2023 and 2024. The fresh files are downloaded and the
# pre-snap metric is rebuilt here on 2016-2025 instead of 2016-2023.
#
# McVAY. His words: "I feel like McVay is lauded for being unpredictable in the
# sense of lots of movement, ya? But what you're saying is he is highly
# predictable based on run/pass?" and Nick's reply, "He evolves a lot year over
# year tbh. I'll dig into that more." The career average cannot answer that, so
# this also computes the look metric per caller-season.
#
# Method is copied exactly from nfl-analysis/scripts/pred_tip_disguise_build.R
# (empirical-Bayes shrinkage K=15, formation x personnel bucket as the "look",
# tip = situational entropy minus entropy once the look is known), with two
# changes: the window runs to 2025, and a per-season table is produced
# alongside the career one. The career QB-cpoe column is dropped because its
# source file lived in a scratchpad that no longer exists.
#
# Out: docs/figures/presnap_mcvay.png
#      docs/figures/presnap_freshness.png
#      data/derived/presnap_tip_2025.csv
#      data/derived/presnap_tip_season.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
Hbin <- function(p) { p <- pmin(pmax(p, 1e-9), 1 - 1e-9); -(p*log2(p) + (1-p)*log2(1-p)) }
K <- 15

d0 <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_plays.rds")))
part <- rbindlist(lapply(2016:2025, function(y) {
  p <- fread(file.path(NFLA, sprintf("data/pbp_participation_%d.csv.gz", y)),
             select = c("nflverse_game_id","play_id","offense_formation","offense_personnel"),
             showProgress = FALSE)
  setnames(p, "nflverse_game_id", "game_id"); p
}))

pers_bucket <- function(x) {
  rb <- as.integer(sub(".*?([0-9]+) RB.*", "\\1", x)); rb[is.na(rb)] <- 1L
  te <- as.integer(sub(".*?([0-9]+) TE.*", "\\1", x)); te[is.na(te)] <- 1L
  fifelse(rb == 1 & te == 1, "11",
  fifelse(rb == 1 & te == 2, "12",
  fifelse(rb == 2 & te == 1, "21",
  fifelse(rb == 1 & te == 3, "13",
  fifelse(rb == 0, "empty", "other")))))
}

# Build the tip table over an arbitrary season window, so the old and new
# windows can be compared on identical code.
build_tip <- function(seasons, by_season = FALSE) {
  d <- d0[season %in% seasons]
  d <- merge(d, part, by = c("game_id","play_id"), all.x = TRUE)
  d <- d[!is.na(offense_formation) & offense_formation != ""]
  d[, pbk := pers_bucket(offense_personnel)]
  d[offense_formation == "EMPTY", pbk := "empty"]
  d[, look := paste(offense_formation, pbk, sep = ":")]
  grp <- if (by_season) c("off_play_caller","season") else "off_play_caller"

  cs_cell <- d[, .(n_cell = .N, p_raw = mean(is_pass), lg_p = lg_p[1]),
               by = c(grp, "cell")]
  cs_cell[, p_shr := (n_cell*p_raw + K*lg_p)/(n_cell + K)][, H_cell := Hbin(p_shr)]
  cs_look <- d[, .(n_cl = .N, p_raw = mean(is_pass)), by = c(grp, "cell", "look")]
  cs_look <- merge(cs_look, cs_cell[, c(grp, "cell", "p_shr"), with = FALSE],
                   by = c(grp, "cell"), all.x = TRUE, suffixes = c("", "_cell"))
  setnames(cs_look, "p_shr", "p_shr_cell")
  cs_look[, p_shr := (n_cl*p_raw + K*p_shr_cell)/(n_cl + K)][, H_cl := Hbin(p_shr)]

  a <- cs_cell[, .(H_situ_m = sum(H_cell*n_cell)/sum(n_cell), n_look = sum(n_cell)), by = grp]
  b <- cs_look[, .(H_look = sum(H_cl*n_cl)/sum(n_cl)), by = grp]
  out <- merge(a, b, by = grp)
  out[, presnap_tip := H_situ_m - H_look][]
}

old <- build_tip(2016:2023)
new <- build_tip(2016:2025)
sea <- build_tip(2016:2025, by_season = TRUE)

cat(sprintf("\ncallers: %d (old 2016-2023 window) vs %d (new 2016-2025 window)\n",
            nrow(old), nrow(new)))
cat(sprintf("plays behind the new window: %s\n", format(sum(new$n_look), big.mark = ",")))
write_csv(as.data.frame(new), "data/derived/presnap_tip_2025.csv")
write_csv(as.data.frame(sea), "data/derived/presnap_tip_season.csv")

# --- residualise both windows the same way (sample-size artifact, see R/07) ---
resid_of <- function(t) { t <- copy(t); t[, tip_resid := residuals(lm(presnap_tip ~ log(n_look)))]; t }
old <- resid_of(old); new <- resid_of(new)
cmp <- merge(old[, .(off_play_caller, old = tip_resid)],
             new[, .(off_play_caller, new = tip_resid, n_look)],
             by = "off_play_caller")
cmp[, `:=`(pct_old = 100*frank(old)/.N, pct_new = 100*frank(new)/.N)]
cat(sprintf("\nrank correlation between the two windows: %.3f\n",
            cor(cmp$pct_old, cmp$pct_new, method = "spearman")))
cat("\n--- biggest movers once 2024-2025 are added ---\n")
cmp[, move := pct_new - pct_old]
print(head(cmp[order(-abs(move)), .(off_play_caller, pct_old = round(pct_old),
                                    pct_new = round(pct_new), move = round(move))], 10))
cat("\n--- the names Michael raised ---\n")
print(cmp[off_play_caller %in% c("Sean McVay","Kyle Shanahan","Andy Reid",
                                 "Gary Kubiak","Klint Kubiak","Ben Johnson"),
          .(off_play_caller, pct_old = round(pct_old), pct_new = round(pct_new))])

# --- chart 1: McVay season by season -----------------------------------------
MIN_S <- 250
s <- sea[n_look >= MIN_S]
s[, z := (presnap_tip - mean(presnap_tip))/sd(presnap_tip), by = season]
mv <- s[off_play_caller == "Sean McVay"][order(season)]
cat("\n--- McVay by season (z = how telegraphed vs that season's league) ---\n")
print(mv[, .(season, n_look, presnap_tip = round(presnap_tip, 4), z = round(z, 2))])

band <- s[, .(lo = quantile(z, .25), hi = quantile(z, .75)), by = season][order(season)]
p1 <- ggplot() +
  geom_ribbon(data = band, aes(season, ymin = lo, ymax = hi),
              fill = "#9db6c9", alpha = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_line(data = mv, aes(season, z), colour = "#D55E00", linewidth = 1.1) +
  geom_point(data = mv, aes(season, z), colour = "#D55E00", size = 2.4) +
  annotate("text", x = min(band$season) + 0.15, y = -1.45, hjust = 0, size = 3.1,
           colour = "grey35", fontface = "bold",
           label = "shaded band = the middle half of play-callers that season") +
  scale_x_continuous(breaks = 2016:2025) +
  labs(
    title = "McVay swings from the most telegraphed caller in football to below average, year to year",
    subtitle = "How much Sean McVay's pre-snap look tells you about run or pass, against the league, each season",
    x = NULL, y = "standard deviations above the league\n→ more telegraphed",
    caption = fig_caption(
      "nflverse participation (formation and personnel) joined to caller-attributed play-by-play",
      sprintf("Play-callers with at least %d formation-coded plays in a season, 2016 to 2025.", MIN_S),
      paste0("\nMichael asked why a coach lauded for pre-snap movement reads as predictable. Two answers. First, motion is not the same as hiding the call: his run/pass choice is only 69th percentile,\n",
             "while his formations have carried real information. Second, and this is what the career average buries, there is no settled McVay to label. He ranges from 2.0 standard deviations\n",
             "above the league in 2018 to 1.3 below in 2022, with no trend, including a below-average 2024. Nick's read on text, that McVay evolves a lot year to year, is exactly what the seasons show.\n",
             "Any career number on this man is an average of ten different coaches. Built by R/09."))
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/presnap_mcvay.png", p1, w = 12, h = 6.6)

# --- chart 2: what the two extra seasons changed -----------------------------
lab <- cmp[order(-abs(move))][1:8]
p2 <- ggplot(cmp, aes(pct_old, pct_new)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.35) +
  geom_point(colour = "#2B8CBE", alpha = 0.55, size = 2) +
  geom_text_repel(data = lab, aes(label = off_play_caller), size = 3.1,
                  fontface = "bold", colour = "#1c5b80", seed = 4,
                  box.padding = 0.5, min.segment.length = 0, max.overlaps = 20) +
  scale_x_continuous(labels = function(x) paste0(x, "th")) +
  scale_y_continuous(labels = function(x) paste0(x, "th")) +
  labs(
    title = "Adding 2024 and 2025 moved individual coaches, but not the overall picture",
    subtitle = "Each play-caller's percentile on pre-snap tell, old 2016-2023 window against the refreshed 2016-2025 window",
    x = "percentile on the old window (through 2023)",
    y = "percentile with 2024 and 2025 added",
    caption = fig_caption(
      "nflverse participation, refreshed 2026-08-13",
      sprintf("%d play-callers present in both windows.", nrow(cmp)),
      paste0("\nAnswers Michael's 'a lot of them were 2022-2023 data, did you not have 2025'. He was right, and the cause was our stale cache: nflverse now publishes participation\n",
             "through 2025 and FTN charting through 2025, so every formation-based chart can be refreshed. The rankings mostly hold, which is reassuring rather than boring.\n",
             "Points far off the line are coaches whose recent seasons differ from their older ones. Built by R/09."))
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/presnap_freshness.png", p2, w = 12, h = 6.6)
