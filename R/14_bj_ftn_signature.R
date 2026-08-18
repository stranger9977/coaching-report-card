# =============================================================================
# 14_bj_ftn_signature.R
#
# Theory to test (verbatim, from the group): "we have a theory that ben
# johsnons play sequencing is unique." This is the CHARTED SIGNATURE piece,
# built on FTN charting 2022-2025, which covers Ben Johnson's entire
# play-calling career: Detroit offensive coordinator 2022-2024, Chicago head
# coach and play-caller 2025.
#
# Three tests:
#   1. THE INGREDIENT SIGNATURE -- usage rate and payoff (EPA/play) of six
#      FTN-charted ingredients (play action on early downs, motion, screens,
#      RPO, no-huddle, trick plays) for every qualified play-caller. Where
#      does Johnson rank on each, and where is he a true outlier rather than
#      merely above average?
#   2. DOES HIS PLAY ACTION NEED THE RUN? The league claim on the board is
#      that play action works without establishing the run. Tests Johnson's
#      early-down play-action EPA split by whether his own drive already had
#      a successful run before the play-action snap ("cold" PA vs "after a
#      successful run"), against the same split for the league.
#   3. SAME LOOK, DIFFERENT PLAY -- a coarse look proxy (qb_location x
#      offensive backfield count x motion, the only pre-snap picture FTN
#      charts without route or personnel data) crossed with down-distance
#      buckets. Within each caller's common looks, the entropy of the
#      run/dropback/play-action/screen mix that comes out of that look,
#      empirical-Bayes shrunk toward the caller's own down-distance mix so
#      thin cells cannot manufacture a "different play every time" reading.
#
# UNIVERSE: 2022-2025 regular season, competitive game states (vegas_wp
# 0.05-0.95), callers with at least ~1200 charted offensive plays over the
# window (35 of 65 qualify; Ben Johnson has 3,484, the 5th most). Four
# seasons is a short career to call a "signature" off, so every ranked claim
# below ships with a bootstrap noise band, not just a point estimate.
#
# DRIVE HISTORY CAVEAT: the "was there already a successful run this drive"
# flag for test 2 is computed on the full pass/run play sequence BEFORE the
# competitive-game-state filter is applied, so a drive's run history is never
# broken by trimming a blowout play from the middle of it. The competitive
# filter is applied afterward, to the analysis sample only.
#
# Data: FTN charting 2022-2024 read from the local nfl-analysis cache; 2025
# via nflreadr::load_ftn_charting(), cached to data/raw/ftn_2025.csv.gz
# (gitignored) so reruns are offline; falls back to the local nfl-analysis
# copy if the network pull is unavailable. Joined to nfl-analysis
# pbp_slim.rds for epa/down/drive/success, and to playcallers.csv for caller
# attribution (2026 rows are unplayed and dropped by the season filter; Ben
# Johnson is DET 2022-2024, CHI 2025, one name, no in-season caller changes).
#
# NOTE ON FILENAME: the brief asked for R/08_bj_ftn_signature.R, but 08 is
# already 08_by_the_book.R (unrelated, pre-existing). This is 13, the next
# free number; 09_bj_script.R is a sibling piece on the same theory (the
# opening-script test), already in the repo from another pass.
#
# Out: docs/figures/bj_ingredients.png
#      docs/figures/bj_pa_cold.png
#      docs/figures/bj_same_look.png
#      data/derived/bj_ftn_callers.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
YRS <- 2022:2025
QUAL_MIN <- 1200
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)

safe_log2 <- function(p) fifelse(p > 0, log2(p), 0)
ordsuf <- function(n) {
  n <- round(n)
  if (n %% 100 %in% 11:13) return("th")
  switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
}
ord <- function(n) paste0(round(n), ordsuf(n))

# ---------------------------------------------------------------- FTN charting, cached
ftn_cache <- "data/raw/ftn_2025.csv.gz"
if (!file.exists(ftn_cache)) {
  ftn25 <- tryCatch(as.data.table(nflreadr::load_ftn_charting(2025)),
                     error = function(e) NULL)
  if (is.null(ftn25) || nrow(ftn25) == 0) {
    cat("nflreadr pull unavailable, falling back to local nfl-analysis FTN 2025 copy\n")
    ftn25 <- fread(file.path(D, "ftn_charting_2025.csv.gz"), showProgress = FALSE)
  }
  fwrite(ftn25, ftn_cache)
  cat(sprintf("cached FTN 2025 (%s plays) to %s\n", format(nrow(ftn25), big.mark = ","), ftn_cache))
}

FTN_COLS <- c("nflverse_game_id", "nflverse_play_id", "qb_location",
              "n_offense_backfield", "is_no_huddle", "is_motion",
              "is_play_action", "is_screen_pass", "is_rpo", "is_trick_play")
ftn <- rbindlist(c(
  lapply(2022:2024, function(y) fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
                                       select = FTN_COLS, showProgress = FALSE)),
  list(fread(ftn_cache, select = FTN_COLS, showProgress = FALSE))
))
setnames(ftn, c("nflverse_game_id", "nflverse_play_id"), c("game_id", "play_id"))
cat(sprintf("FTN charted plays 2022-2025: %s\n", format(nrow(ftn), big.mark = ",")))

# ---------------------------------------------------------------- pbp + drive history + callers
pbp <- as.data.table(readRDS(file.path(D, "pbp_slim.rds")))[
  season %in% YRS & season_type == "REG" & play_type %in% c("pass", "run") &
  qb_kneel == 0 & qb_spike == 0 & !is.na(epa)]

pc <- fread(file.path(D, "playcallers.csv"), showProgress = FALSE,
            select = c("season", "week", "team", "game_id", "off_play_caller"))
pc[, off_play_caller := trimws(off_play_caller)]
pbp <- merge(pbp, pc, by.x = c("game_id", "season", "week", "posteam"),
             by.y = c("game_id", "season", "week", "team"), all.x = TRUE)

# drive history computed on the FULL pass/run sequence, before the
# competitive-game-state filter, so trimming garbage time never breaks a
# drive's "was there already a successful run" answer.
setorder(pbp, game_id, play_id)
pbp[, drive_key := paste(game_id, drive)]
pbp[, prior_succ_run := shift(cumsum(play_type == "run" & success == 1), fill = 0) > 0, by = drive_key]

d_all <- merge(pbp, ftn, by = c("game_id", "play_id"))
d <- d_all[vegas_wp >= .05 & vegas_wp <= .95 & !is.na(off_play_caller) & off_play_caller != ""]
cat(sprintf("charted offensive plays joined to a caller, 2022-2025 REG, competitive: %s\n",
            format(nrow(d), big.mark = ",")))

QUAL <- d[, .N, by = off_play_caller][N >= QUAL_MIN, off_play_caller]
dq <- d[off_play_caller %in% QUAL]
cat(sprintf("qualified callers (>= %d charted plays): %d of %d\n",
            QUAL_MIN, length(QUAL), uniqueN(d$off_play_caller)))
cat(sprintf("Ben Johnson charted plays: %d (rank %d of %d by volume)\n",
            dq[off_play_caller == "Ben Johnson", .N],
            frank(-d[, .N, by = off_play_caller]$N)[d[, .N, by = off_play_caller]$off_play_caller == "Ben Johnson"],
            length(QUAL)))

dq[, `:=`(is_pa = as.logical(is_play_action), is_motion_l = as.logical(is_motion),
          is_screen = as.logical(is_screen_pass), is_rpo_l = as.logical(is_rpo),
          is_nh = as.logical(is_no_huddle), is_trick = as.logical(is_trick_play))]

# =============================================================================
# TEST 1: the ingredient signature
# =============================================================================
cat("\n\n=== TEST 1: THE INGREDIENT SIGNATURE ===\n")

rate_pa     <- function(x) 100 * sum(x$is_pa & x$down %in% c(1, 2) & x$play_type == "pass", na.rm = TRUE) /
                            sum(x$down %in% c(1, 2) & x$play_type == "pass", na.rm = TRUE)
rate_motion <- function(x) 100 * mean(x$is_motion_l, na.rm = TRUE)
rate_screen <- function(x) 100 * sum(x$is_screen & x$play_type == "pass", na.rm = TRUE) / sum(x$play_type == "pass", na.rm = TRUE)
rate_rpo    <- function(x) 100 * mean(x$is_rpo_l, na.rm = TRUE)
rate_nh     <- function(x) 100 * mean(x$is_nh, na.rm = TRUE)
rate_trick  <- function(x) 100 * mean(x$is_trick, na.rm = TRUE)
RATE_FUN <- list(`Play action (early downs)` = rate_pa, `Motion` = rate_motion,
                  `Screens` = rate_screen, `RPO` = rate_rpo,
                  `No-huddle` = rate_nh, `Trick plays` = rate_trick)
ING <- names(RATE_FUN)

ing <- dq[, .(
  n_plays = .N,
  `Play action (early downs)` = rate_pa(.SD),
  `Motion` = rate_motion(.SD),
  `Screens` = rate_screen(.SD),
  `RPO` = rate_rpo(.SD),
  `No-huddle` = rate_nh(.SD),
  `Trick plays` = rate_trick(.SD)
), by = off_play_caller]

epa_ing <- dq[, .(
  epa_pa      = mean(epa[is_pa & down %in% c(1, 2) & play_type == "pass"], na.rm = TRUE),
  epa_motion  = mean(epa[is_motion_l], na.rm = TRUE),
  epa_screen  = mean(epa[is_screen & play_type == "pass"], na.rm = TRUE),
  epa_rpo     = mean(epa[is_rpo_l], na.rm = TRUE),
  epa_nh      = mean(epa[is_nh], na.rm = TRUE),
  epa_trick   = mean(epa[is_trick], na.rm = TRUE)
), by = off_play_caller]
setnames(epa_ing, c("off_play_caller", ING))

ing_long <- melt(ing, id.vars = c("off_play_caller", "n_plays"), measure.vars = ING,
                  variable.name = "ingredient", value.name = "rate")
ing_long[, `:=`(rank = frank(-rate), pct = 100 * frank(rate) / .N, n_callers = .N), by = ingredient]

epa_long <- melt(epa_ing, id.vars = "off_play_caller", measure.vars = ING,
                  variable.name = "ingredient", value.name = "epa_payoff")
epa_long[, `:=`(epa_rank = frank(-epa_payoff), epa_pct = 100 * frank(epa_payoff) / .N), by = ingredient]

# bootstrap CI on Johnson's rate, clustered by game so within-game
# correlation doesn't understate the noise
boot_rate_ci <- function(sub, fn, B = 1000, seed = 11) {
  set.seed(seed)
  gids <- unique(sub$game_id)
  idx_by_game <- split(seq_len(nrow(sub)), sub$game_id)
  out <- vapply(seq_len(B), function(b) {
    sg <- sample(gids, length(gids), replace = TRUE)
    fn(sub[unlist(idx_by_game[sg], use.names = FALSE)])
  }, numeric(1))
  quantile(out, c(.025, .975), na.rm = TRUE)
}
bj_sub <- dq[off_play_caller == "Ben Johnson"]
ci_tab <- rbindlist(lapply(ING, function(g) {
  ci <- boot_rate_ci(bj_sub, RATE_FUN[[g]])
  data.table(ingredient = g, ci_lo = ci[1], ci_hi = ci[2])
}))

bj1 <- merge(ing_long[off_play_caller == "Ben Johnson"],
             ing_long[, .(lg_median = median(rate), lg_mean = mean(rate), lg_sd = sd(rate)), by = ingredient],
             by = "ingredient")
bj1 <- merge(bj1, ci_tab, by = "ingredient")
bj1 <- merge(bj1, epa_long[off_play_caller == "Ben Johnson", .(ingredient, epa_payoff, epa_rank, epa_pct)],
             by = "ingredient")
bj1[, z := (rate - lg_mean) / lg_sd]
bj1[, direction := fifelse(z >= 0, "above", "below")]
bj1[, outlier_flag := fifelse(abs(z) >= 1.5, paste("TRUE OUTLIER,", direction, "the league"),
                       fifelse(abs(z) >= 0.75, paste(direction, "average"), "league-normal"))]
bj1[, ci_crosses_median := ci_lo <= lg_median & ci_hi >= lg_median]
bj1 <- bj1[order(-abs(z))]

cat("\n--- Ben Johnson, six ingredients, ranked by how extreme he is ---\n")
for (i in seq_len(nrow(bj1))) {
  r <- bj1[i]
  cat(sprintf("%-28s rank #%2d/%2d (%s pct)  rate %5.1f%%  [95%% CI %.1f-%.1f]  league median %5.1f%%  z=%+.2f  %s%s\n",
              r$ingredient, r$rank, r$n_callers, ord(r$pct), r$rate, r$ci_lo, r$ci_hi, r$lg_median, r$z,
              r$outlier_flag, if (r$ci_crosses_median) "  (CI crosses league median, rank could be luck)" else ""))
  cat(sprintf("%-28s EPA/play when used: %+.3f  (payoff rank #%d/%d, %s pct)\n",
              "", r$epa_payoff, r$epa_rank, r$n_callers, ord(r$epa_pct)))
}
n_outlier <- sum(abs(bj1$z) >= 1.5)
cat(sprintf("\nVERDICT (test 1): %d of 6 ingredients are true outliers (|z| >= 1.5): %s. The rest are league-normal to above/below average.\n",
            n_outlier, if (n_outlier) paste(bj1[abs(z) >= 1.5, ingredient], collapse = ", ") else "none"))

# =============================================================================
# TEST 2: does his play action need the run?
# =============================================================================
cat("\n\n=== TEST 2: DOES HIS PLAY ACTION NEED THE RUN? ===\n")

pa_plays <- dq[play_type == "pass" & is_pa == TRUE & down %in% c(1, 2) & !is.na(epa)]
pa_plays[, cond := fifelse(prior_succ_run, "After a successful run", "Cold (no successful run yet)")]
cat(sprintf("early-down play-action dropbacks, qualified callers: %s\n", format(nrow(pa_plays), big.mark = ",")))

pa_caller <- pa_plays[, .(n = .N, epa = mean(epa), se = sd(epa) / sqrt(.N)), by = .(off_play_caller, cond)]
wide <- dcast(pa_caller, off_play_caller ~ cond, value.var = c("n", "epa"))
setnames(wide, make.names(names(wide)))
wide <- wide[!is.na(epa_After.a.successful.run) & !is.na(epa_Cold..no.successful.run.yet.) &
             n_After.a.successful.run >= 30 & n_Cold..no.successful.run.yet. >= 30]
wide[, gap := epa_After.a.successful.run - epa_Cold..no.successful.run.yet.]
cat(sprintf("callers with >=30 PA dropbacks in BOTH conditions: %d\n", nrow(wide)))
cat(sprintf("league gap (after-run minus cold), median %+.3f, IQR [%+.3f, %+.3f], range [%+.3f, %+.3f]\n",
            median(wide$gap), quantile(wide$gap, .25), quantile(wide$gap, .75), min(wide$gap), max(wide$gap)))

bj_pa <- pa_plays[off_play_caller == "Ben Johnson"]
bj_cold <- bj_pa[prior_succ_run == FALSE]; bj_warm <- bj_pa[prior_succ_run == TRUE]
boot_gap <- function(x, B = 2000, seed = 12) {
  set.seed(seed)
  gids <- unique(x$game_id)
  idx_by_game <- split(seq_len(nrow(x)), x$game_id)
  vapply(seq_len(B), function(b) {
    sg <- sample(gids, length(gids), replace = TRUE)
    xb <- x[unlist(idx_by_game[sg], use.names = FALSE)]
    mean(xb$epa[xb$prior_succ_run]) - mean(xb$epa[!xb$prior_succ_run])
  }, numeric(1))
}
bg <- boot_gap(bj_pa)
gap_ci <- quantile(bg, c(.025, .975), na.rm = TRUE)
bj_gap <- mean(bj_warm$epa) - mean(bj_cold$epa)

cat(sprintf("\nBen Johnson cold PA:      n=%d  EPA/play %+.3f  [95%% CI %+.3f, %+.3f]\n",
            nrow(bj_cold), mean(bj_cold$epa), mean(bj_cold$epa) - 1.96 * sd(bj_cold$epa) / sqrt(nrow(bj_cold)),
            mean(bj_cold$epa) + 1.96 * sd(bj_cold$epa) / sqrt(nrow(bj_cold))))
cat(sprintf("Ben Johnson after-run PA: n=%d  EPA/play %+.3f  [95%% CI %+.3f, %+.3f]\n",
            nrow(bj_warm), mean(bj_warm$epa), mean(bj_warm$epa) - 1.96 * sd(bj_warm$epa) / sqrt(nrow(bj_warm)),
            mean(bj_warm$epa) + 1.96 * sd(bj_warm$epa) / sqrt(nrow(bj_warm))))
cat(sprintf("Ben Johnson gap (after-run minus cold): %+.3f  [bootstrap 95%% CI %+.3f, %+.3f]\n", bj_gap, gap_ci[1], gap_ci[2]))
bj_pct_of_gap <- 100 * mean(wide$gap <= bj_gap)
cat(sprintf("his gap sits at the %.0fth percentile of the %d-caller league distribution\n", bj_pct_of_gap, nrow(wide)))
VERDICT2 <- if (gap_ci[1] <= 0 && gap_ci[2] >= 0) "not distinguishable from zero on his own sample" else
            if (bj_pct_of_gap >= 25 && bj_pct_of_gap <= 75) "ordinary, sits in the middle half of the league" else
            "unusual relative to the league"
cat(sprintf("VERDICT (test 2): Johnson's cold-vs-after-run PA split is %s.\n", VERDICT2))

# =============================================================================
# TEST 3: same look, different play
# =============================================================================
cat("\n\n=== TEST 3: SAME LOOK, DIFFERENT PLAY ===\n")

dq3 <- copy(dq)
dq3[, backfield_b := fifelse(n_offense_backfield >= 3, "3+", as.character(n_offense_backfield))]
dq3 <- dq3[!is.na(backfield_b) & !is.na(qb_location) & qb_location %in% c("P", "S", "U")]
dq3[, look := paste(qb_location, backfield_b, is_motion_l, sep = "|")]
dq3[, dd_bucket := fifelse(down == 1, "1st-and-10",
                    fifelse(down == 2 & ydstogo <= 3, "2nd-short",
                    fifelse(down == 2 & ydstogo <= 7, "2nd-medium",
                    fifelse(down == 2, "2nd-long",
                    fifelse(down == 3 & ydstogo <= 3, "3rd-short",
                    fifelse(down == 3 & ydstogo <= 7, "3rd-medium",
                    fifelse(down == 3, "3rd-long", NA_character_)))))))]
dq3 <- dq3[!is.na(dd_bucket)]
dq3[, play_cat := fifelse(is_screen & play_type == "pass", "screen",
                   fifelse(is_pa & play_type == "pass", "pa",
                   fifelse(play_type == "pass", "dropback",
                   fifelse(play_type == "run", "run", NA_character_))))]
dq3 <- dq3[!is.na(play_cat)]
CAT_KEYS <- c("run", "dropback", "pa", "screen")
BUCKET_ORDER <- c("1st-and-10", "2nd-short", "2nd-medium", "2nd-long", "3rd-short", "3rd-medium", "3rd-long")
K <- 15   # EB shrinkage strength, same convention as R/09's presnap tip
MIN_CELL <- 10

cat(sprintf("plays with a usable look proxy (qb_location P/S/U, backfield known, down-distance bucketed): %s\n",
            format(nrow(dq3), big.mark = ",")))

entropy_table <- function(x) {
  # parent: caller x bucket play-type mix (the caller's own baseline for that
  # down-distance situation, what shrinkage pulls thin look-cells toward)
  parent <- x[, .N, by = .(off_play_caller, dd_bucket, play_cat)]
  parent <- dcast(parent, off_play_caller + dd_bucket ~ play_cat, value.var = "N", fill = 0)
  for (k in CAT_KEYS) if (!k %in% names(parent)) parent[[k]] <- 0
  parent[, n_parent := Reduce(`+`, .SD), .SDcols = CAT_KEYS]
  for (k in CAT_KEYS) parent[[paste0("p_", k)]] <- parent[[k]] / parent$n_parent

  cell <- x[, .N, by = .(off_play_caller, dd_bucket, look, play_cat)]
  cell <- dcast(cell, off_play_caller + dd_bucket + look ~ play_cat, value.var = "N", fill = 0)
  for (k in CAT_KEYS) if (!k %in% names(cell)) cell[[k]] <- 0
  cell[, n_cell := Reduce(`+`, .SD), .SDcols = CAT_KEYS]
  cell <- merge(cell, parent[, c("off_play_caller", "dd_bucket", paste0("p_", CAT_KEYS)), with = FALSE],
                by = c("off_play_caller", "dd_bucket"))
  cell <- copy(cell)
  for (k in CAT_KEYS) cell[[paste0("p_shr_", k)]] <- (cell[[k]] + K * cell[[paste0("p_", k)]]) / (cell$n_cell + K)
  cell[, H_shr := -(p_shr_run * safe_log2(p_shr_run) + p_shr_dropback * safe_log2(p_shr_dropback) +
                     p_shr_pa * safe_log2(p_shr_pa) + p_shr_screen * safe_log2(p_shr_screen))]
  cell
}
cell <- entropy_table(dq3)
cell_q <- cell[n_cell >= MIN_CELL & off_play_caller %in% QUAL]
cat(sprintf("look x down-distance-bucket cells with >= %d plays: %s\n", MIN_CELL, format(nrow(cell_q), big.mark = ",")))

caller_H <- cell_q[, .(n_cells = .N, n_plays_counted = sum(n_cell), H_mean = weighted.mean(H_shr, n_cell)),
                    by = off_play_caller][n_plays_counted >= 500]
setorder(caller_H, -H_mean)
caller_H[, rank := frank(-H_mean)]
cat(sprintf("callers scored (>= 500 counted plays across common looks): %d\n", nrow(caller_H)))
cat(sprintf("league median same-look entropy: %.3f bits (max possible with 4 categories: %.3f bits)\n",
            median(caller_H$H_mean), log2(4)))

bj3 <- caller_H[off_play_caller == "Ben Johnson"]
cat(sprintf("\nBen Johnson same-look entropy: %.3f bits, rank #%d of %d, n=%d cells / %s plays counted\n",
            bj3$H_mean, bj3$rank, nrow(caller_H), bj3$n_cells, format(bj3$n_plays_counted, big.mark = ",")))

# bootstrap CI on Johnson's own H_mean, clustered by game, rebuilding
# shrinkage from scratch each draw so the CI reflects both cell noise and how
# much the shrinkage target itself moves
boot_H <- function(sub, B = 300, seed = 13) {
  set.seed(seed)
  gids <- unique(sub$game_id)
  idx_by_game <- split(seq_len(nrow(sub)), sub$game_id)
  vapply(seq_len(B), function(b) {
    sg <- sample(gids, length(gids), replace = TRUE)
    xb <- sub[unlist(idx_by_game[sg], use.names = FALSE)]
    cb <- entropy_table(xb)
    cb <- cb[n_cell >= MIN_CELL]
    if (!nrow(cb)) return(NA_real_)
    weighted.mean(cb$H_shr, cb$n_cell)
  }, numeric(1))
}
bjH_boot <- boot_H(dq3[off_play_caller == "Ben Johnson"])
bjH_ci <- quantile(bjH_boot, c(.025, .975), na.rm = TRUE)
cat(sprintf("bootstrap 95%% CI on his entropy: [%.3f, %.3f]\n", bjH_ci[1], bjH_ci[2]))
lg_median_in_ci <- bjH_ci[1] <= median(caller_H$H_mean) & bjH_ci[2] >= median(caller_H$H_mean)
cat(sprintf("VERDICT (test 3): rank #%d of %d is %s (CI %s the league median).\n",
            bj3$rank, nrow(caller_H),
            if (lg_median_in_ci) "not distinguishable from an average caller" else "likely real, not just luck",
            if (lg_median_in_ci) "crosses" else "does not cross"))

# Johnson's own breakdown by down-distance bucket, for the second chart panel
cb <- cell_q[, .(n = sum(n_cell), H = weighted.mean(H_shr, n_cell)), by = .(off_play_caller, dd_bucket)][n >= 50]
lg_bucket <- cb[, .(H_lg_median = median(H), n_callers = .N), by = dd_bucket]
bj_bucket <- cb[off_play_caller == "Ben Johnson"]
bj_bucket <- merge(bj_bucket, lg_bucket, by = "dd_bucket")
bj_bucket[, dd_bucket := factor(dd_bucket, levels = BUCKET_ORDER)]
cat("\n--- Ben Johnson's same-look diversity by down-distance bucket, vs league median ---\n")
print(bj_bucket[order(dd_bucket), .(dd_bucket, n, H = round(H, 3), H_lg_median = round(H_lg_median, 3),
                                     diff = round(H - H_lg_median, 3))])

# =============================================================================
# write the combined caller table
# =============================================================================
out <- merge(ing[, .(off_play_caller, n_plays)], caller_H[, .(off_play_caller, same_look_H = H_mean, same_look_rank = rank)],
             by = "off_play_caller", all.x = TRUE)
out <- merge(out, dcast(ing_long, off_play_caller ~ ingredient, value.var = "rate"), by = "off_play_caller")
setnames(out, ING, paste0("rate_", make.names(ING)))
write_csv(as.data.frame(out), "data/derived/bj_ftn_callers.csv")
cat(sprintf("\nwrote data/derived/bj_ftn_callers.csv (%d callers)\n", nrow(out)))

# =============================================================================
# CHART A: the ingredient signature, one panel
# =============================================================================
ing_long[, ingredient := factor(ingredient, levels = rev(ING))]
bj_pts <- ing_long[off_play_caller == "Ben Johnson"]
bj_pts[, lab := sprintf("#%d/%d  %.1f%%", rank, n_callers, rate)]

pA <- ggplot(ing_long, aes(pct, ingredient)) +
  geom_vline(xintercept = 50, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_jitter(height = 0.16, width = 0, colour = "#9db6c9", alpha = 0.55, size = 2.1) +
  geom_point(data = bj_pts, colour = "#D55E00", size = 4.2) +
  geom_text(data = bj_pts, aes(label = lab), hjust = 0, nudge_x = 3.5, size = 3.1,
            fontface = "bold", colour = "#8a3d00") +
  scale_x_continuous(limits = c(0, 118), breaks = c(0, 25, 50, 75, 100),
                      labels = c("least", "25th", "league\nmedian", "75th", "most")) +
  labs(
    title = sprintf("Ben Johnson is a true outlier on %d of 6 ingredients, league-normal to above-average on the rest",
                     n_outlier),
    subtitle = "Each grey dot is one qualified play-caller's usage rate, 2022 to 2025. Orange is Ben Johnson, with his rank and rate.",
    x = "percentile among qualified callers", y = NULL,
    caption = fig_caption(
      "FTN charting 2022-2025 + nflverse play-by-play, caller IDs from samhoppen/NFL_public",
      sprintf("%d callers, %d+ charted plays, 2022-2025 regular season, competitive states (win prob 5-95%%).",
              length(QUAL), QUAL_MIN),
      paste0("\nPlay action and no-huddle are true outliers (|z| >= 1.5 vs the league); the rest sit league-normal to above/below average.\n",
             "Rates are bootstrap-noisy in thin categories like trick plays; see the printed table for confidence intervals. Built by R/14.")
    )
  ) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.95), face = "bold"))

save_fig("docs/figures/bj_ingredients.png", pA, w = 12, h = 6.8)

# =============================================================================
# CHART B: cold PA vs after-run PA, Johnson against the league range
# =============================================================================
league_long <- rbind(
  wide[, .(off_play_caller, epa = epa_Cold..no.successful.run.yet., cond = "Cold\n(no successful run yet)")],
  wide[, .(off_play_caller, epa = epa_After.a.successful.run, cond = "After a\nsuccessful run")]
)
league_long[, cond := factor(cond, levels = c("Cold\n(no successful run yet)", "After a\nsuccessful run"))]

bj_df <- data.table(
  cond = factor(c("Cold\n(no successful run yet)", "After a\nsuccessful run"),
                levels = c("Cold\n(no successful run yet)", "After a\nsuccessful run")),
  epa = c(mean(bj_cold$epa), mean(bj_warm$epa)),
  lo  = c(mean(bj_cold$epa) - 1.96 * sd(bj_cold$epa) / sqrt(nrow(bj_cold)),
          mean(bj_warm$epa) - 1.96 * sd(bj_warm$epa) / sqrt(nrow(bj_warm))),
  hi  = c(mean(bj_cold$epa) + 1.96 * sd(bj_cold$epa) / sqrt(nrow(bj_cold)),
          mean(bj_warm$epa) + 1.96 * sd(bj_warm$epa) / sqrt(nrow(bj_warm)))
)

pB <- ggplot() +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.35) +
  geom_boxplot(data = league_long, aes(epa, cond), width = 0.42, colour = "grey55",
               fill = "grey93", outlier.shape = NA) +
  geom_jitter(data = league_long, aes(epa, cond), height = 0.11, colour = "#9db6c9",
              alpha = 0.55, size = 1.7) +
  geom_line(data = bj_df, aes(epa, cond, group = 1), colour = "#D55E00", linewidth = 0.9) +
  geom_errorbarh(data = bj_df, aes(xmin = lo, xmax = hi, y = cond), height = 0.16,
                 colour = "#D55E00", linewidth = 1) +
  geom_point(data = bj_df, aes(epa, cond), colour = "#D55E00", size = 4.4) +
  labs(
    title = sprintf("Johnson's early-down play action gains %+.2f EPA once a run has already worked, %s",
                     bj_gap, if (grepl("not distinguishable", VERDICT2)) "not a clean signal on his own sample" else if (bj_pct_of_gap >= 25 & bj_pct_of_gap <= 75) "an ordinary-sized gap for the league" else "a large gap for the league"),
    subtitle = sprintf("EPA per early-down play-action dropback, split by whether the drive already had a successful run. Grey = %d qualified callers, orange = Ben Johnson.", nrow(wide)),
    x = "EPA per play (orange bars are 95% CI)", y = NULL,
    caption = fig_caption(
      "FTN charting + play-by-play, 2022-2025 regular season, competitive states",
      sprintf("%d callers, 30+ early-down PA dropbacks per condition. League gap median %+.3f, IQR [%+.3f, %+.3f].",
              nrow(wide), median(wide$gap), quantile(wide$gap, .25), quantile(wide$gap, .75)),
      paste0("\nThe board's claim is that play action works without establishing the run. Johnson's own gap sits at the ", ord(bj_pct_of_gap), " percentile\n",
             sprintf("of the league, bootstrap 95%% CI [%+.3f, %+.3f]. Built by R/14.", gap_ci[1], gap_ci[2]))
    )
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/bj_pa_cold.png", pB, w = 12, h = 6.2)

# =============================================================================
# CHART C: same look, different play
# =============================================================================
caller_H[, hl := off_play_caller == "Ben Johnson"]
lab_c <- caller_H[hl == TRUE]
lab_c[, lab := sprintf("Ben Johnson\n#%d of %d", rank, nrow(caller_H))]

pC1 <- ggplot(caller_H, aes(reorder(off_play_caller, H_mean), H_mean)) +
  geom_hline(yintercept = median(caller_H$H_mean), linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(colour = hl, size = hl)) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#9db6c9"), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 4.4, `FALSE` = 2), guide = "none") +
  geom_text(data = lab_c, aes(label = lab), nudge_y = 0.045, size = 3.1, fontface = "bold",
            colour = "#8a3d00", lineheight = 0.9) +
  labs(title = "How many different plays come off the same picture",
       subtitle = "Each dot is one caller's plays-weighted average entropy across his common looks",
       x = NULL, y = "same-look entropy (bits, shrunk)") +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_blank())

bj_bucket_long <- melt(bj_bucket[, .(dd_bucket, `Ben Johnson` = H, `League median` = H_lg_median)],
                       id.vars = "dd_bucket", variable.name = "who", value.name = "H")
pC2 <- ggplot(bj_bucket_long, aes(dd_bucket, H, colour = who, group = who)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c(`Ben Johnson` = "#D55E00", `League median` = "#9db6c9")) +
  labs(title = "Where the diversity shows up",
       subtitle = "Same-look entropy by down-distance bucket",
       x = NULL, y = "entropy (bits, shrunk)") +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = rel(0.78)),
        legend.position = "top", legend.title = element_blank(), legend.justification = "left")

pC <- (pC1 | pC2) + plot_layout(widths = c(1.15, 1)) +
  plot_annotation(
    title = sprintf("Ben Johnson's same-look diversity ranks #%d of %d, %s",
                     bj3$rank, nrow(caller_H),
                     if (lg_median_in_ci) "but the CI can't separate him from an average caller" else "and the gap survives a bootstrap"),
    subtitle = "Look proxy = QB location x offensive backfield count x motion (coarse: FTN has no route or personnel data), 2022 to 2025",
    caption = fig_caption(
      "FTN charting + play-by-play, 2022-2025 regular season, competitive states",
      sprintf("%d callers with 500+ plays across look-cells of 10+ plays.", nrow(caller_H)),
      paste0(sprintf("\nEntropy is shrunk toward each caller's own down-distance mix (K=%d pseudo-plays) so thin cells can't inflate the score.\n", K),
             "Higher = a wider mix of run, dropback pass, play action and screen off the same pre-snap picture", sprintf(" (max possible: %.2f bits).\n", log2(4)),
             sprintf("Bootstrap 95%% CI on Johnson's score: [%.3f, %.3f]. Built by R/14.", bjH_ci[1], bjH_ci[2]))
    ),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/bj_same_look.png", pC, w = 13.6, h = 6.8)

cat("\n\n=== DONE ===\n")
cat("wrote docs/figures/bj_ingredients.png, docs/figures/bj_pa_cold.png, docs/figures/bj_same_look.png\n")
cat("wrote data/derived/bj_ftn_callers.csv\n")
