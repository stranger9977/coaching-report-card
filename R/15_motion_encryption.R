# =============================================================================
# 15_motion_encryption.R
#
# Michael, on McVay and Shanahan's pre-snap movement: "What ways do you think
# we can quantify the McVay pre-snap movement and Shanahan?"
# Nick's reply: "Does the model struggle to predict when those two have
# different motion types pre snap. Struggle to predict run v pass, play
# action etc."
#
# Built exactly that. A pre-snap-only model predicts run vs pass (and,
# separately, play action) from situation plus FTN's pre-snap flags. Every
# play gets an out-of-fold prediction, so no play is scored by a model that
# saw its own outcome. Group by play-caller and split each caller's plays
# into motion and no-motion: if motion is "encryption," the model's log-loss
# should be HIGHER on that caller's motion plays than his still plays. Most
# callers should show the opposite (motion plays telegraph, if anything,
# since teams motion more from obvious personnel/formation looks). The test
# is whether McVay, Shanahan and Ben Johnson sit apart from the pack.
#
# CAVEAT carried from the brief: FTN's is_motion is a binary flag, not a
# motion type. Jet motion, orbit motion, and a simple shift are all coded
# identically. This measures whether a play-caller motions, not what kind of
# motion he uses, and the chart says so.
#
# Sources: ~/stranger9977/nfl-analysis/data/ftn_charting_{2022,2023,2024,
# 2025}.csv.gz (FTN pre-snap charting), play_by_play_{2022..2025}.csv.gz
# (nflverse pbp, situation + EPA), playcallers.csv (off_play_caller by
# team-game, 2022-2025 rows only; 2026 is unplayed).
#
# Method: xgboost, 5-fold cross-validation split by GAME (not by play) so no
# fold ever trains and tests on plays from the same game. Regular season,
# called run/pass plays only (kneels, spikes, two-point tries, special teams
# excluded). Play-action model runs on dropbacks only. No hyperparameter
# tuning was done; params are reasonable defaults, not a search result.
#
# Out: docs/figures/motion_encryption.png
#      docs/figures/motion_pa.png
#      data/derived/motion_callers.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost)
})
source("R/lib/theme_coach.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
YRS <- 2022:2025
SEED <- 2024
HIGHLIGHT <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")
MIN_RP <- 1200   # run/pass model, min called plays per caller (per brief)
MIN_PA <- 800    # play-action model, min dropbacks per caller

# --- load ---------------------------------------------------------------
pbp <- rbindlist(lapply(YRS, function(y) fread(file.path(D, sprintf("play_by_play_%d.csv.gz", y)),
  select = c("game_id","play_id","posteam","down","ydstogo","yardline_100",
             "score_differential","half_seconds_remaining","qtr",
             "posteam_timeouts_remaining","defteam_timeouts_remaining",
             "rush","pass","qb_dropback","qb_kneel","qb_spike","special",
             "season_type","two_point_attempt","epa"))))

ftn <- rbindlist(lapply(YRS, function(y) fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
  select = c("nflverse_game_id","nflverse_play_id","is_motion","is_no_huddle",
             "qb_location","n_offense_backfield","is_play_action"))))
setnames(ftn, c("nflverse_game_id","nflverse_play_id"), c("game_id","play_id"))

pc <- fread(file.path(D, "playcallers.csv"))[season %in% YRS,
      .(game_id, team, off_play_caller)]
stopifnot(!anyDuplicated(pc, by = c("game_id","team")))

d <- merge(pbp, ftn, by = c("game_id","play_id"))
d[pc, off_play_caller := i.off_play_caller, on = c(game_id = "game_id", posteam = "team")]
cat(sprintf("plays with pbp+FTN+play-caller joined: %s (2022-2025 regular + postseason)\n",
            format(nrow(d), big.mark = ",")))

# --- universes ------------------------------------------------------------
uni1 <- d[season_type == "REG" & qb_kneel == 0 & qb_spike == 0 & special == 0 &
          two_point_attempt == 0 & (rush == 1 | pass == 1) & !is.na(down) &
          !is.na(off_play_caller)]
uni2 <- d[season_type == "REG" & qb_dropback == 1 & qb_kneel == 0 & qb_spike == 0 &
          special == 0 & two_point_attempt == 0 & !is.na(down) &
          !is.na(off_play_caller)]
cat(sprintf("run/pass universe: %s called plays. play-action universe: %s dropbacks.\n",
            format(nrow(uni1), big.mark = ","), format(nrow(uni2), big.mark = ",")))

# --- shared game-level 5-fold split (no game crosses a fold) ---------------
games_all <- unique(c(uni1$game_id, uni2$game_id))
set.seed(SEED)
fold_tbl <- data.table(game_id = games_all,
                        fold = sample(rep(1:5, length.out = length(games_all))))
uni1[fold_tbl, fold := i.fold, on = "game_id"]
uni2[fold_tbl, fold := i.fold, on = "game_id"]

# --- feature builder: situation + FTN pre-snap flags only, never the caller ---
FEAT_SITU <- c("down","ydstogo","yardline_100","score_differential",
               "half_seconds_remaining","qtr",
               "posteam_timeouts_remaining","defteam_timeouts_remaining")

build_X <- function(dt) {
  qbloc <- factor(ifelse(is.na(dt$qb_location) | dt$qb_location == "", "unk", dt$qb_location),
                   levels = c("0","P","S","U","unk"))
  qbloc_mm <- model.matrix(~ qbloc - 1, data = data.frame(qbloc = qbloc))
  colnames(qbloc_mm) <- paste0("qb_location_", levels(qbloc))
  cbind(
    as.matrix(dt[, ..FEAT_SITU]),
    is_motion = as.integer(dt$is_motion),
    is_no_huddle = as.integer(dt$is_no_huddle),
    n_offense_backfield = as.numeric(dt$n_offense_backfield),
    qbloc_mm
  )
}

ll_vec <- function(y, p, eps = 1e-15) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -(y * log(p) + (1 - y) * log(1 - p))
}
auc_fn <- function(y, p) {
  r <- rank(p)
  n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
XGB_PARAMS <- list(objective = "binary:logistic", eval_metric = "logloss",
                    max_depth = 4, eta = 0.05, subsample = 0.8,
                    colsample_bytree = 0.8, min_child_weight = 10)

run_oof <- function(dt, y, nrounds = 250) {
  X <- build_X(dt)
  oof <- rep(NA_real_, nrow(dt))
  for (k in 1:5) {
    tr <- which(dt$fold != k); te <- which(dt$fold == k)
    dtr <- xgb.DMatrix(data = X[tr, , drop = FALSE], label = y[tr], missing = NA)
    dte <- xgb.DMatrix(data = X[te, , drop = FALSE], missing = NA)
    set.seed(SEED + k)
    fit <- xgb.train(params = XGB_PARAMS, data = dtr, nrounds = nrounds, verbose = 0)
    oof[te] <- predict(fit, dte)
  }
  oof
}

# --- model 1: run vs pass, pre-snap only -----------------------------------
y1 <- as.integer(uni1$pass)
oof1 <- run_oof(uni1, y1)
uni1[, `:=`(y = y1, p = oof1, ll = ll_vec(y1, oof1))]
ll1 <- mean(uni1$ll); auc1 <- auc_fn(y1, oof1)
cat(sprintf("\n=== RUN vs PASS model (out-of-fold, 5-fold by game) ===\nlog-loss %.4f, AUC %.4f, n = %s\n",
            ll1, auc1, format(nrow(uni1), big.mark = ",")))

# --- model 2: play action, dropbacks only -----------------------------------
y2 <- as.integer(uni2$is_play_action)
oof2 <- run_oof(uni2, y2)
uni2[, `:=`(y = y2, p = oof2, ll = ll_vec(y2, oof2))]
ll2 <- mean(uni2$ll); auc2 <- auc_fn(y2, oof2)
cat(sprintf("\n=== PLAY ACTION model (out-of-fold, 5-fold by game) ===\nlog-loss %.4f, AUC %.4f, n = %s\n",
            ll2, auc2, format(nrow(uni2), big.mark = ",")))

# --- per-caller motion-vs-still gap, either universe ------------------------
caller_gap <- function(dt, min_n) {
  agg <- dt[, .(ll = mean(ll), n = .N), by = .(off_play_caller, is_motion)]
  mt <- agg[is_motion == TRUE,  .(off_play_caller, ll_motion = ll, n_motion = n)]
  mf <- agg[is_motion == FALSE, .(off_play_caller, ll_still  = ll, n_still  = n)]
  out <- merge(mt, mf, by = "off_play_caller")
  out[, `:=`(n = n_motion + n_still, gap = ll_motion - ll_still,
             motion_rate = n_motion / (n_motion + n_still))]
  out[n >= min_n]
}
boot_ci <- function(dt_caller, B = 1000, seed = 7) {
  gids <- unique(dt_caller$game_id)
  idx_by_game <- split(seq_len(nrow(dt_caller)), dt_caller$game_id)
  ll <- dt_caller$ll; mo <- dt_caller$is_motion
  set.seed(seed)
  out <- vapply(seq_len(B), function(b) {
    g <- sample(gids, length(gids), replace = TRUE)
    rows <- unlist(idx_by_game[g], use.names = FALSE)
    mean(ll[rows][mo[rows]]) - mean(ll[rows][!mo[rows]])
  }, numeric(1))
  c(lo = unname(quantile(out, .025, na.rm = TRUE)),
    hi = unname(quantile(out, .975, na.rm = TRUE)))
}
add_ci <- function(gap_tbl, universe) {
  gap_tbl[, `:=`(lo = NA_real_, hi = NA_real_)]
  for (nm in HIGHLIGHT) {
    if (!nm %in% gap_tbl$off_play_caller) next
    ci <- boot_ci(universe[off_play_caller == nm])
    gap_tbl[off_play_caller == nm, `:=`(lo = ci["lo"], hi = ci["hi"])]
  }
  setorder(gap_tbl, -gap)
  gap_tbl[, `:=`(rank = .I, pct = round(100 * frank(gap) / .N))]
  gap_tbl[]
}

rp <- add_ci(caller_gap(uni1, MIN_RP), uni1)
pa <- add_ci(caller_gap(uni2, MIN_PA), uni2)

cat(sprintf("\n--- RUN/PASS motion-vs-still log-loss gap: %d callers with >= %d plays ---\n",
            nrow(rp), MIN_RP))
cat(sprintf("league median gap: %+.4f (positive = motion plays are HARDER to read than still plays)\n",
            median(rp$gap)))
print(rp[off_play_caller %in% HIGHLIGHT,
         .(off_play_caller, n, motion_rate = round(100*motion_rate,1),
           gap = round(gap,4), lo = round(lo,4), hi = round(hi,4),
           rank = paste0(rank,"/",nrow(rp)), pct)])

cat(sprintf("\n--- PLAY ACTION motion-vs-still log-loss gap: %d callers with >= %d dropbacks ---\n",
            nrow(pa), MIN_PA))
cat(sprintf("league median gap: %+.4f\n", median(pa$gap)))
print(pa[off_play_caller %in% HIGHLIGHT,
         .(off_play_caller, n, motion_rate = round(100*motion_rate,1),
           gap = round(gap,4), lo = round(lo,4), hi = round(hi,4),
           rank = paste0(rank,"/",nrow(pa)), pct)])

cat("\n--- who motions most (top 10 of the run/pass qualifiers) ---\n")
print(rp[order(-motion_rate)][1:10, .(off_play_caller, motion_rate = round(100*motion_rate,1))])

epa_h <- uni1[off_play_caller %in% HIGHLIGHT & !is.na(epa),
              .(epa = round(mean(epa),3), n = .N), by = .(off_play_caller, is_motion)]
setorder(epa_h, off_play_caller, -is_motion)
cat("\n--- EPA per play, motion vs still, highlighted callers (run/pass universe) ---\n")
print(epa_h)

verdict_rows <- rp[off_play_caller %in% HIGHLIGHT]
n_above_med <- sum(verdict_rows$gap > median(rp$gap))
cat(sprintf(
  "\n--- VERDICT ---\n%d of the 3 highlighted callers (McVay, Shanahan, Johnson) sit above the league median run/pass gap (%+.4f).\n",
  n_above_med, median(rp$gap)))
for (nm in HIGHLIGHT) {
  row <- rp[off_play_caller == nm]
  cat(sprintf("%-14s run/pass gap %+.4f (rank %d/%d, %dth pct), PA gap %+.4f (rank %s)\n",
              nm, row$gap, row$rank, nrow(rp), row$pct,
              pa[off_play_caller == nm]$gap,
              if (nrow(pa[off_play_caller == nm])) paste0(pa[off_play_caller == nm]$rank,"/",nrow(pa)) else "NA"))
}

write_csv(as.data.frame(rbind(
  cbind(model = "run_pass", as.data.frame(rp)),
  cbind(model = "play_action", as.data.frame(pa))
)), "data/derived/motion_callers.csv")

# --- chart A: run/pass motion-encryption forest plot ------------------------
mk_chart <- function(tbl, model_ll, model_auc, model_n, title, subtitle, y_lab, out_path, cap_extra) {
  tbl <- copy(tbl)
  tbl[, is_hi := off_play_caller %in% HIGHLIGHT]
  hi_ranks <- tbl[is_hi == TRUE, rank]
  extra_lab <- tbl[!(is_hi)][order(-abs(gap))][1:5][
    sapply(rank, function(r) min(abs(r - hi_ranks)) > 1), off_play_caller]
  tbl[, lab := off_play_caller %in% c(HIGHLIGHT, extra_lab)]

  # Highlighted labels get their own strongly-repelled layer (they can sit at
  # adjacent ranks with overlapping CI whiskers) and a small rightward nudge
  # into open space; the handful of extra context labels get gentler repel.
  p <- ggplot(tbl, aes(gap, rank)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
    geom_point(data = tbl[is_hi == FALSE], colour = "#9db6c9", alpha = 0.6, size = 2.1) +
    geom_errorbar(data = tbl[is_hi == TRUE], aes(xmin = lo, xmax = hi),
                  orientation = "y", width = 0.6, linewidth = 0.8, colour = "#D55E00") +
    geom_point(data = tbl[is_hi == TRUE], colour = "#D55E00", size = 3.4) +
    geom_text_repel(data = tbl[is_hi == TRUE], aes(label = off_play_caller),
                    size = 3.2, fontface = "bold", colour = "#8a3d00",
                    seed = 5, box.padding = 1, point.padding = 0.4, force = 10,
                    max.iter = 30000, max.time = 3, nudge_x = 0.015, nudge_y = 0.9,
                    min.segment.length = 0, max.overlaps = Inf) +
    geom_text_repel(data = tbl[is_hi == FALSE & lab == TRUE], aes(label = off_play_caller),
                    size = 3.0, fontface = "bold", colour = "grey35",
                    seed = 5, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
    scale_y_reverse(breaks = NULL) +
    labs(title = title, subtitle = subtitle,
         x = "out-of-fold log-loss, motion plays minus still plays  (→ motion makes him HARDER to read)",
         y = y_lab,
         caption = paste(strwrap(fig_caption(
           "nflverse play-by-play + FTN charting 2022-2025, playcallers.csv",
           sprintf("%s callers with at least the play minimum, model log-loss %.4f / AUC %.4f on %s plays.",
                   nrow(tbl), model_ll, model_auc, format(model_n, big.mark = ",")),
           cap_extra
         ), width = 135), collapse = "\n")) +
    theme_coach(grid = "none")
  save_fig(out_path, p, w = 12, h = 8)
}

# Title computed from what the highlighted callers actually did: name whoever
# separates from the league (above median AND bootstrap CI clear of zero).
hi_rp <- rp[off_play_caller %in% HIGHLIGHT]
sep_rp <- hi_rp[gap > median(rp$gap) & lo > 0, off_play_caller]
oth_rp <- setdiff(sub("^.* ", "", HIGHLIGHT), sub("^.* ", "", sep_rp))
title_a <- if (length(sep_rp) == 0) {
  "Motion does not make any of the three famous motion offenses harder to read"
} else {
  sprintf("Motion scrambles the run-pass model for %s, and not for %s",
          paste(sub("^.* ", "", sep_rp), collapse = " and "),
          paste(oth_rp, collapse = " or "))
}

mk_chart(rp, ll1, auc1, nrow(uni1),
  title = title_a,
  subtitle = "Out-of-fold log-loss on each caller's motion plays minus his no-motion plays, run/pass model.\nCI whiskers (bootstrapped over games) shown for McVay, Shanahan and Johnson only.",
  y_lab = "callers, ranked by gap (top = motion hurts the model most)",
  out_path = "docs/figures/motion_encryption.png",
  cap_extra = paste0(
    "\nFTN's is_motion is a yes/no flag, not a motion type: jet, orbit and a simple shift all code identically, so this tests whether a caller uses motion at all, not what kind. ",
    sprintf("McVay ranks %d/%d (gap %+.4f), Shanahan %d/%d (%+.4f), Johnson %d/%d (%+.4f) on this list. Built by R/15.",
            rp[off_play_caller=="Sean McVay"]$rank, nrow(rp), rp[off_play_caller=="Sean McVay"]$gap,
            rp[off_play_caller=="Kyle Shanahan"]$rank, nrow(rp), rp[off_play_caller=="Kyle Shanahan"]$gap,
            rp[off_play_caller=="Ben Johnson"]$rank, nrow(rp), rp[off_play_caller=="Ben Johnson"]$gap)
  ))

# --- chart B: play action version -------------------------------------------
# The PA chart's real headline is the league-wide effect: check what share of
# callers sit right of zero and title from that.
share_pos_pa <- mean(pa$gap > 0)
hi_pa <- pa[off_play_caller %in% HIGHLIGHT]
sep_pa <- hi_pa[gap > median(pa$gap) & lo > 0, off_play_caller]
title_b <- if (share_pos_pa >= 0.9 & length(sep_pa) == 0) {
  "Motion makes play action harder to read for the whole league, and nobody owns the trick"
} else if (length(sep_pa) > 0) {
  sprintf("Motion makes play action harder to read league-wide, and most of all for %s",
          paste(sub("^.* ", "", sep_pa), collapse = " and "))
} else {
  "Motion and the play-action model, the honest view"
}

mk_chart(pa, ll2, auc2, nrow(uni2),
  title = title_b,
  subtitle = "Out-of-fold log-loss on each caller's motion dropbacks minus his no-motion dropbacks, play-action model.\nCI whiskers (bootstrapped over games) shown for McVay, Shanahan and Johnson only.",
  y_lab = "callers, ranked by gap (top = motion hurts the model most)",
  out_path = "docs/figures/motion_pa.png",
  cap_extra = paste0(
    "\nSame is_motion caveat as the run/pass chart: presence only, not type. ",
    sprintf("McVay ranks %d/%d (gap %+.4f), Shanahan %d/%d (%+.4f), Johnson %d/%d (%+.4f) on the play-action list. Built by R/15.",
            pa[off_play_caller=="Sean McVay"]$rank, nrow(pa), pa[off_play_caller=="Sean McVay"]$gap,
            pa[off_play_caller=="Kyle Shanahan"]$rank, nrow(pa), pa[off_play_caller=="Kyle Shanahan"]$gap,
            pa[off_play_caller=="Ben Johnson"]$rank, nrow(pa), pa[off_play_caller=="Ben Johnson"]$gap)
  ))

cat("\ndone.\n")
