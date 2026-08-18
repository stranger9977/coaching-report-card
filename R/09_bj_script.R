# =============================================================================
# 09_bj_script.R
#
# Theory to test (verbatim, from the group): "we have a theory that ben
# johsnons play sequencing is unique." This is the OPENING SCRIPT piece.
# Play-callers script roughly their first 15 offensive snaps before kickoff.
# Three questions:
#   1. SCRIPT EDGE -- does Ben Johnson's offense play better in the script
#      than the rest of the game, more than other callers' do? (EPA/play,
#      script vs rest, per caller, career 2015-2025.)
#   2. HIDING IN THE SCRIPT -- is Johnson unusually hard to guess (run/pass)
#      during the script specifically, relative to how his script-vs-rest
#      gap compares to everyone else's?
#   3. STABILITY -- does a caller's script edge repeat year to year at all,
#      or is "script quality" mostly noise? If it's mostly noise, Johnson's
#      rank inherits that warning.
#
# Definitions:
#   SCRIPT = a team's first 15 called offensive plays of the game (rush or
#     dropback, kneels/spikes/2pt excluded), in play_id order. No win
#     probability filter -- it's early, that's the point.
#   REST = called plays from the 16th on, filtered to vegas_wp in [0.2, 0.8]
#     to keep garbage time out of the comparison.
#   Guessability = situation-cell (down x togo x field zone x score x half)
#     run/pass rate, empirical-Bayes shrunk toward the league rate in the
#     same cells (K=15 pseudo-plays), same machinery as
#     scripts/predictability_build.R. Reported as guess_xs = extra correct
#     guesses per 100 plays vs. league in the same cells. HIGH guess_xs =
#     MORE predictable/easier to guess (this is the guessability scale, not
#     entropy -- do not read it backwards).
#
# Sources:
#   - ~/stranger9977/nfl-analysis/data/pbp_slim.rds -- local nflverse pbp,
#     already restricted to 2015-2025, one row per play. play_id confirmed
#     monotonic within game+posteam (chronological order of that team's own
#     snaps), verified against 2024_01_ARI_BUF before building on it.
#   - ~/stranger9977/nfl-analysis/data/playcallers.csv -- off_play_caller per
#     team-game, 2015-2025 (2026 rows dropped, unplayed). Ben Johnson: DET
#     2022-2024, CHI 2025 (confirmed no other "Ben Johnson" in the data).
#   - ~/stranger9977/nfl-analysis/data/games.csv -- home_coach/away_coach,
#     fallback for team-games missing from playcallers.csv (same pattern as
#     predictability_build.R).
#
# Out:
#   docs/figures/bj_script_edge.png   -- script EPA vs rest EPA per caller,
#     diagonal = no edge, Johnson + marquee names labeled, noise band shown.
#   docs/figures/bj_script_guess.png  -- script guessability vs rest
#     guessability per caller, same diagonal design, Johnson vs league.
#   data/derived/bj_script_callers.csv -- one row per caller, career
#     2015-2025 (n_games >= 40): script/rest EPA, gap, ranks, guessability.
#   data/derived/bj_script_seasons.csv -- one row per caller-season
#     (n_games >= 12): script/rest EPA, gap. Used for the stability check.
# =============================================================================

suppressMessages({
  library(data.table)
  library(ggplot2)
  library(ggrepel)
  library(scales)
})
source("R/lib/theme_coach.R")
setDTthreads(2)
set.seed(20260818)

NFLA <- path.expand("~/stranger9977/nfl-analysis")
DATA <- file.path(NFLA, "data")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

SEASONS   <- 2015:2025
MIN_GAMES_CAREER <- 40
MIN_GAMES_SEASON <- 12
K <- 15  # EB shrinkage strength, matches predictability_build.R

MARQUEE <- c("Sean McVay", "Kyle Shanahan", "Andy Reid", "Matt LaFleur",
             "Sean Payton", "Ben Johnson")

# -----------------------------------------------------------------------
# 1. Load plays, build the SCRIPT / REST split
# -----------------------------------------------------------------------
keep <- c("game_id","season","week","posteam","home_team","away_team",
          "down","ydstogo","yardline_100","qtr","score_differential",
          "qb_dropback","rush","qb_kneel","qb_spike","two_point_attempt",
          "vegas_wp","epa","play_id")
d <- as.data.table(readRDS(file.path(DATA, "pbp_slim.rds")))[, ..keep]
d <- d[season %in% SEASONS]

d[, called := (qb_dropback == 1 | rush == 1) &
              (is.na(qb_kneel) | qb_kneel != 1) &
              (is.na(qb_spike) | qb_spike != 1) &
              (is.na(two_point_attempt) | two_point_attempt != 1)]
d <- d[called == TRUE & down %in% 1:4 & !is.na(epa) & posteam != ""]
d[, is_pass := as.integer(qb_dropback == 1)]

setorder(d, game_id, posteam, play_id)
d[, script_seq := seq_len(.N), by = .(game_id, posteam)]
d[, segment := fifelse(script_seq <= 15, "script", "rest")]

cat(sprintf("called plays 2015-2025: %d, games x teams: %d\n",
            nrow(d), uniqueN(d[, .(game_id, posteam)])))
cat(sprintf("script plays: %d, rest plays (pre-wp-filter): %d\n",
            sum(d$segment == "script"), sum(d$segment == "rest")))

# -----------------------------------------------------------------------
# 2. Caller attribution (playcallers.csv, games.csv head-coach fallback)
# -----------------------------------------------------------------------
pc <- fread(file.path(DATA, "playcallers.csv"),
            select = c("season","week","team","game_id","off_play_caller"))
pc <- pc[season %in% SEASONS & !is.na(off_play_caller) & off_play_caller != ""]
pc[, off_play_caller := trimws(off_play_caller)]
d <- merge(d, pc, by.x = c("game_id","season","week","posteam"),
                  by.y = c("game_id","season","week","team"), all.x = TRUE)
cat(sprintf("plays w/o caller match before fallback: %d of %d\n",
            sum(is.na(d$off_play_caller)), nrow(d)))

gm <- fread(file.path(DATA, "games.csv"),
            select = c("game_id","home_team","away_team","home_coach","away_coach"))
d <- merge(d, gm[, .(game_id, home_team, home_coach, away_coach)], by = "game_id", all.x = TRUE)
d[is.na(off_play_caller),
  off_play_caller := ifelse(posteam == home_team, home_coach, away_coach)]
d <- d[!is.na(off_play_caller) & off_play_caller != ""]
cat(sprintf("final plays: %d, unique callers: %d\n", nrow(d), uniqueN(d$off_play_caller)))

n_bj <- d[off_play_caller == "Ben Johnson", uniqueN(game_id)]
cat(sprintf("Ben Johnson games in attributed data: %d\n", n_bj))

# -----------------------------------------------------------------------
# 3. TEST 1 -- script edge, career level (2015-2025, min 40 games)
# -----------------------------------------------------------------------
script_pool <- d[segment == "script"]
rest_pool   <- d[segment == "rest" & !is.na(vegas_wp) & vegas_wp >= 0.2 & vegas_wp <= 0.8]

n_games_caller <- d[, .(n_games = uniqueN(game_id)), by = off_play_caller]

script_agg <- script_pool[, .(n_script = .N, script_epa = mean(epa), v_script = var(epa)),
                           by = off_play_caller]
rest_agg   <- rest_pool[, .(n_rest = .N, rest_epa = mean(epa), v_rest = var(epa)),
                         by = off_play_caller]

callers <- merge(n_games_caller, script_agg, by = "off_play_caller", all.x = TRUE)
callers <- merge(callers, rest_agg, by = "off_play_caller", all.x = TRUE)
callers <- callers[n_games >= MIN_GAMES_CAREER & !is.na(script_epa) & !is.na(rest_epa)]
callers[, gap := script_epa - rest_epa]
callers[, se_gap := sqrt(v_script / n_script + v_rest / n_rest)]
callers[, gap_lo := gap - 1.96 * se_gap]
callers[, gap_hi := gap + 1.96 * se_gap]

setorder(callers, -gap)
callers[, rank_gap := seq_len(.N)]
setorder(callers, -script_epa)
callers[, rank_script := seq_len(.N)]
setorder(callers, off_play_caller)

n_callers <- nrow(callers)
bj <- callers[off_play_caller == "Ben Johnson"]

cat(sprintf("\n=== TEST 1: SCRIPT EDGE (career, >=%d games, %d callers) ===\n",
            MIN_GAMES_CAREER, n_callers))
cat(sprintf("Ben Johnson: script EPA = %.3f, rest EPA = %.3f, gap = %+.3f [%.3f, %.3f]\n",
            bj$script_epa, bj$rest_epa, bj$gap, bj$gap_lo, bj$gap_hi))
cat(sprintf("  n_script = %d, n_rest = %d, n_games = %d\n", bj$n_script, bj$n_rest, bj$n_games))
cat(sprintf("  rank on GAP (script minus rest):   %d of %d\n", bj$rank_gap, n_callers))
cat(sprintf("  rank on RAW SCRIPT EPA:             %d of %d\n", bj$rank_script, n_callers))

band95 <- 1.96 * median(callers$se_gap)
cat(sprintf("  league median se(gap) x1.96 (typical 95%% noise band) = +/-%.3f EPA/play\n", band95))
cat(sprintf("  Johnson's gap %s the typical noise band (|gap|=%.3f vs band=%.3f)\n",
            ifelse(abs(bj$gap) > band95, "clears", "does NOT clear"), abs(bj$gap), band95))

fwrite(callers, "data/derived/bj_script_callers.csv")

# -----------------------------------------------------------------------
# 4. TEST 2 -- guessability, script vs rest, per caller (same pool/threshold)
# -----------------------------------------------------------------------
mk_cells <- function(dt) {
  dt[, togo_b  := cut(ydstogo, c(0,3,6,10,100), labels = c("1-3","4-6","7-10","11+"))]
  dt[, zone_b  := cut(yardline_100, c(0,20,50,80,100),
                      labels = c("rz1-20","21-50","51-80","81-100"))]
  dt[, score_b := cut(score_differential, c(-Inf,-8.5,8.5,Inf),
                      labels = c("trail","close","lead"))]
  dt[, half    := ifelse(qtr <= 2, "H1", ifelse(qtr == 5, "OT", "H2"))]
  dt[, cell    := paste(down, togo_b, zone_b, score_b, half, sep = "|")]
  dt
}
script_pool <- mk_cells(script_pool)
rest_pool   <- mk_cells(rest_pool)

# guessability for one segment's play pool: league cell rates computed
# WITHIN that segment, then each caller's shrunk cell rate vs. that
# league-in-segment baseline. Returns one row per off_play_caller.
guessability_by_segment <- function(pool, min_cell_n = 1) {
  lg <- pool[, .(lg_p = mean(is_pass), lg_n = .N), by = cell]
  pool <- merge(pool, lg, by = "cell", all.x = TRUE)
  cc <- pool[, .(n_cell = .N, p_raw = mean(is_pass), lg_p = lg_p[1]),
             by = .(off_play_caller, cell)]
  cc[, p_shr := (n_cell * p_raw + K * lg_p) / (n_cell + K)]
  cc[, g_cell   := pmax(p_shr, 1 - p_shr)]
  cc[, glg_cell := pmax(lg_p,  1 - lg_p)]
  out <- cc[, .(n_plays  = sum(n_cell),
                guess    = sum(g_cell * n_cell) / sum(n_cell),
                guess_lg = sum(glg_cell * n_cell) / sum(n_cell)),
            by = off_play_caller]
  out[, guess_xs := (guess - guess_lg) * 100]
  out[]
}

g_script <- guessability_by_segment(script_pool)
setnames(g_script, c("n_plays","guess","guess_lg","guess_xs"),
                    c("n_script_g","guess_script","guess_lg_script","guess_xs_script"))
g_rest <- guessability_by_segment(rest_pool)
setnames(g_rest, c("n_plays","guess","guess_lg","guess_xs"),
                  c("n_rest_g","guess_rest","guess_lg_rest","guess_xs_rest"))

guess_tab <- merge(g_script, g_rest, by = "off_play_caller")
guess_tab <- merge(guess_tab, n_games_caller, by = "off_play_caller")
guess_tab <- guess_tab[n_games >= MIN_GAMES_CAREER]
guess_tab[, delta_guess := guess_xs_script - guess_xs_rest]  # + = MORE guessable in script than his own normal

setorder(guess_tab, delta_guess)
guess_tab[, rank_delta := seq_len(.N)]  # rank 1 = hides the MOST in the script (most negative delta)
setorder(guess_tab, off_play_caller)

n_guess <- nrow(guess_tab)
bj_g <- guess_tab[off_play_caller == "Ben Johnson"]
delta_mean <- mean(guess_tab$delta_guess)
delta_sd   <- sd(guess_tab$delta_guess)
bj_z <- (bj_g$delta_guess - delta_mean) / delta_sd
bj_pctile <- 100 * mean(guess_tab$delta_guess <= bj_g$delta_guess)

# bootstrap CI on Johnson's own script/rest guessability and delta,
# resampling his OWN plays within each segment, league cell rates held fixed
boot_bj <- function(B = 1000) {
  sp <- script_pool[off_play_caller == "Ben Johnson"]
  rp <- rest_pool[off_play_caller == "Ben Johnson"]
  lg_sp <- script_pool[, .(lg_p = mean(is_pass)), by = cell]
  lg_rp <- rest_pool[, .(lg_p = mean(is_pass)), by = cell]
  one_side <- function(pool, lg) {
    idx <- sample.int(nrow(pool), nrow(pool), replace = TRUE)
    bp <- pool[idx]
    cc <- bp[, .(n_cell = .N, p_raw = mean(is_pass)), by = cell]
    cc <- merge(cc, lg, by = "cell", all.x = TRUE)
    cc[, p_shr := (n_cell * p_raw + K * lg_p) / (n_cell + K)]
    cc[, g_cell   := pmax(p_shr, 1 - p_shr)]
    cc[, glg_cell := pmax(lg_p,  1 - lg_p)]
    sum((cc$g_cell - cc$glg_cell) * cc$n_cell) / sum(cc$n_cell) * 100
  }
  reps <- replicate(B, {
    gs <- one_side(sp, lg_sp)
    gr <- one_side(rp, lg_rp)
    c(guess_xs_script = gs, guess_xs_rest = gr, delta = gs - gr)
  })
  t(apply(reps, 1, quantile, probs = c(0.025, 0.975)))
}
bj_ci <- boot_bj(1000)

cat(sprintf("\n=== TEST 2: GUESSABILITY, script vs rest (>=%d games, %d callers) ===\n",
            MIN_GAMES_CAREER, n_guess))
cat(sprintf("Ben Johnson: guess_xs_script = %.2f [%.2f, %.2f], guess_xs_rest = %.2f [%.2f, %.2f]\n",
            bj_g$guess_xs_script, bj_ci["guess_xs_script",1], bj_ci["guess_xs_script",2],
            bj_g$guess_xs_rest,   bj_ci["guess_xs_rest",1],   bj_ci["guess_xs_rest",2]))
cat(sprintf("  delta (script minus rest, negative = harder to guess IN script) = %.2f [%.2f, %.2f]\n",
            bj_g$delta_guess, bj_ci["delta",1], bj_ci["delta",2]))
cat(sprintf("  league delta_guess: mean = %.2f, sd = %.2f\n", delta_mean, delta_sd))
cat(sprintf("  Johnson's delta: rank %d of %d (1 = hides most), z = %.2f, percentile = %.0f\n",
            bj_g$rank_delta, n_guess, bj_z, bj_pctile))

fwrite(guess_tab, "data/derived/bj_script_guess_callers.csv")

# -----------------------------------------------------------------------
# 5. TEST 3 -- stability: does the script edge repeat year to year?
# -----------------------------------------------------------------------
n_games_cs <- d[, .(n_games = uniqueN(game_id)), by = .(off_play_caller, season)]
script_cs <- script_pool[, .(script_epa = mean(epa), n_script = .N), by = .(off_play_caller, season)]
rest_cs   <- rest_pool[,   .(rest_epa   = mean(epa), n_rest   = .N), by = .(off_play_caller, season)]

seasons_tab <- merge(n_games_cs, script_cs, by = c("off_play_caller","season"), all.x = TRUE)
seasons_tab <- merge(seasons_tab, rest_cs, by = c("off_play_caller","season"), all.x = TRUE)
seasons_tab <- seasons_tab[n_games >= MIN_GAMES_SEASON & !is.na(script_epa) & !is.na(rest_epa)]
seasons_tab[, gap := script_epa - rest_epa]
setorder(seasons_tab, off_play_caller, season)

fwrite(seasons_tab, "data/derived/bj_script_seasons.csv")

pairs <- merge(seasons_tab[, .(off_play_caller, season, gap)],
               seasons_tab[, .(off_play_caller, season = season - 1, gap_next = gap)],
               by = c("off_play_caller","season"))
n_pairs <- nrow(pairs)
r_stab <- cor(pairs$gap, pairs$gap_next)
fz <- function(r, n) { z <- atanh(r); se <- 1/sqrt(n-3); tanh(z + c(-1.96,1.96)*se) }
ci_stab <- if (n_pairs > 4) fz(r_stab, n_pairs) else c(NA, NA)

cat(sprintf("\n=== TEST 3: STABILITY (caller-seasons, >=%d games/season) ===\n", MIN_GAMES_SEASON))
cat(sprintf("caller-seasons meeting threshold: %d, consecutive-season pairs: %d\n",
            nrow(seasons_tab), n_pairs))
cat(sprintf("year-to-year r(gap) = %.3f [%.3f, %.3f], n = %d pairs\n",
            r_stab, ci_stab[1], ci_stab[2], n_pairs))
verdict_stability <- if (!is.na(ci_stab[1]) && ci_stab[1] <= 0) {
  "CI crosses zero -- script edge does not clearly repeat league-wide; treat it as mostly noise."
} else {
  "CI stays above zero -- script edge repeats some, a real trait, but weakly (see r)."
}
cat(sprintf("Verdict: %s\n", verdict_stability))

bj_seasons <- seasons_tab[off_play_caller == "Ben Johnson"]
cat("\nBen Johnson season-by-season script edge:\n")
print(bj_seasons[, .(season, n_games, script_epa = round(script_epa,3),
                      rest_epa = round(rest_epa,3), gap = round(gap,3))])

# -----------------------------------------------------------------------
# 6. CHART A -- script EPA vs rest EPA, career, diagonal = no edge
# -----------------------------------------------------------------------
lab_a <- callers[off_play_caller %in% MARQUEE]
lab_a_bj <- lab_a[off_play_caller == "Ben Johnson"]
lab_a_other <- lab_a[off_play_caller != "Ben Johnson"]

rng <- range(c(callers$script_epa, callers$rest_epa))
pad <- diff(rng) * 0.08
xline <- seq(rng[1] - pad, rng[2] + pad, length.out = 2)
band_df <- data.frame(x = xline, ymin = xline - band95, ymax = xline + band95)

title_a <- sprintf(
  "Ben Johnson's offense is +%.2f EPA/play better in the script than after it (rank %d of %d)",
  bj$gap, bj$rank_gap, n_callers)
if (bj$gap < 0) {
  title_a <- sprintf(
    "Ben Johnson's offense is %.2f EPA/play WORSE in the script than after it (rank %d of %d)",
    bj$gap, bj$rank_gap, n_callers)
}

p_a <- ggplot(callers, aes(script_epa, rest_epa)) +
  geom_ribbon(data = band_df, aes(x = x, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE, fill = "grey70", alpha = 0.18) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.4) +
  geom_point(colour = "#9db6c9", alpha = 0.6, size = 2.1) +
  geom_point(data = lab_a_other, colour = "#D55E00", size = 2.8) +
  geom_point(data = lab_a_bj, colour = "#D6604D", size = 4.2) +
  geom_text_repel(data = lab_a_other, aes(label = off_play_caller), size = 3.2,
                   fontface = "bold", colour = "#8a3d00", seed = 5,
                   box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  geom_text_repel(data = lab_a_bj, aes(label = off_play_caller), size = 3.6,
                   fontface = "bold", colour = "#7a1a1a", seed = 5,
                   box.padding = 0.6, min.segment.length = 0) +
  coord_cartesian(xlim = rng + c(-pad, pad), ylim = rng + c(-pad, pad)) +
  labs(
    title = title_a,
    subtitle = "Each point is one play-caller's career (2015-2025). X is EPA/play on his team's first 15 called offensive\nplays; Y is EPA/play on called plays 16+ (vegas win prob 0.2-0.8). Below the dashed line = better in the script.",
    x = "script EPA/play (first 15 called plays)",
    y = "rest-of-game EPA/play (plays 16+, non-garbage-time)",
    caption = paste(strwrap(fig_caption(
      "nflverse play-by-play, 2015-2025, local pbp_slim.rds + playcallers.csv",
      sprintf("%d play-callers with >= %d games as primary caller. Shaded band = typical +/-1.96 x SE noise range (median across callers).",
              n_callers, MIN_GAMES_CAREER),
      "Script = first 15 called plays (rush/dropback, no kneels/spikes/2pt), no win-prob filter. Rest = called plays 16+, vegas_wp in [0.2,0.8]."
    ), width = 130), collapse = "\n")
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/bj_script_edge.png", p_a, w = 12, h = 7.2)

# -----------------------------------------------------------------------
# 7. CHART B -- script guessability vs rest guessability, diagonal
# -----------------------------------------------------------------------
lab_b_bj <- guess_tab[off_play_caller == "Ben Johnson"]
lab_b_other <- guess_tab[off_play_caller %in% setdiff(MARQUEE, "Ben Johnson")]

rng_g <- range(c(guess_tab$guess_xs_script, guess_tab$guess_xs_rest))
pad_g <- diff(rng_g) * 0.08

title_b <- sprintf(
  "Johnson gets %.1f pts %s to guess in the script than his own normal rate (%s of %d callers, by delta)",
  abs(bj_g$delta_guess), ifelse(bj_g$delta_guess < 0, "HARDER", "EASIER"),
  ifelse(bj_g$delta_guess < 0, sprintf("rank %d hardest", bj_g$rank_delta),
         sprintf("rank %d", bj_g$rank_delta)),
  n_guess)

p_b <- ggplot(guess_tab, aes(guess_xs_rest, guess_xs_script)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.4) +
  geom_point(colour = "#9db6c9", alpha = 0.6, size = 2.1) +
  geom_point(data = lab_b_other, colour = "#D55E00", size = 2.6) +
  geom_point(data = lab_b_bj, colour = "#D6604D", size = 4.2) +
  geom_text_repel(data = lab_b_other, aes(label = off_play_caller), size = 3.0,
                   fontface = "bold", colour = "#8a3d00", seed = 7,
                   box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  geom_text_repel(data = lab_b_bj, aes(label = off_play_caller), size = 3.6,
                   fontface = "bold", colour = "#7a1a1a", seed = 7,
                   box.padding = 0.6, min.segment.length = 0) +
  coord_cartesian(xlim = rng_g + c(-pad_g, pad_g), ylim = rng_g + c(-pad_g, pad_g)) +
  labs(
    title = title_b,
    subtitle = "Guessability = extra correct run/pass guesses per 100 plays vs. league in the same situation cells (down x\ntogo x zone x score x half, EB-shrunk). Below the dashed line = harder to guess in the script than normal.",
    x = "guessability vs league, rest of game (guess_xs_rest)",
    y = "guessability vs league, script (guess_xs_script)",
    caption = paste(strwrap(fig_caption(
      "nflverse play-by-play, 2015-2025, local pbp_slim.rds + playcallers.csv",
      sprintf("%d play-callers with >= %d games. League baseline computed separately within each segment (script, rest).",
              n_guess, MIN_GAMES_CAREER),
      "Higher = MORE predictable than league (this is the guessability scale, not entropy)."
    ), width = 130), collapse = "\n")
  ) +
  theme_coach(grid = "y")

save_fig("docs/figures/bj_script_guess.png", p_b, w = 12, h = 7.2)

cat("\nDONE\n")
