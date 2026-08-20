# =============================================================================
# 46_guess_the_play.R -- the defensive coordinator's actual problem, measured
# directly. Not "how many plays does this offense have" (R/41), but: standing
# on the sideline before the snap, I can see the offense's look and I know
# the down and distance. Can I guess the play?
#
# The McVay anecdote that started this: on one identical pre-snap picture in
# one game, he ran man blocking three times on short yardage, then a trick
# play, then straight dropbacks for touchdowns. The claim under test is that
# McVay is unusually hard to guess at the moment that matters. It is a claim
# about him specifically, so it has to be tested against everyone else, not
# assumed.
#
# THE GUESSING RULE, and why the obvious version cheats. If you just ask "what
# is a caller's single most common play out of this look, in this situation,
# and how often does that guess turn out right," a cell with one snap in it
# guesses itself correctly 100% of the time -- the "most common play" in a
# cell of one is just that one play. That is not a defense predicting
# anything; it is the method grading its own answer key. The fix is
# leave-one-out: for every snap, the guess is built from the caller's OTHER
# snaps in that same look-and-situation cell, never from the snap being
# guessed. A cell with only one snap in it has no "other snaps" to build a
# guess from, so it falls back to the caller's other snaps in that
# down-and-distance bucket generally (dropping the look), and if even that is
# empty it falls back to the caller's overall most common play. Same
# leave-one-out logic runs a second time using ONLY the down-and-distance
# bucket (no look at all), which is the situation-only baseline: what a
# defense could guess just from the game clock and the chains, before the
# offense ever lines up. The gap between the two numbers is what seeing the
# look actually buys the defense.
#
# THE SAMPLE-SIZE TRAP, same fix as R/41. A caller with more charted plays
# will have fuller look-and-situation cells just from having more snaps, which
# makes his guess rate drift for reasons that have nothing to do with how
# predictable he is. Every number below comes from a RAREFIED sample: each
# qualified caller is repeatedly cut down to the same 1,200 plays (the
# qualification floor), 200 times, and the 200 draws are averaged. Same
# machinery, same seed, as R/41.
#
# DEFINITIONS, reused from earlier scripts exactly, not reinvented:
#   PLAY  = R/41's play id. Runs: run_concept + which side the run attacks.
#           Dropbacks: the three deepest-charted route runners' routes,
#           sorted and joined (the charting has no pass-play-name field, so
#           the routes stand in for the call).
#   LOOK  = R/41's dressing: formation x personnel group x where the
#           quarterback lines up.
#   SITUATION = the seven down-and-distance buckets from R/43 (1st-and-10,
#           2nd/3rd short/medium/long).
#
# Universe: same as R/41 -- offensive scrimmage plays, season_type == 0,
# non-garbage-time, caller-attributed, kneels/spikes excluded. Qualified
# callers need >= 1,200 such plays (36 in this data).
#
# Conventions: R/lib/theme_coach.R, plain football words in every rendered
# label, no em dashes, no Michael/Nick anywhere in rendered chart text,
# season spans written "2022-23 through 2025-26."
#
# Out: docs/figures/guess_the_play.png
#      data/derived/guess_the_play.csv
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2)
  library(scales); library(readr)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)
set.seed(20260819)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS   <- 2022:2025
QUAL_MIN  <- 1200
RAREFY_N  <- QUAL_MIN
N_ITER    <- 200
HALF_N    <- 500
HALF_ITER <- 200

NAMED <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")
COL <- c("Kyle Shanahan" = "#AA0000", "Sean McVay" = "#003594", "Ben Johnson" = "#D55E00")

# =============================================================================
# STEP 1: UNIVERSE (identical to R/41)
# =============================================================================
d <- load_sumer(SEASONS)
d_off <- d[season_type == 0 & garbage_time == FALSE & off_caller != "" &
           run_pass %in% c("P", "R") & play_type == "OFF/DEF" &
           !grepl("kneels|spiked", outcome, ignore.case = TRUE)]
cat(sprintf("Universe: offensive scrimmage plays, season_type==0, non-garbage-time, caller-attributed,\n"))
cat(sprintf("kneels/spikes excluded, %s-%s: %s plays\n",
            min(SEASONS), max(SEASONS), format(nrow(d_off), big.mark = ",")))

# =============================================================================
# STEP 2: PLAY ID -- runs (identical to R/41)
# =============================================================================
d_off[, side := fifelse(run_gap_intent_side == "", "NONE", run_gap_intent_side)]
d_off[, run_play_id := paste0("RUN|", run_concept, "|", side)]

# =============================================================================
# STEP 3: PLAY ID -- passes via the three deepest charted routes (identical to R/41)
# =============================================================================
db <- d_off[run_pass == "P"]
PCOLS <- c("sumer_play_id", "season", "side_of_ball", "role", "route",
           "receiving_targets", "receiving_depth_of_target")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
pl <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
pl <- pl[season %in% SEASONS & side_of_ball == "offense"]
pl_routes <- pl[role == "PASS ROUTE" & route != "" & !is.na(route)]

depth_tbl <- pl_routes[receiving_targets == 1 & !is.na(receiving_depth_of_target),
                        .(avg_depth = mean(receiving_depth_of_target)), by = route]

routes_db <- pl_routes[sumer_play_id %in% db$sumer_play_id, .(sumer_play_id, route)]
routes_db <- merge(routes_db, depth_tbl, by = "route")
setorder(routes_db, sumer_play_id, -avg_depth)
top3 <- routes_db[, .(route = route[seq_len(min(3, .N))]), by = sumer_play_id]
concept <- top3[, .(pass_play_id = paste0("PASS|", paste(sort(route), collapse = "+"))), by = sumer_play_id]

db <- merge(db, concept, by = "sumer_play_id", all.x = TRUE)
db[is.na(pass_play_id), pass_play_id := "PASS|UNCHARTED"]

d_off <- merge(d_off, db[, .(sumer_play_id, pass_play_id)], by = "sumer_play_id", all.x = TRUE)
d_off[, play_id := fifelse(run_pass == "R", run_play_id, pass_play_id)]
stopifnot(!any(is.na(d_off$play_id)))
cat(sprintf("combined distinct plays leaguewide (run + pass): %d\n", uniqueN(d_off$play_id)))

# =============================================================================
# STEP 4: LOOK ID (identical to R/41's dressing) and SITUATION (identical to R/43)
# =============================================================================
d_off[, look_id := paste(formation, offensive_personnel_basic, quarterback_alignment, sep = "|")]

d_off[, dd_bucket := fifelse(down == 1, "1st-and-10",
                     fifelse(down == 2 & distance <= 3, "2nd-short",
                     fifelse(down == 2 & distance <= 7, "2nd-medium",
                     fifelse(down == 2, "2nd-long",
                     fifelse(down == 3 & distance <= 3, "3rd-short",
                     fifelse(down == 3 & distance <= 7, "3rd-medium",
                     fifelse(down == 3, "3rd-long", NA_character_)))))))]
d_off <- d_off[!is.na(dd_bucket)]
cat(sprintf("distinct looks leaguewide: %d; down-and-distance buckets: 7\n", uniqueN(d_off$look_id)))

# =============================================================================
# STEP 5: QUALIFIED CALLERS
# =============================================================================
n_by_caller <- d_off[, .N, by = off_caller]
qual <- n_by_caller[N >= QUAL_MIN, off_caller]
cat(sprintf("qualified callers (>= %d plays): %d of %d\n", QUAL_MIN, length(qual), nrow(n_by_caller)))
stopifnot(all(NAMED %in% qual))
dq <- d_off[off_caller %in% qual]

# =============================================================================
# STEP 6: THE LEAVE-ONE-OUT GUESS
#
# For a vector of plays grouped by `grp`, returns for every row the guess a
# defense would make using every OTHER row in that same group -- never the
# row's own play. Implemented without a per-row loop: rank each group's plays
# by count (ties broken alphabetically, so the rule is deterministic), keep
# the top play and the runner-up, then for a row belonging to the top play,
# check whether removing its own one instance still leaves the top play
# ahead of the runner-up. A row belonging to any other play never changes
# who the leader is, so it always gets the group's leader as its guess.
# =============================================================================
loo_guess <- function(play, grp) {
  dt <- data.table(play = play, grp = grp)
  dt[, cnt := .N, by = .(grp, play)]
  u <- unique(dt[, .(grp, play, cnt)])
  setorder(u, grp, -cnt, play)
  u[, rnk := seq_len(.N), by = grp]
  u1 <- u[rnk == 1, .(grp, play1 = play, cnt1 = cnt)]
  u2 <- u[rnk == 2, .(grp, play2 = play, cnt2 = cnt)]
  dt[u1, on = "grp", `:=`(play1 = i.play1, cnt1 = i.cnt1)]
  dt[u2, on = "grp", `:=`(play2 = i.play2, cnt2 = i.cnt2)]
  dt[, adj1 := cnt1 - as.integer(play == play1)]
  dt[, guess := fifelse(is.na(cnt2), play1,
              fifelse(adj1 > cnt2, play1,
              fifelse(adj1 < cnt2, play2,
              fifelse(play1 <= play2, play1, play2))))]
  dt$guess
}

# One caller's guess rates, averaged over rarefied draws. Builds every draw
# at once (draw id x row) so the leave-one-out ranking runs as one grouped
# operation per level instead of one call per draw.
#   headline  = guess uses the look AND the situation (falls back to
#               situation-only, then to the caller's overall habit, exactly
#               as described above)
#   situation = guess uses the situation only, never the look
guess_rates_caller <- function(play, look, dd, iter_n, n_iters) {
  N <- length(play)
  idx_mat <- replicate(n_iters, sample.int(N, iter_n))
  draw <- rep(seq_len(n_iters), each = iter_n)
  idx <- as.vector(idx_mat)

  DT <- data.table(draw = draw, play = play[idx], look = look[idx], dd = dd[idx])
  DT[, cell_key    := paste(draw, look, dd, sep = "::")]
  DT[, bucket_key  := paste(draw, dd, sep = "::")]
  DT[, overall_key := draw]

  DT[, cell_guess    := loo_guess(play, cell_key)]
  DT[, bucket_guess  := loo_guess(play, bucket_key)]
  DT[, overall_guess := loo_guess(play, overall_key)]

  DT[, cell_n   := .N, by = cell_key]
  DT[, bucket_n := .N, by = bucket_key]

  DT[, guess_headline  := fifelse(cell_n > 1, cell_guess,
                          fifelse(bucket_n > 1, bucket_guess, overall_guess))]
  DT[, guess_situation := fifelse(bucket_n > 1, bucket_guess, overall_guess)]

  DT[, hit_headline  := as.integer(guess_headline  == play)]
  DT[, hit_situation := as.integer(guess_situation == play)]

  by_draw <- DT[, .(headline = mean(hit_headline), situation = mean(hit_situation)), by = draw]
  data.table(guess_headline = mean(by_draw$headline), guess_situation = mean(by_draw$situation))
}

cat(sprintf("\nrunning the leave-one-out guess: %d qualified callers x %d draws of %d plays each...\n",
            length(qual), N_ITER, RAREFY_N))
res_list <- lapply(qual, function(nm) {
  sub <- dq[off_caller == nm]
  r <- guess_rates_caller(sub$play_id, sub$look_id, sub$dd_bucket, RAREFY_N, N_ITER)
  r[, off_caller := nm]
  r
})
headline <- rbindlist(res_list)
headline <- merge(headline, n_by_caller[off_caller %in% qual], by = "off_caller")
setnames(headline, "N", "n_plays")
headline[, look_adds := guess_headline - guess_situation]
setorder(headline, guess_headline)
headline[, rank_headline := frank(guess_headline, ties.method = "min")]
setorder(headline, guess_situation)
headline[, rank_situation := frank(guess_situation, ties.method = "min")]
setorder(headline, rank_headline)

n_q <- nrow(headline)
cat(sprintf("\nleague medians (rarefied to %d plays, averaged over %d draws):\n", RAREFY_N, N_ITER))
cat(sprintf("  guessing with the look AND the situation: %.1f%%\n", 100 * median(headline$guess_headline)))
cat(sprintf("  guessing with the situation only:         %.1f%%\n", 100 * median(headline$guess_situation)))
cat(sprintf("  what the look adds:                       %.1f points\n", 100 * median(headline$look_adds)))

# =============================================================================
# STEP 7: PERSISTENCE -- same custom odd/even split as R/41, headline metric only
# =============================================================================
cat("\n=== PERSISTENCE ===\n")
dq[, season_half := fifelse(season %% 2 == 1, "odd", "even")]
half_n <- dq[, .(n_odd = sum(season_half == "odd"), n_even = sum(season_half == "even")), by = off_caller]
half_qual <- half_n[n_odd >= HALF_N & n_even >= HALF_N, off_caller]
cat(sprintf("callers with >= %d plays in BOTH season halves (2023/2025 vs 2022/2024): %d of %d qualified\n",
            HALF_N, length(half_qual), n_q))

half_res <- lapply(half_qual, function(nm) {
  sub_o <- dq[off_caller == nm & season_half == "odd"]
  sub_e <- dq[off_caller == nm & season_half == "even"]
  ro <- guess_rates_caller(sub_o$play_id, sub_o$look_id, sub_o$dd_bucket, HALF_N, HALF_ITER)
  re <- guess_rates_caller(sub_e$play_id, sub_e$look_id, sub_e$dd_bucket, HALF_N, HALF_ITER)
  data.table(off_caller = nm, headline_odd = ro$guess_headline, headline_even = re$guess_headline)
})
half_dt <- rbindlist(half_res)
ct <- cor.test(half_dt$headline_odd, half_dt$headline_even)
cat(sprintf("persistence of the headline guess rate: r = %+.3f [%+.3f, %+.3f], p = %.4f, n = %d callers\n",
            ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value, nrow(half_dt)))

# =============================================================================
# STEP 8: THE McVAY HYPOTHESIS
# =============================================================================
cat("\n=== THE HYPOTHESIS ===\n")
for (cl in NAMED) {
  r <- headline[off_caller == cl]
  cat(sprintf("%s: guessed right %.1f%% of the time knowing the look and situation (rank %d of %d, 1 = hardest),\n",
              cl, 100 * r$guess_headline, r$rank_headline, n_q))
  cat(sprintf("   %.1f%% knowing the situation only (rank %d of %d), the look adds %.1f points\n",
              100 * r$guess_situation, r$rank_situation, n_q, 100 * r$look_adds))
}
mcvay <- headline[off_caller == "Sean McVay"]
hard_half <- mcvay$rank_headline <= n_q / 2
verdict <- if (hard_half) {
  sprintf("HOLDS: McVay is in the harder-to-guess half of qualified callers (rank %d of %d).", mcvay$rank_headline, n_q)
} else {
  sprintf("DOES NOT HOLD: McVay ranks %d of %d, in the easier-to-guess half.", mcvay$rank_headline, n_q)
}
cat(sprintf("\nMcVay hypothesis (harder than most to guess): %s\n", verdict))

# =============================================================================
# WRITE DATA
# =============================================================================
out <- headline[, .(off_caller, n_plays,
                     guess_headline = round(100 * guess_headline, 2), rank_headline,
                     guess_situation = round(100 * guess_situation, 2), rank_situation,
                     look_adds = round(100 * look_adds, 2))]
setorder(out, rank_headline)
write_csv(as.data.frame(out), "data/derived/guess_the_play.csv")
cat(sprintf("\nwrote data/derived/guess_the_play.csv (%d rows)\n", nrow(out)))

# =============================================================================
# CHART
# =============================================================================
headline[, hl := off_caller %in% NAMED]
outlier_ids <- unique(c(headline[which.min(guess_headline)]$off_caller,
                         headline[which.max(guess_headline)]$off_caller))
outlier_ids <- setdiff(outlier_ids, NAMED)
headline[, ol := off_caller %in% outlier_ids]

setorder(headline, -guess_headline)
headline[, caller_lvl := factor(off_caller, levels = off_caller)]
med <- median(headline$guess_headline)

lab_named <- headline[hl == TRUE]
lab_out   <- headline[ol == TRUE]

p <- ggplot(headline, aes(guess_headline, caller_lvl)) +
  geom_vline(xintercept = med, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_segment(aes(x = guess_situation, xend = guess_headline, y = caller_lvl, yend = caller_lvl),
               colour = "grey80", linewidth = 0.6) +
  geom_point(aes(x = guess_situation, y = caller_lvl), colour = "grey65", size = 1.5) +
  geom_point(data = headline[hl == FALSE & ol == FALSE],
             colour = "grey45", size = 2.4) +
  geom_point(data = lab_out, colour = "#1d6a99", size = 2.8) +
  geom_point(data = lab_named, aes(colour = off_caller), size = 4) +
  geom_text(data = lab_named, aes(label = off_caller, colour = off_caller),
            hjust = -0.15, size = 3.1, fontface = "bold", show.legend = FALSE) +
  geom_text(data = lab_out, aes(label = off_caller), hjust = -0.15,
            size = 2.6, fontface = "bold", colour = "#1d6a99") +
  scale_colour_manual(values = COL, guide = "none") +
  scale_x_continuous(labels = percent_format(accuracy = 1), breaks = pretty_breaks(),
                      expand = expansion(mult = c(0.02, 0.16))) +
  coord_cartesian(clip = "off") +
  labs(x = "how often the defense's best pre-snap guess is right", y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.68)))

mc_r <- headline[off_caller == "Sean McVay"]$rank_headline
ks_r <- headline[off_caller == "Kyle Shanahan"]$rank_headline
bj_r <- headline[off_caller == "Ben Johnson"]$rank_headline

TITLE <- sprintf(
  "Guess the play: McVay ranks #%d of %d hardest to guess, Shanahan #%d, Johnson #%d",
  mc_r, n_q, ks_r, bj_r)
SUB <- paste0(
  "If the defense always guessed each caller's most common play for that look and situation,\n",
  "this is how often it would be right (the guess never uses the snap being guessed).\n",
  sprintf("League middle: %.0f%%. Lower = harder to guess where it counts.\n", 100 * med),
  "Grey dot and line = the same guess knowing only down and distance, before the offense\n",
  "lines up; the gap to the colored dot is what seeing the look adds.\n",
  "Every caller is measured on the same 1,200 plays, so a caller with more snaps charted does not look\n",
  "easier or harder to guess just for having more tape."
)

p_final <- p + labs(title = TITLE, subtitle = SUB,
  caption = fig_caption(
    "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
    sprintf(paste0("\n%d callers with at least %s plays each. A play = the run scheme and which side it attacks; for\n",
                   "passes, the three deepest routes on the play (the charting does not name pass plays, so the routes\n",
                   "stand in for the concept). A look = formation, personnel group, and where the quarterback stands.\n",
                   "Situation = down and distance, grouped into seven game-relevant buckets. The guess for each snap\n",
                   "never uses that snap: it is built only from the caller's other snaps in the same cell, falling\n",
                   "back to his other snaps in the same down-and-distance bucket, then to his overall habit, if a cell\n",
                   "is too thin to guess from on its own."),
            n_q, format(QUAL_MIN, big.mark = ",")),
    sprintf(paste0("\nA caller's guess rate is a habit that repeats year to year (r = %+.2f across %d callers on odd-\n",
                   "vs even-numbered seasons), so this is identity, not one hot or cold stretch of tape. Built by R/46."),
            ct$estimate, nrow(half_dt))
  )) +
  theme(plot.margin = margin(10, 60, 8, 10))

save_fig("docs/figures/guess_the_play.png", p_final, w = 12.5, h = 10.5)

# =============================================================================
# VERIFICATION BLOCK
# =============================================================================
cat("\n\n================= VERIFICATION =================\n")
for (cl in NAMED) {
  r <- headline[off_caller == cl]
  cat(sprintf("%-16s look+situation %5.1f%% (rank %2d/%d)  situation only %5.1f%% (rank %2d/%d)  look adds %+.1f pts  n=%d plays\n",
              cl, 100 * r$guess_headline, r$rank_headline, n_q,
              100 * r$guess_situation, r$rank_situation, n_q,
              100 * r$look_adds, r$n_plays))
}
cat(sprintf("\nleague median: look+situation %.1f%%, situation only %.1f%%, look adds %.1f pts (n=%d callers)\n",
            100 * median(headline$guess_headline), 100 * median(headline$guess_situation),
            100 * median(headline$look_adds), n_q))
cat(sprintf("\nMcVay hypothesis: %s\n", verdict))
cat(sprintf("persistence: headline guess rate r=%+.2f\n", ct$estimate))
cat("\nwrote docs/figures/guess_the_play.png, data/derived/guess_the_play.csv\n")
