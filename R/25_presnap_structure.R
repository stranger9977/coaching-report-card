# =============================================================================
# 25_presnap_structure.R -- quantifying "McVay pre-snap movement" without a
# motion field.
#
# Michael's question, verbatim: "What ways do you think we can quantify the
# McVay pre-snap movement and Schnahan?" and, separately: "the McVay motion
# and movement stuff is like the next part of my writing."
#
# THE HONEST LIMIT. No public source in this repo has a motion-type field,
# and Sumer's charting API has no motion field anywhere -- checked against
# its full 193-column play schema and its full 122-column player-play
# schema, neither has one. So this script cannot count motions, pre-snap
# shifts, or jet sweeps that never got the ball. What it measures instead is
# the thing motion exists to produce: how many DIFFERENT pre-snap pictures a
# caller shows an opposing defense. A team that shifts personnel from play to
# play changes what the defense sees before the snap even if not one player
# goes in true motion, and a team that never varies its picture is showing
# the same picture whether or not it moves players around within it. Picture
# variety is the closest honest proxy for what Michael is describing, and
# this script says so on every chart rather than calling it "motion."
#
# DATA, NEWLY PULLED. data/raw/sumer/plays_players_p1.csv.gz +
# plays_players_p2.csv.gz, 4,764,650 rows, one row per player per play,
# never analyzed before this script. Restricted to offense, season
# 2022-2025 (2026 is 58,716 rows of preseason charting, excluded). Joined to
# play context and offensive-caller attribution via
# R/factory/lib_sumer.R's load_sumer() on sumer_play_id.
#
# THE PICTURE. Per play, the sorted set of (alignment, alignment_side,
# off_on_los_alignment) for the offense's skill players (RB, FB, TE, WR,
# SWR/slot WR). Offensive linemen and the QB are dropped: linemen almost
# never vary and Sumer's QB alignment_side is "MIDDLE" 99.98% of the time
# regardless of shotgun/pistol/under center, so it carries no information
# here. This tuple set, concatenated into one string, is the "picture."
#
# ROLE WAS DROPPED. Sumer's player-play table has a `role` field (RUN BLOCK,
# PASS ROUTE, PASS RUSH, etc.) that looked like a natural fourth ingredient
# for the picture. It was checked and rejected: role is what the player did
# AFTER the snap, not his pre-snap picture -- among offense skill players,
# PASS ROUTE is tagged on a pass 94.7% of the time and RUN BLOCK on a run
# 99.98% of the time. Building the picture out of role and then asking
# whether the picture predicts run/pass would be circular, close to reading
# the answer off the question. The picture here is pre-snap-only: alignment,
# side, on/off the line.
#
# THE VARIETY MEASURE. Per caller, per down-distance bucket (same seven
# buckets as R/14: 1st-and-10, 2nd/3rd short/medium/long), the entropy of
# his picture distribution, shrunk toward the LEAGUE's picture distribution
# in that bucket (K=15 pseudo-plays), then plays-weighted-averaged across
# buckets. Controlling for down-distance means the score is about how many
# different pictures a caller shows in the SAME situation, not about facing
# more third-and-longs. Shrinking toward the league prior (rather than
# toward his own average, which is what R/09 and R/14 do for a different
# question) keeps a caller who shows 40 different one-off pictures in 200
# plays from scoring as more "varied" than a caller who shows the same 40
# pictures on repeat -- both are estimating the SAME underlying diversity
# with different sample noise, and EB shrinkage is what separates them.
# Closed-form derivation in eb_entropy_closed() below: the picture
# vocabulary runs into the thousands, too many to cross-join against every
# category a caller never showed, so the shrunk entropy is computed from
# only the cells he actually has plus one league-entropy-per-bucket
# constant (sum over unseen categories of q*log2(q) = -H_league - sum over
# seen categories of q*log2(q), since the two partitions cover everything).
#
# THE LEAK MEASURE. Same nested-shrinkage construction as R/09's presnap
# tip, applied to the player-level picture instead of a formation:personnel
# code: per caller, the entropy of run/pass within a down-distance bucket
# (shrunk toward the league's bucket rate) minus the entropy of run/pass
# within a (bucket, picture) cell (shrunk toward the caller's OWN bucket
# rate). Positive means knowing the picture, on top of the situation, tells
# you more about what's coming. This is what test 3 below calls whether the
# picture "lies": a caller can show many pictures that all still mean
# nothing (variety as camouflage), or few pictures that mean nothing
# (Ben Johnson's known same-look pattern, R/14), or pictures that leak.
#
# UNIVERSE. 2022-2025, run_pass in {P, R}, garbage_time == FALSE (house
# convention since R/18), off_caller present, at least 4 skill players
# charted on the play (drops incompletely-charted rows). Callers need at
# least 1200 qualifying plays, matching R/14's QUAL_MIN.
#
# Out: docs/figures/presnap_variety.png  (headline leaderboard, 3 names marked)
#      docs/figures/presnap_lie.png      (variety x leak, two-axis)
#      data/derived/presnap_callers.csv  (every measure, every qualified caller)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

safe_log2 <- function(p) fifelse(p > 0, log2(p), 0)
Hbin <- function(p) { p <- pmin(pmax(p, 1e-9), 1 - 1e-9); -(p * log2(p) + (1 - p) * log2(1 - p)) }
K <- 15          # EB shrinkage strength, matches R/09 and R/14's convention
QUAL_MIN <- 1200 # matches R/14's QUAL_MIN

NAMED <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson", "Andy Reid", "Matt LaFleur",
           "Mike McDaniel", "Kevin O'Connell", "Sean Payton", "Liam Coen", "Todd Monken",
           "Arthur Smith", "Zac Taylor", "Brian Callahan", "Klint Kubiak", "Bobby Slowik",
           "Joe Brady", "Nathaniel Hackett", "Shane Steichen")
HEADLINE <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")

# ---------------------------------------------------------------- closed-form EB entropy
# See header for the derivation. `x` needs columns group/bucket/cat; returns
# one row per (group, bucket) with the shrunk entropy H.
eb_entropy_closed <- function(x, group, bucket, cat, K) {
  x <- x[, .(g = get(group), b = get(bucket), s = get(cat))]
  cell <- x[, .(n = .N), by = .(g, b, s)]
  league <- cell[, .(N = sum(n)), by = .(b, s)]
  league[, q := N / sum(N), by = b]
  Hq <- league[, .(Hq = -sum(q * safe_log2(q))), by = b]
  cell <- merge(cell, league[, .(b, s, q)], by = c("b", "s"))
  cell[, Ngb := sum(n), by = .(g, b)]
  cell[, p_shr := (n + K * q) / (Ngb + K)]
  agg <- cell[, .(Ngb = Ngb[1], Qobs = sum(q), Sq = sum(q * safe_log2(q)),
                  Hobs = sum(-p_shr * safe_log2(p_shr))), by = .(g, b)]
  agg <- merge(agg, Hq, by = "b")
  agg[, cc := K / (Ngb + K)]
  agg[, H := Hobs - (cc * log2(cc) * (1 - Qobs) + cc * (-Hq - Sq))]
  setnames(agg, c("g", "b"), c(group, bucket))
  agg[]
}

# ---------------------------------------------------------------- play context + callers
plays <- load_sumer(2022:2025)
plays <- plays[run_pass %in% c("P", "R") & garbage_time == FALSE & off_caller != ""]
plays[, dd_bucket := fifelse(down == 1, "1st-and-10",
                      fifelse(down == 2 & distance <= 3, "2nd-short",
                      fifelse(down == 2 & distance <= 7, "2nd-medium",
                      fifelse(down == 2, "2nd-long",
                      fifelse(down == 3 & distance <= 3, "3rd-short",
                      fifelse(down == 3 & distance <= 7, "3rd-medium",
                      fifelse(down == 3, "3rd-long", NA_character_)))))))]
plays <- plays[!is.na(dd_bucket)]
plays[, is_pass := as.integer(run_pass == "P")]
cat(sprintf("Sumer plays, 2022-2025, non-garbage-time, caller-attributed, down-distance bucketed: %s\n",
            format(nrow(plays), big.mark = ",")))

# ---------------------------------------------------------------- per-player pre-snap structure
PCOLS <- c("sumer_play_id", "season", "side_of_ball", "alignment", "alignment_side",
           "off_on_los_alignment", "bunched_alignment", "substitution_in", "substitution_out")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
n_total <- nrow(players)
n_2026 <- players[season == 2026, .N]
n_defense <- players[season %in% 2022:2025 & side_of_ball == "defense", .N]
players <- players[season %in% 2022:2025 & side_of_ball == "offense"]
cat(sprintf("player-play rows: %s total, %s season-2026 dropped, %s defense-side dropped, %s offense 2022-2025 rows kept\n",
            format(n_total, big.mark = ","), format(n_2026, big.mark = ","), format(n_defense, big.mark = ","),
            format(nrow(players), big.mark = ",")))

SKILL <- c("RB", "FB", "TE", "WR", "SWR")
skill <- players[alignment %in% SKILL]
skill[, part := paste(alignment, alignment_side, off_on_los_alignment, sep = ":")]
sig_dt <- skill[, .(
  sig       = paste(sort(part), collapse = "|"),
  shape     = paste(sort(alignment), collapse = ","),
  n_skill   = .N,
  n_off_los = sum(off_on_los_alignment == FALSE, na.rm = TRUE),
  any_bunch = any(bunched_alignment == TRUE, na.rm = TRUE)
), by = sumer_play_id]
cat("\nskill players charted per play (RB/FB/TE/WR/SWR):\n")
print(table(sig_dt$n_skill))

sub_dt <- players[, .(any_sub = any(substitution_in == TRUE | substitution_out == TRUE, na.rm = TRUE)),
                   by = sumer_play_id]
rm(players, skill); gc(verbose = FALSE)

d <- merge(plays, sig_dt, by = "sumer_play_id")
d <- merge(d, sub_dt, by = "sumer_play_id", all.x = TRUE)
d[is.na(any_sub), any_sub := FALSE]
rm(sig_dt, sub_dt); gc(verbose = FALSE)
cat(sprintf("\nplays joined to a pre-snap picture: %s of %s play-context rows (%.1f%%)\n",
            format(nrow(d), big.mark = ","), format(nrow(plays), big.mark = ","), 100 * nrow(d) / nrow(plays)))
d <- d[n_skill >= 4]
cat(sprintf("after dropping incompletely-charted plays (< 4 skill players): %s\n", format(nrow(d), big.mark = ",")))

qual <- d[, .N, by = off_caller][N >= QUAL_MIN, off_caller]
dq <- d[off_caller %in% qual]
cat(sprintf("qualified callers (>= %d plays): %d of %d\n", QUAL_MIN, length(qual), uniqueN(d$off_caller)))
cat(sprintf("distinct pictures observed leaguewide: %s\n", format(uniqueN(dq$sig), big.mark = ",")))

# =============================================================================
# TEST 1: PICTURE VARIETY -- who shows the most different pre-snap pictures
# =============================================================================
cat("\n\n=== TEST 1: PICTURE VARIETY ===\n")

res1 <- eb_entropy_closed(dq, "off_caller", "dd_bucket", "sig", K)
variety <- res1[, .(n_plays = sum(Ngb), H_variety = weighted.mean(H, Ngb)), by = off_caller]
setorder(variety, -H_variety)
variety[, rank := frank(-H_variety)]
cat(sprintf("callers scored: %d | league median variety: %.3f bits | K=%d shrinkage\n",
            nrow(variety), median(variety$H_variety), K))

cat("\n--- most varied pre-snap pictures ---\n")
print(head(variety[, .(off_caller, n_plays, H_variety = round(H_variety, 3), rank)], 8))
cat("\n--- least varied (most repetitive) ---\n")
print(tail(variety[, .(off_caller, n_plays, H_variety = round(H_variety, 3), rank)], 8))

cat("\n--- the three names ---\n")
print(variety[off_caller %in% HEADLINE, .(off_caller, n_plays, H_variety = round(H_variety, 3), rank,
                                          pct = round(100 * (nrow(variety) - rank + 1) / nrow(variety)))])

# =============================================================================
# TEST 2: COMPONENTS -- the ingredients under "pre-snap movement"
# =============================================================================
cat("\n\n=== TEST 2: COMPONENTS ===\n")

comp <- dq[, .(
  n = .N,
  bunch_rate          = 100 * mean(any_bunch),
  off_los_mean         = mean(n_off_los),
  multi_detached_rate = 100 * mean(n_off_los >= 2),
  sub_rate             = 100 * mean(any_sub)
), by = off_caller]

res2 <- eb_entropy_closed(dq, "off_caller", "dd_bucket", "shape", K)
shape_h <- res2[, .(H_shape = weighted.mean(H, Ngb)), by = off_caller]
comp <- merge(comp, shape_h, by = "off_caller")
comp <- merge(comp, variety[, .(off_caller, H_variety, rank)], by = "off_caller")
setorder(comp, -H_variety)

cat(sprintf("league medians -- bunch rate %.1f%%, mean skill players off the line %.2f, 2+ detached rate %.1f%%, sub-churn rate %.1f%%, backfield-shape entropy %.3f bits\n",
            median(comp$bunch_rate), median(comp$off_los_mean), median(comp$multi_detached_rate),
            median(comp$sub_rate), median(comp$H_shape)))

cat("\n--- the three names, every ingredient ---\n")
print(comp[off_caller %in% HEADLINE, .(off_caller, n, bunch_rate = round(bunch_rate, 1),
                                        off_los_mean = round(off_los_mean, 2),
                                        multi_detached_rate = round(multi_detached_rate, 1),
                                        sub_rate = round(sub_rate, 1),
                                        H_shape = round(H_shape, 3),
                                        H_variety = round(H_variety, 3), variety_rank = rank)])

# =============================================================================
# TEST 3: DOES THE PICTURE LIE -- does the picture predict run vs pass?
# =============================================================================
cat("\n\n=== TEST 3: DOES THE PICTURE LIE ===\n")

situ <- dq[, .(n = .N, p_raw = mean(is_pass)), by = .(off_caller, dd_bucket)]
lg_situ <- dq[, .(lg_p = mean(is_pass)), by = dd_bucket]
situ <- merge(situ, lg_situ, by = "dd_bucket")
situ[, p_shr := (n * p_raw + K * lg_p) / (n + K)]
situ[, H_situ := Hbin(p_shr)]

sigp <- dq[, .(n = .N, p_raw = mean(is_pass)), by = .(off_caller, dd_bucket, sig)]
sigp <- merge(sigp, situ[, .(off_caller, dd_bucket, p_shr_situ = p_shr)], by = c("off_caller", "dd_bucket"))
sigp[, p_shr := (n * p_raw + K * p_shr_situ) / (n + K)]
sigp[, H_sig := Hbin(p_shr)]

tip <- merge(
  situ[, .(H_situ_mean = weighted.mean(H_situ, n)), by = off_caller],
  sigp[, .(H_sig_mean = weighted.mean(H_sig, n), n_sig = sum(n)), by = off_caller],
  by = "off_caller"
)
tip[, leak := H_situ_mean - H_sig_mean]
setorder(tip, -leak)
tip[, leak_rank := frank(-leak)]
cat(sprintf("league median leak: %.4f bits (positive = the picture reveals run/pass beyond the situation)\n",
            median(tip$leak)))

cat("\n--- pictures that leak the most ---\n")
print(head(tip[, .(off_caller, leak = round(leak, 4), leak_rank)], 8))
cat("\n--- pictures that leak the least (best camouflage or true no-signal) ---\n")
print(tail(tip[, .(off_caller, leak = round(leak, 4), leak_rank)], 8))

cat("\n--- the three names ---\n")
print(tip[off_caller %in% HEADLINE, .(off_caller, leak = round(leak, 4), leak_rank,
                                       pct = round(100 * (nrow(tip) - leak_rank + 1) / nrow(tip)))])

# =============================================================================
# combined table + honest one-line answers
# =============================================================================
out <- merge(variety[, .(off_caller, n_plays, H_variety, variety_rank = rank)],
             comp[, .(off_caller, bunch_rate, off_los_mean, multi_detached_rate, sub_rate, H_shape)],
             by = "off_caller")
out <- merge(out, tip[, .(off_caller, H_situ_mean, H_sig_mean, leak, leak_rank)], by = "off_caller")
setorder(out, variety_rank)
write_csv(as.data.frame(out), "data/derived/presnap_callers.csv")
cat(sprintf("\nwrote data/derived/presnap_callers.csv (%d callers)\n", nrow(out)))

cat("\n\n=== ANSWERS TO MICHAEL'S QUESTION ===\n")
cat("We cannot count McVay's or Shanahan's motions -- no source in this repo, including Sumer, tags motion type.\n")
cat("What we can measure is the variety of pre-snap pictures each caller shows, which is what motion exists to create.\n\n")
for (nm in HEADLINE) {
  v <- variety[off_caller == nm]; c1 <- comp[off_caller == nm]; t1 <- tip[off_caller == nm]
  if (!nrow(v)) { cat(sprintf("%s: did not qualify (< %d plays).\n\n", nm, QUAL_MIN)); next }
  cat(sprintf("%s: picture variety rank #%d of %d (%.3f bits vs league median %.3f). Bunched-alignment rate %.1f%% (league %.1f%%). Sub-churn rate %.1f%% (league %.1f%%). Picture leak rank #%d of %d (%+.4f bits vs league median %+.4f) -- %s.\n\n",
              nm, v$rank, nrow(variety), v$H_variety, median(variety$H_variety),
              c1$bunch_rate, median(comp$bunch_rate), c1$sub_rate, median(comp$sub_rate),
              t1$leak_rank, nrow(tip), t1$leak, median(tip$leak),
              if (t1$leak > median(tip$leak)) "his picture gives away more than a typical caller's" else "his picture gives away no more than a typical caller's"))
}

# =============================================================================
# CHART A: picture variety leaderboard
# =============================================================================
variety[, hl := off_caller %in% HEADLINE]
lab_a <- variety[hl == TRUE]
lab_a[, lab := sprintf("%s\n#%d of %d", off_caller, rank, nrow(variety))]

pA <- ggplot(variety, aes(reorder(off_caller, H_variety), H_variety)) +
  geom_hline(yintercept = median(variety$H_variety), linetype = "dashed",
             colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(colour = hl, size = hl)) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "#9db6c9"), guide = "none") +
  scale_size_manual(values = c(`TRUE` = 4.2, `FALSE` = 1.9), guide = "none") +
  geom_text_repel(data = lab_a, aes(label = lab), size = 3.0, fontface = "bold",
                   colour = "#8a3d00", lineheight = 0.9, seed = 5,
                   box.padding = 0.5, min.segment.length = 0, max.overlaps = 20) +
  labs(title = "How many different pre-snap looks each play-caller shows",
       subtitle = "Variety of distinct pre-snap alignments (every skill player's spot), measured within down and distance",
       x = NULL, y = "variety of pre-snap looks (bits, shrunk toward the league)") +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_blank())

save_fig("docs/figures/presnap_variety.png", pA, w = 13, h = 6.8)

# =============================================================================
# CHART B: variety vs leak, two-axis
# =============================================================================
both <- merge(variety[, .(off_caller, H_variety)], tip[, .(off_caller, leak)], by = "off_caller")
both[, hl := off_caller %in% HEADLINE]
both[, named := off_caller %in% NAMED]
med_v <- median(both$H_variety); med_l <- median(both$leak)
lab_b <- both[named == TRUE]

pB <- ggplot(both, aes(H_variety, leak)) +
  geom_hline(yintercept = med_l, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = med_v, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(data = both[named == FALSE], colour = "#9db6c9", alpha = 0.6, size = 2.1) +
  geom_point(data = both[named == TRUE & hl == FALSE], colour = "#2B8CBE", size = 2.8) +
  geom_point(data = both[hl == TRUE], colour = "#D55E00", size = 4) +
  geom_text_repel(data = lab_b, aes(label = off_caller, fontface = ifelse(hl, "bold", "plain")),
                   size = 2.9, colour = ifelse(lab_b$hl, "#8a3d00", "#1d6a99"), seed = 9,
                   box.padding = 0.42, min.segment.length = 0, max.overlaps = 25) +
  annotate("text", x = min(both$H_variety), y = max(both$leak), hjust = 0, vjust = 1, size = 2.85,
           fontface = "italic", colour = "grey45", label = "few looks, and they tip the play") +
  annotate("text", x = max(both$H_variety), y = max(both$leak), hjust = 1, vjust = 1, size = 2.85,
           fontface = "italic", colour = "grey45", label = "many looks, and they still tip the play") +
  annotate("text", x = min(both$H_variety), y = min(both$leak), hjust = 0, vjust = 0, size = 2.85,
           fontface = "italic", colour = "grey45", label = "few looks, none of them tip anything") +
  annotate("text", x = max(both$H_variety), y = min(both$leak), hjust = 1, vjust = 0, size = 2.85,
           fontface = "italic", colour = "grey45", label = "many looks, and the variety is the camouflage") +
  labs(subtitle = "Showing many different looks does not automatically hide the play.\nx = how many different pre-snap looks a caller shows; y = how much the look tips run vs pass beyond down, distance, score and clock.",
       x = "variety of pre-snap looks (bits)", y = "how much the look tips run vs pass (bits)") +
  theme_coach(grid = "none")

TITLE_B <- sprintf("McVay: looks-variety #%d of %d, look-tips-the-play #%d of %d; Shanahan #%d and #%d; Ben Johnson #%d and #%d",
                    variety[off_caller == "Sean McVay", rank], nrow(variety),
                    tip[off_caller == "Sean McVay", leak_rank], nrow(tip),
                    variety[off_caller == "Kyle Shanahan", rank], tip[off_caller == "Kyle Shanahan", leak_rank],
                    variety[off_caller == "Ben Johnson", rank], tip[off_caller == "Ben Johnson", leak_rank])

p_final <- pB + plot_annotation(
  title = TITLE_B,
  caption = fig_caption(
    "Sumer play and player charting 2022-2025, play-caller attribution from samhoppen/NFL_public",
    sprintf("%d qualified callers (%d+ plays), %s plays, picture built from RB/FB/TE/WR/SWR alignment only.",
            nrow(both), QUAL_MIN, format(nrow(dq), big.mark = ",")),
    paste0("\nNeither Sumer nor any other source in this repo tags motion type, so this is not a motion count. It measures the variety of pre-snap looks a caller shows and whether\n",
           "those looks give away run or pass on top of what the down, distance, score and clock already tell a defense. Both axes are empirical-Bayes shrunk (K=15) so thin\n",
           "cells cannot manufacture either a varied or a deceptive caller. Note for the record: R/14 found Ben Johnson hides his play mix well from a COARSE look (QB spot, backfield\n",
           "count, motion); on this richer per-player look he does not repeat that pattern; he shows an above-median mix of looks and they tip the play more than a typical caller's. Built by R/25.")
  ),
  theme = theme_coach(grid = "none")
)
save_fig("docs/figures/presnap_lie.png", p_final, w = 13.6, h = 7.6)

cat("\n\n=== DONE ===\n")
cat("wrote docs/figures/presnap_variety.png, docs/figures/presnap_lie.png\n")
cat("wrote data/derived/presnap_callers.csv\n")
