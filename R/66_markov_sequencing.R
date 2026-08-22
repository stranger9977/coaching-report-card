# =============================================================================
# 66_markov_sequencing.R -- play sequencing as a Markov chain.
#
# The ask: "for play sequencing something we didn't think about was markov
# chain ... a good, robust attempt at using it to measure the effect of play
# sequencing and trying to figure out if there are certain play sequencing
# stuff that is an edge and if certain coaches have a play sequencing
# something."
#
# What the earlier passes did (R/13, R/46, nfl-analysis sequencing_death):
# run-vs-pass sequence lift in bits, and the establish-the-run setup test,
# which came up empty for everyone. The Markov framing changes two things:
#   (a) the STATE is a play family, not run/pass. Eight states: outside zone,
#       inside zone, gap run, other run, play action, screen, short dropback,
#       deep dropback. The board's own feedback says run/pass is too coarse
#       to carry a sequencing claim.
#   (b) the whole 8x8 transition matrix is the object, which gives things
#       run/pass cannot: a memory half-life from the chain's eigenvalues, a
#       pair-by-pair map of which transitions carry value, and a clean test
#       of whether the chain needs TWO plays of memory or one.
#
# Five questions, each with a permutation null so thin cells cannot
# manufacture a signal:
#   1. SIGNATURE. How much does the previous play family predict the next
#      one, beyond down and distance? Mutual information I(prev; next | dd),
#      per caller, against a null that shuffles prev within (caller, dd).
#      Also rendered as the defense's number: extra correct guesses per 100
#      plays from knowing the last call (leave-one-out, rarefied to 1,500
#      plays so sample size cannot drive it).
#   2. MEMORY. Second eigenvalue of each caller's raw transition matrix.
#      Half-life = plays until half the information in the last call is
#      gone. Raw, so situation autocorrelation is in it; flagged as such.
#   3. ORDER. Does the play two back add information beyond the play one
#      back? I(prev2; next | dd, prev1) against its own shuffle null. If
#      this is zero, a first-order chain is the whole story.
#   4. PAIR EDGE. For each (prev -> next) pair, league-wide: EPA of the
#      next play minus the league mean for that down-distance AND that play
#      family. Nonzero means the setup play itself moved the result.
#      Null: shuffle prev within (dd, next).
#   5. CALLER VALUE. Two numbers per caller. SELECTION: does his transition
#      mix walk through the league's profitable pairs more than a
#      memoryless version of himself would (his pair frequencies minus
#      the product of his marginals, weighted by the league pair edges)?
#      STRUCTURE: do his own pairs carry any value at all (weighted variance
#      of his pair-mean residuals vs his own shuffle null)?
#
# Transitions are consecutive called plays (run or pass) INSIDE THE SAME
# DRIVE, in game order, garbage time excluded. The first play of a drive has
# no previous play and is dropped from every transition count.
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season spans in rendered text, plain language.
#
# Out: docs/figures/markov_league_matrix.png, markov_order.png
#      docs/figures/markov_signature.png
#      docs/figures/markov_memory.png
#      docs/figures/markov_pair_edge.png
#      docs/figures/markov_caller_value.png
#      data/derived/markov_callers.csv, markov_pairs.csv
#      docs/data/markov.json   (feeds docs/markov.html)
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(ggrepel)
  library(scales); library(jsonlite)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(66)
dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)

MIN_PLAYS <- 1500
RAREFY_N  <- 1500
N_DRAWS   <- 50
N_PERM    <- 200
MARQUEE   <- c("Kyle Shanahan", "Sean McVay", "Ben Johnson", "Andy Reid",
               "Matt LaFleur", "Sean Payton", "Kevin O'Connell")

# ---------------------------------------------------------------- states
d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & off_caller != ""]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)

gap <- c("MAN", "POWER", "PULL LEAD", "COUNTER", "TRAP", "FULLBACK")
d[, state := fcase(
  run_pass == "R" & run_concept == "OUTSIDE ZONE", "outside zone",
  run_pass == "R" & run_concept == "INSIDE ZONE",  "inside zone",
  run_pass == "R" & run_concept %in% gap,          "gap run",
  run_pass == "R" & run_concept != "SCRAMBLE",     "other run",
  run_pass == "P" & screen == TRUE,                "screen",
  run_pass == "P" & play_action == TRUE,           "play action",
  run_pass == "P" & !is.na(depth_of_target) & depth_of_target >= 10, "deep dropback",
  run_pass == "P",                                 "short dropback",
  default = NA_character_)]
# a scramble is a dropback that turned into a run; the CALL was a pass
d[run_pass == "R" & run_concept == "SCRAMBLE", state := "short dropback"]
d <- d[!is.na(state)]
STATES <- c("outside zone", "inside zone", "gap run", "other run",
            "play action", "screen", "short dropback", "deep dropback")
d[, state := factor(state, levels = STATES)]
d[, dd := fifelse(down == 1, "1st",
          fifelse(down == 2 & distance <= 3, "2nd short", fifelse(down == 2 & distance <= 7, "2nd medium",
          fifelse(down == 2, "2nd long", fifelse(down == 3 & distance <= 3, "3rd short",
          fifelse(down == 3 & distance <= 7, "3rd medium", fifelse(down == 3, "3rd long", "4th")))))))]

# previous play inside the same drive
d[, prev  := shift(state), by = drive_id]
d[, prev2 := shift(state, 2), by = drive_id]
d[, k_in_drive := seq_len(.N), by = drive_id]
d[, kb := fifelse(k_in_drive <= 3, "3rd play", fifelse(k_in_drive <= 5, "4th-5th play", "6th+ play"))]
d[, epa   := expected_points_added]
tr <- d[!is.na(prev) & !is.na(epa)]
cat(sprintf("plays: %d   transitions (same drive): %d\n", nrow(d), nrow(tr)))
cat("state mix:\n"); print(round(prop.table(table(d$state)), 3))

callers <- tr[, .N, by = off_caller][N >= MIN_PLAYS][order(-N)]$off_caller
cat(sprintf("%d callers with >= %d same-drive transitions\n", length(callers), MIN_PLAYS))
tr <- tr[off_caller %in% callers]

# ---------------------------------------------------------------- helpers
# conditional mutual information I(a; b | c) in bits, plug-in
cmi <- function(a, b, cc) {
  x <- data.table(aa = a, bb = b, cc = cc)[, .N, by = .(aa, bb, cc)]
  x[, nc := sum(N), by = cc]
  x[, nac := sum(N), by = .(aa, cc)]
  x[, nbc := sum(N), by = .(bb, cc)]
  x[, sum(N / sum(N) * log2(N * nc / (nac * nbc)))]
}
# shuffle a within groups c
shuf <- function(a, c) {
  idx <- data.table(i = seq_along(a), c)[, .(i, j = i[sample.int(.N)]), by = c]
  a[idx[order(i)]$j]
}
# leave-one-out top-guess accuracy. The fine cell's counts (minus the play being
# guessed) are shrunk toward the coarse cell's distribution with K pseudo-plays,
# the same K=15 empirical-Bayes rule as R/13, so a thin cell cannot manufacture
# a guess from itself and cannot be punished for being thin either.
loo_acc <- function(state, fine, coarse, K = 15) {
  s  <- as.integer(factor(state, levels = STATES)); n <- length(s)
  fi <- as.integer(factor(fine)); ci <- as.integer(factor(coarse))
  Fm <- as.matrix(table(factor(fi, levels = seq_len(max(fi))), factor(s, levels = 1:8)))
  Cm <- as.matrix(table(factor(ci, levels = seq_len(max(ci))), factor(s, levels = 1:8)))
  oh <- matrix(0, n, 8); oh[cbind(seq_len(n), s)] <- 1
  f  <- Fm[fi, , drop = FALSE] - oh
  cc <- Cm[ci, , drop = FALSE] - oh
  p  <- f + K * cc / pmax(rowSums(cc), 1)
  own <- p[cbind(seq_len(n), s)]
  p[cbind(seq_len(n), s)] <- -Inf
  mean(own > apply(p, 1, max))     # ties count as wrong
}

# ---------------------------------------------------------------- 1. signature
sig <- rbindlist(lapply(callers, function(cl) {
  x <- tr[off_caller == cl]
  obs <- cmi(x$prev, x$state, x$dd)
  nul <- replicate(N_PERM, cmi(shuf(x$prev, x$dd), x$state, x$dd))
  # guessability, rarefied leave-one-out
  g <- replicate(N_DRAWS, {
    s <- x[sample.int(.N, RAREFY_N)]
    c(sit  = loo_acc(s$state, s$dd, rep("all", nrow(s))),
      seq  = loo_acc(s$state, paste(s$dd, s$prev), s$dd))
  })
  data.table(off_caller = cl, n = nrow(x), mi_obs = obs, mi_null = mean(nul), mi_null_sd = sd(nul),
             mi_excess = obs - mean(nul), mi_z = (obs - mean(nul)) / sd(nul),
             guess_sit = mean(g["sit", ]) * 100, guess_seq = mean(g["seq", ]) * 100)
}))
sig[, guess_gain := guess_seq - guess_sit]
cat("\nsignature (bits beyond situation, excess over shuffle null):\n")
print(sig[order(-mi_excess), .(off_caller, n, mi_excess = round(mi_excess, 4), mi_z = round(mi_z, 1),
                               guess_sit = round(guess_sit, 1), guess_gain = round(guess_gain, 1))])

# league-level signature for the record
lg_obs <- cmi(tr$prev, tr$state, tr$dd)
lg_nul <- replicate(50, cmi(shuf(tr$prev, paste(tr$off_caller, tr$dd)), tr$state, tr$dd))
cat(sprintf("\nleague I(prev; next | dd): %.4f bits, null %.4f (sd %.4f)\n", lg_obs, mean(lg_nul), sd(lg_nul)))

# ---------------------------------------------------------------- 2. memory
tmat <- function(x) {
  m <- as.matrix(table(factor(x$prev, levels = STATES), factor(x$state, levels = STATES)))
  m / pmax(rowSums(m), 1)
}
half_life <- function(P) {
  ev <- eigen(P, only.values = TRUE)$values
  l2 <- sort(Mod(ev), decreasing = TRUE)[2]
  c(lambda2 = l2, half_life = log(0.5) / log(l2))
}
mem <- rbindlist(lapply(callers, function(cl) {
  x <- tr[off_caller == cl]
  h <- half_life(tmat(x))
  # null: same plays, order shuffled within (dd) so situation structure survives
  hn <- replicate(N_PERM, half_life(tmat(data.table(prev = shuf(x$prev, x$dd), state = x$state)))["half_life"])
  data.table(off_caller = cl, lambda2 = h["lambda2"], half_life = h["half_life"],
             half_life_null = mean(hn), half_life_null_sd = sd(hn))
}))
mem[, half_life_excess := half_life - half_life_null]
lg_h <- half_life(tmat(tr))
cat(sprintf("\nleague chain: lambda2 %.3f, half-life %.2f plays\n", lg_h["lambda2"], lg_h["half_life"]))
cat("memory half-life by caller (plays), with situation-only null:\n")
print(mem[order(-half_life), .(off_caller, half_life = round(half_life, 2), null = round(half_life_null, 2))])

# ---------------------------------------------------------------- 3. order
# Does the play TWO back add information beyond the play one back? The trap:
# a play with two predecessors is by definition the 3rd play of the drive or
# later, and deep-drive plays are a different population (the drive is
# working). So both orders are tested on the same plays (3rd-in-drive or
# later) and both condition on where in the drive the play sits, so drive
# length cannot masquerade as memory.
tr2 <- tr[!is.na(prev2)]
ord <- rbindlist(lapply(callers, function(cl) {
  x <- tr2[off_caller == cl]
  c1 <- paste(x$dd, x$kb); c2 <- paste(x$dd, x$kb, x$prev)
  o1 <- cmi(x$prev, x$state, c1);  n1 <- replicate(N_PERM, cmi(shuf(x$prev, c1), x$state, c1))
  o2 <- cmi(x$prev2, x$state, c2); n2 <- replicate(N_PERM, cmi(shuf(x$prev2, c2), x$state, c2))
  data.table(off_caller = cl, n2 = nrow(x),
             mi1_excess = o1 - mean(n1), mi1_z = (o1 - mean(n1)) / sd(n1),
             mi2_excess = o2 - mean(n2), mi2_z = (o2 - mean(n2)) / sd(n2))
}))
c1 <- paste(tr2$dd, tr2$kb); c2 <- paste(tr2$dd, tr2$kb, tr2$prev)
lg1_obs <- cmi(tr2$prev, tr2$state, c1);  lg1_nul <- replicate(50, cmi(shuf(tr2$prev, paste(tr2$off_caller, c1)), tr2$state, c1))
lg2_obs <- cmi(tr2$prev2, tr2$state, c2); lg2_nul <- replicate(50, cmi(shuf(tr2$prev2, paste(tr2$off_caller, c2)), tr2$state, c2))
cat(sprintf("\nleague, drive position controlled: one back %.4f bits (null %.4f), two back %.4f bits (null %.4f)\n",
            lg1_obs, mean(lg1_nul), lg2_obs, mean(lg2_nul)))
cat(sprintf("callers clearing z > 3: one back %d of %d, two back %d of %d\n",
            sum(ord$mi1_z > 3), nrow(ord), sum(ord$mi2_z > 3), nrow(ord)))
cat("order by caller:\n")
print(ord[order(-mi2_excess), .(off_caller, one_back = round(mi1_excess, 4), two_back = round(mi2_excess, 4), z2 = round(mi2_z, 1))])

# ---------------------------------------------------------------- 4. pair edge
tr[, r := epa - mean(epa), by = .(dd, state)]
pairs <- tr[, .(n = .N, edge = mean(r), se = sd(r) / sqrt(.N)), by = .(prev, state)]
pnull <- rbindlist(lapply(seq_len(N_PERM), function(k) {
  y <- data.table(prev = shuf(tr$prev, paste(tr$dd, tr$state)), state = tr$state, r = tr$r)
  y[, .(edge = mean(r)), by = .(prev, state)][, k := k]
}))
pn <- pnull[, .(null_mean = mean(edge), null_sd = sd(edge)), by = .(prev, state)]
pairs <- merge(pairs, pn, by = c("prev", "state"))
pairs[, z := (edge - null_mean) / null_sd]
setorder(pairs, -z)
cat("\npair edges clearing |z| > 3 (64 pairs tested, so about 0.2 expected by luck):\n")
print(pairs[abs(z) > 3, .(prev, next_play = state, n, edge = round(edge, 3), z = round(z, 1))])
cat("\nstrongest pairs either way:\n")
print(rbind(head(pairs, 4), tail(pairs, 4))[, .(prev, next_play = state, n, edge = round(edge, 3), z = round(z, 1))])
write_csv(as.data.frame(pairs), "data/derived/markov_pairs.csv")

# ---------------------------------------------------------------- 5. caller value
edge_mat <- dcast(pairs, prev ~ state, value.var = "edge")
E <- as.matrix(edge_mat[, -1]); rownames(E) <- edge_mat$prev; E <- E[STATES, STATES]
val <- rbindlist(lapply(callers, function(cl) {
  x <- tr[off_caller == cl]
  f  <- as.matrix(table(factor(x$prev, levels = STATES), factor(x$state, levels = STATES))) / nrow(x)
  indep <- outer(rowSums(f), colSums(f))
  selection <- sum((f - indep) * E, na.rm = TRUE)          # points per play from walking the good pairs
  # structure: weighted variance of his own pair-mean residuals vs his own shuffle null
  pm <- x[, .(n = .N, m = mean(r)), by = .(prev, state)]
  obs <- pm[, sum(n * m^2) / sum(n)]
  nul <- replicate(N_PERM, {
    y <- data.table(prev = shuf(x$prev, paste(x$dd, x$state)), state = x$state, r = x$r)
    y[, .(n = .N, m = mean(r)), by = .(prev, state)][, sum(n * m^2) / sum(n)]
  })
  # execution: his raw adjusted EPA, so the reader can see selection is a sliver of it
  data.table(off_caller = cl, selection = selection, structure_obs = obs, structure_null = mean(nul),
             structure_z = (obs - mean(nul)) / sd(nul), adj_epa = mean(x$r))
}))
cat("\ncaller value: selection = points/play from pair choice vs memoryless self; structure z = do his pairs carry any value:\n")
print(val[order(-selection), .(off_caller, selection = round(selection, 4), structure_z = round(structure_z, 1),
                               adj_epa = round(adj_epa, 3))])

# ---------------------------------------------------------------- tables out
out <- Reduce(function(a, b) merge(a, b, by = "off_caller"), list(sig, mem, ord, val))
write_csv(as.data.frame(out), "data/derived/markov_callers.csv")

# ---------------------------------------------------------------- figures
lab_c <- function(x) sub("Kevin O'Connell", "O'Connell", sub("^\\S+ ", "", x))
RUNS <- STATES[1:4]; PASSES <- STATES[5:8]

# fig 1: league lift matrix, P(next | prev) / P(next)
Pl <- tmat(tr); marg <- prop.table(table(factor(tr$state, levels = STATES)))
lift <- as.data.table(as.table(Pl))[, .(prev = V1, nxt = V2, p = N)]
lift <- merge(lift, data.table(nxt = STATES, m = as.numeric(marg)), by = "nxt")
lift[, ratio := p / m]
lift[, `:=`(prev = factor(prev, levels = rev(STATES)), nxt = factor(nxt, levels = STATES))]
p1 <- ggplot(lift, aes(nxt, prev, fill = ratio)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", ratio)), size = 3.2,
            colour = fifelse(lift$ratio > 1.35 | lift$ratio < 0.7, "white", "grey20")) +
  scale_fill_gradient2(low = "#2B8CBE", mid = "grey96", high = "#D55E00", midpoint = 1,
                       limits = c(0.5, 1.6), oob = squish, name = "times the\nbase rate") +
  labs(title = "The league's play-calling chain has a short memory and one strong habit",
       subtitle = paste0("Each cell: how often the NEXT play family (columns) follows the PREVIOUS one (rows), as a multiple of that family's overall rate.\n1.00 means the last call tells you nothing. The strong cells are runs following runs and screens following screens;\n",
                         "almost everything else sits near 1. 
Same drive only, all callers, 2022-23 through 2025-26."),
       x = NULL, y = "previous play",
       caption = fig_caption("SumerSports play charting, regular seasons, garbage time excluded",
         "\nThis is the raw chain, not adjusted for down and distance, which is why runs follow runs: first-down runs hand you second-and-medium, where the league\nruns again. The situation-adjusted version of this question is the signature chart. Built by R/66.")) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "right")
save_fig("docs/figures/markov_league_matrix.png", p1, w = 10.5, h = 7)

# fig 2: signature, guess gain per 100 plays with the bits version as the x-rank
sig[, lab := fifelse(off_caller %in% MARQUEE, lab_c(off_caller), "")]
sig[, band := 2 * mi_null_sd]
p2 <- ggplot(sig[order(mi_excess)], aes(x = reorder(off_caller, mi_excess), y = mi_excess)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_linerange(aes(ymin = -band, ymax = band), colour = "grey85", linewidth = 3) +
  geom_point(aes(colour = mi_z > 3), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, nudge_y = 0.004, direction = "y", segment.colour = "grey70") +
  annotate("label", x = 2.5, y = -0.023, hjust = 0, size = 2.9, colour = "grey35", fill = "white", label.size = 0,
           label = "grey bar: the range this caller's number lands in when his play order\nis shuffled within down and distance (2 standard deviations). Pure noise.") +
  annotate("label", x = nrow(sig) - 0.5, y = 0.0015, hjust = 1, size = 2.9, colour = "grey35", fill = "white", label.size = 0,
           label = "zero: the last play says nothing beyond down and distance") +
  annotate("label", x = nrow(sig) - 0.5, y = sig[order(mi_excess)][nrow(sig)]$mi_excess, hjust = 1, size = 2.9, colour = "grey35", fill = "white", label.size = 0,
           label = "dot: how much it actually says") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  labs(title = sprintf("%d of %d callers have a real sequencing signature, and it is small for all of them",
                       sum(sig$mi_z > 3), nrow(sig)),
       subtitle = paste0("How much the previous play family predicts the next one, beyond what down and distance already predict (bits of information,\n",
                         "minus what shuffling the order would produce). Orange: clears its noise band by 3 standard deviations.\n",
                         "For a defense guessing the play family, knowing the last call is worth ",
                         sprintf("%.1f", mean(sig$guess_gain)), " extra correct guesses per 100 plays\non average, ",
                         sprintf("%.1f", max(sig$guess_gain)), " at the most (", lab_c(sig[which.max(guess_gain)]$off_caller), ")."),
       x = NULL, y = "sequence information beyond situation (bits)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nEight play families as Markov states; transitions inside the same drive; callers with at least %s such transitions. Guessing numbers are leave-one-out,\nrarefied to %s plays per caller so sample size cannot drive them. Built by R/66.",
                 comma(MIN_PLAYS), comma(RAREFY_N)))) +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_blank())
save_fig("docs/figures/markov_signature.png", p2, w = 11, h = 6.5)

# fig 3: memory half-life vs situation-only null
mem[, lab := fifelse(off_caller %in% MARQUEE, lab_c(off_caller), "")]
p3 <- ggplot(mem, aes(half_life_null, half_life)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "22") +
  geom_point(aes(colour = half_life_excess > 2 * half_life_null_sd), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  expand_limits(x = c(0.2, 0.28), y = c(0.2, 0.5)) +
  labs(title = sprintf("The last call is half-forgotten within about %.1f plays, for everyone", median(mem$half_life)),
       subtitle = paste0("Memory half-life of each caller's chain: plays until half the information in the last play family has washed out, from the\n",
                         "second eigenvalue of his transition matrix. Across: the same number when his plays are shuffled within down and distance, which is\n",
                         "the memory that situation alone creates. On the dotted line, a caller's chain remembers nothing that the chains do not already know."),
       x = "half-life if only the situation carried memory (plays)", y = "actual memory half-life (plays)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nOrange: above the situation-only null by more than two of its standard deviations. Built by R/66.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/markov_memory.png", p3, w = 10, h = 7)

# fig 4: pair edge heatmap with significance
pairs[, `:=`(prev_f = factor(prev, levels = rev(STATES)), nxt_f = factor(state, levels = STATES))]
p4 <- ggplot(pairs, aes(nxt_f, prev_f, fill = edge)) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = fifelse(abs(z) > 3, sprintf("%+.2f\nz %.0f", edge, z), sprintf("%+.2f", edge))),
            size = fifelse(abs(pairs$z) > 3, 3.1, 2.8), fontface = fifelse(abs(pairs$z) > 3, "bold", "plain"),
            colour = fifelse(abs(pairs$edge) > 0.06, "white", "grey20")) +
  scale_fill_gradient2(low = "#2B8CBE", mid = "grey96", high = "#D55E00", midpoint = 0,
                       limits = c(-0.1, 0.1), oob = squish, name = "points per play\nvs expectation") +
  labs(title = sprintf("Does the setup play move the next one? %d of 64 pairs clear the noise",
                       sum(abs(pairs$z) > 3)),
       subtitle = paste0("Points added on the NEXT play minus the league average for that down, distance AND play family, split by what came right before.\n",
                         "A nonzero cell means the previous call itself changed the result, not the situation and not the play chosen. Bold cells clear a\n",
                         "shuffle null by 3 standard deviations; about 0.2 of 64 would do that by luck."),
       x = NULL, y = "previous play",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nColumns are the next play, rows the previous one. Same drive only, all qualified callers pooled. Built by R/66.")) +
  theme_coach(grid = "none") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "right")
save_fig("docs/figures/markov_pair_edge.png", p4, w = 10.5, h = 7.2)

# fig 5: caller selection value vs structure z
val[, lab := fifelse(off_caller %in% MARQUEE | abs(structure_z) > 3 | selection > 0 | selection < quantile(selection, 0.1),
                     lab_c(off_caller), "")]
p5 <- ggplot(val, aes(selection * 100, structure_z)) +
  geom_hline(yintercept = 3, colour = "grey80", linetype = "22") +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(aes(colour = structure_z > 3), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70", max.overlaps = 20) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  labs(title = sprintf("Sequencing as a coaching edge: worth under %.1f points per 100 plays for every caller",
                       ceiling(max(abs(val$selection)) * 1000) / 10),
       subtitle = paste0("Across: points per 100 plays a caller earns by steering his chain through the league's profitable pairs, compared with a memoryless\n",
                         "version of himself (same play mix, no order). Up: whether his own play-to-play pairs carry any value at all, in standard deviations\n",
                         "above his shuffle null. Above the dotted line the pairs matter for him; to the right he is picking the good ones."),
       x = "selection value (points per 100 plays vs memoryless self)", y = "pair structure (z vs own shuffle null)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nFor scale, the gap between a good and an average caller in situation-adjusted points per play is about 5 to 8 points per 100 plays. Built by R/66.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/markov_caller_value.png", p5, w = 10, h = 7)


# fig 6: one play back vs two plays back, drive position controlled
ord[, lab := fifelse(off_caller %in% MARQUEE | mi2_z > 3, lab_c(off_caller), "")]
p6 <- ggplot(ord, aes(mi1_excess, mi2_excess)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey80", linetype = "22") +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_point(aes(colour = mi2_z > 3), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70", max.overlaps = 20) +
  annotate("text", x = max(ord$mi1_excess), y = max(ord$mi1_excess) * 0.93, label = "dotted line: two back worth\nas much as one back",
           hjust = 1, vjust = 1, size = 2.9, colour = "grey45") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  labs(title = sprintf("Two plays back adds something for %d of %d callers, and it is a fraction of one play back",
                       sum(ord$mi2_z > 3), nrow(ord)),
       subtitle = paste0("Across: information the previous play carries about the next one, beyond down, distance and where in the drive the play sits.\n",
                         "Up: information the play TWO back adds on top of that, beyond the same situation AND the previous play. Both measured on the\n",
                         "same plays (3rd in the drive or later) so drive length cannot masquerade as memory. Orange: two back clears its shuffle null by 3 sd."),
       x = "one play back (bits beyond situation and drive position)", y = "two plays back (bits beyond that and the previous play)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nBits are minus what a shuffled order produces, so thin cells cannot fake memory. Built by R/66.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/markov_order.png", p6, w = 10, h = 7)

# ---------------------------------------------------------------- json for the page
mat_list <- function(x) {
  P <- tmat(x)
  list(n = nrow(x), matrix = unname(lapply(seq_len(nrow(P)), function(i) round(unname(P[i, ]), 3))),
       marginal = round(as.numeric(prop.table(table(factor(x$state, levels = STATES)))), 3))
}
page <- list(
  states = STATES,
  league = c(mat_list(tr), list(half_life = round(unname(lg_h["half_life"]), 2),
                                mi = round(lg_obs, 4), mi_null = round(mean(lg_nul), 4))),
  callers = setNames(lapply(MARQUEE, function(cl) {
    s <- out[off_caller == cl]
    c(mat_list(tr[off_caller == cl]),
      list(half_life = round(s$half_life, 2), half_life_null = round(s$half_life_null, 2),
           mi_excess = round(s$mi_excess, 4), mi_z = round(s$mi_z, 1),
           guess_sit = round(s$guess_sit, 1), guess_gain = round(s$guess_gain, 1),
           selection_per100 = round(s$selection * 100, 2), structure_z = round(s$structure_z, 1)))
  }), MARQUEE),
  pairs = pairs[, .(prev, nxt = state, n, edge = round(edge, 3), z = round(z, 1))],
  summary = list(n_callers = nrow(sig), n_sig = sum(sig$mi_z > 3), mean_guess_gain = round(mean(sig$guess_gain), 1),
                 max_guess_gain = round(max(sig$guess_gain), 1), max_guess_caller = sig[which.max(guess_gain)]$off_caller,
                 median_half_life = round(median(mem$half_life), 2), n_pairs_sig = sum(abs(pairs$z) > 3),
                 n_order2 = sum(ord$mi2_z > 3), n_order1 = sum(ord$mi1_z > 3),
                 order_ratio = round(mean(ord$mi2_excess) / mean(ord$mi1_excess), 2),
                 coen_selection_per100 = round(val[off_caller == "Liam Coen"]$selection * 100, 2),
                 johnson_selection_per100 = round(val[off_caller == "Ben Johnson"]$selection * 100, 2),
                 max_selection_per100 = round(max(abs(val$selection)) * 100, 2),
                 n_structure = sum(val$structure_z > 3))
)
write_json(page, "docs/data/markov.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nOut: 5 figures, data/derived/markov_callers.csv, markov_pairs.csv, docs/data/markov.json\n")
