# =============================================================================
# 17_ushape.R
#
# Michael, verbatim: "Predictability is U-shaped. You don't want to be at the
# extremes...kind of reminds me of music and movies... we are narrative
# pattern seeking creatures - if you are too unpredictable, then the
# narrative does not make sense, it's nonsensical. Your team needs a
# narrative and structure, but if you're too predictable, the other team
# will gain an edge on you." And: "So we need ways that McVay, Schnahan,
# McDonald, and Johnson are like this."
#
# This is a TEST of that theory, not an attempt to manufacture it. The board
# already has evidence against a simple U: situationally MORE guessable
# offenses have BETTER EPA/play (r = -0.37 on the entropy scale, R/07), and
# Ben Johnson -- the one name in Michael's list who is a pure play-caller --
# is the single least sequence-predictable caller in the league (R/13). Both
# are monotonic findings, not a U. Four tests, from most literal to most
# generous reading of Michael's words:
#
#   1. OFFENSE, SITUATIONAL -- career guessability-vs-league (guess_xs, the
#      guessability-scale twin of R/07's H_vs_lg) vs EPA/play. Linear,
#      quadratic, and GAM fits. Does the quadratic term earn its keep?
#   2. OFFENSE, SEQUENCE -- same battery on R/13's seq_guess_pp (does knowing
#      the previous call predict the next one) vs career EPA/play.
#   3. DEFENSE -- R/16's DC blitz guessability vs EPA allowed, same battery.
#      Note the flipped goodness direction: for defense LOW epa_allowed is
#      good, so the theory-consistent shape here is a U in epa_allowed
#      (worst at both extremes), not a hump.
#   4. THE RESCUE -- Michael's "narrative and structure" language may not be
#      guessability at all. Build STRUCTURE (how strong a caller's
#      situational identity is, lean_abs) x EXPLOITABILITY (guessability vs
#      league, guess_xs) and check whether McVay, Shanahan and Johnson
#      actually share a quadrant: a strong identity the opponent still can't
#      cash. Macdonald is placed via defensive analogues on within-population
#      percentile rank, because the raw units do not translate across sides
#      of the ball -- documented in that section.
#
# Sources:
#   - ~/stranger9977/nfl-analysis/scratch/pred_tab.rds -- caller-season
#     table, 2015-2025, built by scripts/predictability_build.R (read that
#     file first for exact column definitions: H_situ, H_vs_lg, guess,
#     guess_lg, guess_xs, seq_lift, lean, lean_abs, epa_play, n_plays).
#     SIGN TRAP: H_* columns are entropy (high = hard to guess). guess_xs is
#     already on the guessability scale (high = MORE guessable than league in
#     the same situations) -- this script stays on that scale throughout.
#   - data/derived/bj_seq_callers.csv -- career seq_lift/seq_guess_pp per
#     caller, built by R/13_bj_sequencing.R, already career-level (>=1500
#     plays, same threshold used here).
#   - data/derived/def_seq_callers.csv -- DC-level blitz guessability + EPA
#     allowed, built by R/16_def_sequencing.R (Sumer charting, 2022-2025,
#     >=700 career dropbacks).
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/ushape_test.png
#      docs/figures/ushape_structure.png
#      data/derived/ushape_callers.csv
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(ggrepel); library(scales)
  library(mgcv)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

MARQUEE_OFF <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")
MAC <- "Mike Macdonald"

# =============================================================================
# 0. SHAPE-TEST HELPER -- one function, reused for every x/y pair, so the
# battery (linear, quadratic, GAM) is applied identically each time.
# good_high: is a HIGH y value good performance? Determines which quadratic
# shape (hump vs U) would actually count as matching Michael's theory.
# =============================================================================
shape_test <- function(x, y, label, good_high = TRUE) {
  d <- data.table(x = x, y = y)
  lm1 <- lm(y ~ x, data = d)
  lm2 <- lm(y ~ x + I(x^2), data = d)
  av  <- anova(lm1, lm2)
  b2  <- coef(lm2)[["I(x^2)"]]
  b1  <- coef(lm2)[["x"]]
  p2  <- summary(lm2)$coefficients["I(x^2)", "Pr(>|t|)"]
  f_p <- av[["Pr(>F)"]][2]
  vertex <- -b1 / (2 * b2)
  vertex_interior <- is.finite(vertex) && vertex > min(d$x) && vertex < max(d$x)

  g  <- gam(y ~ s(x, k = 5), data = d, method = "REML")
  st <- summary(g)$s.table
  edf   <- st[1, "edf"]
  gam_p <- st[1, ncol(st)]
  qs <- quantile(d$x, c(.1, .5, .9), names = FALSE)
  gam_pred <- as.numeric(predict(g, newdata = data.frame(x = qs)))
  objective_shape <- if (gam_p > 0.10) {
    "flat / no relationship (GAM smooth term does not clear p = 0.10)"
  } else if (gam_pred[2] > gam_pred[1] && gam_pred[2] > gam_pred[3]) {
    "hump (higher in the middle)"
  } else if (gam_pred[2] < gam_pred[1] && gam_pred[2] < gam_pred[3]) {
    "U (lower in the middle)"
  } else if (gam_pred[3] > gam_pred[1]) {
    "monotonic increasing"
  } else {
    "monotonic decreasing"
  }

  quad_real <- p2 < 0.05 && vertex_interior
  theory_shape <- if (good_high) "hump (higher in the middle)" else "U (lower in the middle)"
  theory_match <- quad_real && (objective_shape == theory_shape)

  cat(sprintf("\n--- SHAPE TEST: %s (n = %d, good_high = %s) ---\n", label, nrow(d), good_high))
  cat(sprintf("linear:    r = %+.3f, AIC = %.1f\n", cor(d$x, d$y), AIC(lm1)))
  cat(sprintf("quadratic: b2(x^2) = %+.6f, p(coef) = %.4f, p(F-test add) = %.4f, AIC = %.1f (linear AIC %.1f, %s)\n",
              b2, p2, f_p, AIC(lm2), AIC(lm1),
              ifelse(AIC(lm2) < AIC(lm1) - 2, "quadratic wins on AIC", "no real AIC improvement over linear")))
  cat(sprintf("           vertex at x = %.3f, %s (x observed range %.3f to %.3f)\n",
              vertex,
              ifelse(vertex_interior, "INTERIOR -- a real turning point inside the data",
                     "at/outside the data boundary -- NOT a real interior turning point"),
              min(d$x), max(d$x)))
  cat(sprintf("GAM:       edf = %.2f (1.0 = linear, higher = more flexible), smooth-term p = %.4f, shape at p10/p50/p90 of x: %s\n",
              edf, gam_p, objective_shape))
  cat(sprintf("Michael's theory on this axis predicts a %s. VERDICT: %s\n",
              theory_shape, ifelse(theory_match, "SURVIVES -- shape matches", "DOES NOT SURVIVE -- actual shape differs")))

  list(lm1 = lm1, lm2 = lm2, gam = g, d = d, b2 = b2, p2 = p2, f_p = f_p,
       aic1 = AIC(lm1), aic2 = AIC(lm2), vertex = vertex, vertex_interior = vertex_interior,
       objective_shape = objective_shape, theory_match = theory_match, edf = edf, gam_p = gam_p)
}

# =============================================================================
# 1. LOAD + CAREER AGGREGATION, OFFENSE
# =============================================================================
tab <- as.data.table(readRDS(file.path(NFLA, "scratch/pred_tab.rds")))
cat(sprintf("pred_tab.rds: %d caller-seasons, %d unique callers, seasons %d-%d\n",
            nrow(tab), uniqueN(tab$off_play_caller), min(tab$season), max(tab$season)))

off_career <- tab[, .(
  n_plays  = sum(n_plays),
  guess_xs = weighted.mean(guess_xs, n_plays),
  H_vs_lg  = weighted.mean(H_vs_lg, n_plays),
  lean_abs = weighted.mean(lean_abs, n_plays),
  epa_play = weighted.mean(epa_play, n_plays)
), by = off_play_caller][n_plays >= 1500]
setorder(off_career, off_play_caller)
cat(sprintf("Offense careers >=1500 plays: %d\n", nrow(off_career)))

cat(sprintf("SIGN CHECK: r(guess_xs, H_vs_lg) = %.3f (must be negative -- guessability up means entropy down)\n",
            cor(off_career$guess_xs, off_career$H_vs_lg)))
stopifnot(cor(off_career$guess_xs, off_career$H_vs_lg) < 0)

# =============================================================================
# TEST 1: offense, situational guessability vs EPA
# =============================================================================
t1 <- shape_test(off_career$guess_xs, off_career$epa_play,
                  "TEST 1: offense situational guessability (guess_xs) vs EPA/play, career", good_high = TRUE)

cat("\nMcVay / Shanahan / Johnson on TEST 1 (situational guessability vs league, career EPA):\n")
off_career[, guess_xs_pctile := rank(guess_xs) / .N]
print(off_career[off_play_caller %in% MARQUEE_OFF,
      .(off_play_caller, n_plays, guess_xs = round(guess_xs, 3),
        guess_xs_pctile = round(guess_xs_pctile, 3), epa_play = round(epa_play, 3))])
if (t1$vertex_interior) {
  cat(sprintf("Vertex (peak/trough) sits at guess_xs = %.3f. Distance from vertex: McVay %.3f, Shanahan %.3f, Johnson %.3f\n",
              t1$vertex,
              off_career[off_play_caller == "Sean McVay", guess_xs] - t1$vertex,
              off_career[off_play_caller == "Kyle Shanahan", guess_xs] - t1$vertex,
              off_career[off_play_caller == "Ben Johnson", guess_xs] - t1$vertex))
} else {
  cat("No interior vertex to measure distance from -- the quadratic fit does not describe a real turning point.\n")
}

# =============================================================================
# TEST 2: offense, sequence guessability vs EPA
# =============================================================================
seqc <- fread("data/derived/bj_seq_callers.csv")
seq_career <- merge(seqc[!is.na(seq_guess_pp), .(off_play_caller, n_plays_seq, seq_guess_pp)],
                     off_career[, .(off_play_caller, epa_play, n_plays)],
                     by = "off_play_caller")
cat(sprintf("\nSequence-guessability x EPA join (both >=1500 career plays): %d careers\n", nrow(seq_career)))

t2 <- shape_test(seq_career$seq_guess_pp, seq_career$epa_play,
                  "TEST 2: offense sequence guessability (seq_guess_pp) vs EPA/play, career", good_high = TRUE)

cat("\nMcVay / Shanahan / Johnson on TEST 2 (sequence guessability, career EPA):\n")
seq_career[, seq_guess_pp_pctile := rank(seq_guess_pp) / .N]
print(seq_career[off_play_caller %in% MARQUEE_OFF,
      .(off_play_caller, seq_guess_pp = round(seq_guess_pp, 3),
        seq_guess_pp_pctile = round(seq_guess_pp_pctile, 3), epa_play = round(epa_play, 3))])

# =============================================================================
# TEST 3: defense, blitz guessability vs EPA allowed
# =============================================================================
defc <- fread("data/derived/def_seq_callers.csv")
def_ok <- defc[!is.na(guessability) & !is.na(epa_allowed) & n_career >= 700]
cat(sprintf("\nDefense careers with guessability + EPA allowed, >=700 charted dropbacks: %d\n", nrow(def_ok)))

t3 <- shape_test(def_ok$guessability, def_ok$epa_allowed,
                  "TEST 3: defense blitz guessability vs EPA allowed, career", good_high = FALSE)

cat("\nMacdonald on TEST 3 (blitz guessability, EPA allowed):\n")
def_ok[, guessability_pctile := rank(guessability) / .N]
print(def_ok[def_play_caller == MAC,
      .(def_play_caller, n_career, guessability = round(guessability, 3),
        guessability_pctile = round(guessability_pctile, 3), epa_allowed = round(epa_allowed, 3))])

# =============================================================================
# TEST 4: THE RESCUE -- structure x exploitability
#
# STRUCTURE = how strong a caller's situational identity is, career lean_abs
#   (mean |pass_oe| across all plays -- how far, on average, the caller's own
#   decisions run from the market-neutral situational expectation).
# EXPLOITABILITY = guessability vs league in the same situational cells,
#   guess_xs (the same axis as TEST 1).
#
# Both converted to WITHIN-POPULATION PERCENTILE RANK. This matters for the
# defensive analogue below: lean_abs and guess_xs are offense-only
# constructions (pass_oe, situational cell entropy vs a league baseline built
# the same way for offense). def_seq_callers.csv has no directly equivalent
# columns, so Macdonald is placed using the closest defensive analogues
# available in that table:
#   STRUCTURE_def  = -range_sd (R/16 Test 2): range_sd is the SD of a DC's
#     blitz-rate RESIDUAL across six situational cuts (early/late down, red
#     zone, two-minute, trailing, leading). LOW range_sd means the same
#     blitz identity shows up in every situation -- a persistent identity,
#     which is the closest defensive parallel to "strong situational
#     identity" available, even though it is constructed from variability
#     across cuts rather than average deviation magnitude the way lean_abs
#     is. Negated so higher = more structure, matching the offense axis.
#   EXPLOITABILITY_def = guessability (R/16 Test 3), 1 minus the entropy of
#     the blitz call at fixed situation -- the direct defensive twin of
#     offense guessability, but on an ABSOLUTE scale (no vs-league baseline
#     was computed for defense in R/16).
# Percentile rank is the only way to put these on the same page as the
# offense measures: it answers "where does he sit relative to his own peers"
# on each axis, not "does his raw number equal an offensive coach's raw
# number" (it cannot -- the units do not translate). Read Macdonald's point
# as illustrative of his profile among DCs, not a literal cross-position
# comparison to McVay/Shanahan/Johnson.
# =============================================================================

off_career[, structure_pctile := rank(lean_abs) / .N]
off_career[, exploit_pctile   := rank(guess_xs) / .N]
cat(sprintf("\nSTRUCTURE (lean_abs) range across %d offense careers: %.4f to %.4f (span %.4f) -- %s\n",
            nrow(off_career), min(off_career$lean_abs), max(off_career$lean_abs),
            max(off_career$lean_abs) - min(off_career$lean_abs),
            "NARROW: every NFL caller's situational deviation sits close to the same band, so percentile rank spreads the x-axis out even though the raw values barely differ. Read structure_pctile as a rank, not a strong effect size."))

def_ok2 <- defc[!is.na(guessability) & !is.na(range_sd) & n_career >= 700]
def_ok2[, structure_pctile := rank(-range_sd) / .N]
def_ok2[, exploit_pctile   := rank(guessability) / .N]
cat(sprintf("Defense analogue population (guessability + range_sd + n_career>=700): %d DCs\n", nrow(def_ok2)))

quad_labels <- function(structure_pctile, exploit_pctile) {
  fcase(
    structure_pctile >= 0.5 & exploit_pctile <  0.5, "clear identity, hard to exploit (Michael's ideal)",
    structure_pctile >= 0.5 & exploit_pctile >= 0.5, "clear identity, easy to exploit",
    structure_pctile <  0.5 & exploit_pctile <  0.5, "weak identity, hard to exploit",
    default = "weak identity, easy to exploit"
  )
}
off_career[, quadrant := quad_labels(structure_pctile, exploit_pctile)]
def_ok2[, quadrant := quad_labels(structure_pctile, exploit_pctile)]

cat("\nMcVay / Shanahan / Johnson on TEST 4 (structure vs exploitability, both offense percentile rank):\n")
print(off_career[off_play_caller %in% MARQUEE_OFF,
      .(off_play_caller, lean_abs = round(lean_abs, 4), structure_pctile = round(structure_pctile, 3),
        guess_xs = round(guess_xs, 3), exploit_pctile = round(exploit_pctile, 3), quadrant)])

mac4 <- def_ok2[def_play_caller == MAC]
cat("\nMacdonald on TEST 4 (defensive analogues, percentile rank within DC population):\n")
print(mac4[, .(def_play_caller, range_sd = round(range_sd, 4), structure_pctile = round(structure_pctile, 3),
               guessability = round(guessability, 3), exploit_pctile = round(exploit_pctile, 3), quadrant)])

four_quads_tab <- rbind(
  off_career[off_play_caller %in% MARQUEE_OFF, .(name = off_play_caller, quadrant)],
  mac4[, .(name = def_play_caller, quadrant)]
)
four_quads_tab <- four_quads_tab[match(c(MARQUEE_OFF, MAC), name)]  # fixed display order, names correctly attached
cat("\nDo the four share a quadrant?\n")
print(four_quads_tab)
cluster_verdict <- if (uniqueN(four_quads_tab$quadrant) == 1) {
  sprintf("YES -- all four land in '%s'", four_quads_tab$quadrant[1])
} else {
  sprintf("NO -- %d distinct quadrants among the four: %s",
          uniqueN(four_quads_tab$quadrant), paste(unique(four_quads_tab$quadrant), collapse = " | "))
}
cat("Cluster verdict:", cluster_verdict, "\n")

# Robustness check (print-only, not a separate chart): does swapping in
# sequence-guessability (Test 2's axis, where Johnson is the league extreme)
# for exploitability change the picture for the three offensive names?
seq_pctile_join <- merge(off_career[, .(off_play_caller)],
                          seq_career[, .(off_play_caller, seq_guess_pp)], by = "off_play_caller", all.x = TRUE)
seq_pctile_join[!is.na(seq_guess_pp), seq_exploit_pctile := rank(seq_guess_pp) / .N]
cat("\nROBUSTNESS: exploitability measured by SEQUENCE guessability instead of situational guessability:\n")
print(merge(off_career[off_play_caller %in% MARQUEE_OFF, .(off_play_caller, structure_pctile)],
            seq_pctile_join[off_play_caller %in% MARQUEE_OFF, .(off_play_caller, seq_exploit_pctile)],
            by = "off_play_caller"))
cat("(Johnson's sequence-guessability percentile is far lower than his situational one -- the 'exploitability' story depends heavily on which guessability axis is used; see report.)\n")

# =============================================================================
# CHART A: ushape_test.png -- Test 1 scatter, linear + GAM fit, three names
# =============================================================================
newx <- seq(min(off_career$guess_xs), max(off_career$guess_xs), length.out = 200)
gam_line <- data.table(guess_xs = newx, epa_play = as.numeric(predict(t1$gam, newdata = data.frame(x = newx))))
lin_line <- data.table(guess_xs = newx, epa_play = as.numeric(predict(t1$lm1, newdata = data.frame(x = newx))))

verdict_title <- if (t1$theory_match) {
  "The U survives on offense: EPA peaks at moderate situational guessability"
} else if (t1$objective_shape == "monotonic increasing") {
  "No U here: the more situationally guessable an offense is, the better it performs, straight through"
} else if (t1$objective_shape == "monotonic decreasing") {
  "No U here: more situationally guessable offenses perform worse, straight through"
} else {
  sprintf("No U here: the actual shape is a %s, not a hump", t1$objective_shape)
}

lab1 <- off_career[off_play_caller %in% MARQUEE_OFF]

p1 <- ggplot(off_career, aes(guess_xs, epa_play)) +
  geom_point(colour = "#9db6c9", alpha = 0.65, size = 2.2) +
  geom_line(data = lin_line, aes(guess_xs, epa_play), colour = "grey55", linetype = "dashed", linewidth = 0.6) +
  geom_line(data = gam_line, aes(guess_xs, epa_play), colour = "#045A8D", linewidth = 1) +
  geom_point(data = lab1, colour = "#D55E00", size = 4) +
  geom_text_repel(data = lab1, aes(label = off_play_caller), size = 3.4, fontface = "bold",
                   colour = "#8a3d00", seed = 7, box.padding = 0.6, min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = min(off_career$guess_xs), y = max(off_career$epa_play), hjust = 0, vjust = 1,
           size = 3, colour = "grey40", fontface = "italic",
           label = "grey dashed = linear fit\nblue solid = flexible GAM fit") +
  labs(
    title = verdict_title,
    subtitle = sprintf(paste0("Career situational guessability vs league (guess_xs) vs career EPA/play, %d play-callers, 1,500+ career plays, 2015-2025.\n",
                               "Quadratic term: b = %+.5f, p = %.3f (%s). GAM edf = %.2f, smooth p = %.4f."),
                        nrow(off_career), t1$b2, t1$p2,
                        ifelse(t1$vertex_interior, "interior vertex", "no interior vertex"), t1$edf, t1$gam_p),
    x = "extra correct guesses per 100 plays vs league, same situations (guess_xs)",
    y = "career EPA/play",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tab.rds, built by scripts/predictability_build.R",
      sprintf("%d play-callers, min 1,500 career called plays, 2015-2025.", nrow(off_career)),
      sprintf(paste0("\nMichael's theory (verbatim): predictability is U-shaped, worst at the extremes, best in the middle. Tested here as: does EPA/play\n",
                      "peak at moderate guessability? Linear AIC %.1f vs quadratic AIC %.1f (%s). Shanahan and McVay sit at the high-guessability end\n",
                      "(guess_xs = %.2f, %.2f, top of the league); Johnson sits near the league median (guess_xs = %.2f). The three do not share a position on this axis."),
              t1$aic1, t1$aic2, ifelse(t1$aic2 < t1$aic1 - 2, "quadratic wins" , "no real AIC gain"),
              off_career[off_play_caller == "Kyle Shanahan", guess_xs],
              off_career[off_play_caller == "Sean McVay", guess_xs],
              off_career[off_play_caller == "Ben Johnson", guess_xs])
    )
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/ushape_test.png", p1, w = 12.5, h = 7.4)

# =============================================================================
# CHART B: ushape_structure.png -- structure x exploitability quadrant
# =============================================================================
plot_off <- off_career[, .(name = off_play_caller, structure_pctile, exploit_pctile,
                            side = "Offense", marquee = off_play_caller %in% MARQUEE_OFF)]
plot_def <- def_ok2[, .(name = def_play_caller, structure_pctile, exploit_pctile,
                         side = "Defense (analogue)", marquee = def_play_caller == MAC)]
plot4 <- rbind(plot_off, plot_def)
lab4 <- plot4[marquee == TRUE]

verdict4_title <- if (uniqueN(four_quads_tab$quadrant) == 1) {
  sprintf("McVay, Shanahan, Johnson and Macdonald share one quadrant: %s", four_quads_tab$quadrant[1])
} else {
  "McVay, Shanahan, Johnson and Macdonald do not share a quadrant"
}

p2 <- ggplot(plot4, aes(structure_pctile, exploit_pctile)) +
  annotate("rect", xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5, fill = "#2B8CBE", alpha = 0.08) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(shape = side), colour = "#9db6c9", alpha = 0.6, size = 2.3) +
  geom_point(data = lab4, aes(shape = side), colour = "#D55E00", size = 4.2, stroke = 1.1) +
  geom_text_repel(data = lab4, aes(label = name), size = 3.4, fontface = "bold",
                   colour = "#8a3d00", seed = 3, box.padding = 0.7, min.segment.length = 0, max.overlaps = 30) +
  annotate("text", x = 0.75, y = 0.04, hjust = 0.5, vjust = 0, size = 3, fontface = "italic",
           colour = "#2B6E8C", label = "Michael's ideal quadrant:\nclear identity, hard to exploit") +
  scale_shape_manual(values = c("Offense" = 16, "Defense (analogue)" = 17), name = NULL) +
  scale_x_continuous(labels = percent, limits = c(0, 1)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(
    title = verdict4_title,
    subtitle = "STRUCTURE = career situational-identity strength (lean_abs), EXPLOITABILITY = guessability vs league (guess_xs),\nboth shown as within-population percentile rank. Macdonald uses defensive analogues (see script header) -- illustrative, not a literal cross-position match.",
    x = "STRUCTURE percentile (identity strength, within own side of the ball)",
    y = "EXPLOITABILITY percentile (guessability vs league, within own side of the ball)",
    caption = fig_caption(
      "nfl-analysis scratch/pred_tab.rds + data/derived/def_seq_callers.csv",
      sprintf("%d offense careers (1,500+ plays), %d DC careers (700+ dropbacks).", nrow(off_career), nrow(def_ok2)),
      sprintf(paste0("\nSTRUCTURE range is narrow in raw units (lean_abs %.3f to %.3f across the whole offense population) -- percentile rank spreads the axis out even though\n",
                      "the underlying differences are small. Macdonald's structure axis is -range_sd (flatness of his blitz rate across six situational cuts) and his exploitability\n",
                      "axis is raw blitz guessability (no vs-league baseline exists for defense in this pipeline); neither construction matches the offense side exactly."),
              min(off_career$lean_abs), max(off_career$lean_abs))
    )
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "bottom")
save_fig("docs/figures/ushape_structure.png", p2, w = 12.5, h = 8.5)

# =============================================================================
# WRITE COMBINED CSV
# =============================================================================
off_career[, marquee := off_play_caller %in% MARQUEE_OFF]
off_out <- merge(
  off_career[, .(name = off_play_caller, side = "offense", n = n_plays,
                  guess_xs, H_vs_lg, epa = epa_play, epa_type = "epa_play",
                  lean_abs, structure_pctile, exploit_pctile, quadrant, marquee)],
  seq_career[, .(name = off_play_caller, seq_guess_pp)], by = "name", all.x = TRUE
)
def_ok2[, marquee := def_play_caller == MAC]
def_out <- def_ok2[, .(name = def_play_caller, side = "defense", n = n_career,
                        guessability, epa = epa_allowed, epa_type = "epa_allowed",
                        range_sd, structure_pctile, exploit_pctile, quadrant, marquee)]
out <- rbind(off_out, def_out, fill = TRUE)
fwrite(out, "data/derived/ushape_callers.csv")
cat(sprintf("\nwrote data/derived/ushape_callers.csv (%d rows)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n================= SUMMARY =================\n")
cat(sprintf("TEST 1 (offense situational): %s\n", t1$objective_shape))
cat(sprintf("TEST 2 (offense sequence):    %s\n", t2$objective_shape))
cat(sprintf("TEST 3 (defense blitz):       %s (good_high = FALSE, so theory wants a U)\n", t3$objective_shape))
cat(sprintf("TEST 4 (structure x exploit): %s\n", cluster_verdict))
cat("=============================================\n")
cat("DONE\n")
