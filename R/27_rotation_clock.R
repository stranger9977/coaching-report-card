# =============================================================================
# 27_rotation_clock.R -- does shell rotation tax the quarterback's CLOCK, even
# though R/18 found it does not move the offense's EPA?
#
# Michael, verbatim (same quote as R/18):
#   "There are three points in time a quarterback can read the defense:
#    1 Film study - before the game begins
#    2 Pre snap
#    3 Post snap. There's a lot more pressure post snap...and a lot less
#    time...by shifting at this moment, it adds another mental task in a
#    highly constrained environment. The later you can delay this, the
#    better...(kirk cousins video)."
#
# THE EPA NULL. R/18_shell_rotation.R already ran the obvious test: is
# offensive EPA lower on rotated snaps, net of situation? Answer: -0.011
# EPA/play controlled, 95% CI crosses zero ("not distinguishable from zero").
# Michael's theory was never really about points, though -- it is about TIME:
# rotating late adds a mental task in a "highly constrained" window, and two
# Sumer fields built for exactly that question (time_to_throw,
# time_to_pressure) have never been used in this project. This script tests
# the mechanism directly: does rotation measurably tax the QB's clock and
# decision quality, even where it washes out in points?
#
# THE CONFOUND THAT MAKES A RAW COMPARISON WORTHLESS. Deeper drops and play
# action take longer to develop BY DESIGN, and rotation is not randomly
# assigned to snaps -- it correlates with the rest of the defensive call and
# with down/distance. Checked directly on this data: play-action rate is
# 25.2% on rotated snaps vs 32.6% on static snaps, and blitz rate is 33.4% vs
# 29.8%. Both push a RAW time-to-throw comparison in different directions for
# reasons that have nothing to do with the QB processing a rotation. So every
# headline number below is reported twice: RAW (no controls) and CONTROLLED
# (situation + play design). The controlled model is expect_cont(), a direct
# continuous analogue of lib_sumer.R's sumer_expect(): same season-grouped,
# out-of-sample xgboost design, swapped to squared-error and trained on
# CONTROL_FEATS = the ten sumer_expect() situation features (down, distance,
# field position, quarter, score, two/four-minute, red zone, short yardage,
# goal line) PLUS depth_of_target, play_action and blitz -- the play's design,
# not just its situation. A play's residual is what is left after situation
# and design alone, so the rotated-vs-static gap in the residual is the part
# attributable to the shell actually moving. Binary outcomes (sack, scramble,
# etc.) reuse sumer_expect() from lib_sumer.R directly with the same feature
# set; it already returns .y/.expected in the exact shape battery_test()
# below expects.
#
# TIME_TO_THROW'S DEFINITION IS ITSELF A FINDING, not a footnote: it is
# recorded ONLY when the ball is actually thrown. It is 100% missing on sacks
# and 100% missing on scrambles (checked directly below), because there is no
# "time to throw" on a play where the QB never threw. That means a
# time-to-throw comparison is conditioned on survival to a throw. If
# Michael's mental tax shows up as MORE sacks and scrambles on rotated snaps
# rather than (or in addition to) slower throws, those plays are censored out
# of the time-to-throw sample entirely, which would understate the tax there.
# That is exactly why this script does not stop at time_to_throw: the sack,
# scramble, throwaway, left-pocket and hurry rates below are measured on the
# FULL dropback universe, unconditional on a throw happening, so the
# mechanism cannot hide by exiting through a sack instead of a slow throw.
# time_to_pressure has the mirror-image restriction: it is essentially only
# populated when pressure arrived (96% coverage when pressure == TRUE, 9%
# otherwise), so it is compared on pressured snaps only, and it answers a
# different question than time_to_throw: does the RUSH arrive faster/slower
# on rotated snaps, or does the QB simply hold the ball longer once pressure
# has arrived. If time_to_throw moves but time_to_pressure does not, the
# extra time is on the QB's side of the ledger, not the pass rush's.
#
# UNIVERSE. Same as R/18: is_dropback == TRUE, garbage_time == FALSE, has_shell
# (both look and played are a real OPEN/CLOSED call). rotate is look != played
# on that same restricted set.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()),
# R/factory/lib_sumer.R (load_sumer(), sumer_expect(), SUMER_STATE). No em
# dashes.
#
# Out: docs/figures/rotation_clock.png
#      data/derived/rotation_clock.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

d <- load_sumer()
d <- d[is_dropback == TRUE & garbage_time == FALSE]
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell &
      middle_of_field_coverage_look != middle_of_field_coverage_played)]
d <- d[has_shell == TRUE]
cat(sprintf("dropback snaps, non-garbage-time, charted shell, 2022-2025: %s (%.1f%% rotated)\n",
            format(nrow(d), big.mark = ","), 100*mean(d$rotate)))

# depth_of_target only belongs in the control set for outcomes that are
# defined independent of whether a throw happened. Checked directly on this
# data: depth_of_target is 100% missing exactly when is_sack is TRUE and 100%
# missing exactly when quarterback_scramble is TRUE (there is no target depth
# on a play where the ball never got thrown), 45% missing when
# quarterback_left_pocket is TRUE vs 3% when FALSE, and would drop 30% of the
# time_to_pressure sample (disproportionately the sacked/scrambled pressured
# snaps) -- exactly the extreme end of the tax this script is testing for.
# Forcing it into a model of is_sack or scramble does not just bias the
# result, it collapses the positive class to zero and crashes xgboost's
# logistic loss (base_score outside (0,1)); this was caught by that crash on
# the first run, not designed in ahead of time. So: TTT_FEATS (with
# depth_of_target) is used ONLY for time_to_throw, where the header's "deeper
# routes take longer by design" logic actually applies and the universe is
# already throw-only. Every other outcome uses CONTROL_FEATS (no
# depth_of_target).
TTT_FEATS     <- c(SUMER_STATE, "depth_of_target", "play_action", "blitz")
CONTROL_FEATS <- c(SUMER_STATE, "play_action", "blitz")

# Continuous analogue of lib_sumer.R's sumer_expect(): identical season-
# grouped, out-of-sample design, same .y/.expected output shape, squared-
# error objective on a continuous target instead of binary cross-entropy.
expect_cont <- function(x, target, feats = CONTROL_FEATS, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(get(target))], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x[[target]])
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "reg:squarederror", eta = 0.05, max_depth = 5,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.y = y, .expected = p)]
  x[!is.na(.expected)]
}

# Raw and controlled rotated-minus-static gap with CI, from an already-scored
# table carrying .y, .expected and rotate (whatever produced it: sumer_expect()
# for binary targets, expect_cont() for continuous ones). mult = 100 turns a
# 0/1 target into percentage points; mult = 1 leaves seconds as seconds.
battery_test <- function(ex, label, unit, mult = 1) {
  ex[, resid := .y - .expected]
  r1 <- ex[rotate == 1]; r0 <- ex[rotate == 0]
  tt_raw  <- t.test(r1$.y, r0$.y)
  tt_ctrl <- t.test(r1$resid, r0$resid)
  data.table(outcome = label, unit = unit, n_rotate = nrow(r1), n_static = nrow(r0),
             raw_diff  = mult*unname(tt_raw$estimate[1] - tt_raw$estimate[2]),
             raw_lo    = mult*tt_raw$conf.int[1],  raw_hi = mult*tt_raw$conf.int[2],
             ctrl_diff = mult*unname(tt_ctrl$estimate[1] - tt_ctrl$estimate[2]),
             ctrl_lo   = mult*tt_ctrl$conf.int[1], ctrl_hi = mult*tt_ctrl$conf.int[2],
             p = tt_ctrl$p.value)
}
verdict_label <- function(lo, hi, good_dir = "up") {
  if (good_dir == "up") {
    if (lo > 0) "taxed" else if (hi < 0) "eased" else "null"
  } else {
    if (hi < 0) "taxed" else if (lo > 0) "eased" else "null"
  }
}

# =============================================================================
# SECTION 1: what time_to_throw actually is
# =============================================================================
cat("\n=== 1. time_to_throw: units, coverage, mechanical drivers ===\n")
cat(sprintf("overall missing: %.1f%% of dropbacks\n", 100*mean(is.na(d$time_to_throw))))
d[, outcome_grp := fifelse(is_sack, "sack",
                    fifelse(quarterback_scramble, "scramble",
                    fifelse(throwaway, "throwaway", "pass (target)")))]
grp_tab <- d[, .(n = .N, missing_pct = 100*mean(is.na(time_to_throw)),
                 mean_ttt = mean(time_to_throw, na.rm = TRUE)), by = outcome_grp]
setorder(grp_tab, -n)
cat("outcome        |     n | % missing time_to_throw | mean time_to_throw (sec)\n")
for (i in seq_len(nrow(grp_tab))) with(grp_tab[i], cat(sprintf(
  "  %-13s | %5s | %6.1f%%                  | %s\n",
  outcome_grp, format(n, big.mark=","), missing_pct,
  if (is.nan(mean_ttt)) "n/a (never a throw)" else sprintf("%.2f", mean_ttt))))
cat("time_to_throw is recorded ONLY when a throw happens: 100% missing on sacks,\n")
cat("100% missing on scrambles, 0% missing on throwaways. It is a survival-\n")
cat("conditioned clock, not a play clock -- see header note.\n")

cat(sprintf("\ntime_to_pressure overall missing: %.1f%%. By pressure flag:\n", 100*mean(is.na(d$time_to_pressure))))
tp_cov <- d[, .(n = .N, has_value_pct = 100*mean(!is.na(time_to_pressure))), by = pressure]
for (i in seq_len(nrow(tp_cov))) with(tp_cov[i], cat(sprintf(
  "  pressure=%-5s | n=%6s | %.1f%% have a time_to_pressure value\n",
  pressure, format(n, big.mark=","), has_value_pct)))
cat("time_to_pressure is effectively defined only when pressure arrived; compared\n")
cat("below on pressure == TRUE snaps only.\n")

cat("\nthe confound rotation correlates with (not caused by mental tax, caused by the call bundle):\n")
conf_tab <- rbind(
  d[, .(level = "play_action", rate = 100*mean(rotate)), by = .(val = play_action)][!is.na(val)][order(val)],
  d[man_zone_coverage %in% c("MAN","ZONE"), .(level = "man_zone_coverage", rate = 100*mean(rotate)), by = .(val = man_zone_coverage)],
  d[, .(level = "blitz", rate = 100*mean(rotate)), by = .(val = blitz)], fill = TRUE)
for (i in seq_len(nrow(conf_tab))) with(conf_tab[i], cat(sprintf(
  "  %-18s = %-5s | rotation rate %.1f%%\n", level, as.character(val), rate)))

# =============================================================================
# SECTION 2: THE HEADLINE -- time to throw, raw and controlled
# =============================================================================
cat("\n=== 2. headline: does rotation add time to the QB's throw? ===\n")

tt <- d[!is.na(time_to_throw)]
cat(sprintf("throw-only universe: %s of %s dropbacks (%.1f%%)\n",
            format(nrow(tt), big.mark=","), format(nrow(d), big.mark=","), 100*nrow(tt)/nrow(d)))
ex_ttt <- expect_cont(tt, "time_to_throw", TTT_FEATS)
cat(sprintf("complete cases on controls: %s of %s (%.1f%% kept)\n",
            format(nrow(ex_ttt), big.mark=","), format(nrow(tt), big.mark=","), 100*nrow(ex_ttt)/nrow(tt)))
row_ttt <- battery_test(ex_ttt, "Time to throw", "seconds")
cat(sprintf("raw gap:       %+.3f sec [%.3f, %.3f]\n", row_ttt$raw_diff, row_ttt$raw_lo, row_ttt$raw_hi))
cat(sprintf("controlled gap: %+.3f sec [%.3f, %.3f], p = %.4f -> %s\n",
            row_ttt$ctrl_diff, row_ttt$ctrl_lo, row_ttt$ctrl_hi, row_ttt$p,
            verdict_label(row_ttt$ctrl_lo, row_ttt$ctrl_hi)))
cat(sprintf("controls %s the raw gap (situation + design explains %s of the raw difference).\n",
            if (abs(row_ttt$ctrl_diff) > abs(row_ttt$raw_diff)) "REVERSE/ENLARGE" else "shrink",
            if (row_ttt$raw_diff != 0) sprintf("%.0f%%", 100*(1 - row_ttt$ctrl_diff/row_ttt$raw_diff)) else "n/a"))

# =============================================================================
# SECTION 3: THE OUTCOMES BATTERY -- unconditional on a throw happening
# =============================================================================
cat("\n=== 3. outcomes battery: sack, throwaway, scramble, left-pocket, hurry, pressure timing ===\n")

row_sack   <- battery_test(sumer_expect(d, "is_sack", CONTROL_FEATS),               "Sack rate", "pp", 100)
row_scr    <- battery_test(sumer_expect(d, "quarterback_scramble", CONTROL_FEATS),  "Scramble rate", "pp", 100)
row_throw  <- battery_test(sumer_expect(d, "throwaway", CONTROL_FEATS),             "Throwaway rate", "pp", 100)
row_pocket <- battery_test(sumer_expect(d, "quarterback_left_pocket", CONTROL_FEATS),"Left-pocket rate", "pp", 100)
row_hurry  <- battery_test(sumer_expect(d, "hurry", CONTROL_FEATS),                 "Hurry rate", "pp", 100)

dp <- d[pressure == TRUE & !is.na(time_to_pressure)]
cat(sprintf("\ntime_to_pressure universe (pressure == TRUE only): %s snaps\n", format(nrow(dp), big.mark=",")))
ex_ttp <- expect_cont(dp, "time_to_pressure", CONTROL_FEATS)
row_ttp <- battery_test(ex_ttp, "Time to pressure (pressured snaps)", "seconds")

battery <- rbind(row_ttt, row_ttp, row_sack, row_scr, row_throw, row_pocket, row_hurry)
battery[, verdict := mapply(verdict_label, ctrl_lo, ctrl_hi)]

cat("\noutcome                              | unit    |   n rot |  n stat | raw gap [CI]                      | controlled gap [CI]               | verdict\n")
for (i in seq_len(nrow(battery))) with(battery[i], cat(sprintf(
  "  %-35s | %-7s | %7s | %7s | %+7.3f [%+7.3f, %+7.3f] | %+7.3f [%+7.3f, %+7.3f] | %s\n",
  outcome, unit, format(n_rotate, big.mark=","), format(n_static, big.mark=","),
  raw_diff, raw_lo, raw_hi, ctrl_diff, ctrl_lo, ctrl_hi, verdict)))

n_taxed <- sum(battery$verdict == "taxed")
cat(sprintf("\n%d of %d outcomes show a confident (95%% CI excludes zero) move in the tax direction on rotated snaps.\n",
            n_taxed, nrow(battery)))
ttt_verdict <- verdict_label(row_ttt$ctrl_lo, row_ttt$ctrl_hi)
if (ttt_verdict == "taxed" && n_taxed >= 2) {
  mech_verdict <- "MECHANISM CONFIRMED: rotation measurably taxes the QB's clock and decision quality, even though R/18 found it does not move EPA."
} else if (n_taxed == 0) {
  mech_verdict <- "MECHANISM NOT FOUND: every outcome here is null or points the other way. The mental-tax theory joins the EPA null -- rotation does not show up in this data at all, not just in points."
} else {
  mech_verdict <- "MECHANISM PARTIAL: some outcomes move in the taxed direction, others do not. Mixed evidence, not a clean confirmation."
}
cat(sprintf("\nVERDICT: %s\n", mech_verdict))

# =============================================================================
# SECTION 4: SPLITS -- man/zone, blitz, play action (the headline metric)
# =============================================================================
cat("\n=== 4. splits on the headline metric (time to throw, controlled) ===\n")

split_test <- function(sub, feats, label) {
  ex <- expect_cont(sub, "time_to_throw", feats)
  battery_test(ex, label, "seconds")
}

# All splits stay on the throw-only universe (time_to_throw's native
# universe), so TTT_FEATS (including depth_of_target) applies throughout;
# the split variable itself is dropped from its own feature set since it is
# constant within that subset.
splits <- list(
  "Play action"    = split_test(tt[play_action == 1], setdiff(TTT_FEATS, "play_action"), "PA"),
  "No play action" = split_test(tt[play_action == 0], setdiff(TTT_FEATS, "play_action"), "No PA"),
  "Blitz"          = split_test(tt[blitz == TRUE],  setdiff(TTT_FEATS, "blitz"), "Blitz"),
  "No blitz"       = split_test(tt[blitz == FALSE], setdiff(TTT_FEATS, "blitz"), "No blitz"),
  "Man coverage"   = split_test(tt[man_zone_coverage == "MAN"],  TTT_FEATS, "Man"),
  "Zone coverage"  = split_test(tt[man_zone_coverage == "ZONE"], TTT_FEATS, "Zone")
)
splits_tbl <- rbindlist(lapply(names(splits), function(nm) { s <- splits[[nm]]; s$split <- nm; s }))
splits_tbl[, verdict := mapply(verdict_label, ctrl_lo, ctrl_hi)]

cat("split           |   n rot |  n stat | controlled gap, time to throw [CI]      | verdict\n")
for (i in seq_len(nrow(splits_tbl))) with(splits_tbl[i], cat(sprintf(
  "  %-14s | %7s | %7s | %+.3f [%+.3f, %+.3f] sec              | %s\n",
  split, format(n_rotate, big.mark=","), format(n_static, big.mark=","), ctrl_diff, ctrl_lo, ctrl_hi, verdict)))

# Formal PA interaction test: does the rotation gap differ between PA and non-PA
# snaps? SE recovered from each split's own CI half-width (CI = diff +/- 1.96*se).
pa1 <- splits_tbl[split == "Play action"]; pa0 <- splits_tbl[split == "No play action"]
se1 <- (pa1$ctrl_hi - pa1$ctrl_lo) / (2*1.96)
se0 <- (pa0$ctrl_hi - pa0$ctrl_lo) / (2*1.96)
diff_diff <- pa1$ctrl_diff - pa0$ctrl_diff
se_dd <- sqrt(se1^2 + se0^2)
z <- diff_diff / se_dd
p_int <- 2*pnorm(-abs(z))
lo_dd <- diff_diff - 1.96*se_dd; hi_dd <- diff_diff + 1.96*se_dd
cat(sprintf("\nPA interaction: (PA gap) minus (no-PA gap) = %+.3f sec [%+.3f, %+.3f], z = %.2f, p = %.4f -> %s\n",
            diff_diff, lo_dd, hi_dd, z, p_int,
            if (p_int < 0.05) sprintf("the tax is confidently %s under play action", if (diff_diff > 0) "BIGGER" else "SMALLER")
            else "not a confident difference"))
cat("Michael's theory predicts the PA gap should be the largest: the QB's back is\n")
cat("turned exactly when the shell rotates, so he loses the chance to read it live.\n")

# =============================================================================
# CHART: the outcomes battery, controlled, one clean read
# =============================================================================
chart_tbl <- copy(battery)
chart_tbl[, panel := fifelse(unit == "seconds", "Seconds (rotated minus static) -- the headline", "Percentage points (rotated minus static)")]
chart_tbl[, panel := factor(panel, levels = c("Seconds (rotated minus static) -- the headline", "Percentage points (rotated minus static)"))]
chart_tbl[, outcome := factor(outcome, levels = rev(outcome[order(unit, ctrl_diff)]))]

any_taxed <- any(battery$verdict == "taxed")
chart_title <- if (any_taxed) {
  sprintf("Shell rotation taxes the QB's clock on %d of %d measures, even though it is an EPA null",
          n_taxed, nrow(battery))
} else {
  "Shell rotation does not measurably tax the QB's clock either: the mental-tax theory joins the EPA null"
}

p1 <- ggplot(chart_tbl, aes(ctrl_diff, outcome)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_errorbar(aes(xmin = ctrl_lo, xmax = ctrl_hi), width = 0.18, colour = "grey45", linewidth = 0.6) +
  geom_point(size = 3.2, colour = "#6b4c9a") +
  geom_text(aes(label = sprintf("%+.3f", ctrl_diff)), vjust = -1.2, size = 3.0,
            fontface = "bold", colour = "#4a3570") +
  facet_wrap(~panel, scales = "free", ncol = 1, strip.position = "top") +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  labs(title = chart_title,
       subtitle = "Rotated minus static shells, controlled for situation and play design (time metrics also net out target depth; see caption)",
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play charting 2022-2025",
         sprintf("%s dropback snaps with a charted shell, outside garbage time. Time to throw restricted to snaps that ended in a throw (sacks/scrambles have no value by\ndefinition); time to pressure restricted to snaps where pressure arrived.",
                 format(nrow(d), big.mark = ",")),
         sprintf("\nExpectation from a season-grouped, out-of-sample model of each outcome on situation + design (not just situation), so the residual nets out that rotated snaps\nsee less play action (%.1f%% vs %.1f%%) and more blitz (%.1f%% vs %.1f%%) for reasons unrelated to the mental tax. Target depth is a control ONLY for the two time\nmetrics: it is undefined by construction on sacks and scrambles, so forcing it into the rate models drops those exact rows. R/18_shell_rotation.R found the same\nrotated-vs-static comparison null on offensive EPA (-0.011 [CI crosses zero]); this script tests the clock instead of the scoreboard. Built by R/27_rotation_clock.R.",
                 100*mean(tt[rotate==1]$play_action, na.rm=TRUE), 100*mean(tt[rotate==0]$play_action, na.rm=TRUE),
                 100*mean(tt[rotate==1]$blitz), 100*mean(tt[rotate==0]$blitz)))) +
  theme_coach(grid = "none") +
  theme(strip.text = element_text(face = "bold", size = rel(0.85)),
        axis.text.y = element_text(size = rel(0.85)))
save_fig("docs/figures/rotation_clock.png", p1, w = 12, h = 8.5)

# =============================================================================
# write CSV: battery rows + split rows, one table
# =============================================================================
battery_out <- copy(battery)[, split := "All"]
splits_out  <- copy(splits_tbl)[, outcome := "Time to throw"][, unit := "seconds"]
out_tbl <- rbind(battery_out, splits_out, fill = TRUE)
write_csv(as.data.frame(out_tbl), "data/derived/rotation_clock.csv")

cat("\n=== SUMMARY ===\n")
cat(sprintf("Headline (time to throw, controlled): %+.3f sec [%+.3f, %+.3f], p = %.4f -> %s\n",
            row_ttt$ctrl_diff, row_ttt$ctrl_lo, row_ttt$ctrl_hi, row_ttt$p, verdict_label(row_ttt$ctrl_lo, row_ttt$ctrl_hi)))
cat(sprintf("PA interaction: %+.3f sec [%+.3f, %+.3f], p = %.4f\n", diff_diff, lo_dd, hi_dd, p_int))
cat(sprintf("%s\n", mech_verdict))
cat("\nOut: docs/figures/rotation_clock.png\n")
cat("     data/derived/rotation_clock.csv\n")
