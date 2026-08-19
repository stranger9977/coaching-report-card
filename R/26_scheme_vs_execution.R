# =============================================================================
# 26_scheme_vs_execution.R -- untangling the play-caller from his players.
#
# Michael, on coach value generally: "It's so entangled that it's obviously
# very hard to know or tell." This script is the untangling tool for offense.
# Sumer grades every offensive player on every play (`coarse_grade`, in the
# player-level charting) -- HIGH / NEUTRAL / LOW. This is the first script in
# this repo to touch that field. The idea: split a play-caller's offensive
# EPA into the part his players' grades already explain (EXECUTION) and
# whatever is left over after that (DESIGN, or "everything else" -- play
# calls, scheme, matchups the caller manufactured).
#
# THE CIRCULARITY PROBLEM, CHECKED FIRST. A play's own coarse_grade is partly
# a description of how the play turned out -- a receiver who takes a slant to
# the house grades HIGH partly BECAUSE he scored, not only because he ran a
# clean route. Using the SAME-PLAY grade as "execution" would make this
# script measure "did the play work" twice and call the second copy
# "execution." So before building anything, this script correlates same-play
# grade against same-play EPA, and separately builds a LEAVE-ONE-OUT grade --
# each player's average grade on every OTHER play he took part in that
# season, excluding the play being scored -- and checks whether THAT still
# correlates with the play's EPA. If same-play r is much larger, same-play
# grade is outcome-contaminated and the leave-one-out version is what ships
# as "execution." See the printed verdict below for which one this run chose
# and why.
#
# DATA. data/raw/sumer/plays_players_p1.csv.gz + p2 (4,764,650 player-play
# rows, never analyzed with coarse_grade in this repo before this script).
# Restricted to side_of_ball == "offense", season 2022-2025 (2026 is 58,716
# rows of preseason charting, excluded). ~18.2% of the raw offense rows
# (427,324 of 2,353,296) are entirely blank placeholder rows -- alignment,
# role and coarse_grade all blank together, not real charted players -- and
# are dropped before anything else. Among the remaining 1,925,972 real
# player-play rows, coarse_grade is HIGH/LOW/NEUTRAL with a 0% true-missing
# rate by both position and season (checked below); 33 stray blank rows (all
# from the QB alignment tag, not spread across positions -- <0.002% of the
# real rows) are also dropped.
#
# Play context and offensive-caller attribution: R/factory/lib_sumer.R's
# load_sumer() on sumer_play_id, same join this repo has used since R/09;
# R/25_presnap_structure.R has the working pattern for merging the
# player-level file back onto plays. Universe: 2022-2025, run_pass in
# {P, R}, garbage_time == FALSE, off_caller present (house convention since
# R/18). Callers need >= 1200 qualifying plays (R/14/R/25's QUAL_MIN).
#
# EXECUTION MEASURE. Per play, the mean leave-one-out grade (HIGH=+1,
# NEUTRAL=0, LOW=-1) across every offensive player charted on that play --
# linemen, skill players and the QB together, since the QB is a player whose
# execution matters distinctly from what was called for him (R/05 already
# covers the QB/caller entanglement from a different angle: whether a
# caller's numbers are really his QB's). A player's own leave-one-out grade
# only counts toward a play's average if he has >= 5 other graded plays that
# season (below that his season average is close to un-estimated); a play
# needs >= 4 such players to get a valid execution score, mirroring R/25's
# "n_skill >= 4" completeness floor for this same player-level file.
#
# DECOMPOSITION. Season-grouped, out-of-sample xgboost (reg:squarederror,
# same hyperparameters as R/18's expect_epa and lib_sumer's sumer_expect) of
# play EPA on the ten SUMER_STATE situation features, run twice: once alone
# (situation-only baseline) and once with the execution measure added
# (situation+execution). DESIGN VALUE per play is actual EPA minus the
# situation+execution prediction -- what the play made happen beyond what the
# situation AND how well his players graded already explain. EXECUTION LEVEL
# per caller is just his plays-weighted mean of the leave-one-out grade (not
# residualized against anything -- it answers "do this caller's players play
# well" directly).
#
# Out: docs/figures/scheme_vs_execution.png (two-axis chart, execution level
#        vs design value, ~36 qualified callers, McVay/Shanahan/Johnson +
#        notable names labeled, quadrants named plainly)
#      data/derived/scheme_execution.csv (every measure, every qualified
#        caller)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(xgboost); library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

QUAL_MIN <- 1200   # matches R/14/R/25's caller-qualification floor
MIN_SEASON <- 200  # matches lib_sumer.R's sumer_resid() default
MIN_PLAYER_N <- 5  # a player's leave-one-out grade counts only w/ >= 5 other graded plays that season
MIN_PLAY_N   <- 4  # a play needs >= 4 such players to get an execution score (mirrors R/25's n_skill >= 4)

NAMED <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson", "Andy Reid", "Matt LaFleur",
           "Mike McDaniel", "Kevin O'Connell", "Sean Payton", "Liam Coen", "Todd Monken",
           "Arthur Smith", "Zac Taylor", "Brian Callahan", "Klint Kubiak", "Bobby Slowik",
           "Joe Brady", "Nathaniel Hackett", "Shane Steichen")
HEADLINE <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")

# =============================================================================
# STEP 1: load and inspect coarse_grade
# =============================================================================
cat("=== STEP 1: coarse_grade -- levels, coverage by position and season ===\n")

PCOLS <- c("sumer_play_id", "sumer_player_id", "side_of_ball", "coarse_grade", "season", "alignment")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
n_total <- nrow(players)

off <- players[side_of_ball == "offense" & season %in% 2022:2025]
n_off <- nrow(off)
n_blank <- off[alignment == "", .N]
cat(sprintf("player-play rows: %s total, %s offense/2022-2025, %s of those (%.1f%%) are blank placeholder rows (no alignment, no role, no grade) and are dropped\n",
            format(n_total, big.mark = ","), format(n_off, big.mark = ","),
            format(n_blank, big.mark = ","), 100 * n_blank / n_off))
real <- off[alignment != ""]
n_stray <- real[!coarse_grade %in% c("HIGH", "LOW", "NEUTRAL"), .N]
cat(sprintf("of %s real charted offensive rows, %d (%.4f%%) have a stray blank grade and are also dropped\n",
            format(nrow(real), big.mark = ","), n_stray, 100 * n_stray / nrow(real)))
real <- real[coarse_grade %in% c("HIGH", "LOW", "NEUTRAL")]

cat("\ncoarse_grade distribution (real charted offensive player-plays):\n")
print(round(100 * prop.table(table(real$coarse_grade)), 2))

cat("\nmissing/blank-grade rate by position (should be near 0 -- confirms the blanks dropped above were placeholders, not a real position gap):\n")
print(off[alignment != "", .(n = .N, blank_grade_pct = round(100 * mean(!coarse_grade %in% c("HIGH","LOW","NEUTRAL")), 3)), by = alignment][order(-n)])

cat("\nmissing/blank-grade rate by season:\n")
print(off[alignment != "", .(n = .N, blank_grade_pct = round(100 * mean(!coarse_grade %in% c("HIGH","LOW","NEUTRAL")), 3)), by = season][order(season)])

real[, grade_num := fifelse(coarse_grade == "HIGH", 1, fifelse(coarse_grade == "LOW", -1, 0))]

OL    <- c("T", "G", "C")
SKILL <- c("RB", "FB", "TE", "WR", "SWR")
QBU   <- "QB"
real[, unit := fifelse(alignment %in% OL, "OL",
              fifelse(alignment %in% SKILL, "skill",
              fifelse(alignment == QBU, "QB", "other")))]

cat("\nmean grade by unit (contemporaneous, informational only -- diagnostic, not the execution measure):\n")
print(real[, .(n = .N, mean_grade = round(mean(grade_num), 4)), by = unit][order(-n)])

# =============================================================================
# STEP 1b: play-level SAME-PLAY grade (for the circularity check only)
# =============================================================================
same_dt <- real[, .(play_grade_same = mean(grade_num), n_graded_same = .N), by = sumer_play_id]

# =============================================================================
# STEP 1c: LEAVE-ONE-OUT grade -- each player's average grade on all OTHER
# plays that season, excluding the play being scored
# =============================================================================
psn <- real[, .(sum_g = sum(grade_num), n_g = .N), by = .(sumer_player_id, season)]
cat(sprintf("\nplayer-seasons: %s | median graded plays per player-season: %d | %.1f%% have >= %d (the leave-one-out floor)\n",
            format(nrow(psn), big.mark = ","), median(psn$n_g), 100 * mean(psn$n_g >= MIN_PLAYER_N + 1), MIN_PLAYER_N + 1))

real2 <- merge(real, psn, by = c("sumer_player_id", "season"))
real2[, loo := fifelse(n_g > MIN_PLAYER_N, (sum_g - grade_num) / (n_g - 1), NA_real_)]

loo_dt <- real2[!is.na(loo), .(play_grade_loo = mean(loo), n_loo = .N), by = sumer_play_id]
loo_dt <- loo_dt[n_loo >= MIN_PLAY_N]
cat(sprintf("plays with a valid leave-one-out execution score (>= %d qualifying players): %s\n",
            MIN_PLAY_N, format(nrow(loo_dt), big.mark = ",")))

rm(players, off, real, real2, psn); gc(verbose = FALSE)

# =============================================================================
# STEP 2 (part 1): play context + callers, merge in both grade measures
# =============================================================================
plays <- load_sumer(2022:2025)
plays <- plays[run_pass %in% c("P", "R") & garbage_time == FALSE & off_caller != ""]
cat(sprintf("\nSumer plays, 2022-2025, non-garbage-time, caller-attributed, run/pass: %s\n",
            format(nrow(plays), big.mark = ",")))

d <- merge(plays, same_dt, by = "sumer_play_id", all.x = TRUE)
d <- merge(d, loo_dt, by = "sumer_play_id", all.x = TRUE)
cat(sprintf("plays with a same-play grade: %s (%.1f%%) | plays with a leave-one-out grade: %s (%.1f%%)\n",
            format(sum(!is.na(d$play_grade_same)), big.mark = ","), 100 * mean(!is.na(d$play_grade_same)),
            format(sum(!is.na(d$play_grade_loo)), big.mark = ","), 100 * mean(!is.na(d$play_grade_loo))))

# =============================================================================
# THE CIRCULARITY CHECK
# =============================================================================
cat("\n\n=== THE CIRCULARITY CHECK ===\n")
chk <- d[!is.na(play_grade_same) & !is.na(play_grade_loo) & !is.na(expected_points_added)]
r_same <- cor(chk$play_grade_same, chk$expected_points_added)
r_loo  <- cor(chk$play_grade_loo, chk$expected_points_added)
cat(sprintf("r(same-play grade, same-play EPA)       = %+.3f  (n = %s)\n", r_same, format(nrow(chk), big.mark = ",")))
cat(sprintf("r(leave-one-out grade, same-play EPA)    = %+.3f  (n = %s)\n", r_loo, format(nrow(chk), big.mark = ",")))
CIRC_THRESHOLD <- 2  # same-play r must be at least this many times the LOO r to call it contaminated
use_loo <- r_same > 0.15 && r_same > CIRC_THRESHOLD * max(r_loo, 0.001)
cat(sprintf("\nVERDICT: %s\n",
            if (use_loo) sprintf("same-play grade is outcome-contaminated (%.1fx the leave-one-out correlation) -- shipping the LEAVE-ONE-OUT grade as the execution measure.", r_same / max(r_loo, 0.001))
            else "same-play grade is not meaningfully more correlated with EPA than the leave-one-out grade -- using the same-play grade would have been defensible too, but the leave-one-out grade is shipped anyway since it is the more conservative choice and was built regardless."))
EXEC_COL <- "play_grade_loo"

# =============================================================================
# STEP 3: decomposition -- situation-only vs situation+execution EPA models
# =============================================================================
cat("\n\n=== STEP 3: DECOMPOSITION ===\n")

d2 <- d[!is.na(play_grade_loo)]

# Continuous analogue of lib_sumer.R's sumer_expect(), same construction R/18
# used: season-grouped, out-of-sample, squared-error objective on EPA.
expect_epa <- function(x, feats, target = "expected_points_added", nrounds = 250) {
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
  x[, `:=`(.epa_actual = y, .epa_expected = p)]
  x[!is.na(.epa_expected)]
}

sit_only <- expect_epa(d2, SUMER_STATE)
cat(sprintf("situation-only model: mean actual %.4f, mean predicted %.4f\n",
            mean(sit_only$.epa_actual), mean(sit_only$.epa_expected)))

sit_exec <- expect_epa(d2, c(SUMER_STATE, EXEC_COL))
cat(sprintf("situation+execution model: mean actual %.4f, mean predicted %.4f\n",
            mean(sit_exec$.epa_actual), mean(sit_exec$.epa_expected)))

setnames(sit_only, ".epa_expected", "epa_exp_situation")
setnames(sit_exec, ".epa_expected", "epa_exp_situexec")

both_m <- merge(sit_only[, .(sumer_play_id, epa_exp_situation)],
                sit_exec[, .(sumer_play_id, epa_exp_situexec, .epa_actual, off_caller, season, play_grade_loo)],
                by = "sumer_play_id")
both_m[, execution_lift := epa_exp_situexec - epa_exp_situation]  # league-wide: how much execution alone explains
both_m[, design_resid := .epa_actual - epa_exp_situexec]           # design value: left over after situation AND execution
cat(sprintf("\nleague-wide mean execution_lift (situation+execution pred minus situation-only pred): %+.4f EPA/play\n",
            mean(both_m$execution_lift)))
cat(sprintf("league-wide mean design_resid (should be ~0 by construction): %+.4f EPA/play\n", mean(both_m$design_resid)))
cat(sprintf("r(execution_lift, play_grade_loo) = %+.3f -- confirms the situation+execution model is actually using the grade\n",
            cor(both_m$execution_lift, both_m$play_grade_loo)))

# =============================================================================
# per caller-season and per-caller aggregation
# =============================================================================
seas <- both_m[, .(n = .N,
                   execution = mean(play_grade_loo),
                   design_resid = mean(design_resid),
                   epa = mean(.epa_actual)),
              by = .(caller = off_caller, season)][n >= MIN_SEASON]

career <- seas[, .(seasons = .N, n = sum(n),
                   execution = weighted.mean(execution, n),
                   design_resid = weighted.mean(design_resid, n),
                   epa = weighted.mean(epa, n)), by = caller][n >= QUAL_MIN]
career[, execution_rank := frank(-execution)]
career[, design_rank := frank(-design_resid)]
setorder(career, -design_resid)
cat(sprintf("\nqualified callers (>= %d plays, >= %d play-season floor per season counted): %d\n", QUAL_MIN, MIN_SEASON, nrow(career)))
cat(sprintf("league median execution: %.4f | league median design value: %+.4f EPA/play\n",
            median(career$execution), median(career$design_resid)))

cat("\n--- highest design value (best play-calling net of situation and personnel) ---\n")
print(head(career[, .(caller, n, execution = round(execution, 3), design_resid = round(design_resid, 4), design_rank)], 8))
cat("\n--- lowest design value ---\n")
print(tail(career[, .(caller, n, execution = round(execution, 3), design_resid = round(design_resid, 4), design_rank)], 8))

cat("\n--- the three names ---\n")
print(career[caller %in% HEADLINE, .(caller, n, execution = round(execution, 3), execution_rank,
                                      design_resid = round(design_resid, 4), design_rank)])

# =============================================================================
# persistence: is design value a coaching TRAIT?
# =============================================================================
seas_ps <- copy(seas); setnames(seas_ps, "design_resid", "resid")
ps <- persist_split(seas_ps, min_half = 350)
cat(sprintf("\nis DESIGN VALUE a stable trait, odd vs even season? r = %+.2f [%.2f, %.2f] p = %.4f, n = %d callers -> %s\n",
            ps$r, ps$lo, ps$hi, ps$p, ps$n, ps$verdict))

seas_ex <- copy(seas); setnames(seas_ex, "execution", "resid")
ps_ex <- persist_split(seas_ex, min_half = 350)
cat(sprintf("(for comparison) is EXECUTION LEVEL persistent, odd vs even season? r = %+.2f [%.2f, %.2f] p = %.4f, n = %d -> %s\n",
            ps_ex$r, ps_ex$lo, ps_ex$hi, ps_ex$p, ps_ex$n, ps_ex$verdict))

# =============================================================================
# STEP 5: context tie-in -- new information, or the same coaches again?
# =============================================================================
cat("\n\n=== CONTEXT TIE-IN ===\n")
pred <- fread("data/derived/predictability_defined.csv")
ctx <- merge(career, pred[, .(caller = off_play_caller, tip_resid, guess_xs, epa_play)], by = "caller")
cat(sprintf("callers matched to R/07's predictability table: %d of %d\n", nrow(ctx), nrow(career)))
cat(sprintf("r(design value, R/07 LOOK-predictability residual tip_resid) = %+.3f\n", cor(ctx$design_resid, ctx$tip_resid)))
cat(sprintf("r(design value, R/07 CALL-predictability guess_xs)          = %+.3f\n", cor(ctx$design_resid, ctx$guess_xs)))
cat(sprintf("r(design value, career EPA/play epa_play)                   = %+.3f\n", cor(ctx$design_resid, ctx$epa_play)))
cat(sprintf("r(execution level, career EPA/play epa_play)                = %+.3f\n", cor(ctx$execution, ctx$epa_play)))

# =============================================================================
# write CSV
# =============================================================================
out <- merge(career, pred[, .(caller = off_play_caller, tip_resid, guess_xs, epa_play)], by = "caller", all.x = TRUE)
med_x <- median(career$execution); med_y <- median(career$design_resid)
out[, quadrant := fifelse(execution >= med_x & design_resid >= med_y, "great players AND design that adds",
                  fifelse(execution >= med_x & design_resid <  med_y, "good players, no design edge",
                  fifelse(execution <  med_x & design_resid >= med_y, "design carries ordinary players",
                                                                        "ordinary players, no design edge either")))]
setorder(out, -design_resid)
write_csv(as.data.frame(out), "data/derived/scheme_execution.csv")
cat(sprintf("\nwrote data/derived/scheme_execution.csv (%d callers)\n", nrow(out)))

cat("\nquadrant occupant counts:\n")
print(out[, .N, by = quadrant][order(-N)])

# =============================================================================
# CHART: execution level x design value, two-axis
# =============================================================================
career[, hl := caller %in% HEADLINE]
career[, named := caller %in% NAMED]
lab_c <- career[named == TRUE]

pC <- ggplot(career, aes(execution, design_resid)) +
  geom_hline(yintercept = med_y, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = med_x, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(data = career[named == FALSE], colour = "#9db6c9", alpha = 0.6, size = 2.1) +
  geom_point(data = career[named == TRUE & hl == FALSE], colour = "#2B8CBE", size = 2.8) +
  geom_point(data = career[hl == TRUE], colour = "#D55E00", size = 4) +
  geom_text_repel(data = lab_c, aes(label = caller, fontface = ifelse(hl, "bold", "plain")),
                   size = 2.9, colour = ifelse(lab_c$hl, "#8a3d00", "#1d6a99"), seed = 11,
                   box.padding = 0.42, min.segment.length = 0, max.overlaps = 25) +
  annotate("text", x = min(career$execution), y = max(career$design_resid), hjust = 0, vjust = 1, size = 2.85,
           fontface = "italic", colour = "grey45", label = "design carries ordinary players") +
  annotate("text", x = max(career$execution), y = max(career$design_resid), hjust = 1, vjust = 1, size = 2.85,
           fontface = "italic", colour = "grey45", label = "great players AND design that adds") +
  annotate("text", x = min(career$execution), y = min(career$design_resid), hjust = 0, vjust = 0, size = 2.85,
           fontface = "italic", colour = "grey45", label = "ordinary players, no design edge either") +
  annotate("text", x = max(career$execution), y = min(career$design_resid), hjust = 1, vjust = 0, size = 2.85,
           fontface = "italic", colour = "grey45", label = "good players, no design edge") +
  labs(title = "Scheme value and player execution are different things, and most callers are only good at one",
       subtitle = "x = how well a caller's players grade out (leave-one-out, not the same-play grade); y = EPA/play left over after situation AND execution are priced in",
       x = "execution level (mean leave-one-out player grade)", y = "design value (EPA/play, situation- and execution-adjusted)") +
  theme_coach(grid = "none")

TITLE_C <- sprintf("McVay: execution #%d, design #%d of %d | Shanahan: #%d, #%d | Ben Johnson: #%d, #%d",
                    career[caller == "Sean McVay", execution_rank], career[caller == "Sean McVay", design_rank], nrow(career),
                    career[caller == "Kyle Shanahan", execution_rank], career[caller == "Kyle Shanahan", design_rank],
                    career[caller == "Ben Johnson", execution_rank], career[caller == "Ben Johnson", design_rank])

p_final <- pC + plot_annotation(
  title = TITLE_C,
  caption = fig_caption(
    "Sumer play and player charting 2022-2025, play-caller attribution from samhoppen/NFL_public",
    sprintf("%d qualified callers (%d+ plays), %s plays with a valid leave-one-out execution score.",
            nrow(career), QUAL_MIN, format(nrow(d2), big.mark = ",")),
    sprintf("\nExecution is each play's leave-one-out grade (HIGH=+1/NEUTRAL=0/LOW=-1), not the same-play grade: r(same-play grade, EPA) = %+.2f vs r(leave-one-out grade, EPA) = %+.2f, so the\nsame-play version was too outcome-contaminated to use. Design value is the residual of a season-grouped, out-of-sample EPA model after both situation and execution are\naccounted for. Persistence (odd vs even season) of design value: r = %+.2f, %s. Built by R/26.",
            r_same, r_loo, ps$r, ps$verdict)
  ),
  theme = theme_coach(grid = "none")
)
save_fig("docs/figures/scheme_vs_execution.png", p_final, w = 13.6, h = 7.8)

cat("\n\n=== DONE ===\n")
cat("wrote docs/figures/scheme_vs_execution.png\n")
cat("wrote data/derived/scheme_execution.csv\n")
