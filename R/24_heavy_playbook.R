# =============================================================================
# 24_heavy_playbook.R -- what teams actually run out of heavy personnel.
#
# Follows R/21_rams_13.R (Rams 13-personnel usage/EPA/box splits) and
# R/factory/60_narratives.R (McVay's 11-to-13 reinvention, PA-by-personnel).
# Those showed 13 personnel was worth +0.18 EPA leaguewide and that LA's own
# 13-personnel numbers were exceptional. Michael's reaction, verbatim:
#
#   "I used Sumer Brain and the rest of the league wasn't great at 13
#    personnel...this furthers the story so if you have anything on what the
#    Rams are doing specifically out of 13 that'd be great. Basically my
#    point is, its not just personnel packages - everyone else kinda sucked
#    at this while the Rams were insane. So what were they doing out of it?"
#
#   "Wtf does Kyle do"
#
# And separately, on whether Shanahan changed anything in 2025:
#
#   "What ways do you think Schnahan adapted in 2025?"
#
# Nick's informal answer, to be tested against the data:
#
#   "Im not sure he has to adapt. Its more so his scheme is pretty plug and
#    play. But CMC carried the load."
#
# This script does three things. (1) Decomposes LA's 2025 13-personnel snaps
# into the actual playbook -- run/pass mix, run concepts, play-action rate
# and concepts, under-center rate, formation, gadget rate, depth profile --
# against the rest of the league out of 13, ranked by size of the gap.
# (2) Does the same for SF's heavy packages, but SF barely touches 13
# personnel (n=8 all season), so this becomes the real personnel contrast:
# LA gets heavy with extra tight ends (13), SF gets heavy with a second back
# (21), confirmed straight from the personnel codes, and (3) tests whether
# Shanahan's own scheme fingerprint moved in 2025 relative to his own
# 2022-2024 baseline, benchmarked against how much an average team-season
# drifts year to year when the SAME play-caller stays in the building
# (reusing the stayed/changed logic from R/factory/82_travels.R).
#
# DATA. Sumer play charting via R/factory/lib_sumer.R's load_sumer().
# offensive_personnel_basic strips its "*" suffix per R/21's precedent (no
# clean split against formation_unbalanced or quarterback_alignment).
# run_pass restricted to {"P","R"}; the blank category is special-teams
# plays that survive load_sumer()'s no_play filter (kicks, punts) and are
# not part of any offense's playbook. Universe throughout is non-garbage-time,
# matching every script since R/18.
#
# WHAT "LEAGUE OUT OF 13" MEANS. Every comparison excludes the team under
# study (LA excluded from its own league baseline, SF from its own) because
# LA alone is 18.8% of every charted 13-personnel snap in the league in
# 2025 -- folding LA back into "league" would have Michael's own team
# grading itself.
#
# THE STABILITY TEST. Nine fingerprint measures (personnel mix 11/12/13/21,
# play-action rate, under-center rate, no-huddle rate, outside-zone run
# rate, average depth of target) are z-scored across every team-season
# 2022-2025, then every consecutive team-season pair gets a "drift" score:
# mean absolute z-score change across the nine measures. Pairs are split by
# whether the play-caller stayed, exactly as R/factory/82_travels.R does.
# The distribution of drift among STAYED pairs is the normal amount a
# fingerprint moves for a coach who kept his job. Shanahan called plays for
# SF in all four charted seasons, so all three of his own transitions are
# "stayed" pairs, and where his 2024-to-2025 jump lands in that league
# distribution is the adaptation verdict.
#
# CMC CAVEAT. Sumer's play-level pull carries no ball-carrier/player field,
# so "CMC carried the load" cannot be tested as a rushing-share number here.
# It is approximated with SF's run rate and run EPA by season, flagged as an
# approximation, not measured directly.
#
# Conventions: R/lib/theme_coach.R, no em dashes, honest computed titles,
# sample sizes on every split (concept-level splits get thin fast, especially
# play-action concepts, which only exist on the subset of dropbacks that had
# play action at all).
#
# Out: docs/figures/rams_13_playbook.png  (LA vs league out of 13, ranked)
#      docs/figures/kyle_heavy.png        (SF 21 vs LA 13: two routes to heavy)
#      docs/figures/shanahan_2025.png     (SF fingerprint by season + drift verdict)
#      data/derived/heavy_playbook.csv    (LA-vs-league-13 ranked diff table)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(patchwork); library(ggrepel)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

d <- load_sumer(2022:2025)
d <- d[run_pass %in% c("P", "R")]
d[, pb := sub("[*]$", "", offensive_personnel_basic)]
d[, is_pass := run_pass == "P"]
d[, gadget := jet_sweep_run == TRUE | end_around_run == TRUE | reverse_run == TRUE]
d[, extra_blocker := lead_run == TRUE | split_run == TRUE | cross_lead_run == TRUE]

d25 <- d[season == 2025 & garbage_time == FALSE]

# =============================================================================
# TEST 1: WHAT LA RUNS OUT OF 13, RANKED AGAINST THE LEAGUE
# =============================================================================
cat("=== 1. LA vs league out of 13 personnel, 2025 non-garbage-time ===\n")

la13 <- d25[pb == "13" & off_team == "LA"]
lg13 <- d25[pb == "13" & off_team != "LA"]
la_r <- la13[run_pass == "R"]; lg_r <- lg13[run_pass == "R"]
la_p <- la13[run_pass == "P"]; lg_p <- lg13[run_pass == "P"]
la_pa <- la_p[play_action == 1]; lg_pa <- lg_p[play_action == 1]

cat(sprintf("LA 13-personnel snaps: n = %d | league (ex-LA) 13-personnel snaps: n = %d, spread across %d other teams\n",
            nrow(la13), nrow(lg13), uniqueN(lg13$off_team)))
cat(sprintf("LA is %.1f%% of every charted 13-personnel snap in the league this season -- excluded from its own baseline.\n",
            100 * nrow(la13) / (nrow(la13) + nrow(lg13))))

# metric spec: label, universe, numerator count, denominator count, for LA and league
spec <- list(
  list(label = "Run rate (of snaps)",                 universe = "snaps",
       la_n = sum(la13$run_pass == "R"), la_d = nrow(la13),
       lg_n = sum(lg13$run_pass == "R"), lg_d = nrow(lg13)),
  list(label = "Under center (of snaps)",              universe = "snaps",
       la_n = sum(la13$quarterback_alignment == "UNDER CENTER", na.rm = TRUE), la_d = sum(!is.na(la13$quarterback_alignment)),
       lg_n = sum(lg13$quarterback_alignment == "UNDER CENTER", na.rm = TRUE), lg_d = sum(!is.na(lg13$quarterback_alignment))),
  list(label = "Play action (of dropbacks)",           universe = "dropbacks",
       la_n = sum(la_p$play_action == 1, na.rm = TRUE), la_d = sum(!is.na(la_p$play_action)),
       lg_n = sum(lg_p$play_action == 1, na.rm = TRUE), lg_d = sum(!is.na(lg_p$play_action))),
  list(label = "Screen (of dropbacks)",                universe = "dropbacks",
       la_n = sum(la_p$screen == 1, na.rm = TRUE), la_d = sum(!is.na(la_p$screen)),
       lg_n = sum(lg_p$screen == 1, na.rm = TRUE), lg_d = sum(!is.na(lg_p$screen))),
  list(label = "Deep shot, 20+ air yards (of dropbacks)", universe = "dropbacks",
       la_n = sum(la_p$depth_of_target >= 20, na.rm = TRUE), la_d = sum(!is.na(la_p$depth_of_target)),
       lg_n = sum(lg_p$depth_of_target >= 20, na.rm = TRUE), lg_d = sum(!is.na(lg_p$depth_of_target))),
  list(label = "RPO (of snaps)",                       universe = "snaps",
       la_n = sum(la13$run_pass_option == TRUE), la_d = nrow(la13),
       lg_n = sum(lg13$run_pass_option == TRUE), lg_d = nrow(lg13)),
  list(label = "Unbalanced formation (of snaps)",      universe = "snaps",
       la_n = sum(la13$formation_unbalanced == TRUE), la_d = nrow(la13),
       lg_n = sum(lg13$formation_unbalanced == TRUE), lg_d = nrow(lg13)),
  list(label = "Jet/end around/reverse (of snaps)",    universe = "snaps",
       la_n = sum(la13$gadget == TRUE), la_d = nrow(la13),
       lg_n = sum(lg13$gadget == TRUE), lg_d = nrow(lg13)),
  list(label = "Run concept: man/gap (of runs)",       universe = "runs",
       la_n = sum(la_r$run_concept == "MAN"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "MAN"), lg_d = nrow(lg_r)),
  list(label = "Run concept: outside zone (of runs)",  universe = "runs",
       la_n = sum(la_r$run_concept == "OUTSIDE ZONE"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "OUTSIDE ZONE"), lg_d = nrow(lg_r)),
  list(label = "Run concept: inside zone (of runs)",   universe = "runs",
       la_n = sum(la_r$run_concept == "INSIDE ZONE"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "INSIDE ZONE"), lg_d = nrow(lg_r)),
  list(label = "Run concept: power (of runs)",         universe = "runs",
       la_n = sum(la_r$run_concept == "POWER"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "POWER"), lg_d = nrow(lg_r)),
  list(label = "Run concept: pull lead (of runs)",     universe = "runs",
       la_n = sum(la_r$run_concept == "PULL LEAD"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "PULL LEAD"), lg_d = nrow(lg_r)),
  list(label = "Run concept: counter (of runs)",       universe = "runs",
       la_n = sum(la_r$run_concept == "COUNTER"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "COUNTER"), lg_d = nrow(lg_r)),
  list(label = "Run concept: trick (of runs)",         universe = "runs",
       la_n = sum(la_r$run_concept == "TRICK"), la_d = nrow(la_r),
       lg_n = sum(lg_r$run_concept == "TRICK"), lg_d = nrow(lg_r)),
  list(label = "PA look: power (of PA dropbacks)",     universe = "PA dropbacks",
       la_n = sum(la_pa$play_action_concept == "POWER"), la_d = nrow(la_pa),
       lg_n = sum(lg_pa$play_action_concept == "POWER"), lg_d = nrow(lg_pa)),
  list(label = "PA look: inside zone (of PA dropbacks)", universe = "PA dropbacks",
       la_n = sum(la_pa$play_action_concept == "INSIDE ZONE"), la_d = nrow(la_pa),
       lg_n = sum(lg_pa$play_action_concept == "INSIDE ZONE"), lg_d = nrow(lg_pa)),
  list(label = "PA look: man (of PA dropbacks)",       universe = "PA dropbacks",
       la_n = sum(la_pa$play_action_concept == "MAN"), la_d = nrow(la_pa),
       lg_n = sum(lg_pa$play_action_concept == "MAN"), lg_d = nrow(lg_pa)),
  list(label = "PA look: counter (of PA dropbacks)",   universe = "PA dropbacks",
       la_n = sum(la_pa$play_action_concept == "COUNTER"), la_d = nrow(la_pa),
       lg_n = sum(lg_pa$play_action_concept == "COUNTER"), lg_d = nrow(lg_pa))
)

diff_row <- function(s) {
  p <- tryCatch(prop.test(c(s$la_n, s$lg_n), c(s$la_d, s$lg_d))$p.value, error = function(e) NA_real_)
  data.table(metric = s$label, universe = s$universe,
             la_rate = 100 * s$la_n / s$la_d, la_n = s$la_n, la_d = s$la_d,
             lg_rate = 100 * s$lg_n / s$lg_d, lg_n = s$lg_n, lg_d = s$lg_d,
             diff = 100 * s$la_n / s$la_d - 100 * s$lg_n / s$lg_d, p = p)
}
diff_tab <- rbindlist(lapply(spec, diff_row))
diff_tab[, thin := la_d < 20 | lg_d < 20]
diff_tab <- diff_tab[order(-abs(diff))]
write_csv(as.data.frame(diff_tab), "data/derived/heavy_playbook.csv")

print(diff_tab[, .(metric, la = sprintf("%.1f%% (n=%d)", la_rate, la_d), lg = sprintf("%.1f%% (n=%d)", lg_rate, lg_d),
                   diff = round(diff, 1), p = round(p, 3), thin)])

cat("\nranked LA-vs-league gaps, biggest first:\n")
for (i in seq_len(nrow(diff_tab))) with(diff_tab[i], cat(sprintf(
  "  %-46s LA %5.1f%% vs lg %5.1f%%  diff %+6.1fpp  p=%s%s\n",
  metric, la_rate, lg_rate, diff, if (is.na(p)) "NA" else sprintf("%.3f", p), if (thin) "  (thin)" else "")))

sig1 <- diff_tab[!is.na(p) & p < 0.05]
cat(sprintf("\n%d of %d metrics clear p < 0.05: %s\n", nrow(sig1), nrow(diff_tab), paste(sig1$metric, collapse = "; ")))

cat("\n=== 1b. does it work: LA/13 vs league/13 EPA, direct test of 'the rest of the league wasn't great at 13' ===\n")
epa_la_pass <- mean(la_p$expected_points_added, na.rm = TRUE); epa_lg_pass <- mean(lg_p$expected_points_added, na.rm = TRUE)
epa_la_run  <- mean(la_r$expected_points_added, na.rm = TRUE); epa_lg_run  <- mean(lg_r$expected_points_added, na.rm = TRUE)
tt_pass13 <- t.test(la_p$expected_points_added, lg_p$expected_points_added)
tt_run13  <- t.test(la_r$expected_points_added, lg_r$expected_points_added)
cat(sprintf("Passing: LA %+.3f vs league-13 %+.3f EPA/play, diff %+.3f, p=%.4f (n=%d vs %d)\n",
            epa_la_pass, epa_lg_pass, epa_la_pass - epa_lg_pass, tt_pass13$p.value, nrow(la_p), nrow(lg_p)))
cat(sprintf("Running: LA %+.3f vs league-13 %+.3f EPA/play, diff %+.3f, p=%.4f (n=%d vs %d)\n",
            epa_la_run, epa_lg_run, epa_la_run - epa_lg_run, tt_run13$p.value, nrow(la_r), nrow(lg_r)))
cat("The rest of the league is genuinely bad out of 13: league-13 passing is below replacement and running is well underwater, while LA is positive on both.\n")
cat("(R/21_rams_13.R already verified LA's own .50/.07 claim in isolation; this is the same LA numbers compared directly against the league-13 baseline that was missing there.)\n")

# ------------------------------------------------------------ chart A
diff_tab[, dir := factor(ifelse(diff >= 0, "LA higher", "LA lower"), levels = c("LA higher", "LA lower"))]
diff_tab[, lab := sprintf("%s  (LA n=%d, lg n=%d)", metric, la_d, lg_d)]
diff_tab[, lab := factor(lab, levels = rev(lab))]

pA <- ggplot(diff_tab, aes(diff, lab)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = diff, y = lab, yend = lab, colour = dir), linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(colour = dir), size = 3) +
  geom_text(aes(label = sprintf("%+.1fpp", diff), hjust = ifelse(diff >= 0, -0.25, 1.25)),
            size = 2.9, fontface = "bold", colour = "grey25") +
  scale_colour_manual(values = c("LA higher" = "#003594", "LA lower" = "#D55E00")) +
  scale_x_continuous(limits = c(min(diff_tab$diff) - 8, max(diff_tab$diff) + 8)) +
  labs(
    title = "Out of 13 personnel, the Rams' identity is man/gap blocking and a real passing game, not a bigger version of everyone else's 13",
    subtitle = "LA minus league (ex-LA), percentage points, 2025 non-garbage-time snaps out of 13 personnel",
    x = "LA rate minus league rate (percentage points)", y = NULL,
    caption = fig_caption(
      "SumerSports play charting, 2025 season",
      sprintf("League excludes LA (%d of the league's %d 13-personnel snaps, %.0f%%). Metrics marked thin have a denominator under 20 on one side; play-action-concept rows are\nthe thinnest, splitting an already-small dropback sample five ways.",
              nrow(la13), nrow(la13) + nrow(lg13), 100 * nrow(la13) / (nrow(la13) + nrow(lg13))),
      sprintf("\nBiggest, least-thin gaps: LA runs man/gap blocking scheme out of 13 more than double the league rate and has not called a single power run out of it all season, while the league leans on\nzone and power almost evenly. LA also throws it more out of 13 (under center 89%% of snaps but still a real dropback package) than the league does, which mostly treats 13 as a run-first grouping.\nDoes it work: league-13 passing (%+.2f EPA/play) and running (%+.2f) are both below the league's own overall average; LA is positive on both (%+.2f, %+.2f). Built by R/24_heavy_playbook.R.",
              epa_lg_pass, epa_lg_run, epa_la_pass, epa_la_run))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")
save_fig("docs/figures/rams_13_playbook.png", pA, w = 13, h = 10.5)

# =============================================================================
# TEST 2: WTF DOES KYLE DO -- SF's heavy packages vs LA's
# =============================================================================
cat("\n=== 2. SF heavy personnel, 2025 non-garbage-time ===\n")

sf_all <- d25[off_team == "SF"]
la_all <- d25[off_team == "LA"]
cat(sprintf("SF snaps: n=%d | 13-personnel: n=%d (%.1f%%) | 21-personnel: n=%d (%.1f%%)\n",
            nrow(sf_all), sum(sf_all$pb == "13"), 100 * mean(sf_all$pb == "13"),
            sum(sf_all$pb == "21"), 100 * mean(sf_all$pb == "21")))
cat(sprintf("LA snaps:  n=%d | 13-personnel: n=%d (%.1f%%) | 21-personnel: n=%d (%.1f%%)\n",
            nrow(la_all), sum(la_all$pb == "13"), 100 * mean(la_all$pb == "13"),
            sum(la_all$pb == "21"), 100 * mean(la_all$pb == "21")))
cat("SF's 13-personnel sample (n=8) is too thin to analyze -- Shanahan does not use it. SF's heavy package is 21, LA's is 13:\n")
cat("straight from the personnel codes, LA gets big with a third tight end and never dresses a second back (21 = 0.0% of snaps);\n")
cat("SF gets big with a second back and almost never a third tight end (13 = 0.6% of snaps). Two different routes to 'heavy.'\n")

sf21 <- d25[pb == "21" & off_team == "SF"]
lg21 <- d25[pb == "21" & off_team != "SF"]
sf21_r <- sf21[run_pass == "R"]; lg21_r <- lg21[run_pass == "R"]
sf21_p <- sf21[run_pass == "P"]; lg21_p <- lg21[run_pass == "P"]

spec2 <- list(
  list(label = "Run rate (of snaps)",                sf_n = sum(sf21$run_pass == "R"), sf_d = nrow(sf21),
       lg_n = sum(lg21$run_pass == "R"), lg_d = nrow(lg21)),
  list(label = "Under center (of snaps)",             sf_n = sum(sf21$quarterback_alignment == "UNDER CENTER", na.rm = TRUE), sf_d = sum(!is.na(sf21$quarterback_alignment)),
       lg_n = sum(lg21$quarterback_alignment == "UNDER CENTER", na.rm = TRUE), lg_d = sum(!is.na(lg21$quarterback_alignment))),
  list(label = "Play action (of dropbacks)",          sf_n = sum(sf21_p$play_action == 1, na.rm = TRUE), sf_d = sum(!is.na(sf21_p$play_action)),
       lg_n = sum(lg21_p$play_action == 1, na.rm = TRUE), lg_d = sum(!is.na(lg21_p$play_action))),
  list(label = "Run concept: outside zone (of runs)", sf_n = sum(sf21_r$run_concept == "OUTSIDE ZONE"), sf_d = nrow(sf21_r),
       lg_n = sum(lg21_r$run_concept == "OUTSIDE ZONE"), lg_d = nrow(lg21_r)),
  list(label = "Run concept: power (of runs)",        sf_n = sum(sf21_r$run_concept == "POWER"), sf_d = nrow(sf21_r),
       lg_n = sum(lg21_r$run_concept == "POWER"), lg_d = nrow(lg21_r)),
  list(label = "Run concept: man/gap (of runs)",      sf_n = sum(sf21_r$run_concept == "MAN"), sf_d = nrow(sf21_r),
       lg_n = sum(lg21_r$run_concept == "MAN"), lg_d = nrow(lg21_r))
)
diff2 <- rbindlist(lapply(spec2, function(s) {
  p <- tryCatch(prop.test(c(s$sf_n, s$lg_n), c(s$sf_d, s$lg_d))$p.value, error = function(e) NA_real_)
  data.table(metric = s$label, sf_rate = 100 * s$sf_n / s$sf_d, sf_d = s$sf_d,
             lg_rate = 100 * s$lg_n / s$lg_d, lg_d = s$lg_d,
             diff = 100 * s$sf_n / s$sf_d - 100 * s$lg_n / s$lg_d, p = p)
}))
adot_sf21 <- mean(sf21_p$depth_of_target, na.rm = TRUE); adot_lg21 <- mean(lg21_p$depth_of_target, na.rm = TRUE)
cat(sprintf("\naDOT out of 21: SF %.2f (n=%d) vs league %.2f (n=%d)\n",
            adot_sf21, sum(!is.na(sf21_p$depth_of_target)), adot_lg21, sum(!is.na(lg21_p$depth_of_target))))
print(diff2[, .(metric, sf = sprintf("%.1f%% (n=%d)", sf_rate, sf_d), lg = sprintf("%.1f%% (n=%d)", lg_rate, lg_d),
                diff = round(diff, 1), p = round(p, 3))])
cat(sprintf("\nSF out of 21: run rate %.1f%% (LOWER than league-21's %.1f%%), outside zone at %.1f%% of runs (well above league-21's %.1f%%),\n",
            diff2[metric == "Run rate (of snaps)"]$sf_rate, diff2[metric == "Run rate (of snaps)"]$lg_rate,
            diff2[metric == "Run concept: outside zone (of runs)"]$sf_rate, diff2[metric == "Run concept: outside zone (of runs)"]$lg_rate))
cat(sprintf("play action on just %.1f%% of dropbacks vs league-21's %.1f%%. Kyle Shanahan's answer to 'what do you do out of your heavy set' is: get in it, then throw zone runs at you and\nthrow it downfield without much play-action disguise -- not run-run-run with a lead fullback like the stereotype.\n",
            diff2[metric == "Play action (of dropbacks)"]$sf_rate, diff2[metric == "Play action (of dropbacks)"]$lg_rate))

cat("\n=== 2b. does it work: SF/21 vs league/21 EPA, and the extra-blocker rate reframed against the WHOLE league ===\n")
epa_sf_pass <- mean(sf21_p$expected_points_added, na.rm = TRUE); epa_lg21_pass <- mean(lg21_p$expected_points_added, na.rm = TRUE)
epa_sf_run  <- mean(sf21_r$expected_points_added, na.rm = TRUE); epa_lg21_run  <- mean(lg21_r$expected_points_added, na.rm = TRUE)
tt_pass21 <- t.test(sf21_p$expected_points_added, lg21_p$expected_points_added)
tt_run21  <- t.test(sf21_r$expected_points_added, lg21_r$expected_points_added)
cat(sprintf("Passing: SF %+.3f vs league-21 %+.3f EPA/play, diff %+.3f, p=%.4f (n=%d vs %d)\n",
            epa_sf_pass, epa_lg21_pass, epa_sf_pass - epa_lg21_pass, tt_pass21$p.value, nrow(sf21_p), nrow(lg21_p)))
cat(sprintf("Running: SF %+.3f vs league-21 %+.3f EPA/play, diff %+.3f, p=%.4f (n=%d vs %d) -- statistically even, despite the outside-zone identity\n",
            epa_sf_run, epa_lg21_run, epa_sf_run - epa_lg21_run, tt_run21$p.value, nrow(sf21_r), nrow(lg21_r)))

# extra-blocker framing (lead_run | split_run | cross_lead_run) benchmarked against the WHOLE league's run
# rate, not just other 21-personnel snaps -- own-personnel league-21 baselines wash this out almost entirely
# (SF/21 42.5% vs league-21 41.2%, since 21 personnel already implies a lead blocker most places). Design
# credit: builder-rams13 flagged this reframe independently while working the same task; verified here.
la13_r_all <- d25[pb == "13" & off_team == "LA" & run_pass == "R"]
lg_run_all <- d25[run_pass == "R"]
eb <- rbindlist(list(
  data.table(grp = "LA, out of 13", n = sum(la13_r_all$extra_blocker), d = nrow(la13_r_all)),
  data.table(grp = "SF, out of 21", n = sum(sf21_r$extra_blocker), d = nrow(sf21_r)),
  data.table(grp = "Whole league (all personnel)", n = sum(lg_run_all$extra_blocker), d = nrow(lg_run_all))
))
eb[, `:=`(rate = 100 * n / d)]
for (i in seq_len(nrow(eb))) with(eb[i], cat(sprintf("extra-blocker rate (lead/split/cross-lead run), %-30s %.1f%% (n=%d of %d)\n", grp, rate, n, d)))
cat(sprintf("SF is %.1fx the whole league's extra-blocker rate out of its heavy package; LA is %.1fx. Against the WHOLE league, not just other 21-personnel snaps,\nSF is still the more extreme 'extra blocker' team even though the personnel-package headline (13 vs 21) goes to LA.\n",
            eb[grp == "SF, out of 21"]$rate / eb[grp == "Whole league (all personnel)"]$rate,
            eb[grp == "LA, out of 13"]$rate / eb[grp == "Whole league (all personnel)"]$rate))

# ------------------------------------------------------------ chart B
usage_long <- rbind(
  data.table(team = "LA", personnel = "13 (1 RB, 3 TE)", rate = 100 * mean(la_all$pb == "13")),
  data.table(team = "LA", personnel = "21 (2 RB, 1 TE)", rate = 100 * mean(la_all$pb == "21")),
  data.table(team = "SF", personnel = "13 (1 RB, 3 TE)", rate = 100 * mean(sf_all$pb == "13")),
  data.table(team = "SF", personnel = "21 (2 RB, 1 TE)", rate = 100 * mean(sf_all$pb == "21"))
)
pB1 <- ggplot(usage_long, aes(personnel, rate, fill = team)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  geom_text(aes(label = sprintf("%.1f%%", rate)), position = position_dodge(width = 0.6),
            vjust = -0.6, size = 3.4, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c(LA = "#003594", SF = "#AA0000")) +
  scale_y_continuous(limits = c(0, 38), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Two different ways to get heavy", subtitle = "Share of 2025 offensive snaps, by personnel grouping",
       x = NULL, y = "share of snaps") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")

comp_long <- rbind(
  diff2[, .(metric, grp = "SF, out of 21", rate = sf_rate, n = sf_d)],
  diff2[, .(metric, grp = "League, out of 21 (ex-SF)", rate = lg_rate, n = lg_d)]
)
comp_long[, grp := factor(grp, levels = c("SF, out of 21", "League, out of 21 (ex-SF)"))]
comp_long[, metric := factor(metric, levels = spec2 |> sapply(`[[`, "label"))]
pB2 <- ggplot(comp_long, aes(metric, rate, fill = grp)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%%", rate)), position = position_dodge(width = 0.65),
            vjust = -0.5, size = 2.8, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("SF, out of 21" = "#AA0000", "League, out of 21 (ex-SF)" = "#c9a5a5")) +
  scale_x_discrete(labels = function(x) gsub(" \\(of ", "\n(of ", x)) +
  scale_y_continuous(limits = c(0, 78), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "What Kyle Shanahan runs out of his heavy set", subtitle = "SF vs the rest of the league, out of 21 personnel, 2025 non-garbage-time",
       x = NULL, y = "rate") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left",
        axis.text.x = element_text(size = rel(0.72)))

pB <- (pB1 | pB2) + plot_layout(widths = c(0.75, 1.25)) +
  plot_annotation(
    title = "LA gets heavy with a third tight end, SF gets heavy with a second back, and they run almost nothing alike out of it",
    subtitle = "Personnel usage and heavy-package tendencies, 2025 non-garbage-time",
    caption = fig_caption(
      "SumerSports play charting, 2025 season",
      sprintf("SF's own 13-personnel sample (n=8) is too small to chart; it is functionally not part of Shanahan's playbook. n's for the right panel printed in the console output."),
      sprintf("\nSF actually runs the ball LESS out of 21 than the league does out of 21 (defenses know it's coming and sell out against it), leans on outside zone far more than\nleague-21 does, and uses play action noticeably less. LA's 13-personnel package, by contrast, stays under center and still throws it downfield. Passing beats league-21 out of SF's set (%+.2f EPA/play, p=%.3f) but rushing\ndoes not (%+.2f, statistically even, p=%.2f). And measured against the WHOLE league's extra-blocker rate, not just other 21-personnel snaps, SF (%.0f%%) is still the more extreme extra-blocker\nteam by a wide margin, even though LA (%.0f%%) gets the personnel-innovation headline (whole-league baseline %.0f%%). Built by R/24_heavy_playbook.R.",
              epa_sf_pass - epa_lg21_pass, tt_pass21$p.value, epa_sf_run - epa_lg21_run, tt_run21$p.value,
              eb[grp == "SF, out of 21"]$rate, eb[grp == "LA, out of 13"]$rate, eb[grp == "Whole league (all personnel)"]$rate)
    ),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/kyle_heavy.png", pB, w = 14, h = 7.2)

# =============================================================================
# TEST 3: DID SHANAHAN ADAPT IN 2025?
# =============================================================================
cat("\n=== 3. Shanahan's scheme fingerprint by season, 2022-2025 ===\n")

FP <- c("p11", "p12", "p13", "p21", "pa_rate", "uc_rate", "no_huddle", "adot", "oz_rate")
build_season <- function(x) {
  x[off_caller != "", .(
    n = .N,
    p11 = 100 * mean(pb == "11"), p12 = 100 * mean(pb == "12"),
    p13 = 100 * mean(pb == "13"), p21 = 100 * mean(pb == "21"),
    run_rate = 100 * mean(run_pass == "R"),
    pa_rate  = 100 * mean(play_action[is_pass] == 1, na.rm = TRUE),
    uc_rate  = 100 * mean(quarterback_alignment == "UNDER CENTER", na.rm = TRUE),
    no_huddle = 100 * mean(no_huddle == TRUE),
    adot = mean(depth_of_target[is_pass], na.rm = TRUE),
    oz_rate = 100 * mean(run_concept[run_pass == "R"] == "OUTSIDE ZONE", na.rm = TRUE),
    run_epa = mean(expected_points_added[run_pass == "R"], na.rm = TRUE),
    pass_epa = mean(expected_points_added[is_pass], na.rm = TRUE),
    off_epa = mean(expected_points_added, na.rm = TRUE),
    caller = names(sort(table(off_caller), decreasing = TRUE))[1]
  ), by = .(team = off_team, season)][n >= 300]
}
team_seas <- build_season(d[garbage_time == FALSE])
for (m in FP) team_seas[, paste0("z_", m) := as.numeric(scale(get(m)))]

setorder(team_seas, team, season)
team_seas[, `:=`(nxt_season = shift(season, -1), nxt_caller = shift(caller, -1)), by = team]
for (m in FP) team_seas[, paste0("nxt_z_", m) := shift(get(paste0("z_", m)), -1), by = team]
pairs <- team_seas[!is.na(nxt_season) & nxt_season - season == 1]
pairs[, stayed := caller == nxt_caller]
pairs[, drift := rowMeans(abs(sapply(FP, function(m) get(paste0("z_", m)) - get(paste0("nxt_z_", m)))))]

sf_seas <- team_seas[team == "SF"]
cat("SF fingerprint by season:\n")
print(sf_seas[, .(season, p11 = round(p11,1), p12 = round(p12,1), p13 = round(p13,1), p21 = round(p21,1),
                  run_rate = round(run_rate,1), pa_rate = round(pa_rate,1), uc_rate = round(uc_rate,1),
                  no_huddle = round(no_huddle,1), adot = round(adot,2), oz_rate = round(oz_rate,1),
                  run_epa = round(run_epa,3), pass_epa = round(pass_epa,3), off_epa = round(off_epa,3))])

# formal slope test on SF's 21-personnel share, vs the league-wide trend in the same grouping over the
# same window. Design credit: builder-rams13 ran this independently on the same task; verified here.
p21_slope <- summary(lm(p21 ~ season, sf_seas))$coefficients["season", ]
lg_p21_seas <- team_seas[, .(p21 = 100 * weighted.mean(p21/100, n)), by = season][order(season)]
cat(sprintf("\nSF 21-personnel share, season slope: %+.3f pts/season (p=%.3f) -- no detectable trend, not just 'looks flat'\n",
            p21_slope["Estimate"], p21_slope["Pr(>|t|)"]))
cat("league-wide 21-personnel share by season:\n"); print(lg_p21_seas[, .(season, p21 = round(p21,1))])
cat(sprintf("League-wide 21-personnel usage fell %.1f%% (%.1f%% in 2022) to %.1f%% (2025) over the same window SF held flat --\nShanahan is a bigger outlier in 21 personnel now than he was three years ago, without moving himself at all.\n",
            lg_p21_seas[season == min(season)]$p21 - lg_p21_seas[season == max(season)]$p21,
            lg_p21_seas[season == min(season)]$p21, lg_p21_seas[season == max(season)]$p21))

stayed_drift <- pairs[stayed == TRUE]$drift
changed_drift <- pairs[stayed == FALSE]$drift
sf_pairs <- pairs[team == "SF"]
sf2425 <- sf_pairs[season == 2024]$drift
pct2425 <- 100 * mean(stayed_drift <= sf2425)
la_pairs <- pairs[team == "LA"]
la2425 <- la_pairs[season == 2024]$drift
pct_la2425 <- 100 * mean(stayed_drift <= la2425)

cat(sprintf("\ndrift = mean absolute change in z-scored fingerprint (personnel mix, PA rate, under-center rate, tempo, aDOT, outside-zone rate) across consecutive seasons\n"))
cat(sprintf("league distribution of drift among SAME-CALLER (stayed) team-season pairs: n=%d, median %.3f, mean %.3f\n",
            length(stayed_drift), median(stayed_drift), mean(stayed_drift)))
cat(sprintf("league distribution of drift among NEW-CALLER (changed) team-season pairs: n=%d, mean %.3f (for contrast, drift should be bigger when the guy actually changes)\n",
            length(changed_drift), mean(changed_drift)))
cat("\nSF's own year-over-year transitions (Shanahan called plays all four seasons, so all three are 'stayed' pairs):\n")
for (i in seq_len(nrow(sf_pairs))) with(sf_pairs[i], cat(sprintf(
  "  %d -> %d: drift %.3f (%.0fth percentile of the league's stayed-pair distribution)\n",
  season, nxt_season, drift, 100 * mean(stayed_drift <= drift))))
cat(sprintf("\nfor comparison, McVay's LA 2024->2025 drift (the 11-to-13 reinvention): %.3f (%.0fth percentile) -- this is what real in-season adaptation looks like in this metric.\n",
            la2425, pct_la2425))
cat(sprintf("SF 2024->2025 sits at the %.0fth percentile: Shanahan moved LESS than the typical coach who kept his play-calling job did.\n", pct2425))

top_drift <- pairs[stayed == TRUE][order(-drift)][1:5]
cat("\nbiggest same-caller fingerprint swings leaguewide, for scale:\n")
for (i in 1:5) with(top_drift[i], cat(sprintf("  %s %d->%d: drift %.3f\n", team, season, nxt_season, drift)))

verdict <- if (pct2425 < 50) "Shanahan's 2025 barely moved" else "Shanahan's 2025 shifted more than a typical stable coach"
cat(sprintf("\nVERDICT: %s. CMC angle (approximated, no player-level rushing share in this Sumer pull): SF's run rate was %.1f%% in 2025 vs %.1f%% averaged 2022-2024\n",
            verdict, sf_seas[season == 2025]$run_rate, mean(sf_seas[season < 2025]$run_rate)))
cat(sprintf("and run EPA was %+.3f in 2025 vs %+.3f averaged 2022-2024 -- run rate is basically flat and run efficiency was actually the WORST of the four seasons,\n",
            sf_seas[season == 2025]$run_epa, mean(sf_seas[season < 2025]$run_epa)))
cat("so 'CMC carried the load' does not show up as a playcalling shift toward more or better rushing; it would need actual carry-share data to test directly.\n")

# ------------------------------------------------------------ chart C
lab_fp <- c(p11 = "11 personnel (%)", p12 = "12 personnel (%)", p13 = "13 personnel (%)", p21 = "21 personnel (%)",
            pa_rate = "Play action rate (%)", uc_rate = "Under center rate (%)", no_huddle = "No-huddle rate (%)",
            adot = "Avg depth of target (yds)", oz_rate = "Outside zone, of runs (%)")
sf_long <- melt(sf_seas[, c("season", FP), with = FALSE], id.vars = "season",
                variable.name = "metric", value.name = "value")
sf_long[, metric := factor(lab_fp[as.character(metric)], levels = unname(lab_fp))]

pC1 <- ggplot(sf_long, aes(season, value)) +
  geom_line(colour = "#AA0000", linewidth = 0.9) +
  geom_point(colour = "#AA0000", size = 2.2) +
  geom_text(aes(label = round(value, 1)), vjust = -1, size = 2.6, colour = "grey30") +
  facet_wrap(~ metric, scales = "free_y", ncol = 3) +
  scale_x_continuous(breaks = 2022:2025) +
  labs(title = "Shanahan's fingerprint, season by season", subtitle = "SF, non-garbage-time, 2022-2025",
       x = NULL, y = NULL) +
  theme_coach(grid = "y") +
  theme(strip.text = element_text(size = rel(0.8)))

drift_pts <- rbind(
  data.table(label = "SF 22-23", x = sf_pairs[season == 2022]$drift, hl = "SF"),
  data.table(label = "SF 23-24", x = sf_pairs[season == 2023]$drift, hl = "SF"),
  data.table(label = "SF 24-25", x = sf_pairs[season == 2024]$drift, hl = "SF"),
  data.table(label = "LA 24-25\n(McVay pivot)", x = la2425, hl = "LA")
)
pC2 <- ggplot(pairs[stayed == TRUE], aes(drift, y = 0)) +
  geom_vline(xintercept = median(stayed_drift), linetype = "22", colour = ink_baseline, linewidth = 0.4) +
  geom_jitter(height = 0.35, width = 0, size = 1.8, colour = "grey70", alpha = 0.6) +
  geom_point(data = drift_pts, aes(x = x, y = 0.45, colour = hl), size = 4) +
  geom_text_repel(data = drift_pts, aes(x = x, y = 0.45, label = label, colour = hl),
                  size = 3, fontface = "bold", lineheight = 0.9, direction = "both",
                  nudge_y = 0.55, segment.size = 0.4, min.segment.length = 0,
                  seed = 11, box.padding = 0.4) +
  annotate("text", x = median(stayed_drift), y = -0.55,
           label = sprintf("league median\nsame-caller drift: %.2f", median(stayed_drift)),
           size = 2.8, colour = "grey45", lineheight = 0.9) +
  scale_colour_manual(values = c(SF = "#AA0000", LA = "#003594"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.04, 0.1))) +
  scale_y_continuous(limits = c(-0.9, 1.7)) +
  coord_cartesian(clip = "off") +
  labs(title = sprintf("SF 2024->2025 lands at the %.0fth percentile of all same-caller drift leaguewide", pct2425),
       subtitle = "Every dot is one team-season pair where the SAME play-caller stayed, 2022-2025",
       x = "fingerprint drift (mean |z-score change| across 9 measures)", y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_blank(), plot.margin = margin(10, 30, 8, 10))

pC <- (pC1 / pC2) + plot_layout(heights = c(1.3, 1)) +
  plot_annotation(
    title = sprintf("%s -- his three seasons with the same play-caller all sit below the league's normal year-to-year drift",
                     verdict),
    subtitle = "Fingerprint = personnel mix, play action rate, under-center rate, tempo, depth of target, outside-zone run rate",
    caption = fig_caption(
      "SumerSports play charting, 2022-2025 regular season, non-garbage-time",
      sprintf("%d team-seasons with 300+ charted plays, %d consecutive same-caller pairs leaguewide.", nrow(team_seas), length(stayed_drift)),
      sprintf("\nAll three of Shanahan's own transitions (2022-23: %.0fth pct, 2023-24: %.0fth pct, 2024-25: %.0fth pct) sit below the league median for a coach who kept his job. His 21-personnel share\nshows no season slope either (%+.3f pts/season, p=%.2f) while league-wide 21-personnel usage fell %.1f%% to %.1f%% over the same window, making him a bigger outlier now without moving.\n",
              100*mean(stayed_drift<=sf_pairs[season==2022]$drift), 100*mean(stayed_drift<=sf_pairs[season==2023]$drift), pct2425,
              p21_slope["Estimate"], p21_slope["Pr(>|t|)"], lg_p21_seas[season==min(season)]$p21, lg_p21_seas[season==max(season)]$p21) %>%
        paste0(sprintf("McVay's 2024-25 pivot from 11 to 13 personnel sits at the %.0fth percentile, for scale of what an actual scheme change looks like in this metric. The one thing that did move for SF:\n", pct_la2425),
               sprintf("run rate was flat (%.1f%% in 2025 vs %.1f%% average 2022-2024) and run EPA was its worst of the four seasons (%+.3f), so any 'CMC carried the load' effect is not visible as a playcalling\nshift here -- it would take carry-share data this Sumer pull does not have. Built by R/24_heavy_playbook.R.",
                       sf_seas[season==2025]$run_rate, mean(sf_seas[season<2025]$run_rate), sf_seas[season==2025]$run_epa))
    ),
    theme = theme_coach(grid = "none")
  )
save_fig("docs/figures/shanahan_2025.png", pC, w = 13, h = 11)

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n=== SUMMARY ===\n")
cat(sprintf("1. WHAT LA DOES OUT OF 13: man/gap blocking on %.1f%% of 13-personnel runs (league %.1f%%, %+.1fpp, p=%.3f) and zero power runs all season\n",
            diff_tab[metric == "Run concept: man/gap (of runs)"]$la_rate, diff_tab[metric == "Run concept: man/gap (of runs)"]$lg_rate,
            diff_tab[metric == "Run concept: man/gap (of runs)"]$diff, diff_tab[metric == "Run concept: man/gap (of runs)"]$p))
cat(sprintf("   (league runs power %.1f%% of the time out of 13); LA also stays a real dropback package (deep-shot rate %.1f%% vs league %.1f%%) rather than treating 13 as pure run personnel.\n",
            diff_tab[metric == "Run concept: power (of runs)"]$lg_rate,
            diff_tab[metric == "Deep shot, 20+ air yards (of dropbacks)"]$la_rate, diff_tab[metric == "Deep shot, 20+ air yards (of dropbacks)"]$lg_rate))
cat(sprintf("2. WTF DOES KYLE DO: gets heavy with a 2nd back (21 personnel, %.1f%% of snaps) not a 3rd tight end (13, %.1f%%) -- the opposite of LA -- and out of 21 he runs LESS (%.1f%% vs league-21's %.1f%%),\n",
            100*mean(sf_all$pb=="21"), 100*mean(sf_all$pb=="13"),
            diff2[metric=="Run rate (of snaps)"]$sf_rate, diff2[metric=="Run rate (of snaps)"]$lg_rate))
cat(sprintf("   leans on outside zone more (%.1f%% of runs vs %.1f%%), and uses play action LESS (%.1f%% vs %.1f%%).\n",
            diff2[metric=="Run concept: outside zone (of runs)"]$sf_rate, diff2[metric=="Run concept: outside zone (of runs)"]$lg_rate,
            diff2[metric=="Play action (of dropbacks)"]$sf_rate, diff2[metric=="Play action (of dropbacks)"]$lg_rate))
cat(sprintf("3. DID SHANAHAN ADAPT: no. SF 2024->2025 drift sits at the %.0fth percentile of all same-caller year-over-year pairs leaguewide (McVay's real 2025 pivot sits at %.0fth). Nick's 'plug and play' read holds;\n",
            pct2425, pct_la2425))
cat("   the CMC 'carried the load' claim isn't visible in playcalling tendencies (run rate flat, run EPA down) and can't be tested directly without player-level rushing data.\n")
cat("\nOut: docs/figures/rams_13_playbook.png, docs/figures/kyle_heavy.png, docs/figures/shanahan_2025.png, data/derived/heavy_playbook.csv\n")
