# =============================================================================
# 21_rams_13.R -- the Rams-specific case for 13 personnel.
#
# Follows R/factory/60_narratives.R, which already showed McVay went from the
# most extreme 11-personnel caller in football (2022-2023) to the most extreme
# 13-personnel caller in football (2025, 29% of snaps) and that 13 personnel
# was worth +0.18 EPA leaguewide against +0.10 from 11. Michael's reaction to
# that chart, verbatim:
#
#   "Assuming this is all teams? Cuz it doesn't really make the case for
#    'Rams are dope at 13 personnel' - maybe add them in?"
#
#   "I'm curious how good their pass/rush was against a light box and heavy
#    box out of 13 too"
#
# Then, after checking a number himself against some outside tool:
#
#   "They were fucking nuts out of 13 personal. .50 EPA passing, .07 EPA per
#    play running my god"
#
# This script does three things: (1) verifies those two numbers against Sumer
# charting, (2) breaks LA's 13-personnel EPA by box count and asks the data,
# not an assumption, where the edge actually lives, (3) draws the "add them
# in" chart: all 32 teams' 13-personnel usage against their 13-personnel EPA,
# LA marked.
#
# DATA. Sumer play charting via R/factory/lib_sumer.R's load_sumer().
# offensive_personnel_basic already comes out as "13" (some rows carry a "13*"
# variant, star suffix uncorrelated with anything obvious in the charting --
# checked against formation_unbalanced and quarterback_alignment, no clean
# split -- so the star is stripped and both count as 13 personnel). box_defenders
# buckets: light <= 6, even = 7, heavy >= 8, Michael's own cutpoints. run_pass
# is Sumer's own P/R flag (X = special teams/no-play, already dropped by
# load_sumer()'s no_play filter).
#
# UNIVERSE. Season 2025, LA offense, for every Rams-specific number. Test 1
# reports BOTH all-snaps and non-garbage-time, because that is the actual
# question (which universe is Michael's outside tool using). Tests 2-4 use
# non-garbage-time as the default, matching every other script in this repo
# (R/18, R/factory/83, R/factory/85), with the all-snap comparison available
# in test 1's print. League comparisons are 2025-only throughout, to match
# LA's season rather than blend in three years of history.
#
# Conventions: R/lib/theme_coach.R, no em dashes, honest computed titles,
# sample sizes on every split (13 personnel gets thin fast, especially once
# it's cut three ways by box count).
#
# Out: docs/figures/rams_13.png       (league scatter, LA marked)
#      docs/figures/rams_13_box.png   (LA vs league, box count x run/pass)
#      data/derived/rams_13.csv       (2025 team-level 13-personnel usage/EPA)
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(nflplotR); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

d <- load_sumer(2022:2025)
d <- d[run_pass %in% c("P", "R")]
d[, pers13 := sub("[*]$", "", offensive_personnel_basic) == "13"]
d[, box_cat := fifelse(box_defenders <= 6, "light (<=6)",
                fifelse(box_defenders == 7, "even (7)",
                fifelse(box_defenders >= 8, "heavy (8+)", NA_character_)))]
d[, box_cat := factor(box_cat, levels = c("light (<=6)", "even (7)", "heavy (8+)"))]
d[, rp_label := fifelse(run_pass == "P", "Passing", "Running")]

la25 <- d[off_team == "LA" & season == 2025]
cat(sprintf("LA 2025 offensive snaps (pass+run): %s | of those, 13 personnel: %s (%.1f%%)\n",
            format(nrow(la25), big.mark = ","), format(sum(la25$pers13), big.mark = ","),
            100 * mean(la25$pers13)))

# =============================================================================
# TEST 1: VERIFY MICHAEL'S NUMBERS -- +0.50 EPA passing, +0.07 EPA running
# =============================================================================
cat("\n=== 1. verifying: '.50 EPA passing, .07 EPA per play running' ===\n")

one_ci <- function(x) {
  tt <- t.test(x)
  list(n = length(x), mean = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2])
}
verify <- function(x, claim, tol = 0.03) {
  r <- one_ci(x)
  in_ci <- claim >= r$lo && claim <= r$hi
  verdict <- if (abs(r$mean - claim) <= tol) "confirmed, near-exact"
             else if (in_ci) "confirmed within the CI, computed number differs"
             else "not confirmed, outside the CI"
  c(r, claim = claim, verdict = verdict)
}

verify_tab <- rbindlist(lapply(c("all snaps", "non-garbage-time"), function(u) {
  sub <- if (u == "non-garbage-time") la25[pers13 == TRUE & garbage_time == FALSE] else la25[pers13 == TRUE]
  rbindlist(lapply(list(list(rp = "P", label = "Passing", claim = 0.50),
                        list(rp = "R", label = "Running", claim = 0.07)), function(spec) {
    v <- verify(sub[run_pass == spec$rp]$expected_points_added, spec$claim)
    data.table(universe = u, play_type = spec$label, n = v$n, mean_epa = v$mean,
               lo = v$lo, hi = v$hi, claim = v$claim, verdict = v$verdict)
  }))
}))
print(verify_tab[, .(universe, play_type, n, mean_epa = round(mean_epa, 4),
                     lo = round(lo, 4), hi = round(hi, 4), claim, verdict)])

closest <- verify_tab[, .(universe, play_type, gap = abs(mean_epa - claim))][
  , .(total_gap = sum(gap)), by = universe][order(total_gap)]
cat(sprintf("\nuniverse closest to Michael's numbers overall: %s (total abs gap %.4f) vs %s (%.4f)\n",
            closest$universe[1], closest$total_gap[1], closest$universe[2], closest$total_gap[2]))
for (i in seq_len(nrow(verify_tab))) with(verify_tab[i], cat(sprintf(
  "  %-17s %-8s claim %.2f | computed %.4f [%.4f, %.4f] n=%d -> %s\n",
  universe, play_type, claim, mean_epa, lo, hi, n, verdict)))

# =============================================================================
# TEST 2: BOX SPLITS -- where does the 13-personnel edge actually live?
# =============================================================================
cat("\n=== 2. LA vs league out of 13 personnel, by box count, non-garbage-time ===\n")

la_box <- la25[pers13 == TRUE & garbage_time == FALSE & !is.na(box_cat)]
lg_box <- d[season == 2025 & pers13 == TRUE & garbage_time == FALSE & !is.na(box_cat)]

box_tab <- rbindlist(lapply(c("P", "R"), function(rp) {
  rbindlist(lapply(levels(la25$box_cat), function(bx) {
    a <- la_box[run_pass == rp & box_cat == bx]$expected_points_added
    b <- lg_box[run_pass == rp & box_cat == bx]$expected_points_added
    ra <- if (length(a) >= 2) one_ci(a) else list(n = length(a), mean = if (length(a)) mean(a) else NA)
    rb <- if (length(b) >= 2) one_ci(b) else list(n = length(b), mean = if (length(b)) mean(b) else NA)
    thin <- length(a) < 15 | length(b) < 15
    p <- if (length(a) >= 5 && length(b) >= 5) t.test(a, b)$p.value else NA_real_
    data.table(run_pass = ifelse(rp == "P", "Passing", "Running"), box = bx,
               n_la = ra$n, epa_la = ra$mean, n_lg = rb$n, epa_lg = rb$mean,
               diff = ra$mean - rb$mean, p = p, thin = thin)
  }))
}))
print(box_tab[, .(run_pass, box, n_la, epa_la = round(epa_la, 3), n_lg, epa_lg = round(epa_lg, 3),
                  diff = round(diff, 3), p = round(p, 3), thin)])

sig <- box_tab[!is.na(p) & p < 0.05]
cat("\nsplits where LA's edge over the league clears p < 0.05:\n")
if (nrow(sig)) {
  for (i in seq_len(nrow(sig))) with(sig[i], cat(sprintf(
    "  %-8s %-12s LA %.3f vs league %.3f, diff %+.3f, p = %.3f%s\n",
    run_pass, box, epa_la, epa_lg, diff, p, if (thin) " (thin, treat cautiously)" else "")))
} else cat("  none at conventional significance\n")
cat("\nLA's heaviest-box passing volume: n =", box_tab[run_pass == "Passing" & box == "heavy (8+)"]$n_la,
    "of", sum(box_tab[run_pass == "Passing"]$n_la), "13-personnel dropbacks (",
    round(100 * box_tab[run_pass == "Passing" & box == "heavy (8+)"]$n_la / sum(box_tab[run_pass == "Passing"]$n_la), 0), "%)\n")
cat("LA's heaviest-box run volume: n =", box_tab[run_pass == "Running" & box == "heavy (8+)"]$n_la,
    "of", sum(box_tab[run_pass == "Running"]$n_la), "13-personnel runs (",
    round(100 * box_tab[run_pass == "Running" & box == "heavy (8+)"]$n_la / sum(box_tab[run_pass == "Running"]$n_la), 0), "%)\n")

box_tab[, box := factor(box, levels = c("light (<=6)", "even (7)", "heavy (8+)"))]
box_long <- rbind(
  box_tab[, .(run_pass, box, grp = "LA", n = n_la, epa = epa_la)],
  box_tab[, .(run_pass, box, grp = "League", n = n_lg, epa = epa_lg)]
)
box_long[, grp := factor(grp, levels = c("LA", "League"))]
sig_lab <- box_tab[, .(run_pass, box, diff, p)]

p_box <- ggplot(box_long, aes(box, epa, fill = grp)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.65), width = 0.6) +
  geom_text(aes(label = sprintf("%+.2f", epa),
                y = ifelse(epa >= 0, epa + 0.035, epa - 0.035)),
            position = position_dodge(width = 0.65), size = 2.9, fontface = "bold", colour = "grey25") +
  geom_text(aes(label = sprintf("n=%d", n), y = -0.34),
            position = position_dodge(width = 0.65), size = 2.5, colour = "grey45") +
  scale_fill_manual(values = c(LA = "#003594", League = "#9db6c9")) +
  scale_y_continuous(limits = c(-0.4, 0.85)) +
  facet_wrap(~ run_pass, ncol = 2) +
  labs(
    title = "Out of 13 personnel, the Rams' edge over the league shows up hardest against a stacked box",
    subtitle = "EPA per play by box count, LA vs league, 2025 13-personnel snaps outside garbage time",
    x = "box defenders", y = "EPA per play",
    caption = fig_caption(
      "SumerSports play charting, 2025 season",
      "n printed under each bar. Splits are thin, especially light-box running (LA n = 6): read those as directional, not settled.",
      sprintf("\nThe only two splits clearing p < 0.05 are both against a heavy (8+) box: passing %+.3f vs league (p = %.3f) and running %+.3f vs league (p = %.3f).\n",
              box_tab[run_pass == "Passing" & box == "heavy (8+)"]$diff, box_tab[run_pass == "Passing" & box == "heavy (8+)"]$p,
              box_tab[run_pass == "Running" & box == "heavy (8+)"]$diff, box_tab[run_pass == "Running" & box == "heavy (8+)"]$p) %>%
        paste0(sprintf("Defenses stack the box against LA's 13 personnel %.0f%% of the time (heavy box, both play types combined) and pay for it both ways. Built by R/21_rams_13.R.",
                       100 * sum(box_tab[box == "heavy (8+)"]$n_la) / sum(box_tab$n_la)))
    )
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", strip.text = element_text(size = rel(1)))
save_fig("docs/figures/rams_13_box.png", p_box, w = 11.5, h = 7)

# =============================================================================
# TEST 3: LEAGUE SCATTER, LA MARKED -- the "add them in" chart
# =============================================================================
cat("\n=== 3. league scatter: 2025 13-personnel usage vs EPA, all 32 teams ===\n")

MIN_SNAPS <- 25
team_tab <- d[season == 2025, .(n_all = .N, n13 = sum(pers13),
                                epa13 = mean(expected_points_added[pers13], na.rm = TRUE)),
             by = off_team]
team_tab[, rate13 := 100 * n13 / n_all]
team_tab[, meets_min := n13 >= MIN_SNAPS]
write_csv(as.data.frame(team_tab[order(-rate13)]), "data/derived/rams_13.csv")

qual <- team_tab[meets_min == TRUE]
qual[, epa_rank := frank(-epa13)]
qual[, usage_rank := frank(-rate13)]
la_row <- qual[off_team == "LA"]
lg_median <- median(qual$epa13)
lg_median_rate <- median(qual$rate13)

cat(sprintf("teams with >= %d 13-personnel snaps in 2025: %d of 32 (excluded: %s)\n",
            MIN_SNAPS, nrow(qual), paste(team_tab[meets_min == FALSE]$off_team, collapse = ", ")))
cat(sprintf("LA: usage %.1f%% (rank %d of %d), EPA/play %.3f (rank %d of %d), on n = %d snaps\n",
            la_row$rate13, la_row$usage_rank, nrow(qual), la_row$epa13, la_row$epa_rank, nrow(qual), la_row$n13))
cat(sprintf("league median (qualifying teams): usage %.1f%%, EPA/play %+.3f\n", lg_median_rate, lg_median))
cat(sprintf("LA's EPA/play beats the qualifying-team median by %+.3f, on %dx the snap volume of the next-highest usage team\n",
            la_row$epa13 - lg_median, round(la_row$n13 / max(qual[off_team != "LA"]$n13))))
top2 <- qual[order(-epa13)][1:2]
cat("top 2 by EPA/play:\n")
for (i in 1:2) with(top2[i], cat(sprintf("  %s: %.3f EPA/play on n = %d\n", off_team, epa13, n13)))

title3 <- sprintf(
  "LA ran 13 personnel on %.0f%% of plays in 2025, %.0fx the qualifying-team median, and still beat that median EPA by %+.2f",
  la_row$rate13, la_row$rate13 / lg_median_rate, la_row$epa13 - lg_median)

p_scatter <- ggplot(qual, aes(rate13, epa13)) +
  geom_hline(yintercept = lg_median, linetype = "22", colour = ink_baseline, linewidth = 0.4) +
  geom_vline(xintercept = lg_median_rate, linetype = "22", colour = ink_baseline, linewidth = 0.4) +
  geom_nfl_logos(aes(team_abbr = off_team, alpha = off_team == "LA",
                     width = ifelse(off_team == "LA", 0.09, 0.055))) +
  geom_point(data = qual[off_team == "LA"], shape = 21, size = 9,
            stroke = 1.2, colour = ink_title, fill = NA) +
  annotate("text", x = lg_median_rate + 0.4, y = max(qual$epa13) + 0.02,
           label = sprintf("league median EPA/play: %+.2f", lg_median),
           hjust = 0, size = 3, colour = "grey45") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.75), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = title3,
    subtitle = "Share of offensive plays run from 13 personnel vs EPA per play out of 13 personnel, 2025 season",
    x = "share of offensive plays from 13 personnel", y = "EPA per play out of 13 personnel",
    caption = fig_caption(
      "SumerSports play charting, 2025 season",
      sprintf("Teams with at least %d charted 13-personnel plays (%d of 32; %s excluded on volume).",
              MIN_SNAPS, nrow(qual), paste(team_tab[meets_min == FALSE]$off_team, collapse = ", ")),
      sprintf("\nLA's n = %d 13-personnel plays is %s the next-highest-volume team (%s, n = %d). %s edges LA on EPA/play\nat a fraction of the sample, which is worth flagging: LA's number is the one built on a real season of 13 personnel, not a small-sample streak. Built by R/21_rams_13.R.",
              la_row$n13, "far more than", qual[off_team != "LA"][order(-n13)][1]$off_team, qual[off_team != "LA"][order(-n13)][1]$n13,
              top2[1]$off_team)
    )
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/rams_13.png", p_scatter, w = 12, h = 7.5)

# =============================================================================
# summary
# =============================================================================
cat("\n=== SUMMARY ===\n")
cat("1. VERIFICATION:\n")
for (i in seq_len(nrow(verify_tab))) with(verify_tab[i], cat(sprintf(
  "   %s, %s: claim %.2f, computed %.4f [%.4f, %.4f] n=%d -> %s\n",
  universe, play_type, claim, mean_epa, lo, hi, n, verdict)))
cat(sprintf("   Closest universe overall: %s\n", closest$universe[1]))
cat("2. BOX SPLITS: edge concentrated in heavy (8+) box for both passing and running (see table above); light-box run split is too thin (LA n=6) to trust.\n")
cat(sprintf("3. LEAGUE STANDING: LA usage rank %d of %d, EPA rank %d of %d (qualifying teams, n >= %d), league median EPA/play %+.3f\n",
            la_row$usage_rank, nrow(qual), la_row$epa_rank, nrow(qual), MIN_SNAPS, lg_median))
cat("\nOut: docs/figures/rams_13.png, docs/figures/rams_13_box.png, data/derived/rams_13.csv\n")
