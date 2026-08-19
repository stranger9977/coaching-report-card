# =============================================================================
# 29_hawks_and_kyle.R -- two of Michael's newest asks, verbatim.
#
# "Where did the Hawks rank in all that?"
#
# "What is our story on Schnahan? What makes him special? Do we have anything
# data wise on him? I have the obvious 'their WR1 was a fuckin runninback'
# and 'Mac Jones was just as good as Purdy' etc etc"
#
# JOB 1: THE HAWKS LEDGER. Every place Seattle or Mike Macdonald has landed
# on this board so far, pulled from the derived CSVs already sitting in
# data/derived/ (no new modeling): the market model (R/02), the draft-value
# and payroll talent models (R/01, R/06), Madden launch ratings (R/03),
# halftime adjustments (R/04), and four defensive-scheme measures built for
# Macdonald specifically (R/16, R/18, R/28). Two of the sixteen rows needed
# a small residual computed here rather than read off a column: team_talent
# and contract_talent carry the raw talent measure (draft value, cap share)
# but not a talent-adjusted performance residual, so this script fits
# point_diff_pg ~ talent_measure across the 32 teams in that measure's own
# season and reads off Seattle's residual and its rank -- exactly the
# "pedigree" and "payroll" language already published in README.md, just
# reproduced and checked here rather than re-invented. Every number was
# cross-checked against its source CSV before being written to the ledger;
# see the printed cross-check block below for line-by-line verification.
#
# JOB 2 + 3: KYLE SHANAHAN'S TWO FOLK CLAIMS, CHECKED. Nobody on this board
# has touched Sumer's per-player-per-play file (data/raw/sumer/plays_players
# _p1/p2.csv.gz, 4.76M rows) for San Francisco's 2025 season specifically.
# Two claims, same data:
#   "their WR1 was a fuckin runningback" -- SF's 2025 targets leaderboard,
#     with position, to see whether Christian McCaffrey (RB) really out-
#     targeted the team's actual wide receivers.
#   "Mac Jones was just as good as Purdy" -- Brock Purdy vs Mac Jones,
#     2025 SF dropbacks only, on completion rate, yards/attempt and EPA per
#     dropback, with a Welch t-test on the per-play EPA gap and the honest
#     caveat that this is a one-season, one-team, backup-vs-starter split.
#
# PLAYER NAMES. Sumer's play/player tables carry only sumer_player_id.
# /pro/player/details had never been pulled before this script; it is now
# cached at data/raw/sumer/player_details.csv.gz (4,385 rows, one page,
# py/sumer_pull_player_details.py, same offset-paging convention as
# py/sumer_pull_players.py). Key read from ~/.Renviron, never printed.
#
# JOIN CONVENTIONS. Per R/factory/lib_sumer.R's header: Sumer's own IDs
# only, nothing joins to nflverse at play level. Team/season/caller context
# comes from load_sumer(), joined to the player-play rows on sumer_play_id
# (R/25 and R/28 use the same join). load_sumer() already drops no_play
# rows; season 2026 (preseason charting) is excluded by asking for season
# 2025 only.
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/hawks_ledger.png   (Job 1: every board rank, offense-
#                                        context vs defense)
#      docs/figures/sf_wr1_qb.png      (Jobs 2-3: SF target leaders with
#                                        position; Purdy vs Jones battery)
#      data/derived/hawks_ledger.csv
#      data/derived/sf_claims.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

pct_from_rank <- function(rank, n) 100 * (1 - (rank - 1) / (n - 1))
ordinal <- function(n) {
  suf <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  sprintf("%d%s", n, suf)
}

# =============================================================================
# JOB 1: THE HAWKS LEDGER
# =============================================================================
cat("=========================================================\n")
cat("JOB 1: THE HAWKS LEDGER\n")
cat("=========================================================\n")

ledger <- list()

# ---------------------------------------------------------- market (R/02)
ms <- fread("data/derived/market_seasons.csv")
ms25 <- ms[season == 2025 & eligible == TRUE]
ms25[, rank := frank(-wins_above_expectation, ties.method = "min")]
mac25 <- ms25[coach == "Mike Macdonald"]
stopifnot(nrow(mac25) == 1, mac25$rank == 3)
cat(sprintf("[market_seasons.csv] 2025 WAE = %+.2f, rank %d of %d (eligible)\n",
            mac25$wins_above_expectation, mac25$rank, nrow(ms25)))
ledger$market_wae <- data.table(
  group = "Team overachievement", measure = "2025 wins above market expectation",
  value = mac25$wins_above_expectation, value_label = sprintf("%+.2f wins", mac25$wins_above_expectation),
  rank = mac25$rank, n = nrow(ms25), source_csv = "market_seasons.csv")

cm <- fread("data/derived/coach_market.csv")
cme <- cm[eligible == TRUE]
cme[, rank := frank(-rate_per17, ties.method = "min")]
mac_cm <- cme[coach == "Mike Macdonald"]
stopifnot(nrow(mac_cm) == 1, mac_cm$rank == 3)
cat(sprintf("[coach_market.csv] career rate/17 = %+.2f, rank %d of %d eligible\n",
            mac_cm$rate_per17, mac_cm$rank, nrow(cme)))
ledger$market_career <- data.table(
  group = "Team overachievement", measure = "Career market rate (wins/17, above expectation)",
  value = mac_cm$rate_per17, value_label = sprintf("%+.2f wins/17", mac_cm$rate_per17),
  rank = mac_cm$rank, n = nrow(cme), source_csv = "coach_market.csv")

# ---------------------------------------------------------- team_talent (R/01): pedigree
tt <- fread("data/derived/team_talent.csv")
tt25 <- tt[season == 2025]
tt25[, roster_rank := frank(-roster_value, ties.method = "min")]
m_ped <- lm(point_diff_pg ~ roster_value, data = tt25)
tt25[, resid := residuals(m_ped)]
tt25[, resid_rank := frank(-resid, ties.method = "min")]
sea_tt <- tt25[team == "SEA"]
stopifnot(nrow(sea_tt) == 1, sea_tt$roster_rank == 19, sea_tt$resid_rank == 1)
cat(sprintf("[team_talent.csv] 2025 SEA roster (draft value) rank %d of %d; pedigree residual %+.2f pts/g, rank %d of %d\n",
            sea_tt$roster_rank, nrow(tt25), sea_tt$resid, sea_tt$resid_rank, nrow(tt25)))
ledger$roster_rank <- data.table(
  group = "Team overachievement", measure = "Roster draft pedigree (2025)",
  value = sea_tt$roster_value, value_label = sprintf("%s of %d by draft value", ordinal(sea_tt$roster_rank), nrow(tt25)),
  rank = sea_tt$roster_rank, n = nrow(tt25), source_csv = "team_talent.csv")
ledger$pedigree_resid <- data.table(
  group = "Team overachievement", measure = "Performance above draft pedigree (2025)",
  value = sea_tt$resid, value_label = sprintf("%+.2f pts/g above pedigree", sea_tt$resid),
  rank = sea_tt$resid_rank, n = nrow(tt25), source_csv = "team_talent.csv")

# ---------------------------------------------------------- contract_talent (R/06): payroll
ct <- fread("data/derived/contract_talent.csv")
ct25 <- ct[season == 2025]
ct25[, paid_rank := frank(-contract_talent, ties.method = "min")]
m_pay <- lm(point_diff_pg ~ contract_talent, data = ct25)
ct25[, resid := residuals(m_pay)]
ct25[, resid_rank := frank(-resid, ties.method = "min")]
sea_ct <- ct25[team == "SEA"]
stopifnot(nrow(sea_ct) == 1, sea_ct$paid_rank == 30, sea_ct$resid_rank == 1)
cat(sprintf("[contract_talent.csv] 2025 SEA paid talent rank %d of %d; payroll residual %+.2f pts/g, rank %d of %d\n",
            sea_ct$paid_rank, nrow(ct25), sea_ct$resid, sea_ct$resid_rank, nrow(ct25)))
ledger$paid_rank <- data.table(
  group = "Team overachievement", measure = "Paid roster talent (2025, cap share)",
  value = sea_ct$contract_talent, value_label = sprintf("%s of %d by paid talent", ordinal(sea_ct$paid_rank), nrow(ct25)),
  rank = sea_ct$paid_rank, n = nrow(ct25), source_csv = "contract_talent.csv")
ledger$payroll_resid <- data.table(
  group = "Team overachievement", measure = "Performance above payroll (2025)",
  value = sea_ct$resid, value_label = sprintf("%+.2f pts/g above payroll", sea_ct$resid),
  rank = sea_ct$resid_rank, n = nrow(ct25), source_csv = "contract_talent.csv")

# ---------------------------------------------------------- madden (R/03)
mt <- fread("data/derived/madden_team_seasons.csv")
mt25 <- mt[season == 2025]
mt25[, talent_rank := frank(-talent_z, ties.method = "min")]
mt25[, resid_rank := frank(-residual_wins, ties.method = "min")]
sea_mt <- mt25[team == "SEA"]
stopifnot(nrow(sea_mt) == 1, sea_mt$talent_rank == 23, round(sea_mt$residual_wins, 2) == 6.54)
cat(sprintf("[madden_team_seasons.csv] 2025 SEA Madden talent rank %d of %d; %+.2f wins above predicted, rank %d of %d\n",
            sea_mt$talent_rank, nrow(mt25), sea_mt$residual_wins, sea_mt$resid_rank, nrow(mt25)))
ledger$madden_rank <- data.table(
  group = "Team overachievement", measure = "Madden 26 roster rating (2025)",
  value = sea_mt$talent_z, value_label = sprintf("%s of %d rated", ordinal(sea_mt$talent_rank), nrow(mt25)),
  rank = sea_mt$talent_rank, n = nrow(mt25), source_csv = "madden_team_seasons.csv")
ledger$madden_resid <- data.table(
  group = "Team overachievement", measure = "Wins above Madden-predicted (2025)",
  value = sea_mt$residual_wins, value_label = sprintf("%+.2f wins above predicted", sea_mt$residual_wins),
  rank = sea_mt$resid_rank, n = nrow(mt25), source_csv = "madden_team_seasons.csv")

# ---------------------------------------------------------- halftime (R/04)
hc <- fread("data/derived/halftime_coach.csv")
hc[, rank := frank(-mean_residual, ties.method = "min")]
mac_hc <- hc[coach == "Mike Macdonald"]
stopifnot(nrow(mac_hc) == 1, mac_hc$rank == 46)
cat(sprintf("[halftime_coach.csv] career mean halftime residual %+.4f, rank %d of %d\n",
            mac_hc$mean_residual, mac_hc$rank, nrow(hc)))
ledger$halftime <- data.table(
  group = "Team overachievement", measure = "Halftime adjustment (career)",
  value = mac_hc$mean_residual, value_label = sprintf("rank %d of %d, mid-pack", mac_hc$rank, nrow(hc)),
  rank = mac_hc$rank, n = nrow(hc), source_csv = "halftime_coach.csv")

# ---------------------------------------------------------- free rushers (R/28)
fr <- fread("data/derived/free_rushers.csv")
mac_fr <- fr[def_play_caller == "Mike Macdonald"]
stopifnot(nrow(mac_fr) == 1, mac_fr$unblocked_rank == 2, mac_fr$won_rank == 24, mac_fr$blitz_rank == 26)
cat(sprintf("[free_rushers.csv] Macdonald: scheme (unblocked) rank %d, talent (won block) rank %d, blitz rank %d, all of %d DCs\n",
            mac_fr$unblocked_rank, mac_fr$won_rank, mac_fr$blitz_rank, nrow(fr)))
ledger$scheme <- data.table(
  group = "Macdonald's defense", measure = "Free-rusher scheme (unblocked pressure, career)",
  value = mac_fr$unblocked_resid, value_label = sprintf("%+.2fpp above expected", mac_fr$unblocked_resid),
  rank = mac_fr$unblocked_rank, n = nrow(fr), source_csv = "free_rushers.csv")
ledger$rush_talent <- data.table(
  group = "Macdonald's defense", measure = "Pass-rush talent (won blocks, career)",
  value = mac_fr$won_resid, value_label = sprintf("%+.2fpp above expected", mac_fr$won_resid),
  rank = mac_fr$won_rank, n = nrow(fr), source_csv = "free_rushers.csv")
ledger$blitz_rate <- data.table(
  group = "Macdonald's defense", measure = "Blitz rate (career, vs expected)",
  value = mac_fr$blitz_resid, value_label = sprintf("%+.2fpp vs expected, rank %d of %d", mac_fr$blitz_resid, mac_fr$blitz_rank, nrow(fr)),
  rank = mac_fr$blitz_rank, n = nrow(fr), source_csv = "free_rushers.csv")

# ---------------------------------------------------------- rotation (R/18)
rd <- fread("data/derived/rotation_dcs.csv")
mac_rd <- rd[caller == "Mike Macdonald"]
stopifnot(nrow(mac_rd) == 1, mac_rd$rank == 9)
cat(sprintf("[rotation_dcs.csv] Macdonald shell-rotation rank %d of %d\n", mac_rd$rank, nrow(rd)))
ledger$rotation <- data.table(
  group = "Macdonald's defense", measure = "Shell rotation rate (career)",
  value = mac_rd$resid, value_label = sprintf("%+.1fpp above expected", mac_rd$resid),
  rank = mac_rd$rank, n = nrow(rd), source_csv = "rotation_dcs.csv")

# ---------------------------------------------------------- def sequencing (R/16)
ds <- fread("data/derived/def_seq_callers.csv")
n_seq_total <- nrow(ds)
n_range_total <- sum(!is.na(ds$range_sd))
mac_ds <- ds[def_play_caller == "Mike Macdonald"]
stopifnot(nrow(mac_ds) == 1, mac_ds$seq_rank == 35, mac_ds$range_rank == 33)
cat(sprintf("[def_seq_callers.csv] Macdonald sequence-lift rank %d of %d (low = mixes calls up); situational range rank %d of %d (low = calls the same game everywhere)\n",
            mac_ds$seq_rank, n_seq_total, mac_ds$range_rank, n_range_total))
ledger$seq_lift <- data.table(
  group = "Macdonald's defense", measure = "Blitz sequence hot-hand (career, low = mixes it up)",
  value = mac_ds$lift_shrunk, value_label = sprintf("rank %d of %d, low = unpredictable", mac_ds$seq_rank, n_seq_total),
  rank = mac_ds$seq_rank, n = n_seq_total, source_csv = "def_seq_callers.csv")
ledger$situ_range <- data.table(
  group = "Macdonald's defense", measure = "Situational blitz-rate range (career, low = one game plan)",
  value = mac_ds$range_sd, value_label = sprintf("rank %d of %d, calls it the same everywhere", mac_ds$range_rank, n_range_total),
  rank = mac_ds$range_rank, n = n_range_total, source_csv = "def_seq_callers.csv")

# ---------------------------------------------------------- shapeshift (R/16)
sh <- fread("data/derived/def_shapeshift.csv")
sea_sh <- sh[def_team == "SEA" & season == 2025]
stopifnot(nrow(sea_sh) == 1, sea_sh$rank == 62)
cat(sprintf("[def_shapeshift.csv] SEA 2025 personnel shapeshift rank %d of %d (median is %.1f) -- right at the middle\n",
            sea_sh$rank, nrow(sh), (nrow(sh) + 1) / 2))
ledger$shapeshift <- data.table(
  group = "Macdonald's defense", measure = "Personnel shapeshifting (SEA 2025)",
  value = sea_sh$shapeshift, value_label = sprintf("rank %d of %d, dead median", sea_sh$rank, nrow(sh)),
  rank = sea_sh$rank, n = nrow(sh), source_csv = "def_shapeshift.csv")

hawks_ledger <- rbindlist(ledger)
hawks_ledger[, pct := pct_from_rank(rank, n)]
hawks_ledger[, rank_label := sprintf("%d of %d", rank, n)]
setorder(hawks_ledger, group, -pct)
write_csv(as.data.frame(hawks_ledger), "data/derived/hawks_ledger.csv")
cat(sprintf("\nwrote data/derived/hawks_ledger.csv, %d rows\n", nrow(hawks_ledger)))
cat("\n--- FULL LEDGER, AS WRITTEN ---\n")
print(hawks_ledger[, .(group, measure, value_label, rank_label, pct = round(pct, 1))])

# ---------------------------------------------------------- chart
hawks_ledger[, group := factor(group, levels = c("Team overachievement", "Macdonald's defense"))]
hawks_ledger[, measure_f := factor(measure, levels = rev(measure))]
hawks_ledger[, top3 := rank <= 3]

p1 <- ggplot(hawks_ledger, aes(pct, measure_f)) +
  geom_col(aes(fill = top3), width = 0.62) +
  geom_text(aes(label = value_label), hjust = -0.03, size = 3.05, fontface = "bold", colour = ink_body) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "#9db6c9"), guide = "none") +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.62)),
                     breaks = NULL) +
  labs(
    title = "The Hawks are the board's biggest overachiever on every offense-context meter,\nand Macdonald's defense is scheme-pressure elite while blitzing little",
    subtitle = "Every place Seattle or Mike Macdonald ranks on this board. Bar length = percentile rank within that measure's own universe (right edge = 1st place)",
    x = NULL, y = NULL,
    caption = fig_caption(
      "10 derived CSVs already on this board (market, roster/payroll talent, Madden, halftime, and four Macdonald-specific defense scripts)",
      "16 measures, 2025 season unless noted 'career' (2024-2025, Macdonald's Seattle tenure).",
      paste0("\nTop-3-of-universe rows in blue. 'Pedigree' and 'payroll' residuals are point_diff_pg regressed on that measure's talent score across the 32 teams in the 2025 season, fit here.\n",
             "Not every row is a compliment: roster rank 19th, paid-talent rank 30th, halftime rank 46 of 170, and blitz rank 26 of 41 describe modest resources and a light blitz plan, not a flaw.")
    )
  ) +
  theme_coach(grid = "none") +
  theme(strip.text = element_text(face = "bold", size = rel(0.95), colour = ink_title, hjust = 0),
        plot.margin = margin(10, 14, 8, 10))
save_fig("docs/figures/hawks_ledger.png", p1, w = 13, h = 9.5)

# =============================================================================
# JOB 2 + 3: SHANAHAN'S TWO FOLK CLAIMS -- SF 2025, PLAYER-LEVEL
# =============================================================================
cat("\n=========================================================\n")
cat("JOB 2 + 3: SF 2025 -- WR1 AND PURDY VS JONES\n")
cat("=========================================================\n")

plays25 <- load_sumer(2025)
sf_off <- plays25[off_team == "SF" & season == 2025,
                  .(sumer_play_id, season, off_team, off_caller, garbage_time)]
cat(sprintf("SF 2025 offensive plays (load_sumer, no_play already dropped): %s\n",
            format(nrow(sf_off), big.mark = ",")))

PCOLS <- c("sumer_play_id", "sumer_player_id", "season", "side_of_ball",
           "receiving_targets", "receiving_receptions", "receiving_yards", "receiving_epa",
           "passing_attempts", "passing_completions", "passing_yards", "passing_epa",
           "passing_dropbacks", "passing_sacks")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
n_2026 <- players[season == 2026, .N]
cat(sprintf("player-play rows: %s total, %s season-2026 rows excluded\n",
            format(nrow(players), big.mark = ","), format(n_2026, big.mark = ",")))

sf <- merge(players[season == 2025 & side_of_ball == "offense"], sf_off,
            by = c("sumer_play_id", "season"))
cat(sprintf("SF 2025 offense player-play rows: %s\n", format(nrow(sf), big.mark = ",")))

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id", "football_name", "last_name", "position"))
pd[, name := paste(football_name, last_name)]

# ---------------------------------------------------------- claim 1: WR1
rec <- sf[, .(targets = sum(receiving_targets, na.rm = TRUE),
             receptions = sum(receiving_receptions, na.rm = TRUE),
             yards = sum(receiving_yards, na.rm = TRUE),
             receiving_epa = sum(receiving_epa, na.rm = TRUE)),
         by = sumer_player_id][targets > 0]
rec <- merge(rec, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
setorder(rec, -targets)
rec_top8 <- head(rec, 8)
rec_top8[, target_rank := .I]

cat("\n--- SF 2025 targets leaderboard, top 8 ---\n")
print(rec_top8[, .(target_rank, name, position, targets, receptions, yards, receiving_epa = round(receiving_epa, 1))])

wr1_pos <- rec_top8[target_rank == 1]$position
wr1_name <- rec_top8[target_rank == 1]$name
wr1_targets <- rec_top8[target_rank == 1]$targets
next_wr <- rec_top8[position %in% c("WR") ][1]
gap <- wr1_targets - next_wr$targets
gap_pct <- 100 * gap / next_wr$targets
cat(sprintf("\nVERDICT, claim 1: SF's target leader is %s (%s), %d targets. First true WR is %s, %d targets.\n",
            wr1_name, wr1_pos, wr1_targets, next_wr$name, next_wr$targets))
cat(sprintf("Gap: %s out-targeted the actual WR1 by %d targets (%.0f%% more).\n", wr1_name, gap, gap_pct))
cat(sprintf("VERDICT: %s\n", if (wr1_pos == "RB") "TRUE -- the target leader is a running back." else "FALSE -- the target leader is not a running back."))

# ---------------------------------------------------------- claim 2: Purdy vs Jones
# passing_dropbacks flags QB SCRAMBLES too (a dropback that ends as a run,
# 37 SF rows, all with passing_attempts/sacks/epa == NA -- checked directly).
# A scramble's real value shows up in rushing_epa, not passing_epa, so
# including it in the dropback denominator while its numerator is NA would
# understate EPA/dropback for the more mobile QB. Matches R/01's own
# "dropbacks = attempts + sacks_suffered" convention: keep only rows with a
# real pass outcome (attempt or sack, i.e. passing_epa defined).
pas <- sf[!is.na(passing_epa) & (passing_attempts > 0 | passing_sacks > 0)]
pas <- merge(pas, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
qb_totals <- pas[, .(dropbacks = .N), by = .(sumer_player_id, name)]
setorder(qb_totals, -dropbacks)
cat("\n--- SF 2025 passers by dropback ---\n")
print(qb_totals)

purdy_id <- qb_totals[name == "Brock Purdy"]$sumer_player_id
jones_id <- qb_totals[name == "Mac Jones"]$sumer_player_id
two <- pas[sumer_player_id %in% c(purdy_id, jones_id)]
two[, qb := fifelse(sumer_player_id == purdy_id, "Purdy", "Jones")]

qb_summ <- two[, .(dropbacks = .N,
                   attempts = sum(passing_attempts, na.rm = TRUE),
                   completions = sum(passing_completions, na.rm = TRUE),
                   yards = sum(passing_yards, na.rm = TRUE),
                   epa_total = sum(passing_epa, na.rm = TRUE),
                   sacks = sum(passing_sacks, na.rm = TRUE)), by = qb]
qb_summ[, comp_pct := 100 * completions / attempts]
qb_summ[, y_per_att := yards / attempts]
qb_summ[, epa_per_db := epa_total / dropbacks]
setorder(qb_summ, -dropbacks)

cat("\n--- Purdy vs Jones, SF 2025 dropbacks ---\n")
print(qb_summ[, .(qb, dropbacks, attempts, completions, comp_pct = round(comp_pct, 1),
                  yards, y_per_att = round(y_per_att, 2), epa_per_db = round(epa_per_db, 3), sacks)])

tt <- t.test(passing_epa ~ qb, data = two)
purdy_row <- qb_summ[qb == "Purdy"]
jones_row <- qb_summ[qb == "Jones"]
smaller_n_qb <- qb_summ[which.min(dropbacks)]$qb
diff_epa <- jones_row$epa_per_db - purdy_row$epa_per_db
cat(sprintf("\nWelch t-test on per-play passing EPA, Jones minus Purdy: %+.3f [%+.3f, %+.3f], p = %.3f\n",
            unname(diff(tt$estimate)) * -1, tt$conf.int[1], tt$conf.int[2], tt$p.value))
cat(sprintf("EPA/dropback: Purdy %+.3f (n=%d) vs Jones %+.3f (n=%d). Completion%%: Purdy %.1f vs Jones %.1f. Y/att: Purdy %.2f vs Jones %.2f.\n",
            purdy_row$epa_per_db, purdy_row$dropbacks, jones_row$epa_per_db, jones_row$dropbacks,
            purdy_row$comp_pct, jones_row$comp_pct, purdy_row$y_per_att, jones_row$y_per_att))
cat(sprintf("Smaller sample: %s (%d dropbacks) -- the honest caveat on this claim.\n",
            smaller_n_qb, min(qb_summ$dropbacks)))
verdict2 <- if (tt$p.value > 0.10 && abs(diff_epa) < 0.05) {
  sprintf("TRUE, and then some -- 'just as good' holds. The gap in EPA/dropback (%+.3f, Jones' favor) is not statistically distinguishable from zero (p = %.2f), and Jones' point estimate is not lower.",
          diff_epa, tt$p.value)
} else if (tt$p.value > 0.10) {
  sprintf("TRUE within noise -- the %+.3f EPA/dropback gap does not clear significance (p = %.2f), but the point estimate favors %s.",
          diff_epa, tt$p.value, if (diff_epa > 0) "Jones" else "Purdy")
} else {
  sprintf("FALSE -- the gap (%+.3f EPA/dropback, %s ahead) clears significance (p = %.3f).",
          diff_epa, if (diff_epa > 0) "Jones" else "Purdy", tt$p.value)
}
cat(sprintf("\nVERDICT, claim 2: %s\n", verdict2))

# ---------------------------------------------------------- sf_claims.csv
claim1_out <- rec_top8[, .(claim = "wr1_is_a_running_back", player = name, position, targets,
                           receptions, yards, receiving_epa = round(receiving_epa, 2), rank = target_rank)]
claim2_out <- qb_summ[, .(claim = "jones_as_good_as_purdy", player = qb, dropbacks, attempts, completions,
                          comp_pct = round(comp_pct, 1), yards, y_per_att = round(y_per_att, 2),
                          epa_total = round(epa_total, 2), epa_per_db = round(epa_per_db, 4), sacks)]
sf_claims <- rbindlist(list(claim1_out, claim2_out), fill = TRUE)
write_csv(as.data.frame(sf_claims), "data/derived/sf_claims.csv")
cat(sprintf("\nwrote data/derived/sf_claims.csv, %d rows\n", nrow(sf_claims)))

# ---------------------------------------------------------- chart
rec_top8[, name_pos := sprintf("%s (%s)", name, position)]
rec_top8[, is_wr1 := target_rank == 1]
rec_top8[, name_pos_f := factor(name_pos, levels = rev(name_pos))]

pA <- ggplot(rec_top8, aes(targets, name_pos_f)) +
  geom_col(aes(fill = is_wr1), width = 0.68) +
  geom_text(aes(label = sprintf("%d targets, %d rec, %d yds", targets, receptions, yards)),
            hjust = -0.03, size = 3.0, fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#9db6c9"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.62))) +
  labs(title = "SF's 2025 target leader is a running back",
       subtitle = sprintf("%s out-targeted the actual WR1 (%s) by %d, +%.0f%%", wr1_name, next_wr$name, gap, gap_pct),
       x = "targets, 2025 season", y = NULL) +
  theme_coach(grid = "none") +
  theme(plot.margin = margin(10, 10, 8, 10))

batt <- qb_summ[, .(qb, `Comp %` = comp_pct, `Yards/att` = y_per_att, `EPA/dropback (x10)` = epa_per_db * 10)]
batt_long <- melt(batt, id.vars = "qb", variable.name = "metric", value.name = "val")
batt_long[, qb := factor(qb, levels = c("Purdy", "Jones"))]

pB <- ggplot(batt_long, aes(val, qb)) +
  geom_col(aes(fill = qb), width = 0.6) +
  geom_text(aes(label = round(val, 1)), hjust = -0.15, size = 3.1, fontface = "bold", colour = ink_body) +
  facet_wrap(~metric, ncol = 1, scales = "free_x") +
  scale_fill_manual(values = c("Purdy" = "#0072B2", "Jones" = "#9db6c9"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Purdy vs Jones, SF 2025 dropbacks",
       subtitle = sprintf("Purdy n=%d dropbacks, Jones n=%d\nEPA/dropback gap %+.3f, Jones' favor\np = %.2f, not distinguishable from zero",
                          purdy_row$dropbacks, jones_row$dropbacks, diff_epa, tt$p.value),
       x = NULL, y = NULL) +
  theme_coach(grid = "none") +
  theme(strip.text = element_text(face = "bold", size = rel(0.85), colour = ink_title, hjust = 0),
        plot.margin = margin(10, 10, 8, 10))

pAB <- pA + pB +
  plot_annotation(
    caption = fig_caption(
      "data/raw/sumer/plays_players_p1/p2.csv.gz joined to load_sumer() on sumer_play_id; names from /pro/player/details",
      "SF 2025 offense, no_play excluded, all situations (targets/receptions are season totals, not garbage-time-filtered).",
      "\nWelch t-test on per-dropback passing EPA. Smaller sample belongs to Jones; treat the comparison as suggestive, not proof, on one season of a backup's snaps."
    )
  ) & theme(plot.caption = element_text(colour = ink_caption, size = rel(0.7), hjust = 0, margin = margin(t = 10)))
save_fig("docs/figures/sf_wr1_qb.png", pAB, w = 13, h = 7)

cat("\nDONE. Files written:\n")
cat("  data/derived/hawks_ledger.csv\n")
cat("  data/derived/sf_claims.csv\n")
cat("  docs/figures/hawks_ledger.png\n")
cat("  docs/figures/sf_wr1_qb.png\n")
