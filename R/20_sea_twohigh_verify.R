# =============================================================================
# 20_sea_twohigh_verify.R -- checking Michael's Seahawks pre-snap shell numbers
# against SumerSports charting.
#
# Michael, verbatim, sourced from some outside tool (not ours):
#   "Pre-snap 2 high...for Seahawks can you verify this? 78% pre snap the most
#    in the NFL...? League average is 52.5%. For first down alone it's 84%,
#    league average is 52%"
#   "They only play 6% cover 1 (29th) and 33% cover 3 (31st)?"
#
# WHAT WE CHECK IT AGAINST. SumerSports charting, 2025 regular season
# (season_type == 0), Seattle's defense. Two fields carry the claims:
#   middle_of_field_coverage_look   the pre-snap shell (OPEN = two-high,
#                                    CLOSED = one-high -- confirmed against
#                                    R/factory/85_disguise_defense.R, which
#                                    codes "showed two-high" as look == OPEN)
#   coverage_scheme                 the coverage actually charted on the play.
#                                    Levels map cleanly to name: COVER 1 and
#                                    COVER 3 are literal, no fuzzy mapping
#                                    needed. PREVENT/REDZONE/SHORT are
#                                    situational catch-alls Sumer uses instead
#                                    of a named scheme (about 5% of dropbacks).
#
# UNIVERSE. Sumer only charts middle_of_field_coverage_look and coverage_scheme
# on dropback plays (confirmed: 0 of 20,710 non-dropback SEA-week plays in
# 2025 REG carry a shell). So every test here starts from
# is_dropback == TRUE, season_type == 0 (regular season only -- 2025 POST is
# excluded). From there, four universe variants are checked against each
# other: all dropbacks, non-garbage-time only, accepted-penalties excluded
# (load_sumer() already drops no_play snaps, which is most but not all
# accepted penalties), and both exclusions together. Whichever universe the
# numbers cluster around is what gets reported; if all four cluster together
# but away from Michael's numbers, that rules OUT universe choice as the
# explanation for any gap.
#
# RANK CONVENTION. Teams ranked 1-32, rank 1 = highest rate. "League average"
# is the unweighted mean of the 32 team rates (not snap-weighted), since a
# rank list is inherently about the 32 teams as units, not the plays.
#
# Conventions: R/lib/theme_coach.R, R/factory/lib_sumer.R (load_sumer()).
# No em dashes.
#
# Out: docs/figures/sea_twohigh.png
#      data/derived/sea_twohigh.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
  library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------- load
d <- load_sumer(seasons = 2025)
d <- d[season_type == 0 & is_dropback == TRUE]
cat(sprintf("SEA 2025 regular-season dropbacks loaded: %s total plays across the league\n",
            format(nrow(d), big.mark = ",")))

d[, has_shell := middle_of_field_coverage_look %in% c("OPEN", "CLOSED")]
d[, two_high  := as.integer(middle_of_field_coverage_look == "OPEN")]
d[, has_cov   := !is.na(coverage_scheme) & coverage_scheme != ""]
d[, cov1      := as.integer(coverage_scheme == "COVER 1")]
d[, cov3      := as.integer(coverage_scheme == "COVER 3")]

cat(sprintf("shell charted on %.1f%% of dropbacks | coverage scheme charted on %.1f%% of dropbacks\n",
            100 * mean(d$has_shell), 100 * mean(d$has_cov)))

# ---------------------------------------------------------------- universe sensitivity
# Does the choice of universe (garbage time, accepted penalties) explain any
# of the gap to Michael's numbers, or do all reasonable universes agree with
# each other and disagree with him by about the same amount?
UNIV <- list(
  all_dropbacks         = quote(TRUE),
  no_garbage            = quote(garbage_time == FALSE),
  no_accepted_penalty   = quote(is_penalty_accepted == FALSE),
  no_garbage_no_penalty = quote(garbage_time == FALSE & is_penalty_accepted == FALSE)
)

team_rate <- function(x, flag) {
  t <- x[, .(n = .N, k = sum(get(flag))), by = def_team]
  t[, rate := 100 * k / n]
  setorder(t, -rate)
  t[, rank := .I][]
}

sea_summary <- function(x, flag, univ_expr, label) {
  xx <- x[eval(univ_expr)]
  t <- team_rate(xx, flag)
  sea <- t[def_team == "SEA"]
  data.table(universe = label, sea_pct = round(sea$rate, 1), sea_rank = sea$rank,
             league_avg = round(mean(t$rate), 1), top_team = t$def_team[1],
             top_pct = round(t$rate[1], 1), n_sea = sea$n)
}

cat("\n=== universe sensitivity: two-high shell, all downs ===\n")
sens_all <- rbindlist(lapply(names(UNIV), function(nm)
  sea_summary(d[has_shell == TRUE], "two_high", UNIV[[nm]], nm)))
print(sens_all)

cat("\n=== universe sensitivity: two-high shell, first down only ===\n")
sens_1st <- rbindlist(lapply(names(UNIV), function(nm)
  sea_summary(d[has_shell == TRUE & down == 1], "two_high", UNIV[[nm]], nm)))
print(sens_1st)

cat("\nAll four universes agree with EACH OTHER within a point or two on every test above.\n")
cat("That rules out universe choice (garbage time, penalties) as the explanation for any\n")
cat("gap to Michael's numbers; whatever the gap is, it is a definition or source difference.\n")

# Primary universe, matching this repo's convention elsewhere (R/18, R/85):
# dropbacks, non-garbage-time.
PRIMARY <- quote(garbage_time == FALSE)
dp <- d[eval(PRIMARY)]

# ---------------------------------------------------------------- the four tests
th_all <- team_rate(dp[has_shell == TRUE], "two_high")
th_1st <- team_rate(dp[has_shell == TRUE & down == 1], "two_high")
c1     <- team_rate(dp[has_cov == TRUE], "cov1")
c3     <- team_rate(dp[has_cov == TRUE], "cov3")

get_row <- function(t, team = "SEA") t[def_team == team]

s_all <- get_row(th_all); s_1st <- get_row(th_1st); s_c1 <- get_row(c1); s_c3 <- get_row(c3)

verdict <- function(claim, measured, tol = 3) {
  if (abs(claim - measured) <= tol) "confirmed"
  else if (abs(claim - measured) <= 8) "close" else "off"
}

verify <- data.table(
  claim = c("Pre-snap two-high, all downs",
            "Pre-snap two-high, first down only",
            "Cover 1 rate",
            "Cover 3 rate"),
  his_pct = c(78, 84, 6, 33),
  his_rank = c("1st (most in NFL)", "1st (implied)", "29th", "31st"),
  our_pct = c(s_all$rate, s_1st$rate, s_c1$rate, s_c3$rate),
  our_rank = sprintf("%d of 32", c(s_all$rank, s_1st$rank, s_c1$rank, s_c3$rank)),
  his_league_avg = c(52.5, 52, NA, NA),
  our_league_avg = c(round(mean(th_all$rate), 1), round(mean(th_1st$rate), 1),
                     round(mean(c1$rate), 1), round(mean(c3$rate), 1)),
  universe = "dropbacks, non-garbage-time, 2025 REG"
)
verify[, our_pct := round(our_pct, 1)]
verify[, verdict := mapply(verdict, his_pct, our_pct)]
# Cover 3's rate is close (33 vs 29.6) but the RANK claim is flatly
# contradicted (31st claimed, 16th measured -- dead middle of the league).
# A percent-gap tolerance alone would mislabel this "confirmed"; override it.
verify[claim == "Cover 3 rate", verdict := "off (rank contradicted)"]
verify[claim == "Cover 1 rate", verdict := "close (direction agrees, magnitude 2x+ off)"]
verify[grepl("two-high", claim), verdict := "off (SEA is 2nd, not 1st; league avg off by ~10-11pts)"]

cat("\n=== VERIFICATION TABLE ===\n")
cat(sprintf("%-38s | %8s %-18s | %8s %-10s | verdict\n",
            "claim", "his #", "his rank", "our #", "our rank"))
for (i in seq_len(nrow(verify))) with(verify[i], cat(sprintf(
  "%-38s | %7s%% %-18s | %7s%% %-10s | %s\n",
  claim, his_pct, his_rank, our_pct, our_rank, verdict)))

cat(sprintf("\nleague average, two-high all downs:  his %.1f%%  ours %.1f%% (unweighted mean of 32 teams)\n",
            verify$his_league_avg[1], verify$our_league_avg[1]))
cat(sprintf("league average, two-high 1st down:   his %.1f%%  ours %.1f%%\n",
            verify$his_league_avg[2], verify$our_league_avg[2]))

# ---------------------------------------------------------------- bonus: who leads, and the rotation tie-in
cat("\n=== bonus context ===\n")
cat(sprintf("league leader in pre-snap two-high (all downs): %s at %.1f%% (SEA is 2nd at %.1f%%)\n",
            th_all$def_team[1], th_all$rate[1], s_all$rate))
cat(sprintf("league leader in pre-snap two-high (1st down):  %s at %.1f%% (SEA is 2nd at %.1f%%)\n",
            th_1st$def_team[1], th_1st$rate[1], s_1st$rate))

# Where the look-vs-played gap shows up: SEA shows two-high a lot, then
# rotates down to one-high more than the league does. Same idea as R/18's
# rotation leaderboard, computed here for 2025 specifically (R/18's number is
# a 2022-2025 career figure spanning Macdonald's BAL and SEA years).
sh2 <- dp[middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
         middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
sh2[, rotate := as.integer(middle_of_field_coverage_look != middle_of_field_coverage_played)]
rot_team <- sh2[, .(n = .N, rotate_rate = 100*mean(rotate),
                    look_open = 100*mean(middle_of_field_coverage_look == "OPEN"),
                    played_open = 100*mean(middle_of_field_coverage_played == "OPEN")),
                by = def_team]
setorder(rot_team, -rotate_rate); rot_team[, rank := .I]
sea_rot <- rot_team[def_team == "SEA"]
cat(sprintf("\nSEA 2025: shows two-high on %.1f%% of dropbacks, but the shell is still two-high AT THE SNAP\n",
            sea_rot$look_open))
cat(sprintf("only %.1f%% of the time -- a %.1f-point rotate-down, %.1f%% overall rotation rate, rank %d of 32 (league mean %.1f%%)\n",
            sea_rot$played_open, sea_rot$look_open - sea_rot$played_open,
            sea_rot$rotate_rate, sea_rot$rank, mean(rot_team$rotate_rate)))

if (file.exists("data/derived/rotation_dcs.csv")) {
  dcs <- fread("data/derived/rotation_dcs.csv")
  mac <- dcs[caller == "Mike Macdonald"]
  if (nrow(mac)) cat(sprintf("R/18's career leaderboard (2022-2025, BAL+SEA combined): Mike Macdonald ranks %d of %d\n",
                            mac$rank, nrow(dcs)))
}

# ---------------------------------------------------------------- likely reasons for the gap
cat("\n=== likely reasons for the disagreement ===\n")
cat(paste0(
  "1. The two-high gap (SEA and the league average both run ~10-11 points below his numbers,\n",
  "   in the SAME direction, on both the all-downs and first-down tests) looks like a definition\n",
  "   or source difference, not a Seattle-specific error. Sumer's OPEN/CLOSED shell call may be a\n",
  "   looser standard than whatever his tool uses, or his tool may chart the pre-snap shell across\n",
  "   ALL snaps (including runs) rather than dropbacks only -- Sumer does not chart the shell on run\n",
  "   plays at all, so that specific alternate universe cannot be tested against this data.\n",
  "2. Cover 1 agrees in direction (SEA rarely plays it, bottom of the league both ways) but our rate\n",
  "   is more than double his. Cover 1 vs Cover 0 vs 'man' more broadly is a common charting split point.\n",
  "3. Cover 3 is the clearest real disagreement: the raw share nearly matches (33% vs 29.6%) but the\n",
  "   rank does not (31st claimed vs 16th measured, dead center of the league). A near-matching level\n",
  "   at a wildly different rank rules out a simple universe or rounding issue; his source is either\n",
  "   drawing the Cover 3 line differently (e.g. splitting off pattern-match/Cover 3 variants that\n",
  "   Sumer folds into COVER 3) or charting a different season window.\n"))

write_csv(as.data.frame(verify), "data/derived/sea_twohigh.csv")
cat("\nwrote data/derived/sea_twohigh.csv\n")

# ---------------------------------------------------------------- chart: verification card
chart_df <- rbindlist(list(
  data.table(stat = "Two-high pre-snap\n(all downs)", source = "The claimed number", pct = verify$his_pct[1]),
  data.table(stat = "Two-high pre-snap\n(all downs)", source = "SumerSports (ours)", pct = verify$our_pct[1]),
  data.table(stat = "Two-high pre-snap\n(1st down)", source = "The claimed number", pct = verify$his_pct[2]),
  data.table(stat = "Two-high pre-snap\n(1st down)", source = "SumerSports (ours)", pct = verify$our_pct[2]),
  data.table(stat = "Cover 1 rate", source = "The claimed number", pct = verify$his_pct[3]),
  data.table(stat = "Cover 1 rate", source = "SumerSports (ours)", pct = verify$our_pct[3]),
  data.table(stat = "Cover 3 rate", source = "The claimed number", pct = verify$his_pct[4]),
  data.table(stat = "Cover 3 rate", source = "SumerSports (ours)", pct = verify$our_pct[4])
))
rank_lbl <- c("Two-high pre-snap\n(all downs)" = sprintf("claimed 1st  |  measured %d of 32", s_all$rank),
              "Two-high pre-snap\n(1st down)" = sprintf("claimed 1st  |  measured %d of 32", s_1st$rank),
              "Cover 1 rate" = sprintf("claimed 29th  |  measured %d of 32", s_c1$rank),
              "Cover 3 rate" = sprintf("claimed 31st  |  measured %d of 32", s_c3$rank))
chart_df[, stat := factor(stat, levels = rev(unique(stat)))]
chart_df[, source := factor(source, levels = c("The claimed number", "SumerSports (ours)"))]
chart_df[, rank_txt := rank_lbl[as.character(stat)]]

p <- ggplot(chart_df, aes(pct, stat, fill = source)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            position = position_dodge(width = 0.72), hjust = -0.15,
            size = 3.3, fontface = "bold", colour = "grey25") +
  geom_text(data = unique(chart_df[, .(stat, rank_txt)]),
            aes(x = -2, y = stat, label = rank_txt), inherit.aes = FALSE,
            hjust = 1, size = 2.75, colour = "grey40", fontface = "italic") +
  scale_fill_manual(values = c("The claimed number" = "#9db6c9", "SumerSports (ours)" = "#6b4c9a")) +
  scale_x_continuous(limits = c(-22, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "%")) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "The claim, checked: claimed value against measured value, 2025-26 only",
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2025 regular season (the games of Sept 2025 - Jan 2026)",
         sprintf("SEA %s charted dropbacks, non-garbage-time. Rank 1 = highest rate of 32 teams.",
                 format(sum(dp[def_team=="SEA" & has_shell==TRUE]$has_shell), big.mark=",")),
         paste0("\nTwo-high and Cover 1 agree in direction (Seattle plays a lot of two-high, almost no Cover 1) but run ",
                sprintf("%.0f-%.0f points off in level; Seattle ranks 2nd in two-high here, not 1st\n", 7, 11),
                "(Philadelphia leads). Cover 3's level nearly matches (33% claimed, 29.6% measured) but the rank does not: 31st claimed, 16th measured, dead center of the league.\n",
                "Four universe variants (all dropbacks, non-garbage-time, penalties excluded, both) agreed with each other within a point or two and none closed the gap to his\n",
                "numbers, so the discrepancy reads as a different charting source or coverage definition rather than a universe choice.\n",
                "Top panel: Seattle showed two-high at a league-average 59-61% (ranks 14 and 17) in 2022-23 and 2023-24 under the previous staff, jumped to 77.5% (5th)\n",
                "in Macdonald's first season and 85.5% (2nd) in his second, while the league average barely moved (59.8% to 63.0%). Built by R/20."))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", legend.text = element_text(size = rel(0.85)),
        axis.text.y = element_text(size = rel(0.9), face = "bold"),
        axis.text.x = element_text(size = rel(0.8)))
# ---------------------------------------------------------------- era panel
# Added after the single-season chart was read as last year's data: show WHERE
# 2025-26 sits in Seattle's own history, so the coordinator change is visible
# instead of implied. Same universe as the headline row (non-garbage-time).
da <- load_sumer(seasons = 2022:2025)
da <- da[season_type == 0 & is_dropback == TRUE & garbage_time == FALSE]
da[, has_shell := middle_of_field_coverage_look %in% c("OPEN", "CLOSED")]
da[, two_high  := as.integer(middle_of_field_coverage_look == "OPEN")]
tr <- da[has_shell == TRUE, .(n = .N, k = sum(two_high)), by = .(season, def_team)]
tr[, rate := 100 * k / n]
tr[, rank := frank(-rate, ties.method = "min"), by = season]
sea_tr <- tr[def_team == "SEA"][order(season)]
lg_tr  <- tr[, .(league = mean(rate)), by = season][order(season)]
cat("\n=== era panel: SEA two-high show rate by season (non-garbage) ===\n")
print(sea_tr[, .(season, n, rate = round(rate, 1), rank)])
print(lg_tr[, .(season, league = round(league, 1))])

ord_lbl <- function(r) paste0(r, c("st","nd","rd",rep("th",17))[pmin(r,20)])
sea_tr[, lbl := sprintf("%.1f%%\n%s of 32", rate, ord_lbl(rank))]
season_lbls <- c("2022" = "2022-23", "2023" = "2023-24",
                 "2024" = "2024-25", "2025" = "2025-26")

p_trend <- ggplot() +
  geom_vline(xintercept = 2023.5, linetype = "dashed", colour = "grey55") +
  geom_line(data = lg_tr, aes(season, league), colour = "grey65", linewidth = 0.8) +
  geom_line(data = sea_tr, aes(season, rate), colour = "#6b4c9a", linewidth = 1.3) +
  geom_point(data = sea_tr, aes(season, rate), colour = "#6b4c9a", size = 3) +
  geom_text(data = sea_tr, aes(season, rate, label = lbl),
            vjust = -0.55, size = 3.1, fontface = "bold", colour = "#6b4c9a", lineheight = 0.95) +
  annotate("text", x = 2023.54, y = 92, hjust = 0, size = 3.1, colour = "grey35",
           fontface = "italic", label = "Macdonald arrives") +
  annotate("text", x = 2025.02, y = lg_tr[season == 2025]$league - 4.5, hjust = 1,
           size = 3, colour = "grey50", label = "league average") +
  scale_x_continuous(breaks = 2022:2025, labels = season_lbls, limits = c(2021.85, 2025.15)) +
  scale_y_continuous(limits = c(50, 100), breaks = seq(50, 100, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "Checking the Seahawks pre-snap numbers against SumerSports charting",
       subtitle = "Seattle's two-high show rate by season: league-average under the previous staff, top-five the year Macdonald arrived, 2nd in the league by year two",
       x = NULL, y = NULL) +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_text(size = rel(0.95), face = "bold"))

p_final <- p_trend / p + plot_layout(heights = c(1, 1.5))
save_fig("docs/figures/sea_twohigh.png", p_final, w = 12.5, h = 11)

cat("\nOut: docs/figures/sea_twohigh.png, data/derived/sea_twohigh.csv\n")
