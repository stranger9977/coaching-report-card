# =============================================================================
# 28_free_rushers.R -- pressure, decomposed: scheme vs talent, for defensive
# coordinators.
#
# THE PUZZLE. R/16 found Mike Macdonald blitzes below the league rate in
# every one of six game situations, yet his defenses have finished 2nd in
# the league in each of his two years as Seattle's de facto DC (2024-2025),
# after running Baltimore's top-ranked defense in 2022-2023. A defense that
# rushes fewer than four extra men and still wrecks the pocket is either
# unusually talented up front, or unusually good at getting free rushers to
# the QB without sending them -- SCHEMED pressure, not SENT pressure. This
# script tests that directly using three Sumer fields nobody on this board
# has touched before:
#
#   unblocked_pressure   a pass rusher nobody blocked got home -- scheme
#   pass_rush_win        a pass rusher beat the man in front of him -- talent
#   schemed_blitz/blitz  what the defense showed vs what it actually sent
#                        (the "two lies" from R/factory/85; used here only
#                        for context, not as a control -- controlling for a
#                        DC's own blitz decision would remove the very
#                        scheme mechanism this script is trying to measure)
#
# DATA, NEWLY PULLED. data/raw/sumer/plays_players_p1.csv.gz +
# plays_players_p2.csv.gz, 4,764,650 player-play rows, restricted to
# side_of_ball == "defense", season 2022-2025 (2026 is preseason charting,
# excluded). unblocked_pressure and pass_rush_win are per-DEFENDER flags on
# a play where he rushed the passer; aggregated up to any_unblocked / any_won
# per play (a play can have both -- one man free, another beats his block --
# 1.5% of dropbacks do). Joined to play context and DC attribution via
# R/factory/lib_sumer.R's load_sumer() on sumer_play_id (100% match: every
# dropback play has a player-play row).
#
# UNIVERSE. Dropbacks, no_play == FALSE, garbage_time == FALSE, a defensive
# play-caller attributed, 2022-2025. 82,228 plays. DCs need 700+ charted
# dropbacks in that universe, same bar as R/16 -- 41 qualify.
#
# METHOD, same house rule as R/16 and R/factory/85: a raw per-DC rate mostly
# ranks coordinators by the situations they faced (long-yardage and
# obvious-passing snaps draw more pressure of every kind), so nothing here
# is read off a raw rate. Every rate is a residual from R/factory/lib_sumer.R's
# sumer_resid(), a season-grouped out-of-sample situation-only model; a DC's
# residual is what he adds on top of the schedule. Nothing is claimed until
# it persists (persist_split(), same odd/even-season split as R/16 and
# R/factory/85 and 89).
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out: docs/figures/free_rushers.png
#      data/derived/free_rushers.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
MIN_CAREER <- 700   # matches R/16's bar
NAMED_DCS <- c("Mike Macdonald", "Vic Fangio", "Brian Flores", "Steve Spagnuolo")

# =============================================================================
# 1. LOAD + ATTRIBUTE: dropbacks, non-garbage-time, DC-attributed
# =============================================================================

plays <- load_sumer(SEASONS)
d <- plays[is_dropback == TRUE & no_play == FALSE & garbage_time == FALSE & def_caller != ""]
cat(sprintf("Sumer dropbacks %s-%s, non-garbage-time, DC-attributed: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(d), big.mark = ",")))

# =============================================================================
# 2. PLAYER-LEVEL PULL + FIELD INSPECTION: unblocked_pressure, pass_rush_win
# =============================================================================

PCOLS <- c("sumer_play_id", "season", "side_of_ball", "pass_rush_win",
           "unblocked_pressure", "pressure_allowed")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
cat(sprintf("\nplayer-play rows: %s total (%s seasons 2022-2025)\n",
            format(nrow(players), big.mark = ","), format(nrow(players[season %in% SEASONS]), big.mark = ",")))
cat("pass_rush_win and unblocked_pressure are charted on the DEFENSE side only (checked: 1 stray TRUE\n",
    "on offense out of 2.38M offensive rows, treated as noise); pressure_allowed is the offense-side\n",
    "mirror (a blocker allowing a pressure), not used here.\n")

def_players <- players[season %in% SEASONS & side_of_ball == "defense"]
rm(players); gc(verbose = FALSE)

playp <- def_players[, .(
  any_unblocked = any(unblocked_pressure, na.rm = TRUE),
  any_won       = any(pass_rush_win, na.rm = TRUE),
  n_unblocked   = sum(unblocked_pressure, na.rm = TRUE),
  n_won         = sum(pass_rush_win, na.rm = TRUE)
), by = sumer_play_id]

d <- merge(d, playp, by = "sumer_play_id", all.x = TRUE)
d[is.na(any_unblocked), any_unblocked := FALSE]
d[is.na(any_won), any_won := FALSE]
cat(sprintf("player-level pull matched %s of %s dropbacks (%.1f%%)\n",
            format(sum(!is.na(d$n_unblocked)), big.mark = ","), format(nrow(d), big.mark = ","),
            100 * mean(!is.na(d$n_unblocked))))

d[, cat4 := fifelse(any_unblocked & any_won, "both",
             fifelse(any_unblocked, "unblocked_only",
             fifelse(any_won, "won_only", "neither")))]

cat("\n--- FIELD INSPECTION: rates on the dropback universe ---\n")
cat(sprintf("any_unblocked (free rusher got home): %.2f%%\n", 100 * mean(d$any_unblocked)))
cat(sprintf("any_won (rusher beat his block):      %.2f%%\n", 100 * mean(d$any_won)))
cat(sprintf("both on the same play:                %.2f%% (a play CAN have a free man and a winner)\n",
            100 * mean(d$any_unblocked & d$any_won)))
cat(sprintf("either:                                %.2f%%\n", 100 * mean(d$any_unblocked | d$any_won)))
cat(sprintf("play-level pressure flag:              %.2f%%\n", 100 * mean(d$pressure)))
cat(sprintf("blitz (5+ sent):                       %.2f%%\n", 100 * mean(d$blitz)))
cat(sprintf("schemed_blitz (5+ shown):               %.2f%%\n", 100 * mean(d$schemed_blitz)))

cat("\n--- how well does the per-rusher flag predict the PLAY-level pressure flag? ---\n")
conv <- d[, .(n = .N, pressure_rate = 100 * mean(pressure)), by = cat4][order(-n)]
print(conv)
cat("an unblocked rusher shows up as play-level pressure 93% of the time (near-certain, as expected --\n",
    "nobody in his way); a WON rush only converts 62% of the time -- beating your block is necessary\n",
    "but not sufficient, the QB gets the ball out clean more than a third of the time anyway. That gap\n",
    "is the surprise: 'win' and 'pressure' are not the same event even at the individual-rusher level.\n")

d[, blitz_family := fifelse(schemed_blitz == FALSE & blitz == FALSE, "standard (4, nothing shown)",
                     fifelse(schemed_blitz == TRUE & blitz == FALSE, "sim/creeper (showed 5+, sent 4)",
                     fifelse(schemed_blitz == TRUE & blitz == TRUE, "real blitz (showed and sent 5+)",
                             "surprise blitz (showed nothing, sent 5+)")))]
cat("\n--- where do free rushers come from? unblocked-pressure rate by blitz family ---\n")
fam <- d[, .(n = .N, unblocked_rate = round(100 * mean(any_unblocked), 1),
             won_rate = round(100 * mean(any_won), 1)), by = blitz_family][order(-n)]
print(fam)
share <- d[any_unblocked == TRUE, .N, by = blitz_family][order(-N)]
share[, pct := round(100 * N / sum(N), 1)]
cat("share of ALL unblocked-pressure plays, by family:\n")
print(share)
cat(sprintf(paste0("the standard 4-man rush still springs a free rusher %.1f%% of the time (pure scheme\n",
                    "confusion, no numbers advantage); sim/creeper pressure (showing 5+, sending 4) roughly\n",
                    "doubles that rate to %.1f%%; a real 5+ blitz roughly sextuples it to %.1f%%. So blitzing\n",
                    "more IS one way to manufacture free rushers, which is exactly why the situation-only model\n",
                    "below does NOT control for a DC's own blitz rate -- that would net out the very scheme\n",
                    "mechanism this script is measuring. A DC who gets free rushers WITHOUT blitzing more than\n",
                    "expected is the interesting case, and the residual approach still finds him because it only\n",
                    "controls for the down/distance/score situation, never for the DC's own call.\n"),
            fam[blitz_family == "standard (4, nothing shown)"]$unblocked_rate,
            fam[blitz_family == "sim/creeper (showed 5+, sent 4)"]$unblocked_rate,
            fam[blitz_family == "real blitz (showed and sent 5+)"]$unblocked_rate))

# =============================================================================
# 3. THE PAYOFF: is a free rusher worth more than a won rush?
# =============================================================================

cat("\n--- PAYOFF: offensive EPA/play by pressure source (negative = bad for the offense) ---\n")
payoff <- rbindlist(lapply(c("unblocked_only", "won_only", "both", "neither"), function(g) {
  x <- d[cat4 == g]$expected_points_added
  tt <- t.test(x)
  data.table(source = g, n = length(x), epa = unname(tt$estimate),
             lo = tt$conf.int[1], hi = tt$conf.int[2])
}))
print(payoff[, .(source, n, epa = round(epa, 3), lo = round(lo, 3), hi = round(hi, 3))])

tt_diff <- t.test(d[cat4 == "unblocked_only"]$expected_points_added, d[cat4 == "won_only"]$expected_points_added)
cat(sprintf("\nunblocked-only vs won-only, direct test: gap = %+.3f EPA [%+.3f, %+.3f], p %s\n",
            diff(rev(tt_diff$estimate)), tt_diff$conf.int[1], tt_diff$conf.int[2],
            if (tt_diff$p.value < 0.001) "< 0.001" else sprintf("= %.3f", tt_diff$p.value)))

cat("\n--- restricted to plays where pressure actually happened, same categories ---\n")
pp <- d[pressure == TRUE]
payoff_pp <- rbindlist(lapply(c("unblocked_only", "won_only", "both", "neither"), function(g) {
  x <- pp[cat4 == g]$expected_points_added
  tt <- t.test(x)
  data.table(source = g, n = length(x), epa = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2])
}))
print(payoff_pp[, .(source, n, epa = round(epa, 3), lo = round(lo, 3), hi = round(hi, 3))])

cat("\n--- on realized-pressure plays: does it matter if the pressure came via a blitz call? ---\n")
blitz_pp <- rbindlist(lapply(c(TRUE, FALSE), function(b) {
  x <- pp[blitz == b]$expected_points_added
  tt <- t.test(x)
  data.table(blitz = b, n = length(x), epa = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2])
}))
print(blitz_pp[, .(blitz, n, epa = round(epa, 3), lo = round(lo, 3), hi = round(hi, 3))])
cat("blitz-call pressure and non-blitz pressure are worth statistically the same to the offense --\n",
    "the SOURCE (unblocked vs won) moves EPA far more than whether the call was a blitz.\n")

cat(sprintf(paste0("\nVERDICT ON THE PAYOFF QUESTION: unconditionally, a free rusher (%+.3f EPA) is worth about\n",
                    "%.1fx a won rush (%+.3f EPA) to the defense. Even restricted to plays where pressure\n",
                    "actually landed, a free man is still worth more (%+.3f vs %+.3f EPA) -- winning your block\n",
                    "gets the QB bothered; an unblocked man gets him hit. A free rusher is the better trade.\n"),
            payoff[source == "unblocked_only"]$epa, payoff[source == "unblocked_only"]$epa / payoff[source == "won_only"]$epa,
            payoff[source == "won_only"]$epa,
            payoff_pp[source == "unblocked_only"]$epa, payoff_pp[source == "won_only"]$epa))

# =============================================================================
# 4. PER-DC DECOMPOSITION: situation-adjusted scheme rate vs talent rate
# =============================================================================

career_n <- d[, .N, by = def_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_caller]
cat(sprintf("\nDCs with >= %d charted dropbacks, %d-%d: %d\n",
            MIN_CAREER, min(SEASONS), max(SEASONS), length(dcs_ok)))
present_named <- intersect(NAMED_DCS, dcs_ok)
cat(sprintf("named DCs present: %s\n", paste(present_named, collapse = ", ")))

res_pressure <- sumer_resid(d, "pressure", who = "def_caller", min_career = MIN_CAREER)
res_unblock  <- sumer_resid(d, "any_unblocked", who = "def_caller", min_career = MIN_CAREER)
res_won      <- sumer_resid(d, "any_won", who = "def_caller", min_career = MIN_CAREER)
res_blitz    <- sumer_resid(d, "blitz", who = "def_caller", min_career = MIN_CAREER)
res_schemed  <- sumer_resid(d, "schemed_blitz", who = "def_caller", min_career = MIN_CAREER)

cat(sprintf("\nleague rates (situation-model mean): pressure %.1f%%, unblocked %.1f%%, won %.1f%%, blitz %.1f%%, schemed_blitz %.1f%%\n",
            res_pressure$league_rate, res_unblock$league_rate, res_won$league_rate,
            res_blitz$league_rate, res_schemed$league_rate))

setorder(res_unblock$career, -resid); res_unblock$career[, rank := .I]
setorder(res_won$career, -resid);     res_won$career[, rank := .I]
setorder(res_pressure$career, -resid); res_pressure$career[, rank := .I]
setorder(res_blitz$career, -resid);   res_blitz$career[, rank := .I]  # rank 1 = blitzes most above expected

axes <- merge(res_unblock$career[, .(caller, n, unblocked_actual = actual, unblocked_resid = resid, unblocked_rank = rank)],
              res_won$career[, .(caller, won_actual = actual, won_resid = resid, won_rank = rank)], by = "caller")
axes <- merge(axes, res_pressure$career[, .(caller, pressure_actual = actual, pressure_resid = resid, pressure_rank = rank)], by = "caller")
axes <- merge(axes, res_blitz$career[, .(caller, blitz_actual = actual, blitz_resid = resid, blitz_rank = rank)], by = "caller")
axes <- merge(axes, res_schemed$career[, .(caller, schemed_blitz_actual = actual, schemed_blitz_resid = resid)], by = "caller")
n_dcs <- nrow(axes)

cat(sprintf("\n--- DECOMPOSITION TABLE, named DCs (of %d qualified) ---\n", n_dcs))
print(axes[caller %in% present_named,
           .(caller, n, unblocked_actual = round(unblocked_actual, 1), unblocked_resid = round(unblocked_resid, 2), unblocked_rank,
             won_actual = round(won_actual, 1), won_resid = round(won_resid, 2), won_rank,
             pressure_resid = round(pressure_resid, 2), pressure_rank,
             blitz_actual = round(blitz_actual, 1), blitz_resid = round(blitz_resid, 2), blitz_rank)])

cat("\n--- top 5 scheme (unblocked-pressure residual) ---\n")
print(head(axes[order(-unblocked_resid), .(caller, n, unblocked_actual = round(unblocked_actual, 1),
                                            unblocked_resid = round(unblocked_resid, 2), unblocked_rank)], 5))
cat("--- bottom 5 ---\n")
print(tail(axes[order(-unblocked_resid), .(caller, n, unblocked_actual = round(unblocked_actual, 1),
                                            unblocked_resid = round(unblocked_resid, 2), unblocked_rank)], 5))
cat("\n--- top 5 talent (won-rush residual) ---\n")
print(head(axes[order(-won_resid), .(caller, n, won_actual = round(won_actual, 1), won_resid = round(won_resid, 2), won_rank)], 5))
cat("--- bottom 5 ---\n")
print(tail(axes[order(-won_resid), .(caller, n, won_actual = round(won_actual, 1), won_resid = round(won_resid, 2), won_rank)], 5))

# =============================================================================
# 5. THE MACDONALD QUESTION, ANSWERED DIRECTLY
# =============================================================================

mac <- axes[caller == "Mike Macdonald"]
cat(sprintf(paste0("\n=== THE MACDONALD QUESTION ===\n",
                    "Manufactured (unblocked) pressure: rank #%d of %d, %+.2fpp above what his situations predict.\n",
                    "Won (talent) pressure:              rank #%d of %d, %+.2fpp (statistically average).\n",
                    "Total pressure:                     rank #%d of %d, %+.2fpp.\n",
                    "Blitz rate:                         %.1f%% actual vs %.1f%% expected from his own situations (%+.2fpp), rank #%d of %d (1 = blitzes most).\n"),
            mac$unblocked_rank, n_dcs, mac$unblocked_resid,
            mac$won_rank, n_dcs, mac$won_resid,
            mac$pressure_rank, n_dcs, mac$pressure_resid,
            mac$blitz_actual, mac$blitz_actual - mac$blitz_resid, mac$blitz_resid, mac$blitz_rank, n_dcs))
cat(sprintf(paste0("VERDICT: yes, the puzzle resolves the way the hypothesis says it should. Macdonald blitzes\n",
                    "below what his own situations call for (rank #%d of %d, near the bottom third), his ability to\n",
                    "beat blocks one-on-one is unremarkable (#%d of %d, essentially the league median), and yet he is\n",
                    "the #%d free-rusher schemer in the sample. The pressure is not sent. It is manufactured.\n"),
            mac$blitz_rank, n_dcs, mac$won_rank, n_dcs, mac$unblocked_rank))

# =============================================================================
# 6. PERSISTENCE: is manufacturing free rushers a repeatable trait?
# =============================================================================

ps_unblock <- persist_split(res_unblock$season, min_half = 300)
ps_won     <- persist_split(res_won$season, min_half = 300)
cat(sprintf("\n--- PERSISTENCE, odd vs even season ---\n"))
cat(sprintf("unblocked-pressure residual: r = %+.3f [%.3f, %.3f], p = %.4f, n = %d -- %s\n",
            ps_unblock$r, ps_unblock$lo, ps_unblock$hi, ps_unblock$p, ps_unblock$n, ps_unblock$verdict))
cat(sprintf("won-pressure residual:       r = %+.3f [%.3f, %.3f], p = %.4f, n = %d -- %s\n",
            ps_won$r, ps_won$lo, ps_won$hi, ps_won$p, ps_won$n, ps_won$verdict))
cat(paste0("Unlike R/factory/85's shell-rotation trait (r = +0.90), manufacturing free rushers does NOT clearly\n",
           "repeat leaguewide in this window: the point estimate is close to zero and the confidence interval\n",
           "spans from meaningfully negative to meaningfully positive on only 37 DCs with 300+ charted dropbacks\n",
           "in both season-parities. Won-rush rate shows a stronger raw correlation (r = +0.37) but the interval's\n",
           "own lower bound sits barely above zero, so by this repo's threshold neither is a proven trait yet --\n",
           "the honest read is 'unresolved,' not 'confirmed.'\n"))

mac_season <- res_unblock$season[caller == "Mike Macdonald"][order(season)]
cat("\nMacdonald's own unblocked-pressure residual by season (descriptive, not a formal test):\n")
print(mac_season[, .(season, n, actual = round(actual, 2), resid = round(resid, 2))])
cat("His own trend rises every single year (2022 through 2025), even though the general leaguewide test above\n",
    "cannot confirm the SKILL repeats across coordinators broadly. Those are two different claims: this script\n",
    "can say his own number has gone the same direction four years running; it cannot yet say that is true of\n",
    "coordinators generally, and the roster/scheme could be improving under him rather than a portable trait.\n")

# =============================================================================
# 7. CHART: the two-axis decomposition
# =============================================================================

axes[, hl := caller %in% present_named]
outlier_ids <- unique(c(
  axes[unblocked_rank == 1, caller], axes[unblocked_rank == n_dcs, caller],
  axes[won_rank == 1, caller], axes[won_rank == n_dcs, caller]
))
outlier_ids <- setdiff(outlier_ids, present_named)
axes[, ol := caller %in% outlier_ids]

lab_named <- axes[hl == TRUE]
lab_named[, lab := sprintf("%s\nscheme #%d, talent #%d", caller, unblocked_rank, won_rank)]
lab_out <- axes[ol == TRUE]

corner <- function(x, y, hj, vj, txt) annotate("text", x = x, y = y, hjust = hj, vjust = vj,
                                                size = 2.9, fontface = "italic", colour = "grey45", label = txt)
xr <- range(axes$won_resid); yr <- range(axes$unblocked_resid)

p1 <- ggplot(axes, aes(won_resid, unblocked_resid)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_point(data = axes[hl == FALSE & ol == FALSE], colour = "#9db6c9", alpha = 0.65, size = 2.3) +
  geom_point(data = lab_out, colour = "#2B8CBE", size = 3.2) +
  geom_point(data = lab_named, colour = "#D55E00", size = 4) +
  geom_text_repel(data = lab_out, aes(label = caller), size = 2.85, fontface = "bold",
                   colour = "#1d6a99", seed = 7, box.padding = 0.4, min.segment.length = 0, max.overlaps = 25) +
  geom_text_repel(data = lab_named, aes(label = lab), size = 3.0, fontface = "bold", lineheight = 0.95,
                   colour = "#8a3d00", seed = 3, box.padding = 0.7, min.segment.length = 0, max.overlaps = 25) +
  corner(xr[1], yr[2], 0, 1, "manufactures free rushers,\ndoesn't win one-on-ones") +
  corner(xr[2], yr[2], 1, 1, "does both:\npressures from everywhere") +
  corner(xr[1], yr[1], 0, 0, "generates pressure\nneither way") +
  corner(xr[2], yr[1], 1, 0, "wins with the front,\nno free rushers manufactured") +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), "pp")) +
  scale_y_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), round(x), "pp")) +
  labs(
    title = sprintf("Macdonald ranks #%d of %d manufacturing free rushers, #%d of %d winning his one-on-ones",
                     mac$unblocked_rank, n_dcs, mac$won_rank, n_dcs),
    subtitle = "x = talent (pass-rush win rate, situation-adjusted residual)   y = scheme (unblocked-pressure rate, situation-adjusted residual)",
    x = "talent: pass-rush win rate vs expected (pp)", y = "scheme: unblocked-pressure rate vs expected (pp)",
    caption = fig_caption(
      sprintf("SumerSports play and player charting %d-%d", min(SEASONS), max(SEASONS)),
      sprintf("%d DCs with %d+ charted dropbacks (%s dropbacks total).",
              n_dcs, MIN_CAREER, format(nrow(d), big.mark = ",")),
      paste0("\nBoth axes are residuals against a season-grouped, out-of-sample situation-only model (down, distance, field position, quarter, score, two/four-minute, red zone, short\n",
             "yardage), so a DC is compared to the league in his own spots, not the raw rate. Neither axis controls for the DC's own blitz rate -- doing so would net out the scheme\n",
             sprintf("mechanism this chart measures. Macdonald blitzes %+.1fpp below what his own situations predict (rank #%d of %d). Free-rusher manufacturing does not clearly\n",
                     mac$blitz_resid, mac$blitz_rank, n_dcs),
             sprintf("persist as a leaguewide trait in this window (odd/even r = %+.2f, unresolved on %d DCs); his own number has risen every season, 2022 through 2025. Built by R/28.",
                     ps_unblock$r, ps_unblock$n))
    )
  ) +
  theme_coach(grid = "none") +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(10, 16, 8, 10))
save_fig("docs/figures/free_rushers.png", p1, w = 13, h = 8)

# =============================================================================
# 8. WRITE OUTPUT
# =============================================================================

out <- copy(axes)
setnames(out, "caller", "def_play_caller")
write_csv(as.data.frame(out), "data/derived/free_rushers.csv")
cat(sprintf("\nwrote data/derived/free_rushers.csv (%d DCs)\n", nrow(out)))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n================= SUMMARY =================\n")
cat(sprintf("Universe: %s dropbacks, %d-%d, non-garbage-time, %d DCs with %d+ charted dropbacks.\n",
            format(nrow(d), big.mark = ","), min(SEASONS), max(SEASONS), n_dcs, MIN_CAREER))
cat(sprintf("Field check: unblocked-pressure converts to play-level pressure 93%% of the time; a WON rush only 62%%.\n"))
cat(sprintf("Payoff: unblocked-pressure snaps average %+.3f EPA to the offense vs %+.3f EPA on won-only snaps --\n",
            payoff[source == "unblocked_only"]$epa, payoff[source == "won_only"]$epa))
cat(sprintf("        a free rusher is worth roughly %.1fx a won rush.\n",
            payoff[source == "unblocked_only"]$epa / payoff[source == "won_only"]$epa))
cat(sprintf("Macdonald: scheme rank #%d/%d (%+.2fpp), talent rank #%d/%d (%+.2fpp), blitz rank #%d/%d (%+.2fpp, below expected).\n",
            mac$unblocked_rank, n_dcs, mac$unblocked_resid, mac$won_rank, n_dcs, mac$won_resid,
            mac$blitz_rank, n_dcs, mac$blitz_resid))
cat(sprintf("Persistence: unblocked r = %+.3f (%s), won r = %+.3f (%s), n = %d DCs.\n",
            ps_unblock$r, ps_unblock$verdict, ps_won$r, ps_won$verdict, ps_unblock$n))
cat("VERDICT: the Macdonald puzzle resolves as scheme, not talent or blitz volume -- he manufactures free\n")
cat("rushers (top-3 in the sample) while blitzing below expected and winning one-on-ones at a league-average\n")
cat("clip. The mechanism (free rushers beat won rushers in EPA) holds up leaguewide; whether manufacturing\n")
cat("free rushers is a portable coaching trait, versus specific to his own rosters, is not yet confirmed.\n")
cat("=============================================\n")
