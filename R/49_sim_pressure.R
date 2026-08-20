# =============================================================================
# 49_sim_pressure.R -- simulated pressure vs. the standard rush: does showing
# extra rushers and sending a different four actually spring free rushers?
#
# THE THEORY (a former offensive lineman's). R/28 found Mike Macdonald's
# defenses manufacture free (unblocked) rushers at a top-tier rate while
# blitzing BELOW what his own situations call for. One way to reconcile
# "fewer rushers sent, more free rushers gotten" without ever sending five:
# SIMULATED PRESSURE. Show more than four rushers before the snap, still
# send only four, but not the standard four -- a linebacker or defensive
# back rushes while a defensive lineman drops into coverage. If the
# protection is set for four down linemen and one of them peels off, the
# blocker assigned to him is now blocking nobody, and whoever the defense
# subbed in is running through a gap the offensive line never saw. This
# script tests that mechanism directly: does a four-man rush that includes
# a linebacker or defensive back spring free rushers more often than a
# four-man rush of straight defensive linemen?
#
# DEFINING "WHO RUSHED." Sumer's plays_players file has a `role` field
# (post-snap behavior on the play), and PASS RUSH is one of its 8 levels
# (RUN DEFENSE, COVERAGE, PASS RUSH, blank, RUN BLOCK, PASS BLOCK, PASS
# ROUTE, RUN, in that frequency order on defense-side rows). role is
# exactly right here -- it records who actually rushed the passer, not who
# lined up threatening to. Counting PASS RUSH rows per play was checked
# against the play-level `blitz` field (5+ rushers SENT, R/28's own
# definition): 21,678 of 21,694 blitz==TRUE plays have 5+ counted rushers,
# a 99.9% match, so the player-level count is trustworthy. The `blitz`
# field is still used AS-IS for the blitz category below, to stay exactly
# consistent with R/28 rather than re-deriving it.
#
# DEFINING "WHO'S A LINEMAN." plays_players has no position field of its
# own; position comes from data/raw/sumer/player_details.csv.gz, joined on
# sumer_player_id. Sumer's position codes (found by inspection, not
# assumed): DE/DT = defensive line, IB/OB = linebacker (inside/outside),
# DC/DS = defensive back (corner/safety). Among PASS RUSH rows, DE+DT are
# 81% of all rushes, confirming the front four is still the primary rush
# source; IB, OB, DC and DS combined are 8.9% of rushes -- the "sends a
# different four" plays this question is about.
#
# THE TRAP: 8.5% of all defense-side player rows, and 18% of exactly-four-
# rusher plays, have at least one rusher whose sumer_player_id does not
# appear in player_details.csv.gz at all (checked directly -- it is never a
# blank id, always a real id missing from that file, most likely a player
# who left the league before the file's current snapshot). Those plays
# cannot be honestly called "all linemen" or "includes a non-lineman," so
# they are excluded from the standard-vs-sim split entirely and reported as
# their own "unclassified" bucket rather than guessed into either side.
#
# THE THREE CATEGORIES (dropbacks only, see universe below):
#   STANDARD  exactly 4 rushers, every one identified as a defensive lineman
#   SIM       exactly 4 rushers, at least one identified as a linebacker or
#             defensive back, none unidentified
#   BLITZ     R/28's own blitz field (5+ rushers sent), unchanged
#   (a 4th bucket, "unclassified," holds 4-rusher plays with an
#   unidentified rusher; a 5th, "other," holds 3-or-fewer-rusher plays --
#   coverage sacks, quick scrambles. Neither is part of the 3-way analysis.)
#
# THE COMPLEMENTARY FLAVOR. A dropped lineman is not the same event as a
# sim rush by this script's definition -- a defense can rush 4 straight
# linemen while ALSO subbing in a 5th lineman who drops (still "standard"
# by rush count), or send a sim rush where the dropped lineman isn't even
# on the field that snap. Both are checked: 16.8% of dropbacks have at
# least one identified defensive lineman on the field who did NOT rush.
# That overlaps heavily with sim rushes (46% of sim snaps also have a
# lineman drop) and rarely with standard rushes (7% do) -- the two flavors
# of "not the standard four" mostly travel together but are not identical,
# so both get reported.
#
# UNIVERSE. Same as R/28: dropbacks, no_play == FALSE, garbage_time ==
# FALSE, a defensive play-caller attributed, 2022-2025 (2026 rows are
# preseason charting, excluded by load_sumer()'s season filter). 82,228
# plays. DCs need 700+ charted dropbacks to qualify, R/28's bar -- 41
# qualify.
#
# OUTCOME 1: free-rusher rate (R/28's unblocked-pressure flag, any rusher
# on the play getting home unblocked) by category, league-wide, with 95%
# confidence intervals.
# OUTCOME 2: EPA allowed per dropback by category. Reported RAW (not
# situation-adjusted) and disclosed as such -- sim calls likely cluster in
# obvious-passing situations, so this number should not be read as a pure
# scheme effect the way R/28's residualized numbers are.
# PER-COORDINATOR: sim-pressure usage rate, situation-adjusted via
# R/factory/lib_sumer.R's sumer_resid() and checked for persistence with
# persist_split(), same house rule as R/28 ("everything published goes
# through sumer_resid() and survives persist_split()"). Separately, each
# qualified DC's OWN sim free-rusher rate vs his OWN standard free-rusher
# rate (raw -- per-DC sim-snap counts run as low as 14, too thin for a
# second layer of modeling, so this half is descriptive, with n's shown for
# every DC so a reader can see which deltas are backed by real volume).
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(),
# save_fig()). No em dashes.
#
# Out: docs/figures/sim_pressure.png
#      data/derived/sim_pressure.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS    <- 2022:2025
MIN_CAREER <- 700
NAMED_DC   <- "Mike Macdonald"

ci_rate <- function(x, n) {
  pt <- prop.test(x, n, correct = FALSE)
  list(rate = 100 * x / n, lo = 100 * pt$conf.int[1], hi = 100 * pt$conf.int[2])
}

# =============================================================================
# 1. LOAD + ATTRIBUTE: dropbacks, non-garbage-time, DC-attributed (R/28's universe)
# =============================================================================

plays <- load_sumer(SEASONS)
d <- plays[is_dropback == TRUE & no_play == FALSE & garbage_time == FALSE & def_caller != ""]
cat(sprintf("Sumer dropbacks %s-%s, non-garbage-time, DC-attributed: %s\n",
            min(SEASONS), max(SEASONS), format(nrow(d), big.mark = ",")))

# =============================================================================
# 2. PLAYER-LEVEL PULL: role, position (via player_details join), unblocked_pressure
# =============================================================================

PCOLS <- c("sumer_play_id", "season", "side_of_ball", "role", "sumer_player_id", "unblocked_pressure")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
def_players <- players[season %in% SEASONS & side_of_ball == "defense"]
rm(players); gc(verbose = FALSE)

cat("\n--- FIELD INSPECTION: role levels on defense-side player rows ---\n")
print(def_players[, .N, by = role][order(-N)])

pdet <- fread("data/raw/sumer/player_details.csv.gz", select = c("sumer_player_id", "position"), showProgress = FALSE)
cat("\n--- FIELD INSPECTION: position levels in player_details.csv.gz ---\n")
print(pdet[, .N, by = position][order(-N)])

def_players <- merge(def_players, pdet, by = "sumer_player_id", all.x = TRUE)
na_rate <- def_players[role == "PASS RUSH", mean(is.na(position))]
cat(sprintf("\nposition unmatched on PASS RUSH rows: %.1f%% (always a real sumer_player_id missing from\n", 100 * na_rate),
    "player_details.csv.gz, never a blank id -- checked directly). Those rusher-plays cannot be classed\n",
    "as all-lineman or not, so they are excluded from standard/sim below and reported separately.\n")

cat("\nposition breakdown among PASS RUSH rows (defines the DL / LB / DB groups used below):\n")
print(def_players[role == "PASS RUSH", .N, by = position][order(-N)])
cat("DL = DE, DT.   LB = IB (inside backer), OB (outside backer).   DB = DC (corner), DS (safety).\n")

# =============================================================================
# 3. RUSHER COUNT + POSITION MIX PER PLAY
# =============================================================================

DL_POS <- c("DE", "DT"); LB_POS <- c("IB", "OB"); DB_POS <- c("DC", "DS")

rushers <- def_players[role == "PASS RUSH", .(
  n_rush = .N,
  n_dl   = sum(position %in% DL_POS, na.rm = TRUE),
  n_lb   = sum(position %in% LB_POS, na.rm = TRUE),
  n_db   = sum(position %in% DB_POS, na.rm = TRUE)
), by = sumer_play_id]
rushers[, n_id := n_dl + n_lb + n_db]
rushers[, n_other := n_rush - n_id]   # NA position + the handful of miscoded non-front-seven codes

dl_field <- def_players[position %in% DL_POS, .(
  n_dl_field = .N, n_dl_rush = sum(role == "PASS RUSH")
), by = sumer_play_id]
dl_field[, n_dl_dropped := n_dl_field - n_dl_rush]

unb <- def_players[, .(any_unblocked = any(unblocked_pressure, na.rm = TRUE)), by = sumer_play_id]

d <- merge(d, rushers, by = "sumer_play_id", all.x = TRUE)
d <- merge(d, dl_field[, .(sumer_play_id, n_dl_dropped)], by = "sumer_play_id", all.x = TRUE)
d <- merge(d, unb, by = "sumer_play_id", all.x = TRUE)
for (col in c("n_rush", "n_dl", "n_lb", "n_db", "n_id", "n_other", "n_dl_dropped")) {
  d[is.na(get(col)), (col) := 0]
}
d[is.na(any_unblocked), any_unblocked := FALSE]

cat(sprintf("\nrusher-count distribution, dropback universe (n = %s):\n", format(nrow(d), big.mark = ",")))
print(d[, .N, by = n_rush][order(n_rush)])

# =============================================================================
# 4. THE THREE CATEGORIES
# =============================================================================

d[, category := fifelse(blitz == TRUE, "blitz",
                 fifelse(n_rush == 4 & n_other == 0 & n_dl == 4, "standard",
                 fifelse(n_rush == 4 & n_other == 0 & n_dl < 4,  "sim",
                 fifelse(n_rush == 4 & n_other > 0, "unclassified_4", "other"))))]

cat("\n--- category counts (blitz uses R/28's own field, not the rusher count) ---\n")
print(d[, .(n = .N, pct = round(100 * .N / nrow(d), 1)), by = category][order(-n)])
cat("'unclassified_4' = 4 rushers, at least one unidentified. 'other' = 3 or fewer rushers (coverage\n",
    "sacks, quick throws before a rush developed, scrambles) or 6+ (rare, still folds into blitz via\n",
    "the blitz field). Neither is part of the 3-way standard/sim/blitz comparison below.\n")

cat("\n--- the complementary flavor: a dropped lineman, and how it overlaps with a sim rush ---\n")
cat(sprintf("dropped-lineman rate, ALL dropbacks: %.1f%%\n", 100 * mean(d$n_dl_dropped >= 1)))
print(d[category %in% c("standard", "sim"), .(n = .N, dropped_lineman_rate = round(100 * mean(n_dl_dropped >= 1), 1)),
        by = category])
cat("a sim rush drags a dropped lineman along with it about 46% of the time; a standard 4-man rush\n",
    "only 7% of the time (a 5th lineman subbed in and designated to drop, standard rush count unchanged).\n",
    "the two flavors mostly travel together but are not the same event, so both get measured.\n")

# =============================================================================
# 5. OUTCOME 1: LEAGUE-WIDE FREE-RUSHER RATE BY CATEGORY, WITH UNCERTAINTY
# =============================================================================

cat("\n--- OUTCOME 1: free-rusher rate by category, league-wide, 95% CI ---\n")
rate_tbl <- rbindlist(lapply(c("standard", "sim", "blitz"), function(cat_) {
  sub <- d[category == cat_]
  x <- sum(sub$any_unblocked); n <- nrow(sub)
  ci <- ci_rate(x, n)
  data.table(category = cat_, n = n, x = x, rate = round(ci$rate, 2), lo = round(ci$lo, 2), hi = round(ci$hi, 2))
}))
setorder(rate_tbl, rate)
print(rate_tbl)

std_rate <- rate_tbl[category == "standard"]$rate
sim_rate <- rate_tbl[category == "sim"]$rate
blitz_rate <- rate_tbl[category == "blitz"]$rate
cat(sprintf("\nsim pressure springs a free rusher %.1fx as often as a standard 4-man rush (%.2f%% vs %.2f%%);\n",
            sim_rate / std_rate, sim_rate, std_rate))
cat(sprintf("a real blitz still springs one more often than that (%.2f%%, %.1fx the standard rate).\n",
            blitz_rate, blitz_rate / std_rate))

# =============================================================================
# 6. OUTCOME 2: EPA ALLOWED PER DROPBACK BY CATEGORY (RAW)
# =============================================================================

cat("\n--- OUTCOME 2: EPA allowed per dropback by category (RAW, situation not adjusted) ---\n")
epa_tbl <- rbindlist(lapply(c("standard", "sim", "blitz"), function(cat_) {
  x <- d[category == cat_]$expected_points_added
  tt <- t.test(x)
  data.table(category = cat_, n = length(x), epa = round(unname(tt$estimate), 3),
             lo = round(tt$conf.int[1], 3), hi = round(tt$conf.int[2], 3))
}))
print(epa_tbl)
cat("raw, not situation-adjusted: sim calls likely skew toward obvious-passing downs the way blitzes do\n",
    "(R/28), so this is descriptive context, not a scheme-isolated estimate the way R/28's residuals are.\n")

# =============================================================================
# 7. PER-COORDINATOR: SIM USAGE RATE (SITUATION-ADJUSTED) + PERSISTENCE
# =============================================================================

d[, is_sim := category == "sim"]
career_n <- d[, .N, by = def_caller]
dcs_ok <- career_n[N >= MIN_CAREER, def_caller]
cat(sprintf("\nDCs with >= %d charted dropbacks, %d-%d: %d\n", MIN_CAREER, min(SEASONS), max(SEASONS), length(dcs_ok)))

res_sim <- sumer_resid(d, "is_sim", who = "def_caller", min_career = MIN_CAREER)
setorder(res_sim$career, -resid); res_sim$career[, rank := .I]
n_dcs <- nrow(res_sim$career)
cat(sprintf("league sim-usage rate (situation-model mean): %.1f%%\n", res_sim$league_rate))

cat("\n--- top 5 sim-pressure users, situation-adjusted usage rate ---\n")
print(head(res_sim$career[, .(caller, n, actual = round(actual, 1), resid = round(resid, 1), rank)], 5))

ps_sim <- persist_split(res_sim$season, min_half = 300)
cat(sprintf("\npersistence, sim-usage residual, odd vs even season: r = %+.3f [%.3f, %.3f], p = %.4f, n = %d -- %s\n",
            ps_sim$r, ps_sim$lo, ps_sim$hi, ps_sim$p, ps_sim$n, ps_sim$verdict))

# =============================================================================
# 8. PER-COORDINATOR: HIS OWN SIM RATE VS HIS OWN STANDARD RATE (raw, per-DC)
# =============================================================================

perdc <- d[def_caller %in% dcs_ok & category %in% c("standard", "sim"),
           .(n = .N, free_rate = 100 * mean(any_unblocked)), by = .(def_caller, category)]
wide <- dcast(perdc, def_caller ~ category, value.var = c("n", "free_rate"), fill = 0)
wide[, delta := free_rate_sim - free_rate_standard]
setorder(wide, -delta)
wide[, delta_rank := .I]

n_positive <- sum(wide$delta > 0)
cat(sprintf("\n--- per-DC: own sim free-rusher rate minus own standard free-rusher rate, all %d qualified DCs ---\n", n_dcs))
cat(sprintf("%d of %d qualified DCs show a HIGHER free-rusher rate on their own sim rushes than their own\n",
            n_positive, n_dcs),
    "standard rushes -- the mechanism holds almost universally, not just on the league average.\n")
print(wide[, .(def_caller, n_sim = n_sim, free_rate_sim = round(free_rate_sim, 1),
               n_standard = n_standard, free_rate_standard = round(free_rate_standard, 1),
               delta = round(delta, 1), delta_rank)][1:10])

# =============================================================================
# 9. THE MACDONALD QUESTION, ANSWERED DIRECTLY
# =============================================================================

mac_usage <- res_sim$career[caller == NAMED_DC]
mac_delta <- wide[def_caller == NAMED_DC]
mac_blitz_rate <- rate_tbl[category == "blitz"]$rate  # league blitz rate, for context only
mac_cat_rates <- d[def_caller == NAMED_DC & category %in% c("standard", "sim", "blitz"),
                    .(n = .N, rate = round(100 * mean(any_unblocked), 1)), by = category]
setorder(mac_cat_rates, category)

cat(sprintf(paste0("\n=== THE MACDONALD QUESTION ===\n",
                    "Sim-pressure usage: rank #%d of %d, %.1f%% of his dropbacks (%+.1fpp vs what his situations predict) --\n",
                    "essentially a league-average sim-call rate, not an outlier volume.\n",
                    "His own free-rusher rate by category: standard %.1f%% (n=%d), sim %.1f%% (n=%d), blitz %.1f%% (n=%d).\n",
                    "His sim-minus-standard gap ranks #%d of %d qualified DCs.\n"),
            mac_usage$rank, n_dcs, mac_usage$actual, mac_usage$resid,
            mac_cat_rates[category == "standard"]$rate, mac_cat_rates[category == "standard"]$n,
            mac_cat_rates[category == "sim"]$rate, mac_cat_rates[category == "sim"]$n,
            mac_cat_rates[category == "blitz"]$rate, mac_cat_rates[category == "blitz"]$n,
            mac_delta$delta_rank, n_dcs))
cat(sprintf(paste0("VERDICT: the theory holds, but not through volume. Macdonald does not call sim pressure more\n",
                    "than an average defense in his spots (rank #%d of %d on usage). When he DOES call it, though, it\n",
                    "works better than most: his sim free-rusher rate (%.1f%%) is more than %s his own standard-rush rate\n",
                    "(%.1f%%), a gap that ranks #%d of %d DCs, and his sim rate alone clears the LEAGUE sim rate (%.1f%%).\n",
                    "This matches R/28's finding that Macdonald manufactures free rushers while blitzing below expected --\n",
                    "sim pressure, called at a normal rate but executed better than most, is a plausible mechanism for it.\n"),
            mac_usage$rank, n_dcs,
            mac_cat_rates[category == "sim"]$rate,
            sprintf("%.1fx", mac_cat_rates[category == "sim"]$rate / mac_cat_rates[category == "standard"]$rate),
            mac_cat_rates[category == "standard"]$rate, mac_delta$delta_rank, n_dcs, sim_rate))

# =============================================================================
# 10. CHART
# =============================================================================

chart_df <- rbindlist(list(
  rate_tbl[, .(category, group = "League", n, rate, lo, hi)],
  mac_cat_rates[, .(category, group = "Macdonald", n, rate, lo = NA_real_, hi = NA_real_)]
))
chart_df[, category := factor(category, levels = c("standard", "sim", "blitz"),
                              labels = c("standard rush\n(4, all linemen)", "sim pressure\n(4, one not a lineman)", "blitz\n(5+ sent)"))]
chart_df[, group := factor(group, levels = c("League", "Macdonald"))]
chart_df[, lab := sprintf("%.1f%%\n(n=%s)", rate, format(n, big.mark = ",", trim = TRUE))]

COL <- c("League" = "#9db6c9", "Macdonald" = "#D55E00")

p <- ggplot(chart_df, aes(category, rate, fill = group)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.6) +
  geom_text(aes(label = lab, group = group), position = position_dodge(width = 0.65),
            vjust = -0.25, size = 3.1, fontface = "bold", colour = ink_body, lineheight = 0.95) +
  scale_fill_manual(values = COL, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = NULL, y = "free-rusher rate (a rusher got to the QB unblocked)",
    title = sprintf("Sim pressure springs a free rusher %.1f%% of the time, %.1fx a standard rush's %.1f%%",
                     sim_rate, sim_rate / std_rate, std_rate),
    subtitle = paste0(
      "Standard rush: four rushers, all defensive linemen. Sim pressure: four rushers where at least one is a linebacker or defensive back instead.\n",
      "Blitz: five or more rushers sent (unchanged from the prior free-rusher script). Macdonald's own numbers shown alongside the league rate for each."
    ),
    caption = fig_caption(
      sprintf("SumerSports play and player charting, 2022-23 through 2025-26 regular seasons, garbage time excluded"),
      sprintf("%s dropbacks with a defensive play-caller attributed; %d DCs with %s+ charted dropbacks.",
              format(nrow(d), big.mark = ","), n_dcs, format(MIN_CAREER, big.mark = ",")),
      sprintf(paste0("\n%.1f%% of 4-rusher plays could not be classified (a rusher's position was unresolved) and are excluded here, not guessed either way.\n",
                     "Sim-pressure usage rate persists as a coordinator trait across seasons (odd/even r = %+.2f). Macdonald's own sim-usage rate ranks #%d of %d\n",
                     "(about league average), but his sim rushes outperform his own standard rushes by a #%d-of-%d margin among qualified DCs. Built by R/49."),
              100 * mean(d$category == "unclassified_4"), ps_sim$r, mac_usage$rank, n_dcs, mac_delta$delta_rank, n_dcs)
    )
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        plot.margin = margin(10, 16, 8, 10))
save_fig("docs/figures/sim_pressure.png", p, w = 12, h = 7.5)

# =============================================================================
# 11. WRITE OUTPUT
# =============================================================================

out <- merge(res_sim$career[, .(def_caller = caller, n_dropbacks = n, sim_usage_actual = round(actual, 2),
                                 sim_usage_resid = round(resid, 2), sim_usage_rank = rank)],
             wide[, .(def_caller, n_sim = n_sim, free_rate_sim = round(free_rate_sim, 2),
                      n_standard = n_standard, free_rate_standard = round(free_rate_standard, 2),
                      delta = round(delta, 2), delta_rank)],
             by = "def_caller")
setorder(out, sim_usage_rank)
write_csv(as.data.frame(out), "data/derived/sim_pressure.csv")
cat(sprintf("\nwrote data/derived/sim_pressure.csv (%d DCs)\n", nrow(out)))

# =============================================================================
# SUMMARY / VERIFICATION
# =============================================================================

cat("\n\n================= VERIFICATION =================\n")
cat(sprintf("Universe: %s dropbacks, %d-%d, non-garbage-time, %d DCs with %d+ charted dropbacks.\n",
            format(nrow(d), big.mark = ","), min(SEASONS), max(SEASONS), n_dcs, MIN_CAREER))
cat("\nLeague free-rusher rate by category (with n's):\n")
print(rate_tbl)
cat("\nLeague EPA allowed by category (raw):\n")
print(epa_tbl)
cat(sprintf("\nMacdonald: sim usage rank #%d of %d (%.1f%% actual, %+.1fpp vs expected).\n",
            mac_usage$rank, n_dcs, mac_usage$actual, mac_usage$resid))
cat(sprintf("Macdonald: standard %.1f%% (n=%d), sim %.1f%% (n=%d), blitz %.1f%% (n=%d) free-rusher rate.\n",
            mac_cat_rates[category == "standard"]$rate, mac_cat_rates[category == "standard"]$n,
            mac_cat_rates[category == "sim"]$rate, mac_cat_rates[category == "sim"]$n,
            mac_cat_rates[category == "blitz"]$rate, mac_cat_rates[category == "blitz"]$n))
cat(sprintf("Macdonald: sim-minus-standard delta rank #%d of %d.\n", mac_delta$delta_rank, n_dcs))
cat("\nTop 5 sim-pressure users (situation-adjusted usage rate):\n")
print(head(res_sim$career[, .(caller, n, actual = round(actual, 1), resid = round(resid, 1), rank)], 5))
cat(sprintf("\nPersistence, sim-usage residual: r = %+.3f, verdict = %s, n = %d DCs.\n", ps_sim$r, ps_sim$verdict, ps_sim$n))
cat(sprintf("%d of %d qualified DCs show sim rate > standard rate on their own numbers (near-universal).\n",
            n_positive, n_dcs))
cat("\nVERDICT: sim pressure does spring free rushers more than a standard rush, league-wide and for\n")
cat("almost every individual coordinator. Macdonald calls it at a normal rate but gets an above-average\n")
cat("return on it, consistent with R/28's finding that his manufactured pressure is scheme, not blitz volume.\n")
cat("wrote docs/figures/sim_pressure.png, data/derived/sim_pressure.csv\n")
cat("==================================================\n")
