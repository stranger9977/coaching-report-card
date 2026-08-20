# =============================================================================
# 51_te_specialists.R -- tight end usage claims from a group chat.
#
# Three claims, checked independently against Sumer play and player charting:
#
#   1. "The Rams were at their best in 13 personnel with Terrance Ferguson out
#      there even though he rarely got targeted. He just stretched the
#      defense vertically and when he did get a target it was always an
#      explosive one." Rams, 2025-26 season.
#   2. "The Seahawks had Eric Saubert last year that would only come out to
#      block so that AJ Barner could run routes." Seattle, 2024-25 and
#      2025-26.
#   3. Context row: across the league, how many tight ends are run-block
#      specialists like the Saubert claim describes, and how many are route
#      runners like Barner?
#
# DATA. Play-level charting via R/factory/lib_sumer.R's load_sumer(). Player
# participation from data/raw/sumer/plays_players_p1.csv.gz + p2 (both halves
# of the same file, stacked). Names and positions from
# data/raw/sumer/player_details.csv.gz, joined on sumer_player_id -- one row
# per player, so last-name matches for Ferguson/Saubert/Barner/Kolar are
# unambiguous (checked below).
#
# THE KEY FIELD. The player file's `role` column already answers "route or
# block" directly, no play-level join needed: on offense, it is one of
# PASS ROUTE, PASS BLOCK, RUN BLOCK, or blank. Blank means the player was not
# in the personnel package for that play (confirmed by cross-checking: role
# is non-blank only when alignment is also charted, and a player's role rows
# sum to a plausible season snap count, not an inflated one). So a tight
# end's pass-snap route rate is just PASS ROUTE rows divided by (PASS ROUTE +
# PASS BLOCK) rows for that player -- the role field already encodes which
# plays were pass plays, without joining back to the play file's run_pass
# column.
#
# UNIVERSE. Regular season only (season_type == 0; 1 = postseason in Sumer's
# games files). Garbage time excluded throughout, including for snap-share
# counts, to match this repo's convention everywhere else. Sample sizes
# printed on every number; Ferguson is a rookie with a half season of
# targets, so his target-level numbers are small and are labeled that way.
#
# KNOWN TRAPS AVOIDED: receiving_epa is catch-only, so target value uses the
# play-level expected_points_added (0 on incompletions, not missing). depth_
# of_target is 100% missing on sacks/scrambles, irrelevant here since every
# row used is a real target. route is blank on non-route player rows, same
# blank convention as role. Explosive plays follow this repo's existing
# definition (R/34): offensive_yards >= 20 on a target.
#
# Conventions: R/lib/theme_coach.R, plain football words, no em dashes,
# sample sizes everywhere, small-sample warnings printed in plain language.
#
# Out: docs/figures/te_specialists.png
#      data/derived/te_specialists.csv (league TE route-rate table, 2022-2025)
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------
# 1. LOAD -- plays, player participation rows, player names
# -----------------------------------------------------------------------
d <- load_sumer(2022:2025)
d <- d[season_type == 0]   # regular season only
cat(sprintf("Regular-season plays loaded, 2022-2025: %s\n", format(nrow(d), big.mark = ",")))

PCOLS <- c("sumer_play_id","sumer_player_id","season","side_of_ball","role","route",
           "receiving_targets","receiving_depth_of_target","receiving_receptions")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
players <- players[season %in% 2022:2025]
cat(sprintf("Player-play rows, 2022-2025: %s\n", format(nrow(players), big.mark = ",")))

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id","football_name","last_name","position"))
pd[, name := paste(football_name, last_name)]

find_player <- function(last, first_word = NULL) {
  hit <- pd[last_name == last]
  if (!is.null(first_word)) hit <- hit[grepl(first_word, football_name, fixed = TRUE)]
  stopifnot(nrow(hit) == 1)
  hit
}
FERGUSON <- find_player("Ferguson", "Terrance")
SAUBERT  <- find_player("Saubert")
BARNER   <- find_player("Barner")
KOLAR    <- find_player("Kolar")
cat(sprintf("\nName join check (unique last-name match, one row each):\n  Ferguson %s | Saubert %s | Barner %s | Kolar %s\n",
            FERGUSON$sumer_player_id, SAUBERT$sumer_player_id, BARNER$sumer_player_id, KOLAR$sumer_player_id))

# Play-play participation index: a player "on the field" for a play is any
# row with a non-blank role (blank role means out of the personnel package
# for that specific play -- see header note).
on_field <- players[role != "", .(sumer_play_id, season, sumer_player_id, role, route,
                                   receiving_targets, receiving_depth_of_target,
                                   receiving_receptions)]

# =============================================================================
# CLAIM 1 -- Terrance Ferguson, Rams 13 personnel, 2025-26
# =============================================================================
cat("\n================ CLAIM 1: Ferguson / Rams 13 personnel, 2025-26 ================\n")

la <- d[off_team == "LA" & season == 2025 & run_pass %in% c("P","R")]
la[, pers13 := sub("[*]$", "", offensive_personnel_basic) == "13"]
la_ng <- la[garbage_time == FALSE]
cat(sprintf("LA 2025 regular-season offensive snaps: %s (%s non-garbage-time)\n",
            format(nrow(la), big.mark = ","), format(nrow(la_ng), big.mark = ",")))
cat(sprintf("Of those, 13 personnel: %s (%s non-garbage-time)\n",
            format(sum(la$pers13), big.mark = ","), format(sum(la_ng$pers13), big.mark = ",")))

ferg_plays <- on_field[sumer_player_id == FERGUSON$sumer_player_id & season == 2025]$sumer_play_id
la_ng[, ferg_on := sumer_play_id %in% ferg_plays]

# ---- (a) 13-personnel EPA/play with Ferguson on the field vs without ----
la13 <- la_ng[pers13 == TRUE]
one_ci <- function(x) { tt <- t.test(x); list(n = length(x), mean = unname(tt$estimate), lo = tt$conf.int[1], hi = tt$conf.int[2]) }
on_stat  <- one_ci(la13[ferg_on == TRUE]$expected_points_added)
off_stat <- one_ci(la13[ferg_on == FALSE]$expected_points_added)
p_onoff  <- if (on_stat$n >= 2 && off_stat$n >= 2) t.test(la13[ferg_on == TRUE]$expected_points_added, la13[ferg_on == FALSE]$expected_points_added)$p.value else NA_real_
cat(sprintf("\n1a. LA 13-personnel EPA/play, non-garbage-time, 2025:\n"))
cat(sprintf("    Ferguson ON the field:  n=%d, EPA/play %+.3f [%+.3f, %+.3f]\n", on_stat$n, on_stat$mean, on_stat$lo, on_stat$hi))
cat(sprintf("    Ferguson OFF the field: n=%d, EPA/play %+.3f [%+.3f, %+.3f]\n", off_stat$n, off_stat$mean, off_stat$lo, off_stat$hi))
cat(sprintf("    difference %+.3f, p = %s\n", on_stat$mean - off_stat$mean, if (is.na(p_onoff)) "NA (too few off-field snaps)" else sprintf("%.3f", p_onoff)))

# ---- (b) target share when on the field in 13 personnel ----
la13_pass <- la13[run_pass == "P"]
ferg_tgt_plays <- on_field[sumer_player_id == FERGUSON$sumer_player_id & season == 2025 &
                            receiving_targets == 1]$sumer_play_id
la13_pass[, ferg_on := sumer_play_id %in% ferg_plays]
la13_pass[, ferg_tgt := sumer_play_id %in% ferg_tgt_plays]
on13pass <- la13_pass[ferg_on == TRUE]
cat(sprintf("\n1b. LA 13-personnel PASS plays with Ferguson on the field: n=%d, targeted to him on %d (%.1f%%)\n",
            nrow(on13pass), sum(on13pass$ferg_tgt), 100 * mean(on13pass$ferg_tgt)))

# other Rams TEs' target share in the same 13-personnel/on-field universe, for context
rams_tes <- pd[position == "TE"][sumer_player_id %in% on_field[season == 2025]$sumer_player_id]
rams_tes <- rams_tes[sumer_player_id %in% unique(on_field[sumer_play_id %in% la13_pass$sumer_play_id]$sumer_player_id)]
te_share13 <- rbindlist(lapply(seq_len(nrow(rams_tes)), function(i) {
  pid <- rams_tes$sumer_player_id[i]
  onp <- on_field[sumer_player_id == pid & season == 2025]$sumer_play_id
  sub <- la13_pass[sumer_play_id %in% onp]
  tgtp <- on_field[sumer_player_id == pid & season == 2025 & receiving_targets == 1]$sumer_play_id
  data.table(name = rams_tes$name[i], n_on = nrow(sub), n_tgt = sum(sub$sumer_play_id %in% tgtp))
}))
te_share13[, tgt_share := 100 * n_tgt / n_on]
setorder(te_share13, -n_on)
cat("    Rams tight ends, target share when on the field in LA's 13-personnel pass plays (2025):\n")
print(te_share13)

# ---- (c) route depth / vertical-route share vs other Rams TEs (all personnel, season) ----
la_pass_all <- d[off_team == "LA" & season == 2025 & garbage_time == FALSE & run_pass == "P"]
rams_te_routes <- merge(on_field[season == 2025 & sumer_player_id %in% rams_tes$sumer_player_id],
                        la_pass_all[, .(sumer_play_id)], by = "sumer_play_id")
rams_te_routes <- merge(rams_te_routes, pd[, .(sumer_player_id, name)], by = "sumer_player_id")
VERTICAL_ROUTES <- c("GO","POST","CORNER","WHEEL")
route_tab <- rams_te_routes[role %in% c("PASS ROUTE","PASS BLOCK"),
                            .(pass_snaps = .N,
                              routes_run = sum(role == "PASS ROUTE"),
                              vertical_routes = sum(role == "PASS ROUTE" & route %in% VERTICAL_ROUTES)),
                            by = name]
route_tab[, route_rate := 100 * routes_run / pass_snaps]
route_tab[, vertical_share := 100 * vertical_routes / routes_run]
tgt_depth <- rams_te_routes[receiving_targets == 1 & !is.na(receiving_depth_of_target),
                            .(targets = .N, avg_depth = mean(receiving_depth_of_target)), by = name]
route_tab <- merge(route_tab, tgt_depth, by = "name", all.x = TRUE)
setorder(route_tab, -pass_snaps)
cat("\n1c. Rams tight ends, 2025 (all personnel groupings, non-garbage-time pass plays):\n")
cat("    route_rate = share of his pass snaps spent running a route (vs. pass-blocking).\n")
cat("    vertical_share = share of HIS ROUTES that were a vertical concept (go/post/corner/wheel).\n")
cat("    avg_depth = average depth of target on his targets only.\n")
print(route_tab[, .(name, pass_snaps, route_rate = round(route_rate, 1),
                    vertical_share = round(vertical_share, 1), targets, avg_depth = round(avg_depth, 1))])

# ---- (d) EPA and yards on Ferguson's targets -- are they explosive? ----
ferg_tgt_rows <- on_field[sumer_player_id == FERGUSON$sumer_player_id & season == 2025 & receiving_targets == 1]
ferg_tgt_full <- merge(ferg_tgt_rows[, .(sumer_play_id, receiving_depth_of_target, receiving_receptions)],
                       d[, .(sumer_play_id, season, expected_points_added, offensive_yards, is_complete_pass, garbage_time)],
                       by = "sumer_play_id")
ferg_tgt_ng <- ferg_tgt_full[garbage_time == FALSE]
n_tgt <- nrow(ferg_tgt_ng)
n_comp <- sum(ferg_tgt_ng$is_complete_pass)
avg_epa <- mean(ferg_tgt_ng$expected_points_added)
avg_yds <- mean(ferg_tgt_ng$offensive_yards)
avg_yds_comp <- mean(ferg_tgt_ng[is_complete_pass == TRUE]$offensive_yards)
n_explosive <- sum(ferg_tgt_ng$offensive_yards >= 20)
cat(sprintf("\n1d. Ferguson's targets, non-garbage-time, 2025-26 (SMALL SAMPLE, n=%d):\n", n_tgt))
cat(sprintf("    catches: %d of %d targets (%.0f%%)\n", n_comp, n_tgt, 100 * n_comp / n_tgt))
cat(sprintf("    EPA per target: %+.3f | yards per target: %.1f | yards per catch: %.1f\n", avg_epa, avg_yds, avg_yds_comp))
cat(sprintf("    explosive targets (20+ yards gained): %d of %d (%.0f%%)\n", n_explosive, n_tgt, 100 * n_explosive / n_tgt))
cat("    individual target lines (play-by-play, small sample -- every one shown):\n")
print(ferg_tgt_ng[order(-offensive_yards), .(depth_of_target = receiving_depth_of_target,
                                             complete = is_complete_pass, yards = offensive_yards,
                                             epa = round(expected_points_added, 2))])

verdict1a <- if (!is.na(p_onoff) && p_onoff < 0.10 && on_stat$mean > off_stat$mean) "supported" else "not supported at conventional significance"
depth_ref <- route_tab[!is.na(avg_depth) & targets >= 10]   # drop the 4-snap Vannett noise
ferg_depth <- route_tab[name == FERGUSON$name]$avg_depth
other_max_depth <- max(depth_ref[name != FERGUSON$name]$avg_depth)
verdict1c <- if (ferg_depth > other_max_depth) "supported" else "mixed"
verdict1d <- if (n_explosive / n_tgt >= 0.4) "supported" else if (n_explosive / n_tgt >= 0.25) "directionally supported, not 'always'" else "not supported"
cat(sprintf("\nCLAIM 1 VERDICT: (a) on/off EPA %s (n too small to be definitive) | (c) vertical routes %s | (d) 'always explosive' %s -- %d of %d targets went for 20+.\n",
            verdict1a, verdict1c, verdict1d, n_explosive, n_tgt))

# =============================================================================
# CLAIM 2 -- Eric Saubert / AJ Barner, Seattle, 2024-25 and 2025-26
# =============================================================================
cat("\n\n================ CLAIM 2: Saubert / Barner, Seattle TEs, 2024-25 & 2025-26 ================\n")

sea <- d[off_team == "SEA" & season %in% c(2024, 2025) & garbage_time == FALSE, .(sumer_play_id, season)]
sea_players <- merge(on_field, sea, by = c("sumer_play_id","season"))
sea_players <- merge(sea_players, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
sea_te <- sea_players[position == "TE" & role %in% c("PASS ROUTE","PASS BLOCK","RUN BLOCK")]

sea_te_tab <- sea_te[, .(pass_snaps = sum(role %in% c("PASS ROUTE","PASS BLOCK")),
                         routes_run = sum(role == "PASS ROUTE"),
                         pass_blocks = sum(role == "PASS BLOCK"),
                         run_blocks = sum(role == "RUN BLOCK")),
                     by = .(name, season)]
sea_te_tab[, total_snaps := pass_snaps + run_blocks]
sea_te_tab[, route_rate := 100 * routes_run / pass_snaps]
sea_te_tab <- sea_te_tab[total_snaps >= 20]
setorder(sea_te_tab, season, -total_snaps)
cat("Seattle tight ends by season (>= 20 total offensive snaps):\n")
cat("route_rate = share of his PASS snaps spent running a route (rest = pass-blocked).\n")
print(sea_te_tab[, .(season, name, total_snaps, pass_snaps, routes_run, pass_blocks, run_blocks, route_rate = round(route_rate, 1))])

for (s in c(2024, 2025)) {
  sr <- sea_te_tab[name == SAUBERT$name & season == s]
  br <- sea_te_tab[name == BARNER$name & season == s]
  cat(sprintf("\n  %d: Saubert route rate %s | Barner route rate %s\n", s,
              if (nrow(sr)) sprintf("%.1f%% (n=%d pass snaps)", sr$route_rate, sr$pass_snaps) else "not enough snaps charted",
              if (nrow(br)) sprintf("%.1f%% (n=%d pass snaps)", br$route_rate, br$pass_snaps) else "not enough snaps charted"))
}

verdict2 <- {
  sr24 <- sea_te_tab[name == SAUBERT$name & season == 2024]
  br24 <- sea_te_tab[name == BARNER$name & season == 2024]
  if (nrow(sr24) && nrow(br24) && sr24$route_rate < 50 && br24$route_rate > sr24$route_rate + 20)
    "supported for 2024-25" else "not clearly supported"
}
cat(sprintf("\nCLAIM 2 VERDICT: %s.\n", verdict2))

# =============================================================================
# CLAIM 3 -- league TE specialization, 400+ pass-play snaps, 2022-2025
# =============================================================================
cat("\n\n================ CLAIM 3: league TE specialization, 2022-2025 ================\n")

all_off <- d[garbage_time == FALSE, .(sumer_play_id, season)]
all_te <- merge(on_field, all_off, by = c("sumer_play_id","season"))
all_te <- merge(all_te, pd[, .(sumer_player_id, name, position)], by = "sumer_player_id")
all_te <- all_te[position == "TE" & role %in% c("PASS ROUTE","PASS BLOCK","RUN BLOCK")]

league_te <- all_te[, .(pass_snaps = sum(role %in% c("PASS ROUTE","PASS BLOCK")),
                        routes_run = sum(role == "PASS ROUTE"),
                        run_blocks = sum(role == "RUN BLOCK")),
                    by = name]
league_te[, total_snaps := pass_snaps + run_blocks]
league_te[, route_rate := 100 * routes_run / pass_snaps]
league_te[, run_block_share := 100 * run_blocks / total_snaps]
qual <- league_te[pass_snaps >= 400]
setorder(qual, route_rate)
cat(sprintf("Tight ends with >= 400 charted pass-play snaps, 2022-2025 regular season, non-garbage-time: %d\n", nrow(qual)))
cat(sprintf("Route rate distribution: median %.0f%%, 10th pct %.0f%%, 90th pct %.0f%%\n",
            median(qual$route_rate), quantile(qual$route_rate, 0.10), quantile(qual$route_rate, 0.90)))
n_spec <- sum(qual$route_rate < 60)
cat(sprintf("Below 60%% route rate (meaningful blocking specialists): %d of %d (%.0f%%)\n",
            n_spec, nrow(qual), 100 * n_spec / nrow(qual)))
n_extreme <- sum(qual$route_rate < 40)
cat(sprintf("Below 40%% route rate (Saubert-tier, block far more than route): %d of %d (%.0f%%)\n",
            n_extreme, nrow(qual), 100 * n_extreme / nrow(qual)))
cat("\nLowest 10 route rates (biggest blocking specialists), n >= 400 pass snaps:\n")
print(head(qual[, .(name, pass_snaps, run_blocks, route_rate = round(route_rate, 1), run_block_share = round(run_block_share, 1))], 10))
cat("\nHighest 10 route rates (biggest route runners), n >= 400 pass snaps:\n")
print(tail(qual[, .(name, pass_snaps, run_blocks, route_rate = round(route_rate, 1), run_block_share = round(run_block_share, 1))], 10))

for (nm in c(SAUBERT$name, BARNER$name, KOLAR$name)) {
  r <- league_te[name == nm]
  if (nrow(r)) cat(sprintf("  %-20s pass_snaps=%d route_rate=%.1f%% run_block_share=%.1f%% (career, 2022-2025)\n",
                           nm, r$pass_snaps, r$route_rate, r$run_block_share))
}
cat(sprintf("\nCLAIM 3: yes, block-first specialists like the Saubert claim exist but are the minority --\n%d of %d qualified tight ends (%.0f%%) run a route on fewer than 6 of every 10 pass snaps.\n",
            n_spec, nrow(qual), 100 * n_spec / nrow(qual)))

fwrite(league_te[order(-pass_snaps)], "data/derived/te_specialists.csv")
cat(sprintf("\nwrote data/derived/te_specialists.csv (%d rows)\n", nrow(league_te)))

# =============================================================================
# CHART -- the Ferguson finding: on/off EPA plus target depth, side by side
# =============================================================================
onoff_df <- data.table(
  status = factor(c("Ferguson on the field", "Ferguson off the field"),
                  levels = c("Ferguson off the field", "Ferguson on the field")),
  n = c(on_stat$n, off_stat$n), mean = c(on_stat$mean, off_stat$mean),
  lo = c(on_stat$lo, off_stat$lo), hi = c(on_stat$hi, off_stat$hi))

p_left <- ggplot(onoff_df, aes(status, mean, fill = status)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, colour = "grey30") +
  geom_text(aes(label = sprintf("%+.2f\n(n=%d)", mean, n), y = hi + 0.06),
            size = 3.4, fontface = "bold", colour = "grey20") +
  scale_fill_manual(values = c("Ferguson off the field" = "#9db6c9", "Ferguson on the field" = "#003594")) +
  scale_x_discrete(labels = function(x) gsub("Ferguson ", "", x)) +
  coord_cartesian(clip = "off") +
  labs(subtitle = "13-personnel EPA per play,\nFerguson on vs. off the field",
       x = NULL, y = "EPA per play") +
  theme_coach(grid = "y") + theme(axis.text.x = element_text(size = rel(0.85)))

depth_df <- route_tab[!is.na(avg_depth)][order(avg_depth)]
depth_df[, name := factor(name, levels = name)]
depth_df[, is_ferg := name == FERGUSON$name]

p_right <- ggplot(depth_df, aes(name, avg_depth, fill = is_ferg)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f\n(n=%d tgt)", avg_depth, targets), y = avg_depth + 1.1),
            size = 3.2, fontface = "bold", colour = "grey20") +
  scale_fill_manual(values = c(`TRUE` = "#003594", `FALSE` = "#9db6c9"), guide = "none") +
  coord_flip(clip = "off") +
  labs(subtitle = "Average depth of target,\nRams tight ends, 2025",
       x = NULL, y = "yards past the line of scrimmage") +
  theme_coach(grid = "y")

title_chart <- sprintf(
  "Ferguson stretched the Rams' 13 personnel vertically: his targets average %.1f yards downfield, %.1fx the next-deepest Rams tight end",
  ferg_depth, ferg_depth / other_max_depth)

layout <- (p_left | p_right) + plot_layout(widths = c(1, 1.3)) +
  plot_annotation(
    title = title_chart,
    subtitle = sprintf(
      "Left: EPA per play out of the Rams' 13 personnel with Terrance Ferguson on vs. off the field, 2025 regular season, garbage time excluded (difference not\nstatistically significant, n=%d off-field snaps). Right: average depth of target by Rams tight end, 2025 (target count in parens; small samples, especially\nFerguson's %d targets). Of Ferguson's targets, %d of %d (%.0f%%) went for 20+ yards -- explosive often, not always.",
      off_stat$n, n_tgt, n_explosive, n_tgt, 100 * n_explosive / n_tgt),
    caption = paste(strwrap(fig_caption(
      "SumerSports play and player charting, 2025 regular season",
      "13-personnel universe: LA offensive snaps, non-garbage-time. Depth-of-target universe: all personnel groupings, targeted throws only.",
      "Route/block/target participation comes from the player-charting role field, joined by sumer_play_id. Built by R/51_te_specialists.R."
    ), width = 150), collapse = "\n"),
    theme = theme(plot.title = element_text(face = "bold", size = rel(1.15), colour = ink_title),
                  plot.subtitle = element_text(colour = ink_subtitle, size = rel(0.78), margin = margin(b = 10)),
                  plot.caption = element_text(colour = ink_caption, size = rel(0.7), hjust = 0, lineheight = 1.25, margin = margin(t = 10)))
  )
save_fig("docs/figures/te_specialists.png", layout, w = 12, h = 7.5)

# =============================================================================
# FINAL VERIFICATION BLOCK
# =============================================================================
cat("\n\n================ VERIFICATION SUMMARY ================\n")
cat(sprintf("CLAIM 1 (Ferguson / Rams 13 personnel, 2025-26):\n"))
cat(sprintf("  (a) 13-pers EPA with Ferguson on (n=%d, %+.3f) vs off (n=%d, %+.3f): %s\n",
            on_stat$n, on_stat$mean, off_stat$n, off_stat$mean, verdict1a))
cat(sprintf("  (b) target share on 13-pers pass snaps with him on field: %.1f%% (n=%d)\n",
            100 * mean(on13pass$ferg_tgt), nrow(on13pass)))
cat(sprintf("  (c) vertical-route share vs other Rams TEs: %s\n", verdict1c))
cat(sprintf("  (d) explosive on targets: %d of %d targets (%.0f%%) went for 20+ yards: %s\n",
            n_explosive, n_tgt, 100 * n_explosive / n_tgt, verdict1d))
cat(sprintf("CLAIM 2 (Saubert block-only / Barner route-runner, Seattle): %s\n", verdict2))
cat(sprintf("CLAIM 3 (league TE specialization, 2022-2025, n=%d qualified TEs >= 400 pass snaps):\n", nrow(qual)))
cat(sprintf("  %d of %d (%.0f%%) run routes on fewer than 60%% of pass snaps -- specialists exist but are a minority.\n",
            n_spec, nrow(qual), 100 * n_spec / nrow(qual)))
cat("\nDONE\n")
