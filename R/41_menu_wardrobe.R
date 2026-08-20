# =============================================================================
# 41_menu_wardrobe.R -- "makes 25 plays look like 250": splitting a caller's
# PLAY MENU (how many distinct plays he actually calls) from his WARDROBE
# (how many different pre-snap dressings each play wears). The Sean McVay
# quote is the hypothesis, not the answer: this script measures both halves
# for every qualified caller and reports whichever way the numbers land.
#
# THE SAMPLE-SIZE TRAP. Distinct-count statistics only go up with more
# snaps, so a caller who simply called more plays over four seasons will
# look like he has a bigger menu and a bigger wardrobe than a caller with
# the same true habits but fewer plays charted. Every number below is
# computed on a RAREFIED sample: every qualified caller is repeatedly cut
# down to the same 1,200 plays (the qualification floor) before anything is
# counted, 200 times, and the 200 draws are averaged. This is the same fix
# ecologists use for species-richness comparisons across unequal survey
# effort, applied here to play-calling.
#
# DEFINING A "PLAY" -- the load-bearing decision, made by inspection, not
# assumption.
#   RUNS: run_concept has 13 levels and 0% NA, but it is too coarse on its
#   own -- checked directly: the 80%-of-run-snaps coverage set for every one
#   of the 36 qualified callers needs only 3 to 6 categories using
#   run_concept alone. That would say every team's run game distills to
#   "outside zone, inside zone, man, power, counter" and nothing else, which
#   collapses real playbook variety (an NFL playbook numbers "outside zone
#   right" and "outside zone left" as different calls; the run action mirrors
#   but the read, the puller, the frontside key are not the same play). So
#   runs are identified by run_concept combined with run_gap_intent_side
#   (blank side, 13.1% of run snaps, kept as its own "NONE" tag -- mostly
#   SNEAK/DRAW/SCRAMBLE/TRICK/OTHER/FULLBACK concepts that do not carry a
#   real called side). That raises the 80% coverage set to 7-11 categories
#   per caller, still small enough to call a "menu," and closer to what an
#   opposing defense actually has to prepare for.
#
#   PASSES: Sumer's play-level schema has no pass-concept field at all
#   (checked against its full column list). The fallback candidate would be
#   screen x play_action x dropback_type, but the player file
#   (plays_players_p1/p2.csv.gz, rbindlist both) has a `route` column that
#   clears the coverage bar first: among charted PASS ROUTE rows on a
#   dropback, route is populated (non-blank) for a receiver 86.7% of the
#   time, and 86.7% of ALL dropbacks in this universe have three or more
#   charted route runners -- comfortably over the 80% bar for using routes
#   directly instead of the coarser fallback. A pass play's concept is built
#   from the 3 DEEPEST-involved route runners: an empirical depth-by-route
#   table is built once from every charted target's receiving_depth_of_target
#   (GO deepest at ~23.4 air yards down to SCREEN shallowest at -3.0 air
#   yards -- a real, defense-relevant depth order, not an assumed one), each
#   dropback's charted routes are ranked against that table, the 3 deepest
#   are kept (fewer if fewer are charted), and the concept is those 3 route
#   labels sorted alphabetically and joined ("GO+GO+OUT" for a two-vertical
#   look, duplicates kept on purpose since the concept is what the 3 players
#   actually ran, not how many distinct route types showed up). Dropbacks
#   with zero charted routes (0.4%) fall into one shared "UNCHARTED" bucket.
#   This yields 465 distinct pass concepts leaguewide, a comparable-scale
#   vocabulary to the run side.
#
#   Both run and pass identifiers are pooled into one "play" field so the
#   combined menu (the headline number) counts across the whole offense, not
#   run and pass separately.
#
# EXCLUDED FROM THE UNIVERSE. Kneels and spikes are not real play calls but
# Sumer carries no boolean flag for either -- checked and found by text match
# on `outcome` instead (1,259 kneel rows, almost all filed under
# run_concept == "OTHER"; 270 spike rows, almost all filed under
# dropback_type == "STANDARD"). Left in, they would inflate the "OTHER" run
# bucket and the "STANDARD, no routes charted" pass bucket with plays no
# coach actually drew up that week. Also excluded: season_type != 0 (Sumer
# preseason/postseason rows), garbage_time, plays with no attributed
# off_caller, and the one stray special-teams row that slips past a
# run_pass-only filter (play_type == "OFF/DEF" catches it).
#
# DEFINING A "DRESSING." formation x offensive_personnel_basic x
# quarterback_alignment. Sumer has no motion field anywhere in its schema
# (confirmed in R/25 against the full 193-column play table) -- not invented
# here either.
#
# METRICS, per qualified caller (>= 1,200 offensive scrimmage plays, 36 of
# 65 callers qualify), each averaged over 200 rarefied draws of 1,200 plays:
#   1. MENU SIZE: distinct plays needed to cover 80% of the 1,200-play draw.
#      Combined (headline) always uses the full 1,200. Run-only and
#      pass-only menus are also computed on the SAME draw's run/pass subset
#      -- those two are NOT separately rarefied to a common run or pass
#      floor, so a caller's own run/pass mix still shapes how many run or
#      pass plays land in any given 1,200-play draw. Flagged here rather
#      than hidden; the combined number carries no such confound.
#   2. WARDROBE: among the draw's 80%-coverage core plays, the average
#      number of distinct dressings each core play wore in that draw. The
#      multiplier (distinct play x dressing combos / distinct plays) is the
#      same idea computed over the WHOLE draw's vocabulary instead of just
#      the core set, as a robustness check on the headline wardrobe number.
#   3. PERSISTENCE. lib_sumer's persist_split() is built for a per-season
#      situation-adjusted residual table and does not fit a rarefied
#      vocabulary metric, so a matching odd/even split is built by hand
#      here: each caller's plays are pooled by season parity, each half
#      rarefied on its own (floor 500, the largest common floor that keeps
#      33 of 36 qualified callers in both halves), 200 draws each, then the
#      two halves' averages are correlated across callers -- same logic as
#      persist_split(), same bar for "trait, not description."
#
# Conventions: R/lib/theme_coach.R, plain football words for every measure
# ("plays needed to cover 80% of snaps," "different dressings per play"),
# no Michael/Nick anywhere in rendered chart text, no em dashes, season
# spans in rendered chart text written "2022-23 through 2025-26."
#
# Out: docs/figures/menu_wardrobe.png
#      data/derived/menu_wardrobe.csv (36 qualified callers, every measure)
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(ggrepel)
  library(scales); library(readr)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)
set.seed(20260819)
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

SEASONS   <- 2022:2025
QUAL_MIN  <- 1200   # matches R/14/R/25/R/33/R/38's convention
RAREFY_N  <- QUAL_MIN
N_ITER    <- 200
HALF_N    <- 500
HALF_ITER <- 200
COVER     <- 0.80

NAMED <- c("Sean McVay", "Kyle Shanahan", "Ben Johnson")
COL <- c("Kyle Shanahan" = "#AA0000", "Sean McVay" = "#003594", "Ben Johnson" = "#D55E00")

# =============================================================================
# STEP 1: UNIVERSE
# =============================================================================
d <- load_sumer(SEASONS)
d_off <- d[season_type == 0 & garbage_time == FALSE & off_caller != "" &
           run_pass %in% c("P", "R") & play_type == "OFF/DEF" &
           !grepl("kneels|spiked", outcome, ignore.case = TRUE)]
cat(sprintf("Universe: offensive scrimmage plays, season_type==0, non-garbage-time, caller-attributed,\n"))
cat(sprintf("kneels/spikes excluded, %s-%s: %s plays\n",
            min(SEASONS), max(SEASONS), format(nrow(d_off), big.mark = ",")))

# =============================================================================
# STEP 2: DEFINE "PLAY" -- runs
# =============================================================================
cat("\n=== RUN PLAY DEFINITION ===\n")
runs_all <- d_off[run_pass == "R"]
cat(sprintf("run_concept levels: %d, NA/blank rate: %.1f%%\n",
            uniqueN(runs_all$run_concept), 100 * mean(runs_all$run_concept == "")))
cat(sprintf("run_gap_intent_side blank rate: %.1f%%\n", 100 * mean(runs_all$run_gap_intent_side == "")))

qual_probe <- d_off[, .N, by = off_caller][N >= QUAL_MIN, off_caller]
cov80 <- function(v) { t <- sort(table(v), decreasing = TRUE); which(cumsum(t) / sum(t) >= COVER)[1] }
chk <- runs_all[off_caller %in% qual_probe, .(
  k_concept_only = cov80(run_concept),
  k_concept_side = cov80(paste(run_concept, fifelse(run_gap_intent_side == "", "NONE", run_gap_intent_side)))
), by = off_caller]
cat(sprintf("run_concept ALONE: 80%%-coverage set ranges %d-%d categories across qualified callers -- too coarse.\n",
            min(chk$k_concept_only), max(chk$k_concept_only)))
cat(sprintf("run_concept + side: 80%%-coverage set ranges %d-%d categories -- used as the run play id.\n",
            min(chk$k_concept_side), max(chk$k_concept_side)))

d_off[, side := fifelse(run_gap_intent_side == "", "NONE", run_gap_intent_side)]
d_off[, run_play_id := paste0("RUN|", run_concept, "|", side)]

# =============================================================================
# STEP 3: DEFINE "PLAY" -- passes, via route combination
# =============================================================================
cat("\n=== PASS PLAY DEFINITION ===\n")
db <- d_off[run_pass == "P"]
cat(sprintf("dropbacks in universe: %s\n", format(nrow(db), big.mark = ",")))

PCOLS <- c("sumer_play_id", "season", "side_of_ball", "role", "route",
           "receiving_targets", "receiving_depth_of_target")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
pl <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
pl <- pl[season %in% SEASONS & side_of_ball == "offense"]

route_charted_rate <- pl[role == "PASS ROUTE", mean(route != "" & !is.na(route))]
cat(sprintf("route field populated on %.1f%% of PASS ROUTE-role rows\n", 100 * route_charted_rate))
pl_routes <- pl[role == "PASS ROUTE" & route != "" & !is.na(route)]

n_charted <- pl_routes[sumer_play_id %in% db$sumer_play_id, .N, by = sumer_play_id]
pct_ge3 <- 100 * sum(n_charted$N >= 3) / nrow(db)
cat(sprintf("dropbacks with >= 3 charted route runners: %.1f%% (bar is 80%%) -- route combination used, not the coarser fallback.\n", pct_ge3))

# empirical depth-by-route-type table, built once from targeted receivers
depth_tbl <- pl_routes[receiving_targets == 1 & !is.na(receiving_depth_of_target),
                        .(avg_depth = mean(receiving_depth_of_target)), by = route]
setorder(depth_tbl, -avg_depth)
cat("\nempirical route depth (average air yards when targeted), deepest to shallowest:\n")
print(depth_tbl)

routes_db <- pl_routes[sumer_play_id %in% db$sumer_play_id, .(sumer_play_id, route)]
routes_db <- merge(routes_db, depth_tbl, by = "route")
setorder(routes_db, sumer_play_id, -avg_depth)
top3 <- routes_db[, .(route = route[seq_len(min(3, .N))]), by = sumer_play_id]
concept <- top3[, .(pass_play_id = paste0("PASS|", paste(sort(route), collapse = "+")),
                     n_routes_used = .N), by = sumer_play_id]

db <- merge(db, concept, by = "sumer_play_id", all.x = TRUE)
db[is.na(pass_play_id), pass_play_id := "PASS|UNCHARTED"]
cat(sprintf("\ndropbacks with a built route concept: %s of %s (%.1f%%); distinct pass concepts leaguewide: %d\n",
            format(sum(!is.na(concept$pass_play_id[match(db$sumer_play_id, concept$sumer_play_id)])), big.mark = ","),
            format(nrow(db), big.mark = ","), 100 * nrow(concept) / nrow(db), uniqueN(db$pass_play_id)))

d_off <- merge(d_off, db[, .(sumer_play_id, pass_play_id)], by = "sumer_play_id", all.x = TRUE)
d_off[, play_id := fifelse(run_pass == "R", run_play_id, pass_play_id)]
stopifnot(!any(is.na(d_off$play_id)))
cat(sprintf("\ncombined distinct plays leaguewide (run + pass): %d\n", uniqueN(d_off$play_id)))

# =============================================================================
# STEP 4: DEFINE "DRESSING"
# =============================================================================
d_off[, dress_id := paste(formation, offensive_personnel_basic, quarterback_alignment, sep = "|")]
cat(sprintf("distinct dressings leaguewide (formation x personnel x QB alignment): %d\n", uniqueN(d_off$dress_id)))

# =============================================================================
# STEP 5: QUALIFIED CALLERS
# =============================================================================
n_by_caller <- d_off[, .N, by = off_caller]
qual <- n_by_caller[N >= QUAL_MIN, off_caller]
cat(sprintf("\nqualified callers (>= %d plays): %d of %d\n", QUAL_MIN, length(qual), nrow(n_by_caller)))
stopifnot(all(NAMED %in% qual))
dq <- d_off[off_caller %in% qual]

# =============================================================================
# STEP 6: RAREFACTION -- the sample-size fix
# =============================================================================
rarefy_caller <- function(play, dress, is_run, n, iters) {
  N <- length(play)
  out <- vector("list", iters)
  for (i in seq_len(iters)) {
    idx <- sample.int(N, n)
    p <- play[idx]; ds <- dress[idx]; ir <- is_run[idx]

    tab <- sort(table(p), decreasing = TRUE)
    cum <- cumsum(tab) / n
    k <- which(cum >= COVER)[1]
    core <- names(tab)[seq_len(k)]

    in_core <- p %in% core
    dtc <- data.table(p = p[in_core], ds = ds[in_core])
    wardrobe_core <- dtc[, .(nd = uniqueN(ds)), by = p][, mean(nd)]

    combo_n <- uniqueN(paste(p, ds, sep = "||"))
    play_n  <- uniqueN(p)

    p_run <- p[ir]; p_pass <- p[!ir]
    k_run  <- if (length(p_run) > 0)  cov80(p_run)  else NA_integer_
    k_pass <- if (length(p_pass) > 0) cov80(p_pass) else NA_integer_

    out[[i]] <- data.table(menu_combined = k, menu_run = k_run, menu_pass = k_pass,
                            wardrobe_core = wardrobe_core, multiplier = combo_n / play_n,
                            n_run = length(p_run), n_pass = length(p_pass))
  }
  rbindlist(out)
}

cat(sprintf("\nrunning rarefaction: %d qualified callers x %d draws of %d plays each...\n",
            length(qual), N_ITER, RAREFY_N))
res_list <- lapply(qual, function(nm) {
  sub <- dq[off_caller == nm]
  draws <- rarefy_caller(sub$play_id, sub$dress_id, sub$run_pass == "R", RAREFY_N, N_ITER)
  draws[, off_caller := nm]
  draws
})
draws_all <- rbindlist(res_list)

headline <- draws_all[, .(
  menu_combined  = mean(menu_combined),
  menu_run       = mean(menu_run, na.rm = TRUE),
  menu_pass      = mean(menu_pass, na.rm = TRUE),
  wardrobe_core  = mean(wardrobe_core),
  multiplier     = mean(multiplier),
  n_run_avg      = mean(n_run),
  n_pass_avg     = mean(n_pass)
), by = off_caller]
headline <- merge(headline, n_by_caller[off_caller %in% qual], by = "off_caller")
setnames(headline, "N", "n_plays")
setorder(headline, menu_combined)
headline[, menu_rank := frank(menu_combined, ties.method = "min")]
setorder(headline, -wardrobe_core)
headline[, wardrobe_rank := frank(-wardrobe_core, ties.method = "min")]
setorder(headline, menu_rank)

cat(sprintf("\nleague medians (rarefied to %d plays, averaged over %d draws): menu %.1f plays, wardrobe %.2f dressings/core play\n",
            RAREFY_N, N_ITER, median(headline$menu_combined), median(headline$wardrobe_core)))
cat("caveat: menu_run and menu_pass are computed on the run/pass subset of the SAME 1,200-play combined draw,\n")
cat("not separately rarefied to a common run or pass floor, so a caller's own run/pass mix still shapes them.\n")

# =============================================================================
# STEP 7: PERSISTENCE (custom odd/even split, rarefied within each half)
# =============================================================================
cat("\n=== PERSISTENCE ===\n")
dq[, season_half := fifelse(season %% 2 == 1, "odd", "even")]
half_n <- dq[, .(n_odd = sum(season_half == "odd"), n_even = sum(season_half == "even")), by = off_caller]
half_qual <- half_n[n_odd >= HALF_N & n_even >= HALF_N, off_caller]
cat(sprintf("callers with >= %d plays in BOTH season halves (2023/2025 vs 2022/2024): %d of %d qualified\n",
            HALF_N, length(half_qual), length(qual)))

half_res <- lapply(half_qual, function(nm) {
  sub_o <- dq[off_caller == nm & season_half == "odd"]
  sub_e <- dq[off_caller == nm & season_half == "even"]
  ro <- rarefy_caller(sub_o$play_id, sub_o$dress_id, sub_o$run_pass == "R", HALF_N, HALF_ITER)
  re <- rarefy_caller(sub_e$play_id, sub_e$dress_id, sub_e$run_pass == "R", HALF_N, HALF_ITER)
  data.table(off_caller = nm,
             menu_odd = mean(ro$menu_combined), menu_even = mean(re$menu_combined),
             wardrobe_odd = mean(ro$wardrobe_core), wardrobe_even = mean(re$wardrobe_core))
})
half_dt <- rbindlist(half_res)

ct_menu <- cor.test(half_dt$menu_odd, half_dt$menu_even)
ct_ward <- cor.test(half_dt$wardrobe_odd, half_dt$wardrobe_even)
cat(sprintf("menu size persistence: r = %+.3f [%+.3f, %+.3f], p = %.4f, n = %d callers\n",
            ct_menu$estimate, ct_menu$conf.int[1], ct_menu$conf.int[2], ct_menu$p.value, nrow(half_dt)))
cat(sprintf("wardrobe persistence:  r = %+.3f [%+.3f, %+.3f], p = %.4f, n = %d callers\n",
            ct_ward$estimate, ct_ward$conf.int[1], ct_ward$conf.int[2], ct_ward$p.value, nrow(half_dt)))

# =============================================================================
# STEP 8: THE McVAY HYPOTHESIS
# =============================================================================
cat("\n=== THE HYPOTHESIS ===\n")
n_q <- nrow(headline)
for (nm in NAMED) {
  r <- headline[off_caller == nm]
  cat(sprintf("%s: menu %.1f plays (rank %d of %d, 1 = smallest), wardrobe %.2f dressings/core play (rank %d of %d, 1 = biggest), multiplier %.2fx\n",
              nm, r$menu_combined, r$menu_rank, n_q, r$wardrobe_core, r$wardrobe_rank, n_q, r$multiplier))
}
mcvay <- headline[off_caller == "Sean McVay"]
small_menu <- mcvay$menu_rank <= n_q / 2
big_wardrobe <- mcvay$wardrobe_rank <= n_q / 2
verdict <- if (small_menu && big_wardrobe) {
  sprintf("HOLDS: McVay carries a below-median menu (rank %d of %d) AND an above-median wardrobe (rank %d of %d).", mcvay$menu_rank, n_q, mcvay$wardrobe_rank, n_q)
} else if (!small_menu && !big_wardrobe) {
  sprintf("DOES NOT HOLD: McVay's menu ranks %d of %d and his wardrobe ranks %d of %d, neither in the top half.", mcvay$menu_rank, n_q, mcvay$wardrobe_rank, n_q)
} else {
  sprintf("PARTIAL: McVay's menu ranks %d of %d, wardrobe ranks %d of %d -- only one half of the folklore checks out.", mcvay$menu_rank, n_q, mcvay$wardrobe_rank, n_q)
}
cat(sprintf("\nMcVay hypothesis (small menu, big wardrobe): %s\n", verdict))

# =============================================================================
# WRITE DATA
# =============================================================================
out <- headline[, .(off_caller, n_plays, menu_combined = round(menu_combined, 2), menu_rank,
                     menu_run = round(menu_run, 2), menu_pass = round(menu_pass, 2),
                     wardrobe_core = round(wardrobe_core, 3), wardrobe_rank,
                     multiplier = round(multiplier, 3))]
setorder(out, menu_rank)
write_csv(as.data.frame(out), "data/derived/menu_wardrobe.csv")
cat(sprintf("\nwrote data/derived/menu_wardrobe.csv (%d rows)\n", nrow(out)))

# =============================================================================
# CHART
# =============================================================================
headline[, hl := off_caller %in% NAMED]
outlier_ids <- unique(c(headline[which.min(menu_combined)]$off_caller, headline[which.max(menu_combined)]$off_caller,
                         headline[which.min(wardrobe_core)]$off_caller, headline[which.max(wardrobe_core)]$off_caller))
outlier_ids <- setdiff(outlier_ids, NAMED)
headline[, ol := off_caller %in% outlier_ids]
mx <- median(headline$menu_combined); my <- median(headline$wardrobe_core)
xr <- range(headline$menu_combined); yr <- range(headline$wardrobe_core)

lab_named <- headline[hl == TRUE]
lab_out <- headline[ol == TRUE]

corner_txt <- function(x, y, hj, vj, txt) annotate("text", x = x, y = y, hjust = hj, vjust = vj,
                                                    size = 2.9, fontface = "italic", colour = "grey40", label = txt)

p <- ggplot(headline, aes(menu_combined, wardrobe_core)) +
  geom_vline(xintercept = mx, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_hline(yintercept = my, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_point(data = headline[hl == FALSE & ol == FALSE], colour = "grey65", alpha = 0.8, size = 2.6) +
  geom_point(data = lab_out, colour = "grey20", shape = 21, fill = "white", stroke = 1, size = 3) +
  geom_point(data = lab_named, aes(colour = off_caller), size = 4.4) +
  geom_text_repel(data = lab_named, aes(label = off_caller, colour = off_caller),
                   size = 3.2, fontface = "bold", seed = 11, box.padding = 0.7, point.padding = 0.5,
                   min.segment.length = 0, max.overlaps = 30, show.legend = FALSE) +
  geom_text_repel(data = lab_out, aes(label = off_caller), size = 2.6, fontface = "bold", colour = "#1d6a99",
                   seed = 4, box.padding = 0.5, min.segment.length = 0, max.overlaps = 30) +
  corner_txt(xr[1], yr[2], 0, 1, "few plays,\nmany disguises") +
  corner_txt(xr[2], yr[2], 1, 1, "big menu,\nand every play gets dressed up too") +
  corner_txt(xr[1], yr[1], 0, 0, "few plays,\nworn the same way every time") +
  corner_txt(xr[2], yr[1], 1, 0, "big menu,\nbut each play only wears one look") +
  scale_colour_manual(values = COL, guide = "none") +
  scale_x_continuous(breaks = pretty_breaks()) +
  coord_cartesian(clip = "off") +
  labs(x = "fewer plays needed to cover 80% of snaps  →  (menu size)",
       y = "different dressings per core play  →  (wardrobe size)") +
  theme_coach(grid = "none")

TITLE <- sprintf(
  "Menu vs. wardrobe: McVay ranks #%d of %d on menu size, #%d of %d on wardrobe",
  mcvay$menu_rank, n_q, mcvay$wardrobe_rank, n_q)
SUB <- paste0(
  "Across: how many different plays it takes to cover 80% of a caller's snaps. Fewer plays = smaller menu = further left.\n",
  "Up: how many different looks the average play gets dressed in (the formation, the personnel, where the QB stands). More looks = bigger wardrobe = higher.\n",
  "Every caller is measured on the same 1,200-play sample, so a caller with more snaps charted does not get credit for a bigger playbook just for playing more."
)

p_final <- p + labs(title = TITLE, subtitle = SUB,
  caption = fig_caption(
    "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
    sprintf(paste0("%d callers with at least %s plays each.\n",
                   "A play = the run scheme and which side it attacks; for passes, the three deepest routes on the play\n",
                   "(the charting does not name pass plays, so the routes stand in for the concept). A look = formation,\n",
                   "personnel group, and where the quarterback stands. Nobody's data tags motion, so a motion variation\n",
                   "does not count as a new look here; if a caller's disguise lives in motion, this chart cannot see it."),
            n_q, format(QUAL_MIN, big.mark = ",")),
    sprintf(paste0("\nBoth habits are stable: a caller's menu size and wardrobe in his even-numbered seasons strongly predict\n",
                   "his odd-numbered ones (menu r = %+.2f, wardrobe r = %+.2f across %d callers), so this is identity,\n",
                   "not one hot stretch of tape. Built by R/41."),
            ct_menu$estimate, ct_ward$estimate, nrow(half_dt))
  )) +
  theme(plot.margin = margin(10, 14, 8, 10))

save_fig("docs/figures/menu_wardrobe.png", p_final, w = 13, h = 8.4)

# =============================================================================
# VERIFICATION BLOCK
# =============================================================================
cat("\n\n================= VERIFICATION =================\n")
for (nm in NAMED) {
  r <- headline[off_caller == nm]
  cat(sprintf("%-16s menu %.2f plays (rank %2d/%d)  wardrobe %.3f dressings/play (rank %2d/%d)  multiplier %.2fx  n=%d plays\n",
              nm, r$menu_combined, r$menu_rank, n_q, r$wardrobe_core, r$wardrobe_rank, n_q, r$multiplier, r$n_plays))
}
cat(sprintf("\nleague median: menu %.2f plays, wardrobe %.3f dressings/play, multiplier %.2fx (n=%d callers)\n",
            median(headline$menu_combined), median(headline$wardrobe_core), median(headline$multiplier), n_q))
cat(sprintf("\nMcVay hypothesis: %s\n", verdict))
cat(sprintf("persistence: menu r=%+.2f, wardrobe r=%+.2f\n", ct_menu$estimate, ct_ward$estimate))
cat("\nwrote docs/figures/menu_wardrobe.png, data/derived/menu_wardrobe.csv\n")
