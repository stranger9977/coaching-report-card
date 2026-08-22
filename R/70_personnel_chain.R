# =============================================================================
# 70_personnel_chain.R -- the chain over PERSONNEL GROUPS, and Shanahan by
# season.
#
# The asks, verbatim: "what we define as a chain could be the personnel usage
# like whats at the top is there anything there with the markov chain stuff?"
# and "I bet Shany in 2023-24 was nuts in terms of sequencing too".
#
# 1. THE PERSONNEL CHAIN. States are the personnel group on the field (11,
#    12, 13, 21, 22, 10, other), transitions inside a drive. Three numbers per
#    caller: how often he HOLDS the group from one snap to the next (against
#    the league), how much the previous group predicts the next one beyond
#    down, distance and drive position (bits vs a shuffle null, the R/66
#    machinery), and the chain's memory half-life. Plus the league's
#    personnel transition matrix as a lift heatmap.
#
# 2. SHANAHAN BY SEASON. His play-family sequence information, his hold rate
#    in 21 personnel, and his 21-personnel third-look payoff (survivorship
#    handled as in R/68), each season 2022-23 through 2025-26, with the league
#    alongside.
#
# Out: docs/figures/personnel_chain.png, personnel_matrix.png, personnel_memory.png,
#      shanahan_by_season.png; data/derived/personnel_chain.csv,
#      shanahan_by_season.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(70)
N_PERM <- 200; MIN_PLAYS <- 1500
MARQUEE <- c("Kyle Shanahan", "Sean McVay", "Ben Johnson", "Andy Reid", "Matt LaFleur", "Sean Payton", "Kevin O'Connell")
lab_c <- function(x) sub("Kevin O'Connell", "O'Connell", sub("^\\S+ ", "", x))
cmi <- function(a, b, cc) {
  x <- data.table(aa = a, bb = b, cc = cc)[, .N, by = .(aa, bb, cc)]
  x[, nc := sum(N), by = cc]; x[, nac := sum(N), by = .(aa, cc)]; x[, nbc := sum(N), by = .(bb, cc)]
  x[, sum(N / sum(N) * log2(N * nc / (nac * nbc)))]
}
shuf <- function(a, c) { idx <- data.table(i = seq_along(a), c)[, .(i, j = i[sample.int(.N)]), by = c]; a[idx[order(i)]$j] }
half_life <- function(P) { l2 <- sort(Mod(eigen(P, only.values = TRUE)$values), decreasing = TRUE)[2]; log(0.5) / log(l2) }

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & off_caller != "" &
       offensive_personnel_basic != "" & !is.na(expected_points_added)]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
PG <- c("11", "12", "13", "21", "22", "10", "other")
d[, pers := sub("\\*", "", offensive_personnel_basic)]
d[!pers %in% PG, pers := "other"]
d[, pers := factor(pers, levels = PG)]
gap <- c("MAN", "POWER", "PULL LEAD", "COUNTER", "TRAP", "FULLBACK")
d[, state := fcase(
  run_pass == "R" & run_concept == "OUTSIDE ZONE", "outside zone",
  run_pass == "R" & run_concept == "INSIDE ZONE",  "inside zone",
  run_pass == "R" & run_concept %in% gap,          "gap run",
  run_pass == "R" & run_concept != "SCRAMBLE",     "other run",
  run_pass == "P" & screen == TRUE,                "screen",
  run_pass == "P" & play_action == TRUE,           "play action",
  run_pass == "P" & !is.na(depth_of_target) & depth_of_target >= 10, "deep dropback",
  run_pass == "P",                                 "short dropback",
  default = NA_character_)]
d[run_pass == "R" & run_concept == "SCRAMBLE", state := "short dropback"]
d <- d[!is.na(state)]
d[, dd := fifelse(down == 1, "1", fifelse(down == 2 & distance <= 3, "2s", fifelse(down == 2 & distance <= 7, "2m",
         fifelse(down == 2, "2l", fifelse(down == 3 & distance <= 3, "3s", fifelse(down == 3 & distance <= 7, "3m", fifelse(down == 3, "3l", "4")))))))]
d[, k := seq_len(.N), by = drive_id]
d[, kb := fifelse(k == 1, "1", fifelse(k == 2, "2", fifelse(k == 3, "3", fifelse(k <= 5, "4-5", "6+"))))]
d[, prev_pers := shift(pers), by = drive_id]
d[, prev := shift(state), by = drive_id]
d[, new_grp := is.na(prev_pers) | pers != prev_pers]
d[, streak_id := cumsum(new_grp), by = drive_id]
d[, pos := seq_len(.N), by = .(drive_id, streak_id)]
d[, pos_b := fifelse(pos == 1, "1st", fifelse(pos == 2, "2nd", fifelse(pos == 3, "3rd", fifelse(pos <= 5, "4th-5th", "6th+"))))]
d[, adj := expected_points_added - mean(expected_points_added), by = .(dd, kb, pos_b)]
tr <- d[!is.na(prev_pers)]
callers <- tr[, .N, by = off_caller][N >= MIN_PLAYS]$off_caller
tr <- tr[off_caller %in% callers]

# ---------------------------------------------------------------- 1. personnel chain
tmat <- function(x) { m <- as.matrix(table(x$prev_pers, x$pers)); m / pmax(rowSums(m), 1) }
pc <- rbindlist(lapply(c("league", callers), function(cl) {
  x <- if (cl == "league") tr else tr[off_caller == cl]
  cc <- paste(x$dd, x$kb); g <- if (cl == "league") paste(x$off_caller, cc) else cc
  np <- if (cl == "league") 40 else N_PERM
  o <- cmi(x$prev_pers, x$pers, cc); nul <- replicate(np, cmi(shuf(x$prev_pers, g), x$pers, cc))
  hl <- half_life(tmat(x)); hln <- replicate(np, half_life(tmat(data.table(prev_pers = shuf(x$prev_pers, cc), pers = x$pers))))
  data.table(off_caller = cl, n = nrow(x), hold = mean(x$pers == x$prev_pers),
             bits = o - mean(nul), bits_z = (o - mean(nul)) / sd(nul),
             half_life = hl, half_life_null = mean(hln))
}))
# 1b. one group back, two groups back, and the drive so far, for the PERSONNEL
# chain, same controls as R/69 (down, distance, drive position, score and half)
d[, gs := paste(fifelse(offense_score_diff < -8, "trail", fifelse(offense_score_diff > 8, "lead", "close")), fifelse(quarter <= 2, "H1", "H2"))]
d[, prev_pers2 := shift(pers, 2), by = drive_id]
d[, is_run := run_pass == "R" & run_concept != "SCRAMBLE"]
d[, runs_before := cumsum(is_run) - is_run, by = drive_id]
d[, pa_before := cumsum(play_action %in% TRUE) - (play_action %in% TRUE), by = drive_id]
d[, heavy_before := cumsum(pers %in% c("12", "13", "21", "22")) - (pers %in% c("12", "13", "21", "22")), by = drive_id]
d[, drive_state := paste(pmin(runs_before, 3), pmin(pa_before, 1), pmin(heavy_before, 3))]
tr2 <- d[!is.na(prev_pers2) & off_caller %in% callers]
po <- rbindlist(lapply(c("league", callers), function(cl) {
  x <- if (cl == "league") tr2 else tr2[off_caller == cl]
  np <- if (cl == "league") 30 else N_PERM
  test <- function(a, cc) {
    g <- if (cl == "league") paste(x$off_caller, cc) else cc
    o <- cmi(a, x$pers, cc); n <- replicate(np, cmi(shuf(a, g), x$pers, cc))
    c(o - mean(n), (o - mean(n)) / sd(n))
  }
  c1 <- paste(x$dd, x$kb, x$gs); c2 <- paste(x$dd, x$kb, x$gs, x$prev_pers)
  one <- test(x$prev_pers, c1); two <- test(x$prev_pers2, c2); drv <- test(x$drive_state, c2)
  data.table(off_caller = cl, n = nrow(x), one_back = one[1], one_back_z = one[2], two_back = two[1], two_back_z = two[2],
             drive = drv[1], drive_z = drv[2])
}))
cat("personnel chain, memory depth (bits beyond down, distance, drive position, score, half; two back and drive also beyond one back):\n")
print(po[order(-two_back), .(off_caller, one_back = round(one_back, 4), two_back = round(two_back, 4), z2 = round(two_back_z, 1),
                             drive = round(drive, 4), zd = round(drive_z, 1))])
cat(sprintf("callers clearing z > 3: one back %d, two back %d, drive %d of %d; league ratios two/one %.2f, drive/one %.2f\n",
            sum(po[off_caller != "league"]$one_back_z > 3), sum(po[off_caller != "league"]$two_back_z > 3),
            sum(po[off_caller != "league"]$drive_z > 3), length(callers),
            po[off_caller == "league"]$two_back / po[off_caller == "league"]$one_back,
            po[off_caller == "league"]$drive / po[off_caller == "league"]$one_back))
write_csv(as.data.frame(po), "data/derived/personnel_memory.csv")
poc <- po[off_caller != "league"]
poc[, lab := fifelse(off_caller %in% MARQUEE | two_back_z > 3 & two_back > quantile(two_back, 0.8), lab_c(off_caller), "")]
pol <- melt(poc, id.vars = c("off_caller", "lab"), measure.vars = c("one_back", "two_back", "drive"), variable.name = "depth", value.name = "bits")
pol[, depth := factor(depth, levels = c("one_back", "two_back", "drive"),
                      labels = c("last group\n(beyond situation)", "group two back\n(beyond that and the last group)", "drive so far: runs, fakes, heavy groups\n(beyond the last group)"))]
pm <- ggplot(pol, aes(depth, bits, group = off_caller)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_line(colour = "grey80", linewidth = 0.4) +
  geom_point(aes(colour = off_caller %in% MARQUEE), size = 2.2) +
  geom_text_repel(data = pol[lab != "" & depth == levels(pol$depth)[1]], aes(label = lab), size = 2.8, direction = "y", nudge_x = -0.25, hjust = 1, segment.colour = "grey70") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey55"), guide = "none") +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(title = sprintf("The personnel chain remembers: last group %.2f bits, two back adds %.2f, the drive so far %.2f",
                       po[off_caller == "league"]$one_back, po[off_caller == "league"]$two_back, po[off_caller == "league"]$drive),
       subtitle = paste0("Information about the NEXT personnel group from three memories, one line per caller. Left: the last group, beyond down, distance, drive\n",
                         "position, score and half. Middle: the group two back, beyond all that and the last group. Right: a summary of the drive so far (runs,\n",
                         "play-action fakes, heavy groups), beyond the last group. Same machinery as the play-family chain, which carries about 0.04 bits on the left."),
       x = NULL, y = "bits beyond the controls",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nPlays 3rd in the drive or later so all three memories exist on the same plays. Bits are minus a shuffle null. Clearing the null by 3 sd: last group %d, two back %d,\ndrive %d of %d callers. Built by R/70.",
                 sum(poc$one_back_z > 3), sum(poc$two_back_z > 3), sum(poc$drive_z > 3), nrow(poc)))) +
  theme_coach(grid = "y")
save_fig("docs/figures/personnel_memory.png", pm, w = 10.5, h = 7)

cat("personnel chain:\n")
print(pc[order(-hold), .(off_caller, n, hold = round(hold, 3), bits = round(bits, 4), z = round(bits_z, 1),
                         half_life = round(half_life, 2), null = round(half_life_null, 2))])
write_csv(as.data.frame(pc), "data/derived/personnel_chain.csv")

# league lift matrix
Pl <- tmat(tr); marg <- prop.table(table(tr$pers))
lift <- as.data.table(as.table(Pl))[, .(prev = V1, nxt = V2, p = N)]
lift <- merge(lift, data.table(nxt = names(marg), m = as.numeric(marg)), by = "nxt")
lift[, ratio := p / m]
lift[, `:=`(prev = factor(prev, levels = rev(PG)), nxt = factor(nxt, levels = PG))]
p0 <- ggplot(lift, aes(nxt, prev, fill = pmin(ratio, 3))) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.1f", ratio)), size = 3.2, colour = fifelse(lift$ratio > 2 | lift$ratio < 0.6, "white", "grey20")) +
  scale_fill_gradient2(low = "#2B8CBE", mid = "grey96", high = "#D55E00", midpoint = 1, limits = c(0.3, 3), oob = squish, name = "times the\nbase rate") +
  labs(title = sprintf("The personnel chain is sticky: the league holds its group on %.0f%% of snaps", 100 * pc[off_caller == "league"]$hold),
       subtitle = paste0("How often the NEXT personnel group (columns) follows the PREVIOUS one (rows), as a multiple of that group's overall rate.\n",
                         "The diagonal is the hold. Same drive only, all callers, 2022-23 through 2025-26."),
       x = NULL, y = "previous snap's group",
       caption = fig_caption("SumerSports play charting, regular seasons, garbage time excluded",
         "\nColumns are the next snap's personnel group. 10 = 1 back, 0 tight ends; 21 = 2 backs, 1 tight end; 13 = 1 back, 3 tight ends. Built by R/70.")) +
  theme_coach(grid = "none") + theme(legend.position = "right")
save_fig("docs/figures/personnel_matrix.png", p0, w = 9.5, h = 6.5)

pcc <- pc[off_caller != "league"]
pcc[, lab := fifelse(off_caller %in% MARQUEE | hold > quantile(hold, 0.9) | hold < quantile(hold, 0.1), lab_c(off_caller), "")]
lg <- pc[off_caller == "league"]
p1 <- ggplot(pcc, aes(hold, bits)) +
  geom_vline(xintercept = lg$hold, colour = "grey80", linetype = "22") +
  geom_hline(yintercept = lg$bits, colour = "grey80", linetype = "22") +
  geom_point(aes(colour = bits_z > 3), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70", max.overlaps = 30) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(title = sprintf("Who holds the look: hold rates run from %.0f%% to %.0f%%, and the last group always predicts the next",
                       100 * min(pcc$hold), 100 * max(pcc$hold)),
       subtitle = paste0("Across: how often a caller keeps the same personnel group from one snap to the next inside a drive. Up: how much the previous\n",
                         "group predicts the next one beyond down, distance and drive position (bits, minus a shuffle null). Dotted lines: the league.\n",
                         sprintf("Orange: clears the null by 3 standard deviations (%d of %d callers). This is the chain the look-then-strike chart lives on.",
                                 sum(pcc$bits_z > 3), nrow(pcc))),
       x = "hold rate: same personnel group as the previous snap", y = "personnel sequence information beyond situation (bits)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nSeven personnel states (11, 12, 13, 21, 22, 10, other); transitions inside the same drive; callers with at least %s. Built by R/70.", comma(MIN_PLAYS)))) +
  theme_coach(grid = "y")
save_fig("docs/figures/personnel_chain.png", p1, w = 10.5, h = 7)

# ---------------------------------------------------------------- 2. Shanahan by season
d[, is21 := pers == "21"]
sea <- rbindlist(lapply(2022:2025, function(y) {
  x <- tr[season == y]; s <- x[off_caller == "Kyle Shanahan"]
  cc_s <- paste(s$dd, s$kb); cc_x <- paste(x$off_caller, x$dd, x$kb)
  o_s <- cmi(s$prev, s$state, cc_s); n_s <- replicate(N_PERM, cmi(shuf(s$prev, cc_s), s$state, cc_s))
  o_x <- cmi(x$prev, x$state, paste(x$dd, x$kb)); n_x <- replicate(20, cmi(shuf(x$prev, cc_x), x$state, paste(x$dd, x$kb)))
  third_s <- d[season == y & off_caller == "Kyle Shanahan" & pers == "21" & pos == 3]
  third_l <- d[season == y & pos == 3]
  rbind(
    data.table(season = y, who = "Shanahan", what = "play-family sequence information (bits beyond situation)", v = o_s - mean(n_s), se = sd(n_s)),
    data.table(season = y, who = "league",   what = "play-family sequence information (bits beyond situation)", v = o_x - mean(n_x), se = sd(n_x)),
    data.table(season = y, who = "Shanahan", what = "hold rate in 21 personnel (next snap stays 21)", v = s[prev_pers == "21", mean(pers == "21")], se = NA_real_),
    data.table(season = y, who = "league",   what = "hold rate in 21 personnel (next snap stays 21)", v = x[prev_pers == "21", mean(pers == "21")], se = NA_real_),
    data.table(season = y, who = "Shanahan", what = "3rd straight snap of 21: EPA vs league at the same depth", v = mean(third_s$adj), se = sd(third_s$adj) / sqrt(nrow(third_s))),
    data.table(season = y, who = "league",   what = "3rd straight snap of 21: EPA vs league at the same depth", v = third_l[pers == "21", mean(adj)], se = NA_real_),
    data.table(season = y, who = "Shanahan", what = "share of snaps in 21 personnel", v = mean(d[season == y & off_caller == "Kyle Shanahan"]$is21), se = NA_real_),
    data.table(season = y, who = "league",   what = "share of snaps in 21 personnel", v = mean(d[season == y]$is21), se = NA_real_))
}))
sea[, n3 := NA_integer_]
n3 <- d[off_caller == "Kyle Shanahan" & pers == "21" & pos == 3, .N, by = season]
cat("\nShanahan by season:\n"); print(dcast(sea, what ~ season + who, value.var = "v")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("3rd-look 21 snaps by season:\n"); print(n3)
write_csv(as.data.frame(sea), "data/derived/shanahan_by_season.csv")
sea[, lab := fifelse(grepl("hold|share", what), sprintf("%.0f%%", 100 * v), sprintf("%+.2f", v))]
sea[, what := factor(what, levels = c("play-family sequence information (bits beyond situation)", "share of snaps in 21 personnel",
                                      "hold rate in 21 personnel (next snap stays 21)", "3rd straight snap of 21: EPA vs league at the same depth"))]
sea[, season_lab := sprintf("%d-%s", season, substr(season + 1, 3, 4))]
p2 <- ggplot(sea, aes(season_lab, v, group = who, colour = who)) +
  geom_hline(yintercept = 0, colour = "grey80", linewidth = 0.4) +
  geom_line(linewidth = 1.1) + geom_point(size = 2.6) +
  geom_text(data = sea[who == "Shanahan"], aes(label = lab), vjust = -1, size = 2.9, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ what, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c("Shanahan" = "#D55E00", "league" = "grey60"), name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0.15, 0.35))) +
  labs(title = "Shanahan by season: 2023-24 was the year the third look paid most, and the year his play order said the least",
       subtitle = paste0("Four views of the same caller, season by season, league in grey. Sequence information is the play-family chain beyond down, distance and\n",
                         "drive position. The third-look payoff is survivorship-handled as on the streak chart (league at the same drive position and streak depth)."),
       x = NULL, y = NULL,
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nThird-look snaps in 21 personnel per season: %s. Built by R/70.",
                 paste(sprintf("%d-%s %d", n3$season, substr(n3$season + 1, 3, 4), n3$N), collapse = ", ")))) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left", strip.text = element_text(face = "bold", hjust = 0, size = 10.5))
save_fig("docs/figures/shanahan_by_season.png", p2, w = 11, h = 7.5)
cat("\nOut: personnel_chain.png, personnel_matrix.png, shanahan_by_season.png\n")
