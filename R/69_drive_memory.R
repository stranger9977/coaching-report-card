# =============================================================================
# 69_drive_memory.R -- two follow-ups on the chain.
#
# The asks, verbatim: "if you compare to non sequenced plays does something
# come out of it" and "does it always have to be one play before or can it
# have the memory of the drive".
#
# 1. SEQUENCED vs NOT. Same down and distance, same position in the drive:
#    does a play where the look STAYED (same personnel group as the previous
#    snap) earn more than one where it CHANGED? And the play-family version:
#    does repeating the previous family earn more than switching? League and
#    per caller, with standard errors.
#
# 2. MEMORY OF THE DRIVE. The Markov state does not have to be the last play.
#    Here the state is a summary of the drive so far: runs so far (0, 1, 2,
#    3+), play-action fakes so far (0, 1+), and length of the current
#    personnel streak (1, 2, 3+). Question: does that summary predict the next
#    play family beyond down, distance, drive position AND the last play?
#    Measured as conditional mutual information against a shuffle null, the
#    same machinery as R/66, so it is directly comparable with the one-play-
#    back number.
#
# Out: docs/figures/sequenced_vs_not.png, docs/figures/drive_memory.png
#      data/derived/drive_memory.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
set.seed(69)
N_PERM <- 200; MIN_PLAYS <- 1500
MARQUEE <- c("Kyle Shanahan", "Sean McVay", "Ben Johnson", "Andy Reid", "Matt LaFleur", "Sean Payton", "Kevin O'Connell")
lab_c <- function(x) sub("Kevin O'Connell", "O'Connell", sub("^\\S+ ", "", x))

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & off_caller != "" & !is.na(expected_points_added)]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
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
d[, prev := shift(state), by = drive_id]
d[, prev_pers := shift(offensive_personnel_basic), by = drive_id]
d[, is_run := run_pass == "R" & run_concept != "SCRAMBLE"]
d[, runs_before := cumsum(is_run) - is_run, by = drive_id]
d[, pa_before := cumsum(play_action == TRUE) - (play_action == TRUE), by = drive_id]
d[, same_pers := offensive_personnel_basic == prev_pers]
d[, streak := { s <- integer(.N); for (i in seq_len(.N)) s[i] <- if (i > 1 && same_pers[i]) s[i - 1] + 1L else 1L; s }, by = drive_id]
# the streak as it stood BEFORE this snap; the current snap's personnel is the
# present look, not the drive's memory, and would leak straight into the family
d[, streak_before := shift(streak), by = drive_id]
d[, drive_state := paste(pmin(runs_before, 3), pmin(pa_before, 1), pmin(streak_before, 3))]
# game script: a drive full of runs usually means a lead and a clock to kill,
# which predicts the next call without being "memory" of anything
d[, gs := paste(fifelse(offense_score_diff < -8, "trail", fifelse(offense_score_diff > 8, "lead", "close")),
                fifelse(quarter <= 2, "H1", "H2"))]
# baselines for the stayed-vs-changed test: the play's value against the league
# at the same down, distance, drive position, score, half AND the same play
# family (or the same personnel group), so "repeated" is not just "ran again"
d[, adj_f := expected_points_added - mean(expected_points_added), by = .(dd, kb, gs, state)]
d[, adj_p := expected_points_added - mean(expected_points_added), by = .(dd, kb, gs, offensive_personnel_basic)]
d[, adj := expected_points_added - mean(expected_points_added), by = .(dd, kb)]
tr <- d[!is.na(prev)]
callers <- tr[, .N, by = off_caller][N >= MIN_PLAYS]$off_caller
tr <- tr[off_caller %in% callers]

# ---------------------------------------------------------------- 1. sequenced vs not
tr[, same_fam := state == prev]
seq_tab <- function(x, who) rbind(
  x[!is.na(same_pers), .(n = .N, adj = mean(adj_p), se = sd(adj_p) / sqrt(.N)), by = .(stayed = same_pers)][, `:=`(who = who, what = "same personnel group as last snap")],
  x[, .(n = .N, adj = mean(adj_f), se = sd(adj_f) / sqrt(.N)), by = .(stayed = same_fam)][, `:=`(who = who, what = "same play family as last snap")])
sv <- rbind(seq_tab(tr, "league"), rbindlist(lapply(callers, function(cl) seq_tab(tr[off_caller == cl], cl))))
sv[, cond := fifelse(stayed, "stayed", "changed")][, stayed := NULL]
sv <- dcast(sv, who + what ~ cond, value.var = c("n", "adj", "se"))
sv[, `:=`(diff = adj_stayed - adj_changed, se = sqrt(se_stayed^2 + se_changed^2))]
sv[, z := diff / se]
cat("league, stayed minus changed (EPA per play vs league at same down-distance, drive position, score, half and same family / personnel):\n")
print(sv[who == "league", .(what, n_stayed, n_changed, stayed = round(adj_stayed, 3), changed = round(adj_changed, 3), diff = round(diff, 3), z = round(z, 1))])
cat(sprintf("callers where stayed beats changed by z > 2: personnel %d of %d, family %d of %d; z < -2: personnel %d, family %d\n",
            sv[who != "league" & what == "same personnel group as last snap", sum(z > 2)], length(callers),
            sv[who != "league" & what == "same play family as last snap", sum(z > 2)], length(callers),
            sv[who != "league" & what == "same personnel group as last snap", sum(z < -2)],
            sv[who != "league" & what == "same play family as last snap", sum(z < -2)]))
print(sv[who != "league"][order(-diff)][, .(who, what, diff = round(diff, 3), z = round(z, 1))][c(1:6, (.N - 5):.N)])

# ---------------------------------------------------------------- 2. drive memory
cmi <- function(a, b, cc) {
  x <- data.table(aa = a, bb = b, cc = cc)[, .N, by = .(aa, bb, cc)]
  x[, nc := sum(N), by = cc]; x[, nac := sum(N), by = .(aa, cc)]; x[, nbc := sum(N), by = .(bb, cc)]
  x[, sum(N / sum(N) * log2(N * nc / (nac * nbc)))]
}
shuf <- function(a, c) { idx <- data.table(i = seq_along(a), c)[, .(i, j = i[sample.int(.N)]), by = c]; a[idx[order(i)]$j] }
mem <- rbindlist(lapply(c("league", callers), function(cl) {
  x <- if (cl == "league") tr else tr[off_caller == cl]
  np <- if (cl == "league") 30 else N_PERM
  test <- function(a, cc) {
    g <- if (cl == "league") paste(x$off_caller, cc) else cc
    o <- cmi(a, x$state, cc); n <- replicate(np, cmi(shuf(a, g), x$state, cc))
    c(o - mean(n), (o - mean(n)) / sd(n))
  }
  c1 <- paste(x$dd, x$kb); c2 <- paste(x$dd, x$kb, x$prev)
  g1 <- paste(x$dd, x$kb, x$gs); g2 <- paste(x$dd, x$kb, x$gs, x$prev)
  lp  <- test(x$prev, c1);        dm  <- test(x$drive_state, c2)
  lpg <- test(x$prev, g1);        dmg <- test(x$drive_state, g2)
  runs <- test(pmin(x$runs_before, 3), g2); pa <- test(pmin(x$pa_before, 1), g2); stk <- test(pmin(x$streak_before, 3), g2)
  data.table(off_caller = cl, n = nrow(x),
             last_play = lp[1], last_play_z = lp[2], drive_memory = dm[1], drive_memory_z = dm[2],
             last_play_gs = lpg[1], last_play_gs_z = lpg[2], drive_memory_gs = dmg[1], drive_memory_gs_z = dmg[2],
             runs_gs = runs[1], runs_gs_z = runs[2], pa_gs = pa[1], pa_gs_z = pa[2], streak_gs = stk[1], streak_gs_z = stk[2])
}))
cat("\ninformation about the next play family, bits beyond down, distance, drive position (and for drive memory, beyond the last play too):\n")
print(mem[order(-drive_memory_gs)][, .(off_caller, last_play = round(last_play, 4), drive_mem = round(drive_memory, 4),
                                        last_play_gs = round(last_play_gs, 4), drive_mem_gs = round(drive_memory_gs, 4), z_gs = round(drive_memory_gs_z, 1),
                                        runs = round(runs_gs, 4), pa = round(pa_gs, 4), streak = round(streak_gs, 4))])
cat(sprintf("without game script: drive memory clears z > 3 for %d of %d, league ratio to last play %.2f\n",
            sum(mem[off_caller != "league"]$drive_memory_z > 3), length(callers),
            mem[off_caller == "league"]$drive_memory / mem[off_caller == "league"]$last_play))
cat(sprintf("WITH game script (score, half): drive memory clears z > 3 for %d of %d, league ratio to last play %.2f; components runs %.4f, pa %.4f, streak %.4f\n",
            sum(mem[off_caller != "league"]$drive_memory_gs_z > 3), length(callers),
            mem[off_caller == "league"]$drive_memory_gs / mem[off_caller == "league"]$last_play_gs,
            mem[off_caller == "league"]$runs_gs, mem[off_caller == "league"]$pa_gs, mem[off_caller == "league"]$streak_gs))
write_csv(as.data.frame(merge(mem, dcast(sv, who ~ what, value.var = c("diff", "z")), by.x = "off_caller", by.y = "who")), "data/derived/drive_memory.csv")

# ---------------------------------------------------------------- figures
svc <- sv[who != "league"]
svc[, lab := fifelse(who %in% MARQUEE | abs(z) > 2, lab_c(who), "")]
svc[, what := factor(what, levels = c("same personnel group as last snap", "same play family as last snap"))]
lgd <- sv[who == "league"]
p1 <- ggplot(svc, aes(x = reorder(who, diff), y = diff)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_hline(data = lgd, aes(yintercept = diff), colour = "#2B8CBE", linetype = "22") +
  geom_linerange(aes(ymin = diff - 2 * se, ymax = diff + 2 * se), colour = "grey80", linewidth = 1.4) +
  geom_point(aes(colour = abs(z) > 2), size = 2.4) +
  geom_text_repel(aes(label = lab), size = 2.9, direction = "y", segment.colour = "grey70", max.overlaps = 30) +
  facet_wrap(~ what, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  scale_y_continuous(labels = label_number(style_positive = "plus", accuracy = 0.01)) +
  labs(title = sprintf("Sequenced vs not: staying in the look earns %s; repeating the play family earns %s",
                       fifelse(abs(lgd[what == "same personnel group as last snap"]$z) < 2, "nothing", sprintf("%+.2f", lgd[what == "same personnel group as last snap"]$diff)),
                       fifelse(abs(lgd[what == "same play family as last snap"]$z) < 2, "nothing", sprintf("%+.2f", lgd[what == "same play family as last snap"]$diff))),
       subtitle = paste0("EPA per play when the look STAYED minus when it CHANGED, each against the league at the same down, distance, drive position, score,\nhalf and the same play family (bottom) or personnel group (top), so a repeat is not just a run after a run.\n",
                         "One dot per caller, grey bar two standard errors, dotted blue line the league. Orange: clears two standard errors either way.\n",
                         "Top: same personnel group as the previous snap. Bottom: same play family as the previous snap."),
       x = NULL, y = "stayed minus changed (EPA per play)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\nPlays inside a drive after the first; callers with at least %s such plays. Built by R/69.", comma(MIN_PLAYS)))) +
  theme_coach(grid = "y") +
  theme(axis.text.x = element_blank(), strip.text = element_text(face = "bold", hjust = 0))
save_fig("docs/figures/sequenced_vs_not.png", p1, w = 11, h = 8)

mc <- mem[off_caller != "league"]
mc[, `:=`(last_play = last_play_gs, drive_memory = drive_memory_gs, drive_memory_z = drive_memory_gs_z)]
mc[, lab := fifelse(off_caller %in% MARQUEE | drive_memory_z > 3, lab_c(off_caller), "")]
p2 <- ggplot(mc, aes(last_play, drive_memory)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey80", linetype = "22") +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_point(aes(colour = drive_memory_z > 3), size = 2.6) +
  geom_text_repel(aes(label = lab), size = 3.1, segment.colour = "grey70", max.overlaps = 20) +
  annotate("text", x = max(mc$last_play), y = 0.004, label = "dotted line: the drive's history worth\nas much as the last play alone",
           hjust = 1, vjust = 0, size = 2.9, colour = "grey45") +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey45"), guide = "none") +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) + scale_y_continuous(labels = label_number(accuracy = 0.001)) +
  labs(title = sprintf("Giving the chain a memory of the whole drive adds something for %d of %d callers", sum(mc$drive_memory_z > 3), nrow(mc)),
       subtitle = paste0("Across: information the LAST PLAY carries about the next play family, beyond down, distance, drive position, score and half.\n",
                         "Up: information a summary of the DRIVE SO FAR (runs so far, play-action fakes so far, personnel streak before this snap) adds on top\n",
                         "of that, beyond the same situation and the last play. Orange: clears its shuffle null by 3 standard deviations."),
       x = "last play (bits beyond situation)", y = "drive so far (bits beyond situation and the last play)",
       caption = fig_caption("SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nBits are minus what a shuffled order produces. A Markov chain conditions on its state; this is the same chain with a richer state. Score and half are\nin the conditioning because a drive full of runs usually means a lead and a clock to kill, which is game script, not memory. Built by R/69.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/drive_memory.png", p2, w = 10, h = 7)
cat("\nOut: sequenced_vs_not.png, drive_memory.png\n")
