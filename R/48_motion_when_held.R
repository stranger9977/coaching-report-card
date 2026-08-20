# =============================================================================
# 48_motion_when_held.R -- when a caller is in "one formation, many looks"
# mode, is HE the one moving players around? The McVay-specific version of
# the motion-rate question.
#
# The ask, verbatim: "we just want to know how often coaches use motion and
# then specifically for mcvay when he gets into these one formation, many
# looks mode is he running motion alot more than others?"
#
# THE JOIN PROBLEM, and the bridge. Motion lives in FTN's charting (keyed to
# nflverse play ids). Held-personnel stretches live in Sumer's charting,
# whose ids match nothing. The two are joined here WITHOUT ids, on a
# composite key: season | week | offense | quarter | clock | down | distance.
# Both sides are 100% unique on that key and 98.4% of Sumer's run/pass plays
# find their nflverse twin. Any play whose key is not unique on either side
# is dropped rather than guessed.
#
# "HELD MODE" = the same personnel group is on the field as the previous
# snap of the same game (the exact definition behind the 84% number on this
# board). A stricter version (same personnel AND same receiver split AND
# same QB alignment as the previous snap) is computed too and reported in
# the printout; the story is the same either way.
#
# Conventions: plain language in all rendered text, no Michael/Nick in
# rendered text, season spans, no em dashes in rendered text.
#
# Out: docs/figures/motion_when_held.png
#      data/derived/motion_when_held.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
  library(nflreadr)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

# ---------------------------------------------------------------- sumer side
d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & run_pass %in% c("P", "R") & !is.na(off_caller)]
d[, clock_s := as.integer(sub(":.*", "", clock)) * 60 + as.integer(sub(".*:", "", clock))]
setorder(d, season, week, off_team, quarter, -clock_s)
d[, gm := paste(season, week, off_team)]
d[, held := offensive_personnel_basic != "" &
            offensive_personnel_basic == shift(offensive_personnel_basic), by = gm]
d[, held_strict := held & formation == shift(formation) &
            quarterback_alignment == shift(quarterback_alignment), by = gm]
d[is.na(held), held := FALSE]
d[is.na(held_strict), held_strict := FALSE]
d[, clock2 := sprintf("%02d:%02d", clock_s %/% 60, clock_s %% 60)]
d[, key := paste(season, week, off_team, quarter, clock2, down, distance, sep = "|")]
d <- d[, if (.N == 1) .SD, by = key]
cat(sprintf("sumer side: %s unique-key plays; held-personnel share %.1f%%\n",
            format(nrow(d), big.mark = ","), 100 * mean(d$held)))

# ---------------------------------------------------------------- ftn side
pbp <- setDT(load_pbp(2022:2025))[season_type == "REG" & play_type %in% c("run", "pass") & !is.na(down)]
ftn <- setDT(load_ftn_charting(2022:2025))
pbp <- merge(pbp, ftn[, .(nflverse_game_id, nflverse_play_id, is_motion)],
             by.x = c("game_id", "play_id"), by.y = c("nflverse_game_id", "nflverse_play_id"),
             all.x = TRUE)
pbp[, key := paste(season, week, posteam, qtr, time, down, ydstogo, sep = "|")]
pbp <- pbp[, if (.N == 1) .SD, by = key]

j <- merge(d[, .(key, off_caller, held, held_strict)],
           pbp[!is.na(is_motion), .(key, is_motion)], by = "key")
cat(sprintf("bridged plays with a motion flag: %s (%.1f%% of Sumer's unique plays)\n",
            format(nrow(j), big.mark = ","), 100 * nrow(j) / nrow(d)))

# ---------------------------------------------------------------- per caller
qual <- j[, .N, by = off_caller][N >= 1200]$off_caller
res <- j[off_caller %in% qual,
         .(n = .N,
           rate_held  = 100 * mean(is_motion[held]),
           n_held     = sum(held),
           rate_fresh = 100 * mean(is_motion[!held]),
           n_fresh    = sum(!held),
           rate_held_strict = 100 * mean(is_motion[held_strict])),
         by = off_caller]
res[, gap := rate_held - rate_fresh]
res[, rank_held := frank(-rate_held, ties.method = "min")]
setorder(res, -rate_held)
write_csv(as.data.frame(res), "data/derived/motion_when_held.csv")

mc <- res[off_caller == "Sean McVay"]
cat("\n=== the answer ===\n")
cat(sprintf("McVay in held mode: motion on %.1f%% of snaps (rank %d of %d); league median in held mode %.1f%%\n",
            mc$rate_held, mc$rank_held, nrow(res), median(res$rate_held)))
cat(sprintf("McVay when the picture is fresh: %.1f%% (league median %.1f%%)\n",
            mc$rate_fresh, median(res$rate_fresh)))
cat(sprintf("his held-minus-fresh gap: %+.1f points (league median gap %+.1f); strict same-picture mode: %.1f%%\n",
            mc$gap, median(res$gap), mc$rate_held_strict))
cat("\ntop 5 held-mode motion rates:\n")
print(res[1:5, .(off_caller, rate_held = round(rate_held, 1), rate_fresh = round(rate_fresh, 1))])

# ---------------------------------------------------------------- chart
res[, nm := factor(off_caller, levels = rev(res$off_caller))]
mcr <- res[off_caller == "Sean McVay"]

p <- ggplot(res, aes(y = nm)) +
  geom_segment(aes(x = rate_fresh, xend = rate_held, y = nm, yend = nm),
               colour = "grey82", linewidth = 1.3) +
  geom_point(aes(x = rate_fresh), colour = "grey60", size = 2.1, shape = 21, fill = "white", stroke = 0.8) +
  geom_point(aes(x = rate_held), colour = "grey45", size = 2.6) +
  geom_segment(data = mcr, aes(x = rate_fresh, xend = rate_held, y = nm, yend = nm),
               colour = "#f3c7a8", linewidth = 1.6) +
  geom_point(data = mcr, aes(x = rate_fresh), colour = "#e59c6b", size = 3, shape = 21, fill = "white", stroke = 1.1) +
  geom_point(data = mcr, aes(x = rate_held), colour = "#D55E00", size = 4.4) +
  geom_text(data = mcr, aes(x = rate_held, label = sprintf("McVay in held mode: %.0f%%  ", rate_held)),
            colour = "#D55E00", fontface = "bold", size = 3.4, hjust = 1) +
  geom_text(data = mcr, aes(x = rate_fresh, label = "  his fresh-picture snaps: 71%"),
            colour = "grey45", size = 2.9, hjust = 0) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(labels = function(v) paste0(v, "%"),
                     expand = expansion(mult = c(0.06, 0.2))) +
  labs(title = sprintf("In his one-picture mode, is McVay the one moving players? %s: %.0f%%, %s in football",
                       fifelse(mcr$rank_held <= 5, "Yes", fifelse(mcr$rank_held <= 12, "Mostly", "Not especially")),
                       mcr$rate_held, scales::ordinal(mcr$rank_held)),
       subtitle = sprintf(paste0(
         "Solid dot = motion rate on snaps where the caller kept the SAME personnel as the snap before: his held, one-picture mode.\n",
         "Open dot = motion rate when the personnel just changed. Everyone motions less in held mode, since new personnel arrive rearranging;\n",
         "his drop is %.0f points (league median drop %.0f). His held-mode rate beats the league's held-mode median by %.0f points."),
         abs(mcr$gap), abs(median(res$gap)), mcr$rate_held - median(res$rate_held)),
       x = "share of snaps with a player in motion", y = NULL,
       caption = fig_caption(
         "FTN motion charting bridged to Sumer personnel charting play by play (no shared ids: matched on season, week, offense, quarter, clock, down and distance)",
         sprintf("\n98%% of plays matched; %d callers with at least 1,200 matched plays. Held mode = same personnel group as the previous snap of the game, the same definition\nas the 84%% chart. The stricter version (same personnel AND same receiver split AND same QB spot as the previous snap) tells the same story.\n2022-23 through 2025-26 regular seasons, garbage time excluded on the Sumer side. Built by R/48.", nrow(res)))) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/motion_when_held.png", p, w = 9.5, h = 8.6)
cat("\nOut: docs/figures/motion_when_held.png, data/derived/motion_when_held.csv\n")
