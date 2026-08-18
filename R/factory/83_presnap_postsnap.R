# =============================================================================
# factory/83_presnap_postsnap.R -- static before the snap, moving after it.
#
# Michael, 17 Aug:
#   "I think the Seahawks defense was very predictable in terms of formation.
#    From what I remember they ran Nickel like the whole time and didn't give af
#    what the offense was doing. But their 'unpredictability' came when the ball
#    was snapped in the way the disguised coverage and blitzes - idk if we have a
#    way of looking at that"
#
# We do now, and he is right on both halves, which makes this the cleanest
# two-axis idea in the project. Sumer charts the defensive personnel package on
# every snap AND the middle-of-field shell both before and after the snap, so
# the two things he is describing can be measured separately:
#
#   PRE-SNAP RESPONSIVENESS   how much a coordinator changes his personnel
#                             package when the offense changes theirs. Nickel
#                             rate against 11 personnel minus nickel rate
#                             against heavy personnel. A coordinator who matches
#                             the offense scores high. One who lines up the same
#                             way regardless scores low.
#
#   POST-SNAP DISGUISE        shell rotation rate above what the situation calls
#                             for, from R/factory/85.
#
# They are close to independent, so the interesting coordinators are in the
# corners, and Macdonald sits in the one Michael described.
#
# Out: docs/figures/factory/presnap_postsnap.png
#      data/factory/presnap_postsnap.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/factory/lib_sumer.R")
source("R/lib/theme_coach.R")

d <- load_sumer()

# ---------------------------------------------------------------- pre-snap
p <- d[def_caller != "" & defensive_personnel_package %in% c("Nickel","Base","Dime")]
p[, off_p := fifelse(offensive_personnel_basic == "11", "light",
              fifelse(offensive_personnel_basic %in% c("12","21","13","22"), "heavy", NA_character_))]
q <- p[!is.na(off_p), .(n = .N, nk = 100*mean(defensive_personnel_package == "Nickel")),
       by = .(caller = def_caller, off_p)]
w <- dcast(q, caller ~ off_p, value.var = c("n","nk"))
w <- w[n_light >= 400 & n_heavy >= 250]
w[, respond := nk_light - nk_heavy]
cat(sprintf("coordinators with enough of both: %d | league mean responsiveness %.1f points\n",
            nrow(w), mean(w$respond)))

# is responsiveness itself a trait?
qs <- p[!is.na(off_p), .(n = .N, nk = 100*mean(defensive_personnel_package == "Nickel")),
        by = .(caller = def_caller, season, off_p)]
ws <- dcast(qs, caller + season ~ off_p, value.var = c("n","nk"))
ws <- ws[n_light >= 150 & n_heavy >= 90][, respond := nk_light - nk_heavy]
a <- ws[season %% 2 == 1, .(r = mean(respond)), by = caller]
b <- ws[season %% 2 == 0, .(r = mean(respond)), by = caller]
mm <- merge(a, b, by = "caller", suffixes = c("_odd","_even"))
ctp <- cor.test(mm$r_odd, mm$r_even)
cat(sprintf("responsiveness persistence: r = %+.2f [%.2f, %.2f] p = %.4f, n = %d\n",
            ctp$estimate, ctp$conf.int[1], ctp$conf.int[2], ctp$p.value, nrow(mm)))

# ---------------------------------------------------------------- post-snap
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell & middle_of_field_coverage_look != middle_of_field_coverage_played)]
rr <- sumer_resid(d[has_shell == TRUE], "rotate", "def_caller")

z <- merge(w[, .(caller, respond, nk_light, nk_heavy)],
           rr$career[, .(caller, rotate_resid = resid, n_shell = n)], by = "caller")
setorder(z, -rotate_resid)
write_csv(as.data.frame(z), "data/factory/presnap_postsnap.csv")
cat(sprintf("\ncoordinators on both axes: %d | correlation between them: %+.2f\n",
            nrow(z), cor(z$respond, z$rotate_resid)))
mac <- z[caller == "Mike Macdonald"]
cat(sprintf("\nMACDONALD: responsiveness %.1f (rank %d of %d, 1 = most responsive) | rotation %+.1f (rank %d)\n",
            mac$respond, which(z[order(-respond)]$caller == "Mike Macdonald"), nrow(z),
            mac$rotate_resid, which(z[order(-rotate_resid)]$caller == "Mike Macdonald")))
cat(sprintf("  nickel vs 11 personnel %.0f%%, vs heavy %.0f%%\n", mac$nk_light, mac$nk_heavy))

# ---------------------------------------------------------------- chart
NAMED <- c("Mike Macdonald","Vic Fangio","Brian Flores","Steve Spagnuolo","Dan Quinn",
           "Todd Bowles","Jim Schwartz","DeMeco Ryans","Lou Anarumo","Aaron Glenn",
           "Sean McDermott","Bill Belichick","Ejiro Evero","Zach Orr","Raheem Morris",
           "Jesse Minter","Dennis Allen","Robert Saleh","Jeff Ulbrich","Anthony Weaver")
lab <- z[caller %in% NAMED]
mx <- median(z$respond); my <- 0

p1 <- ggplot(z, aes(respond, rotate_resid)) +
  annotate("rect", xmin = -Inf, xmax = mx, ymin = my, ymax = Inf,
           fill = "#6b4c9a", alpha = 0.07) +
  annotate("text", x = min(z$respond) + 1, y = max(z$rotate_resid) - 0.6,
           label = "SAME LOOK EVERY SNAP,\nTHEN IT MOVES",
           hjust = 0, size = 3.5, fontface = "bold", colour = "#4a3570", lineheight = 1.05) +
  annotate("text", x = max(z$respond) - 1, y = min(z$rotate_resid) + 0.6,
           label = "matches your personnel,\nthen plays what it showed",
           hjust = 1, size = 3, colour = "grey45", lineheight = 1.05) +
  geom_hline(yintercept = my, colour = ink_baseline, linewidth = 0.4) +
  geom_vline(xintercept = mx, linetype = "22", colour = ink_baseline, linewidth = 0.4) +
  geom_point(aes(size = n_shell), colour = "#9db6c9", alpha = 0.8) +
  geom_point(data = lab, aes(size = n_shell), colour = "#6b4c9a") +
  geom_point(data = z[caller == "Mike Macdonald"], aes(size = n_shell), colour = "#D55E00") +
  geom_text_repel(data = lab[caller != "Mike Macdonald"], aes(label = caller), size = 2.8,
                  fontface = "bold", colour = "#4a3570", seed = 12,
                  box.padding = 0.42, min.segment.length = 0, max.overlaps = 22) +
  geom_text_repel(data = z[caller == "Mike Macdonald"], aes(label = caller), size = 3.4,
                  fontface = "bold", colour = "#8a3d00", seed = 12,
                  box.padding = 0.7, min.segment.length = 0, nudge_y = 2.2) +
  scale_size_area(max_size = 6, guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, " pts")) +
  labs(
    title = "Static before the snap, moving after it: the Seahawks defense on two axes",
    subtitle = "How much a coordinator changes his personnel to match the offense, against how often his coverage shell rotates after the snap",
    x = "pre-snap responsiveness  (nickel rate vs 11 personnel minus vs heavy personnel)  → adapts more",
    y = "post-snap shell rotation above expected",
    caption = fig_caption(
      "SumerSports play charting 2022-2025; defensive play-caller attribution from samhoppen/NFL_public",
      sprintf("%d coordinators on both axes. The two measures correlate at %+.2f, so they really are separate things.",
              nrow(z), cor(z$respond, z$rotate_resid)),
      paste0(sprintf("\nA defense can be readable before the snap and unreadable after it, and these two measures are close to independent, so they separate cleanly. Macdonald\n"),
             sprintf("plays nickel on %.0f%% of snaps against 11 personnel and %.0f%% against heavy, a gap of %.0f points where the league average is %.0f, which puts him %d of %d for\n",
                     mac$nk_light, mac$nk_heavy, mac$respond, mean(w$respond),
                     which(z[order(-respond)]$caller == "Mike Macdonald"), nrow(z)),
             sprintf("adapting his personnel. Then he rotates the shell %+.1f points more than the situation calls for. Responsiveness is itself a repeatable trait (r = %+.2f). The shaded\n",
                     mac$rotate_resid, ctp$estimate),
             "corner is the combination he described: give the offense nothing to read before the snap, and change everything the moment it starts. Built by R/factory/83.")))+
  theme_coach(grid = "y")
save_fig("docs/figures/factory/presnap_postsnap.png", p1, w = 12.6, h = 7.6)
