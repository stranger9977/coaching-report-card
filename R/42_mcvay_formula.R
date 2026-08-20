# =============================================================================
# 42_mcvay_formula.R -- the "25 plays seem like 250" composite, one graphic.
#
# The ask: players and coordinators are quoted saying Sean McVay "makes 25
# plays seem like 250." Four findings already on this board line up behind
# that quote; this script puts them in ONE chart so the argument reads
# top-to-bottom without flipping between four figures.
#
# NOTHING IS RECOMPUTED HERE. Each panel reads the derived CSV its source
# script shipped, so the numbers are exactly the carded ones:
#   Panel 1  data/derived/presnap_callers.csv  (R/25)  picture variety
#   Panel 2  data/derived/hold_and_vary.csv    (R/33)  same-personnel rate
#   Panel 3  data/derived/hold_and_vary.csv    (R/33)  play variety in streaks
#   Panel 4  data/derived/motion_callers.csv   (R/15)  motion vs run/pass guess
#
# HONESTY NOTES BAKED INTO THE LABELS.
# - Panel 1: McVay is 32nd of 36 in variety = 5th-FEWEST pictures; the axis
#   is drawn so "fewer pictures" reads left and the label says which end he
#   is on. Not "the fewest" -- 5th-fewest, the extreme end but not rank 1.
# - Panel 4: by raw size McVay's motion effect is 6th of 37, but the three
#   larger raw gaps carry no interval (thin samples). Two callers in the
#   league have an interval clear of zero: McVay (0.124) and Shanahan
#   (0.047, a third the size). The label says exactly that; earlier board
#   language ("McVay-only") was too strong and is not repeated here.
# - Trick-look rate (10.3% of snaps, ~2x league, 4th of 36, from the
#   fingerprint grid) stays in the caption as context, not a panel: it is a
#   plain rate with no situation control.
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season labels in span style ("2022-23 through 2025-26").
#
# Out: docs/figures/mcvay_formula.png
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(patchwork); library(scales)
})
source("R/lib/theme_coach.R")

ORANGE <- "#D55E00"; GREY <- "grey62"; BLUE <- "#2B8CBE"

variety <- fread("data/derived/presnap_callers.csv")
hold    <- fread("data/derived/hold_and_vary.csv")[!is.na(hold_rank)]
instr   <- fread("data/derived/hold_and_vary.csv")[!is.na(instr_rank)]
motion  <- fread("data/derived/motion_callers.csv")[model == "run_pass"]

stopifnot(nrow(variety) >= 30, nrow(hold) >= 30, nrow(motion) >= 30)

mcv <- list(
  var  = variety[off_caller == "Sean McVay"],
  hold = hold[off_caller == "Sean McVay"],
  ins  = instr[off_caller == "Sean McVay"],
  mot  = motion[off_play_caller == "Sean McVay"]
)

cat("=== the four numbers, as shipped by their source scripts ===\n")
cat(sprintf("P1 picture variety: %.3f bits, rank %d of %d (rank 1 = most variety, so %d = number %d counting from fewest)\n",
            mcv$var$H_variety, mcv$var$variety_rank, nrow(variety),
            mcv$var$variety_rank, nrow(variety) - mcv$var$variety_rank + 1))
cat(sprintf("P2 same-personnel rate, back-to-back snaps: %.1f%%, rank %d of %d (league median %.1f%%)\n",
            mcv$hold$hold_rate, mcv$hold$hold_rank, nrow(hold), median(hold$hold_rate)))
cat(sprintf("P3 play variety inside same-personnel streaks: %.3f bits, rank %d of %d\n",
            mcv$ins$instr_H, mcv$ins$instr_rank, nrow(instr)))
cat(sprintf("P4 motion effect on run/pass guessability: %.3f [%.3f, %.3f], raw rank %d of %d; interval-clear callers: %s\n",
            mcv$mot$gap, mcv$mot$lo, mcv$mot$hi, mcv$mot$rank, nrow(motion),
            paste(motion[lo > 0][order(-gap)]$off_play_caller, collapse = ", ")))

# ---------------------------------------------------------------- four charts + a diagram
# Fourth design, per direct feedback: break the composite into FOUR separate
# charts, each a plain ranked leaderboard in real units, plus a DIAGRAM that
# explains the mechanism. Entropy values are shown as "effectively different
# pictures/plays" (2^H), which is the standard way to turn that measure into
# a count a person can read; the caption says rare looks count less.

lolli <- function(dt, valcol, namecol, title, sub, xlab, capline, fname,
                  higher_is_mcvay = TRUE, pctx = FALSE) {
  d2 <- copy(dt)[, v := get(valcol)][, nm2 := get(namecol)]
  setorder(d2, v)
  d2[, nm2 := factor(nm2, levels = nm2)]
  mc2 <- d2[nm2 == "Sean McVay"]
  pp <- ggplot(d2, aes(v, nm2)) +
    geom_segment(aes(x = min(d2$v), xend = v, y = nm2, yend = nm2),
                 colour = "grey88", linewidth = 1.6) +
    geom_point(colour = "grey60", size = 2.3) +
    geom_segment(data = mc2, aes(x = min(d2$v), xend = v, y = nm2, yend = nm2),
                 colour = "#f3c7a8", linewidth = 1.6) +
    geom_point(data = mc2, colour = ORANGE, size = 4) +
    geom_text(data = mc2, aes(label = "Sean McVay"), colour = ORANGE,
              fontface = "bold", size = 3.4, hjust = -0.15) +
    scale_x_continuous(labels = if (pctx) function(v) paste0(v, "%") else waiver(),
                       expand = expansion(mult = c(0.01, 0.16))) +
    labs(title = title, subtitle = sub, x = xlab, y = NULL,
         caption = fig_caption(capline,
           "\nSumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded. Built by R/42.")) +
    theme_coach(grid = "none") +
    theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"),
          plot.subtitle = element_text(lineheight = 1.1))
  save_fig(fname, pp, w = 9.5, h = 8.6)
}

variety[, eff_pics := 2^H_variety]
lolli(variety, "eff_pics", "off_caller",
  "1. McVay shows fewer different pre-snap pictures than almost anyone",
  sprintf("Effectively different pictures each caller shows a defense: McVay about %.0f, league middle about %.0f. Only %d callers show fewer.",
          variety[off_caller=="Sean McVay"]$eff_pics, median(variety$eff_pics), nrow(variety) - mcv$var$variety_rank),
  "effectively different pre-snap pictures shown (rare ones count less)",
  "A picture = where the backs, tight ends and receivers line up, compared within the same downs and distances so situation does not fake variety (R/25)",
  "docs/figures/mcvay_formula_pictures.png")

lolli(hold, "hold_rate", "off_caller",
  "2. Nobody holds one group of players on the field like McVay",
  sprintf("Share of snaps keeping the previous snap's personnel on the field: McVay %.0f%%, first in football; league middle %.0f%%.",
          mcv$hold$hold_rate, median(hold$hold_rate)),
  "share of snaps keeping the same personnel as the snap before",
  "Same-personnel rate on back-to-back offensive snaps (R/33)",
  "docs/figures/mcvay_formula_hold.png", pctx = TRUE)

instr[, eff_plays := 2^instr_H]
lolli(instr, "eff_plays", "off_caller",
  "3. Yet inside those held stretches, almost nobody runs more different plays",
  sprintf("Effectively different plays coming out of a held stretch: McVay about %.1f, 4th of %d; league middle about %.1f.",
          instr[off_caller=="Sean McVay"]$eff_plays, nrow(instr), median(instr$eff_plays)),
  "effectively different plays out of the same held personnel (rare ones count less)",
  "Play variety within stretches where the personnel group never changes (R/33)",
  "docs/figures/mcvay_formula_variety.png")

# chart 4: motion effect with its uncertainty shown
m4 <- copy(motion)[, nm2 := off_play_caller]
setorder(m4, gap)
m4[, nm2 := factor(nm2, levels = nm2)]
m4[, clear := !is.na(lo) & lo > 0]
mc4 <- m4[nm2 == "Sean McVay"]; sh4 <- m4[nm2 == "Kyle Shanahan"]
p4 <- ggplot(m4, aes(gap, nm2)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                colour = "grey80", linewidth = 0.5, na.rm = TRUE) +
  geom_point(aes(shape = clear), colour = "grey55", size = 2.3, fill = "white") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21), guide = "none") +
  geom_errorbar(data = mc4, aes(xmin = lo, xmax = hi), orientation = "y",
                width = 0, colour = "#f3c7a8", linewidth = 0.9) +
  geom_point(data = mc4, colour = ORANGE, size = 4) +
  geom_text(data = mc4, aes(x = hi, label = "Sean McVay: clearly real"), colour = ORANGE,
            fontface = "bold", size = 3.3, hjust = -0.08) +
  geom_point(data = sh4, colour = BLUE, size = 3.2) +
  geom_text(data = sh4, aes(x = hi, label = "Shanahan: the only other clearly real one"),
            colour = BLUE, size = 2.9, hjust = -0.08) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.28))) +
  labs(title = "4. And when a player goes in motion, McVay's run/pass tell dies hardest",
       subtitle = paste0("How much harder run vs pass becomes to guess on each caller's motion snaps, against his own no-motion snaps.\n",
                         "Solid dots are effects the data clearly stands behind; open dots are not established (the uncertainty crosses zero, or the sample is too thin to tell).\n",
                         "He motions on 62% of snaps (4th of 37; league median 46%)."),
       x = "how much harder run vs pass is to guess when he motions (0 = no change)", y = NULL,
       caption = fig_caption(
         "The offense's own pre-snap movement, nothing to do with defensive safety rotation",
         "\nA few callers show bigger raw numbers on samples too thin to trust (open dots, uncertainty crossing zero); only McVay's and Shanahan's effects are clearly real (R/15).\nSumerSports + FTN charting, 2022-23 through 2025-26 regular seasons. Built by R/42.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/mcvay_formula_motion.png", p4, w = 9.5, h = 8.6)

# chart 4a: how often is he actually in motion? (the "is it every play?" answer)
mr <- copy(motion)[, nm2 := off_play_caller][, mrate := 100 * motion_rate]
setorder(mr, mrate)
mr[, nm2 := factor(nm2, levels = nm2)]
mcr <- mr[nm2 == "Sean McVay"]
pr <- ggplot(mr, aes(mrate, nm2)) +
  geom_segment(aes(x = 0, xend = mrate, y = nm2, yend = nm2), colour = "grey88", linewidth = 1.6) +
  geom_point(colour = "grey60", size = 2.3) +
  geom_vline(xintercept = 100, colour = "grey35", linewidth = 0.5, linetype = "dashed") +
  annotate("text", x = 99, y = 4.5, label = "every play\nwould be here", hjust = 1,
           size = 3, colour = "grey40", fontface = "italic", lineheight = 0.95) +
  geom_vline(xintercept = median(mr$mrate), colour = "grey70", linewidth = 0.4, linetype = "dotted") +
  annotate("text", x = median(mr$mrate) + 1, y = 2.1, label = "league middle",
           hjust = 0, size = 2.9, colour = "grey55") +
  geom_segment(data = mcr, aes(x = 0, xend = mrate, y = nm2, yend = nm2),
               colour = "#f3c7a8", linewidth = 1.6) +
  geom_point(data = mcr, colour = ORANGE, size = 4) +
  geom_text(data = mcr, aes(label = "Sean McVay: 62%"), colour = ORANGE,
            fontface = "bold", size = 3.4, hjust = -0.1) +
  scale_x_continuous(limits = c(0, 104), breaks = seq(0, 100, 25),
                     labels = function(v) paste0(v, "%")) +
  labs(title = "How often is a player actually in motion? A lot, and nowhere near every play",
       subtitle = paste0("Share of snaps with a player in motion before the snap: McVay 62% (4th of 37), league middle 46%, nobody past 74%.\n",
                         "So the motion chart compares his 62% motion snaps against his own 38% still snaps, about 1,600 plays."),
       x = "share of snaps with pre-snap motion", y = NULL,
       caption = fig_caption(
         "This is the OFFENSE's own player moving before the snap",
         "\nIt has nothing to do with the defensive \"shell\" talk elsewhere on this board, which is about the defense's safeties changing the coverage picture after the snap.\nFTN charting, 2022-23 through 2025-26 regular seasons. Built by R/42.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"),
        plot.subtitle = element_text(lineheight = 1.12))
save_fig("docs/figures/mcvay_formula_motionrate.png", pr, w = 9.5, h = 8.6)

# ---------------------------------------------------------------- the diagram
bx <- function(x1, x2, y1, y2, fill) annotate("rect", xmin = x1, xmax = x2, ymin = y1, ymax = y2,
                                              fill = fill, colour = NA)
tx <- function(x, y, lab, size = 3.6, face = "bold", col = "grey10", hj = 0.5, lh = 1.05)
  annotate("text", x = x, y = y, label = lab, size = size, fontface = face, colour = col, hjust = hj, lineheight = lh)
ar <- function(x1, x2, y) annotate("segment", x = x1, xend = x2, y = y, yend = y,
                                   linewidth = 0.7, colour = "grey45",
                                   arrow = arrow(length = unit(7, "pt"), type = "closed"))

pd <- ggplot() +
  bx(0.0, 2.4, 1.2, 3.0, "#eef1f4") +
  tx(1.2, 2.55, "The same 11 players\nstay on the field") +
  tx(1.2, 1.75, "84% of snaps,\nmost in football", 2.9, "plain", "grey45") +
  ar(2.5, 3.1, 2.1) +
  bx(3.2, 5.6, 1.2, 3.0, "#eef1f4") +
  tx(4.4, 2.55, "So the defense sees the\nsame picture, again and again") +
  tx(4.4, 1.75, "5th-fewest different\npictures in football", 2.9, "plain", "grey45") +
  ar(5.7, 6.3, 2.1) +
  bx(6.4, 8.8, 1.2, 3.0, "#fdeadd") +
  tx(7.6, 2.55, "Then one player moves,\nlate, before the snap") +
  tx(7.6, 1.75, "motion on 62% of snaps;\nthe run/pass tell dies", 2.9, "plain", "grey45") +
  annotate("segment", x = 8.9, xend = 9.5, y = c(2.1, 2.1, 2.1, 2.1), yend = c(3.6, 2.6, 1.6, 0.6),
           linewidth = 0.6, colour = "#D55E00", arrow = arrow(length = unit(6, "pt"), type = "closed")) +
  tx(9.65, 3.6, "outside zone", 3.3, "bold", "#C0504D", 0) +
  tx(9.65, 2.6, "man block, either side", 3.3, "bold", "#C0504D", 0) +
  tx(9.65, 1.6, "play action, any depth", 3.3, "bold", "#2B8CBE", 0) +
  tx(9.65, 0.6, "trick play", 3.3, "bold", "#C0504D", 0) +
  tx(9.65, 4.15, "...and out of that one picture,\nalmost his whole menu (4th-most variety)", 3.0, "italic", "grey35", 0) +
  scale_x_continuous(limits = c(-0.1, 12.3)) +
  scale_y_continuous(limits = c(0.2, 4.6)) +
  labs(title = "How 25 plays get dressed up as 250",
       subtitle = "The same people, the same picture, movement at the last moment, and any play in the menu on the way.",
       x = NULL, y = NULL,
       caption = fig_caption(
         "The numbers behind each box are charts 1 through 4 of this set",
         "\nReal example on film: against New Orleans in week 9 of 2025-26 he showed one picture 20 times and ran 11 different plays out of it, three for touchdowns. Built by R/42.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
save_fig("docs/figures/mcvay_formula_diagram.png", pd, w = 12.5, h = 4.6)

cat("\nOut: mcvay_formula_{diagram,pictures,hold,variety,motion}.png\n")
