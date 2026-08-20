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

# ---------------------------------------------------------------- strip panel
strip_panel <- function(dt, xcol, name_col, title, xlab, mc_lab,
                        second = NULL, second_lab = NULL, xpct = FALSE) {
  dt <- copy(dt)[, x := get(xcol)][, nm := get(name_col)]
  mc <- dt[nm == "Sean McVay"]
  med <- median(dt$x)
  p <- ggplot(dt, aes(x, y = 0)) +
    geom_vline(xintercept = med, linetype = "dotted", colour = "grey55") +
    geom_point(colour = GREY, size = 2.4, alpha = 0.75,
               position = position_jitter(height = 0.16, width = 0, seed = 7)) +
    geom_point(data = mc, aes(x, 0), colour = ORANGE, size = 4.6) +
    geom_text(data = mc, aes(x, 0, label = mc_lab), colour = ORANGE,
              fontface = "bold", size = 3.15, vjust = -1.7, lineheight = 0.95) +
    annotate("text", x = med, y = 0.62, label = "league median",
             size = 2.6, colour = "grey50", hjust = -0.05)
  if (!is.null(second)) {
    sc <- dt[nm == second]
    p <- p + geom_point(data = sc, aes(x, 0), colour = BLUE, size = 3.2) +
      geom_text(data = sc, aes(x, 0, label = second_lab), colour = BLUE,
                size = 2.8, vjust = 2.6)
  }
  p +
    scale_x_continuous(labels = if (xpct) function(v) paste0(v, "%") else waiver()) +
    scale_y_continuous(limits = c(-0.8, 0.9)) +
    labs(subtitle = title, x = xlab, y = NULL) +
    theme_coach(grid = "none") +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.subtitle = element_text(face = "bold", size = rel(0.98), colour = "grey15"),
          axis.title.x = element_text(size = rel(0.78), colour = "grey40", hjust = 0))
}

p1 <- strip_panel(variety, "H_variety", "off_caller",
  sprintf("1. He shows among the fewest pre-snap pictures in football: %s-fewest of %d",
          scales::ordinal(nrow(variety) - mcv$var$variety_rank + 1), nrow(variety)),
  "how many different pre-snap pictures a caller shows (fewer = further left; each dot is one caller)",
  sprintf("McVay: %s-fewest", scales::ordinal(nrow(variety) - mcv$var$variety_rank + 1)))

p2 <- strip_panel(hold, "hold_rate", "off_caller",
  sprintf("2. And he holds the picture: same personnel on back-to-back snaps %.0f%% of the time, 1st of %d (league %.0f%%)",
          mcv$hold$hold_rate, nrow(hold), median(hold$hold_rate)),
  "share of snaps keeping the previous snap's personnel group on the field",
  "McVay: 84%, 1st", xpct = TRUE)

p3 <- strip_panel(instr, "instr_H", "off_caller",
  sprintf("3. Inside those held stretches, close to the most different plays in the league: %s of %d",
          scales::ordinal(mcv$ins$instr_rank), nrow(instr)),
  "how many different plays come out of those held stretches (more = further right)",
  sprintf("McVay: %s-most", scales::ordinal(mcv$ins$instr_rank)))

p4 <- strip_panel(motion, "gap", "off_play_caller",
  "4. And when someone moves, the run/pass tell dies: the biggest motion effect in football that is clearly real, not noise",
  "how much harder run vs pass gets when he motions. He motions on 62% of snaps (4th of 37; league median 46%), so the comparison is his own other 38%",
  "McVay: +0.124, clearly real",
  second = "Kyle Shanahan", second_lab = "Shanahan: the only other clearly real effect, a third the size")

title_txt <- 'The quote is "he makes 25 plays seem like 250." The four measurements that agree'
sub_txt <- paste0(
  "Each strip is every qualified play-caller (2022-23 through 2025-26 seasons); the orange dot is Sean McVay.\n",
  "The method the four panels add up to: a handful of pictures, held on the field for stretches, with the play menu and the movement doing the disguising inside them.")

cap <- fig_caption(
  "Nothing new is computed here: each panel replots its source script's shipped numbers",
  paste0("\nPanel 1: variety of pre-snap pictures (skill-player alignment sets), down-and-distance-controlled and shrunk; his static pictures also TIP run/pass more than anyone's, which is\n",
         "the paradox the motion panel resolves (R/25). Panel 2 and 3: same-personnel rate on back-to-back snaps and play variety inside those stretches (R/33). Panel 4: how\n",
         "much a pre-snap model's run/pass guess degrades on motion snaps (FTN's any-motion flag, the offense's own movement, nothing to do with defensive shell rotation); three callers show bigger raw gaps but on samples too thin to trust\n",
         "(their uncertainty bands include zero); only his and Shanahan's effects are clearly more than noise (R/15). Context from the fingerprint grid: trick looks on 10.3% of snaps, about double the league, 4th of 36. Built by R/42."))

p_final <- p1 / p2 / p3 / p4 +
  plot_annotation(title = title_txt, subtitle = sub_txt, caption = cap,
                  theme = theme_coach(grid = "none"))

save_fig("docs/figures/mcvay_formula.png", p_final, w = 12.5, h = 12)
cat("\nOut: docs/figures/mcvay_formula.png\n")
