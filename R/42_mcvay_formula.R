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

# ---------------------------------------------------------------- one shared scale
# Redesign after feedback that four separate panels with raw units read
# unclearly: every measurement becomes "his place among all callers" on one
# 0-100 scale, oriented so the direction the claim describes is always to
# the RIGHT. Four rows, one axis, no units to decode.

pctl <- function(v) 100 * (frank(v) - 1) / (length(v) - 1)

rows <- rbindlist(list(
  variety[, .(nm = off_caller, x = pctl(-H_variety), row = "r1")],
  hold[,    .(nm = off_caller, x = pctl(hold_rate),  row = "r2")],
  instr[,   .(nm = off_caller, x = pctl(instr_H),    row = "r3")],
  motion[,  .(nm = off_play_caller, x = pctl(gap),   row = "r4")]
))

row_titles <- c(
  r1 = sprintf("Shows FEWER different pre-snap pictures than almost anyone (%s-fewest of %d)",
               scales::ordinal(nrow(variety) - mcv$var$variety_rank + 1), nrow(variety)),
  r2 = sprintf("Keeps the same players on the field snap after snap (%.0f%% of the time, 1st of %d; league %.0f%%)",
               mcv$hold$hold_rate, nrow(hold), median(hold$hold_rate)),
  r3 = sprintf("But runs almost the most DIFFERENT plays inside those stretches (%s of %d)",
               scales::ordinal(mcv$ins$instr_rank), nrow(instr)),
  r4 = "And when a player goes in motion, run vs pass gets harder to guess: the biggest effect in the league that is clearly real"
)
rows[, row_f := factor(row, levels = rev(c("r1","r2","r3","r4")))]

mc  <- rows[nm == "Sean McVay"]
sha <- rows[nm == "Kyle Shanahan" & row == "r4"]
thin <- rows[row == "r4" & nm %in% motion[lo <= 0 & gap > mcv$mot$gap]$off_play_caller]

p <- ggplot(rows, aes(x, row_f)) +
  geom_point(colour = GREY, size = 2.6, alpha = 0.75,
             position = position_jitter(height = 0.14, width = 0, seed = 7)) +
  geom_point(data = thin, shape = 21, colour = "grey45", fill = "white", stroke = 0.9, size = 2.8) +
  geom_text(data = thin[1], aes(label = "open dots: bigger raw numbers, samples too thin to trust"),
            colour = "grey45", size = 2.7, vjust = 2.6, hjust = 0.85) +
  geom_point(data = sha, colour = BLUE, size = 3.4) +
  geom_text(data = sha, aes(label = "Shanahan: the only other clearly real one"),
            colour = BLUE, size = 2.8, vjust = -1.4, hjust = 0.6) +
  geom_point(data = mc, colour = ORANGE, size = 5) +
  geom_text(data = mc, aes(label = "McVay"), colour = ORANGE,
            fontface = "bold", size = 3.3, vjust = -1.5) +
  scale_x_continuous(limits = c(-3, 103), breaks = c(2, 98),
                     labels = c("least of what the line says", "most of what the line says")) +
  scale_y_discrete(labels = NULL) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = rel(0.9), face = "italic", colour = "grey45"))

# row titles drawn above each strip
for (rr in c("r1","r2","r3","r4")) {
  p <- p + annotate("text", x = -2, y = which(levels(rows$row_f) == rr) + 0.42,
                    label = paste0(match(rr, c("r1","r2","r3","r4")), ". ", row_titles[[rr]]),
                    hjust = 0, size = 3.4, fontface = "bold", colour = "grey15")
}

title_txt <- 'The quote is "he makes 25 plays seem like 250." Four measurements agree'
sub_txt <- paste0(
  "Every grey dot is one NFL play-caller (2022-23 through 2025-26 seasons); the orange dot is Sean McVay.\n",
  "All four rows share one scale: his place among all callers, from least to most of what each line says.\n",
  "Read together: a handful of pictures, held on the field, with the play menu and the movement doing the disguising inside them.")

cap <- fig_caption(
  "Each row replots numbers already shipped by this project's earlier charts; nothing new is computed here",
  paste0("\nRow 1: how many different pre-snap pictures (where the skill players line up) a caller shows, compared within the same downs and distances (R/25). His static\n",
         "pictures also TIP run vs pass more than anyone's, which is the puzzle row 4 answers. Rows 2 and 3: keeping the same personnel on back-to-back snaps, and how\n",
         "many different plays come out of those stretches (R/33). Row 4: how much harder a run/pass guess gets on his motion snaps; he motions on 62% of snaps (4th of 37,\n",
         "league median 46%), so the comparison is his own other 38%; this is the offense's own movement, nothing to do with defensive safety rotation. Three callers show\n",
         "bigger raw motion numbers but on samples too thin to trust; only his and Shanahan's are clearly real (R/15). Built by R/42."))

p_final <- p + labs(title = title_txt, subtitle = sub_txt, caption = cap) +
  theme(plot.margin = margin(10, 16, 8, 10))

save_fig("docs/figures/mcvay_formula.png", p_final, w = 12.5, h = 8)
cat("\nOut: docs/figures/mcvay_formula.png\n")
