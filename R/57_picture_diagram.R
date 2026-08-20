# =============================================================================
# 57_picture_diagram.R -- draw the exact charted picture behind the Detroit
# film card (R/44), as asked: "give me a picture of the formation you are
# claiming he is running out of."
#
# The charted signature of the 11-snap picture, Rams at Detroit, week 1 of
# 2024-25:  RB middle off the line | two slot receivers left off the line |
# one receiver left on the line | tight end right on the line; QB in pistol;
# 11 personnel, a 3x1 (trips left, tight end alone to the right).
#
# The charting records position type, side, and on/off the line, NOT exact
# splits or depths, so spacing here is illustrative and the caption says so.
#
# Out: docs/figures/picture_diagram.png
# =============================================================================

suppressMessages({ library(data.table); library(ggplot2) })
source("R/lib/theme_coach.R")

pl <- data.table(
  x    = c(-1.0, -0.5, 0, 0.5, 1.0,   0,    0,     0,    -3.9, -2.9, -2.2,  2.0),
  y    = c(0.35, 0.35, 0.35, 0.35, 0.35, -0.55, -1.45, 0.35,  0.35, -0.25, -0.25, 0.35),
  lab  = c("T","G","C","G","T",  "QB", "RB",  "",   "WR", "slot", "slot", "TE"),
  role = c(rep("ol",5), "qb", "rb", "none", "wr", "wr", "wr", "te")
)
pl <- pl[lab != ""]

p <- ggplot(pl, aes(x, y)) +
  geom_hline(yintercept = 0.72, colour = "grey30", linewidth = 0.7) +
  annotate("text", x = 4.35, y = 0.9, label = "line of scrimmage", hjust = 1,
           size = 2.9, colour = "grey45", fontface = "italic") +
  geom_point(aes(fill = role), shape = 21, size = 11, colour = "grey25", stroke = 0.8,
             show.legend = FALSE) +
  scale_fill_manual(values = c(ol = "grey85", qb = "#f3c7a8", rb = "#D55E00",
                               wr = "#2B8CBE", te = "#08306b")) +
  geom_text(aes(label = lab), size = 3, fontface = "bold",
            colour = c(rep("grey30",5), "grey20", "white", "white", "white", "white", "white")) +
  annotate("text", x = -3.0, y = 1.35, label = "trips left: one receiver on the line,\ntwo slots off it",
           size = 3.1, colour = "#1d6a99", fontface = "bold", lineheight = 1) +
  annotate("text", x = 2.0, y = 1.35, label = "tight end alone,\non the line",
           size = 3.1, colour = "#08306b", fontface = "bold", lineheight = 1) +
  annotate("text", x = 1.1, y = -0.55, label = "pistol: QB short, back directly behind",
           hjust = 0, size = 3, colour = "grey40", fontface = "italic") +
  coord_fixed(ratio = 0.62, xlim = c(-4.6, 4.6), ylim = c(-2.0, 1.75)) +
  labs(title = "The picture itself: 11 personnel, trips left, tight end right, pistol",
       subtitle = "The exact charted alignment shown 11 times at Detroit in week 1 of 2024-25, with 9 different plays called out of it.",
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports player charting",
         "\nThe charting records each player's position type, side of the field, and on-or-off the line; it does not record exact splits or depths,\nso the spacing drawn here is illustrative. Built by R/57.")) +
  theme_coach(grid = "none") +
  theme(axis.text = element_blank(), axis.ticks = element_blank())
save_fig("docs/figures/picture_diagram.png", p, w = 9.5, h = 5.6)
cat("Out: docs/figures/picture_diagram.png\n")
