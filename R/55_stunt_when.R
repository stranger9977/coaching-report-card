# =============================================================================
# 55_stunt_when.R -- WHEN does each coordinator stunt: on blitzes, or on
# regular rushes?
#
# The ask, verbatim: "How much did the stunt on blitz's and non blitzs? I
# think Houston doesn't do any of that and the Seahawks do it a lot"
#
# The hunch inverts in a sharper way than guessed: the league stunts MORE
# on blitzes (29% vs 25%). Macdonald does the opposite: 3rd-most stunting
# on regular rushes, 34th of 39 on blitzes. He saves the moving pieces for
# the downs where he is NOT paying for extra rushers, which is the
# free-rusher formula again. Houston (DeMeco Ryans) is the mirror image.
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
#
# Out: docs/figures/stunt_when.png
#      data/derived/stunt_when.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE & is_dropback == TRUE &
       !is.na(def_caller) & !is.na(stunt)]

t <- d[, .(n = .N,
           st_reg = 100 * mean(stunt[blitz == FALSE]), n_reg = sum(!blitz),
           st_blitz = 100 * mean(stunt[blitz == TRUE]), n_blitz = sum(blitz)),
       by = def_caller][n >= 700]
t[, r_reg := frank(-st_reg, ties.method = "min")]
t[, r_blitz := frank(-st_blitz, ties.method = "min")]
write_csv(as.data.frame(t), "data/derived/stunt_when.csv")

mac <- t[def_caller == "Mike Macdonald"]; hou <- t[def_caller == "DeMeco Ryans"]
cat(sprintf("Macdonald: regular %.1f%% (rank %d of %d), blitz %.1f%% (rank %d)\n",
            mac$st_reg, mac$r_reg, nrow(t), mac$st_blitz, mac$r_blitz))
cat(sprintf("Ryans (HOU): regular %.1f%% (rank %d), blitz %.1f%% (rank %d)\n",
            hou$st_reg, hou$r_reg, hou$st_blitz, hou$r_blitz))
cat(sprintf("league medians: regular %.1f%%, blitz %.1f%%\n", median(t$st_reg), median(t$st_blitz)))

named <- t[def_caller %in% c("DeMeco Ryans", "Dan Quinn", "Vic Fangio")]

p <- ggplot(t, aes(st_reg, st_blitz)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey75", linewidth = 0.4) +
  annotate("text", x = max(t$st_reg) - 1, y = max(t$st_reg) + 1.2, label = "same rate both ways",
           hjust = 1, size = 2.8, colour = "grey55", fontface = "italic", angle = 29) +
  geom_point(colour = "grey62", size = 2.7, alpha = 0.85) +
  geom_point(data = named, colour = "#2B8CBE", size = 3.4) +
  geom_text_repel(data = named, aes(label = def_caller), colour = "#1d6a99",
                  size = 3, fontface = "bold", seed = 3, box.padding = 0.5, min.segment.length = 0) +
  geom_point(data = mac, colour = "#D55E00", size = 5) +
  geom_text_repel(data = mac, aes(label = sprintf("Mike Macdonald\nregular rush: %.0f%%, 3rd-most\nblitz: %.0f%%, %s of %d",
                                                  st_reg, st_blitz, scales::ordinal(r_blitz), nrow(t))),
                  colour = "#D55E00", size = 3.2, fontface = "bold", seed = 7,
                  box.padding = 1.2, min.segment.length = 0, lineheight = 1.05) +
  annotate("text", x = min(t$st_reg) + 0.3, y = max(t$st_blitz), hjust = 0, vjust = 1,
           label = "stunts mostly when blitzing\n(the moving pieces ride along with extra rushers)",
           size = 2.9, colour = "grey40", fontface = "italic", lineheight = 1) +
  annotate("text", x = max(t$st_reg), y = min(t$st_blitz) + 0.3, hjust = 1, vjust = 0,
           label = "stunts mostly on regular rushes\n(manufacturing pressure without paying for it)",
           size = 2.9, colour = "grey40", fontface = "italic", lineheight = 1) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  scale_y_continuous(labels = function(v) paste0(v, "%")) +
  labs(title = "When does Macdonald stunt? On the rushes where he is NOT blitzing",
       subtitle = paste0("Stunt rate on regular rushes across, on blitzes up. The league stunts MORE when it blitzes (median 29% vs 25%).\n",
                         "Macdonald runs the opposite way: 3rd-most stunting on regular rushes, 34th of 39 on blitzes. Houston is the mirror image."),
       x = "stunt rate on regular rushes (four or fewer sent)",
       y = "stunt rate on blitzes (five or more sent)",
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         sprintf("\n%d defensive callers with 700+ charted dropbacks. A stunt = rushers crossing or looping so blockers must pass off moving targets.", nrow(t)),
         paste0("\nRead with the free-rusher file: his pressure comes from unblocked rushers (2nd-most in football) at the league's 10th-lowest blitz rate, and the stunts\n",
                "are concentrated exactly where that trick lives, on the ordinary-looking four-man rushes. Built by R/55."))) +
  theme_coach(grid = "none")
save_fig("docs/figures/stunt_when.png", p, w = 11.5, h = 8)
cat("\nOut: docs/figures/stunt_when.png, data/derived/stunt_when.csv\n")
