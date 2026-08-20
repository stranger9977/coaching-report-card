# =============================================================================
# 50_blitz_vs_free.R -- the outlier chart: blitz rate against manufactured
# free rushers, one dot per coordinator.
#
# The ask, verbatim: "Did you add blitz rate as a comp? Like how little he
# does this with his overall free rushers? Because the optics of that graph
# would put him way more in outlier territory."
#
# Nothing new is computed: both axes come from data/derived/free_rushers.csv
# (R/28). x = blitz rate (share of dropbacks with 5+ rushers). y = share of
# dropbacks where a pressure came from a rusher NOBODY BLOCKED. The outlier
# corner is low blitz + many free rushers: pressure that is manufactured by
# design rather than bought with extra rushers.
#
# Conventions: plain language, no Michael/Nick in rendered text, season
# spans, no em dashes in rendered text.
#
# Out: docs/figures/blitz_vs_free.png
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales); library(ggrepel)
})
source("R/lib/theme_coach.R")

r <- fread("data/derived/free_rushers.csv")
stopifnot(nrow(r) >= 30)
r[, blitz := blitz_actual]
r[, free := unblocked_actual]
mx <- median(r$blitz); my <- median(r$free)

mac <- r[def_play_caller == "Mike Macdonald"]
named <- r[def_play_caller %in% c("Brian Flores", "Vic Fangio", "Steve Spagnuolo", "Don Martindale")]
r[, extreme := def_play_caller %in% c(r[order(-free)][1:2]$def_play_caller,
                                      r[order(blitz)][1:2]$def_play_caller) &
               !def_play_caller %in% c("Mike Macdonald", named$def_play_caller)]

cat(sprintf("Macdonald: blitz %.1f%% (rank %d of %d fewest), free rushers %.1f%% (rank %d of %d most)\n",
            mac$blitz, frank(r$blitz)[r$def_play_caller == "Mike Macdonald"], nrow(r),
            mac$free, mac$unblocked_rank, nrow(r)))
cat(sprintf("league medians: blitz %.1f%%, free rushers %.1f%%\n", mx, my))

mac_rank_blitz <- frank(r$blitz)[r$def_play_caller == "Mike Macdonald"]
mac[, mac_lab := sprintf("Mike Macdonald\nblitz: %.0f%% (%s-fewest of %d)\nfree rushers: %.1f%% (2nd-most)",
                         blitz, scales::ordinal(mac_rank_blitz), nrow(r), free)]

p <- ggplot(r, aes(blitz, free)) +
  geom_vline(xintercept = mx, linetype = "dashed", colour = "grey75", linewidth = 0.4) +
  geom_hline(yintercept = my, linetype = "dashed", colour = "grey75", linewidth = 0.4) +
  geom_point(colour = "grey62", size = 2.7, alpha = 0.85) +
  geom_point(data = named, colour = "#2B8CBE", size = 3.4) +
  geom_text_repel(data = named, aes(label = def_play_caller), colour = "#1d6a99",
                  size = 3, fontface = "bold", seed = 3, box.padding = 0.5,
                  min.segment.length = 0) +
  geom_point(data = mac, colour = "#D55E00", size = 5) +
  geom_text_repel(data = mac, aes(label = mac_lab),
                  colour = "#D55E00", size = 3.2, fontface = "bold", seed = 5,
                  box.padding = 1.1, min.segment.length = 0, lineheight = 1.05) +
  annotate("text", x = min(r$blitz), y = max(r$free), hjust = 0, vjust = 1,
           label = "rarely blitzes,\nyet floods the pocket with unblocked rushers",
           size = 3, colour = "grey40", fontface = "italic", lineheight = 1) +
  annotate("text", x = max(r$blitz), y = min(r$free), hjust = 1, vjust = 0,
           label = "blitzes often,\nfew free rushers to show for it",
           size = 3, colour = "grey40", fontface = "italic", lineheight = 1) +
  scale_x_continuous(labels = function(v) paste0(v, "%")) +
  scale_y_continuous(labels = function(v) paste0(v, "%")) +
  labs(title = "The pressure is manufactured: free rushers against blitz rate, one dot per coordinator",
       subtitle = paste0("Across: share of dropbacks sending five or more rushers. Up: share of dropbacks where a pressure came from a rusher nobody blocked.\n",
                         "Extra rushers are the expensive way to get someone home free: Martindale buys his with a 43% blitz rate. Macdonald gets there without paying."),
       x = "blitz rate (share of dropbacks with five or more rushers)",
       y = "dropbacks with an unblocked-rusher pressure",
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons",
         sprintf("\n%d coordinators with 700+ charted dropbacks. Both numbers are each coordinator's plain career rate.", nrow(r)),
         "\nA free rusher is worth about three times a won pass-rush block to the defense, and Macdonald manufactures them 2nd-best in football\nwhile ranking 24th at winning blocks: the pressure is scheme, not talent and not volume (R/28). Built by R/50.")) +
  theme_coach(grid = "none")

save_fig("docs/figures/blitz_vs_free.png", p, w = 11.5, h = 7.5)
cat("\nOut: docs/figures/blitz_vs_free.png\n")
