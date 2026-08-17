# =============================================================================
# factory/87_tendencies.R -- is the CALLER a coin flip, or only the situation?
#
# Nick, 17 Aug: "i want to see tendencies in certain situations... we can view
# the data in this coin flip situations and see if coaches are hitting on 16 or
# if they are as much of a coin flip themselves."
#
# This is the missing half of the certainty work. The model says a spot is
# 50/50 because that is what the LEAGUE does there. A defensive coordinator is
# not preparing for the league, he is preparing for one man, and that man may
# throw 72% of the time in the exact spot the league treats as a toss-up. The
# model calls it ambiguous; the tape does not.
#
# Two views:
#
#   1. SECOND DOWN, which is where coordinators hunt for tendencies, split by
#      distance. Who sits nearest 50/50 in each bucket.
#   2. THE MODEL'S OWN COIN FLIPS. Take only the snaps where the situation-only
#      model landed between 50 and 60%, then ask what each caller actually did.
#      Distance from 50% is how much a coordinator gains by scouting him
#      rather than scouting the situation.
#
# 2026 play-callers are not in our data, so "expected 2026 caller" is
# approximated by everyone who called plays in 2025, and that is stated on the
# chart rather than implied.
#
# Out: docs/figures/factory/tendencies_second_down.png
#      docs/figures/factory/tendencies_coinflip.png
#      data/factory/tendencies.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

mt <- as.data.table(readRDS("data/factory/model_table.rds"))
pr <- as.data.table(readRDS("data/factory/y_pass_preds.rds"))

ACTIVE <- sort(unique(mt[season == 2025 & !is.na(off_play_caller) &
                         off_play_caller != ""]$off_play_caller))
cat(sprintf("callers who called plays in 2025, used as the 2026 proxy: %d\n", length(ACTIVE)))

# ---------------------------------------------------------------- 1. second down
d2 <- mt[!is.na(off_play_caller) & off_play_caller != "" & !is.na(y_pass) & down == 2]
d2[, bucket := cut(ydstogo, c(0, 2, 6, 10), labels = c("2nd & 1-2","2nd & 3-6","2nd & 7-10"))]
d2 <- d2[!is.na(bucket)]
lg2 <- d2[, .(lg = 100*mean(y_pass)), by = bucket]

t2 <- d2[off_play_caller %in% ACTIVE,
         .(n = .N, pass = 100*mean(y_pass)), by = .(caller = off_play_caller, bucket)][n >= 150]
t2 <- merge(t2, lg2, by = "bucket")
t2[, from50 := abs(pass - 50)]
t2[, vs_lg := pass - lg]
setorder(t2, bucket, from50)
write_csv(as.data.frame(t2), "data/factory/tendencies.csv")

cat("\n=== second down, closest to a true coin flip ===\n")
for (b in levels(t2$bucket)) {
  x <- t2[bucket == b]
  cat(sprintf("\n%s | league %.1f%% pass | %d callers\n", b, x$lg[1], nrow(x)))
  print(head(x[, .(caller, n, pass = round(pass,1), from50 = round(from50,1))], 4))
  bj <- which(x$caller == "Ben Johnson")
  if (length(bj)) cat(sprintf("  Ben Johnson: %.1f%% pass, rank %d of %d\n",
                              x[bj]$pass, bj, nrow(x)))
}

HL <- c("Ben Johnson","Chip Kelly","Sean McVay","Kyle Shanahan","Andy Reid",
        "Liam Coen","Kevin Stefanski","Kellen Moore","Mike McDaniel","Klint Kubiak")
t2[, hl := caller %in% HL]
lab2 <- t2[hl == TRUE]

p1 <- ggplot(t2, aes(pass, bucket)) +
  geom_vline(xintercept = 50, linetype = "22", colour = "#D55E00", linewidth = 0.6) +
  geom_point(data = t2[hl == FALSE], colour = "#c3ced8", size = 2.5, alpha = 0.9) +
  geom_point(data = lg2, aes(x = lg, y = bucket), shape = 124, size = 8,
             colour = "#1e2126", inherit.aes = FALSE) +
  geom_point(data = lab2, aes(colour = caller == "Ben Johnson"), size = 3.2) +
  geom_text_repel(data = lab2, aes(label = caller, colour = caller == "Ben Johnson"),
                  size = 2.85, fontface = "bold", seed = 2, box.padding = 0.42,
                  min.segment.length = 0, max.overlaps = 24, direction = "y") +
  geom_text(data = lg2, aes(x = lg, y = bucket, label = sprintf("league %.0f%%", lg)),
            inherit.aes = FALSE, vjust = -1.9, size = 2.9, colour = "grey35") +
  annotate("text", x = 50, y = 3.62, label = "a true coin flip", size = 3,
           colour = "#8a3d00", fontface = "bold") +
  scale_colour_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#2B8CBE"), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(20, 85)) +
  coord_cartesian(clip = "off", ylim = c(0.6, 3.45)) +
  labs(
    title = "Second down is where coordinators hunt for tendencies, and Ben Johnson does not give them one",
    subtitle = "Pass rate on second down by distance, one dot per play-caller. The tall bar is the league; the dashed line is a genuine 50/50.",
    x = "pass rate", y = NULL,
    caption = fig_caption(
      "nflverse play-by-play 2015-2025; play-caller attribution from samhoppen/NFL_public",
      sprintf("Callers who called plays in 2025, minimum 150 snaps in the bucket. %d caller-buckets.", nrow(t2)),
      paste0("\nThe league is run-heavy on 2nd and short and pass-heavy on 2nd and long, so sitting near 50% means different things in each row: on 2nd and 1-2 the balanced\n",
             "caller is the one who throws MORE than everyone else, and on 2nd and 7-10 he is the one who runs more. Johnson is the closest to a coin flip in the first two\n",
             "buckets, 47.7% and 50.3%. On 2nd and 7-10 he is third at 58.3%, behind Chip Kelly at 52.9%. Built by R/factory/87."))
  ) +
  theme_coach(grid = "none")
save_fig("docs/figures/factory/tendencies_second_down.png", p1, w = 12, h = 6.6)

# ---------------------------------------------------------------- 2. the model's coin flips
pr <- pr[!is.na(off_play_caller) & off_play_caller != ""]
pr[, certainty := pmax(p, 1 - p)]
cf <- pr[certainty < 0.6]
tc <- cf[off_play_caller %in% ACTIVE,
         .(n = .N, pass = 100*mean(y), epa = mean(epa, na.rm = TRUE)),
         by = .(caller = off_play_caller)][n >= 400]
tc[, from50 := abs(pass - 50)]
setorder(tc, from50)
cat(sprintf("\n=== inside the model's coin flips (%s snaps), how balanced is each caller? ===\n",
            format(nrow(cf), big.mark = ",")))
cat(sprintf("league pass rate in coin-flip spots: %.1f%% | caller spread: %.1f to %.1f\n",
            100*mean(cf$y), min(tc$pass), max(tc$pass)))
cat("\n--- genuinely a coin flip themselves ---\n")
print(head(tc[, .(caller, n, pass = round(pass,1), from50 = round(from50,1))], 6))
cat("\n--- the model says 50/50 but they do not ---\n")
print(tail(tc[, .(caller, n, pass = round(pass,1), from50 = round(from50,1))], 6))

tc[, side := fifelse(pass >= 50, "leans pass", "leans run")]
setorder(tc, pass)
tc[, caller := factor(caller, levels = caller)]
p2 <- ggplot(tc, aes(pass, caller)) +
  geom_vline(xintercept = 50, linetype = "22", colour = "#D55E00", linewidth = 0.6) +
  geom_segment(aes(x = 50, xend = pass, yend = caller, colour = side), linewidth = 1.5) +
  geom_point(aes(colour = side), size = 2.6) +
  geom_text(aes(label = sprintf("%.0f%%", pass),
                hjust = ifelse(pass >= 50, -0.35, 1.35)),
            size = 2.7, colour = "grey30") +
  scale_colour_manual(values = c("leans pass" = "#2B8CBE", "leans run" = "#D55E00")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.10, 0.10))) +
  labs(
    title = "When the model calls a spot a coin flip, most callers really are one",
    subtitle = "Each caller's own pass rate on snaps where the situation-only model landed between 50 and 60%",
    x = "his own pass rate in the league's coin-flip spots", y = NULL,
    caption = fig_caption(
      "Situation-only run/pass model, season-grouped out-of-sample, 2015 to 2025",
      sprintf("%s coin-flip snaps. Callers active in 2025 with at least 400 of them, %d of them.",
              format(nrow(cf), big.mark = ","), nrow(tc)),
      paste0("\nThis is the number a defensive coordinator actually cares about, because he is preparing for one man rather than for the league. The answer is mostly reassuring\n",
             "for the model: 21 of 31 callers sit within five points of a true 50/50, and the whole field spans 39% to 57% with a standard deviation of 3.9 points. The spots\n",
             "the model finds ambiguous are ambiguous for the individual too. The exceptions are worth naming: Andy Reid throws 57% of the time in them and Zac Robinson runs\n",
             "on 61%, so scouting those two beats scouting the situation. Built by R/factory/87."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left",
        axis.text.y = element_text(size = rel(0.82)))
save_fig("docs/figures/factory/tendencies_coinflip.png", p2, w = 11.5, h = 8.4)
