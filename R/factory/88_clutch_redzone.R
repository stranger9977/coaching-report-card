# =============================================================================
# factory/88_clutch_redzone.R -- "clutch", and the red zone.
#
# Michael, 17 Aug, looking at the leverage dial:
#   "So in theory... what play is called during a high leverage moment is more
#    impactful... right?"  "Is there any truth to some coaches dialing it up
#    better than others?"  "Clutch vibes."
#   "What play callers have success in the red zone?"
#
# Two questions, and they get the same treatment: measure it, then test whether
# it repeats. Clutch is the single most over-claimed idea in sports analysis,
# so the standard here is the one used everywhere else in this repo. A gap that
# does not predict itself across seasons is a description of what happened, not
# a skill, and the chart has to say which one it is.
#
# CLUTCH IS MEASURED AGAINST THE CALLER'S OWN BASELINE. Asking who has the best
# EPA in high-leverage spots just finds the best offenses. The question is
# whether a caller is BETTER THAN HIMSELF when it matters, so every number is
# his EPA in the moment minus his EPA the rest of the time.
#
# THREE DEFINITIONS OF THE MOMENT, because they disagree and the disagreement
# is the point. Top-quartile leverage means a CLOSE game and skews early (36%
# first quarter). Fourth quarter one-score is late and close. Playoffs is the
# one everybody actually means.
#
# Out: docs/figures/factory/clutch.png
#      docs/figures/factory/redzone_callers.png
#      data/factory/clutch.csv, data/factory/redzone.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

mt <- as.data.table(readRDS("data/factory/model_table.rds"))
d <- mt[!is.na(epa) & !is.na(off_play_caller) & off_play_caller != ""]
hi <- quantile(d$leverage, 0.75, na.rm = TRUE)
d[, `:=`(
  close   = leverage >= hi,
  late    = qtr >= 4 & abs(score_differential) <= 8,
  playoff = is_playoff == 1,
  rz      = is_redzone == 1
)]

# ---------------------------------------------------------------- signal test
# The correlation across seasons answers "does it repeat" but not "how much of
# what we are looking at is even real". This does: the observed spread between
# callers is signal plus sampling noise, and the sampling noise can be computed
# directly from each caller's own variance and sample size. What is left is the
# part that is actually about the caller.
signal_share <- function(stat, se) {
  tau2 <- var(stat) - mean(se^2)
  list(sd_obs = sd(stat), sd_noise = sqrt(mean(se^2)),
       sd_signal = if (tau2 > 0) sqrt(tau2) else 0,
       share = if (tau2 > 0) tau2/var(stat) else 0)
}

# ---------------------------------------------------------------- 1. clutch
MOMENTS <- list(
  list(k = "close",   lab = "Close game\n(top-quartile leverage)"),
  list(k = "late",    lab = "4th quarter or OT,\none score"),
  list(k = "playoff", lab = "Playoffs")
)

clutch_tbl <- function(x, k, min_in = 250, min_out = 800) {
  z <- x[, .(n_in  = sum(get(k)), n_out = sum(!get(k)),
             epa_in  = mean(epa[get(k)]), epa_out = mean(epa[!get(k)]),
             se = sqrt(var(epa[get(k)])/sum(get(k)) + var(epa[!get(k)])/sum(!get(k)))),
         by = .(caller = off_play_caller)]
  z <- z[n_in >= min_in & n_out >= min_out]
  z[, lift := epa_in - epa_out][]
}

cl <- rbindlist(lapply(MOMENTS, function(m) {
  z <- clutch_tbl(d, m$k); z[, moment := m$lab][, key := m$k][]
}))

# how much of each spread is real
SIG <- rbindlist(lapply(MOMENTS, function(m) {
  z <- cl[key == m$k]; s <- signal_share(z$lift, z$se)
  data.table(mkey = m$k, lab = gsub("\n", " ", m$lab), callers = nrow(z),
             sd_obs = s$sd_obs, sd_noise = s$sd_noise, share = s$share)
}))
ov <- d[, .(n = .N, stat = mean(epa), se = sd(epa)/sqrt(.N)), by = off_play_caller][n >= 2000]
s_ov <- signal_share(ov$stat, ov$se)
cat("\n=== how much of each spread is the caller, and how much is sampling noise? ===\n")
print(SIG[, .(lab, callers, sd_obs = round(sd_obs,4), sd_noise = round(sd_noise,4),
              signal = sprintf("%.0f%%", 100*share))])
cat(sprintf("  for contrast, OVERALL EPA per caller: %.0f%% signal\n", 100*s_ov$share))
SH_CLOSE <- SIG[mkey == "close"]$share
write_csv(as.data.frame(cl), "data/factory/clutch.csv")

cat("=== does anyone raise their game? EPA in the moment minus their own baseline ===\n")
for (m in MOMENTS) {
  z <- cl[key == m$k]; setorder(z, -lift)
  cat(sprintf("\n%s | %d callers | league lift %+.3f | spread sd %.3f\n",
              gsub("\n", " ", m$lab), nrow(z),
              mean(d[get(m$k) == TRUE]$epa) - mean(d[get(m$k) == FALSE]$epa), sd(z$lift)))
  print(head(z[, .(caller, n_in, lift = round(lift, 3))], 4))
  print(tail(z[, .(caller, n_in, lift = round(lift, 3))], 3))
}

# THE TEST. Split seasons odd against even and see whether a caller's clutch
# lift predicts his own clutch lift in the other half.
persist_lift <- function(k, min_in = 120, min_out = 400) {
  a <- clutch_tbl(d[season %% 2 == 1], k, min_in, min_out)[, .(caller, lift_odd = lift)]
  b <- clutch_tbl(d[season %% 2 == 0], k, min_in, min_out)[, .(caller, lift_even = lift)]
  m <- merge(a, b, by = "caller")
  if (nrow(m) < 10) return(NULL)
  ct <- cor.test(m$lift_odd, m$lift_even)
  list(m = m, ct = ct, k = k)
}
cat("\n=== IS CLUTCH A TRAIT? odd seasons against even ===\n")
pers <- list()
for (m in MOMENTS) {
  p <- persist_lift(m$k)
  if (is.null(p)) { cat(sprintf("%-38s too few callers\n", gsub("\n"," ",m$lab))); next }
  cat(sprintf("%-38s r = %+.3f [%.3f, %.3f]  p = %.3f  n = %d\n",
              gsub("\n", " ", m$lab), p$ct$estimate, p$ct$conf.int[1],
              p$ct$conf.int[2], p$ct$p.value, nrow(p$m)))
  pers[[m$k]] <- p
}
VERD <- sapply(pers, function(p) p$ct$conf.int[1] > 0.1)

cl[, moment := factor(moment, levels = sapply(MOMENTS, `[[`, "lab"))]
NAMED <- c("Andy Reid","Sean McVay","Kyle Shanahan","Ben Johnson","Matt LaFleur",
           "Sean Payton","Kevin O'Connell","Mike McDaniel","Josh McDaniels",
           "Zac Taylor","Arthur Smith","Liam Coen","Kellen Moore","Todd Monken")
lab <- cl[caller %in% NAMED]
pA <- ggplot(cl, aes(lift, moment)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_point(colour = "#c3ced8", size = 2.4, alpha = 0.9,
             position = position_jitter(height = 0.14, seed = 3)) +
  geom_point(data = lab, colour = "#2B8CBE", size = 2.9,
             position = position_jitter(height = 0.14, seed = 3)) +
  geom_text_repel(data = lab, aes(label = caller), size = 2.6, fontface = "bold",
                  colour = "#1d6a99", seed = 3, box.padding = 0.34,
                  min.segment.length = 0, max.overlaps = 14,
                  position = position_jitter(height = 0.14, seed = 3)) +
  labs(title = "Does anyone raise their game when it matters?",
       subtitle = "EPA per play in the moment minus that caller's own EPA the rest of the time",
       x = "EPA above his own baseline", y = NULL) +
  theme_coach(grid = "none")

pp <- pers[["close"]]
pB <- ggplot(pp$m, aes(lift_odd, lift_even)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#D55E00",
              fill = "#D55E00", alpha = 0.1, linewidth = 0.8) +
  geom_point(colour = "#9db6c9", size = 2.5, alpha = 0.85) +
  labs(title = sprintf("Is it a trait? r = %+.2f", pp$ct$estimate),
       subtitle = sprintf("Close-game lift, odd seasons against even, %d callers", nrow(pp$m)),
       x = "odd seasons", y = "even seasons") +
  theme_coach(grid = "y")

p1 <- (pA | pB) + plot_layout(widths = c(1.35, 1)) +
  plot_annotation(
    title = sprintf("Clutch play-calling is mostly noise: %.0f%% of the spread is real, against %.0f%% for overall play-calling quality",
                    100*SH_CLOSE, 100*s_ov$share),
    subtitle = "Every caller measured against himself, so this is not a leaderboard of the best offenses",
    caption = fig_caption(
      "nflverse play-by-play 2015-2025; play-caller attribution from samhoppen/NFL_public",
      "Minimum 250 plays in the moment and 800 outside it. Leverage is 4 x wp x (1 - wp), high is the top quartile.",
      paste0(sprintf("\nTwo tests, same answer. Splitting the observed spread into caller and sampling noise leaves %.0f%% real for close games and %.0f%% for the fourth quarter, against\n",
                     100*SH_CLOSE, 100*SIG[mkey == "late"]$share),
             sprintf("%.0f%% when the same test is run on overall EPA per caller, which is a skill that plainly exists. And a caller's close-game lift in one set of seasons predicts\n", 100*s_ov$share),
             sprintf("his lift in the other at r = %+.2f (p = %.2f). Someone leads this table every year and it is a different someone each time. Note also what the top row contains:\n", pp$ct$estimate, pp$ct$p.value),
             "top-quartile leverage means a CLOSE game and is 36% first quarter, so it is not the late-game drama the word clutch suggests. Built by R/factory/88.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/clutch.png", p1, w = 13.2, h = 6.8)

# ---------------------------------------------------------------- 2. red zone
rz <- d[rz == TRUE, .(plays = .N, epa = mean(epa), success = 100*mean(success, na.rm = TRUE),
                      pass = 100*mean(y_pass, na.rm = TRUE)),
        by = .(caller = off_play_caller)][plays >= 400]
base <- d[rz == FALSE, .(epa_out = mean(epa)), by = .(caller = off_play_caller)]
rz <- merge(rz, base, by = "caller")
rz[, rz_lift := epa - epa_out]
setorder(rz, -epa)
write_csv(as.data.frame(rz), "data/factory/redzone.csv")
cat(sprintf("\n=== red zone: %d callers with 400+ snaps inside the 20 | league EPA %+.3f, success %.1f%% ===\n",
            nrow(rz), mean(d[rz == TRUE]$epa), 100*mean(d[rz == TRUE]$success, na.rm = TRUE)))
cat("\n--- best in the red zone ---\n")
print(head(rz[, .(caller, plays, epa = round(epa,3), success = round(success,1), pass = round(pass,1))], 8))
cat("\n--- worst ---\n")
print(tail(rz[, .(caller, plays, epa = round(epa,3), success = round(success,1))], 5))

rzp <- {
  a <- d[rz == TRUE & season %% 2 == 1, .(n = .N, e = mean(epa)), by = .(caller = off_play_caller)][n >= 200]
  b <- d[rz == TRUE & season %% 2 == 0, .(n = .N, e = mean(epa)), by = .(caller = off_play_caller)][n >= 200]
  m <- merge(a, b, by = "caller", suffixes = c("_odd","_even"))
  list(m = m, ct = cor.test(m$e_odd, m$e_even))
}
cat(sprintf("\nred zone EPA persistence, odd against even: r = %+.3f [%.3f, %.3f] p = %.4f, n = %d callers\n",
            rzp$ct$estimate, rzp$ct$conf.int[1], rzp$ct$conf.int[2], rzp$ct$p.value, nrow(rzp$m)))
rz_se <- d[is_redzone == 1, .(n = .N, stat = mean(epa), se = sd(epa)/sqrt(.N)),
           by = off_play_caller][n >= 400]
s_rz <- signal_share(rz_se$stat, rz_se$se)
cat(sprintf("red zone signal share: %.0f%% (observed sd %.4f, noise sd %.4f)\n",
            100*s_rz$share, s_rz$sd_obs, s_rz$sd_noise))

top <- rbind(head(rz, 12)[, side := "Best inside the 20"],
             tail(rz, 12)[, side := "Worst inside the 20"])
setorder(top, epa); top[, caller := factor(caller, levels = caller)]
pC <- ggplot(top, aes(epa, caller, fill = side)) +
  geom_vline(xintercept = mean(d[rz == TRUE]$epa), linetype = "22",
             colour = "grey45", linewidth = 0.5) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%+.3f", epa), hjust = ifelse(epa >= 0, -0.2, 1.2)),
            size = 2.85, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Best inside the 20" = "#2B8CBE",
                               "Worst inside the 20" = "#D55E00")) +
  scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
  labs(title = "Red zone EPA per play by play-caller",
       subtitle = "Dashed line is the league average inside the 20",
       x = "EPA per play inside the 20", y = NULL) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", axis.text.y = element_text(size = rel(0.85)))

pD <- ggplot(rzp$m, aes(e_odd, e_even)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.3) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#2B8CBE",
              fill = "#2B8CBE", alpha = 0.1, linewidth = 0.8) +
  geom_point(colour = "#9db6c9", size = 2.6, alpha = 0.85) +
  geom_text_repel(data = rzp$m[caller %in% NAMED], aes(label = caller), size = 2.7,
                  fontface = "bold", colour = "#1d6a99", seed = 8,
                  box.padding = 0.4, min.segment.length = 0, max.overlaps = 18) +
  geom_point(data = rzp$m[caller %in% NAMED], colour = "#2B8CBE", size = 3) +
  labs(title = sprintf("Does red zone skill repeat? r = %+.2f", rzp$ct$estimate),
       subtitle = sprintf("Red zone EPA, odd seasons against even, %d callers", nrow(rzp$m)),
       x = "odd seasons", y = "even seasons") +
  theme_coach(grid = "y")

p2 <- (pC | pD) + plot_layout(widths = c(1, 1.05)) +
  plot_annotation(
    title = sprintf("Who moves the ball inside the 20, and why the leaderboard is shakier than it looks (%.0f%% of the spread is real)",
                    100*s_rz$share),
    subtitle = "EPA per play on snaps inside the opponent 20, play-callers with at least 400 of them, 2015 to 2025",
    caption = fig_caption(
      "nflverse play-by-play 2015-2025; play-caller attribution from samhoppen/NFL_public",
      sprintf("%d callers with 400 or more red zone snaps. League average inside the 20 is %+.3f EPA per play.",
              nrow(rz), mean(d[rz == TRUE]$epa)),
      paste0("\nThis is raw team performance with a caller's name on it, so part of it is the roster rather than the caller. Two checks worth making before anyone is called a red\n",
             sprintf("zone specialist. Splitting the spread into caller and sampling noise leaves %.0f%% real, against %.0f%% when the same test is run on overall EPA. And a caller's red\n",
                     100*s_rz$share, 100*s_ov$share),
             sprintf("zone EPA in one set of seasons predicts the other at only r = %+.2f (p = %.2f). Sean Payton leading is the most durable name here; most of the rest of the order\n",
                     rzp$ct$estimate, rzp$ct$p.value),
             "would look different over a different set of seasons. Built by R/factory/88.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/redzone_callers.png", p2, w = 13.4, h = 7.2)
