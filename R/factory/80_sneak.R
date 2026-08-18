# =============================================================================
# factory/80_sneak.R -- everybody knows it works and nobody runs it.
#
# From the Sumer mining workflow, and re-derived here from scratch before
# publishing. Sumer charts a run concept called SNEAK. Among short-yardage runs
# it is the single most efficient call in football and it is used on one in
# eight of them.
#
# THE COMPARISON IS MATCHED. Sneaks against ALL runs would be meaningless,
# because a sneak only happens on short yardage. Every number here compares a
# sneak with other RUNS in the same distance band, and then again inside each
# down, which is the version that survives.
#
# WHOSE HABIT IS IT. The workflow's verifier found the caller-level version of
# sneak rate does not hold up: the persistence rides on three men, and it
# travels with the building rather than the man. So this is framed by TEAM, and
# the team-level stability is reported rather than assumed.
#
# Out: docs/figures/factory/sneak.png
#      data/factory/sneak.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(scales); library(patchwork)
})
source("R/factory/lib_sumer.R")
source("R/lib/theme_coach.R")

d <- load_sumer()
sy <- d[run_pass == "R" & (short_yardage == TRUE | distance <= 2)]
sy[, sneak := run_concept == "SNEAK"]
cat(sprintf("short-yardage runs %s | sneaks %s (%.1f%%)\n",
            format(nrow(sy), big.mark = ","), format(sum(sy$sneak), big.mark = ","),
            100*mean(sy$sneak)))

ov <- sy[, .(n = .N, epa = mean(expected_points_added, na.rm = TRUE),
             fd = 100*mean(first_down_gained == TRUE)), by = sneak]
cat("\noverall:\n"); print(ov)

bydown <- sy[distance <= 2 & down %in% 1:4,
             .(n = .N, epa = mean(expected_points_added, na.rm = TRUE),
               fd = 100*mean(first_down_gained == TRUE)), by = .(down, sneak)]
bydown[, dl := paste0(c("1st","2nd","3rd","4th")[down], " down")]
cat("\nby down (2 yards or fewer):\n"); print(bydown[order(down, -sneak)])
write_csv(as.data.frame(bydown), "data/factory/sneak.csv")

# whose habit? team level, and how stable
tm <- sy[, .(n = .N, rate = 100*mean(sneak)), by = .(team = off_team, season)][n >= 40]
tc <- tm[, .(seasons = .N, n = sum(n), rate = weighted.mean(rate, n)), by = team][seasons >= 3]
setorder(tc, -rate)
a <- tm[season %% 2 == 1, .(r = weighted.mean(rate, n)), by = team]
b <- tm[season %% 2 == 0, .(r = weighted.mean(rate, n)), by = team]
mm <- merge(a, b, by = "team", suffixes = c("_odd","_even"))
ct <- cor.test(mm$r_odd, mm$r_even)
cat(sprintf("\nteam sneak rate, odd vs even seasons: r = %+.2f [%.2f, %.2f] p = %.4f, n = %d teams\n",
            ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value, nrow(mm)))
cat("\nmost:\n"); print(head(tc, 5)); cat("least:\n"); print(tail(tc, 5))

# ---------------------------------------------------------------- chart
bydown[, lab := fifelse(sneak, "Quarterback sneak", "Any other run")]
bydown[, lab := factor(lab, levels = c("Any other run","Quarterback sneak"))]
pA <- ggplot(bydown, aes(dl, epa, fill = lab)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%+.2f", epa), vjust = ifelse(epa >= 0, -0.5, 1.4)),
            position = position_dodge(width = 0.72), size = 3.2,
            fontface = "bold", colour = "grey25") +
  geom_text(aes(y = -0.10, label = sprintf("n=%s", format(n, big.mark = ","))),
            position = position_dodge(width = 0.72), size = 2.5, colour = "grey45") +
  scale_fill_manual(values = c("Any other run" = "#9db6c9", "Quarterback sneak" = "#D55E00")) +
  scale_y_continuous(limits = c(-0.14, 0.92)) +
  labs(title = "It is better on every down",
       subtitle = "EPA per play on runs with two yards or fewer to go",
       x = NULL, y = "EPA per play") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")

top <- rbind(head(tc, 8), tail(tc, 8))
setorder(top, rate); top[, team := factor(team, levels = team)]
pB <- ggplot(top, aes(rate, team)) +
  geom_vline(xintercept = 100*mean(sy$sneak), linetype = "22",
             colour = "grey45", linewidth = 0.5) +
  geom_col(aes(fill = rate > 100*mean(sy$sneak)), width = 0.68) +
  geom_text(aes(label = sprintf("%.0f%%", rate)), hjust = -0.2, size = 2.9,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("TRUE" = "#D55E00", "FALSE" = "#9db6c9"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.16)),
                     labels = function(x) paste0(x, "%")) +
  labs(title = "And it is a franchise habit, not a coach one",
       subtitle = sprintf("Share of short-yardage runs that are sneaks. Dashed line is the league at %.0f%%.",
                          100*mean(sy$sneak)),
       x = "share of short-yardage runs", y = NULL) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(face = "bold", size = rel(0.9)))

p <- (pA | pB) + plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = sprintf("Everybody knows the sneak works. It is used on %.0f%% of short-yardage runs.", 100*mean(sy$sneak)),
    subtitle = "Quarterback sneaks against every other run with two yards or fewer to go, SumerSports charting 2022 to 2025",
    caption = fig_caption(
      "SumerSports play charting 2022-2025",
      sprintf("%s short-yardage runs, %s of them sneaks. Sneaks gain a first down %.0f%% of the time against %.0f%% for other runs.",
              format(nrow(sy), big.mark = ","), format(sum(sy$sneak), big.mark = ","),
              ov[sneak == TRUE]$fd, ov[sneak == FALSE]$fd),
      paste0(sprintf("\nComparing sneaks with all runs would be meaningless because a sneak only happens on short yardage, so every comparison here is against other RUNS at the same\n"),
             sprintf("distance, and it holds inside every down. On fourth and short it is worth %+.2f EPA against %+.2f. Whose habit is it: the caller-level version of this does not\n",
                     bydown[down == 4 & sneak == TRUE]$epa, bydown[down == 4 & sneak == FALSE]$epa),
             sprintf("survive scrutiny, but the TEAM version does, repeating at r = %+.2f across odd and even seasons. Some buildings sneak and some do not. Built by R/factory/80.",
                     ct$estimate))),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/sneak.png", p, w = 13, h = 6.8)
