# =============================================================================
# 58_bj_file.R -- three Ben Johnson answers, one chart each:
#   1. Why Caleb Williams' sack rate disappeared (docs/figures/caleb_sacks.png)
#   2. Play-action rate vs the league, and the payoff  (bj_pa.png)
#   3. What he does with early-down running           (bj_earlyruns.png)
#
# The asks, verbatim: "why do you think sack rate seemingly disappeared for
# Caleb?" / "What is their play action rate compared to the league and his
# EPA per pass on PA vs. regular?" / "what about on early downs as it
# relates to running... what does Ben Johnson do?"
#
# Conventions: plain language, no Michael/Nick/Tej in rendered text, season
# spans, no em dashes in rendered text.
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & garbage_time == FALSE]
db <- d[is_dropback == TRUE]

# ---------------------------------------------------------------- 1. Caleb
cw <- db[off_team == "CHI" & season %in% 2024:2025]
cs <- cw[, .(sack = 100 * mean(is_sack, na.rm = TRUE),
             press = 100 * mean(pressure, na.rm = TRUE),
             sack_gp = 100 * mean(is_sack[pressure == TRUE], na.rm = TRUE),
             pa = 100 * mean(play_action, na.rm = TRUE),
             roll = 100 * mean(dropback_type == "ROLLOUT", na.rm = TRUE),
             n = .N), by = season]
cat("CALEB:\n"); print(cs)
cm <- melt(cs, id.vars = c("season", "n"),
           measure.vars = c("sack", "press", "sack_gp"))
cm[, metric := factor(variable, levels = c("sack", "press", "sack_gp"),
                      labels = c("sack rate", "pressure rate", "share of pressured\nsnaps ending in a sack"))]
cm[, yr := factor(season, levels = c(2024, 2025), labels = c("2024-25 (before)", "2025-26 (Ben Johnson)"))]

p1 <- ggplot(cm, aes(metric, value, fill = yr)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", value)),
            position = position_dodge(width = 0.7), vjust = -0.5,
            size = 3.3, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("grey65", "#D55E00"), name = NULL) +
  scale_y_continuous(labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0, 0.14))) +
  labs(title = "Caleb's sacks vanished, and the pressure did not: it stopped converting",
       subtitle = paste0("Chicago dropbacks, the season before Ben Johnson against his first. Sack rate fell 9.9% to 3.4% while pressure stayed identical;\n",
                         "the share of pressured snaps that ended in a sack collapsed from 33% to 11%. The escape hatches doubled: play action 17% to 35%,\n",
                         "designed rollouts 5.9% to 10.7%. He did not fix the protection; he made the pressure useless."),
       x = NULL, y = NULL,
       caption = fig_caption(
         "SumerSports play charting, Chicago dropbacks, garbage time excluded",
         sprintf("\n%s dropbacks in 2024-25, %s in 2025-26. League sack rate in 2025-26: 6.5%%.", format(cs[season==2024]$n, big.mark=","), format(cs[season==2025]$n, big.mark=",")),
         "\nThe league-wide version of this mechanism is on the movement chart: designed movement roughly halves sacks without cutting pressure,\nbecause a moving quarterback is harder to bring down, not harder to reach. Built by R/58.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/caleb_sacks.png", p1, w = 10.5, h = 6.8)

# ---------------------------------------------------------------- 2. PA rate + payoff
r <- db[!is.na(off_caller), .(n = .N, pa = 100 * mean(play_action, na.rm = TRUE)), by = off_caller][n >= 1200]
r[, rank := frank(-pa, ties.method = "min")]
setorder(r, -pa)
bj <- r[off_caller == "Ben Johnson"]
bj_epa_pa  <- mean(db[off_caller == "Ben Johnson" & play_action == TRUE]$expected_points_added, na.rm = TRUE)
bj_epa_no  <- mean(db[off_caller == "Ben Johnson" & play_action == FALSE]$expected_points_added, na.rm = TRUE)
lg_epa_pa  <- mean(db[play_action == TRUE]$expected_points_added, na.rm = TRUE)
lg_epa_no  <- mean(db[play_action == FALSE]$expected_points_added, na.rm = TRUE)
cat(sprintf("BJ PA %.1f%% rank %d/%d | EPA PA %+.2f vs noPA %+.2f (league %+.2f / %+.2f)\n",
            bj$pa, bj$rank, nrow(r), bj_epa_pa, bj_epa_no, lg_epa_pa, lg_epa_no))
r[, nm := factor(off_caller, levels = rev(r$off_caller))]

p2 <- ggplot(r, aes(pa, nm)) +
  geom_segment(aes(x = 0, xend = pa, y = nm, yend = nm), colour = "grey88", linewidth = 1.5) +
  geom_point(colour = "grey55", size = 2.4) +
  geom_vline(xintercept = median(r$pa), linetype = "dotted", colour = "grey60") +
  annotate("text", x = median(r$pa) + 0.4, y = 2.1, label = "league middle", hjust = 0, size = 2.9, colour = "grey55") +
  geom_segment(data = r[off_caller == "Ben Johnson"], aes(x = 0, xend = pa, y = nm, yend = nm),
               colour = "#f3c7a8", linewidth = 1.5) +
  geom_point(data = r[off_caller == "Ben Johnson"], colour = "#D55E00", size = 4.2) +
  geom_text(data = r[off_caller == "Ben Johnson"],
            aes(label = sprintf("  Ben Johnson: %.0f%%, %s", pa, scales::ordinal(rank))),
            colour = "#D55E00", fontface = "bold", size = 3.3, hjust = 0) +
  scale_x_continuous(labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0.01, 0.22))) +
  labs(title = sprintf("Ben Johnson play-fakes on %.0f%% of dropbacks, %s in football", bj$pa, scales::ordinal(bj$rank)),
       subtitle = sprintf(paste0("Share of dropbacks with a play fake, career, %d qualified callers; league middle %.0f%%.\n",
                                 "The payoff: his play-action dropbacks average %+.2f points per play against %+.2f without the fake,\n",
                                 "and both halves beat the league (%+.2f with, %+.2f without). The fake is his biggest single lever."),
                          nrow(r), median(r$pa), bj_epa_pa, bj_epa_no, lg_epa_pa, lg_epa_no),
       x = "share of dropbacks with a play fake", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nEarlier board finding worth pairing: his play action works COLD, with no run needed first, and his early-down play-fake rate is 2nd of 35. Built by R/58.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.62), colour = "grey45"))
save_fig("docs/figures/bj_pa.png", p2, w = 9.5, h = 7.6)

# ---------------------------------------------------------------- 3. early-down running
ed <- d[run_pass %in% c("P", "R") & down %in% 1:2]
t <- ed[!is.na(off_caller), .(n = .N, run = 100 * mean(run_pass == "R")), by = off_caller][n >= 800]
t[, rank := frank(-run, ties.method = "min")]
setorder(t, -run)
bj3 <- t[off_caller == "Ben Johnson"]
bj_run_epa  <- mean(ed[off_caller == "Ben Johnson" & run_pass == "R"]$expected_points_added, na.rm = TRUE)
bj_pass_epa <- mean(ed[off_caller == "Ben Johnson" & run_pass == "P"]$expected_points_added, na.rm = TRUE)
cat(sprintf("BJ early-down run %.1f%% rank %d/%d | run EPA %+.2f pass EPA %+.2f\n",
            bj3$run, bj3$rank, nrow(t), bj_run_epa, bj_pass_epa))
t[, nm := factor(off_caller, levels = rev(t$off_caller))]

p3 <- ggplot(t, aes(run, nm)) +
  geom_segment(aes(x = 0, xend = run, y = nm, yend = nm), colour = "grey88", linewidth = 1.4) +
  geom_point(colour = "grey55", size = 2.2) +
  geom_vline(xintercept = median(t$run), linetype = "dotted", colour = "grey60") +
  annotate("text", x = median(t$run) + 0.4, y = 2.1, label = "league middle", hjust = 0, size = 2.9, colour = "grey55") +
  geom_segment(data = t[off_caller == "Ben Johnson"], aes(x = 0, xend = run, y = nm, yend = nm),
               colour = "#f3c7a8", linewidth = 1.4) +
  geom_point(data = t[off_caller == "Ben Johnson"], colour = "#D55E00", size = 4.2) +
  geom_text(data = t[off_caller == "Ben Johnson"],
            aes(label = sprintf("  Ben Johnson: %.0f%%, %s-most of %d", run, scales::ordinal(rank), nrow(t))),
            colour = "#D55E00", fontface = "bold", size = 3.3, hjust = 0) +
  scale_x_continuous(labels = function(v) paste0(v, "%"), expand = expansion(mult = c(0.01, 0.32))) +
  labs(title = "The surprise: Ben Johnson runs on early downs MORE than most of the league",
       subtitle = sprintf(paste0("Share of 1st and 2nd down plays that are called runs, career; league middle %.0f%%. He sits %s-most of %d, run-heavier than the\n",
                                 "analytics narrative would guess, and his ledger shows the usual price: his early-down runs average %+.2f points per play\n",
                                 "against %+.2f on his early-down passes. The unpredictability lives in the sequencing and the fake, not in passing more."),
                          median(t$run), scales::ordinal(bj3$rank), nrow(t), bj_run_epa, bj_pass_epa),
       x = "share of early-down plays that are runs", y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, garbage time excluded",
         "\nPlain rates, not situation-adjusted: a team protecting leads runs more. His heavy early-down play-fake rate (2nd of 35) is the\ncounterweight that keeps the run-heavy mix from being cheap to defend. Built by R/58.")) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.6), colour = "grey45"))
save_fig("docs/figures/bj_earlyruns.png", p3, w = 9.5, h = 8.2)

cat("\nOut: caleb_sacks.png, bj_pa.png, bj_earlyruns.png\n")
