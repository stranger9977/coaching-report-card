# =============================================================================
# factory/20_dashboard.R -- the diagnostics Nick asked to see.
#
# "slice the performance by time, space, all the categoricals and continuous
#  binned in a smart way okay so i can see where the model is week and where it
#  is strong like every feature can be bined and you do average actual vs
#  predicted with confidence bars or like the r plots with the confidence
#  interval shading so i can see ok"
#
# Per target, three figures:
#   _calib   the calibration curve, deciles of predicted against actual
#   _slices  every binned feature, actual with a 95% ribbon against predicted
#   _resid   coach residual leaderboard, only drawn if the model cleared
#
# The slice figure is the one that matters. A model can look fine overall and
# be badly wrong inside the 10 or inside two minutes, and those are exactly the
# spots where a coach residual would otherwise look like a personality.
#
# Out: docs/figures/factory/*.png
# =============================================================================

suppressMessages({
  library(data.table); library(ggplot2); library(readr); library(scales)
})
source("R/lib/theme_coach.R")

FIG <- "docs/figures/factory"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
fits <- list.files("data/factory/fits", full.names = TRUE)

PRETTY <- c(down = "Down", ydstogo = "Yards to go", yardline_100 = "Yards from end zone",
            score_diff = "Score differential", half_clock = "Seconds left in half",
            quarter = "Quarter", posteam_to = "Timeouts left", vegas_wp = "Win probability",
            season = "Season", leverage = "Leverage")

for (f in fits) {
  o <- readRDS(f); tg <- o$target
  cleared <- all(o$grade$pass)
  cat(sprintf("%-16s %s\n", tg, if (cleared) "cleared" else "NOT cleared"))

  # ---- calibration ---------------------------------------------------------
  dec <- as.data.table(o$deciles)
  pc <- ggplot(dec, aes(predicted, actual)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed",
                colour = ink_baseline, linewidth = 0.4) +
    geom_errorbar(aes(ymin = actual - 1.96*se, ymax = actual + 1.96*se),
                  width = 0, colour = "#9db6c9", linewidth = 0.8) +
    geom_point(colour = "#2B8CBE", size = 2.6) +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(title = sprintf("%s: is the model honest about its own confidence?", o$label),
         subtitle = sprintf("Deciles of predicted probability against what actually happened. ECE %.4f.", o$metrics$ece),
         x = "predicted", y = "actual",
         caption = fig_caption("Season-grouped cross-validated predictions",
           sprintf("%s plays, %d to %d.", format(o$n, big.mark = ","), o$seasons[1], o$seasons[2]),
           "\nPoints on the dashed line mean the model's stated probability is the real one. Off the line means every residual in that band is partly model error. Built by R/factory.")) +
    theme_coach(grid = "y")
  save_fig(file.path(FIG, sprintf("%s_calib.png", tg)), pc, w = 7.5, h = 6)

  # ---- slices --------------------------------------------------------------
  sl <- as.data.table(o$slices)[n >= 100]
  sl[, feature_lab := PRETTY[feature]]
  # order bins sensibly inside each facet
  sl[, ord := suppressWarnings(as.numeric(sub("^[\\(\\[]([-0-9.e+]+).*", "\\1", bin)))]
  sl[is.na(ord), ord := suppressWarnings(as.numeric(bin))]
  sl[is.na(ord), ord := rank(bin), by = feature]
  setorder(sl, feature, ord)
  # Bin labels repeat across features ("3" is a down and a quarter), so the
  # factor is keyed by feature|bin and only the bin half is displayed.
  sl[, bin_key := paste(feature, bin, sep = "|")]
  sl[, bin_key := factor(bin_key, levels = unique(bin_key))]

  ps <- ggplot(sl, aes(bin_key, group = 1)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#9db6c9", alpha = 0.45) +
    geom_line(aes(y = actual), colour = "#1c5b80", linewidth = 0.8) +
    geom_line(aes(y = predicted), colour = "#D55E00", linewidth = 0.8, linetype = "22") +
    facet_wrap(~feature_lab, scales = "free", ncol = 3) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_x_discrete(labels = function(k) sub("^[^|]*\\|", "", k)) +
    labs(title = sprintf("%s: where the model is right and where it is not", o$label),
         subtitle = "Blue is what actually happened with a 95% band. Orange dashed is what the model said. Gaps are where residuals cannot be trusted.",
         x = NULL, y = NULL,
         caption = fig_caption("Season-grouped cross-validated predictions, binned by feature",
           sprintf("Bins holding at least 100 plays. %s of slices with 200+ plays sit inside their interval.",
                   sprintf("%.0f%%", 100*mean(as.data.table(o$slices)[n >= 200]$inside))),
           "\nThis is the figure that decides whether a coach residual in a given situation is signal or model error. Built by R/factory.")) +
    theme_coach(grid = "y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.62)),
          strip.text = element_text(size = rel(0.85)),
          panel.spacing = unit(0.9, "lines"))
  save_fig(file.path(FIG, sprintf("%s_slices.png", tg)), ps, w = 13, h = 9)

  # ---- residual leaderboard, only if the model earned the right ------------
  if (cleared) {
    cr <- as.data.table(o$residuals$career)
    setnames(cr, 1, "coach")
    setorder(cr, -resid)
    top <- rbind(head(cr, 12), tail(cr, 12))
    top[, side := rep(c("Does it more than the situation says", "Does it less"), each = 12)]
    top[, coach := factor(coach, levels = rev(coach))]
    pr <- ggplot(top, aes(resid, coach, fill = side)) +
      geom_col(width = 0.7) +
      geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.4) +
      geom_text(aes(label = sprintf("%+.1f", resid),
                    hjust = ifelse(resid >= 0, -0.2, 1.2)),
                size = 3, fontface = "bold", colour = "grey25") +
      scale_fill_manual(values = c("#2B8CBE", "#D55E00")) +
      scale_x_continuous(labels = function(x) paste0(x, "pp"),
                         expand = expansion(mult = c(0.12, 0.12))) +
      labs(title = sprintf("%s: who deviates most from the situation", o$label),
           subtitle = sprintf("Career residual in percentage points. Year-over-year persistence r = %.2f.", o$residuals$persist),
           x = NULL, y = NULL,
           caption = fig_caption("Residual = actual rate minus the situation-only model's expected rate",
             sprintf("%s, %d to %d. Coaches with enough charted plays.", o$label, o$seasons[1], o$seasons[2]),
             "\nOnly drawn because this model cleared all five rubric rules, including residual persistence. Built by R/factory.")) +
      theme_coach(grid = "none") + theme(legend.position = "none")
    save_fig(file.path(FIG, sprintf("%s_resid.png", tg)), pr, w = 11, h = 7)
  }
}
cat("\ndashboard figures written to", FIG, "\n")
