# =============================================================================
# factory/71_madden_qb.R -- put Madden quarterback ratings INTO the models.
#
# Nick, 17 Aug: "lets use madden ratings in our models to get qb quality so we
# control for that and see if that changes things in the model factory. do the
# models get better? does controlling for qb quality matter?"
#
# Two separate questions, and they have different answers, so they are measured
# separately.
#
#   1. DO THE MODELS GET BETTER? Fit each target twice, situation only against
#      situation plus the quarterback's Madden ratings, and compare AUC, lift
#      over the lookup baseline, calibration and the rubric verdict.
#
#   2. DOES CONTROLLING FOR QB QUALITY MATTER? Even a model that barely moves
#      on AUC can move the thing we actually publish, which is the coach
#      residual. So the residual leaderboards are rebuilt under both models and
#      compared coach by coach.
#
# WHY THIS IS NOT LEAKAGE. Madden ratings ship in August, before a snap is
# played, so they are known to the play-caller at decision time. That is the
# test the factory uses everywhere. What WOULD be leakage is anything derived
# from how the quarterback then played, which is why the lagged-EPA controls
# live elsewhere and none of them are here.
#
# WHY THE PRIMARY QB RATHER THAN THE PASSER ON THE PLAY. Run plays have no
# passer, so joining on the play's passer would silently restrict the run/pass
# model to passes and destroy the target. The coach knows who his quarterback is
# before he calls anything, so every play of a team-season carries the rating of
# the man who took the most dropbacks that season.
#
# FAIRNESS. Madden covers 2017 to 2025 and the factory table starts in 2015, so
# both variants are fit on exactly the same rows, the ones where a rating
# exists. Otherwise the comparison would be confounded by sample.
#
# Out: docs/figures/factory/madden_qb.png
#      data/factory/madden_qb_experiments.csv
#      data/factory/madden_qb_residual_shift.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork); library(xgboost)
})
source("R/factory/lib_factory.R")
source("R/lib/theme_coach.R")

mt   <- as.data.table(readRDS("data/factory/model_table.rds"))
spec <- readRDS("data/factory/feature_spec.rds")
STATE <- spec$state

# ---------------------------------------------------------------- Madden QBs
MAD <- c("overallrating","awareness","throwpower","throwaccuracyshort",
         "throwaccuracymid","throwaccuracydeep","playaction","throwonrun",
         "playrecognition","speed","acceleration")
mad <- rbindlist(lapply(2017:2025, function(y)
  fread(sprintf("data/raw/madden/%d.csv", y), showProgress = FALSE)), fill = TRUE)
qb <- mad[position == "QB" & !is.na(overallrating),
          c("player_id","season","age","yearspro", MAD), with = FALSE]
qb <- unique(qb, by = c("player_id","season"))
cat(sprintf("Madden QB-seasons 2017-2025: %s\n", format(nrow(qb), big.mark = ",")))

# primary quarterback of each team-season, by dropbacks
prim <- mt[qb_dropback == 1 & !is.na(passer_player_id) & passer_player_id != "",
           .N, by = .(posteam, season, player_id = passer_player_id)]
setorder(prim, posteam, season, -N)
prim <- prim[, .SD[1], by = .(posteam, season)]
prim <- merge(prim, qb, by = c("player_id","season"))
setnames(prim, c(MAD, "age", "yearspro"),
         paste0("qb_", c(MAD, "age", "yearspro")))
QBF <- paste0("qb_", c(MAD, "age", "yearspro"))

d <- merge(mt, prim[, c("posteam","season", QBF), with = FALSE],
           by = c("posteam","season"), all.x = TRUE)
d <- d[season >= 2017 & !is.na(qb_overallrating)]
cat(sprintf("plays with a rated primary QB: %s of %s (%.0f%%), %d team-seasons matched\n",
            format(nrow(d), big.mark = ","), format(nrow(mt[season >= 2017]), big.mark = ","),
            100*nrow(d)/nrow(mt[season >= 2017]), nrow(prim)))
cat(sprintf("QB overall rating: mean %.1f, sd %.1f, range %d to %d\n",
            mean(prim$qb_overallrating), sd(prim$qb_overallrating),
            min(prim$qb_overallrating), max(prim$qb_overallrating)))

# ---------------------------------------------------------------- run both variants
TARGETS <- list(
  list(t = "y_pass",  lab = "Run or pass",  who = "off_play_caller"),
  list(t = "y_pa",    lab = "Play action",  who = "off_play_caller"),
  list(t = "y_blitz", lab = "Blitz",        who = "def_caller_opp")
)

rows <- list(); shifts <- list()
for (z in TARGETS) {
  cat(sprintf("\n================ %s ================\n", z$lab))
  # same rows for both arms: drop anything either arm could not score
  dd <- d[!is.na(get(z$t))]
  dd <- dd[complete.cases(dd[, c(STATE, QBF), with = FALSE])]
  cat(sprintf("rows in both arms: %s\n", format(nrow(dd), big.mark = ",")))

  fits <- list()
  for (arm in c("situation only", "plus Madden QB")) {
    f <- if (arm == "situation only") STATE else c(STATE, QBF)
    fit <- fit_target(dd, z$t, f, label = paste(z$lab, arm), perm_repeats = 1)
    sl  <- slice_table(fit)
    res <- coach_residuals(fit, who = z$who)
    g   <- grade(fit, sl, res)
    m   <- fit$metrics
    rows[[length(rows)+1]] <- data.table(
      target = z$lab, arm = arm, n = fit$n,
      auc = m$auc, auc_base = m$auc_base, lift = m$auc - m$auc_base,
      logloss = m$logloss, ece = m$ece,
      persist = res$persist, passed = all(g$pass),
      failed_rules = paste(g[pass == FALSE]$rule, collapse = "; "))
    fits[[arm]] <- list(fit = fit, res = res)
    cat(sprintf("  %-15s AUC %.4f (base %.4f, lift %+.4f) | ECE %.4f | persist %+.2f | %s\n",
                arm, m$auc, m$auc_base, m$auc - m$auc_base, m$ece, res$persist,
                if (all(g$pass)) "CLEARED" else paste("FAILS:", paste(g[pass == FALSE]$rule, collapse = ", "))))
  }

  # does the coach leaderboard move?
  a <- fits[["situation only"]]$res$career[, .(coach, resid_sit = resid, n)]
  b <- fits[["plus Madden QB"]]$res$career[, .(coach, resid_mad = resid)]
  cmp <- merge(a, b, by = "coach")
  cmp[, `:=`(target = z$lab, shift = resid_mad - resid_sit)]
  r <- cor(cmp$resid_sit, cmp$resid_mad)
  cat(sprintf("  coach residuals before vs after: r = %+.3f | mean |shift| = %.2f pts | max %.2f (%s)\n",
              r, mean(abs(cmp$shift)), max(abs(cmp$shift)),
              cmp[which.max(abs(shift))]$coach))
  shifts[[length(shifts)+1]] <- cmp
}

ex <- rbindlist(rows); sh <- rbindlist(shifts)
write_csv(as.data.frame(ex), "data/factory/madden_qb_experiments.csv")
write_csv(as.data.frame(sh), "data/factory/madden_qb_residual_shift.csv")
cat("\n=== summary ===\n"); print(ex[, .(target, arm, n, auc = round(auc,4),
                                         lift = round(lift,4), ece = round(ece,4),
                                         persist = round(persist,2), passed)])

# ---------------------------------------------------------------- chart
w <- dcast(ex, target ~ arm, value.var = c("auc","ece","persist"), fun.aggregate = NULL)
setnames(w, gsub(" ", "_", names(w)))
cat("\n--- what adding the quarterback changed ---\n")
print(w[, .(target,
            auc_sit = round(auc_situation_only, 4), auc_qb = round(auc_plus_Madden_QB, 4),
            d_auc = round(auc_plus_Madden_QB - auc_situation_only, 4),
            ece_sit = round(ece_situation_only, 4), ece_qb = round(ece_plus_Madden_QB, 4),
            persist_sit = round(persist_situation_only, 2),
            persist_qb = round(persist_plus_Madden_QB, 2))])

ex[, arm := factor(arm, levels = c("situation only","plus Madden QB"))]
pA <- ggplot(ex, aes(target, auc, fill = arm)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%.3f", auc), vjust = -0.5),
            position = position_dodge(width = 0.72), size = 3.2,
            fontface = "bold", colour = "grey25") +
  geom_hline(yintercept = RUBRIC$auc_min, linetype = "22", colour = "#D55E00",
             linewidth = 0.5) +
  annotate("text", x = 0.55, y = RUBRIC$auc_min + 0.012, label = "rubric floor 0.65",
           hjust = 0, size = 2.9, colour = "#8a3d00") +
  scale_fill_manual(values = c("situation only" = "#9db6c9",
                               "plus Madden QB" = "#2B8CBE")) +
  coord_cartesian(ylim = c(0.5, max(ex$auc) + 0.06)) +
  labs(title = "Do the models get better?",
       subtitle = "Out-of-sample AUC, season-grouped, same rows in both arms",
       x = NULL, y = "AUC") +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left")

sh[, target := factor(target, levels = ex[, unique(as.character(target))])]
rr <- sh[, .(r = cor(resid_sit, resid_mad), mad = mean(abs(shift))), by = target]
lab_s <- sh[, .SD[order(-abs(shift))][1:6], by = target]
pB <- ggplot(sh, aes(resid_sit, resid_mad)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.4) +
  geom_point(aes(size = n), colour = "#9db6c9", alpha = 0.75) +
  geom_point(data = lab_s, aes(size = n), colour = "#D55E00") +
  geom_text_repel(data = lab_s, aes(label = coach), size = 2.7, fontface = "bold",
                  colour = "#8a3d00", seed = 9, box.padding = 0.4,
                  min.segment.length = 0, max.overlaps = 18) +
  geom_text(data = rr, aes(x = -Inf, y = Inf, label = sprintf("r = %+.3f", r)),
            hjust = -0.25, vjust = 1.6, size = 3.2, fontface = "bold",
            colour = "grey25", inherit.aes = FALSE) +
  scale_size_area(max_size = 5, guide = "none") +
  facet_wrap(~ target, scales = "free") +
  labs(title = "Does it change who the leaderboard names?",
       subtitle = "Each coach's career residual under the situation-only model against the same figure once the quarterback is in it",
       x = "residual, situation only", y = "residual, plus Madden QB") +
  theme_coach(grid = "y")

p <- pA / pB + plot_layout(heights = c(1, 1.35)) +
  plot_annotation(
    title = "Adding the quarterback's Madden ratings to the models",
    subtitle = "Same plays, same folds, same rubric. The only difference is eleven ratings plus age and years pro for the team's primary quarterback.",
    caption = fig_caption(
      "Madden player ratings 2017-2025 joined to nflverse play-by-play on gsis player id",
      sprintf("%s plays with a rated primary quarterback, %d team-seasons. Ratings ship in August, so they are known before the season they describe.",
              format(nrow(d), big.mark = ","), nrow(prim)),
      paste0("\nRatings are the team's primary quarterback by dropbacks, applied to every one of that team's plays, because run plays have no passer and joining on the play's\n",
             "passer would quietly turn the run/pass model into a pass-only model. Both arms are fit on identical rows so the comparison is not a sample effect.\n",
             "Built by R/factory/71."))
  ) & theme(plot.title = element_text(size = 15))
save_fig("docs/figures/factory/madden_qb.png", p, w = 13, h = 11)
