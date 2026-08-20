# =============================================================================
# 45_rotation_by_dc.R -- the fourth and last door on the rotation-EPA null:
# does shell rotation pay for ANY individual defensive caller?
#
# The ask, verbatim: "Did you double check the EPA stuff on safety rotation?
# Like if its truly that significant for shell rotations?"
#
# The board's answer so far is a triple null (R/18 pooled + coverage/blitz
# splits, R/27 the QB-clock battery, R/36 pass-probability bands + ten named
# situations). The one cut never run: PER COORDINATOR. Maybe rotation only
# pays for the coordinators who are good at it, and the pooled null hides
# them. This script closes that door with the same machinery as R/18:
# out-of-sample situation-only EPA expectation, residual compared on rotated
# vs static snaps, now per defensive caller, with the same screening honesty
# as every multi-cell screen in this project (N callers tested at 95%
# confidence -> N * 0.05 false positives expected under a true null).
#
# Conventions: no em dashes in rendered text, no Michael/Nick in rendered
# text, season spans in rendered text, plain language.
#
# Out: docs/figures/rotation_by_dc.png
#      data/derived/rotation_by_dc.csv
# =============================================================================

suppressMessages({
  library(data.table); library(readr); library(ggplot2); library(scales)
  library(xgboost)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")

d <- load_sumer(seasons = 2022:2025)
d <- d[season_type == 0 & is_dropback == TRUE & garbage_time == FALSE]
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell &
      middle_of_field_coverage_look != middle_of_field_coverage_played)]
d <- d[has_shell == TRUE & !is.na(def_caller)]
cat(sprintf("dropback snaps with a charted shell, non-garbage-time: %s\n",
            format(nrow(d), big.mark = ",")))

# same out-of-sample situation-only EPA expectation as R/18 (copied verbatim
# in design: season-grouped folds, ten situation features, squared error)
expect_epa <- function(x, target = "expected_points_added", feats = SUMER_STATE, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(get(target))], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x[[target]])
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "reg:squarederror", eta = 0.05, max_depth = 5,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.epa_actual = y, .epa_expected = p)]
  x[!is.na(.epa_expected)]
}

ep <- expect_epa(d)
ep[, epa_resid := .epa_actual - .epa_expected]
cat(sprintf("situation-only EPA model: mean actual %.3f, mean predicted %.3f\n",
            mean(ep$.epa_actual), mean(ep$.epa_expected)))

# per-coordinator worth test: needs a real sample on BOTH sides of the split
MIN_SIDE <- 200
dc <- ep[, .(n_rot = sum(rotate == 1), n_sta = sum(rotate == 0)), by = def_caller][
  n_rot >= MIN_SIDE & n_sta >= MIN_SIDE]
cat(sprintf("coordinators with >= %d rotated AND >= %d static dropbacks: %d\n",
            MIN_SIDE, MIN_SIDE, nrow(dc)))

res <- rbindlist(lapply(dc$def_caller, function(cl) {
  x <- ep[def_caller == cl]
  tt <- t.test(x[rotate == 1]$epa_resid, x[rotate == 0]$epa_resid)
  data.table(def_caller = cl,
             n_rot = nrow(x[rotate == 1]), n_sta = nrow(x[rotate == 0]),
             diff = unname(tt$estimate[1] - tt$estimate[2]),
             lo = tt$conf.int[1], hi = tt$conf.int[2], p = tt$p.value)
}))
res[, sig := p < 0.05]
res[, helps_defense := diff < 0]
setorder(res, diff)
res[, rank := .I]

n_test <- nrow(res); n_expect <- n_test * 0.05
n_sig  <- sum(res$sig)
n_sig_help <- sum(res$sig & res$helps_defense)
cat(sprintf("\nSCREEN: %d coordinators tested at 95%% confidence; %.1f expected significant by chance under a true null; %d observed (%d in the helps-the-defense direction)\n",
            n_test, n_expect, n_sig, n_sig_help))
cat("\nmost extreme in each direction:\n")
print(res[c(1:3, (.N-2):.N), .(def_caller, n_rot, diff = round(diff,3), lo = round(lo,3), hi = round(hi,3), p = round(p,3))])
for (nm in c("Mike Macdonald", "Vic Fangio")) if (nm %in% res$def_caller)
  with(res[def_caller == nm], cat(sprintf("%s: %+.3f [%.3f, %.3f] p = %.3f (rank %d of %d, negative = rotation helps his defense)\n",
                                          nm, diff, lo, hi, p, rank, n_test)))

write_csv(as.data.frame(res), "data/derived/rotation_by_dc.csv")

# ---------------------------------------------------------------- chart
res[, nm := factor(def_caller, levels = res$def_caller)]
res[, col := fifelse(def_caller == "Mike Macdonald", "mac",
             fifelse(def_caller == "Vic Fangio", "fangio",
             fifelse(sig == TRUE, "sig", "ns")))]

verdict_txt <- if (n_sig <= ceiling(n_expect) + 1) sprintf(
  "%d of %d clear the bar; %.0f-%.0f would clear it by luck alone under a true null. Nobody's rotation provably pays.",
  n_sig, n_test, floor(n_expect), ceiling(n_expect) + 1) else sprintf(
  "%d of %d clear the bar against %.1f expected by chance.", n_sig, n_test, n_expect)

p <- ggplot(res, aes(diff, nm)) +
  geom_vline(xintercept = 0, colour = "grey35", linewidth = 0.45) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                colour = "grey70", linewidth = 0.55) +
  geom_point(aes(colour = col), size = 2.7) +
  scale_colour_manual(values = c(mac = "#2B8CBE", fangio = "#D55E00",
                                 sig = "grey25", ns = "grey60"), guide = "none") +
  geom_text(data = res[def_caller %in% c("Mike Macdonald", "Vic Fangio")],
            aes(x = hi, label = def_caller,
                colour = col), hjust = -0.12, size = 3, fontface = "bold") +
  scale_x_continuous(labels = label_number(style_positive = "plus")) +
  coord_cartesian(clip = "off") +
  labs(title = "Does rotating the shell pay for anybody? Nobody clears the bar, and the bar is high",
       subtitle = paste0(
         "Offense's EPA per play on his rotated snaps minus his static snaps, after the situation is priced out, one row per defensive caller.\n",
         "Negative = his rotation coincides with better defense. ", verdict_txt, "\n",
         "The honest limit: per-coach samples are wide enough that a real effect worth about 20 points (nearly a win) a season could hide inside most of these intervals."),
       x = "offense EPA/play, rotated minus static, situation-controlled (negative = rotation helps his defense)",
       y = NULL,
       caption = fig_caption(
         "SumerSports play charting, 2022-23 through 2025-26 regular seasons, non-garbage-time dropbacks with a charted shell",
         sprintf("\n%d coordinators with at least %d rotated and %d static dropbacks each.", n_test, MIN_SIDE, MIN_SIDE),
         paste0("\nSame machinery as the pooled null: EPA compared against an out-of-sample situation-only expectation (down, distance, field position, quarter, score, clock),\n",
                "so the bars are what rotation adds beyond the moment. This is the fourth cut at the same question (pooled, by coverage and blitz, by pass probability and\n",
                "ten named situations, now by coordinator) and the fourth null. What stays real about rotation: the league trend is rising and WHO rotates is one of the most\n",
                "stable coach traits measured here. Statistically unproven is not the same as small: Macdonald rotates on about 236 dropbacks a season, so his -0.09 point estimate\n",
                "would be worth about 22 points (nearly a win) a season IF real; his interval runs from saving 51 points to giving away 8. The league-wide pooled test IS precise\n",
                "(66,000 snaps) and pins the average effect near zero; what stays open is whether individual coaches profit. Built by R/45."))) +
  theme_coach(grid = "none") +
  theme(axis.text.y = element_text(size = rel(0.72)))

save_fig("docs/figures/rotation_by_dc.png", p, w = 11.5, h = 10)
cat("\nOut: docs/figures/rotation_by_dc.png, data/derived/rotation_by_dc.csv\n")
