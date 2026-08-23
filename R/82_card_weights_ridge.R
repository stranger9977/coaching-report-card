# =============================================================================
# 82_card_weights_ridge.R -- proper weights for the report card.
#
# The ask: "it also seems like we should do like a ridge regression model
# across all these to see what predicts win out of time", then "i dont care
# how good it is at predicting wins i just want proper weights".
#
# So the point here is the weights, not the R2. The unweighted GPA treats a
# coach's fourth-down decisions and his offense as equally important, which is
# an editorial choice pretending to be a measurement. This replaces it with
# weights the data picks.
#
# THE FIT. Unit: coach-season. Features: each report-card line as it stood
# BEFORE that season, that is the coach's career-to-date average through
# season t-1. Outcome: his wins per 17 games in season t. Nothing from season
# t is on the right-hand side, so a line earns its weight by predicting a
# season it has not seen. Ridge (alpha = 0) because the lines are correlated
# with each other; lambda chosen by cross-validation blocked on season, so a
# fold never trains on the year it predicts.
#
# TWO FITS, because two of the lines are results rather than decisions:
#   all lines   the honest predictive answer, where past results carry most
#               of the weight, as past results always do
#   decisions   fourth downs, two-point, penalties, offense, defense only,
#               which is what a decision-making report card should weigh
#
# The card uses the DECISION weights for the five decision lines and keeps the
# two results lines at the average of the decision weights, so results are not
# double-counted through the grade AND the ranking.
#
# Negative weights are reported, not hidden, and are floored at zero for the
# GPA: a line that predicts nothing gets no weight rather than an inverted one.
#
# Out: data/derived/card_weights.csv, docs/figures/card_weights.png
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(glmnet); library(ggplot2); library(scales) })
source("R/lib/theme_coach.R")
set.seed(82)

ps <- fread("data/derived/card_lines_seasons.csv")
# does the head coach call his own plays? tested, not assumed
pcf <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
pcf[, `:=`(head_coach = trimws(head_coach), off_play_caller = trimws(off_play_caller), def_play_caller = trimws(def_play_caller))]
callf <- unique(pcf[, .(season, team, coach = head_coach,
                        calls = as.integer(head_coach == off_play_caller | head_coach == def_play_caller))])
callf <- callf[, .(calls_s = max(calls)), by = .(coach, season)]
ps <- merge(ps, callf, by = c("coach", "season"), all.x = TRUE)
ps[is.na(calls_s), calls_s := 0L]
setorder(ps, coach, season)
LINES <- c(fourth_s = "Fourth downs", two_s = "Going for two", penalties_s = "Penalties",
           offense_s = "Offense above talent", defense_s = "Defense above talent",
           wae17 = "Beats the spread", wat17 = "Results above resources",
           calls_s = "Calls his own plays")
# career to date, through the previous season, with an expanding mean
for (k in names(LINES)) {
  ps[, (paste0("prior_", k)) := {
    v <- get(k); out <- rep(NA_real_, .N)
    for (i in seq_len(.N)) if (i > 1) { w <- v[1:(i - 1)]; if (any(!is.na(w))) out[i] <- mean(w, na.rm = TRUE) }
    out
  }, by = coach]
}
ps[, prior_seasons := seq_len(.N) - 1L, by = coach]
FEAT <- paste0("prior_", names(LINES))
d <- ps[prior_seasons >= 1 & !is.na(act17)]
cat(sprintf("panel: %d coach-seasons with at least one prior season, %d coaches, %d-%d\n",
            nrow(d), uniqueN(d$coach), min(d$season), max(d$season)))
cat("missing by feature:\n"); print(d[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = FEAT])
# fourth downs only exist from 2018, so the all-seven fit starts there
d7 <- d[complete.cases(d[, ..FEAT])]
cat(sprintf("complete rows for the seven-line fit: %d (%d-%d)\n", nrow(d7), min(d7$season), max(d7$season)))

fit_ridge <- function(dat, feats, label) {
  x <- as.matrix(dat[, ..feats]); y <- dat$act17
  sdx <- apply(x, 2, sd); mx <- colMeans(x)
  xs <- scale(x, center = mx, scale = sdx)
  folds <- as.integer(factor(dat$season))
  cv <- cv.glmnet(xs, y, alpha = 0, foldid = folds, standardize = FALSE)
  co <- as.matrix(coef(cv, s = "lambda.min"))[-1, 1]
  # out-of-time check, blocked by season, reported but not the point
  pred <- rep(NA_real_, nrow(dat))
  for (ss in sort(unique(dat$season))[-1]) {
    tr <- dat$season < ss; te <- dat$season == ss
    if (sum(tr) < 40) next
    m <- glmnet(xs[tr, , drop = FALSE], y[tr], alpha = 0, lambda = cv$lambda.min, standardize = FALSE)
    pred[te] <- as.numeric(predict(m, xs[te, , drop = FALSE]))
  }
  r <- cor(pred, y, use = "complete.obs")
  data.table(fit = label, line = names(co), coef = as.numeric(co), oos_r = r, n = nrow(dat))
}
w7 <- fit_ridge(d7, FEAT, "all lines")
DEC <- paste0("prior_", c("fourth_s", "two_s", "penalties_s", "offense_s", "defense_s", "calls_s"))
w5 <- fit_ridge(d7, DEC, "decision lines only")
cat(sprintf("\ncoaches who call their own plays: %.0f%% of coach-seasons; their mean wins per 17 %.2f vs %.2f for the rest\n",
            100*mean(d7$calls_s), d7[calls_s==1, mean(act17)], d7[calls_s==0, mean(act17)]))
W <- rbind(w7, w5)
W[, line_lab := LINES[sub("^prior_", "", line)]]
cat("\nridge coefficients, wins per 17 per standard deviation of each line:\n")
print(dcast(W, line_lab ~ fit, value.var = "coef")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat(sprintf("\nout-of-time r: all seven %.3f, decisions only %.3f (reported for the record, not the point)\n",
            w7$oos_r[1], w5$oos_r[1]))

# ------------------------------------------------- does the coordinator record carry over
# The question behind "as a caller should be important": for a coach who calls
# his own plays, the offense line IS his play-calling, so the two are the same
# measurement twice. The non-redundant version is the record he built BEFORE he
# was a head coach, which is the only calling evidence a first-time hire has.
ca <- fread("data/derived/caller_above_talent.csv")
coord <- ca[seasons_as_coord >= 2, .(coach = caller, coord_adj = caller_adj, side, seasons_as_coord)]
first3 <- ps[, .SD[order(season)][1:3], by = coach][!is.na(act17)]
hcw <- first3[, .(hc_wins = mean(act17), hc_wat = mean(wat17, na.rm = TRUE), n = .N), by = coach][n >= 2]
carry <- merge(hcw, coord, by = "coach")
cat(sprintf("\ncoordinator record vs head-coaching start: %d coaches with 2+ coordinator seasons and 2+ head-coaching seasons\n", nrow(carry)))
cat(sprintf("  correlation with wins per 17 in his first seasons: %.2f; with wins above talent: %.2f\n",
            cor(carry$coord_adj, carry$hc_wins), cor(carry$coord_adj, carry$hc_wat, use = "complete.obs")))
lmc <- summary(lm(hc_wins ~ coord_adj, carry))$coefficients
cat(sprintf("  a coordinator one standard deviation better is worth %+.2f wins per 17 as a head coach (t = %.1f, p = %.3f)\n",
            lmc["coord_adj", 1] * sd(carry$coord_adj), lmc["coord_adj", 3], lmc["coord_adj", 4]))
add_val <- data.table(test = "coordinator record carries to head coaching",
                      r = cor(carry$coord_adj, carry$hc_wins), n = nrow(carry),
                      wins_per_sd = lmc["coord_adj", 1] * sd(carry$coord_adj), p = lmc["coord_adj", 4])
write_csv(as.data.frame(add_val), "data/derived/caller_carryover.csv")

# ---------------------------------------------------------------- the card's weights
dec <- w5[, .(line = sub("^prior_", "", line), coef)]
# THE PLAY-CALLING LINE. The binary "does he call plays" earns +0.13 wins per
# standard deviation in the fit above. But the card's play-calling line is not
# binary: it grades how WELL he called, and the right coefficient for that is
# what a coordinator's record above talent is worth when he becomes a head
# coach, which is +0.30 wins per standard deviation on 47 coaches. That is the
# point estimate; its p-value is 0.37, so this is the one weight on the card
# resting on a number the data cannot pin down. It is used because grading how
# well a man called plays and then weighting it as if the question were only
# whether he called them understates the line.
dec[line == "calls_s", coef := lmc["coord_adj", 1] * sd(carry$coord_adj)]
dec[, w := pmax(coef, 0)]
if (sum(dec$w) == 0) stop("no decision line earned a positive weight")
dec[, w := w / sum(w)]
res_w <- mean(dec$w)                                   # results lines get the average decision weight
weights <- rbind(dec[, .(line, weight = w, source = "ridge, decision fit")],
                 data.table(line = c("wae17", "wat17"), weight = res_w, source = "held at the average"))
rg <- fread("data/derived/retread_gap.csv")
# the new-job line: a coach whose club moved on from him starts 0.86 wins above
# talent behind a first-time hire (p = 0.22, 34 against 66). Same treatment as
# the play-calling line: the point estimate is used and its noise is stated.
job_coef <- abs(rg$per_sd[1]) * sd(d7$act17)
cat(sprintf("\nnew-job line: retread gap %+.2f wins (p = %.2f) -> coefficient %+.3f\n", rg$estimate_wins[1], rg$p[1], job_coef))
weights[, line_key := c("fourth", "two_pt", "penalties", "offense", "defense", "calls", "spread", "results")[
  match(line, c("fourth_s", "two_s", "penalties_s", "offense_s", "defense_s", "calls_s", "wae17", "wat17"))]]
weights[, weight := weight / sum(weight)]
weights[, line_lab := LINES[match(line, names(LINES))]]
cat(sprintf("\nplay-calling line: binary coefficient %+.3f replaced with the coordinator carry-over estimate %+.3f (p = %.2f)\n",
            w5[line == "prior_calls_s"]$coef, lmc["coord_adj", 1] * sd(carry$coord_adj), lmc["coord_adj", 4]))
cat("\nweights the card will use:\n"); print(weights[order(-weight), .(line_lab, weight = round(weight, 3), source)])
write_csv(as.data.frame(merge(W, weights[, .(line = paste0("prior_", line), weight)], by = "line", all.x = TRUE)),
          "data/derived/card_weights.csv")
write_csv(as.data.frame(weights[, .(line_key, line_lab, weight, source)]), "data/derived/card_weights_final.csv")

# ---------------------------------------------------------------- figure
W[, fit := factor(fit, levels = c("all lines", "decision lines only"))]
p <- ggplot(W, aes(x = coef, y = reorder(line_lab, coef), colour = fit)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_point(size = 3.2, position = position_dodge(width = 0.5)) +
  geom_text(aes(label = sprintf("%+.2f", coef)), position = position_dodge(width = 0.5),
            hjust = -0.25, size = 2.9, show.legend = FALSE) +
  scale_colour_manual(values = c("all lines" = "grey55", "decision lines only" = "#2B8CBE"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.08, 0.25))) +
  labs(title = "What a ridge fit pays for: wins next season per standard deviation of each report-card line",
       subtitle = paste0("Every line measured as the coach's career average BEFORE the season being predicted, so nothing from the season in question is on the\n",
                         "right-hand side. Ridge because the lines move together; lambda by cross-validation blocked on season. Grey: all seven lines together,\n",
                         "where past results carry the weight, as past results always do. Blue: the decision lines on their own, which is what the card weighs."),
       x = "wins per 17 games per standard deviation", y = NULL,
       caption = fig_caption("nflverse play-by-play and schedules, nfl4th, OverTheCap and Madden via R/81, 2012-2025 (fourth downs 2018-2025)",
         sprintf("\nFit on %d coach-seasons with at least one prior season. Out-of-time correlation with next-season wins: %.2f for all seven, %.2f for decisions only.\nThe weights are the point here, not the fit. A line whose coefficient is not positive gets no weight on the card rather than an inverted one. Built by R/82.",
                 nrow(d7), w7$oos_r[1], w5$oos_r[1]))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/card_weights.png", p, w = 11, h = 6)
cat("\nOut: card_weights.png, data/derived/card_weights.csv, card_weights_final.csv\n")
