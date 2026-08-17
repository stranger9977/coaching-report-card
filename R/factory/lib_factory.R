# =============================================================================
# factory/lib_factory.R -- the rubric, the fitting engine, the diagnostics.
#
# THE RUBRIC (Nick asked me to define and write it, 15 Aug).
#
# The whole project rests on one claim: that a coach's residual means something
# about the coach. That claim is only safe if the model is right about the
# situation. So a model is CLEARED FOR RESIDUAL USE only if it passes all five:
#
#   R1 DISCRIMINATION.  Out-of-sample AUC >= 0.65, and at least 0.03 better
#       than a down-and-distance-only lookup table. If the model cannot beat a
#       lookup table it has learned nothing worth residualising.
#
#   R2 CALIBRATION.  Expected calibration error <= 0.020 and no probability
#       decile off by more than 0.05. This is the one that actually protects
#       the residuals: if the model is systematically low at p = 0.8, then
#       every coach in those spots gets a fake positive residual.
#
#   R3 SLICE SAFETY.  In every slice of every binned SITUATIONAL feature, the
#       gap between mean actual and mean predicted must sit inside its own 95%
#       interval, for at least 90% of slices holding >= 200 plays. This is
#       where a model hides its bodies: fine overall, badly wrong inside the 10.
#       Season is excluded from this rule on purpose. It is a time index, not a
#       football situation, and R4's season-grouped design guarantees the model
#       cannot know the league's pass rate that year. That drift is real, so it
#       is removed at the residual stage instead, by centering every coach's
#       residual within his own season. Without that centering a coach in a
#       run-heavy year looks pass-shy for reasons that have nothing to do with
#       him.
#
#   R4 HONEST HOLDOUT.  Metrics come from season-grouped cross-validation, so
#       no play is scored by a model that saw its own season. Coaching changes
#       year to year; letting a model see 2024 while scoring 2024 lets it learn
#       the coach, which is precisely the thing we want left in the residual.
#
#   R5 RESIDUAL PERSISTENCE.  A coach's residual in one season must correlate
#       with his residual the next at r >= 0.30. A residual that does not
#       persist is noise, and no story should be built on it. This is the
#       criterion that separates "interesting" from "spurious", and it is the
#       one most modeling write-ups skip.
#
# A model that fails R1-R4 is a modeling problem: fix the features. A model
# that passes R1-R4 but fails R5 is a finding in itself, and it means the
# behavior genuinely is not a stable coaching trait.
#
# AMENDED 15 Aug, after the experiment loop in factory/70. That first sentence
# is only true until you have actually tried. Six feature sets and two tree
# depths were run at the defensive targets, including the theoretically
# motivated one (giving the coordinator the offensive personnel and formation
# he can plainly see before he calls anything). Blitz moved from AUC 0.614 to
# 0.619 and man coverage got worse on the rubric, not better.
#
# So R1 failure has two readings, and they are distinguished by R5:
#   low AUC + low persistence  = a bad model. Keep working.
#   low AUC + HIGH persistence = a real finding. The behavior is a property of
#     the coach rather than a response to the situation, which is exactly why
#     the situation cannot predict it.
# Blitz is the second kind: AUC 0.62, barely above a lookup table, with the
# highest residual persistence of any target at 0.78. Coordinators blitz
# according to who they are, not what is in front of them.
# =============================================================================

suppressMessages({
  library(data.table); library(xgboost); library(ggplot2)
})

RUBRIC <- list(
  auc_min          = 0.65,
  auc_lift_min     = 0.03,
  ece_max          = 0.020,
  decile_gap_max   = 0.05,
  slice_pass_min   = 0.90,
  slice_min_n      = 200,
  persist_min      = 0.30
)

# ---------------------------------------------------------------- metrics
auc_fast <- function(y, p) {
  # as.numeric is load-bearing: n1*n0 overflows 32-bit integer past ~46k of
  # each class, which silently returned NA for the 381k-play run/pass model.
  r <- rank(p); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1]) - n1*(n1 + 1)/2) / (n1*n0)
}
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y*log(p) + (1-y)*log(1-p)) }
brier   <- function(y, p) mean((p - y)^2)

# Expected calibration error over equal-count bins.
ece <- function(y, p, bins = 20) {
  b <- cut(p, quantile(p, seq(0, 1, length.out = bins + 1), na.rm = TRUE),
           include.lowest = TRUE, labels = FALSE)
  d <- data.table(y, p, b)[, .(n = .N, a = mean(y), e = mean(p)), by = b]
  sum(d$n * abs(d$a - d$e)) / sum(d$n)
}

decile_table <- function(y, p) {
  b <- cut(p, quantile(p, seq(0, 1, 0.1), na.rm = TRUE), include.lowest = TRUE, labels = FALSE)
  data.table(y, p, b)[, .(n = .N, actual = mean(y), predicted = mean(p),
                          se = sqrt(mean(y)*(1-mean(y))/.N)), by = b][order(b)]
}

# ---------------------------------------------------------------- baseline
# Down x distance-bucket lookup. The thing a model has to beat to be worth it.
baseline_lookup <- function(train, test, target) {
  bk <- function(d) paste(d$down, cut(d$ydstogo, c(0,2,4,7,10,15,100), labels = FALSE))
  tr <- data.table(k = bk(train), y = train[[target]])[, .(p = mean(y), n = .N), by = k]
  gm <- mean(train[[target]])
  p <- tr$p[match(bk(test), tr$k)]
  fifelse(is.na(p), gm, p)
}

# ---------------------------------------------------------------- fit
# Season-grouped CV (R4). Every play is scored by a model that never saw its
# own season.
fit_target <- function(dt, target, features, params = list(), nrounds = 400,
                       min_rows = 5000, label = target, perm_repeats = 3) {
  d <- dt[!is.na(get(target))]
  d <- d[complete.cases(d[, ..features])]
  if (nrow(d) < min_rows) stop(sprintf("%s: only %d rows", label, nrow(d)))
  seasons <- sort(unique(d$season))
  p_oos <- rep(NA_real_, nrow(d)); p_base <- rep(NA_real_, nrow(d))
  fold_models <- list(); fold_idx <- list()

  pr <- modifyList(list(objective = "binary:logistic", eval_metric = "logloss",
                        eta = 0.05, max_depth = 6, subsample = 0.8,
                        colsample_bytree = 0.8, min_child_weight = 20,
                        nthread = 4), params)
  for (s in seasons) {
    i_te <- which(d$season == s); i_tr <- which(d$season != s)
    if (length(i_tr) < 1000) next
    dtr <- xgb.DMatrix(as.matrix(d[i_tr, ..features]), label = d[[target]][i_tr])
    m <- xgb.train(pr, dtr, nrounds = nrounds, verbose = 0)
    p_oos[i_te] <- predict(m, xgb.DMatrix(as.matrix(d[i_te, ..features])))
    p_base[i_te] <- baseline_lookup(d[i_tr], d[i_te], target)
    fold_models[[length(fold_models) + 1]] <- m
    fold_idx[[length(fold_idx) + 1]] <- i_te
  }
  ok <- !is.na(p_oos)
  d <- d[ok]; p_oos <- p_oos[ok]; p_base <- p_base[ok]
  y <- d[[target]]

  full <- xgb.train(pr, xgb.DMatrix(as.matrix(d[, ..features]), label = y),
                    nrounds = nrounds, verbose = 0)
  imp <- as.data.table(xgb.importance(model = full))

  # Held-out permutation importance, using each fold's own model. Re-index the
  # fold test sets onto the filtered rows first.
  keep_map <- cumsum(ok); fold_idx <- lapply(fold_idx, function(i) keep_map[i[ok[i]]])
  perm <- perm_importance(d, y, features, fold_models, fold_idx,
                          base_auc = auc_fast(y, p_oos), repeats = perm_repeats)

  list(label = label, target = target, features = features, n = nrow(d),
       data = d, pred = p_oos, base = p_base, y = y, importance = imp, perm = perm,
       metrics = list(
         auc = auc_fast(y, p_oos), auc_base = auc_fast(y, p_base),
         logloss = logloss(y, p_oos), logloss_base = logloss(y, p_base),
         brier = brier(y, p_oos), ece = ece(y, p_oos), rate = mean(y)))
}

# ---------------------------------------------------------------- permutation importance
# Gain importance is fit in-sample and is biased toward continuous, high
# cardinality features. This is the honest version: shuffle a feature in the
# HELD-OUT fold, re-score with that fold's model, and measure how much
# discrimination is lost. Nothing here ever sees its own season.
#
# THE COLLINEARITY PROBLEM, and why the grouped version is the one to read.
# The state features are deliberately redundant: qtr and game_seconds_remaining
# correlate at -0.96, is_inside10 and is_goal_to_go at 0.86, vegas_wp and
# score_differential at 0.86. Permuting ONE of a correlated pair costs the model
# almost nothing, because it just reads the twin instead. Taken alone, single
# feature permutation would say the clock does not matter, which is false.
# So concepts are permuted as blocks too, and the block numbers are the real
# answer to "what is this model using".
PERM_GROUPS <- list(
  "Down and distance" = c("down","ydstogo"),
  "Field position"    = c("yardline_100","is_redzone","is_inside10","is_goal_to_go","is_own_deep"),
  "Clock"             = c("game_seconds_remaining","half_seconds_remaining","qtr",
                          "is_2min","is_4min","is_half_end","is_second_half"),
  # vegas_wp and spread_posteam sit WITH the score block, not apart from it.
  # An earlier version split them out, which defeated the point of grouping:
  # vegas_wp correlates with score_differential at 0.86, so permuting either
  # alone cost almost nothing and both looked unimportant.
  "Score and game state" = c("score_differential","score_abs","is_one_score",
                             "is_trailing","is_blowout","vegas_wp","spread_posteam"),
  "Timeouts"          = c("posteam_timeouts_remaining","defteam_timeouts_remaining","to_diff")
)

# models: one xgboost per season fold; idx: row indices of each fold's test set.
perm_importance <- function(d, y, features, models, idx, base_auc, repeats = 3) {
  X <- as.matrix(d[, ..features])
  score_with <- function(cols) {
    aucs <- numeric(repeats)
    for (r in seq_len(repeats)) {
      p <- rep(NA_real_, nrow(X))
      for (k in seq_along(models)) {
        i <- idx[[k]]; if (!length(i)) next
        Xt <- X[i, , drop = FALSE]
        # shuffle within the fold, so the marginal distribution is preserved
        for (cl in cols) Xt[, cl] <- Xt[sample.int(nrow(Xt)), cl]
        p[i] <- predict(models[[k]], xgb.DMatrix(Xt))
      }
      ok <- !is.na(p)
      aucs[r] <- auc_fast(y[ok], p[ok])
    }
    c(mean = mean(aucs), sd = stats::sd(aucs))
  }
  single <- rbindlist(lapply(features, function(f) {
    v <- score_with(f)
    data.table(kind = "feature", name = f, auc_permuted = v[["mean"]],
               drop = base_auc - v[["mean"]], sd = v[["sd"]])
  }))
  grps <- PERM_GROUPS[vapply(PERM_GROUPS, function(g) any(g %in% features), TRUE)]
  grouped <- rbindlist(lapply(names(grps), function(g) {
    cols <- intersect(grps[[g]], features)
    v <- score_with(cols)
    data.table(kind = "group", name = g, auc_permuted = v[["mean"]],
               drop = base_auc - v[["mean"]], sd = v[["sd"]])
  }))
  rbind(single, grouped)[order(kind, -drop)]
}

# ---------------------------------------------------------------- slices
# Every feature binned; actual vs predicted per bin with a 95% interval.
BIN_SPEC <- list(
  down            = function(x) factor(x$down),
  ydstogo         = function(x) cut(x$ydstogo, c(0,1,2,3,5,7,10,15,99)),
  yardline_100    = function(x) cut(x$yardline_100, c(0,5,10,20,40,60,80,90,100)),
  score_diff      = function(x) cut(x$score_differential, c(-99,-17,-9,-4,-1,0,3,8,16,99)),
  half_clock      = function(x) cut(x$half_seconds_remaining, c(-1,60,120,300,600,900,1800)),
  quarter         = function(x) factor(x$qtr),
  posteam_to      = function(x) factor(x$posteam_timeouts_remaining),
  vegas_wp        = function(x) cut(x$vegas_wp, seq(0, 1, 0.125)),
  season          = function(x) factor(x$season),
  leverage        = function(x) cut(x$leverage, c(-.01,.25,.5,.75,.9,1))
)

slice_table <- function(fit) {
  d <- copy(fit$data); d[, `:=`(.y = fit$y, .p = fit$pred)]
  rbindlist(lapply(names(BIN_SPEC), function(nm) {
    b <- tryCatch(BIN_SPEC[[nm]](d), error = function(e) NULL)
    if (is.null(b)) return(NULL)
    t <- d[, .(n = .N, actual = mean(.y), predicted = mean(.p),
               se = sd(.y)/sqrt(.N)), by = .(bin = as.character(b))]
    t[, `:=`(feature = nm, gap = actual - predicted,
             lo = actual - 1.96*se, hi = actual + 1.96*se)]
    t[, inside := (predicted >= lo & predicted <= hi)]
    t[!is.na(bin)]
  }), fill = TRUE)
}

# ---------------------------------------------------------------- residuals
# One number per coach-season: how much more often he did the thing than the
# situation said he would.
coach_residuals <- function(fit, who = "off_play_caller", min_n = 150) {
  d <- copy(fit$data); d[, `:=`(.y = fit$y, .p = fit$pred)]
  d <- d[!is.na(get(who)) & get(who) != ""]
  # Carry the coach-season's modal team so persistence can be split by whether
  # he stayed put. Without this, R5 cannot tell a coach effect from a building.
  tcol <- if (who == "def_caller_opp") "defteam" else "posteam"
  d[, .team := get(tcol)]
  # Center within season: subtract the league's own residual that year, so
  # league-wide drift in how often anyone passes does not masquerade as a
  # coach's tendency. See R3.
  lg <- d[, .(lg_resid = 100*(mean(.y) - mean(.p))), by = season]
  cs <- d[, .(n = .N, actual = mean(.y), expected = mean(.p),
              raw_resid = 100*(mean(.y) - mean(.p)),
              team = names(sort(table(.team), decreasing = TRUE))[1]),
          by = c(who, "season")][n >= min_n]
  cs <- merge(cs, lg, by = "season")
  cs[, resid := raw_resid - lg_resid]
  setnames(cs, who, "coach")
  setorder(cs, coach, season)
  cs[, `:=`(prev = shift(resid), prev_season = shift(season),
            prev_team = shift(team)), by = coach]
  pr <- cs[!is.na(prev) & season - prev_season == 1]
  pr[, moved := team != prev_team]
  # THE TEST THAT MATTERS. Within-team persistence can be a property of the
  # building: the same roster, coordinators and personnel produce the same
  # tendencies whoever is nominally calling it. Persistence ACROSS a change of
  # club is the only version that isolates the man.
  same <- pr[moved == FALSE]; move <- pr[moved == TRUE]
  list(season = cs,
       career = cs[, .(n = sum(n), resid = weighted.mean(resid, n)), by = coach][n >= min_n*3],
       persist = if (nrow(pr) > 20) cor(pr$resid, pr$prev) else NA_real_,
       n_pairs = nrow(pr),
       persist_same = if (nrow(same) > 15) cor(same$resid, same$prev) else NA_real_,
       n_same = nrow(same),
       persist_moved = if (nrow(move) > 5) cor(move$resid, move$prev) else NA_real_,
       n_moved = nrow(move))
}

# ---------------------------------------------------------------- verdict
grade <- function(fit, sl, res) {
  m <- fit$metrics
  big <- sl[n >= RUBRIC$slice_min_n & feature != "season"]
  dec <- decile_table(fit$y, fit$pred)
  checks <- data.table(
    rule = c("R1 discrimination","R2 calibration","R3 slice safety",
             "R4 honest holdout","R5 residual persistence"),
    value = c(sprintf("AUC %.3f (lookup %.3f, lift %+.3f)", m$auc, m$auc_base, m$auc - m$auc_base),
              sprintf("ECE %.4f, worst decile gap %.3f", m$ece, max(abs(dec$actual - dec$predicted))),
              sprintf("%.0f%% of %d slices inside interval", 100*mean(big$inside), nrow(big)),
              "season-grouped CV",
              sprintf("year-over-year r = %.2f (n = %d); same team %.2f (n = %d), changed team %.2f (n = %d)",
                      res$persist, res$n_pairs,
                      res$persist_same, res$n_same, res$persist_moved, res$n_moved)),
    pass = c(m$auc >= RUBRIC$auc_min && (m$auc - m$auc_base) >= RUBRIC$auc_lift_min,
             m$ece <= RUBRIC$ece_max && max(abs(dec$actual - dec$predicted)) <= RUBRIC$decile_gap_max,
             mean(big$inside) >= RUBRIC$slice_pass_min,
             TRUE,
             !is.na(res$persist) && res$persist >= RUBRIC$persist_min))
  checks[, status := fifelse(pass, "pass", "FAIL")]
  checks[]
}
