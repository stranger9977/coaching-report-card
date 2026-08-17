# =============================================================================
# factory/70_experiments.R -- the experiment loop.
#
# Nick's spec: "a rubric that experiments with different model structures and
# experiments with different features to accomplish the goal."
#
# Four of six targets failed the rubric, and every one of them failed R1,
# discrimination. R1 failing is a modeling problem by definition, so this is
# where it gets fixed rather than excused.
#
# THE HYPOTHESIS. The base feature set is situation only, which is right for
# offensive targets: a play-caller picks run or pass from down, distance,
# clock and score. But it is wrong for DEFENSIVE targets. A coordinator
# deciding whether to blitz, or whether to play man, is standing on the
# sideline looking at the offense's personnel grouping and formation. That
# information exists before his decision, so it is part of his game state, not
# leakage. The blitz and man models have been asked to guess blind.
#
# WHY THIS IS NOT CHEATING. The test for leakage is whether the feature is
# known to the decision-maker at decision time. Offensive personnel is
# announced by the substitution and the formation is set before the snap, so
# both are. What WOULD be leakage is the offense's play, the outcome, or
# anything derived from them, and none of that goes in.
#
# For offensive targets the same features are a different beast: a caller
# chooses his own personnel, so adding it explains his call with his other
# call. That is circular, so offensive targets are tested with the offense's
# own personnel too, but the result is read with that caveat attached.
#
# Every variant is graded by the same five rules. Nothing is promoted on AUC
# alone.
#
# Out: data/factory/experiments.csv
#      data/factory/fits/<target>.rds  (overwritten when a variant wins)
# =============================================================================

suppressMessages({ library(data.table); library(readr) })
source("R/factory/lib_factory.R")

mt   <- as.data.table(readRDS("data/factory/model_table.rds"))
spec <- readRDS("data/factory/feature_spec.rds")
STATE <- spec$state

# ---------------------------------------------------------------- extra features
# The offensive look, encoded as integers so xgboost can use them.
pers_bucket <- function(x) {
  rb <- suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", x))); rb[is.na(rb)] <- 1L
  te <- suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", x))); te[is.na(te)] <- 1L
  fifelse(rb == 1 & te == 1, 0L, fifelse(rb == 1 & te == 2, 1L,
  fifelse(rb == 2 & te == 1, 2L, fifelse(rb == 1 & te == 3, 3L,
  fifelse(rb == 0, 4L, 5L)))))
}
mt[, look_pers := pers_bucket(offense_personnel)]
mt[, look_form := as.integer(factor(offense_formation))]
mt[, look_rb := suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", offense_personnel)))]
mt[, look_te := suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", offense_personnel)))]
mt[, look_shotgun := as.integer(offense_formation == "SHOTGUN")]
mt[, look_empty := as.integer(offense_formation == "EMPTY")]

LOOK <- c("look_pers","look_form","look_rb","look_te","look_shotgun","look_empty")

# Sequence: what the same offense did on the previous snap of this drive. Known
# to both sides, and the natural test of Michael's "sequencing" section.
setorder(mt, game_id, posteam, drive, play_id)
mt[, prev_pass := shift(y_pass), by = .(game_id, posteam, drive)]
mt[, prev_epa  := shift(epa),    by = .(game_id, posteam, drive)]
mt[, prev_pass := fifelse(is.na(prev_pass), -1L, as.integer(prev_pass))]
mt[, prev_epa  := fifelse(is.na(prev_epa), 0, prev_epa)]
SEQ <- c("prev_pass","prev_epa")

EXPERIMENTS <- list(
  list(name = "base",              feats = STATE),
  list(name = "base + look",       feats = c(STATE, LOOK)),
  list(name = "base + sequence",   feats = c(STATE, SEQ)),
  list(name = "base + look + seq", feats = c(STATE, LOOK, SEQ)),
  list(name = "deeper trees",      feats = STATE, params = list(max_depth = 9, eta = 0.04)),
  list(name = "look + deeper",     feats = c(STATE, LOOK), params = list(max_depth = 9, eta = 0.04))
)

TARGETS <- list(
  list(y = "y_blitz",  who = "def_caller_opp",  rows = quote(qb_dropback == 1), label = "Blitz",           side = "defense"),
  list(y = "y_man",    who = "def_caller_opp",  rows = quote(qb_dropback == 1), label = "Man coverage",    side = "defense"),
  list(y = "y_motion", who = "off_play_caller", rows = quote(rep(TRUE, .N)),    label = "Pre-snap motion", side = "offense"),
  list(y = "y_rpo",    who = "off_play_caller", rows = quote(rep(TRUE, .N)),    label = "RPO",             side = "offense")
)

res <- list()
for (t in TARGETS) {
  cat("\n\n############ ", t$label, " (", t$side, ") ############\n", sep = "")
  d <- mt[eval(t$rows)]
  best <- NULL
  for (e in EXPERIMENTS) {
    # look features only exist where charting exists; drop rows without them
    feats <- e$feats
    dd <- if (any(LOOK %in% feats)) d[!is.na(look_pers) & !is.na(look_form)] else d
    fit <- tryCatch(fit_target(dd, t$y, feats, params = if (is.null(e$params)) list() else e$params,
                               label = t$label, perm_repeats = 1),
                    error = function(err) { cat("  ", e$name, "failed:", conditionMessage(err), "\n"); NULL })
    if (is.null(fit)) next
    sl <- slice_table(fit); rr <- coach_residuals(fit, who = t$who); gr <- grade(fit, sl, rr)
    npass <- sum(gr$pass)
    cat(sprintf("  %-18s AUC %.3f (lift %+.3f)  ECE %.4f  persist %+.2f  -> %d/5 %s\n",
                e$name, fit$metrics$auc, fit$metrics$auc - fit$metrics$auc_base,
                fit$metrics$ece, rr$persist, npass, if (all(gr$pass)) "CLEARED" else ""))
    res[[length(res) + 1]] <- data.table(
      target = t$y, label = t$label, side = t$side, variant = e$name,
      n = fit$n, auc = fit$metrics$auc, lift = fit$metrics$auc - fit$metrics$auc_base,
      ece = fit$metrics$ece, persist = rr$persist, passed = npass, cleared = all(gr$pass))
    if (is.null(best) || npass > best$npass ||
        (npass == best$npass && fit$metrics$auc > best$fit$metrics$auc)) {
      best <- list(fit = fit, sl = sl, rr = rr, gr = gr, npass = npass, name = e$name)
    }
  }
  if (!is.null(best) && best$name != "base") {
    cat(sprintf("  >> winner: %s (%d/5). saving over the base fit.\n", best$name, best$npass))
    saveRDS(list(label = t$label, target = t$y, who = t$who, variant = best$name,
                 metrics = best$fit$metrics, importance = best$fit$importance,
                 perm = best$fit$perm, slices = best$sl,
                 deciles = decile_table(best$fit$y, best$fit$pred),
                 residuals = best$rr, grade = best$gr, n = best$fit$n,
                 seasons = range(best$fit$data$season)),
            sprintf("data/factory/fits/%s.rds", t$y))
  }
}

ex <- rbindlist(res)
write_csv(as.data.frame(ex), "data/factory/experiments.csv")
cat("\n\n================ EXPERIMENT LOG ================\n")
print(ex[, .(label, variant, auc = round(auc,3), lift = round(lift,3),
             ece = round(ece,4), persist = round(persist,2), passed, cleared)])
