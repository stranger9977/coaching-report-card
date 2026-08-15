# =============================================================================
# factory/10_run.R -- fit every target, grade it, save everything the
# dashboard needs.
#
# Six targets, each with its own row universe:
#   y_pass   run or pass          all called plays, 2015-2025
#   y_blitz  did the defense send it   dropbacks only, 2022-2025 (FTN charting)
#   y_man    man or zone              charted coverage, 2022-2025
#   y_motion pre-snap motion          charted plays, 2022-2025
#   y_pa     play action              dropbacks only, 2022-2025
#   y_rpo    run-pass option          charted plays, 2022-2025
#
# Each is graded against the five rules in lib_factory.R. Nothing is presented
# as a coaching finding unless it clears them.
#
# Out: data/factory/fits/<target>.rds  (metrics, slices, residuals, grade)
#      data/factory/scorecard.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr) })
source("R/factory/lib_factory.R")

mt   <- as.data.table(readRDS("data/factory/model_table.rds"))
spec <- readRDS("data/factory/feature_spec.rds")
STATE <- spec$state
dir.create("data/factory/fits", showWarnings = FALSE, recursive = TRUE)

TARGETS <- list(
  list(y = "y_pass",   who = "off_play_caller", rows = quote(rep(TRUE, .N)),
       label = "Run or pass"),
  list(y = "y_blitz",  who = "def_caller_opp",  rows = quote(qb_dropback == 1),
       label = "Blitz"),
  list(y = "y_man",    who = "def_caller_opp",  rows = quote(qb_dropback == 1),
       label = "Man coverage"),
  list(y = "y_motion", who = "off_play_caller", rows = quote(rep(TRUE, .N)),
       label = "Pre-snap motion"),
  list(y = "y_pa",     who = "off_play_caller", rows = quote(qb_dropback == 1),
       label = "Play action"),
  list(y = "y_rpo",    who = "off_play_caller", rows = quote(rep(TRUE, .N)),
       label = "RPO")
)

sc <- list()
for (t in TARGETS) {
  cat("\n================ ", t$label, " ================\n")
  d <- mt[eval(t$rows)]
  fit <- tryCatch(fit_target(d, t$y, STATE, label = t$label),
                  error = function(e) { cat("  skipped:", conditionMessage(e), "\n"); NULL })
  if (is.null(fit)) next
  sl  <- slice_table(fit)
  res <- coach_residuals(fit, who = t$who)
  gr  <- grade(fit, sl, res)
  print(gr[, .(rule, value, status)])

  saveRDS(list(label = t$label, target = t$y, who = t$who,
               metrics = fit$metrics, importance = fit$importance,
               slices = sl, deciles = decile_table(fit$y, fit$pred),
               residuals = res, grade = gr, n = fit$n,
               seasons = range(fit$data$season)),
          sprintf("data/factory/fits/%s.rds", t$y))

  sc[[t$y]] <- data.table(
    target = t$y, label = t$label, n = fit$n, rate = fit$metrics$rate,
    auc = fit$metrics$auc, auc_base = fit$metrics$auc_base,
    ece = fit$metrics$ece, brier = fit$metrics$brier,
    persist = res$persist, n_rules_passed = sum(gr$pass),
    cleared = all(gr$pass))
}

scorecard <- rbindlist(sc)
write_csv(as.data.frame(scorecard), "data/factory/scorecard.csv")
cat("\n\n================ SCORECARD ================\n")
print(scorecard[, .(label, n, rate = round(rate,3), auc = round(auc,3),
                    lift = round(auc - auc_base,3), ece = round(ece,4),
                    persist = round(persist,2), passed = n_rules_passed, cleared)])
