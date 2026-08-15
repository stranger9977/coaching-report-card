# =============================================================================
# factory/40_controls.R -- the ctrl_ columns.
#
# Nick's spec: "one of those is qb quality we can use career av, some salary
# proxy for talent, madden ratings. test each one individually or make an
# index. age is important in there somewhere too. control for the proper
# things okay."
#
# These NEVER enter the base models. The base model answers "what would the
# average coach do here", and if a quarterback's ability is in it, the residual
# stops being the coach's choice. Controls exist for stage two: once we have a
# residual, we ask whether it survives knowing who was taking the snaps.
#
# THE CIRCULARITY TRAP, and how each proxy avoids it. If we control for a
# quarterback's CURRENT season EPA and then ask whether coaching residuals
# predict EPA, we have partly regressed the outcome on itself. So every
# performance-based control is LAGGED to the prior season. The others (salary,
# draft slot, age, team payroll) are known before the season starts and are
# safe as-is.
#
#   ctrl_qb_prior_epa     QB's EPA per dropback last season      lagged
#   ctrl_qb_prior_cpoe    QB's completion pct over expected      lagged
#   ctrl_qb_apy_pct       QB's contract APY as share of the cap  pre-season
#   ctrl_qb_draft         draft slot, 262 for undrafted          fixed
#   ctrl_qb_age           age on 1 September that season         pre-season
#   ctrl_team_contract    team talent from cap allocation        pre-season
#   ctrl_team_madden      team talent from Madden launch ratings pre-season
#   ctrl_index            standardised average of the above
#
# Out: data/factory/controls.rds  (one row per season x QB and season x team)
# =============================================================================

suppressMessages({
  library(data.table); library(nflreadr); library(readr)
})

SEASONS <- 2015:2025
OUT <- "data/factory"

# ---------------------------------------------------------------- QB season performance
mt <- as.data.table(readRDS(file.path(OUT, "model_table.rds")))
pbp_qb <- mt[qb_dropback == 1 & !is.na(passer_player_id) & passer_player_id != "",
             .(dropbacks = .N, epa = mean(epa, na.rm = TRUE)),
             by = .(season, qb_id = passer_player_id, qb = passer_player_name)]

cpoe <- rbindlist(lapply(SEASONS, function(s) {
  f <- sprintf("/Users/nick/stranger9977/nfl-analysis/data/play_by_play_%d.csv.gz", s)
  if (!file.exists(f)) return(NULL)
  d <- fread(f, select = c("season","passer_player_id","cpoe","qb_dropback"),
             showProgress = FALSE)
  d[qb_dropback == 1 & !is.na(cpoe) & !is.na(passer_player_id),
    .(cpoe = mean(cpoe)), by = .(season, qb_id = passer_player_id)]
}))
qb <- merge(pbp_qb, cpoe, by = c("season","qb_id"), all.x = TRUE)

# Lag to the prior season. This is the whole point: a control that already
# happened cannot be contaminated by the outcome we are about to test.
setorder(qb, qb_id, season)
qb[, `:=`(ctrl_qb_prior_epa = shift(epa), ctrl_qb_prior_cpoe = shift(cpoe),
          prior_season = shift(season), prior_db = shift(dropbacks)), by = qb_id]
qb[prior_season != season - 1 | prior_db < 100,
   `:=`(ctrl_qb_prior_epa = NA_real_, ctrl_qb_prior_cpoe = NA_real_)]

# ---------------------------------------------------------------- salary, draft, age
ct <- tryCatch(as.data.table(load_contracts()), error = function(e) NULL)
if (!is.null(ct) && "gsis_id" %in% names(ct)) {
  ctq <- ct[position == "QB" & !is.na(gsis_id) & gsis_id != "" & !is.na(apy_cap_pct),
            .(gsis_id, year_signed, years, apy_cap_pct)]
  # expand each deal across the seasons it covers, keep the richest active deal
  ctq <- ctq[!is.na(years) & years > 0]
  # by= groups can hold several deals signed the same year, so seq() has to be
  # driven per row rather than per group.
  ctq[, row_id := .I]
  exp_ct <- ctq[, .(season = seq.int(year_signed, year_signed + max(1L, as.integer(years)) - 1L),
                    apy_cap_pct = apy_cap_pct), by = row_id]
  exp_ct <- merge(exp_ct, ctq[, .(row_id, gsis_id)], by = "row_id")
  exp_ct <- exp_ct[season %in% SEASONS, .(ctrl_qb_apy_pct = max(apy_cap_pct)),
                   by = .(qb_id = gsis_id, season)]
  qb <- merge(qb, exp_ct, by = c("qb_id","season"), all.x = TRUE)
} else qb[, ctrl_qb_apy_pct := NA_real_]

ros <- rbindlist(lapply(SEASONS, function(s) {
  r <- tryCatch(as.data.table(load_rosters(seasons = s)), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  cols <- intersect(c("season","gsis_id","birth_date","draft_number","years_exp","position"), names(r))
  r[position == "QB", ..cols]
}), fill = TRUE)
if (nrow(ros)) {
  ros[, ctrl_qb_age := as.numeric(difftime(as.Date(paste0(season, "-09-01")),
                                           as.Date(birth_date), units = "days")) / 365.25]
  ros[, ctrl_qb_draft := fifelse(is.na(draft_number), 262, as.numeric(draft_number))]
  qb <- merge(qb, unique(ros[, .(qb_id = gsis_id, season, ctrl_qb_age, ctrl_qb_draft,
                                 ctrl_qb_exp = years_exp)]),
              by = c("qb_id","season"), all.x = TRUE)
}

# ---------------------------------------------------------------- team talent
tm <- data.table(season = integer(), team = character())
ctf <- "data/derived/contract_talent.csv"
if (file.exists(ctf)) {
  c1 <- as.data.table(read_csv(ctf, show_col_types = FALSE))
  tm <- c1[, .(season, team, ctrl_team_contract = contract_talent)]
}
mdf <- "data/derived/madden_team_seasons.csv"
if (file.exists(mdf)) {
  m1 <- as.data.table(read_csv(mdf, show_col_types = FALSE))
  m1 <- m1[, .(season, team, ctrl_team_madden = talent_z)]
  tm <- if (nrow(tm)) merge(tm, m1, by = c("season","team"), all = TRUE) else m1
}

# ---------------------------------------------------------------- index
z <- function(x) { s <- sd(x, na.rm = TRUE); if (is.na(s) || s == 0) return(rep(NA_real_, length(x))); (x - mean(x, na.rm = TRUE))/s }
qb[, `:=`(z_epa = z(ctrl_qb_prior_epa), z_cpoe = z(ctrl_qb_prior_cpoe),
          z_apy = z(ctrl_qb_apy_pct), z_draft = -z(log(ctrl_qb_draft)))]
qb[, ctrl_index := rowMeans(cbind(z_epa, z_cpoe, z_apy, z_draft), na.rm = TRUE)]
qb[is.nan(ctrl_index), ctrl_index := NA_real_]

QCOLS <- grep("^ctrl_", names(qb), value = TRUE)
cat("QB-season rows:", nrow(qb), "\ncoverage of each control:\n")
for (c in QCOLS) cat(sprintf("  %-22s %5.1f%%\n", c, 100*mean(!is.na(qb[[c]]))))

# how much do the proxies agree? if they were identical we would only need one
ind <- qb[dropbacks >= 100]
cm <- cor(ind[, .(ctrl_qb_prior_epa, ctrl_qb_prior_cpoe, ctrl_qb_apy_pct,
                  ctrl_qb_draft, ctrl_qb_age, ctrl_index)], use = "pairwise.complete.obs")
cat("\ncorrelation between the QB-quality proxies (n =", nrow(ind), "QB-seasons):\n")
print(round(cm, 2))

saveRDS(list(qb = qb, team = tm, qb_cols = QCOLS), file.path(OUT, "controls.rds"))
cat("\nwrote", file.path(OUT, "controls.rds"), "\n")
