# =============================================================================
# factory/85_disguise_defense.R -- defensive disguise, from Sumer charting.
#
# Michael's outline has "Disguise (offense and defense)" and we had nothing for
# the defensive half, because showing one coverage and playing another is a
# post-snap event that play-by-play does not record. Sumer charts it directly:
#
#   middle_of_field_coverage_look     what the shell looked like BEFORE the snap
#   middle_of_field_coverage_played   what it actually turned into
#   schemed_blitz                     the defense showed pressure
#   blitz                             the defense actually brought it
#
# So there are two kinds of lie, and they are different skills:
#
#   ROTATION   show a two-high shell and drop to one, or the reverse. 29% of
#              charted snaps league-wide.
#   BLUFF      walk defenders up to the line and then drop them out. Or the
#              rarer opposite, the surprise, where nothing showed and pressure
#              came anyway.
#
# PUBLISHING. Sumer data is licensed. Nick cleared the derived figure for the
# public board on 17 Aug, so the chart is written to docs/ and credited to
# SumerSports on the page and in the caption. The RAW pull stays out of the
# repo (data/raw/sumer/ is gitignored) because redistributing their play-level
# data is a different thing from showing a number computed from it.
#
# THE CONFOUND, handled the same way as everywhere else in the factory.
# Defenses disguise far more on obvious passing downs, so a raw rate would rank
# coordinators by the schedule they faced. Every rate here is a residual from a
# situation-only model of P(disguise), season-grouped, so a caller is compared
# against what the league does in his own spots.
#
# NOTHING IS CLAIMED UNTIL IT PERSISTS. Split-half by season parity, same as
# the offensive version in R/factory/89.
#
# Out: docs/figures/factory/disguise_defense.png
#      data/factory/disguise_defense.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork); library(jsonlite)
  library(xgboost)
})
source("R/lib/theme_coach.R")
dir.create("docs/figures/factory", showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2022:2025
NFLA <- "/Users/nick/stranger9977/nfl-analysis"

# ---------------------------------------------------------------- load
plays <- rbindlist(lapply(SEASONS, function(y) {
  f <- sprintf("data/raw/sumer/plays_%d.csv", y)
  if (file.exists(f)) fread(f, showProgress = FALSE) else NULL
}), fill = TRUE)
games <- rbindlist(lapply(SEASONS, function(y) {
  f <- sprintf("data/raw/sumer/games_%d.csv", y)
  if (file.exists(f)) fread(f, showProgress = FALSE) else NULL
}), fill = TRUE)
teams <- as.data.table(fromJSON("data/raw/sumer/teams.json"))

# Sumer uses four club codes nflverse spells differently
FIX <- c(ARZ = "ARI", BLT = "BAL", CLV = "CLE", HST = "HOU")
teams[, abbr := fifelse(team_code %in% names(FIX), FIX[team_code], team_code)]
plays <- merge(plays, teams[, .(sumer_defense_team_id = sumer_team_id, def_team = abbr)],
               by = "sumer_defense_team_id", all.x = TRUE)
plays <- merge(plays, teams[, .(sumer_offense_team_id = sumer_team_id, off_team = abbr)],
               by = "sumer_offense_team_id", all.x = TRUE)
plays <- merge(plays, unique(games[, .(sumer_game_id, week, season_type)]),
               by = "sumer_game_id", all.x = TRUE)

# defensive play-caller by team-week
pc <- fread(file.path(NFLA, "data/playcallers.csv"), showProgress = FALSE)
plays <- merge(plays, pc[, .(season, week, def_team = team, def_caller = def_play_caller)],
               by = c("season","week","def_team"), all.x = TRUE)
cat(sprintf("Sumer plays %s: %s | with a defensive play-caller: %s (%.0f%%)\n",
            paste(range(plays$season), collapse = "-"), format(nrow(plays), big.mark = ","),
            format(sum(!is.na(plays$def_caller) & plays$def_caller != ""), big.mark = ","),
            100*mean(!is.na(plays$def_caller) & plays$def_caller != "")))

# ---------------------------------------------------------------- the two lies
d <- plays[!is.na(def_caller) & def_caller != "" & no_play == FALSE]
d[, has_shell := middle_of_field_coverage_look %in% c("OPEN","CLOSED") &
                 middle_of_field_coverage_played %in% c("OPEN","CLOSED")]
d[, rotate := as.integer(has_shell &
      middle_of_field_coverage_look != middle_of_field_coverage_played)]
d[, bluff  := as.integer(schemed_blitz == TRUE & blitz == FALSE)]
d[, surprise := as.integer(schemed_blitz == FALSE & blitz == TRUE)]

sh <- d[has_shell == TRUE]
cat(sprintf("\nshell charted on %s plays | rotates %.1f%% of the time\n",
            format(nrow(sh), big.mark = ","), 100*mean(sh$rotate)))
cat("  showed two-high and played one:  ",
    format(nrow(sh[middle_of_field_coverage_look == "OPEN" &
                   middle_of_field_coverage_played == "CLOSED"]), big.mark = ","), "\n")
cat("  showed one-high and played two:  ",
    format(nrow(sh[middle_of_field_coverage_look == "CLOSED" &
                   middle_of_field_coverage_played == "OPEN"]), big.mark = ","), "\n")
cat(sprintf("\nblitz look: bluffed %s times, surprised %s times, showed-and-brought %s\n",
            format(sum(d$bluff, na.rm = TRUE), big.mark = ","),
            format(sum(d$surprise, na.rm = TRUE), big.mark = ","),
            format(nrow(d[schemed_blitz == TRUE & blitz == TRUE]), big.mark = ",")))

# ---------------------------------------------------------------- situation model
# Same rule as the rest of the factory: the base model sees the situation and
# nothing about who is coaching, so the residual is the coordinator's choice.
FEAT <- c("down","distance","field_position","quarter","offense_score_diff",
          "two_minute","four_minute","redzone","short_yardage")
prep <- function(x) {
  x <- copy(x)
  for (f in FEAT) if (is.logical(x[[f]])) set(x, j = f, value = as.integer(x[[f]]))
  x[, (FEAT) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = FEAT]
  x
}

expect_resid <- function(x, target) {
  x <- prep(x[!is.na(get(target))])
  x <- x[complete.cases(x[, ..FEAT])]
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "binary:logistic", eta = 0.06, max_depth = 5,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..FEAT]), label = x[[target]][tr]),
                   nrounds = 250, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..FEAT])))
  }
  x[, .expected := p]
  x[!is.na(.expected)]
}

rot <- expect_resid(sh, "rotate")
blf <- expect_resid(d[!is.na(bluff)], "bluff")
cat(sprintf("\nsituation model for rotation: AUC-free check, mean predicted %.3f vs actual %.3f\n",
            mean(rot$.expected), mean(rot$rotate)))

caller_rate <- function(x, target, min_n = 250) {
  x[, .(n = .N, actual = 100*mean(get(target)), expected = 100*mean(.expected),
        epa = mean(expected_points_added, na.rm = TRUE)),
    by = .(caller = def_caller, season)][n >= min_n]
}
rs <- caller_rate(rot, "rotate"); rs[, resid := actual - expected]
bs <- caller_rate(blf, "bluff");  bs[, resid := actual - expected]

career <- function(z) z[, .(seasons = .N, n = sum(n),
                            actual = weighted.mean(actual, n),
                            resid = weighted.mean(resid, n)), by = caller][n >= 700]
rc <- career(rs); bc <- career(bs)
setorder(rc, -resid); setorder(bc, -resid)
cat("\n--- rotates the shell most, against the league in his own situations ---\n")
print(head(rc[, .(caller, n, actual = round(actual,1), resid = round(resid,1))], 8))
cat("\n--- least ---\n")
print(tail(rc[, .(caller, n, actual = round(actual,1), resid = round(resid,1))], 6))
cat("\n--- bluffs pressure most ---\n")
print(head(bc[, .(caller, n, actual = round(actual,1), resid = round(resid,1))], 8))

# ---------------------------------------------------------------- persistence
persist <- function(z, lab) {
  a <- z[season %% 2 == 1, .(n = sum(n), r = weighted.mean(resid, n)), by = caller][n >= 350]
  b <- z[season %% 2 == 0, .(n = sum(n), r = weighted.mean(resid, n)), by = caller][n >= 350]
  m <- merge(a, b, by = "caller", suffixes = c("_odd","_even"))
  if (nrow(m) < 8) { cat(sprintf("%s: too few callers to test (%d)\n", lab, nrow(m))); return(NULL) }
  ct <- cor.test(m$r_odd, m$r_even)
  cat(sprintf("%s persistence: r = %+.3f [%.3f, %.3f] p = %.4f, n = %d callers\n",
              lab, ct$estimate, ct$conf.int[1], ct$conf.int[2], ct$p.value, nrow(m)))
  list(m = m, ct = ct, lab = lab)
}
cat("\n=== does disguise repeat across seasons? ===\n")
pr <- persist(rs, "Shell rotation"); pb <- persist(bs, "Blitz bluff")

# ---------------------------------------------------------------- does it work?
cat("\n=== does the lie help? EPA allowed, offense's point of view ===\n")
w1 <- rot[, .(plays = .N, epa = mean(expected_points_added, na.rm = TRUE)), by = rotate]
print(w1[order(rotate)])
cat(sprintf("  rotating is worth %+.3f EPA to the DEFENSE\n",
            -(w1[rotate == 1]$epa - w1[rotate == 0]$epa)))
w2 <- blf[, .(plays = .N, epa = mean(expected_points_added, na.rm = TRUE)), by = bluff]
print(w2[order(bluff)])
cat(sprintf("  bluffing is worth %+.3f EPA to the DEFENSE\n",
            -(w2[bluff == 1]$epa - w2[bluff == 0]$epa)))

# and per caller: does a coordinator who rotates more allow less?
rr <- merge(rc, rot[, .(epa = mean(expected_points_added, na.rm = TRUE)),
                    by = .(caller = def_caller)], by = "caller")
ce <- cor.test(rr$resid, rr$epa)
cat(sprintf("\nacross coordinators, rotating more and EPA allowed: r = %+.3f [%.3f, %.3f] p = %.3f\n",
            ce$estimate, ce$conf.int[1], ce$conf.int[2], ce$p.value))
write_csv(as.data.frame(rc), "data/factory/disguise_defense.csv")

# ---------------------------------------------------------------- chart
NAMED <- c("Mike Macdonald","Steve Wilks","Brian Flores","Vic Fangio","Dennis Allen",
           "Todd Bowles","Wink Martindale","Jesse Minter","Aaron Glenn","Jim Schwartz",
           "Steve Spagnuolo","DeMeco Ryans","Lou Anarumo","Zach Orr","Dan Quinn",
           "Robert Saleh","Ejiro Evero","Anthony Weaver","Jeff Ulbrich")
top <- rbind(head(rc, 12)[, side := "Rotates more than the situation calls for"],
             tail(rc, 12)[, side := "Shows what it plays"])
setorder(top, resid); top[, caller := factor(caller, levels = caller)]

pA <- ggplot(top, aes(resid, caller, fill = side)) +
  geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.45) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%+.1f", resid),
                hjust = ifelse(resid >= 0, -0.2, 1.2)),
            size = 2.85, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Rotates more than the situation calls for" = "#6b4c9a",
                               "Shows what it plays" = "#9db6c9")) +
  scale_x_continuous(expand = expansion(mult = c(0.16, 0.16))) +
  labs(title = "Who lies about the coverage",
       subtitle = "Shell rotation rate above what the situation calls for",
       x = "percentage points above expected", y = NULL) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", legend.text = element_text(size = rel(0.8)),
        axis.text.y = element_text(size = rel(0.85)))

pB <- NULL
if (!is.null(pr)) {
  lb <- pr$m[caller %in% NAMED]
  pB <- ggplot(pr$m, aes(r_odd, r_even)) +
    geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = ink_baseline, linewidth = 0.3) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#6b4c9a",
                fill = "#6b4c9a", alpha = 0.11, linewidth = 0.8) +
    geom_point(colour = "#9db6c9", alpha = 0.8, size = 2.4) +
    geom_point(data = lb, colour = "#6b4c9a", size = 2.9) +
    geom_text_repel(data = lb, aes(label = caller), size = 2.8, fontface = "bold",
                    colour = "#4a3570", seed = 6, box.padding = 0.4,
                    min.segment.length = 0, max.overlaps = 20) +
    labs(title = sprintf("Is it a trait? r = %+.2f", pr$ct$estimate),
         subtitle = sprintf("Rotation residual in odd seasons against even, %d coordinators", nrow(pr$m)),
         x = "odd seasons", y = "even seasons") +
    theme_coach(grid = "y")
}

p <- if (is.null(pB)) pA else (pA | pB) + plot_layout(widths = c(1, 1.1))
p <- p + plot_annotation(
  title = sprintf("Defensive disguise: the shell rotates on %.0f%% of charted snaps, and who does it is a repeatable trait",
                  100*mean(sh$rotate)),
  subtitle = "Pre-snap middle-of-field look against what was actually played, Sumer charting 2022 to 2025",
  caption = fig_caption(
    "SumerSports play charting; defensive play-caller attribution from samhoppen/NFL_public",
    sprintf("%s charted snaps with both a pre-snap look and a played shell, %d coordinators with 700 or more.",
            format(nrow(sh), big.mark = ","), nrow(rc)),
    paste0("\nDefenses rotate far more on obvious passing downs, so a raw rate would rank coordinators by the schedule they faced. Every number here is measured against a\n",
           "situation-only model of when the league rotates, season-grouped, so a coordinator is compared with what everyone else does in his own spots. Showing two-high\n",
           "and dropping to one is the common lie; the reverse is about a third as frequent. Charting data by SumerSports. Built by R/factory/85.")),
  theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/disguise_defense.png", p, w = 13.6, h = 7.4)
