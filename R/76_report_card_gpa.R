# =============================================================================
# 76_report_card_gpa.R -- the report card: letter grades, a GPA out of 4.0,
# tiers and class year, for the 32 head coaches on a sideline in 2026.
#
# Only active coaches are on the card. A fired coach's grades are still used to
# set the curve, because a grade should mean the same thing every year, but he
# is not on the list: Sean McDermott topped the first version of this card and
# Buffalo had already replaced him.
#
# Class year, from seasons as a head coach in the window:
#   Freshman 1-2, Sophomore 3-4, Upperclassman 5+.
#
# The ask: "a report card on their decision making maybe WAR is one component
# of that ... a full report card that actually looks like a report card at
# the top and ranks them into tiers by overall grade or gives them a gpa out
# of 4.0".
#
# Eight lines, each graded by percentile among the coaches on the card (A =
# top fifth, B, C, D, F = bottom fifth). The GPA is a WEIGHTED average of the
# letter points (A 4, B 3, C 2, D 1, F 0), with weights from R/82's ridge fit
# on next-season wins rather than a flat average. A coach needs at least four
# lines to get a GPA; missing lines read "?" and their weight drops out.
#
#   Fourth downs     R/81: win-probability cost of his fourth-down choices vs
#                    the league in his own seasons, 2018-2025. Lower is better.
#   Going for two    R/81: chart-following rate where he has 10+ chart spots,
#                    otherwise his overall rate against his era.
#   Penalties        R/81: own pre-snap and holding penalties per play vs the
#                    league that season. Fewer is better.
#   Offense          R/81: team offense EPA per play above the same talent
#   Defense          controls the WAR model uses. Higher is better.
#   Beats the spread R/71 coaching_war.csv, surprise_per_season: wins beyond
#                    the closing line, shrunk. Higher is better.
#   Results above    R/78's market-free WAR: point differential above payroll,
#   resources        roster ratings and the quarterback's pay and play. The
#                    market-anchored version is deliberately NOT used: 58% of
#                    its top coach's number was the betting line rating his
#                    team above its roster, which is partly its opinion of him.
#
# Tiers by GPA: Dean's list 3.3+, Honor roll 2.7 to 3.3, Passing 2.0 to 2.7,
# Probation under 2.0.
#
# Out: data/derived/report_card_gpa.csv, docs/figures/report_card_tiers.png
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel); library(nflreadr) })
source("R/lib/theme_coach.R")

w  <- fread("data/derived/coaching_war.csv")
nmk <- fread("data/derived/coaching_war_no_market.csv")[, .(coach, war_no_market)]
pcf <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
ACTIVE <- unique(trimws(pcf[season == 2026 & week == 1]$head_coach))
TEAM26 <- unique(pcf[season == 2026 & week == 1, .(coach = trimws(head_coach), team = team)])
sn <- fread("data/derived/coaching_war_sensitivity.csv")
# R/81 rebuilds these five lines per coach-season with a one-season minimum, so
# a first-year head coach is graded on the season he actually coached instead of
# failing the career minimums the old files carried
cl <- fread("data/derived/card_lines.csv")
dv <- cl[, .(coach, fourth)]
tp <- cl[, .(coach, two_pt)]
di <- cl[, .(coach, penalties)]
ta <- cl[, .(coach, offense, defense)]
rc <- w[coach %in% ACTIVE, .(coach, seasons, war = war_per_season, spread = surprise_per_season)]
rc <- merge(rc, nmk, by = "coach", all.x = TRUE)
rc <- merge(rc, sn[, .(coach, r_qb = rank_war_talent_same_season_qb, r_pd = rank_pd_effect_ppg)], by = "coach", all.x = TRUE)
rc[, results := war_no_market]   # the market-free board, available for every coach
for (x in list(dv, tp, di, ta)) rc <- merge(rc, x, by = "coach", all.x = TRUE)
# every 2026 head coach gets a row, including the ones who have never been one
rc <- merge(TEAM26, rc, by = "coach", all.x = TRUE)
rc[is.na(seasons), seasons := 0L]
cat(sprintf("the class of 2026: %d coaches, %d with a head-coaching record\n", nrow(rc), sum(rc$seasons > 0)))

pcall <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
pcall[, `:=`(head_coach = trimws(head_coach), off_play_caller = trimws(off_play_caller), def_play_caller = trimws(def_play_caller))]
CALLERS <- unique(pcall[season == 2026 & week == 1 &
                        (head_coach == off_play_caller | head_coach == def_play_caller)]$head_coach)
# play-calling ABOVE TALENT from R/81, not raw EPA: a caller with a franchise
# quarterback no longer outranks one without, which is what made this line weak
ca <- fread("data/derived/caller_above_talent.csv")
rc <- merge(rc, ca[, .(coach = caller, pc_epa = caller_adj, pc_side = side, pc_seasons = seasons,
                       pc_coord_seasons = seasons_as_coord)], by = "coach", all.x = TRUE)
gr_side <- function(v, sd_) {
  pool <- ca[side == sd_]$caller_adj
  p <- sapply(v, function(x) if (is.na(x)) NA_real_ else mean(pool <= x, na.rm = TRUE))
  fifelse(is.na(p), NA_character_, fifelse(p > 0.8, "A", fifelse(p > 0.6, "B", fifelse(p > 0.4, "C", fifelse(p > 0.2, "D", "F")))))
}
# a head coach who does not call plays is penalised, not excused: the ridge fit
# says calling is worth about a tenth of a win per season, so the line counts
rc[, calls := coach %in% CALLERS]
# a coach whose last club moved on from him starts the new job carrying that:
# retreads average 0.86 wins above talent below first-time hires in year one
jc <- fread("data/derived/job_change_2026.csv")[, .(coach, job_status = status)]
rc <- merge(rc, jc, by = "coach", all.x = TRUE)
rg <- fread("data/derived/retread_gap.csv")
rc[, g_caller := NA_character_]
rc[pc_side == "offense", g_caller := gr_side(pc_epa, "offense")]
rc[pc_side == "defense", g_caller := gr_side(pc_epa, "defense")]
# does not call plays this season: a D on this line, which costs about a quarter
# of a grade point at its weight, rather than a free pass
rc[calls == FALSE & seasons > 0, `:=`(g_caller = "D", pc_side = "none")]
cat(sprintf("\n%d of the 2026 head coaches call their own plays; %d do not and take a D on that line\n",
            sum(rc$calls), rc[calls == FALSE & seasons > 0, .N]))


LINES <- c(fourth = "Fourth downs", two_pt = "Going for two", penalties = "Penalties", offense = "Offense above talent",
           defense = "Defense above talent", spread = "Beats the spread", results = "Results above resources")
# graded on the curve within the class of 2026: A is the top fifth of the coaches
# on this card, F the bottom fifth, exactly as a teacher curves one classroom
grade <- function(x) {
  p <- frank(x, na.last = "keep") / sum(!is.na(x))
  fifelse(is.na(x), NA_character_, fifelse(p > 0.8, "A", fifelse(p > 0.6, "B", fifelse(p > 0.4, "C", fifelse(p > 0.2, "D", "F")))))
}
pts <- c(A = 4, B = 3, C = 2, D = 1, F = 0)
# weights from R/82's ridge fit instead of a flat average: each line is worth
# what it is worth at predicting the next season, not what an editor decided
WT <- fread("data/derived/card_weights_final.csv")
wvec <- setNames(WT$weight, WT$line_key)
cat("weights in use:\n"); print(WT[order(-weight)])
for (k in names(LINES)) rc[, (paste0("g_", k)) := grade(get(k))]
gcols <- paste0("g_", names(LINES))
rc[, n_lines := rowSums(!is.na(as.matrix(.SD))), .SDcols = gcols]
rc[, n_lines := n_lines + as.integer(!is.na(g_caller))]
gcols2 <- c(gcols, "g_caller")
pmat <- apply(as.matrix(rc[, ..gcols2]), 2, function(g) unname(pts[g]))
colnames(pmat) <- c(names(LINES), "calls")
wrow <- wvec[c(names(LINES), "calls")]
rc[, gpa := {
  num <- rowSums(sweep(pmat, 2, wrow, "*"), na.rm = TRUE)
  den <- rowSums(sweep(!is.na(pmat), 2, wrow, "*"), na.rm = TRUE)
  fifelse(den > 0, num / den, NA_real_)
}]
rc[n_lines < 4, gpa := NA_real_]
# THE NEW-JOB DEDUCTION. A coach whose club moved on from him starts the new job
# 0.86 wins above talent behind a first-time hire (p = 0.22, 34 against 66). It
# is charged to the four coaches it applies to rather than weighted into
# everybody's GPA, and it is sized by how many grade points a win is worth on
# this card: the slope of GPA on career wins above talent among the graded.
sl <- rc[!is.na(gpa) & !is.na(war_no_market), coef(lm(gpa ~ war_no_market))[2]]
DEDUCT <- round(abs(rg$estimate_wins[1]) * unname(sl), 2)
rc[, gpa_before := gpa]
rc[job_status == "his last club moved on from him" & !is.na(gpa), gpa := gpa - DEDUCT]
cat(sprintf("\nnew-job deduction: %.2f wins x %.2f grade points per win = %.2f, charged to %d coaches\n",
            abs(rg$estimate_wins[1]), sl, DEDUCT, rc[job_status == "his last club moved on from him" & !is.na(gpa), .N]))
print(rc[job_status == "his last club moved on from him", .(coach, team, gpa_before = round(gpa_before, 2), gpa = round(gpa, 2))])
# tiers are set within the class, like a curve: the top of a class makes the
# dean's list even in a year when nobody is historically great
# scale the class so the best coach in it sits at 4.0: the GPA is a ranking of
# this year's field, and a 3.2 top score invites the question of who the missing
# 0.8 belongs to. The raw weighted score is kept in the file as gpa_raw.
rc[, gpa_raw := gpa]
rc[!is.na(gpa), gpa := gpa * 4 / max(gpa, na.rm = TRUE)]
cat(sprintf("scaled the class so the top is 4.0: raw top %.2f -> 4.00\n", max(rc$gpa_raw, na.rm = TRUE)))
# the coaching tree, rebuilt on real staff titles from Wikipedia (py/fetch_staffs.py)
# rather than the playcaller file, which could not see a coordinator working for a
# head coach who called his own plays. Still a badge and not a grade: it does not
# predict, and net of tenure it points the wrong way.
tr <- fread("data/derived/coaching_tree.csv")[, .(coach = mentor, branches, tree_net = tree_resid, branch_names)]
rc <- merge(rc, tr, by = "coach", all.x = TRUE)
rc[is.na(branches), branches := 0L]
rc[, pct := frank(-gpa, na.last = "keep") / sum(!is.na(gpa))]
rc[, tier := fifelse(is.na(gpa), "Incomplete",
             fifelse(pct <= 0.15, "Dean's list",
             fifelse(pct <= 0.45, "Honor roll",
             fifelse(pct <= 0.80, "Passing", "Probation"))))]
cut_dean <- rc[tier == "Dean's list", min(gpa)]; cut_hon <- rc[tier == "Honor roll", min(gpa)]; cut_pass <- rc[tier == "Passing", min(gpa)]
rc[, class := fifelse(seasons >= 5, "Upperclassman", fifelse(seasons >= 3, "Sophomore", fifelse(seasons >= 1, "Freshman", "New hire")))]
rc[, active := TRUE]
cat(sprintf("graded against %d coaches in the era; %d of the %d active head coaches have a row\n",
            nrow(rc), sum(rc$active), length(ACTIVE)))
rc <- rc[active == TRUE]
setorder(rc, -gpa, na.last = TRUE)
rc[!is.na(gpa), gpa_rank := seq_len(.N)]
cat(sprintf("%d active coaches on the card, %d with a GPA (4+ lines)\n", nrow(rc), sum(!is.na(rc$gpa))))
cat("tiers:\n"); print(rc[, .N, by = tier])
cat("\nplay-caller grades for coaches without a head-coaching record:\n")
print(rc[is.na(gpa), .(coach, team, seasons, pc_side, pc_epa = round(pc_epa, 4), pc_seasons, g_caller)])
cat("\nthe card:\n")
print(rc[, c("gpa_rank", "coach", "team", "seasons", "class", gcols, "gpa", "tier"), with = FALSE][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], nrows = 60)
cat("\nline coverage:\n"); print(rc[, lapply(.SD, function(g) sum(!is.na(g))), .SDcols = gcols])
cat(sprintf("\nGPA vs WAR rank correlation: %.2f\n", cor(rc$gpa, rc$war, use = "complete", method = "spearman")))

logos <- as.data.table(load_teams(current = TRUE))[, .(team = team_abbr, logo = team_logo_espn, team_name)]
rc <- merge(rc, logos, by = "team", all.x = TRUE)
setorder(rc, -gpa, na.last = TRUE)
rc[!is.na(gpa), gpa_rank := seq_len(.N)]
out <- rc[, c("gpa_rank", "coach", "team", "team_name", "logo", "seasons", "class", "gpa", "tier", "n_lines", gcols,
              "g_caller", "pc_side", "pc_epa", "pc_seasons", "pc_coord_seasons", "job_status", "gpa_before", "gpa_raw", "branches", "tree_net", "branch_names", names(LINES), "r_qb", "r_pd"), with = FALSE]
write_csv(as.data.frame(out), "data/derived/report_card_gpa.csv")

# ---------------------------------------------------------------- figure: tiers
g <- rc[!is.na(gpa)]
g[, tier := factor(tier, levels = c("Dean's list", "Honor roll", "Passing", "Probation"))]
g[, lab := sprintf("%s  %.2f", coach, gpa)]
g[, y := -seq_len(.N)]
tier_cols <- c("Dean's list" = "#1B7837", "Honor roll" = "#2B8CBE", "Passing" = "grey45", "Probation" = "#C0504D")
g[, coach_team := sprintf("%s  %s", coach, team)]
p <- ggplot(g, aes(x = gpa, y = reorder(coach_team, gpa), colour = tier)) +
  geom_vline(xintercept = c(cut_pass, cut_hon, cut_dean), colour = "grey85", linetype = "22") +
  geom_segment(aes(x = 0, xend = gpa, yend = reorder(coach_team, gpa)), colour = "grey88", linewidth = 0.6) +
  geom_point(aes(shape = class), size = 3) +
  scale_shape_manual(values = c(Freshman = 17, Sophomore = 15, Upperclassman = 16), name = NULL) +
  geom_text(aes(label = sprintf("%.2f  (%s)", gpa, apply(as.matrix(g[, ..gcols]), 1, function(r) paste(ifelse(is.na(r), "-", r), collapse = "")))),
            hjust = 0, nudge_x = 0.07, size = 2.8, colour = "grey30") +
  scale_colour_manual(values = tier_cols, name = NULL) +
  scale_x_continuous(limits = c(0, 5.6), breaks = 0:4) +
  labs(title = sprintf("The 2026 report card: %s make the dean's list, %d on probation",
                       paste(sub("^\\S+ ", "", g[tier == "Dean's list"]$coach), collapse = ", "), sum(g$tier == "Probation")),
       subtitle = paste0("GPA out of 4.0 over seven lines: fourth-down decisions, going for two when the chart says to, penalties, offense and defense above\n",
                         "talent, beating the spread, and results above resources. Each is a letter by percentile against the other coaches on this card, and the GPA\n",
                         sprintf("weights them by what a ridge fit says each is worth at predicting the next season: offense %.0f%%, fourth downs %.0f%%, defense %.0f%%, the two\nresults lines %.0f%% each, and nothing for two-point or penalties, which did not predict. Letters in the label are in line order.\n", 100*wvec["offense"], 100*wvec["fourth"], 100*wvec["defense"], 100*wvec["spread"]),
                         sprintf("Shape is class year. Dean's list is the top 15%% of the class, probation the bottom fifth: the cuts fall at %.2f, %.2f and %.2f this year.", cut_dean, cut_hon, cut_pass)),
       x = "GPA", y = NULL,
       caption = fig_caption("nflverse, SumerSports and OverTheCap via the scripts named in R/76's header, 2012-2025 (fourth downs 2018-2025)",
         "\nA GPA needs at least four of the seven lines, so first-year head coaches are not plotted; they are graded on their play-calling record on the page instead.\nClass year is seasons as a head coach since 2012: freshman 1-2, sophomore 3-4, upperclassman 5+. Built by R/76.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/report_card_tiers.png", p, w = 12, h = 3 + nrow(g) * 0.26)
cat("\nOut: report_card_tiers.png, data/derived/report_card_gpa.csv\n")
