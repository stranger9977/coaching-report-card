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
# Seven lines, every one an existing validated number from this repo, each
# graded by percentile among the coaches on the card (A = top fifth, B, C, D,
# F = bottom fifth). Nothing is hand-weighted: the GPA is the plain mean of
# the letter points (A 4, B 3, C 2, D 1, F 0) over the lines a coach has. A
# coach needs at least four lines to get a GPA; missing lines read "Inc".
#
#   Fourth downs     R/factory/95 decision_value.csv, era_shrunk: win-probability
#                    cost of fourth-down choices vs the league in his own
#                    seasons, 2018-2025. Lower is better.
#   Going for two    R/22 two_point.csv, chart_era_shrunk: how often he goes
#                    for two when the chart says to, vs era. Higher is better.
#                    Needs 10+ chart situations.
#   Penalties        R/39 discipline.csv, head_coach resid_shrunk: penalty rate
#                    beyond the situation model. Lower is better.
#   Offense          R/factory/97 talent_adjusted.csv, adj: offense EPA per
#                    play above the talent controls. Higher is better.
#   Defense          same file, adj_def. Higher is better.
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

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel) })
source("R/lib/theme_coach.R")

w  <- fread("data/derived/coaching_war.csv")
nmk <- fread("data/derived/coaching_war_no_market.csv")[, .(coach, war_no_market)]
pcf <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
ACTIVE <- unique(trimws(pcf[season == 2026 & week == 1]$head_coach))
TEAM26 <- unique(pcf[season == 2026 & week == 1, .(coach = trimws(head_coach), team = team)])
sn <- fread("data/derived/coaching_war_sensitivity.csv")
dv <- fread("data/factory/decision_value.csv")[, .(coach, fourth = -era_shrunk)]
tp <- fread("data/derived/two_point.csv")[!is.na(chart_n) & chart_n >= 10, .(coach, two_pt = chart_era_shrunk)]
di <- fread("data/derived/discipline.csv")[role == "head_coach", .(coach = entity, penalties = -resid_shrunk)]
ta <- fread("data/factory/talent_adjusted.csv")[, .(coach, offense = adj, defense = adj_def)]
rc <- w[, .(coach, seasons, war = war_per_season, spread = surprise_per_season)]
rc <- merge(rc, nmk, by = "coach", all.x = TRUE)
rc <- merge(rc, sn[, .(coach, r_qb = rank_war_talent_same_season_qb, r_pd = rank_pd_effect_ppg)], by = "coach", all.x = TRUE)
rc[, results := war_no_market]   # the market-free board, available for every coach
for (x in list(dv, tp, di, ta)) rc <- merge(rc, x, by = "coach", all.x = TRUE)

LINES <- c(fourth = "Fourth downs", two_pt = "Going for two", penalties = "Penalties", offense = "Offense above talent",
           defense = "Defense above talent", spread = "Beats the spread", results = "Results above resources")
grade <- function(x) {
  p <- frank(x, na.last = "keep") / sum(!is.na(x))
  fifelse(is.na(x), NA_character_, fifelse(p > 0.8, "A", fifelse(p > 0.6, "B", fifelse(p > 0.4, "C", fifelse(p > 0.2, "D", "F")))))
}
pts <- c(A = 4, B = 3, C = 2, D = 1, F = 0)
for (k in names(LINES)) rc[, (paste0("g_", k)) := grade(get(k))]
gcols <- paste0("g_", names(LINES))
rc[, n_lines := rowSums(!is.na(as.matrix(.SD))), .SDcols = gcols]
rc[, gpa := rowMeans(apply(as.matrix(.SD), 2, function(g) unname(pts[g])), na.rm = TRUE), .SDcols = gcols]
rc[n_lines < 4, gpa := NA_real_]
rc[, tier := fifelse(is.na(gpa), "Incomplete", fifelse(gpa >= 3.3, "Dean's list", fifelse(gpa >= 2.7, "Honor roll", fifelse(gpa >= 2.0, "Passing", "Probation"))))]
rc[, class := fifelse(seasons >= 5, "Upperclassman", fifelse(seasons >= 3, "Sophomore", "Freshman"))]
rc[, active := coach %in% ACTIVE]
rc <- merge(rc, TEAM26, by = "coach", all.x = TRUE)
cat(sprintf("graded against %d coaches in the era; %d of the %d active head coaches have a row\n",
            nrow(rc), sum(rc$active), length(ACTIVE)))
rc <- rc[active == TRUE]
setorder(rc, -gpa, na.last = TRUE)
rc[!is.na(gpa), gpa_rank := seq_len(.N)]
cat(sprintf("%d active coaches on the card, %d with a GPA (4+ lines)\n", nrow(rc), sum(!is.na(rc$gpa))))
cat("tiers:\n"); print(rc[, .N, by = tier])
cat("\nthe card:\n")
print(rc[, c("gpa_rank", "coach", "team", "seasons", "class", gcols, "gpa", "tier"), with = FALSE][, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)], nrows = 60)
cat("\nline coverage:\n"); print(rc[, lapply(.SD, function(g) sum(!is.na(g))), .SDcols = gcols])
cat(sprintf("\nGPA vs WAR rank correlation: %.2f\n", cor(rc$gpa, rc$war, use = "complete", method = "spearman")))

out <- rc[, c("gpa_rank", "coach", "team", "seasons", "class", "gpa", "tier", "n_lines", gcols, names(LINES), "r_qb", "r_pd"), with = FALSE]
write_csv(as.data.frame(out), "data/derived/report_card_gpa.csv")

# ---------------------------------------------------------------- figure: tiers
g <- rc[!is.na(gpa)]
g[, tier := factor(tier, levels = c("Dean's list", "Honor roll", "Passing", "Probation"))]
g[, lab := sprintf("%s  %.2f", coach, gpa)]
g[, y := -seq_len(.N)]
tier_cols <- c("Dean's list" = "#1B7837", "Honor roll" = "#2B8CBE", "Passing" = "grey45", "Probation" = "#C0504D")
g[, coach_team := sprintf("%s  %s", coach, team)]
p <- ggplot(g, aes(x = gpa, y = reorder(coach_team, gpa), colour = tier)) +
  geom_vline(xintercept = c(2.0, 2.7, 3.3), colour = "grey85", linetype = "22") +
  geom_segment(aes(x = 0, xend = gpa, yend = reorder(coach_team, gpa)), colour = "grey88", linewidth = 0.6) +
  geom_point(aes(shape = class), size = 3) +
  scale_shape_manual(values = c(Freshman = 17, Sophomore = 15, Upperclassman = 16), name = NULL) +
  geom_text(aes(label = sprintf("%.2f  (%s)", gpa, apply(as.matrix(g[, ..gcols]), 1, function(r) paste(ifelse(is.na(r), "-", r), collapse = "")))),
            hjust = 0, nudge_x = 0.07, size = 2.8, colour = "grey30") +
  scale_colour_manual(values = tier_cols, name = NULL) +
  scale_x_continuous(limits = c(0, 5.6), breaks = 0:4) +
  labs(title = sprintf("The report card, 2026 head coaches: %d on the dean's list, %d on the honor roll, %d on probation",
                       sum(g$tier == "Dean's list"), sum(g$tier == "Honor roll"), sum(g$tier == "Probation")),
       subtitle = paste0("GPA out of 4.0 over seven lines: fourth-down decisions, going for two when the chart says to, penalties, offense and defense above\n",
                         "talent, beating the spread, and results above resources (the market-free board). Each is a letter by percentile against every coach of the\n",
                         "era, not only the ones still working, so a grade means the same thing every year. Letters in the label are in that order, a dash is an\n",
                         "incomplete. Shape is class year. Dotted lines are the tier cuts: 2.0 passing, 2.7 honor roll, 3.3 dean's list."),
       x = "GPA", y = NULL,
       caption = fig_caption("nflverse, SumerSports and OverTheCap via the scripts named in R/76's header, 2012-2025 (fourth downs 2018-2025)",
         "\nA GPA needs at least four of the seven lines; a coach with fewer is left off. Class year is seasons as a head coach since 2012: freshman 1-2, sophomore 3-4,\nupperclassman 5+. Built by R/76.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/report_card_tiers.png", p, w = 12, h = 3 + nrow(g) * 0.26)
cat("\nOut: report_card_tiers.png, data/derived/report_card_gpa.csv\n")
