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
cc <- fread("data/derived/caller_classes.csv")
off <- cc[side == "Offense: EPA per play", .(coach = caller, pc_off = epa, pc_off_n = plays, pc_off_seasons = seasons)]
dfc <- cc[side != "Offense: EPA per play", .(coach = caller, pc_def = epa, pc_def_n = plays, pc_def_seasons = seasons)]
rc <- merge(rc, off, by = "coach", all.x = TRUE); rc <- merge(rc, dfc, by = "coach", all.x = TRUE)
rc[, pc_side := fifelse(is.na(pc_off) & is.na(pc_def), NA_character_,
                fifelse(is.na(pc_def) | (!is.na(pc_off) & pc_off_n >= pc_def_n), "offense", "defense"))]
rc[, pc_epa := fifelse(pc_side == "offense", pc_off, pc_def)]
rc[, pc_seasons := fifelse(pc_side == "offense", pc_off_seasons, pc_def_seasons)]
# graded against every caller on that side of the ball, 2012-2025
gr_side <- function(v, side) {
  pool <- cc[side == side]$epa
  p <- sapply(v, function(x) if (is.na(x)) NA_real_ else mean(pool <= x, na.rm = TRUE))
  fifelse(is.na(p), NA_character_, fifelse(p > 0.8, "A", fifelse(p > 0.6, "B", fifelse(p > 0.4, "C", fifelse(p > 0.2, "D", "F")))))
}
# a head coach who does not call plays is penalised, not excused: the ridge fit
# says calling is worth about a tenth of a win per season, so the line counts
rc[, calls := coach %in% CALLERS]
rc[, g_caller := NA_character_]
rc[pc_side == "offense", g_caller := gr_side(pc_epa, "Offense: EPA per play")]
rc[pc_side == "defense", g_caller := gr_side(pc_epa, "Defense: EPA per play allowed, flipped")]
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
# tiers are set within the class, like a curve: the top of a class makes the
# dean's list even in a year when nobody is historically great
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
              "g_caller", "pc_side", "pc_epa", "pc_seasons", names(LINES), "r_qb", "r_pd"), with = FALSE]
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
