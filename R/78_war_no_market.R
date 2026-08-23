# =============================================================================
# 78_war_no_market.R -- the combined board: no betting market, same-season
# quarterback play controlled. This replaces the market-anchored board as the
# headline measure.
#
# Why. The market-anchored board credits a coach with two things: the market
# rating his team above its priced talent (the "premium") and the team beating
# that line (the "surprise"). The premium is partly the market's opinion of the
# coach himself, so the board pays him for his own reputation. It shows: 58% of
# Sean McDermott's number, 85% of Sean McVay's and 92% of Bill Belichick's came
# from premium rather than from beating the line, McDermott ranked first, and
# Buffalo replaced him for 2026. Four of that board's top ten are not head
# coaches this season.
#
# This script fits one model that takes both objections at once:
#   outcome  adjusted point differential per game (no market anywhere)
#   controls payroll (top-25 cap share), Madden roster rating 2017 on,
#            the starting quarterback's cap share, his EPA per dropback LAST
#            season, and his EPA per dropback THIS season, so a coach is paid
#            for neither his quarterback's contract nor his quarterback's play
#   effects  coach, franchise and season, coach shrunk toward zero
#   scale    converted to wins per 17 games with the same points-to-wins slope
#            R/71 uses, and set against the same replacement line (what a
#            week-1 first-season hire delivered, interims excluded)
#
# THE COST, stated plainly: a coach who develops his quarterback gets no credit
# for it here. That is the price of not paying him for the quarterback he was
# handed, and it is why the board is shown next to its two halves rather than
# alone.
#
# Out: docs/figures/war_no_market.png, docs/figures/war_market_reputation.png
#      data/derived/coaching_war_no_market.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(lme4); library(ggrepel) })
source("R/lib/theme_coach.R")

s <- fread("data/derived/coaching_war_seasons.csv")[in_main_window == TRUE & !is.na(pd_adj_pg)]
w <- fread("data/derived/coaching_war.csv")
s[, season_f := factor(season)]
s[, madden_z17 := fifelse(is.na(madden_z), 0, madden_z)][, post17 := as.integer(season >= 2017)]
s[, wt := games / 17]
s <- s[!is.na(contract_z) & !is.na(qb_z) & !is.na(qbp_z) & !is.na(qbcur_z)]
cat(sprintf("panel: %d coach-seasons, %d coaches\n", nrow(s), uniqueN(s$coach)))

# points per game to wins per 17: the slope R/71 uses, recomputed here for the record
ts <- s[, .(pd = mean(pd_adj_pg), wins = mean(act17)), by = .(season, team)]
slope <- coef(lm(wins ~ pd, ts))[2]
cat(sprintf("points per game to wins per 17: %.3f wins per point\n", slope))

f <- lm(pd_adj_pg ~ contract_z + qb_z + qbp_z + qbcur_z + madden_z17:post17 + season_f, data = s, weights = wt)
cat(sprintf("talent + same-season QB fit on point differential: R2 %.3f; same-season QB %+.2f pts/game per SD\n",
            summary(f)$r.squared, coef(f)["qbcur_z"]))
s[, resid_pd := pd_adj_pg - fitted(f)]
m <- lmer(resid_pd ~ 1 + (1 | coach) + (1 | team) + (1 | season), data = s, weights = wt, REML = TRUE,
          control = lmerControl(calc.derivs = FALSE))
vc <- as.data.table(VarCorr(m)); cat("variance components (points per game):\n"); print(vc[, .(grp, sdcor = round(sdcor, 3))])
b <- as.data.table(ranef(m)$coach, keep.rownames = "coach"); setnames(b, "(Intercept)", "effect_pd")
bt <- as.data.table(ranef(m)$team, keep.rownames = "team"); setnames(bt, "(Intercept)", "team_pd")
bs <- as.data.table(ranef(m)$season, keep.rownames = "season"); setnames(bs, "(Intercept)", "season_pd")
s <- merge(s, bt, by = "team", all.x = TRUE); s[, season_chr := as.character(season)]
s <- merge(s, bs, by.x = "season_chr", by.y = "season", all.x = TRUE)
s[, rpc := resid_pd - (fixef(m)[1] + team_pd + season_pd)]
rep_line <- s[spell_yr == 1 & interim == FALSE, weighted.mean(rpc, wt)]
cat(sprintf("replacement line: %+.3f points per game (%d first-season rows, interims excluded)\n", rep_line, nrow(s[spell_yr == 1 & interim == FALSE])))

nm <- merge(b, s[, .(seasons = uniqueN(season), games = sum(games), teams = uniqueN(team),
                     team_list = paste(sort(unique(team)), collapse = "/"),
                     first_season = min(season), last_season = max(season)), by = coach], by = "coach")
nm[, war_no_market := (effect_pd - rep_line) * slope]
nm <- merge(nm, w[, .(coach, war_market = war_per_season, rank_market = rank, eligible)], by = "coach", all.x = TRUE)
nm[is.na(eligible), eligible := FALSE]
setorder(nm, -war_no_market)
nm[eligible == TRUE & !is.na(rank_market), rank_no_market := seq_len(.N)]
nm[, move := rank_market - rank_no_market]

pc <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv")
act <- unique(pc[season == 2026 & week == 1, trimws(head_coach)])
nm[, still_hc := coach %in% act]
nmE <- nm[eligible == TRUE & !is.na(rank_no_market)]
write_csv(as.data.frame(nm[, .(rank_no_market, coach, teams, team_list, seasons, games, war_no_market, effect_pd,
                               rank_market, war_market, move, still_hc, eligible)]), "data/derived/coaching_war_no_market.csv")
cat("\nthe board:\n")
print(nmE[, .(rank_no_market, coach, seasons, war = round(war_no_market, 2), pts_g = round(effect_pd, 2),
             was = rank_market, move, hc26 = still_hc)][1:20])
cat(sprintf("\nrank correlation with the market board: %.2f; with a head coach in 2026 in the top 10: %d of 10\n",
            cor(nmE$rank_no_market, nmE$rank_market, method = "spearman"), nmE[rank_no_market <= 10, sum(still_hc)]))

# ---------------------------------------------------------------- fig 1: the board
top <- nmE[1:25]
top[, lab := sprintf("%+.2f   %d seasons%s", war_no_market, seasons, fifelse(still_hc, "", ", not a head coach in 2026"))]
p1 <- ggplot(top, aes(x = war_no_market, y = reorder(coach, war_no_market))) +
  geom_vline(xintercept = 0, colour = "grey60", linetype = "22") +
  geom_segment(aes(x = 0, xend = war_no_market, yend = reorder(coach, war_no_market)), colour = "grey85", linewidth = 3) +
  geom_point(aes(colour = still_hc), size = 3.2) +
  geom_text(aes(label = lab, colour = still_hc), hjust = 0, nudge_x = 0.03, size = 2.9, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#2B8CBE", `FALSE` = "grey55"),
                      labels = c(`TRUE` = "head coach in 2026", `FALSE` = "no longer a head coach"), name = NULL) +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.02, 0.55))) +
  labs(title = "Coaching WAR without the betting market, and without credit for the quarterback",
       subtitle = paste0("Wins per season above a first-season hire, from point differential per game with payroll, Madden roster ratings, the quarterback's pay,\n",
                         "his play LAST season and his play THIS season all controlled. No betting line anywhere, so no coach is paid for his own reputation.\n",
                         "The cost: a coach who develops his quarterback gets nothing for it here. Top 25 of ", nrow(nmE), " head coaches with 4+ seasons, 2012-2025."),
       x = "wins per season above replacement", y = NULL,
       caption = fig_caption("nflverse schedules; OverTheCap contracts via nflreadr 2012-2025; Madden launch ratings 2017-2025; SumerSports quarterback EPA",
         sprintf("\nPoints converted to wins at %.2f wins per point per game. Replacement is what a week-1 first-season hire delivered, interims excluded. Built by R/78.", slope))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/war_no_market.png", p1, w = 12, h = 9)

# ---------------------------------------------------------------- fig 2: why the market board was wrong
rep <- w[eligible == TRUE & !is.na(rank)][, .(coach, war = war_per_season, rank = as.integer(rank),
                                              premium = premium_per_season, surprise = surprise_per_season)]
rep[, share := premium / (abs(premium) + abs(surprise))]
rep <- merge(rep, nm[, .(coach, still_hc, rank_no_market)], by = "coach")
rp <- rep[rank <= 15]
rp[, lab := sprintf("%s%s", coach, fifelse(still_hc, "", "  (fired)"))]
long <- melt(rp, id.vars = c("coach", "lab", "rank", "share"), measure.vars = c("premium", "surprise"),
             variable.name = "part", value.name = "v")
long[, part := factor(part, levels = c("premium", "surprise"),
                      labels = c("what the market rated the team above its roster (reputation)", "what the team did beyond the line (results)"))]
p2 <- ggplot(long, aes(x = v, y = reorder(lab, -rank), fill = part)) +
  geom_col(width = 0.68) +
  geom_text(data = rp, aes(x = premium + surprise + 0.02, y = reorder(lab, -rank),
                           label = sprintf("%.0f%% reputation", 100 * share)), inherit.aes = FALSE,
            hjust = 0, size = 2.8, colour = "grey35") +
  scale_fill_manual(values = c("#D55E00", "#2B8CBE"), name = NULL) +
  scale_x_continuous(labels = label_number(style_positive = "plus"), expand = expansion(mult = c(0.02, 0.3))) +
  labs(title = sprintf("The market-anchored board paid coaches for their own reputation: %.0f%% of the top name's number, %.0f%% of Belichick's",
                       100 * rp[rank == 1]$share, 100 * rp[coach == "Bill Belichick"]$share),
       subtitle = paste0("The old headline board split into its two halves, top 15. Orange is the part where the betting market rated the team above what its payroll,\n",
                         "roster ratings and quarterback explain, which includes the market's opinion of the coach. Blue is the part where the team beat that line.\n",
                         "Four of that board's top ten are not head coaches in 2026, including the coach it ranked first."),
       x = "wins per season", y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025",
         "\nThe two halves add to the old WAR exactly; the shrunk halves shown here can differ from the total by up to 0.26 wins. Built by R/78 from R/71's table.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/war_market_reputation.png", p2, w = 12, h = 7)

# ---------------------------------------------------------------- fig 3: the consensus headline
sn <- fread("data/derived/coaching_war_sensitivity.csv")[eligible == TRUE & !is.na(rank_main)]
cons <- sn[, .(coach, r_qb = rank_war_talent_same_season_qb, r_pd = rank_pd_effect_ppg, r_mkt = rank_main,
               v_qb = war_talent_same_season_qb, v_pd = pd_effect_ppg)]
cons <- merge(cons, nmE[, .(coach, r_comb = rank_no_market, war_no_market, still_hc, seasons, team_list)], by = "coach")
act26 <- unique(pc[season == 2026 & week == 1, .(coach = trimws(head_coach), team26 = team)])
cons <- merge(cons, act26, by = "coach", all.x = TRUE)
cons[, mean_rank := (r_qb + r_pd) / 2]
setorder(cons, mean_rank)
cons[, rank_cons := seq_len(.N)]
write_csv(as.data.frame(cons), "data/derived/coaching_war_consensus.csv")
cat("\nCONSENSUS board (mean rank of the two market-free-ish boards):\n")
print(cons[1:15, .(rank_cons, coach, seasons, mean_rank, r_qb, r_pd, r_comb, was_market = r_mkt, hc26 = still_hc)])
cat(sprintf("top 10 who are still head coaches: %d of 10; market board managed %d of 10\n",
            cons[rank_cons <= 10, sum(still_hc)], nmE[rank_market <= 10, sum(still_hc)]))

tc <- cons[1:20]
tc[, lab := fifelse(still_hc, sprintf("%s  %s", coach, team26), sprintf("%s  (%s, not coaching in 2026)", coach, team_list))]
lng <- melt(tc, id.vars = c("coach", "lab", "rank_cons", "mean_rank"), measure.vars = c("r_qb", "r_pd"),
            variable.name = "board", value.name = "r")
lng[, board := factor(board, levels = c("r_qb", "r_pd"),
                      labels = c("quarterback's own play removed", "no betting market: point differential"))]
p3 <- ggplot(lng, aes(x = r, y = reorder(lab, -mean_rank))) +
  geom_line(aes(group = lab), colour = "grey80", linewidth = 1.2) +
  geom_point(aes(colour = board), size = 3) +
  geom_text(data = tc, aes(x = -1.5, y = reorder(lab, -mean_rank), label = rank_cons), inherit.aes = FALSE,
            size = 3.1, fontface = "bold", colour = "grey35") +
  scale_colour_manual(values = c("#D55E00", "#2B8CBE"), name = NULL) +
  scale_x_continuous(limits = c(-3, 50), breaks = c(1, 10, 20, 30, 40, 50)) +
  labs(title = "The board without the market's opinion in it: where the two honest measures agree",
       subtitle = paste0("Each coach's rank on the two boards that do not pay him for his own reputation, joined by a line; the number on the left is the average of the\n",
                         "two. Orange takes the quarterback's play in the same season out, so a coach gets nothing for his passer. Blue drops the betting line entirely\n",
                         "and scores point differential above payroll, roster ratings and the quarterback's pay. A short line means the two agree."),
       x = "rank among the 50 head coaches with 4+ seasons", y = NULL,
       caption = fig_caption("nflverse schedules and closing spreads; OverTheCap contracts via nflreadr 2012-2025; Madden launch ratings 2017-2025",
         "\nNeither board is the truth: the first cannot credit a coach for developing his quarterback, the second cannot see anything a coach does that does not\nshow up in points. Where they agree is the safest thing this data says. Built by R/78.")) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/war_consensus.png", p3, w = 12, h = 8)
cat("\nOut: war_no_market.png, war_market_reputation.png, war_consensus.png\n")
