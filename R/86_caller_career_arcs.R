# =============================================================================
# 86_caller_career_arcs.R -- the career arc, for play-callers.
#
# The ask: the same picture as the head-coaching career arc, but for offensive
# and defensive play-callers, cumulative EPA, controlling for team talent with
# the Madden ratings in the controls.
#
# WHAT IS BANKED. For every game a man called plays, his unit's EPA per play
# minus what that team's talent predicts for the season, times the plays he
# called. Summed in career order. Climbing means his unit beat what its
# payroll, its quarterback and its Madden roster rating said it should be,
# game after game.
#
# THE CONTROLS, the same ones the report card uses: top-25 payroll as a share
# of the cap, the starting quarterback's cap share, his EPA per dropback the
# season before, a flag for a first-year starter, and the roster's Madden
# rating from 2017 when the ratings begin. Season fixed effects hold the era
# constant. Defence is flipped so climbing is good on both sides.
#
# WHAT IT CANNOT DO: the talent controls are team-season, so a coordinator is
# credited with the whole gap between his unit and its roster, including
# whatever the head coach contributes. Attribution is the play-caller of
# record for that team-season, so a man who shares the call gets all of it.
#
# Out: docs/figures/caller_career_arcs.png, data/derived/caller_career_arcs.csv
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel) })
source("R/lib/theme_coach.R")
NFLA <- "/Users/nick/stranger9977/nfl-analysis/data"
YRS <- 2012:2025
MIN_GAMES <- 48

pbp <- rbindlist(lapply(YRS, function(y)
  fread(file.path(NFLA, sprintf("play_by_play_%d.csv.gz", y)),
        select = c("season", "week", "game_id", "posteam", "defteam", "epa", "play_type", "season_type"),
        showProgress = FALSE)))
pbp <- pbp[season_type == "REG" & play_type %in% c("pass", "run") & !is.na(epa) & !is.na(posteam)]
pc <- fread(file.path(NFLA, "playcallers.csv"),
            select = c("season", "week", "team", "head_coach", "off_play_caller", "def_play_caller"))
for (k in c("head_coach", "off_play_caller", "def_play_caller")) pc[, (k) := trimws(get(k))]

# talent expectation per team-season, exactly the report card's controls
pan <- unique(fread("data/derived/coaching_war_seasons.csv")[
  , .(season, team, contract_z, qb_z, qbp_z, madden_z, qbp_na)], by = c("season", "team"))
off_ts <- pbp[, .(off_plays = .N, off_epa = mean(epa)), by = .(season, team = posteam)]
def_ts <- pbp[, .(def_plays = .N, def_epa = -mean(epa)), by = .(season, team = defteam)]
ts <- merge(merge(off_ts, def_ts, by = c("season", "team")), pan, by = c("season", "team"))
ts[, `:=`(madden_z17 = fifelse(is.na(madden_z), 0, madden_z), post17 = as.integer(season >= 2017),
          new_qb = as.integer(qbp_na %in% c(TRUE, "TRUE")), season_f = factor(season))]
ts <- ts[!is.na(contract_z) & !is.na(qb_z) & !is.na(qbp_z)]
fo <- lm(off_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + season_f, ts, weights = off_plays)
fd <- lm(def_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + season_f, ts, weights = def_plays)
ts[, `:=`(exp_off = fitted(fo), exp_def = fitted(fd))]
cat(sprintf("talent fits: offense R2 %.3f, defense R2 %.3f, on %d team-seasons; Madden %+.4f and %+.4f EPA per play per SD from 2017\n",
            summary(fo)$r.squared, summary(fd)$r.squared, nrow(ts),
            coef(fo)["madden_z17:post17"], coef(fd)["madden_z17:post17"]))

# per game, per caller, EPA banked above that team-season's expectation
og <- pbp[, .(plays = .N, epa = mean(epa)), by = .(season, week, game_id, team = posteam)]
dg <- pbp[, .(plays = .N, epa = -mean(epa)), by = .(season, week, game_id, team = defteam)]
og <- merge(og, ts[, .(season, team, exp = exp_off)], by = c("season", "team"))
dg <- merge(dg, ts[, .(season, team, exp = exp_def)], by = c("season", "team"))
og <- merge(og, pc[, .(season, week, team, caller = off_play_caller)], by = c("season", "week", "team"))
dg <- merge(dg, pc[, .(season, week, team, caller = def_play_caller)], by = c("season", "week", "team"))
g <- rbind(og[, side := "Offensive play-callers"], dg[, side := "Defensive play-callers"])[caller != ""]
g[, banked := (epa - exp) * plays]
setorder(g, side, caller, season, week)
g[, gm := seq_len(.N), by = .(side, caller)]
g[, cum := cumsum(banked), by = .(side, caller)]
tot <- g[, .(games = .N, final = last(cum), per_game = last(cum) / .N), by = .(side, caller)][games >= MIN_GAMES]
setorder(tot, side, -final)
cat(sprintf("\n%d callers with %d+ games\n", nrow(tot), MIN_GAMES))
cat("\ntop and bottom, offense:\n"); print(rbind(head(tot[side == "Offensive play-callers"], 8), tail(tot[side == "Offensive play-callers"], 4))[, .(caller, games, final = round(final), per_game = round(per_game, 2))])
cat("\ntop and bottom, defense:\n"); print(rbind(head(tot[side == "Defensive play-callers"], 8), tail(tot[side == "Defensive play-callers"], 4))[, .(caller, games, final = round(final), per_game = round(per_game, 2))])
write_csv(as.data.frame(tot), "data/derived/caller_career_arcs.csv")

# ---------------------------------------------------------------- figure
g2 <- merge(g, tot[, .(side, caller, final, games)], by = c("side", "caller"))
# highlight the biggest totals, the best rates (so a short, blazing career like
# Ben Johnson's or Mike Macdonald's is not hidden by men with twice the games),
# and the two worst on each side
pick <- function(sd_) {
  x <- tot[side == sd_]
  rbind(head(x[order(-final)], 5), head(x[order(-per_game)], 3), tail(x[order(-final)], 2))
}
hi <- unique(rbind(pick("Offensive play-callers"), pick("Defensive play-callers")))
g2[, isHi := paste(side, caller) %in% paste(hi$side, hi$caller)]
ends <- g2[, .SD[.N], by = .(side, caller)][isHi == TRUE]
ends[, lab := sprintf("%s  %+.0f", caller, final)]
pal <- c("#1f3f8f", "#2B8CBE", "#1B7837", "#7b3fa0", "#0f8f8f", "#D55E00", "#b23a3a")
g2[, cid := as.integer(factor(caller)), by = side]
p <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "22") +
  geom_line(data = g2[isHi == FALSE], aes(gm, cum, group = caller), colour = "grey85", linewidth = 0.35) +
  geom_line(data = g2[isHi == TRUE], aes(gm, cum, group = caller, colour = caller), linewidth = 1.05) +
  geom_point(data = ends, aes(gm, cum, colour = caller), size = 2.4) +
  geom_text_repel(data = ends, aes(gm, cum, label = lab, colour = caller), size = 3.1, fontface = "bold",
                  hjust = 0, direction = "y", nudge_x = 18, min.segment.length = 0, box.padding = 0.42,
                  segment.colour = "grey75", segment.size = 0.3, max.overlaps = 40, seed = 86, show.legend = FALSE) +
  facet_wrap(~ side, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = rep(pal, 4), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.30))) +
  labs(title = "The career arc for play-callers: points banked above what the roster predicted",
       subtitle = paste0("Cumulative across every regular-season game a man called plays, 2012-2025. Each game is his unit's EPA per play minus what that team's\n",
                         "payroll, quarterback and Madden roster rating predict for the season, times the plays he called. Climbing means the unit beat its own\n",
                         "roster, week after week. Grey lines are the other callers with ", MIN_GAMES, " games or more. Defence is flipped, so up is good on both."),
       x = "games called (in career order)", y = "cumulative EPA above the roster's expectation",
       caption = fig_caption("nflverse play-by-play 2012-2025; OverTheCap contracts via nflreadr; Madden launch ratings 2017-2025; playcaller attribution from the nflverse file",
         paste0("\nThe controls are team-season, so a coordinator is credited with everything his unit did above its roster, including whatever the head coach contributed.\n",
                "Attribution is the play-caller of record, so a man who shares the call gets all of it. Built by R/86."))) +
  theme_coach(grid = "y") +
  theme(strip.text = element_text(face = "bold", hjust = 0, size = 12))
save_fig("docs/figures/caller_career_arcs.png", p, w = 14, h = 11.5)
cat("\nOut: caller_career_arcs.png\n")
