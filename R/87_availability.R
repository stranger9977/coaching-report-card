# =============================================================================
# 87_availability.R -- doing more with less: talent controls that also know who
# was actually on the field.
#
# The ask: Shanahan has been without his best players for years, so a fair
# measure should control for talent AND for losing that talent to injury.
#
# HOW AVAILABILITY IS MEASURED. Payroll and Madden price the roster a team
# assembled; neither knows that the roster spent October in street clothes. So
# for every team-season:
#   1. Intended starters are the eleven biggest snap-takers from the PREVIOUS
#      season who are still on this year's roster, unioned with this season's
#      weeks 1 to 4 leaders, snap-weighted. Using last season matters: a star
#      hurt from week 1 never appears in his own team's early snaps, and
#      Christian McCaffrey's 2024 is exactly that case.
#   2. For every week from 5 on, availability is the share of that weighted
#      group who took a snap. One is the whole intended lineup on the field;
#      0.8 means a fifth of it, by snap weight, was missing.
#   3. A team-season's availability is the average across those weeks.
#
# Then the same offense and defense fits the report card uses are run again
# with availability added, and the coach and play-caller residuals are compared
# before and after. A coach who was missing more of his lineup than the league
# gets that back.
#
# HONEST LIMITS: snap counts cannot tell an injury from a benching or a
# healthy scratch, weeks 1 to 4 can themselves be injured, and a man who plays
# hurt counts as present. It is availability, not health.
#
# Out: data/derived/availability.csv, docs/figures/availability_effect.png
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(nflreadr); library(ggplot2)
                   library(scales); library(ggrepel) })
source("R/lib/theme_coach.R")
YRS <- 2012:2025

sc <- as.data.table(load_snap_counts(seasons = YRS))
sc <- sc[game_type == "REG" | is.na(game_type)]
sc[, `:=`(off = as.numeric(offense_snaps), def = as.numeric(defense_snaps))]
sc[is.na(off), off := 0][is.na(def), def := 0]
cat(sprintf("snap rows %s, seasons %d-%d, teams %d\n", comma(nrow(sc)), min(sc$season), max(sc$season), uniqueN(sc$team)))

# Intended starters come from the PRIOR season's snap leaders who are still on
# the roster, unioned with this season's weeks 1-4 leaders. That matters: a star
# hurt from week 1 never appears in his own team's early snaps, so an
# early-snaps-only definition cannot see him. Christian McCaffrey's 2024 is the
# case in point, and it is exactly the season this question is about.
ros <- as.data.table(load_rosters(seasons = YRS))[, .(season, team, player = full_name)]
ros <- unique(ros[!is.na(player)])
avail_side <- function(snapcol, label) {
  x <- sc[get(snapcol) > 0, .(season, week, team, player, s = get(snapcol))]
  # last season's top snap-takers, by team
  prior <- x[, .(s = sum(s)), by = .(season, team, player)]
  prior[, share := s / sum(s), by = .(season, team)]
  setorder(prior, season, team, -share)
  prior11 <- prior[, head(.SD, 11), by = .(season, team)][, .(season = season + 1L, team, player, w_prior = share)]
  prior11 <- merge(prior11, unique(ros[, .(season, team, player)]), by = c("season", "team", "player"))   # still on the roster this year
  # this season's early leaders
  early <- x[week <= 4, .(s = sum(s)), by = .(season, team, player)]
  early[, share := s / sum(s), by = .(season, team)]
  setorder(early, season, team, -share)
  early11 <- early[, head(.SD, 11), by = .(season, team)][, .(season, team, player, w_early = share)]
  starters <- merge(prior11, early11, by = c("season", "team", "player"), all = TRUE)
  starters[is.na(w_prior), w_prior := 0][is.na(w_early), w_early := 0]
  starters[, w := pmax(w_prior, w_early)]
  starters[, w := w / sum(w), by = .(season, team)]
  late <- x[week >= 5, .(season, team, week, player, played = 1L)]
  grid <- unique(late[, .(season, team, week)])
  grid <- merge(grid, starters[, .(season, team, player, w)], by = c("season", "team"), allow.cartesian = TRUE)
  grid <- merge(grid, late, by = c("season", "team", "week", "player"), all.x = TRUE)
  grid[is.na(played), played := 0L]
  wk <- grid[, .(avail = sum(w * played) / sum(w)), by = .(season, team, week)]
  out <- wk[, .(avail = mean(avail), weeks = .N), by = .(season, team)]
  setnames(out, "avail", label)[]
}
ao <- avail_side("off", "avail_off")
ad <- avail_side("def", "avail_def")
av <- merge(ao[, .(season, team, avail_off)], ad[, .(season, team, avail_def)], by = c("season", "team"))
cat(sprintf("\navailability built for %d team-seasons; league mean offense %.3f, defense %.3f\n",
            nrow(av), mean(av$avail_off), mean(av$avail_def)))
write_csv(as.data.frame(av), "data/derived/availability.csv")

# ---------------------------------------------------------------- who lost the most
pcf <- fread("/Users/nick/stranger9977/nfl-analysis/data/playcallers.csv",
             select = c("season", "week", "team", "head_coach", "off_play_caller", "def_play_caller"))
for (k in c("head_coach", "off_play_caller", "def_play_caller")) pcf[, (k) := trimws(get(k))]
hc <- unique(pcf[, .(season, team, coach = head_coach, oc = off_play_caller)])[, .SD[1], by = .(season, team)]
av2 <- merge(av, hc, by = c("season", "team"))
av2 <- merge(av2, av[, .(lg_off = mean(avail_off), lg_def = mean(avail_def)), by = season], by = "season")
by_coach <- av2[, .(seasons = .N, off = mean(avail_off - lg_off), def = mean(avail_def - lg_def)), by = coach][seasons >= 4]
cat("\nleast available offensive lineups against the league, by head coach:\n")
print(by_coach[order(off)][1:10, .(coach, seasons, off = round(off, 4), def = round(def, 4))])
cat("\nShanahan by season, offensive availability against the league:\n")
print(av2[coach == "Kyle Shanahan", .(season, avail_off = round(avail_off, 3), league = round(lg_off, 3),
                                      gap = round(avail_off - lg_off, 3))][order(season)])

# ---------------------------------------------------------------- refit with availability
pbp <- rbindlist(lapply(YRS, function(y)
  fread(sprintf("/Users/nick/stranger9977/nfl-analysis/data/play_by_play_%d.csv.gz", y),
        select = c("season", "posteam", "defteam", "epa", "play_type", "season_type"), showProgress = FALSE)))
pbp <- pbp[season_type == "REG" & play_type %in% c("pass", "run") & !is.na(epa) & !is.na(posteam)]
o <- pbp[, .(off_plays = .N, off_epa = mean(epa)), by = .(season, team = posteam)]
d2 <- pbp[, .(def_plays = .N, def_epa = -mean(epa)), by = .(season, team = defteam)]
ts <- merge(o, d2, by = c("season", "team"))
pan <- unique(fread("data/derived/coaching_war_seasons.csv")[
  , .(season, team, contract_z, qb_z, qbp_z, madden_z, qbp_na)], by = c("season", "team"))
ts <- merge(merge(ts, pan, by = c("season", "team")), av, by = c("season", "team"))
ts[, `:=`(madden_z17 = fifelse(is.na(madden_z), 0, madden_z), post17 = as.integer(season >= 2017),
          new_qb = as.integer(qbp_na %in% c(TRUE, "TRUE")), season_f = factor(season))]
ts <- ts[!is.na(contract_z) & !is.na(qb_z) & !is.na(qbp_z)]
base_o <- lm(off_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + season_f, ts, weights = off_plays)
full_o <- lm(off_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + avail_off + season_f, ts, weights = off_plays)
base_d <- lm(def_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + season_f, ts, weights = def_plays)
full_d <- lm(def_epa ~ contract_z + qb_z + qbp_z + new_qb + madden_z17:post17 + avail_def + season_f, ts, weights = def_plays)
co <- summary(full_o)$coefficients; cd <- summary(full_d)$coefficients
cat(sprintf("\navailability in the offense fit: %+.3f EPA per play per unit (t = %.1f, p = %.3f); R2 %.3f to %.3f\n",
            co["avail_off", 1], co["avail_off", 3], co["avail_off", 4], summary(base_o)$r.squared, summary(full_o)$r.squared))
cat(sprintf("availability in the defense fit: %+.3f (t = %.1f, p = %.3f); R2 %.3f to %.3f\n",
            cd["avail_def", 1], cd["avail_def", 3], cd["avail_def", 4], summary(base_d)$r.squared, summary(full_d)$r.squared))
cat(sprintf("a lineup one standard deviation less available costs %.3f EPA per play on offense\n",
            co["avail_off", 1] * sd(ts$avail_off)))

ts[, `:=`(off_base = off_epa - fitted(base_o), off_full = off_epa - fitted(full_o),
          def_base = def_epa - fitted(base_d), def_full = def_epa - fitted(full_d))]
ts <- merge(ts, hc, by = c("season", "team"))
res <- ts[, .(seasons = .N,
              off_before = weighted.mean(off_base, off_plays), off_after = weighted.mean(off_full, off_plays),
              def_before = weighted.mean(def_base, def_plays), def_after = weighted.mean(def_full, def_plays),
              avail = mean(avail_off)), by = coach][seasons >= 4]
res[, `:=`(off_gain = off_after - off_before, rank_before = frank(-off_before), rank_after = frank(-off_after))]
setorder(res, -off_gain)
cat("\nwho gains most on offense once availability is in the controls:\n")
print(res[1:10, .(coach, seasons, avail = round(avail, 3), before = round(off_before, 4), after = round(off_after, 4),
                  gain = round(off_gain, 4), rank_before, rank_after)])
cat("\nShanahan:\n"); print(res[grepl("Shanahan", coach), .(coach, seasons, avail = round(avail, 3),
      before = round(off_before, 4), after = round(off_after, 4), gain = round(off_gain, 4), rank_before, rank_after)])
write_csv(as.data.frame(res[order(-off_after)]), "data/derived/availability_effect.csv")

# ---------------------------------------------------------------- figure
res[, lab := fifelse(coach %in% c("Kyle Shanahan", "Sean McVay", "Andy Reid", "Matt LaFleur", "Sean Payton",
                                  "Nick Sirianni", "Mike McCarthy", "Kevin O'Connell") |
                     abs(off_gain) > quantile(abs(off_gain), 0.9), coach, "")]
p <- ggplot(res, aes(avail, off_after)) +
  geom_hline(yintercept = 0, colour = "grey70") +
  geom_vline(xintercept = mean(ts$avail_off), colour = "grey85", linetype = "22") +
  geom_point(aes(colour = coach == "Kyle Shanahan"), size = 2.8) +
  geom_text_repel(aes(label = lab, colour = coach == "Kyle Shanahan"), size = 3.1,
                  segment.colour = "grey75", max.overlaps = 30, show.legend = FALSE) +
  scale_colour_manual(values = c(`TRUE` = "#D55E00", `FALSE` = "grey55"), guide = "none") +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_number(accuracy = 0.01, style_positive = "plus")) +
  labs(title = "Shanahan is second in offense above roster, and injuries are not the reason",
       subtitle = paste0("Across: the share of a coach's intended offensive lineup on the field, weeks 5 on. Up: offense EPA per play above what payroll,\n",
                         "the quarterback, Madden ratings and availability predict. Dotted line is the league."),
       x = "share of the intended lineup available", y = "offense above what the roster predicted",
       caption = fig_caption("nflverse snap counts, rosters and play-by-play 2013-2025; OverTheCap contracts; Madden 2017-2025",
         sprintf("\nAvailability is worth %+.3f EPA per play per unit (t = %.1f). Adding it moves Shanahan by %+.3f. Snap counts cannot tell an injury from a benching. Built by R/87.",
                 co["avail_off", 1], co["avail_off", 3], res[coach == "Kyle Shanahan"]$off_gain))) +
  theme_coach(grid = "y")
save_fig("docs/figures/availability_effect.png", p, w = 12.5, h = 7.5)

# ---------------------------------------------------------------- Shanahan by season
sh <- av2[coach == "Kyle Shanahan", .(season, avail_off, lg_off, gap = avail_off - lg_off)]
setorder(sh, season)
sh[, lab := sprintf("%+.1f", 100 * gap)]
p2 <- ggplot(sh, aes(factor(season), gap)) +
  geom_hline(yintercept = 0, colour = "grey55") +
  geom_col(aes(fill = gap > 0), width = 0.62) +
  geom_text(aes(label = lab, vjust = fifelse(gap > 0, -0.5, 1.35)), size = 3.2, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c(`TRUE` = "#1B7837", `FALSE` = "#D55E00"), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), " pts"), expand = expansion(mult = c(0.16, 0.16))) +
  labs(title = "The injury story is one or two seasons, not a career: Shanahan's lineup availability against the league",
       subtitle = "Points of percentage above or below the league, share of his intended lineup on the field from week 5 on. 2024 is the season the argument is about: McCaffrey played four games.",
       x = NULL, y = "availability vs the league",
       caption = fig_caption("nflverse snap counts and rosters, 2017-2025",
         sprintf("\nCareer average %s against a league %s. Ten points of availability is worth about %.03f EPA per play. Built by R/87.",
                 percent(mean(sh$avail_off), accuracy = 0.1), percent(mean(sh$lg_off), accuracy = 0.1), 0.1 * co["avail_off", 1]))) +
  theme_coach(grid = "y")
save_fig("docs/figures/shanahan_availability.png", p2, w = 11, h = 5.5)
cat("\nOut: availability.csv, availability_effect.png\n")
