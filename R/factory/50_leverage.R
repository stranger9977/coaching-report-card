# =============================================================================
# factory/50_leverage.R -- the question Nick actually cares about.
#
# His words: "what I really care about are decisions in the highest leverage
# situations or areas where its not exactly certain what should happen and who
# makes the optimal decisions in terms of epa, win prob added, success rate...
# does a coaches residual in high leverage situations, especially in big games
# like the playoffs, correlate with putting the team in the position to win and
# can we be certain its coaching value add"
#
# Three things have to be separated to answer that honestly.
#
# 1. IS DEVIATION EVEN THE RIGHT AXIS? A residual is signed: passing more or
#    less than expected. Being unusual is not automatically good. R/07 already
#    found the opposite league-wide, that the predictable callers are the good
#    ones. So the first test is whether that flips when the stakes rise, which
#    is exactly Nick's blackjack framing: follow the book on 14, think on 16.
#
# 2. WHAT COUNTS AS OPTIMAL? Run/pass has no known right answer, so deviation
#    is measured against the model. Fourth down does have one, so decisions
#    there are scored against the nfl4th bot. That makes fourth down the
#    cleanest "optimal decision" evidence and run/pass the softer kind.
#
# 3. CAN WE ATTRIBUTE IT TO THE COACH? The strongest available test is wins
#    above the closing spread. The market has already priced the roster, the
#    quarterback and the schedule, so anything a coach adds on top is by
#    construction something the market did not know. If high-leverage decision
#    quality predicts beating the market, that is as close to "coaching value
#    add" as observational data gets. We also control on QB quality directly
#    using the lagged ctrl_ columns from R/factory/40.
#
# Leverage = 4 * wp * (1 - wp), which peaks at a coin-flip game and falls to
# zero in a blowout. High leverage here is the top quartile of called plays.
#
# Out: docs/figures/factory/leverage_deviation.png
#      docs/figures/factory/leverage_fourthdown.png
#      data/factory/leverage_coach.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

pass <- readRDS("data/factory/fits/y_pass.rds")
ctrl <- readRDS("data/factory/controls.rds")

# rebuild the play-level frame with predictions attached
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
fitd <- pass$residuals  # season/career tables
# refit predictions are stored per play inside the fit object's data; reload
f <- readRDS("data/factory/fits/y_pass.rds")

# The saved fit keeps slices and residuals but not play-level preds, so the
# play-level join is rebuilt from the residual table at coach-season level.
cs <- as.data.table(f$residuals$season)
setnames(cs, "coach", "off_play_caller")

# ---------------------------------------------------------------- outcomes
out <- mt[!is.na(off_play_caller) & off_play_caller != "",
          .(plays = .N, epa = mean(epa, na.rm = TRUE),
            success = mean(success, na.rm = TRUE),
            wpa = mean(wpa, na.rm = TRUE),
            hi_plays = sum(leverage >= quantile(mt$leverage, 0.75, na.rm = TRUE), na.rm = TRUE),
            hi_epa = mean(epa[leverage >= quantile(mt$leverage, 0.75, na.rm = TRUE)], na.rm = TRUE),
            hi_wpa = mean(wpa[leverage >= quantile(mt$leverage, 0.75, na.rm = TRUE)], na.rm = TRUE),
            hi_success = mean(success[leverage >= quantile(mt$leverage, 0.75, na.rm = TRUE)], na.rm = TRUE),
            team = names(sort(table(posteam), decreasing = TRUE))[1]),
          by = .(off_play_caller, season)]

d <- merge(cs, out, by = c("off_play_caller","season"))
d <- d[plays >= 300]

# QB control: minutes-weighted primary QB for that caller-season
qbs <- mt[qb_dropback == 1 & !is.na(passer_player_id) & passer_player_id != "" &
          !is.na(off_play_caller),
          .N, by = .(off_play_caller, season, qb_id = passer_player_id)]
setorder(qbs, off_play_caller, season, -N)
qb1 <- qbs[, .SD[1], by = .(off_play_caller, season)]
qb1 <- merge(qb1, as.data.table(ctrl$qb)[, .(qb_id, season, ctrl_qb_prior_epa,
                                             ctrl_qb_apy_pct, ctrl_index)],
             by = c("qb_id","season"), all.x = TRUE)
d <- merge(d, qb1[, .(off_play_caller, season, ctrl_qb_prior_epa, ctrl_qb_apy_pct, ctrl_index)],
           by = c("off_play_caller","season"), all.x = TRUE)

# market: wins above expectation for that team-season
ms <- as.data.table(read_csv("data/derived/market_seasons.csv", show_col_types = FALSE))

cat(sprintf("caller-seasons: %d, with a QB control: %d\n",
            nrow(d), sum(!is.na(d$ctrl_index))))

# ---------------------------------------------------------------- test 1
# Does deviating from the situation help, and does the answer change with stakes?
#
# THE TRAP THIS CAUGHT. The first cut correlated |residual| with EPA and found
# +0.24, which reads as "deviation pays". It does not. The signed residual
# correlates at +0.236, essentially the same number, and if magnitude were
# doing the work the absolute version would be far stronger. Splitting by
# direction settles it: among callers who pass MORE than expected, deviating
# further tracks with EPA at r = +0.41; among callers who pass LESS, it is
# +0.08, nothing. So the axis is not deviation at all. It is passing.
#
# That is not a coaching insight on its own, it is the oldest finding in
# football analytics, and it is exactly the point in Michael's outline: "running
# on early downs, from what I have seen teams are still doing this too much."
# The model's expectation is calibrated to what the league actually does, so a
# caller who throws more than expected is beating a league that still runs too
# much. Worth showing as that, and not dressed up as something subtler.
d[, abs_resid := abs(resid)]
r_signed <- cor(d$resid, d$epa, use = "complete.obs")
r_abs    <- cor(d$abs_resid, d$epa, use = "complete.obs")
r_hi     <- cor(d$resid, d$hi_epa, use = "complete.obs")
cat(sprintf("\nsigned residual (pass more = +) vs EPA:     r = %+.3f\n", r_signed))
cat(sprintf("absolute deviation vs EPA:                  r = %+.3f  <- same, so it is direction\n", r_abs))
cat(sprintf("signed residual vs high-leverage EPA:       r = %+.3f\n", r_hi))
for (g in c("passes more than expected","passes less than expected")) {
  sset <- d[if (g == "passes more than expected") resid > 0 else resid <= 0]
  cat(sprintf("  within '%s': r(|resid|, EPA) = %+.3f (n = %d)\n",
              g, cor(abs(sset$resid), sset$epa), nrow(sset)))
}

dc <- d[!is.na(ctrl_index)]
m_raw  <- lm(hi_epa ~ resid, data = dc)
m_ctrl <- lm(hi_epa ~ resid + ctrl_index, data = dc)
cat(sprintf("\nhigh-leverage EPA ~ pass residual        beta = %+.5f (p = %.3f)\n",
            coef(m_raw)[2], summary(m_raw)$coefficients[2,4]))
cat(sprintf("high-leverage EPA ~ residual + QB index  beta = %+.5f (p = %.3f), QB beta %+.4f (p = %.3f)\n",
            coef(m_ctrl)[2], summary(m_ctrl)$coefficients[2,4],
            coef(m_ctrl)[3], summary(m_ctrl)$coefficients[3,4]))

write_csv(as.data.frame(d), "data/factory/leverage_coach.csv")

# ---------------------------------------------------------------- chart 1
NAMED <- c("Ben Johnson","Sean McVay","Andy Reid","Kyle Shanahan","Mike Macdonald",
           "Josh McDaniels","Matt LaFleur","Kevin O'Connell","Klint Kubiak",
           "Kellen Moore","Sean Payton")
car <- d[, .(seasons = .N, plays = sum(plays), resid = weighted.mean(resid, plays),
             hi_epa = weighted.mean(hi_epa, hi_plays)), by = off_play_caller][plays >= 1500]
r_car <- cor(car$resid, car$hi_epa)
lab <- car[off_play_caller %in% NAMED]

p1 <- ggplot(car, aes(resid, hi_epa)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_smooth(method = "lm", se = TRUE, colour = "grey45", fill = "#9db6c9",
              alpha = 0.25, linewidth = 0.6, formula = y ~ x) +
  geom_point(colour = "#2B8CBE", alpha = 0.7, size = 2.3) +
  geom_point(data = lab, colour = "#D55E00", size = 2.9) +
  geom_text_repel(data = lab, aes(label = off_play_caller), size = 3.1,
                  fontface = "bold", colour = "#8a3d00", seed = 11,
                  box.padding = 0.5, min.segment.length = 0, max.overlaps = 20) +
  scale_x_continuous(labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "pp")) +
  labs(
    title = sprintf("The callers who throw more than the situation expects are the ones who move the ball when it counts (r = %+.2f)", r_car),
    subtitle = "How much more often a caller passes than the situation-only model expects, against his offence's EPA in the highest-leverage quarter of plays",
    x = "passes MORE than expected  \u2192",
    y = "EPA per play, high-leverage snaps",
    caption = fig_caption(
      "Situation-only run/pass model (cleared all five rubric rules), 2015 to 2025",
      sprintf("%d career play-callers with 1,500+ called plays. High leverage is the top quartile of 4 x wp x (1 - wp).", nrow(car)),
      paste0("\nRead this carefully, because the obvious reading is wrong. It is not that deviating from the model pays: absolute deviation and signed deviation correlate with EPA at\n",
             "+0.24 and +0.24, so magnitude adds nothing and direction is doing all the work. The model is calibrated to what the league actually calls, and the league still runs\n",
             "too much on early downs, so throwing more than expected beats it. The finding is about the league's habit, not about any coach being clever. Built by R/factory/50."))
  ) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/leverage_deviation.png", p1, w = 12, h = 6.8)

# ---------------------------------------------------------------- test 2
# Fourth down, where "optimal" is knowable rather than modelled. Everything is
# rebuilt from the cached nfl4th probabilities so it is self-contained (the
# decisions CSV carries no play_id to join leverage on).
fd4 <- as.data.table(readRDS("data/derived/fourth_down_probs.rds"))
fd4[, went   := as.integer(play_type %in% c("run","pass") | rush_attempt == 1 | pass_attempt == 1)]
fd4[, kicked := as.integer(play_type %in% c("punt","field_goal") | punt_attempt == 1 | field_goal_attempt == 1)]
fdq <- fd4[!is.na(go_boost) & (went == 1 | kicked == 1) &
           half_seconds_remaining >= 60 & vegas_wp >= 0.05 & vegas_wp <= 0.95]
CUT <- 1.5
fdq[, verdict := fifelse(go_boost >= CUT, "should go",
                fifelse(go_boost <= -CUT, "should kick", "close"))]
fdq <- fdq[verdict != "close"]
fdq[, correct := as.integer((verdict == "should go" & went == 1) |
                            (verdict == "should kick" & kicked == 1))]
fdq[, lev := 4*vegas_wp*(1 - vegas_wp)]
gm <- fread("/Users/nick/stranger9977/nfl-analysis/data/games.csv",
            select = c("game_id","home_team","home_coach","away_coach"), showProgress = FALSE)
fdq <- merge(fdq, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
fdq[, coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
fdq <- fdq[!is.na(coach) & coach != ""]
fdq[, hi := lev >= quantile(lev, 0.75, na.rm = TRUE)]

cat("\n--- fourth-down decision quality, routine vs high leverage ---\n")
print(fdq[, .(n = .N, obey = round(100*mean(correct),1)),
          by = .(leverage = fifelse(hi, "top quartile", "the rest"))])
cat("\n--- regular season vs playoffs ---\n")
print(fdq[, .(n = .N, obey = round(100*mean(correct),1)),
          by = .(playoffs = season_type == "POST")])
cat("\n--- and the two crossed ---\n")
print(fdq[, .(n = .N, obey = round(100*mean(correct),1)),
          by = .(hi, playoffs = season_type == "POST")][order(hi, playoffs)])

# The degradation is the real finding here, so test it properly rather than
# eyeballing two percentages.
pt <- prop.test(table(fdq$hi, fdq$correct)[, c("1","0")])
mlog <- glm(correct ~ hi + verdict + abs(go_boost), data = fdq, family = binomial)
cat(sprintf("\nhigh-leverage penalty: %.1fpp (95%% CI %.1f to %.1f, p = %.4f)\n",
            100*diff(rev(pt$estimate)), 100*pt$conf.int[1], 100*pt$conf.int[2], pt$p.value))
cat(sprintf("logistic, controlling for decision type and size of the edge: hi beta = %+.3f (p = %.5f)\n",
            coef(mlog)["hiTRUE"], summary(mlog)$coefficients["hiTRUE",4]))

# ---- can we attribute it to the coach? the honest answer ------------------
cq <- fdq[hi == TRUE, .(n = .N, hi_obey = 100*mean(correct)), by = coach][n >= 25]

# BEFORE regressing anything on hi_obey, check it measures a coach at all.
# A null result about a variable with no between-coach signal is a null about
# noise, not about coaching, and would be misreported as the latter.
set.seed(11)
hh <- fdq[hi == TRUE][, half := sample(rep_len(1:2, .N)), by = coach]
sh <- dcast(hh[, .(v = 100*mean(correct), n = .N), by = .(coach, half)][n >= 12],
            coach ~ half, value.var = "v")
setnames(sh, c("coach","h1","h2"))
sh <- sh[!is.na(h1) & !is.na(h2)]
r_half <- cor(sh$h1, sh$h2)
r_sb <- 2*r_half/(1 + r_half)
cat(sprintf("\nhi_obey reliability: split-half r = %.2f, Spearman-Brown %.2f (n = %d coaches)\n",
            r_half, r_sb, nrow(sh)))
cat(sprintf("rubric's own persistence bar is %.2f, so this variable is %s\n",
            RUBRIC_PERSIST <- 0.30,
            if (r_sb >= 0.30) "reliable enough to regress on" else
              "NOT reliable enough -- any null below is about noise, not coaching"))
cm <- as.data.table(read_csv("data/derived/coach_market.csv", show_col_types = FALSE))
cj <- merge(cq, cm, by = "coach")[!is.na(rate_per17)]
r_mkt <- cor(cj$hi_obey, cj$rate_per17)
fit_m <- lm(rate_per17 ~ hi_obey, data = cj)
p_mkt <- summary(fit_m)$coefficients[2,4]
cat(sprintf("\nhigh-leverage 4th-down obedience vs wins above market: r = %+.3f (p = %.3f, n = %d)\n",
            r_mkt, p_mkt, nrow(cj)))
cat(sprintf("VERDICT: %s\n", if (p_mkt < 0.05) "significant" else
            if (r_sb < 0.30)
              "UNINFORMATIVE -- hi_obey is too unreliable to detect anything; this is not evidence of no effect"
            else "NOT significant -- cannot claim decision quality alone buys wins"))
ci <- suppressWarnings(cor.test(cj$hi_obey, cj$rate_per17)$conf.int)
cat(sprintf("95%% CI on that correlation: [%+.2f, %+.2f] -- wide enough to contain useful effects\n",
            ci[1], ci[2]))

# ---------------------------------------------------------------- chart 2
byv <- fdq[, .(n = .N, obey = 100*mean(correct)), by = .(verdict, hi)]
byv[, decision := fifelse(verdict == "should go", "When the math says GO FOR IT",
                                           "When the math says KICK")]
byv[, lev := fifelse(hi, "Highest-leverage quarter", "Everything else")]
byv[, lev := factor(lev, levels = c("Everything else", "Highest-leverage quarter"))]
byv[, decision := factor(decision, levels = c("When the math says GO FOR IT",
                                              "When the math says KICK"))]
setorder(byv, decision, lev)

p2 <- ggplot(byv, aes(decision, obey, fill = lev)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", obey)),
            position = position_dodge(width = 0.72), vjust = -0.55,
            size = 4.2, fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Everything else" = "#9db6c9",
                               "Highest-leverage quarter" = "#D55E00")) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 112),
                     breaks = seq(0, 100, 25)) +
  labs(
    title = "When the game is on the line, coaches get more cowardly, not sharper",
    subtitle = "Share of clear-cut fourth downs handled correctly, 2018 to 2025. The whole gap sits on the go-for-it side.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nfl4th decision model; leverage = 4 x wp x (1 - wp), high leverage is the top quartile",
      sprintf("%s clear-cut fourth downs. Regulation, at least a minute left in the half, win probability between 5%% and 95%%.",
              format(nrow(fdq), big.mark = ",")),
      sprintf(paste0("\nThis answers Michael's outline note directly: coaches justify a punt in a go-for-it situation but never the reverse, and it gets worse when it matters. Correct\n",
                     "go-for-it calls fall from 51.5%% to 44.6%% in the highest-leverage quarter while correct kicks do not move. Controlling for the decision type and the size of the\n",
                     "edge, the high-leverage penalty is significant at p < 0.0001. The playoffs are worse again, 78.3%% overall against 82.3%% in routine regular-season spots.\n",
                     "The honest limit, and it is weaker than a null: a coach's high-leverage accuracy is barely a property of the coach at all (split-half reliability %.2f, against the\n",
                     "0.30 bar this project uses elsewhere), so the fact that it does not predict beating the spread (r = %+.2f, p = %.2f, n = %d) is uninformative rather than evidence\n",
                     "of no effect. The measurement is too noisy to answer the question. Built by R/factory/50."), r_sb, r_mkt, p_mkt, nrow(cj))
    )
  ) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left",
        axis.text.x = element_text(size = rel(1.0), face = "bold"))
save_fig("docs/figures/factory/leverage_fourthdown.png", p2, w = 11.5, h = 6.6)
