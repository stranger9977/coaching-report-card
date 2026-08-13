# =============================================================================
# 08_by_the_book.R
#
# Michael's new ask, 13 Aug:
#   "Another thought - what about coaches that are the least/most by the
#    analytics book? As in, what coach does what he should and what coach does
#    not do what he should?"
#
# Fourth down is the cleanest place to answer it, because there is a defensible
# right answer on every single snap and the decision is entirely the head
# coach's. nfl4th (Ben Baldwin's model, the same one the public 4th-down bots
# run on) gives `go_boost`: the win probability a team gains, in percentage
# points, by going for it instead of taking the better of punt and field goal.
# Positive means go. Negative means kick.
#
# The measure here is deliberately narrow. Only the OBVIOUS calls count, the
# ones where go_boost is at least 1.5 points either way, because those are the
# spots where nobody serious disagrees. Coaches are then scored on how often
# they did the obvious thing. That keeps this from being an argument about
# model assumptions in the close calls.
#
# It also pairs with the predictability work in R/07. Nick's blackjack framing
# to Michael was "just do what the book says" and this is the literal version:
# there is a book, and we can count who follows it.
#
# Filters follow the usual 4th-down-bot convention: regulation only, at least a
# minute left in the half, win probability between 5% and 95%, so end-of-game
# desperation and blowouts do not pollute the rate.
#
# Source: ~/stranger9977/nfl-analysis/data/play_by_play_20{18..25}.csv.gz
# (local nflverse) and data/games.csv for head-coach attribution.
#
# Out: docs/figures/book_leaderboard.png
#      docs/figures/book_by_type.png
#      data/derived/fourth_down_decisions.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(nfl4th); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
SEASONS <- 2018:2025
CACHE <- "data/derived/fourth_down_probs.rds"
dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)

if (!file.exists(CACHE)) {
  # Two things nfl4th needs that a naive 4th-down subset destroys:
  #   home_opening_kickoff, which nflfastR derives from the game's FIRST
  #     possession (if the away team has the ball first, the home team kicked
  #     off), so it has to be computed before filtering to fourth downs; and
  #   quarter_seconds_remaining, which prepare_df() uses to rebuild the clock.
  # So keep every column and only filter rows.
  raw <- rbindlist(lapply(SEASONS, function(s) {
    d <- fread(file.path(NFLA, sprintf("data/play_by_play_%d.csv.gz", s)),
               showProgress = FALSE)
    d[, home_opening_kickoff :=
        as.integer(posteam[!is.na(posteam)][1] != home_team[1]), by = game_id]
    d[down == 4 & qtr <= 4 & !is.na(yardline_100) & !is.na(posteam)]
  }), fill = TRUE)
  # nfl4th expects a `type` column of reg/post
  raw[, type := ifelse(season_type == "POST", "post", "reg")]
  cat("4th downs loaded:", nrow(raw), "\n")
  probs <- as.data.table(nfl4th::add_4th_probs(raw))
  saveRDS(probs, CACHE)
}
p <- as.data.table(readRDS(CACHE))

# --- what the coach actually did --------------------------------------------
p[, went := as.integer(play_type %in% c("run", "pass") |
                       rush_attempt == 1 | pass_attempt == 1)]
p[, kicked := as.integer(play_type %in% c("punt", "field_goal") |
                         punt_attempt == 1 | field_goal_attempt == 1)]
d <- p[!is.na(go_boost) & (went == 1 | kicked == 1) &
       half_seconds_remaining >= 60 &
       vegas_wp >= 0.05 & vegas_wp <= 0.95]

CUT <- 1.5
d[, verdict := fifelse(go_boost >= CUT, "should go",
              fifelse(go_boost <= -CUT, "should kick", "close call"))]
d <- d[verdict != "close call"]
d[, correct := as.integer((verdict == "should go" & went == 1) |
                          (verdict == "should kick" & kicked == 1))]

cat(sprintf("\nclear-cut 4th downs %d-%d: %s (%s should go, %s should kick)\n",
            min(SEASONS), max(SEASONS), format(nrow(d), big.mark = ","),
            format(sum(d$verdict == "should go"), big.mark = ","),
            format(sum(d$verdict == "should kick"), big.mark = ",")))
cat(sprintf("league obeys the book on %.1f%% of them (%.1f%% of should-gos, %.1f%% of should-kicks)\n",
            100*mean(d$correct),
            100*mean(d$correct[d$verdict == "should go"]),
            100*mean(d$correct[d$verdict == "should kick"])))

# --- head coach attribution --------------------------------------------------
gm <- fread(file.path(NFLA, "data/games.csv"),
            select = c("game_id","home_team","home_coach","away_coach"))
d <- merge(d, gm, by = "game_id", all.x = TRUE, suffixes = c("", "_gm"))
d[, coach := ifelse(posteam == home_team_gm, home_coach, away_coach)]
d <- d[!is.na(coach) & coach != ""]
write_csv(as.data.frame(d[, .(game_id, season, coach, posteam, ydstogo, yardline_100,
                              go_boost, verdict, went, correct)]),
          "data/derived/fourth_down_decisions.csv")

MIN_N <- 60
co <- d[, .(n = .N, obey = 100*mean(correct),
            n_go = sum(verdict == "should go"),
            go_rate = 100*mean(went[verdict == "should go"]),
            kick_ok = 100*mean(correct[verdict == "should kick"])),
        by = coach][n >= MIN_N]
setorder(co, -obey)
cat(sprintf("\ncoaches with >= %d clear decisions: %d\n", MIN_N, nrow(co)))
cat("\n--- most by the book ---\n"); print(head(co, 10))
cat("\n--- least by the book ---\n"); print(tail(co, 10))

lg_obey <- 100*mean(d$correct)

# --- chart 1: the leaderboard ------------------------------------------------
top <- rbind(head(co, 12), tail(co, 12))
top[, side := rep(c("Most by the book", "Least by the book"), each = 12)]
top[, coach := factor(coach, levels = rev(coach))]

p1 <- ggplot(top, aes(obey, coach, fill = side)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.0f%%", obey)), hjust = -0.18,
            size = 3.2, fontface = "bold", colour = "grey25") +
  geom_vline(xintercept = lg_obey, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.4) +
  scale_fill_manual(values = c("Most by the book" = "#2B8CBE",
                               "Least by the book" = "#D55E00")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  facet_wrap(~side, scales = "free_y", ncol = 2) +
  labs(
    title = "Who actually does what the fourth-down math says",
    subtitle = sprintf("Share of clear-cut fourth downs handled correctly, %d to %d. Dashed line is the league at %.0f%%.",
                       min(SEASONS), max(SEASONS), lg_obey),
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse play-by-play; decision model from the nfl4th package (Baldwin)",
      sprintf("Head coaches with at least %d clear-cut fourth downs. Regulation only, at least a minute left in the half, win probability between 5%% and 95%%.", MIN_N),
      paste0("\nA fourth down counts as clear-cut when going for it is worth at least 1.5 win-probability points more than kicking, or at least 1.5 points less. Close calls are dropped\n",
             "on purpose, so this is not an argument about model assumptions at the margin. It is only the plays where nobody serious disagrees. Built by R/08."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "none", strip.text = element_text(size = rel(1.0)))

save_fig("docs/figures/book_leaderboard.png", p1, w = 12.5, h = 7.2)

# --- chart 2: the asymmetry --------------------------------------------------
# Almost all disobedience is one-directional: coaches kick when they should go.
byv <- d[, .(obey = 100*mean(correct), n = .N), by = verdict]
cat("\n--- obedience by direction ---\n"); print(byv)

# A scatter is the wrong shape for this: kick-correctness barely varies (every
# coach sits between 96% and 100%), so the horizontal axis does no work and all
# the points pile into one corner. The finding is the GAP between a coach's two
# obedience rates, so draw the gap itself: one row per coach, two dots, joined.
sc <- co[, .(coach, go_rate, kick_ok, n_go)]
setorder(sc, go_rate)
sc[, coach := factor(coach, levels = coach)]
long <- rbind(
  sc[, .(coach, kind = "On the obvious kicks", v = kick_ok)],
  sc[, .(coach, kind = "On the obvious go-for-its", v = go_rate)]
)
lg_kick <- 100*mean(d$correct[d$verdict == "should kick"])
lg_go   <- 100*mean(d$correct[d$verdict == "should go"])

p2 <- ggplot() +
  geom_segment(data = sc, aes(x = go_rate, xend = kick_ok, y = coach, yend = coach),
               colour = "grey80", linewidth = 0.9) +
  geom_point(data = long, aes(v, coach, colour = kind), size = 2.6) +
  scale_colour_manual(values = c("On the obvious kicks" = "#2B8CBE",
                                 "On the obvious go-for-its" = "#D55E00")) +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(15, 104),
                     breaks = seq(20, 100, 20)) +
  labs(
    title = "Every coach is near perfect on the obvious kicks and a coin flip on the obvious go-for-its",
    subtitle = sprintf("Each row is a head coach. The gap is the whole story: leaguewide %.0f%% correct when the math says kick, %.0f%% when it says go.",
                       lg_kick, lg_go),
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse play-by-play; nfl4th decision model",
      sprintf("Head coaches with at least %d clear-cut fourth downs, %d to %d, sorted by how often they correctly go.", MIN_N, min(SEASONS), max(SEASONS)),
      paste0("\nThe error is almost entirely one-directional. Coaches essentially never gamble when the math says kick, and they kick about half the time when it says gamble.\n",
             "Nobody's blue dot is below 96%. Nobody's orange dot reaches 71%. Built by R/08."))
  ) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left",
        axis.text.y = element_text(size = rel(0.82)))

save_fig("docs/figures/book_by_type.png", p2, w = 11.5, h = 9)
