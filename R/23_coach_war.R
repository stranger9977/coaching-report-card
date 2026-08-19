# =============================================================================
# 23_coach_war.R
#
# Michael's hunch, verbatim: "I wonder how much a good OC/coach is worth.
# It's so entangled that it's obviously very hard to know or tell...but it
# certainly feels like there's an argument that they are the second most
# valuable person on the roster behind QB." And: "Like it 'feels' as if
# McDonald, Schanahan, and McVay are responsible for 3-4 wins a year. A WAR
# of 3-4 for the outlier guys."
#
# This script builds no new model. It assembles the three coach-value
# measures this repo already computed independently and puts Michael's 3-4
# win guess up against the measured ranges, loosest-controlled to
# strictest-controlled:
#   - data/derived/coach_vs_roster.csv (R/01): coach random effect from a
#     mixed model of point differential on DRAFT-CAPITAL roster talent (+ QB
#     draft value), wins/season, 2003-2025. Controls only for the talent a
#     draft slot implies, nothing else: not scheme, not luck, not injuries,
#     not opponent quality.
#   - data/derived/coach_vs_contracts.csv (R/06): same design, talent
#     measured by actual contract value (APY % of cap) instead of draft
#     slot, wins/season, 2012-2025. A better talent proxy than draft capital
#     (a real second contract prices a bust or a breakout; a draft slot
#     never updates), but still controls only for roster pay + QB pay.
#   - data/derived/coach_market.csv (R/02): career wins above the closing
#     point spread's implied win probability, per 17 games, all-time. The
#     strictest measure by construction: the spread already prices in
#     everything the market can see, including the coach himself, so a
#     genuinely great coach facing a fair market should land close to zero.
#     Anything a coach shows here is a LOWER BOUND on his true value, not an
#     estimate of it.
#
# Eligibility floors are each source script's own, reused as-is: >= 4 seasons
# for the two talent models (R/01, R/06's MIN_SEASONS_COACHED), >= 16 games
# for the market model (R/02's min_games_career). The market model's ELITE
# tier additionally requires clearing R/02's own 95% luck cone (|z| > 1.96)
# AND >= 100 games, because the raw top of that measure by point estimate
# alone is small-sample noise (Liam Coen, 17 games, +3.85 wins/17 -- the
# single highest number in this entire script, and not a finding).
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out:
#   data/derived/coach_war.csv   -- assembled long table, all three measures
#   docs/figures/coach_war.png   -- the comparison chart
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(ggrepel)
})

source("R/lib/theme_coach.R")

MIN_SEASONS       <- 4    # R/01 and R/06's own leaderboard floor
MIN_GAMES_MARKET  <- 100  # career-length floor for the market ELITE tier only
                            # (raw eligibility from R/02 stays >= 16 games)

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------
# 1. Load the three source tables, standardize to one schema.
# -----------------------------------------------------------------------
roster    <- read.csv("data/derived/coach_vs_roster.csv", stringsAsFactors = FALSE)
contracts <- read.csv("data/derived/coach_vs_contracts.csv", stringsAsFactors = FALSE)
market    <- read.csv("data/derived/coach_market.csv", stringsAsFactors = FALSE)

measure_levels <- c(
  "Market model (career, prices the coach in)",
  "Contract-talent model (2012-2025)",
  "Draft-talent model (2003-2025)"
)

# Every coach who ever shows up in a source table, NOT pre-filtered to the
# eligibility floor. Macdonald (2 seasons everywhere) needs to appear on all
# three rows with a short-sample caveat, not be silently dropped from two of
# them; the "eligible" column carries each source script's own floor instead
# so the distribution/elite-tier work below can filter on it explicitly.
roster_long <- roster %>%
  transmute(measure = measure_levels[3], coach = head_coach,
            wins_per_season, sample = seasons, sample_unit = "seasons",
            eligible = seasons >= MIN_SEASONS)

contracts_long <- contracts %>%
  transmute(measure = measure_levels[2], coach = head_coach,
            wins_per_season, sample = seasons, sample_unit = "seasons",
            eligible = seasons >= MIN_SEASONS)

market_long <- market %>%
  transmute(measure = measure_levels[1], coach,
            wins_per_season = rate_per17, sample = games, sample_unit = "games",
            eligible, outside_cone, z)

coach_war <- bind_rows(roster_long, contracts_long, market_long) %>%
  mutate(measure = factor(measure, levels = measure_levels))

write.csv(coach_war, "data/derived/coach_war.csv", row.names = FALSE)
cat(sprintf("wrote data/derived/coach_war.csv (%d rows, %d eligible)\n",
            nrow(coach_war), sum(coach_war$eligible)))

# -----------------------------------------------------------------------
# 2. Elite tier per measure.
#    Roster/contract: top 5 of the >= 4-season pool (no SE column exists to
#    do a proper significance cut on a raw lmer random effect, so top-N is
#    the honest option available).
#    Market: R/02's own statistical bar (95% luck cone) restricted to
#    careers of real length, because that measure DOES carry an SE and the
#    unrestricted top is dominated by 17-34 game samples.
# -----------------------------------------------------------------------
elite_roster    <- roster_long    %>% filter(eligible) %>% slice_max(wins_per_season, n = 5)
elite_contracts <- contracts_long %>% filter(eligible) %>% slice_max(wins_per_season, n = 5)
elite_market <- market_long %>%
  filter(eligible, outside_cone, wins_per_season > 0, sample >= MIN_GAMES_MARKET) %>%
  arrange(desc(wins_per_season))

elite_tiers <- bind_rows(
  tibble(measure = measure_levels[3], lo = min(elite_roster$wins_per_season),
         hi = max(elite_roster$wins_per_season), def = "top 5, >= 4 seasons"),
  tibble(measure = measure_levels[2], lo = min(elite_contracts$wins_per_season),
         hi = max(elite_contracts$wins_per_season), def = "top 5, >= 4 seasons"),
  tibble(measure = measure_levels[1], lo = min(elite_market$wins_per_season),
         hi = max(elite_market$wins_per_season),
         def = sprintf("beats market at 95%% conf., >= %d games", MIN_GAMES_MARKET))
) %>%
  mutate(measure = factor(measure, levels = measure_levels))

# -----------------------------------------------------------------------
# 3. Named coaches Michael called out, plus Belichick/Reid as the
#    established outliers to compare them against. Macdonald's HC sample
#    is 2 seasons everywhere; every mention of him below carries that label.
# -----------------------------------------------------------------------
named_coaches <- c("Bill Belichick", "Andy Reid", "Sean McVay",
                    "Kyle Shanahan", "Mike Macdonald")

highlight <- coach_war %>%
  filter(coach %in% named_coaches) %>%
  mutate(label = ifelse(coach == "Mike Macdonald", "Macdonald (2 seasons)", coach))

# -----------------------------------------------------------------------
# 4. Print block: elite-tier ranges, named-coach values, the verdict.
# -----------------------------------------------------------------------
cat("\n================ ELITE TIER PER MEASURE ================\n")
for (i in seq_len(nrow(elite_tiers))) {
  cat(sprintf("%-45s %+.2f to %+.2f wins/season  (%s)\n",
              as.character(elite_tiers$measure[i]), elite_tiers$lo[i], elite_tiers$hi[i],
              elite_tiers$def[i]))
}

cat("\n================ NAMED COACHES, BY MEASURE ================\n")
for (nm in named_coaches) {
  cat(sprintf("\n%s:\n", nm))
  rows <- highlight %>% filter(coach == nm) %>% arrange(measure)
  for (j in seq_len(nrow(rows))) {
    cat(sprintf("  %-45s %+.2f wins/season  (%d %s)\n",
                as.character(rows$measure[j]), rows$wins_per_season[j],
                rows$sample[j], rows$sample_unit[j]))
  }
}

verdict <- paste(
  "VERDICT: Michael's 3-4-win claim clears the bar on exactly one of three",
  "measures, the loosest-controlled one. Draft-talent model: Belichick",
  "(+4.16) clears it and Dungy (+3.03) sits inside it, but that model",
  "controls for nothing but a draft slot. Add real contract-value talent as",
  "the control and the ceiling drops under 3 (Belichick +2.93, nobody",
  "crosses 3). Price the coach into the actual betting market, the way a",
  "book does every week, and the sustained elite tier is +0.96 to +1.34",
  "wins/season, about a third of the 3-4 claim. McVay, Shanahan, and",
  "Macdonald, the three coaches Michael named, never clear +1.7 wins/season",
  "on ANY measure; Shanahan's career market number is slightly NEGATIVE",
  "(-0.26); Macdonald's market number looks huge (+2.37) but is 2 seasons",
  "and not the kind of sample this project treats as signal."
)
cat("\n================ VERDICT ================\n")
cat(strwrap(verdict, width = 78), sep = "\n")
cat("\n")

# -----------------------------------------------------------------------
# 5. Chart.
# -----------------------------------------------------------------------
y_pos <- setNames(seq_along(measure_levels), measure_levels)  # market=1 (bottom) .. draft=3 (top)

coach_war_y  <- coach_war  %>% filter(eligible) %>% mutate(y = y_pos[as.character(measure)])
elite_tiers_y <- elite_tiers %>% mutate(y = y_pos[as.character(measure)])
highlight_y  <- highlight  %>% mutate(y = y_pos[as.character(measure)])

band_fill      <- "#FDB863"  # Michael's claim under test: warm, single accent
elite_colour   <- ink_title  # the honest ceiling: near-black, not a "finding" hue
highlight_fill <- "#045A8D"  # reused from coach_vs_contracts.png's active-coach blue

band_df <- data.frame(xmin = 3, xmax = 4, ymin = 0.5, ymax = 3.5)

p <- ggplot() +
  geom_rect(data = band_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = band_fill, alpha = 0.28) +
  # NOTE: geom_baseline() draws its reference at y = 0, meant for charts where
  # the value is on the y-axis. Here wins/season is on the X axis, so the
  # "coach performed exactly as expected" reference has to be a vline instead.
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_jitter(data = coach_war_y, aes(x = wins_per_season, y = y),
              height = 0.16, width = 0, colour = "grey60", size = 1.5, alpha = 0.45) +
  geom_segment(data = elite_tiers_y,
               aes(x = lo, xend = hi, y = y + 0.30, yend = y + 0.30),
               colour = elite_colour, linewidth = 1.7, lineend = "round") +
  geom_text(data = elite_tiers_y,
            aes(x = hi, y = y + 0.30, label = sprintf(" elite: %+.2f to %+.2f", lo, hi)),
            hjust = 0, vjust = 0.5, size = 3.1, fontface = "bold", colour = elite_colour) +
  geom_point(data = highlight_y, aes(x = wins_per_season, y = y),
             colour = highlight_fill, size = 2.6) +
  geom_text_repel(data = highlight_y, aes(x = wins_per_season, y = y, label = label),
                   colour = highlight_fill, fontface = "bold", size = 3.2,
                   segment.colour = "grey55", segment.size = 0.3, min.segment.length = 0,
                   nudge_y = -0.34, direction = "both", seed = 23, max.overlaps = Inf,
                   box.padding = 0.3) +
  annotate("text", x = 3.5, y = 3.46, label = "the hypothesis: 3-4 wins/season",
           fontface = "bold", colour = "#B35806", size = 3.3, vjust = 0) +
  scale_y_continuous(breaks = y_pos, labels = names(y_pos), limits = c(0.35, 3.75)) +
  coord_cartesian(xlim = c(-2.6, 4.7), clip = "off") +
  labs(
    title = "The 3-4-win coach shows up only in the measure that controls for the least",
    subtitle = paste0(
      "Three independent estimates of a coach's wins per season above expectation, sorted loosest control\n",
      "(top) to strictest (bottom). Grey dots are every eligible head coach; the black bar is each measure's own elite tier."
    ),
    x = "Wins per season above expectation",
    y = NULL,
    caption = fig_caption(
      "R/01, R/02, R/06 (this repo, not re-modeled here)",
      paste0(
        "Draft/contract-talent models: coach random effect from point differential ~ roster talent + QB pay, min 4 seasons.\n",
        "Market model: career wins above the closing-spread win probability, min 16 games; elite tier restricted to |z| > 1.96\n",
        "AND 100+ games (else Liam Coen at 17 games tops the list). The market number is a LOWER BOUND by construction: the\n",
        "spread already prices in everything it can see, including the coach. The two talent models control only for roster\n",
        "or contract pay plus the starting QB's own pay, nothing else."
      )
    )
  ) +
  theme_coach(grid = "none") +
  theme(
    axis.text.y = element_text(face = "bold", colour = ink_title, size = rel(0.85)),
    plot.margin = margin(10, 90, 8, 10)
  )

save_fig("docs/figures/coach_war.png", p, w = 12.5, h = 7.5)
