# The Coaching Report Card

Working board and analysis for the next video with Michael MacKelvie: how do you
actually grade NFL head coaches and play-callers?

Live board: https://stranger9977.github.io/coaching-report-card/

Companion to [ghost-town](https://github.com/stranger9977/ghost-town) and
hometown-effect, same working setup: R scripts write PNGs to `docs/figures/`,
one static `docs/index.html` carries the findings as a card board (Michael's
verbatim ask, what the data says, the chart, the catch), GitHub Pages serves
`main:/docs`.

Most of the charts were built in July in a separate local research repo
(`nfl-analysis`) before this project had a name. Those are copied into
`docs/figures/` as-is and credited card by card. The numbered scripts here are
the new work.

## What holds up so far

| Finding | Number |
|---|---|
| 2025 Seahawks beat the market | 14 wins vs 10.8 expected, 2nd-biggest overperformance of 2025 |
| Beating the spread is not a repeatable coach skill | r = -0.03 season-to-season, n = 654 coach-season pairs |
| Coaches outside the market luck cone | 10 of 157, vs ~8 expected by chance |
| Best play-callers are MORE predictable | naive r = +0.33 flips to r = -0.37 situation-controlled |
| Andy Reid's predictability | ~3 extra guessed calls per 100 plays, best offense of the era |
| Pre-snap disguise excuse | busts; effect collapses under QB control |
| Establish the run | pass-after-run vs fresh pass: -0.015 EPA, zero in all 3 metrics |
| Steelman survivors | light boxes are earned by passing; no wear-down; cold is the one kernel of truth |
| Andy Reid timeout myth | dies with timeouts in 4 of 61 close losses vs 15% league rate |
| Norv Turner timeout sin | 11 of 31 close losses with timeouts unused (35%, p = 0.003) |
| Icing the kicker | -2.9 pts make probability, 95% CI crosses zero; 20 of 21 coaches inside luck cone |
| Pete Carroll icing outlier | the fluke screening 21 coaches predicts, on 8 kicks |
| Wind savvy | long-FG attempt rate 50% calm to 34% at 11-15 mph; make rate on attempts flat |
| Blitz autopilot | blitz rate uncorrelated with blitz payoff across coordinators (r = 0.06) |
| Motion vs man coverage | +0.03 EPA, CI crosses zero; no deterrence effect |

(New-build rows for roster talent, Madden, and halftime adjustments get added
when those scripts land.)

## Scripts

Run from repo root. Each script prints its headline numbers and writes its
figures and derived CSVs.

| Script | Builds |
|---|---|
| `R/01_roster_talent.R` | coach_vs_roster.png, seahawks_2025.png (in progress) |
| `R/02_market_expectation.R` | market_funnel.png, market_2025.png |
| `R/03_madden.R` | madden_scatter.png, madden_coaches.png (in progress) |
| `R/04_halftime_adjustments.R` | halftime_cone.png, halftime_persistence.png (in progress) |

```sh
for f in R/0*.R; do Rscript "$f"; done
```

`R/lib/theme_coach.R` is the shared chart theme (copied from ghost-town's
theme_ghost.R, functions renamed).

## Data sources

- nflverse play-by-play and schedules, 1999-2025. The schedules file
  (scores, coaches, closing spread lines) is read from the local nfl-analysis
  repo copy; scripts fall back to `nflreadr` when it is absent.
- Play-caller attribution: samhoppen/NFL_public, offensive and defensive
  play-caller per team-game, 1999-2025.
- FTN charting 2022-2024, NGS participation 2016-2023 (charts built in
  nfl-analysis).
- Madden ratings: https://github.com/theedgepredictor/nfl-madden-data
  (in progress).

## The measurement problem, stated up front

Two structural traps every coach metric has to answer for, both carded on the
board:

1. The market already prices the coach. Wins above the spread is a lower bound
   on skill, not a measure of it; a coach exactly as good as his reputation
   shows up at zero.
2. Screening many coaches at a 95% threshold hands ~5% of them to luck. Every
   leaderboard on the board ships with its luck cone for that reason.

## Open problems

- Roster-talent baseline: draft capital misses UDFA stars and aging; the model
  ships with that caveat until a better talent measure lands.
- Halftime adjustments: repeatability test in progress; the audience belief is
  strong and the data may not support it.
- "Adjustments" beyond halftime (in-series, week-to-week) is unmeasured.
- The composite report-card table (one grade per coach) needs a weighting
  decision, which is an editorial call, not a statistical one.
