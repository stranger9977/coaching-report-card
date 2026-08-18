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
| 2025 Seahawks beat the market | 14 wins vs 10.8 expected, 3rd-biggest overperformance of 2025 |
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
| Halftime adjustments repeat | split-half r = 0.48, 44 of 159 coaches outside the noise cone (~8 expected) |
| But they are mostly team quality | adjustment score correlates 0.81 with plain point margin |
| Draft pedigree explains ~nothing | r = -0.005 / -0.010 / -0.027 under the JJ, Stuart, and OTC charts, 736 team-seasons |
| Madden sees what draft charts cannot | 2025: Madden launch r = 0.39 with point diff, all three draft charts ~0 |
| Payroll is a real talent meter | contract value (top-25 APY % of cap) vs point diff: r = 0.30, 2012-2025 |
| Seattle 2025 vs its payroll | 30th of 32 by paid talent, biggest beat of the contract model; Darnold 15th of 32 QBs by cap share |
| Wins above roster talent | Belichick +4.2/season on top; Gase, Bradley, Saleh at the bottom |
| Seattle 2025 vs its pedigree | roster 19th of 32 by draft value, biggest positive outlier in the league |
| Madden knows something, weakly | launch talent explains 19% of win variance (r = 0.43), 2017-2025 |
| Seattle 2025 vs the video game | rated 23rd of 32, priced at 7.5 wins, won 14 |
| Ben Johnson's sequencing is unique | least sequence-predictable of 73 careers since 2015 (z = -2.3), every season, both teams |
| Nobody cashes the setup play | 8 of 95 callers clear their own noise band, ~5 expected by chance; Johnson mid-pack |
| The opening script is a weak trait | year-to-year r = 0.15, CI barely clears zero; Johnson 19th of 64, one rookie-year blip |
| Johnson's one outlier ingredient | early-down play action 39.5% (2nd of 35); trick plays carry the sample's best payoff |
| Play action works cold league-wide | median after-run-minus-cold PA gap is negative; the setup story fails again |
| Motion blinds the model for McVay only | run/pass gap rank 6 of 37, CI clear of zero; Shanahan below median; on play action the whole league benefits |
| Defensive callers run hot, nobody mixes | previous blitz raises next-blitz odds ~3pp league-wide beyond situation; no DC anti-correlates |
| Blitz guessability does not cost anything | r = +0.12 with EPA allowed, CI crosses zero; the offensive predictability result has no defensive twin |
| Macdonald calls one game everywhere | situational range 59th of 71; BAL 2022 most static personnel of 128 team-seasons, SEA at median |

## Scripts

Run from repo root. Each script prints its headline numbers and writes its
figures and derived CSVs.

| Script | Builds |
|---|---|
| `R/01_roster_talent.R` | coach_vs_roster.png, seahawks_2025.png |
| `R/02_market_expectation.R` | market_funnel.png, market_2025.png |
| `R/03_madden.R` | madden_scatter.png, madden_coaches.png |
| `R/04_halftime_adjustments.R` | halftime_cone.png, halftime_persistence.png |
| `R/05_caller_vs_qb.R` | caller_qb_stability.png, caller_qb_control.png (rebuilt from nfl-analysis scratch caches; reproduces the original numbers exactly) |
| `R/06_contract_talent.R` | contract_2025.png, coach_vs_contracts.png |
| `R/09_bj_script.R` | bj_script_edge.png, bj_script_guess.png |
| `R/13_bj_sequencing.R` | bj_seq_lift.png, bj_setup_cash.png |
| `R/14_bj_ftn_signature.R` | bj_ingredients.png, bj_pa_cold.png, bj_same_look.png |
| `R/15_motion_encryption.R` | motion_encryption.png, motion_pa.png |
| `R/16_def_sequencing.R` | def_seq_lift.png, def_situational.png, def_shapeshift.png |

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
- Contracts: OverTheCap via nflreadr::load_contracts(). Coverage supports a
  top-25-per-team build from 2012 on (measured, not assumed: the minimum
  matched players per team-roster first clears 25 in 2012). ~8% of contract
  rows lack a gsis_id; a name+position fallback recovers most.
- Madden ratings: https://github.com/theedgepredictor/nfl-madden-data,
  launch ratings rebuilt per season with nflverse team codes, 2017-2025 used.
  Warning for anyone retracing: the maddenratings.weebly.com "Madden 25" page
  is the 2013 anniversary game, not the 2024-season one.

## The measurement problem, stated up front

Two structural traps every coach metric has to answer for, both carded on the
board:

1. The market already prices the coach. Wins above the spread is a lower bound
   on skill, not a measure of it; a coach exactly as good as his reputation
   shows up at zero.
2. Screening many coaches at a 95% threshold hands ~5% of them to luck. Every
   leaderboard on the board ships with its luck cone for that reason.

## Open problems

- Roster-talent baseline: draft capital turned out to explain ~0% of team
  performance (and the recent-drafts variant runs negative, since losing earns
  high picks). The coach model ships with that stated plainly; the upgrade is
  a performance-based talent meter (snap-weighted AV, OTC contract value, or
  PFF grades).
- nflreadr data gotchas hit during the build: load_rosters() game_type is
  unreliable for REG filtering (use distinct players per season roster);
  draft_number has a 2016-2019 missing-data spike (backfill from
  load_players()); the 2025 schedules/player-stats data misattributes ~97 IND
  dropbacks to Philip Rivers's gsis_id.
- Halftime adjustments: repeatability test in progress; the audience belief is
  strong and the data may not support it.
- "Adjustments" beyond halftime (in-series, week-to-week) is unmeasured.
- The composite report-card table (one grade per coach) needs a weighting
  decision, which is an editorial call, not a statistical one.
