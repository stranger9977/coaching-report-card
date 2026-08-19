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
| Defensive callers run hot, nobody mixes | previous blitz raises the next-blitz residual ~3.6pp league-wide net of situation; no DC anti-correlates; the identity persists odd-vs-even seasons at r = 0.78 |
| Blitz guessability does not cost anything | r = +0.08 with EPA allowed, CI well across zero; the offensive predictability result has no defensive twin |
| Macdonald calls one game everywhere | situational range 33rd of 42, below league blitz rate in all six cuts; BAL 2023 second-most static personnel of 121 coordinator-seasons, SEA at median |
| The U-shape theory is dead | no U on any predictability axis; offense monotonic, defense flat; structure is universal among the elite, Johnson alone combines it with unexploitability |
| Shell rotation is rising | 29.1% of dropbacks (2022) to 32.3% (2025), +1.2pp/season, CI clear of zero |
| But rotation is not provably worth it | -0.01 EPA to the offense after situation controls, CI spans zero in every split |
| Macdonald's Super Bowl trap | the Drake Maye pick came on a rotated shell (Q4 8:49); SEA 29, NE 13 verified vs nflverse |
| Cowardly and brave boards | Harbaugh/Patricia/Ryans vs Kingsbury/McDermott/LaFleur; go/kick rate clears the trait bar (r = 0.76) |
| McDaniels for the animator | middling on clear-cut fourth downs, 1 of 17 in the razor-thin band |
| SEA two-high, verified | 85.5% shown (2nd, PHI leads), played only 56.2%: a 29-point spin-down, 6th in rotation |
| Rams out of 13, verified | his .50/.07 EPA confirmed in CI; the edge lives vs stacked boxes (72% of their 13 snaps) |
| Nick's 2pt hunch dies | 28% of coaches are brave on 4th and conservative on 2pt (Reid, Campbell, LaFleur); r = 0.10 |
| The 2pt card is a late-game religion | obeyed 99.6% in Q4/OT, 48.6% before |
| Coach worth: the ladder | elite tier +2.5-4.2 vs pedigree, under 3 vs payroll, ~1 win vs the market; the 3-4-win coach only exists on the loosest ruler |
| Rams 13: different playbook | man/gap runs 40.6% vs league 17.7%, zero power all season, 89% under center, PA fakes inside zone at 4x league |
| Kyle's heavy is 21, not 13 | 8 snaps of 13 all year; out of 21 he throws MORE than league and play-fakes less; 69% extra-blocker runs, 2.2x the league |
| The league really is bad out of 13 | league-13 passing negative, running well underwater; LA positive on both (p = 0.0003 / 0.01) |
| Shanahan did not adapt in 2025 | all three same-caller transitions at the 4th-17th pctile of league drift; McVay's pivot at the 98th |
| McVay's pictures are few and leaky | variety rank 32/36, leak rank 1/36, 55% bunch; his motion (rank 6/37 model-scramble) buys the leak back |
| Shanahan: deception by menu | picture variety rank 3/36, leak mid-pack; below-median motion effect |
| Johnson's coarse-look camouflage does not repeat on the rich picture | above-median variety AND leak per-player; R/14 vs R/25 contrast disclosed |
| Scheme and execution, split | Shanahan exec #2 / design #5, Johnson #5/#3, McVay median on both; same-play grades rejected as 11x outcome-contaminated |
| The mental-tax theory dies on the clock too | 0 of 7 QB outcomes move after controls; the PA prediction runs backwards (p = 0.0003) |
| Macdonald's pressure is scheme | #2 of 41 in unblocked rushers, #24 winning blocks, #26 blitz rate; a free rusher is worth 3x a won rush |
| The Hawks ledger | 1st above pedigree, 1st above payroll, 2nd above Madden, 3rd vs market; halftime mid-pack is the one nothing-special row |
| CMC was the WR1 | 143 targets, +46% over Jennings; 1,029 receiving yards |
| Jones was as good as Purdy | EPA/dropback +0.009 in Jones's favor, p = 0.94, on 334 vs 363 dropbacks; suggestive, not proof |
| Belichick's +4.16 is a Brady effect | with Brady +4.9; post-Brady (29-38) +0.5 to -0.4 depending on QB control, at/below the +1.01 market floor |

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
| `R/17_ushape.R` | ushape_test.png, ushape_structure.png |
| `R/18_shell_rotation.R` | rotation_trend.png, rotation_worth.png, rotation_dcs.png, macdonald_reel.csv |
| `R/19_brave_cowardly.R` | cowardly.png, brave.png |
| `R/20_sea_twohigh_verify.R` | sea_twohigh.png |
| `R/21_rams_13.R` | rams_13.png, rams_13_box.png |
| `R/22_two_point.R` | two_point.png |
| `R/23_coach_war.R` | coach_war.png |
| `R/24_heavy_playbook.R` | rams_13_playbook.png, kyle_heavy.png, shanahan_2025.png |
| `R/25_presnap_structure.R` | presnap_variety.png, presnap_lie.png |
| `R/26_scheme_vs_execution.R` | scheme_vs_execution.png |
| `R/27_rotation_clock.R` | rotation_clock.png |
| `R/28_free_rushers.R` | free_rushers.png |
| `R/29_hawks_and_kyle.R` | hawks_ledger.png, sf_wr1_qb.png |
| `R/30_brady_confound.R` | brady_confound.png |

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
