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
| The YAC factory | Shanahan +0.76 YAC/completion over expected, 1st of 37, at ordinary throw depth (16th); Johnson 3rd, McVay 11th |
| Inverted run geometries | Shanahan 46% outside zone (3rd) vs McVay 50% man; the bounce folklore is null for everyone |
| The mismatch machine | CMC covered by LB/S 81% (league RB 82%, SF WRs 34%) at WR1 volume; hunting soft coverage pays at every depth |
| Matchup hunting ranks | Johnson 1st of 41, Shanahan 5th, McVay 37th |
| The sequencing rumor is McVay's | Shanahan holds personnel 28th of 37 (below league); McVay 1st at 84% snap-to-snap |
| Watch-then-strike fails in reverse | repeated looks get MORE repetitive league-wide; nobody gains EPA on later showings |
| SEA defense read: 3 of 4 confirmed | nickel 5th (2nd in 2025), blitz 11th-fewest, explosives 4th-fewest; zone dead-average pooled |
| Why the rare blitz is dangerous | 1 in 4 SEA blitzes springs a free rusher (45% above league); those allow -0.996 EPA, the rest are ordinary |
| "Always wide open," checked | Shanahan above median on both openness proxies, top-third on neither; last 3 years comp-over-expected rank 9 (+3pp) |
| Same system, easy throws for both | Purdy +4.7pp and Jones +3.9pp completions over expected, p = 0.81 |
| Hunting and cashing mismatches are different skills | Shanahan #5/#8 (does both), Johnson #1 hunt but median cash, McVay bottom on both |
| The Hudl article, fact-checked | 4 of 11 claims confirm (run-look setup a bullseye), 3 close, 4 contradicted (TE-attached runs backwards; "tied narrowest" holds for McVay only) |
| Rotation's null is uniform | no xpass gradient (interaction p = 0.82); 0 of 15 screened cells confirm vs 0.75 expected by chance; no situation where rotating pays |
| Blitz volume and payoff are independent | r = 0.04 across 41 DCs; Macdonald's rare-but-lethal corner (10th-fewest, danger #1, CI clear) is his own, not a trend |
| "Boot off wide zone" is folklore | outside-zone rate vs rollout rate r = -0.01 across 37 callers; Shanahan: 39.9% OZ, 6.9% rollouts (below median) |
| The tree's movement signature is payoff, not volume | adjusted volume: McVay 4, Johnson 20, Shanahan 26, LaFleur 32; all four above median payoff; usage persists r = +0.76 |
| The fake pays, not the movement | rollout without play action: -0.22 EPA/play; with it, indistinguishable from a PA straight drop |
| Movement's clean benefit is sacks | rollout sack rate 3.9-4.0% vs standard 6.1-6.6%; pressure rate barely moves; ball comes out later |
| Pre-snap discipline is a scoreboard, not a skill | vs point diff r = -0.43, but same-coach persistence r = +0.15 (CI spans zero) |
| Cadence is closer to a real weapon | flags drawn persist r = +0.31; Zac Taylor #1: ~86 free yards + 6.5 free first downs/season; McVay top-3 both directions |
| The YAC factory runs on broken tackles | Shanahan's +0.76 = +0.20 clean (5th, CI spans zero) + 0.56 contact (1st); Deebo 10th of 265 in tackles beaten/catch |
| The scheme-space crown is Johnson's | 1st of 37 before contact, 50/50 split, spread across 19 players; channels correlate only r = 0.60 |
| "25 plays seem like 250," composited | 5th-fewest pictures + personnel held 84% (1st) + 4th-most play variety inside stretches + largest interval-backed motion effect |
| McVay's pistol experiment | 0% -> 7.7% (2023-24) -> 10.6% (2024-25), ~2x league both years, killed to 0.7% in 2025-26 |
| Blitzing TIGHTENS coverage league-wide | uncontested rate -1.0pp on blitzes (p=.002), completions -2.8pp vs expected; the hurried throw costs more than the free look |
| SEA openness allowed, split | regular rush: tighter than average (26/39); his blitzes: 11th-most open of 39, the price of the free-rusher designs |
| WR openness by tier | Johnson's WR3 most open in football (1/28), WR2 top-10; McVay WR2 3rd/WR3 5th but converts poorly; Shanahan's openness is TE (#2) + RB (#6), not WRs |
| Ferguson stretches 13p vertically | 18.6-yd avg target depth, 3.1x next Rams TE; 28% explosive; on/off +0.24 vs +0.15 (n=89, underpowered) |
| Blocking-TE specialists barely exist | Saubert ran routes on 71% of pass snaps; 1 of 76 qualified TEs under a 60% route rate |
| Surrender Index x peer test | r=0.43 on all 2,034 punts of 2025-26; comedy metric worships the clock + only sees punts; peer test owns the early-game sins |
| The broken-pocket menu, priced | sack -1.9, throwaway -0.9, quick throw -0.1, extend+throw +0.0, escape +0.4 (24k pressured dropbacks) |
| Caleb's sacks became extends | pressured-snap sack share 33->11%; extend-and-throw 18->34%; throwaways flat 14->17% |
| Worst 4th downs, v2: the peer test | WP-cost version crowned leverage, not sin (old #1 = a punt 0% of coaches skip); rebuilt by peer behavior: Reid FG down big (93% go), Vrabel SB punt (86% go), Ryans 3x |
| Bears PA before Johnson | 34% (4th) -> 27% (5th) -> 17% (30th of 32) -> Johnson: 35% (2nd) |
| Bravery spread, one axis | Kingsbury +10 to Jim Harbaugh -18 on clear-cut go chances; 28-pt spread, repeats r=0.76 |
| Caleb's sacks: pressure stopped converting | sack 9.9->3.4% w/ pressure flat (30.2->30.4); sack-per-pressure 33->11%; PA 17->35%, rollouts 5.9->10.7% |
| Johnson play-fakes 2nd in football | 32% of dropbacks (league 25%); PA +0.25 vs no-PA +0.10, both beat league (+0.09/-0.03) |
| Johnson runs early MORE than most | 50% early-down runs, 12th-most of 45; his early runs -0.03 vs passes +0.15 |
| Macdonald stunts backwards from the league | league stunts more on blitzes (29 vs 25%); he is 3rd-most on regular rushes, 34/39 on blitzes; Ryans the mirror |
| Super Bowl sims, clock-stamped | 3 sims all Q3, all won (Emmanwori 3rd&3 -2.1; Witherspoon 3rd&4 -1.8, 2nd&10 -0.7); SF wk1's three all lost |
| Macdonald stunts, a lot | 42% of passing-down dropbacks (7th of 44; league 37%); sims on passing downs league-average volume (25/48) |
| Sim pressure springs free rushers | standard 3.1% -> sim 8.5% (2.7x) -> blitz 17.1%; 41 of 41 DCs individually confirm; Macdonald's sim: 16.7%, volume rank 19 |
| The Macdonald outlier chart | blitz 22% (10th-fewest of 41) x free-rusher pressure 9.3% (2nd); only Martindale higher, bought with 43% blitzing |
| Motion in held mode: the asked split | McVay 62% motion in one-picture mode, 4th of 36, +20 over held-mode median; everyone motions less when personnel holds |
| FTN-Sumer bridge join works | no shared ids; season/week/off/qtr/clock/down/dist key is 100% unique both sides, 98.4% match |
| Guess the play: McVay is EASY to guess | 33rd of 36 hardest at play level (2 independent play defs); look adds +1.3 vs league +0.7; guessability != quality |
| Shanahan's look misleads a play guess | 3rd-most misleading of 36 (-2.0 pts vs before lineup); Johnson -0.9; hardest to guess: Zac Taylor, Petzing |
| Rotation null, 4th cut: by coordinator | 43 DCs tested, 2 clear 95% (2.1 expected by luck), one each direction; Macdonald -0.09 CI spans zero |
| One game, one look, 11 plays | Rams vs NO 2025-26 wk9: same look 20 snaps, 11 distinct plays, 3 TDs; biggest: SEA 2022-23 wk13 (28/15); BUF 2024-25 wk14 (16/12) |
| McVay motions on 62% of snaps | 4th of 37, league median 46%; panel-4 comparison is his own other 38% |
| The tip, made concrete | 11 pers/3x2/shotgun: 96% pass vs 67% expected; every shotgun look +6..+29, every under-center look -4..-19: the tell is the QB's feet |
| Menu vs wardrobe: quote half-true | McVay menu 3rd-smallest of 36 but wardrobe 27th; Ben Johnson owns the folklore corner (5th + 6th); both traits stable (r=.81/.72) |

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
| `R/31_kyle_yac_cutback.R` | kyle_yac.png, kyle_cutback.png |
| `R/32_matchup_hunting.R` | cmc_mismatch.png, matchup_hunting.png |
| `R/33_hold_and_vary.R` | hold_personnel.png, one_play_many_ways.png |
| `R/34_sea_defense_battery.R` | sea_defense_battery.png |
| `R/35_open_and_mismatch.R` | schemed_open.png, mismatch_category.png |
| `R/36_rotation_xpass.R` | rotation_xpass.png |
| `R/37_blitz_rate_danger.R` | blitz_rate_danger.png |
| `R/38_qb_movement.R` | qb_movement.png |
| `R/39_discipline.R` | discipline.png |
| `R/40_yac_mechanism.R` | yac_mechanism.png |
| `R/41_menu_wardrobe.R` | menu_wardrobe.png |
| `R/42_mcvay_formula.R` | mcvay_formula_diagram.png + _pictures/_hold/_variety/_motion.png |
| `R/43_tip_explainer.R` | tip_explainer.png |
| `R/44_one_look_one_game.R` | one_look_one_game.png |
| `R/45_rotation_by_dc.R` | rotation_by_dc.png |
| `R/46_guess_the_play.R` | guess_the_play.png |
| `R/48_motion_when_held.R` | motion_when_held.png |
| `R/49_sim_pressure.R` | sim_pressure.png |
| `R/51_te_specialists.R` | te_specialists.png |
| `R/52_wr_open_by_tier.R` | wr_open_by_tier.png |
| `R/53_open_allowed.R` | open_allowed.png |
| `R/54_stunt_rate.R` | stunt_rate.png |
| `R/55_stunt_when.R` | stunt_when.png |
| `R/56_sim_reel.R` | sim_reel.png |
| `R/58_bj_file.R` | caleb_sacks.png, bj_pa.png, bj_earlyruns.png |
| `R/59_worst_fourth.R` | worst_fourth.png, bravery_simple.png |
| `R/60_throwaways.R` | pressure_menu.png, caleb_menu.png |
| `R/61_fourth_v2_bears_pa.R` | worst_fourth_v2.png, bears_pa.png |
| `R/62_surrender_cross.R` | surrender_cross.png |
| `R/50_blitz_vs_free.R` | blitz_vs_free.png |

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
