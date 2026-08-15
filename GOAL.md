# The goal

**Build the first public measure of NFL head-coaching value that separates a
coach's decisions from his roster and his players' execution, and prove it is
real by showing it predicts something out of sample.**

Everything in this repo is either a step toward that or a test that failed on
the way. This file is the north star the work gets judged against, not a
description of what has been done.

## Why it is worth attempting

Every existing public answer to "who is a good coach" is one of three things:

1. **Wins.** Which is mostly the roster. The 2007 Patriots would have won games
   with a traffic cone in the headset.
2. **Reputation.** Which is mostly wins, laundered through narrative.
3. **A single analytics stat**, usually fourth-down aggression, which measures
   one visible decision and quietly implies it stands in for judgement
   generally.

Nobody has built the thing in between: a measure that starts from what the
average coach would do in a given situation, prices what this coach did
instead, and then proves the difference is his rather than his quarterback's.
That is a genuinely hard measurement problem, and it is why the answer is
worth having.

## What "done" looks like

Five criteria. All of them are falsifiable, and the project fails honestly if
they cannot be met.

**G1. The baseline is trustworthy.** Every model whose residual is read as
coaching must clear all five rubric rules in `R/factory/lib_factory.R`,
including out-of-sample calibration and slice safety. A residual from an
uncalibrated model is model error wearing a coach's name.
*Status: 2 of 6 targets clear. Run/pass and play action.*

**G2. The measure is a trait, not a season.** A coach's value must persist
year over year at r >= 0.30, and must persist across a change of team. The
across-team test is the hard one and the one that matters: if a coach's number
follows him from Baltimore to Seattle, it is about him.
*Status: within-team persistence is met on five of six targets. The across-team
split is computed and printed in every grade, but it CANNOT yet answer the
question: only 11 to 42 coach-seasons involve a move, and the confidence
intervals on the changed-team correlations run as wide as [-0.43, 0.59]. Two
of the six targets point the opposite way. It stays in the diagnostics as a
number to watch as more coaches change jobs; it is not strong enough to
support a conclusion in either direction, and an earlier version of this file
wrongly treated it as one.*

**G3. Decisions are separated from outcomes.** Value must be computed
counterfactually at the moment of the decision, not from whether the play
worked. `R/factory/95` does this for fourth down, where the win-probability
cost of a choice is exactly computable. The ambition is to extend that pricing
to a second and third decision class.
*Status: fourth down done, and corrected twice after adversarial review. The
cost is now priced against all three options rather than go-versus-kick, which
had been giving every coach a free pass on punting when the field goal was
better. Each coach is scored against the league in his own seasons, because
leaguewide waste fell 31% from 2018 to 2025 and a pooled ranking was 22% an
artefact of when a coach worked. Rates are empirical-Bayes shrunk and plotted
with intervals, because the raw metric's split-half reliability is only about
0.50 and roughly half the named coaches cannot be separated from average.
The metric's own year-over-year persistence is r = 0.21 to 0.26, BELOW the G2
bar of 0.30, so it is a description of behaviour that does not yet qualify as
a trait. Timeouts and play-calling are still unpriced.*

**G4. It survives the quarterback.** Every claim must be re-run controlling for
quarterback quality using the lagged `ctrl_` columns. Where the effect
disappears, that is reported as the finding.
*Status: done for the high-leverage work, where the QB turned out to matter
about ten times more than the play-calling residual. Not yet applied to the
decision-cost ledger.*

**G5. It predicts.** The measure must beat the closing spread out of sample, or
predict next season's wins above expectation, at conventional significance.
This is the criterion that separates a real metric from a well-dressed
description.
*Status: NOT YET TESTED, which is a correction to what this file said before.
It previously recorded G5 as FAILING on the grounds that high-leverage
fourth-down accuracy does not predict beating the market (r = +0.18, p = 0.23,
n = 46). Adversarial review showed that was not a test at all: the variable has
a split-half reliability of -0.18 between coaches, so it carries no coach
signal to begin with, and the confidence interval on that correlation runs from
-0.12 to +0.45 and comfortably contains effects worth having. A null computed
on noise is uninformative, not negative. G5 therefore remains open and needs a
measure that clears a reliability check first. Until then the honest claim is
unchanged: we have measured coaching behaviour, not coaching value.*

## The standard of proof

The project is worth more as an honest null than as an overclaim. Three
findings have already died on contact with better data or a better test, and
each of those is on the public board with its evidence:

- "Defensive coordinators blitz on autopilot" (r = 0.06 on two seasons became
  r = 0.32 on four)
- Josh Allen as the most profitable blitz target (+58pp collapsed to +14 once
  the unstable denominator was given more data)
- "Deviating from the situation pays" (magnitude added nothing; it was
  direction, and direction is just the league running too much)
- "High-leverage fourth-down accuracy does not predict wins" (retracted: the
  measure had no between-coach reliability, so the null was about noise)
- "Coaching tendencies are mostly the building, not the coach" (retracted the
  same day it was written: 11 to 42 moved-coach pairs cannot carry it, and two
  of six targets pointed the other way)
- The first version of the decision-cost leaderboard (priced only two of the
  three options, ignored a 31% era trend, and ranked unshrunk rates without
  intervals)

An adversarial review of this repo in August 2026 ran 24 agents across four
dimensions and confirmed eleven defects, of which the four largest are fixed
above. That review is the reason several numbers on the public page changed.

Any future claim gets the same treatment. If G5 cannot be met, the deliverable
is a rigorous measurement of coaching *behaviour* plus a clear statement that
its link to winning is unproven, which is still more than anyone else has
published.

## Non-goals

- A single composite coach rating. The dimensions disagree with each other and
  that disagreement is the interesting part: Belichick is first on beating the
  market and near-worst on fourth down.
- Predicting individual games.
- Anything that requires tracking data after 2022, which we cannot license.
