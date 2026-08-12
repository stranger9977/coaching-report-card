# Coaching report card, board design

2026-08-12. Approved by Nick before build ("go").

## What this is

A brainstorm board for the Michael MacKelvie coaching/play-caller video,
built exactly like ghost-town: public repo, Pages at
stranger9977.github.io/coaching-report-card, `main:/docs`, one static
index.html, card board with status pills, R scripts numbered in `R/`,
`R/lib/theme_coach.R` copied from theme_ghost.R.

Workflow: private-ish brainstorm page for iteration -> Michael finalizes the
script and films -> Nick turns the script into a Substack article -> the video
links the Substack, not this board.

## Decisions made at approval

- Name: coaching-report-card (Nick's own phrase from the Aug 7 texts).
- Reuse: charts from the local July repo nfl-analysis are copied as-is into
  docs/figures and credited per card; only new analyses get new scripts here.
- The three lead findings: play-caller predictability sign-flip, the Seahawks
  question answered with new talent/market models, Andy Reid timeout
  redemption.
- Board groups: (1) the Seahawks question, (2) playcalling, (3) adjustments,
  (4) game management, (5) deception parked, then a ranked ideas section.
- New builds: roster-talent mixed model (draft capital baseline, lme4),
  market expectation (closing spreads), Madden ratings version (Nick's
  pointer: theedgepredictor/nfl-madden-data), halftime-adjustment
  repeatability test.
- Honesty rules: verify every number against script output before it goes on
  the page; luck cones on every leaderboard; dead hypotheses stay on the board
  with evidence; chart titles must not overstate.
- Precision fix carried from the audit: the icing luck-cone claim is 20 of 21
  coaches inside, Pete Carroll outside (and his outlier status is itself the
  expected fluke from screening 21 coaches).
