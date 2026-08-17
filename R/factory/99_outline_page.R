# =============================================================================
# factory/99_outline_page.R -- writes docs/outline.html
#
# The script outline, in order, with our best chart under each line and no
# commentary. Built as its own page so Nick can send him one link that maps
# straight onto the outline document.
#
# Where we have nothing solid (Disguise), research ideas go there instead,
# clearly marked as ideas rather than findings.
# =============================================================================

FIG <- "figures/factory"
OLD <- "figures"

fig <- function(src, alt, cap = NULL) sprintf(
  '<figure><img src="%s" alt="%s">%s</figure>', src, alt,
  if (is.null(cap)) "" else sprintf('<figcaption>%s</figcaption>', cap))

sec <- function(title, ...) sprintf('<section><h3>%s</h3>%s</section>',
                                    title, paste(c(...), collapse = "\n"))

gap <- function(txt) sprintf('<p class="gap">%s</p>', txt)
idea <- function(txt) sprintf('<li>%s</li>', txt)
# neutral note, for answering a question rather than flagging a hole
note <- function(txt) sprintf('<p class="note">%s</p>', txt)

css <- '
:root{--bg:#fbfbfa;--ink:#1e2126;--ink2:#4a5058;--ink3:#778089;--accent:#2B8CBE;--card:#fff;--line:#e3e5e8;--warn:#b26a00;--warnbg:#fdf3e3}
@media(prefers-color-scheme:dark){:root{--bg:#16181c;--ink:#e8eaed;--ink2:#b3b9c0;--ink3:#848b94;--accent:#62aed4;--card:#1e2126;--line:#2e3238;--warn:#e0a458;--warnbg:#2a2114}}
*{margin:0;padding:0;box-sizing:border-box}
body{background:var(--bg);color:var(--ink);font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;line-height:1.6;font-size:17px}
main{max-width:1040px;margin:0 auto;padding:0 20px 90px}
header{padding:54px 0 4px;text-align:center}
.kicker{font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--accent);font-weight:600}
h1{font-size:clamp(30px,5vw,46px);line-height:1.1;margin:12px auto 12px;max-width:820px;letter-spacing:-.02em}
.dek{font-size:clamp(16px,2vw,20px);color:var(--ink2);max-width:720px;margin:0 auto}
.nav{text-align:center;margin:20px 0 0;font-size:15px}
.nav a{color:var(--accent);text-decoration:none;margin:0 10px}
h2{font-size:clamp(22px,3vw,29px);margin:56px 0 4px;letter-spacing:-.01em;border-bottom:2px solid var(--accent);padding-bottom:8px}
h3{font-size:19px;margin:0 0 12px;color:var(--ink)}
section{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:16px 18px;margin:18px 0}
figure{margin:12px 0 8px;background:#fff;border:1px solid var(--line);border-radius:9px;padding:7px;overflow:hidden}
figure img{width:100%;height:auto;display:block;border-radius:4px}
figcaption{font-size:12px;color:var(--ink3);padding:7px 4px 2px;line-height:1.45}
.gap{font-size:14px;color:var(--warn);background:var(--warnbg);border-left:3px solid var(--warn);padding:9px 13px;border-radius:0 8px 8px 0;margin:10px 0}
.note{font-size:14.5px;color:var(--ink2);border-left:3px solid var(--accent);padding:8px 13px;margin:10px 0}
.note b{color:var(--ink)}
ul{margin:8px 0 4px 20px}li{font-size:14.5px;color:var(--ink2);margin:6px 0}
li b{color:var(--ink)}
footer{border-top:1px solid var(--line);margin-top:60px;padding-top:18px;font-size:13.5px;color:var(--ink3);max-width:760px;margin-left:auto;margin-right:auto}
'

body <- paste(c(

'<h2>Setup: Defense vs. Offense &mdash; McVay vs. MacDonald</h2>',
sec('McVay (offense)',
    fig(file.path(FIG,'mcvay_reinvention.png'),
        'Sean McVay 11 and 13 personnel usage by season against the league',
        'Led the league in 11 personnel in 2022 and 2023, then led it in 13 personnel in 2025.'),
    fig(file.path(OLD,'market_2025.png'),
        'Wins above market expectation by team in 2025',
        'Wins above what the closing spread expected, 2025.')),
sec('MacDonald (defense)',
    fig(file.path(FIG,'macdonald_clock.png'),
        'Macdonald defensive EPA allowed year one vs year two at Baltimore and Seattle',
        'Ordinary in year one, second in the league in year two. Twice.'),
    fig(file.path(FIG,'defense_leaders.png'),
        'Career EPA per play allowed by defensive play-caller',
        'Career EPA per play allowed, minimum 1,500 charted plays.')),
sec('Have coordinators decreased in age? How much? Offense? Defense?',
    fig(file.path(FIG,'coordinator_age.png'),
        'Average age of offensive and defensive play-callers by season, 1999 to 2025',
        'Offense drifts down all window. Defense got older to a peak of 53.9 in 2019, then fell to 47.6.')),
sec('The Seahawks defense struggled against the Rams, but their offense shredded them',
    fig(file.path(FIG,'outline_sea_lar.png'),
        'EPA per play by each offense in the four Seahawks-Rams games of 2024 and 2025',
        'Both offenses in all four meetings, including the playoff game.')),

'<h2>Personnel</h2>',
sec('What 11 and 13 personnel are, and who uses them',
    fig(file.path(FIG,'outline_personnel.png'),
        'Share of snaps by personnel grouping with pass rate and EPA',
        'The five groupings, their share of snaps, pass rate and EPA.')),
sec('What Sean McVay did',
    fig(file.path(FIG,'mcvay_reinvention.png'),
        'Sean McVay 11 and 13 personnel usage by season',
        'Same chart as the setup section: 92% and 95% in 11 personnel, then 29% in 13.')),
sec('How does this do against lighter boxes?',
    fig(file.path(FIG,'outline_boxes.png'),
        'EPA per play by personnel grouping against light and heavy boxes',
        'Every grouping does better against a light box. 13 personnel is the least likely to see one.')),
sec('Is play action more effective out of 13 personnel?',
    fig(file.path(FIG,'pa_by_personnel.png'),
        'Play action EPA edge by personnel grouping',
        '+0.117 EPA out of 11 personnel, +0.089 out of 12, and nothing out of 13.')),
sec('Run vs. Pass',
    fig(file.path(FIG,'y_pass_resid.png'),
        'Play-caller residual leaderboard for run versus pass',
        'Who passes more or less than the game state predicts. Model cleared all five rubric rules.'),
    fig(file.path(FIG,'leverage_deviation.png'),
        'Pass residual against high-leverage EPA by play-caller',
        'Callers who throw more than expected move the ball better when it matters.')),
sec('What a coin-flip high-leverage play is',
    note('Two things have to be true at once. <b>High leverage</b> means the game is still live, with win probability between 36% and 64%. That is the top quarter of all snaps by how much a single play can swing the result. <b>Coin flip</b> means the situation gives the defense nothing, with the model landing between 50 and 60% on run or pass. It knows down, distance, field position, score and clock, and it does not know who is playing. The two together are 10.6% of all called plays, and the typical one is 1st and 10 near midfield in a one-score game.'),
    note('One thing to say carefully on camera. High leverage here means a <b>close</b> game, not a <b>late</b> one. Leverage peaks when a game is even and games are most even at the start, so 36% of the high-leverage bucket is the first quarter and only 14% is the fourth. That is why several of the examples below are tied in the first quarter.'),
    fig(file.path(FIG,'coinflip_anatomy.png'),
        'Leverage as a function of win probability, and model pass probability by down and distance',
        'The two dials, drawn separately. A play has to sit in the shaded band of both.'),
    fig(file.path(FIG,'coinflip_examples.png'),
        'Two real plays from each of the six certainty and leverage buckets',
        'Twelve real plays, one that followed the model and one that went the other way in each spot.')),
sec('When is it worth being unpredictable?',
    note('<b>How to read the next chart.</b> The model gives every snap a run/pass probability from the situation alone. How far that number sits from 50/50 is how obvious the call was, and the league splits into roughly even thirds: <b>coin flip</b> at 50 to 60% sure, <b>leaning</b> at 60 to 80%, <b>obvious</b> at 80% or more. Within each third, compare the plays where the caller did the expected thing against the plays where he did not. On obvious calls, going against the situation costs 0.04 EPA a play. On coin flips it gains 0.09. The book is right on the easy hands, and the game is what you do on the hard ones.'),
    fig(file.path(FIG,'nuance_by_certainty.png'),
        'EPA per play by how sure the model was and whether the caller followed it',
        'Going against the grain costs when the call is obvious and gains +0.09 EPA when it is a coin flip. A third of snaps are coin flips.'),
    fig(file.path(FIG,'certainty_examples.png'),
        'Two real plays from each of the six bars on the certainty chart, with what happened on each',
        'What each bar is made of. Two plays per bar with the play description and the league average for that bar, so a row illustrates the spot rather than standing in for the number.'),
    fig(file.path(FIG,'nuance_leverage.png'),
        'The coin-flip deviation edge under seven definitions of when a play matters',
        'Holds in close games, in both halves and in the playoffs. In the fourth quarter of a one-score game it is gone, and in the last five minutes it reverses. Leverage peaks when a game is even, and games are most even early, so the top leverage quartile is 36% first quarter.'),
    fig(file.path(FIG,'nuance_where_deviate.png'),
        'Which callers gain most when they go against the model on coin flips',
        'Whose surprises actually work. Kyle Shanahan and Ben Johnson are both top five.')),
sec('Is the coach a coin flip, or only the situation?',
    note('The model calls a spot 50/50 because that is what the <b>league</b> does there. A defensive coordinator is preparing for one man, not the league, so the question worth asking is what each caller does in those same spots. Mostly the answer is reassuring: 21 of 31 callers sit within five points of a true 50/50, and the whole field spans 39% to 57%. Second down is where coordinators go looking for a tendency, and that is where the differences show up.'),
    fig(file.path(FIG,'tendencies_second_down.png'),
        'Pass rate on second down by distance, one dot per play-caller, against the league and a true 50/50',
        'Ben Johnson is the closest to a coin flip on 2nd and 1-2 (47.7%) and 2nd and 3-6 (50.3%). On 2nd and 7-10 he is third at 58.3%, behind Chip Kelly at 52.9%.'),
    fig(file.path(FIG,'tendencies_coinflip.png'),
        'Each play-caller pass rate on the snaps where the situation-only model landed between 50 and 60 percent',
        'Andy Reid throws on 57% of the league coin flips and Zac Robinson runs on 61%, so scouting those two beats scouting the situation. Most of the field is genuinely balanced.')),

'<h2>4th Downs</h2>',
sec('How do coaches look here?',
    note('Every fourth-down chart on this page is credited to the <b>head coach</b>, not the play-caller. That is why Mike Macdonald appears on them even though he calls the defense. The head coach has the final say on whether to go, and it lands on him when it fails.'),
    fig(file.path(OLD,'book_leaderboard.png'),
        'Head coaches most and least often correct on clear-cut fourth downs',
        'Share of clear-cut fourth downs handled correctly, 2018 to 2025.')),
sec('Old coaches against young coaches',
    fig(file.path(FIG,'age_fourthdown.png'),
        'Coach age against how often he goes for it on clear go-for-it fourth downs',
        'Age barely moves it. Kliff Kingsbury at 41 is the most aggressive coach in the data and DeMeco Ryans at 40 is the most cautious. Only the 60-and-over group separates from the league.')),
sec('Coaches will punt when they should go for it, but not the other way around',
    fig(file.path(OLD,'book_by_type.png'),
        'Correct on obvious kicks against correct on obvious go-for-its by coach',
        '98.8% correct when the math says kick. 49.8% when it says go.'),
    fig(file.path(FIG,'leverage_fourthdown.png'),
        'Fourth-down accuracy in routine versus high-leverage situations',
        'And it gets worse in the highest-leverage quarter: 51.5% down to 44.6%.')),
sec('Is anyone clutch?',
    note('Michael asked whether some coaches dial it up better than others when it matters. Measured against each caller own baseline, the gaps are there but they do not survive a noise test: only 23% of the spread in close-game performance is real, against 79% when the same test is run on overall play-calling quality. A caller close-game lift in one set of seasons predicts his lift in the other at r = +0.05. Someone tops this table every year and it is a different someone each time.'),
    fig(file.path(FIG,'clutch.png'),
        'EPA above each play-caller own baseline in close games, late one-score situations and the playoffs',
        'Everyone measured against himself, so this is not a leaderboard of the best offenses. The spread is mostly sampling noise.')),
sec('What play-callers have success in the red zone?',
    fig(file.path(FIG,'redzone_callers.png'),
        'Red zone EPA per play by play-caller, and whether it repeats across seasons',
        'Sean Payton leads at +0.131 EPA per play inside the 20. Worth saying carefully though: only 25% of this spread is real and it repeats across seasons at r = +0.12, so most of the order below the top would look different over different seasons.')),
sec('What the decisions cost',
    fig(file.path(FIG,'decision_cost.png'),
        'Win probability wasted per game on fourth down by head coach',
        'Priced against all three options, scored against the league in each coach\'s own seasons.'),
    fig(file.path(FIG,'career_arc.png'),
        'Cumulative wins above market expectation across coaching careers',
        'The career arc: wins banked above what the market expected, game by game.')),

'<h2>Running on early downs</h2>',
sec('Teams are still doing this too much',
    fig(file.path(FIG,'outline_early_downs.png'),
        'Early-down run rate by season against the EPA advantage of passing',
        'Run rate 44.8% in 2015, 45.5% in 2025. Passing has been worth more every single season.')),
sec('Which play-callers are best and worst at it?',
    fig(file.path(FIG,'early_down_callers.png'),
        'Early-down run rate above expected by play-caller, and the offense that goes with it',
        'Measured against what the game state called for, so a caller who spent three years with a lead is not punished for it. Andy Reid is 8.4 points below expected, Arthur Smith 6.2 above.')),

'<h2>Adaptability</h2>',
sec('First half vs second half adjustments',
    fig(file.path(OLD,'halftime_cone.png'),
        'Halftime adjustment funnel by coach',
        'Second-half improvement against first-half performance, with a luck cone.')),
sec('Adapting to the players you have',
    fig(file.path(FIG,'mcvay_reinvention.png'),
        'McVay personnel reinvention',
        'The clearest case in the data of a coach rebuilding around new personnel.')),
sec('Kyle Shanahan and Ben Johnson',
    fig(file.path(FIG,'nuance_where_deviate.png'),
        'Which callers gain most when they go against the model on coin flips',
        'Both are top five at making a surprise pay: Shanahan +0.20 EPA, Johnson +0.20.'),
    fig(file.path(FIG,'outline_callers.png'),
        'Play-action rate against offensive EPA by play-caller',
        'Every caller with 1,200+ charted plays, 2022 to 2025.'),
    fig(file.path(OLD,'pred_blackjack.png'),
        'Call predictability against offensive EPA',
        'Ben Johnson is the one caller who is unpredictable and good.')),

'<h2>Disguise (offense and defense)</h2>',
sec('Same look, different play',
    note('This is the one that worked. For every caller and every look he uses (formation crossed with personnel), compare what he actually called against what the situation alone predicted. That isolates what the <b>look</b> gives away on top of the down, distance, score and clock the defense already knows. Every gap is shrunk by its own standard error first, which is the step the earlier pre-snap chart was missing.'),
    fig(file.path(FIG,'disguise_same_look.png'),
        'How much each play-caller pre-snap look gives away, and whether it repeats across seasons',
        'It repeats hard (r = +0.79), and it is the man rather than his formation menu, which explains only 5%. Matt LaFleur and Sean McVay hide the most from the same looks everyone else uses. Being readable does not visibly cost offense (r = +0.12, p = 0.37), so this describes style more than it grades it.')),
sec('Where the rest of this stands',
    gap('Post-snap disguise still needs tracking data, which we cannot license past 2022. The older pre-snap tell chart below is superseded by the one above. Research ideas at the bottom for the group to pick from.'),
    fig(file.path(OLD,'pred_two_axes.png'),
        'Call predictability against pre-snap look predictability',
        'The earlier version: the call and the look are separate axes. Superseded by the chart above.'),
    '<h3 style="margin-top:18px">Research ideas to discuss</h3>',
    '<ul>',
    idea('<b>Motion at the snap versus motion before it.</b> FTN flags motion but not whether the man was still moving at the snap. Split those and the deception question gets sharper, because only one of them actually moves a defender late.'),
    idea('<b>Formation variety per caller.</b> How many distinct formation and personnel combinations does a caller use, and does a wider menu buy anything? Entropy over the look distribution, same method as the predictability work.'),
    idea('<b>Same look, different play.</b> Built, and it is now the chart at the top of this section.'),
    idea('<b>Late motion and defensive response.</b> Participation data has the defensive personnel on the field. If the defense substitutes or changes shell after motion, that is deception working. Needs care but the fields exist.'),
    idea('<b>Cadence and hard counts.</b> Pre-snap penalties drawn on the defense per snap is a crude but real proxy for a caller who manipulates cadence. Nobody publishes it as a coaching stat.'),
    idea('<b>Play-action after a run versus after a pass.</b> Directly tests the sequencing claim below with the disguise framing: is the fake more convincing when the run game has been established that drive?'),
    '</ul>'),

'<h2>Sequencing</h2>',
sec('Play action: rate, and what the situation expected',
    fig(file.path(FIG,'outline_pa_rate.png'),
        'Actual play-action rate against the rate the situation predicts, by caller',
        'Above the line means more play action than the game state calls for. This model cleared all five rubric rules.'),
    fig(file.path(FIG,'y_pa_resid.png'),
        'Play-action residual leaderboard',
        'The same thing as a leaderboard.')),
sec('Does running set up the play-action pass?',
    fig(file.path(OLD,'run-sets-up-nothing.png'),
        'Whether running earlier in a drive improves later play action',
        'The existing answer: it does not.'),
    fig(file.path(OLD,'steelman-pass-sets-run.png'),
        'The strongest case for the run game setting up the pass',
        'The best version of the opposite argument, tested.'),
    gap('Still to do: a literature check on what is already published on sequencing, so we are not re-deriving something or contradicting a known result without knowing it. Worth asking Paganetti directly, since this is his area.')),

'<h2>Play Action</h2>',
sec('Everything on play action in one place',
    fig(file.path(FIG,'outline_pa_rate.png'),
        'Play-action rate versus expected by caller',
        'Who uses it more than the situation calls for.'),
    fig(file.path(FIG,'pa_by_personnel.png'),
        'Play action edge by personnel grouping',
        'It stops working out of heavy personnel.'))

), collapse = "\n")

html <- paste0('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Script Outline &mdash; Coaching Report Card</title><style>', css, '</style></head><body>
<main>
<header>
  <div class="kicker">Coaching report card</div>
  <h1>Script Outline</h1>
  <p class="dek">The script outline in order, with our best version of each thing underneath it. Orange boxes are where we do not have something solid yet.</p>
  <div class="nav"><a href="index.html">The working board</a> &nbsp;&middot;&nbsp; <a href="models.html">Model Factory</a></div>
</header>
', body, '
<footer>
Charts built from nflverse play-by-play 2015-2025, FTN charting and participation 2022-2025, play-caller attribution from samhoppen/NFL_public, and the nfl4th decision model. Model diagnostics and the grading rubric are on the <a href="models.html">Model Factory</a> tab.
</footer>
</main></body></html>')

writeLines(html, "docs/outline.html")
cat("wrote docs/outline.html\n")
