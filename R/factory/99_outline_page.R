# =============================================================================
# factory/99_outline_page.R -- writes docs/outline.html
#
# Michael's outline, in his order, with our best chart under each line and no
# commentary. Built as its own page so Nick can send him one link that maps
# straight onto the document he wrote.
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
    gap('No chart yet. Coach and coordinator birth dates are not in nflverse or any other feed we use, so this needs a scraped source (Pro Football Reference coach pages, or Wikipedia) before it can be answered. Flagging rather than guessing.')),
sec('The Seahawks defense struggled against the Rams, but their offense shredded them',
    fig(file.path(FIG,'outline_sea_lar.png'),
        'EPA per play by each offence in the four Seahawks-Rams games of 2024 and 2025',
        'Both offences in all four meetings, including the playoff game.')),

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

'<h2>4th Downs</h2>',
sec('How do coaches look here?',
    fig(file.path(OLD,'book_leaderboard.png'),
        'Head coaches most and least often correct on clear-cut fourth downs',
        'Share of clear-cut fourth downs handled correctly, 2018 to 2025.')),
sec('Coaches will punt when they should go for it, but not the other way around',
    fig(file.path(OLD,'book_by_type.png'),
        'Correct on obvious kicks against correct on obvious go-for-its by coach',
        '98.8% correct when the math says kick. 49.8% when it says go.'),
    fig(file.path(FIG,'leverage_fourthdown.png'),
        'Fourth-down accuracy in routine versus high-leverage situations',
        'And it gets worse in the highest-leverage quarter: 51.5% down to 44.6%.')),
sec('What the decisions cost',
    fig(file.path(FIG,'decision_cost.png'),
        'Win probability wasted per game on fourth down by head coach',
        'Priced against all three options, scored against the league in each coach\'s own seasons.'),
    fig(file.path(FIG,'decision_cost_cumulative.png'),
        'Cumulative win probability wasted on fourth down across careers',
        'The running tab. Slope is the coach; length is just tenure.')),

'<h2>Running on early downs</h2>',
sec('Teams are still doing this too much',
    fig(file.path(FIG,'outline_early_downs.png'),
        'Early-down run rate by season against the EPA advantage of passing',
        'Run rate 44.8% in 2015, 45.5% in 2025. Passing has been worth more every single season.')),

'<h2>Adaptability</h2>',
sec('First half vs second half adjustments',
    fig(file.path(OLD,'halftime_cone.png'),
        'Halftime adjustment funnel by coach',
        'Second-half improvement against first-half performance, with a luck cone.'),
    fig(file.path(OLD,'halftime_persistence.png'),
        'Halftime adjustment persistence season over season',
        'Does being a good adjuster repeat? Split-half r = 0.475, but it correlates 0.81 with plain point margin.')),
sec('Adapting to the players you have',
    fig(file.path(FIG,'mcvay_reinvention.png'),
        'McVay personnel reinvention',
        'The clearest case in the data of a coach rebuilding around new personnel.')),
sec('Kyle Shanahan and Ben Johnson',
    fig(file.path(FIG,'outline_callers.png'),
        'Play-action rate against offensive EPA by play-caller',
        'Every caller with 1,200+ charted plays, 2022 to 2025.'),
    fig(file.path(OLD,'pred_blackjack.png'),
        'Call predictability against offensive EPA',
        'Ben Johnson is the one caller who is unpredictable and good.')),

'<h2>Disguise (offense and defense)</h2>',
sec('Where this stands',
    gap('Nothing here is solid enough to build on yet. The one chart we have (pre-snap tell) turned out to be contaminated by sample size, and after correcting it the effect mostly disappeared. Post-snap disguise needs tracking data, which we cannot license past 2022. Research ideas below for the group to pick from.'),
    fig(file.path(OLD,'pred_two_axes.png'),
        'Call predictability against pre-snap look predictability',
        'What we do have: the call and the look are separate axes. Reid hides his; McVay does not.'),
    '<h3 style="margin-top:18px">Research ideas to discuss</h3>',
    '<ul>',
    idea('<b>Motion at the snap versus motion before it.</b> FTN flags motion but not whether the man was still moving at the snap. Split those and the deception question gets sharper, because only one of them actually moves a defender late.'),
    idea('<b>Formation variety per caller.</b> How many distinct formation and personnel combinations does a caller use, and does a wider menu buy anything? Entropy over the look distribution, same method as the predictability work.'),
    idea('<b>Same look, different play.</b> The real disguise measure: for each caller, take his most common formations and ask how evenly run and pass are split from that exact look. High split means the look tells you nothing. This is buildable now.'),
    idea('<b>Late motion and defensive response.</b> Participation data has the defensive personnel on the field. If the defence substitutes or changes shell after motion, that is deception working. Needs care but the fields exist.'),
    idea('<b>Cadence and hard counts.</b> Pre-snap penalties drawn on the defence per snap is a crude but real proxy for a caller who manipulates cadence. Nobody publishes it as a coaching stat.'),
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
        'It stops working out of heavy personnel.'),
    fig(file.path(FIG,'y_pa_slices.png'),
        'Where the play-action model is right and where it is not',
        'Model diagnostics: blue is what happened, orange dashed is the model.')),

'<h2>Further questions from the outline</h2>',
sec('Not built yet',
    '<ul>',
    idea('<b>Matching personnel.</b> Does the defence answer 13 personnel with heavier bodies, and who exploits it when they do? The defensive personnel field exists, so this is buildable.'),
    idea('<b>Hybrid players (Kupp, Puka in the slot / TE / receiver).</b> Needs alignment data, which means tracking. Not licensable past 2022.'),
    idea('<b>What does Sam think of personnel?</b> Interview question, not a data question.'),
    idea('<b>Coordinator ages over time.</b> Needs a scraped birth-date source.'),
    '</ul>')

), collapse = "\n")

html <- paste0('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>The Outline &mdash; Coaching Report Card</title><style>', css, '</style></head><body>
<main>
<header>
  <div class="kicker">Coaching report card</div>
  <h1>Michael&rsquo;s outline, with the charts</h1>
  <p class="dek">Your outline in your order, with our best version of each thing underneath it. Orange boxes are where we do not have something solid yet.</p>
  <div class="nav"><a href="index.html">The working board</a> &nbsp;&middot;&nbsp; <a href="models.html">Model Factory</a></div>
</header>
', body, '
<footer>
Charts built from nflverse play-by-play 2015-2025, FTN charting and participation 2022-2025, play-caller attribution from samhoppen/NFL_public, and the nfl4th decision model. Model diagnostics and the grading rubric are on the <a href="models.html">Model Factory</a> tab.
</footer>
</main></body></html>')

writeLines(html, "docs/outline.html")
cat("wrote docs/outline.html\n")
