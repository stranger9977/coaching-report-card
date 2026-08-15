# =============================================================================
# factory/30_page.R -- writes docs/models.html, the modelling dashboard tab.
#
# Generated from the fits rather than hand-written, so the page can never drift
# out of sync with the scorecard. Rerun after any refit.
# =============================================================================

suppressMessages({ library(data.table); library(readr) })

sc <- as.data.table(read_csv("data/factory/scorecard.csv", show_col_types = FALSE))
fits <- lapply(list.files("data/factory/fits", full.names = TRUE), readRDS)
names(fits) <- vapply(fits, function(o) o$target, "")
sc <- sc[order(-cleared, -auc)]

esc <- function(x) gsub("&","&amp;", gsub("<","&lt;", x))

# ---- scorecard table --------------------------------------------------------
rows <- paste(vapply(seq_len(nrow(sc)), function(i) {
  r <- sc[i]; o <- fits[[r$target]]
  cls <- if (isTRUE(r$cleared)) "ok" else "no"
  sprintf('<tr class="%s"><td class="nm">%s</td><td>%s</td><td>%.3f</td><td>%.3f</td><td>%+.3f</td><td>%.4f</td><td>%.2f</td><td><b>%d/5</b></td><td>%s</td></tr>',
          cls, esc(r$label), format(r$n, big.mark = ","), r$rate, r$auc,
          r$auc - r$auc_base, r$ece, r$persist, r$n_rules_passed,
          if (isTRUE(r$cleared)) "cleared" else paste(o$grade[pass == FALSE]$rule, collapse = ", "))
}, ""), collapse = "\n")

# ---- per-target sections ----------------------------------------------------
sect <- paste(vapply(seq_len(nrow(sc)), function(i) {
  r <- sc[i]; o <- fits[[r$target]]; tg <- r$target
  g <- o$grade
  grows <- paste(sprintf('<li><span class="%s">%s</span> <b>%s</b> — %s</li>',
                         ifelse(g$pass, "pass", "fail"), ifelse(g$pass, "PASS", "FAIL"),
                         esc(g$rule), esc(g$value)), collapse = "\n")
  resid_fig <- if (isTRUE(r$cleared))
    sprintf('<figure><img src="figures/factory/%s_resid.png" alt="Coach residual leaderboard for %s"><figcaption>Residual leaderboard. Only drawn for models that cleared all five rules.</figcaption></figure>', tg, esc(r$label))
  else
    sprintf('<p class="note">No residual leaderboard: this model did not clear the rubric, so its residuals are not safe to read as coaching. It fails %s.</p>',
            esc(paste(g[pass == FALSE]$rule, collapse = " and ")))
  sprintf('<section id="%s">
  <h3>%s <span class="tag %s">%s</span></h3>
  <ul class="rules">%s</ul>
  <figure><img src="figures/factory/%s_slices.png" alt="Actual versus predicted by binned feature for %s"><figcaption>Blue is what happened with a 95%% band, orange dashed is the model. This is the figure that decides whether a residual is signal or model error.</figcaption></figure>
  <figure><img src="figures/factory/%s_calib.png" alt="Calibration curve for %s"><figcaption>Calibration: deciles of predicted probability against what actually happened.</figcaption></figure>
  %s
</section>', tg, esc(r$label), if (isTRUE(r$cleared)) "ok" else "no",
          if (isTRUE(r$cleared)) "cleared" else sprintf("%d/5", r$n_rules_passed),
          grows, tg, esc(r$label), tg, esc(r$label), resid_fig)
}, ""), collapse = "\n")

css <- '
:root{--bg:#fbfbfa;--ink:#1e2126;--ink2:#4a5058;--ink3:#778089;--accent:#2B8CBE;--card:#fff;--line:#e3e5e8;--ok:#1c7a43;--no:#b23a3a;--okbg:#e0f2e7;--nobg:#f7e0e0}
@media(prefers-color-scheme:dark){:root{--bg:#16181c;--ink:#e8eaed;--ink2:#b3b9c0;--ink3:#848b94;--accent:#62aed4;--card:#1e2126;--line:#2e3238;--ok:#6fce97;--no:#e08a8a;--okbg:#153021;--nobg:#331717}}
*{margin:0;padding:0;box-sizing:border-box}
body{background:var(--bg);color:var(--ink);font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;line-height:1.65;font-size:17px}
main{max-width:1120px;margin:0 auto;padding:0 20px 90px}
header{padding:56px 0 6px;text-align:center}
.kicker{font-size:13px;letter-spacing:.12em;text-transform:uppercase;color:var(--accent);font-weight:600}
h1{font-size:clamp(30px,5vw,46px);line-height:1.1;margin:12px auto 14px;max-width:820px;letter-spacing:-.02em}
.dek{font-size:clamp(17px,2.2vw,21px);color:var(--ink2);max-width:760px;margin:0 auto}
.nav{text-align:center;margin:22px 0 0;font-size:15px}
.nav a{color:var(--accent);text-decoration:none;margin:0 10px}
.prose{max-width:760px;margin:0 auto}
h2{font-size:clamp(23px,3.2vw,30px);margin:54px 0 14px;letter-spacing:-.01em}
h3{font-size:20px;margin:0 0 10px}
p{margin:0 0 16px;color:var(--ink2)}
table{width:100%;border-collapse:collapse;margin:18px 0 8px;font-size:14px;background:var(--card);border:1px solid var(--line);border-radius:10px;overflow:hidden}
th,td{padding:9px 10px;text-align:right;border-bottom:1px solid var(--line)}
th{background:var(--line);color:var(--ink);font-weight:700;text-align:right;font-size:12.5px;letter-spacing:.02em}
th:first-child,td.nm{text-align:left}
td.nm{font-weight:650;color:var(--ink)}
tr.ok td.nm{color:var(--ok)}
tbody tr:last-child td{border-bottom:none}
section{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:18px 20px;margin:26px 0}
.tag{font-size:10.5px;font-weight:800;letter-spacing:.05em;text-transform:uppercase;padding:3px 8px;border-radius:999px;vertical-align:middle;margin-left:8px}
.tag.ok{background:var(--okbg);color:var(--ok)}.tag.no{background:var(--nobg);color:var(--no)}
ul.rules{list-style:none;margin:6px 0 14px;font-size:14px}
ul.rules li{margin:4px 0;color:var(--ink2)}
.pass{color:var(--ok);font-weight:800;font-size:11px}.fail{color:var(--no);font-weight:800;font-size:11px}
figure{margin:16px 0 10px;background:#fff;border:1px solid var(--line);border-radius:10px;padding:8px;overflow:hidden}
figure img{width:100%;height:auto;display:block;border-radius:5px}
figcaption{font-size:12.5px;color:var(--ink3);padding:8px 4px 2px;line-height:1.5}
.note{font-size:14px;color:var(--ink3);background:var(--nobg);border-left:3px solid var(--no);padding:10px 14px;border-radius:0 8px 8px 0}
.rubric{background:var(--card);border:1px solid var(--line);border-left:3px solid var(--accent);border-radius:0 10px 10px 0;padding:16px 20px;margin:18px 0}
.rubric dt{font-weight:700;color:var(--ink);margin-top:10px;font-size:14.5px}
.rubric dd{margin:2px 0 0;color:var(--ink2);font-size:14px}
footer{border-top:1px solid var(--line);margin-top:60px;padding-top:20px;font-size:13.5px;color:var(--ink3);max-width:760px;margin-left:auto;margin-right:auto}
'

html <- sprintf('<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>Model Factory — Coaching Report Card</title><style>%s</style></head><body>
<main>
<header>
  <div class="kicker">Coaching report card</div>
  <h1>The Model Factory</h1>
  <p class="dek">Given the game state, what would the average coach do? Everything a coach does beyond that is the residual. This page is where we check whether the models are good enough for that residual to mean anything.</p>
  <div class="nav"><a href="index.html">&larr; Back to the board</a></div>
</header>

<div class="prose">
<h2>Why this page exists</h2>
<p>Eric Eager\'s framing, passed along by Michael: start with what the average coach would do given the situation, and take the residual. That only works if the model is right about the situation. If it is wrong inside the 10, then every coach who calls plays inside the 10 gets a residual that is really just model error wearing a coach\'s name.</p>
<p>So every model here is graded before anyone is allowed to read a leaderboard off it. Five rules, all of them have to pass.</p>

<div class="rubric">
<dl>
<dt>R1 &nbsp;Discrimination</dt><dd>Out-of-sample AUC at least 0.65, and at least 0.03 better than a plain down-and-distance lookup table. Beat the lookup table or you have learned nothing worth residualising.</dd>
<dt>R2 &nbsp;Calibration</dt><dd>Expected calibration error at most 0.020, no probability decile off by more than 0.05. If the model runs low at 80%%, every coach in those spots gets a fake positive residual.</dd>
<dt>R3 &nbsp;Slice safety</dt><dd>At least 90%% of situational slices with 200+ plays must have their predicted mean inside the actual mean\'s 95%% interval. Season is deliberately excluded: it is a time index, not a football situation, and league drift is removed at the residual stage instead by centring each coach within his own season.</dd>
<dt>R4 &nbsp;Honest holdout</dt><dd>Season-grouped cross-validation. No play is ever scored by a model that saw its own season, because letting the model see 2024 while scoring 2024 lets it learn the coach, which is the thing we want left in the residual.</dd>
<dt>R5 &nbsp;Residual persistence</dt><dd>A coach\'s residual one season must correlate with the next at r of at least 0.30. A residual that does not persist is noise. This is the rule that separates a trait from a coincidence, and it is the one most write-ups skip.</dd>
</dl>
</div>
<p>Failing R1 to R4 is a modelling problem and means go fix the features. Passing R1 to R4 but failing R5 is not a modelling problem at all: it means the behaviour genuinely is not a stable coaching trait, which is itself worth knowing.</p>
</div>

<h2>Scorecard</h2>
<table>
<thead><tr><th>Target</th><th>plays</th><th>base rate</th><th>AUC</th><th>lift vs lookup</th><th>ECE</th><th>persistence</th><th>rules</th><th>status</th></tr></thead>
<tbody>
%s
</tbody></table>
<p class="prose" style="font-size:14px;color:var(--ink3);margin-top:10px">Persistence is the year-over-year correlation of a coach\'s residual. Lift is AUC above a down-and-distance lookup table. Every figure below is season-grouped out-of-sample.</p>

<h2>Model by model</h2>
%s

<footer>
Built by <code>R/factory/</code>: <code>00_features.R</code> builds one modelling table, <code>lib_factory.R</code> holds the rubric and the fitting engine, <code>10_run.R</code> fits and grades every target, <code>20_dashboard.R</code> draws these figures, <code>30_page.R</code> writes this page. Data: nflverse play-by-play 2015-2025, FTN charting and participation 2022-2025, play-caller attribution from samhoppen/NFL_public. Models are xgboost with season-grouped cross-validation.
</footer>
</main></body></html>', css, rows, sect)

writeLines(html, "docs/models.html")
cat("wrote docs/models.html\n")
cat(sprintf("cleared: %s\n", paste(sc[cleared == TRUE]$label, collapse = ", ")))
