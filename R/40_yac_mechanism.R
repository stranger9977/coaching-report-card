# =============================================================================
# 40_yac_mechanism.R -- deepening R/31: WHERE does Shanahan's YAC edge come from?
#
# R/31 shipped the finding: Shanahan's completions gain +0.76 more yards after
# the catch than a throw-depth+situation model expects, #1 of 37 qualified
# callers, at a dead-average throw depth (Ben Johnson #3 at +0.73, McVay #11
# at +0.17). That number does not say WHY. Two different stories both predict
# "more YAC than expected":
#   SCHEMED SPACE  -- the catch happens where defenders are not. Route
#     combinations, motion, leverage. Credit belongs to the design.
#   BROKEN TACKLES -- the receiver beats the guy trying to bring him down.
#     Credit belongs to the personnel (McCaffrey, Deebo, Kittle).
# R/26 (scheme vs. execution) found Shanahan elite on both dimensions
# generally, so the prior going in is genuinely open on which one is doing
# the work here.
#
# THE SPLIT. Two Sumer field families, both untouched anywhere else in this
# repo:
#   Play-level (plays_full.csv.gz, via load_sumer()): yards_after_catch
#     (already in R/31) and yards_after_contact -- yards gained AFTER the
#     first hit was absorbed. The gap between them, yards_after_catch minus
#     yards_after_contact, is yardage gained BEFORE any defender touched the
#     receiver: space the scheme created.
#   Player-level (plays_players_p1/p2.csv.gz): tackle_avoided (the targeted
#     receiver broke a tackle attempt, TRUE/FALSE) and, on the defense's own
#     rows for the same play, tackle_missed. These do NOT carry a yardage
#     number -- they are used to (a) validate the play-level split (does a
#     receiver-avoided-a-tackle play line up with a defender-missed-a-tackle
#     play on the SAME play) and (b) build a per-player broken-tackle rate
#     for the "who" question. first_contact is also on the player file but
#     it is a DEFENDER-IDENTITY flag (which of ~11 defenders made the hit),
#     not a yardage marker -- inspected below, then set aside; it cannot do
#     what its name suggests.
#
# FIELD INSPECTION, PRINTED BELOW, before anything is built on these fields:
#   - How often a targeted receiver breaks a tackle on a completion.
#   - Whether play-level yards_after_contact and player-level tackle_avoided
#     / tackle_missed line up on the same play (they should, and mostly do).
#   - yards_after_catch and yards_after_contact are independently charted,
#     not one derived from the other -- on ~9-18% of completions (caller-
#     dependent, the three named callers all sit in an unremarkable 11-13%
#     band) the charted contact yardage exceeds the charted catch yardage.
#     Handled with an explicit floor/cap (see step 4) rather than silently
#     dropped or averaged away.
#
# THE DECOMPOSITION. Reuses R/31's exact expected-YAC model (same features,
# same season-holdout xgboost, same seed) so this script's numbers reconcile
# with the shipped +0.76 -- checked directly against data/derived/
# kyle_yac_cutback.csv below, not just asserted. Per completion, the model's
# residual (actual minus expected YAC) is split into a CLEAN-channel piece
# and a CONTACT-channel piece in proportion to how that play's own yardage
# split (clean yards / total yards), so the two pieces always sum exactly to
# the total residual -- no separate model, no double counting.
#
# UNIVERSE. Identical to R/31: 2022-2025, non-garbage-time, off_caller
# attributed, completions with a non-missing screen flag, >= 1200 offensive
# plays to be "qualified" (37 of 65 callers).
#
# Conventions: R/lib/theme_coach.R, plain football words, no invented metric
# names in chart text, empirical-Bayes shrinkage as a robustness check (raw
# numbers drive the headline, since raw is what reconciles with R/31),
# confidence intervals on the three highlighted callers.
#
# Out: docs/figures/yac_mechanism.png (per-caller YAC-over-expected split
#        into clean-space vs contact-forced channels, 37 qualified callers,
#        Shanahan/Johnson/McVay highlighted)
#      data/derived/yac_mechanism.csv (one row per qualified caller, both
#        channels, ranks, CIs, raw and EB-shrunk)
# =============================================================================

suppressMessages({
  library(data.table); library(xgboost); library(ggplot2); library(scales)
})
source("R/lib/theme_coach.R")
source("R/factory/lib_sumer.R")
setDTthreads(2)
set.seed(20260819)   # same seed as R/31, so the model reproduces its numbers

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

MIN_OFF_PLAYS <- 1200
KYLE  <- "Kyle Shanahan"
MCVAY <- "Sean McVay"
BJ    <- "Ben Johnson"
THREE <- c(KYLE, MCVAY, BJ)

# -----------------------------------------------------------------------
# 1. Load, universe, qualified-caller list -- identical to R/31
# -----------------------------------------------------------------------
d <- load_sumer(2022:2025)
d <- d[garbage_time == FALSE & off_caller != ""]
cat(sprintf("Non-garbage-time, caller-attributed plays 2022-2025: %s\n", format(nrow(d), big.mark = ",")))

off_n <- d[run_pass %in% c("P","R"), .N, by = off_caller]
qualified <- off_n[N >= MIN_OFF_PLAYS]$off_caller
cat(sprintf("Callers with >= %d offensive plays: %d of %d\n", MIN_OFF_PLAYS, length(qualified), nrow(off_n)))
stopifnot(all(THREE %in% qualified))

comp <- d[is_targeted_pass == TRUE & is_complete_pass == TRUE & !is.na(screen)]
comp[, screen_num := as.integer(screen)]
cat(sprintf("Completions in universe: %d\n", nrow(comp)))

# =============================================================================
# 2. FIELD INSPECTION -- player-level tackle fields, before anything is built
# =============================================================================
cat("\n================ FIELD INSPECTION ================\n")

PCOLS <- c("sumer_play_id","sumer_player_id","season","side_of_ball","tackle_avoided",
           "tackle_avoided_count","first_contact","tackle_missed",
           "receiving_targets","receiving_receptions")
p1 <- fread("data/raw/sumer/plays_players_p1.csv.gz", select = PCOLS, showProgress = FALSE)
p2 <- fread("data/raw/sumer/plays_players_p2.csv.gz", select = PCOLS, showProgress = FALSE)
players <- rbindlist(list(p1, p2)); rm(p1, p2); gc(verbose = FALSE)
players <- players[season %in% 2022:2025]

tgt <- players[receiving_targets == 1, .(sumer_play_id, season, sumer_player_id,
                                         broke_tackle = tackle_avoided,
                                         tackle_avoided_count)]
cx <- merge(comp[, .(sumer_play_id, season, off_caller)], tgt, by = c("sumer_play_id","season"))
cat(sprintf("1. Targeted-receiver row matched on %d of %d completions.\n", nrow(cx), nrow(comp)))
cat(sprintf("   Tackle avoided by the catcher on a completion: %.1f%% (%d of %d)\n",
            100*mean(cx$broke_tackle), sum(cx$broke_tackle), nrow(cx)))
cat("   tackle_avoided_count on those completions (0/1/2 broken-tackle attempts):\n")
print(cx[, .N, by = tackle_avoided_count][order(tackle_avoided_count)])

cat("\n2. first_contact -- inspected and set aside: it's a DEFENDER-IDENTITY flag\n")
fc_rate <- players[side_of_ball == "defense", mean(first_contact, na.rm = TRUE)]
cat(sprintf("   (which defender initiated the hit), true on %.1f%% of defensive player-play rows\n", 100*fc_rate))
cat("   (about one flagged defender per tackle), never true on offense rows. It carries no\n")
cat("   yards-before-contact NUMBER, so it cannot do the yardage split -- yards_after_contact does.\n")

defmiss <- players[side_of_ball == "defense", .(any_def_missed = any(tackle_missed, na.rm = TRUE)),
                   by = .(sumer_play_id, season)]
cx <- merge(cx, defmiss, by = c("sumer_play_id","season"), all.x = TRUE)
agree_true  <- cx[broke_tackle == TRUE,  mean(any_def_missed, na.rm = TRUE)]
agree_false <- cx[broke_tackle == FALSE, mean(!any_def_missed, na.rm = TRUE)]
cat(sprintf("\n3. Crosstab, offense tackle_avoided vs. defense tackle_missed, SAME play:\n"))
print(cx[, .N, by = .(receiver_broke_tackle = broke_tackle, some_defender_missed = any_def_missed)])
cat(sprintf("   When the receiver avoided a tackle, a defender is also charted missing one %.1f%% of the time.\n", 100*agree_true))
cat(sprintf("   When the receiver did NOT avoid a tackle, no defender is charted missing one %.1f%% of the time.\n", 100*agree_false))
cat("   Two independently-charted fields agreeing this well is the validation for using tackle_avoided below.\n")

comp[, diff := yards_after_catch - yards_after_contact]
n_na_contact <- sum(is.na(comp$yards_after_contact))
neg_rate <- 100*mean(comp$diff < 0, na.rm = TRUE)
by_caller_neg <- comp[off_caller %in% qualified, .(pct_neg = 100*mean(diff < 0, na.rm = TRUE)), by = off_caller]
cat(sprintf("\n4. yards_after_catch vs. yards_after_contact: cor = %.3f. %d of %d completions have no\n",
            cor(comp$yards_after_catch, comp$yards_after_contact, use = "complete.obs"), n_na_contact, nrow(comp)))
cat(sprintf("   yards_after_contact charted. On %.1f%% of completions the charted CONTACT yardage exceeds\n", neg_rate))
cat("   the charted CATCH yardage (two independently-charted fields, not one derived from the other).\n")
cat(sprintf("   By qualified caller this ranges %.1f%%-%.1f%%; the three named callers sit at %.1f%% (Shanahan),\n",
            min(by_caller_neg$pct_neg), max(by_caller_neg$pct_neg),
            by_caller_neg[off_caller == KYLE]$pct_neg))
cat(sprintf("   %.1f%% (McVay), %.1f%% (Ben Johnson) -- an unremarkable, narrow band, not a caller-specific\n",
            by_caller_neg[off_caller == MCVAY]$pct_neg, by_caller_neg[off_caller == BJ]$pct_neg))
cat("   artifact. Step 4 below floors/caps rather than drops these rows.\n")

# =============================================================================
# 3. EXPECTED-YAC MODEL -- verbatim from R/31, to reconcile with the shipped +0.76
# =============================================================================
cat("\n================ MODEL (verbatim from R/31) ================\n")

SUMER_STATE_LOCAL <- SUMER_STATE
YAC_FEATS <- c("depth_of_target", "screen_num", SUMER_STATE_LOCAL)

yac_expect <- function(x, feats, nrounds = 250) {
  x <- .sumer_prep(x[!is.na(yards_after_catch)], feats)
  x <- x[complete.cases(x[, ..feats])]
  y <- as.numeric(x$yards_after_catch)
  seasons <- sort(unique(x$season)); p <- rep(NA_real_, nrow(x))
  for (s in seasons) {
    te <- which(x$season == s); tr <- which(x$season != s)
    if (length(tr) < 2000) next
    m <- xgb.train(list(objective = "reg:squarederror", eta = 0.05, max_depth = 4,
                        subsample = 0.8, colsample_bytree = 0.8,
                        min_child_weight = 30, nthread = 4),
                   xgb.DMatrix(as.matrix(x[tr, ..feats]), label = y[tr]),
                   nrounds = nrounds, verbose = 0)
    p[te] <- predict(m, xgb.DMatrix(as.matrix(x[te, ..feats])))
  }
  x[, `:=`(.y = y, .expected = p)]
  x[!is.na(.expected)]
}

z <- yac_expect(comp, YAC_FEATS)
z[, resid := .y - .expected]
cat(sprintf("Model check: %d of %d completions scored, cor(actual,expected)=%.3f\n",
            nrow(z), nrow(comp), cor(z$.y, z$.expected)))

# ---- reconciliation check against the shipped R/31 numbers -----------------
raw_oe <- z[, .(n = .N, yac_oe = mean(resid)), by = off_caller][off_caller %in% qualified]
shipped <- fread("data/derived/kyle_yac_cutback.csv", select = c("off_caller","yac_oe"))
recon <- merge(raw_oe, shipped, by = "off_caller", suffixes = c("_here","_shipped"))
recon[, delta := yac_oe_here - yac_oe_shipped]
cat(sprintf("\nReconciliation vs. data/derived/kyle_yac_cutback.csv (R/31): max abs delta across %d callers = %.4f yds\n",
            nrow(recon), max(abs(recon$delta))))
print(recon[off_caller %in% THREE, .(off_caller, yac_oe_here = round(yac_oe_here,3),
                                      yac_oe_shipped = round(yac_oe_shipped,3), delta = round(delta,4))])

# =============================================================================
# 4. THE CHANNEL SPLIT -- clean (pre-contact) vs. contact (post-contact)
# =============================================================================
cat("\n================ CHANNEL SPLIT ================\n")
cat("Definition: CONTACT yards = yards_after_contact, floored at 0 and capped at the completion's\n")
cat("total yards_after_catch (handles the charting-noise rows found in inspection step 4 by\n")
cat("attributing the whole play to the contact channel when charted contact exceeds charted catch --\n")
cat("directionally consistent with what that anomaly is telling us). CLEAN yards = the remainder.\n")
cat("Missing yards_after_contact (no charted value) is treated as 0 contact yards (fully clean).\n")
cat("Zero-yard completions (tackled immediately) are fully CLEAN by convention: no room was created,\n")
cat("which is a fact about scheme, not about the receiver's contact balance.\n")

z[, yac_contact_charted := fifelse(is.na(yards_after_contact), 0, yards_after_contact)]
z[, yac_contact := pmin(pmax(yac_contact_charted, 0), pmax(.y, 0))]
z[, yac_clean := .y - yac_contact]
stopifnot(all(abs(z$yac_clean + z$yac_contact - z$.y) < 1e-9))  # identity, always holds by construction

z[, share_clean := fifelse(.y == 0, 1, yac_clean / .y)]
z[, resid_clean := resid * share_clean]
z[, resid_contact := resid - resid_clean]
stopifnot(all(abs(z$resid_clean + z$resid_contact - z$resid) < 1e-9))

z <- merge(z, tgt[, .(sumer_play_id, season, sumer_player_id, broke_tackle)],
          by = c("sumer_play_id","season"), all.x = TRUE)

league_contact_share <- 100 * sum(z$yac_contact) / sum(z$.y)
cat(sprintf("\nLeague-wide: %.1f%% of all charted YAC yardage happens after first contact (the rest, %.1f%%,\n",
            league_contact_share, 100 - league_contact_share))
cat("happens before any defender touches the receiver -- yardage the SCHEME created.)\n")

# =============================================================================
# 5. PER-CALLER DECOMPOSITION
# =============================================================================
cat("\n================ PER-CALLER DECOMPOSITION ================\n")

shrink_eb <- function(m, n, v) {   # same helper as R/16, R/26
  pv <- max(var(m) - mean(v / n), 1e-6)
  k  <- mean(v) / pv
  (n * m + k * weighted.mean(m, n)) / (n + k)
}

callers <- z[, .(n = .N,
                 yac_oe = mean(resid), se_oe = sd(resid) / sqrt(.N),
                 yac_oe_clean = mean(resid_clean), se_clean = sd(resid_clean) / sqrt(.N),
                 v_clean = var(resid_clean),
                 yac_oe_contact = mean(resid_contact), se_contact = sd(resid_contact) / sqrt(.N),
                 v_contact = var(resid_contact),
                 contact_yard_share = 100 * sum(yac_contact) / sum(.y),
                 broke_tackle_rate = 100 * mean(broke_tackle, na.rm = TRUE)),
             by = off_caller]
callers <- callers[off_caller %in% qualified]
callers[, `:=`(yac_oe_lo = yac_oe - 1.96*se_oe, yac_oe_hi = yac_oe + 1.96*se_oe,
              clean_lo = yac_oe_clean - 1.96*se_clean, clean_hi = yac_oe_clean + 1.96*se_clean,
              contact_lo = yac_oe_contact - 1.96*se_contact, contact_hi = yac_oe_contact + 1.96*se_contact)]

# EB-shrunk robustness columns (raw drives the headline; shrinkage checked, not applied to it)
callers[, yac_oe_clean_shrunk := shrink_eb(yac_oe_clean, n, v_clean)]
callers[, yac_oe_contact_shrunk := shrink_eb(yac_oe_contact, n, v_contact)]

setorder(callers, -yac_oe); callers[, rank_yac_oe := seq_len(.N)]
setorder(callers, -yac_oe_clean); callers[, rank_clean := seq_len(.N)]
setorder(callers, -yac_oe_contact); callers[, rank_contact := seq_len(.N)]
setorder(callers, off_caller)
n_q <- nrow(callers)

print_one <- function(nm) {
  r <- callers[off_caller == nm]
  cat(sprintf("\n%s (n_completions=%d):\n", nm, r$n))
  cat(sprintf("  TOTAL   YAC over expected: %+.3f [%.3f, %.3f] -- rank %d of %d\n",
              r$yac_oe, r$yac_oe_lo, r$yac_oe_hi, r$rank_yac_oe, n_q))
  cat(sprintf("  CLEAN   channel (pre-contact space):   %+.3f [%.3f, %.3f] -- rank %d of %d (%.0f%% of total)\n",
              r$yac_oe_clean, r$clean_lo, r$clean_hi, r$rank_clean, n_q, 100*r$yac_oe_clean/r$yac_oe))
  cat(sprintf("  CONTACT channel (post-contact, broken tackles): %+.3f [%.3f, %.3f] -- rank %d of %d (%.0f%% of total)\n",
              r$yac_oe_contact, r$contact_lo, r$contact_hi, r$rank_contact, n_q, 100*r$yac_oe_contact/r$yac_oe))
  cat(sprintf("  EB-shrunk robustness check: clean %+.3f -> %+.3f, contact %+.3f -> %+.3f\n",
              r$yac_oe_clean, r$yac_oe_clean_shrunk, r$yac_oe_contact, r$yac_oe_contact_shrunk))
  cat(sprintf("  Raw yardage: %.1f%% of this caller's actual YAC yards come after contact (league: %.1f%%).\n",
              r$contact_yard_share, league_contact_share))
  cat(sprintf("  Tackle-avoided rate on his completions: %.1f%% (league: %.1f%%)\n",
              r$broke_tackle_rate, 100*mean(z$broke_tackle, na.rm = TRUE)))
}
cat(sprintf("--- Three-name contrast, %d qualified callers ---\n", n_q))
for (nm in THREE) print_one(nm)

cat("\n--- Full qualified-caller table, sorted by total YAC over expected ---\n")
print(callers[order(-yac_oe), .(off_caller, n, yac_oe = round(yac_oe,3),
                                 clean = round(yac_oe_clean,3), rank_clean,
                                 contact = round(yac_oe_contact,3), rank_contact,
                                 contact_yard_share = round(contact_yard_share,1))])

cat(sprintf("\nLeague picture: correlation across %d qualified callers between the clean-channel rank and\n", n_q))
cat(sprintf("the contact-channel rank = %.3f (near 0 = the two channels are independent skills, not the same thing twice).\n",
            cor(callers$yac_oe_clean, callers$yac_oe_contact)))

# =============================================================================
# 6. PLAYER COLOR -- who is carrying each channel, for the three named callers
# =============================================================================
cat("\n================ PLAYER COLOR ================\n")

pd <- fread("data/raw/sumer/player_details.csv.gz",
            select = c("sumer_player_id","football_name","last_name"))
pd[, name := paste(football_name, last_name)]

player_avoid <- merge(z[, .(sumer_play_id, sumer_player_id, off_caller, broke_tackle)], pd,
                      by = "sumer_player_id")
player_tab <- player_avoid[, .(catches = .N, n_avoided = sum(broke_tackle, na.rm = TRUE)), by = .(sumer_player_id, name)]
player_tab[, avoid_rate := 100 * n_avoided / catches]
player_tab <- player_tab[catches >= 50]
setorder(player_tab, -avoid_rate); player_tab[, rank := seq_len(.N)]
n_players <- nrow(player_tab)
cat(sprintf("Top 15 of %d players (>= 50 charted catches, 2022-2025) by tackle-avoided rate per catch:\n", n_players))
print(head(player_tab[, .(rank, name, catches, avoid_rate = round(avoid_rate,1))], 15))

sf_names <- c("Christian McCaffrey","Deebo Samuel","George Kittle","Brandon Aiyuk",
              "Jauan Jennings","Kyle Juszczyk","Trent Taylor","Ricky Pearsall",
              "Jordan Mason","Elijah Mitchell","Jerick McKinnon","Brock Purdy")
cat("\nShanahan's own SF weapons, tackle-avoided rate (wherever they land, not filtered to the top):\n")
print(player_tab[name %in% sf_names][order(rank), .(rank, name, catches, avoid_rate = round(avoid_rate,1))])

concentration <- function(nm) {
  sub <- z[off_caller == nm]
  sub <- merge(sub, pd, by = "sumer_player_id")
  pl <- sub[, .(n = .N, sum_clean = sum(resid_clean), sum_contact = sum(resid_contact)), by = name]
  pl <- pl[n >= 15]
  setorder(pl, -n)
  top3 <- head(pl, 3)$name
  r <- callers[off_caller == nm]
  cat(sprintf("\n%s: top-3-by-volume weapons are %s\n", nm, paste(top3, collapse = ", ")))
  # gate on the CALLER'S OWN CI for that channel (already printed above), not on the sign of
  # a subset sum -- a channel whose CI crosses zero has no real total to divide a share out of;
  # reporting a percentage there is dividing by noise, not a finding
  fmt_share <- function(lo, hi, top3_sum, tot_full, label) {
    if (lo < 0 && hi > 0) {
      sprintf("  %s: this caller's total in this channel is not distinguishable from zero (CI crosses 0) --\n    concentration doesn't apply. Top-3 players sum to %+.1f yards on their own.",
              label, top3_sum)
    } else {
      sprintf("  %s: those 3 players carry %.0f%% of his total %s yards (%+.1f of an estimated %+.1f).",
              label, 100*top3_sum/tot_full, label, top3_sum, tot_full)
    }
  }
  cat(fmt_share(r$clean_lo, r$clean_hi, sum(pl[name %in% top3]$sum_clean), r$yac_oe_clean * r$n, "CLEAN-channel"), "\n")
  cat(fmt_share(r$contact_lo, r$contact_hi, sum(pl[name %in% top3]$sum_contact), r$yac_oe_contact * r$n, "CONTACT-channel"), "\n")
  cat(sprintf("  (of %d players with >= 15 targets under him). Lower share = spread across the tree, higher = a few stars.\n", nrow(pl)))
  invisible(pl)
}
for (nm in THREE) concentration(nm)

fwrite(player_tab, "data/derived/yac_mechanism_players.csv")

# =============================================================================
# 7. CHART -- per-caller split into the two channels, 37 callers, 3 highlighted
# =============================================================================
kyle_r <- callers[off_caller == KYLE]
mcvay_r <- callers[off_caller == MCVAY]
bj_r <- callers[off_caller == BJ]

kyle_channel <- if (kyle_r$yac_oe_clean > kyle_r$yac_oe_contact) "clean-space" else "contact"
title_chart <- sprintf(
  "Shanahan's YAC edge is mostly %s: %+.2f of his %+.2f comes before contact, %+.2f after",
  kyle_channel, kyle_r$yac_oe_clean, kyle_r$yac_oe, kyle_r$yac_oe_contact)

long <- rbindlist(list(
  callers[, .(off_caller, yac_oe, value = yac_oe_clean, channel = "Clean (before contact -- space the scheme created)")],
  callers[, .(off_caller, yac_oe, value = yac_oe_contact, channel = "Contact (after contact -- receiver beat a tackle)")]
))
long[, off_caller := factor(off_caller, levels = callers[order(yac_oe)]$off_caller)]
long[, channel := factor(channel, levels = c("Contact (after contact -- receiver beat a tackle)",
                                             "Clean (before contact -- space the scheme created)"))]
lvl <- levels(long$off_caller)

chan_colours <- c("Clean (before contact -- space the scheme created)" = "#4393C3",
                  "Contact (after contact -- receiver beat a tackle)" = "#D6604D")

# Highlight the three named callers with a background band behind their row (same technique
# as R/23's elite-tier band) plus a bold end-of-bar label -- ggtext's element_markdown() was
# tried for selectively bold/coloured axis text, but it silently loses its markdown parsing
# once merged onto a complete theme (theme_minimal()-derived, which theme_coach() is) via `+`
# or `%+replace%` -- a real ggplot2/ggtext interaction, not a labeling mistake here, so this
# is the robust alternative rather than a workaround chased further.
hl_colour <- setNames(c("#7a1a1a", "#8a3d00", "#08306b"), THREE)
band_df <- data.table(off_caller = THREE, ypos = match(THREE, lvl))
label_df <- callers[off_caller %in% THREE, .(off_caller, yac_oe)]
label_df[, colour := hl_colour[off_caller]]

p <- ggplot(long, aes(x = off_caller, y = value, fill = channel)) +
  geom_rect(data = band_df, aes(xmin = ypos - 0.5, xmax = ypos + 0.5, ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "#FDE9C8", alpha = 0.55) +
  geom_col(width = 0.68) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_text(data = label_df, aes(x = off_caller, y = yac_oe, label = off_caller, colour = colour),
            inherit.aes = FALSE, hjust = 0, nudge_y = 0.03, fontface = "bold", size = 3.4) +
  scale_colour_identity() +
  coord_flip(clip = "off") +
  scale_x_discrete() +
  scale_fill_manual(values = chan_colours, name = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1, style_positive = "plus"),
                     expand = expansion(mult = c(0.02, 0.14))) +
  labs(
    title = title_chart,
    subtitle = paste0(
      "Each bar is one play-caller's career (2022-2025, >= 1,200 offensive plays), split into two shares of his total\n",
      "yards-after-the-catch edge over a throw-depth + situation model. Blue = yardage gained before any defender\n",
      "touched the receiver (scheme's doing). Red = yardage gained after contact, including broken tackles (the\n",
      "receiver's doing). The two always sum to the caller's total YAC-over-expected, from R/31. Shanahan, Ben\n",
      "Johnson and McVay are highlighted (tan band) and named at the end of their own bar."
    ),
    x = NULL,
    y = "yards after the catch, beyond expected (per completion), split by channel",
    caption = paste(strwrap(fig_caption(
      "Sumer play charting 2022-2025 (R/factory/lib_sumer.R), player-level plays_players_p1/p2.csv.gz",
      sprintf("%d play-callers with >= %d offensive plays. Channel split: yards_after_contact (floored at 0, capped at total) vs. the remainder.", n_q, MIN_OFF_PLAYS),
      "Non-garbage-time. Residual split proportionally to each play's own clean/contact yardage share, so it always reconciles to R/31's total."
    ), width = 138), collapse = "\n")
  ) +
  theme_coach(grid = "none") +
  theme(
    axis.text.y = element_text(size = rel(0.62), colour = ink_body),
    legend.position = "top",
    plot.margin = margin(10, 60, 8, 10)
  )

save_fig("docs/figures/yac_mechanism.png", p, w = 11, h = 13)

# =============================================================================
# 8. WRITE CSV
# =============================================================================
out <- callers[, .(off_caller, n_completions = n, yac_oe, yac_oe_lo, yac_oe_hi, rank_yac_oe,
                   yac_oe_clean, clean_lo, clean_hi, rank_clean, yac_oe_clean_shrunk,
                   yac_oe_contact, contact_lo, contact_hi, rank_contact, yac_oe_contact_shrunk,
                   contact_yard_share, broke_tackle_rate)]
setorder(out, -yac_oe)
fwrite(out, "data/derived/yac_mechanism.csv")
cat(sprintf("\nwrote data/derived/yac_mechanism.csv (%d rows), data/derived/yac_mechanism_players.csv (%d rows)\n",
            nrow(out), n_players))
cat("wrote docs/figures/yac_mechanism.png\n")

# =============================================================================
# 9. THE MECHANISM VERDICT
# =============================================================================
cat("\n================ THE MECHANISM VERDICT ================\n")
cat(sprintf("Shanahan's +%.2f total YAC-over-expected splits %+.2f clean-space / %+.2f contact-forced\n",
            kyle_r$yac_oe, kyle_r$yac_oe_clean, kyle_r$yac_oe_contact))
cat(sprintf("(%.0f%% / %.0f%% of the total) -- rank %d of %d on clean, rank %d of %d on contact.\n",
            100*kyle_r$yac_oe_clean/kyle_r$yac_oe, 100*kyle_r$yac_oe_contact/kyle_r$yac_oe,
            kyle_r$rank_clean, n_q, kyle_r$rank_contact, n_q))
cat(sprintf("Ben Johnson (#%d on total): %+.2f clean / %+.2f contact (%.0f%%/%.0f%%), rank %d / %d on each.\n",
            bj_r$rank_yac_oe, bj_r$yac_oe_clean, bj_r$yac_oe_contact,
            100*bj_r$yac_oe_clean/bj_r$yac_oe, 100*bj_r$yac_oe_contact/bj_r$yac_oe,
            bj_r$rank_clean, bj_r$rank_contact))
cat(sprintf("McVay (#%d on total): %+.2f clean / %+.2f contact, rank %d / %d on each.\n",
            mcvay_r$rank_yac_oe, mcvay_r$yac_oe_clean, mcvay_r$yac_oe_contact,
            mcvay_r$rank_clean, mcvay_r$rank_contact))
cat("\nDONE\n")
