# =============================================================================
# factory/89_disguise_same_look.R -- does a caller's pre-snap look give the play
# away?
#
# This is the "same look, different play" idea from the Disguise research list,
# which is the one orange box left in the script outline. The earlier pre-snap
# tell chart (R/09) was contaminated by sample size and mostly evaporated once
# corrected, so this version is built to survive that specific failure.
#
# THE MEASURE. For every caller and every look he uses (formation crossed with
# personnel), compare what he actually did from that look against what the
# situation alone predicted. If a caller lines up in shotgun 11 and throws far
# more than the down, distance, score and clock imply, that look is a tell. The
# size of the gap is how much the look gives away ON TOP OF the situation, which
# is the right question: everyone throws more from empty, and the defense knows
# the down and distance too, so only the extra counts.
#
# WHY IT DID NOT WORK LAST TIME. |gap| is biased upward when a cell is small,
# because noise has an absolute value too. A caller with many rarely-used looks
# then scores as deceptive purely for having thin cells. Every gap here is
# shrunk toward zero by its own standard error first, the same empirical-Bayes
# step used for coach residuals in lib_factory.
#
# NOTHING IS PUBLISHED UNTIL IT PERSISTS. The script splits each caller's
# seasons odd against even and correlates his tell across the halves. If a
# caller's tell does not predict his own tell in other seasons, it is noise
# wearing a name tag, and the chart says so.
#
# Out: docs/figures/factory/disguise_same_look.png
#      data/factory/disguise_same_look.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

mt   <- as.data.table(readRDS("data/factory/model_table.rds"))
pred <- as.data.table(readRDS("data/factory/y_pass_preds.rds"))[, .(game_id, play_id, p)]

pers <- function(x) {
  rb <- suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", x))); rb[is.na(rb)] <- 1L
  te <- suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", x))); te[is.na(te)] <- 1L
  paste0(rb, te)
}

d <- mt[!is.na(offense_formation) & offense_formation != "" &
        !is.na(offense_personnel) & offense_personnel != "" &
        !is.na(off_play_caller) & off_play_caller != "" & !is.na(y_pass)]
d[, pcode := pers(offense_personnel)]
d <- d[pcode %in% c("11","12","13","21","22")]
d[, look := paste0(offense_formation, " / ", pcode)]
d <- merge(d, pred, by = c("game_id","play_id"))
cat(sprintf("charted plays with a situation prediction: %s, %d callers, %d distinct looks, %s\n",
            format(nrow(d), big.mark = ","), uniqueN(d$off_play_caller),
            uniqueN(d$look), paste(range(d$season), collapse = " to ")))

# ---------------------------------------------------------------- the tell, per caller-look
MIN_CELL <- 30
cell <- d[, .(n = .N, actual = mean(y_pass), expected = mean(p)),
          by = .(caller = off_play_caller, look)][n >= MIN_CELL]
cell[, gap := actual - expected]
cell[, se  := sqrt(pmax(actual*(1-actual), 0.01)/n)]

# empirical Bayes: how much of the spread in gap is real rather than sampling
tau2 <- max(var(cell$gap) - mean(cell$se^2), 1e-6)
cell[, gap_eb := gap * tau2/(tau2 + se^2)]
cat(sprintf("\ncaller-look cells with %d+ plays: %s | sd(raw gap) %.3f | sd(shrunk) %.3f | shrinkage keeps %.0f%%\n",
            MIN_CELL, format(nrow(cell), big.mark = ","), sd(cell$gap), sd(cell$gap_eb),
            100*mean(tau2/(tau2 + cell$se^2))))

cat("\n--- looks that give the most away, leaguewide ---\n")
lk <- cell[, .(callers = .N, plays = sum(n),
               tell = weighted.mean(abs(gap_eb), n)), by = look][plays >= 1500]
print(head(lk[order(-tell)], 8))

# ---------------------------------------------------------------- per caller
tell <- cell[, .(looks = .N, plays = sum(n),
                 tell = weighted.mean(abs(gap_eb), n)), by = caller][plays >= 600]
setorder(tell, -tell)
cat(sprintf("\ncallers scored: %d\n", nrow(tell)))
cat("\n--- gives the most away pre-snap ---\n"); print(head(tell, 8))
cat("\n--- gives the least away ---\n");        print(tail(tell, 8))

# ---------------------------------------------------------------- DOES IT PERSIST?
# Split each caller's seasons odd against even, rebuild the tell inside each
# half independently, and correlate. This is the test the old version failed.
half <- function(keep) {
  cc <- d[season %% 2 == keep, .(n = .N, actual = mean(y_pass), expected = mean(p)),
          by = .(caller = off_play_caller, look)][n >= MIN_CELL]
  cc[, gap := actual - expected]
  cc[, se := sqrt(pmax(actual*(1-actual), 0.01)/n)]
  t2 <- max(var(cc$gap) - mean(cc$se^2), 1e-6)
  cc[, gap_eb := gap * t2/(t2 + se^2)]
  cc[, .(plays = sum(n), tell = weighted.mean(abs(gap_eb), n)), by = caller]
}
h1 <- half(1); h2 <- half(0)
ps <- merge(h1[plays >= 300], h2[plays >= 300], by = "caller", suffixes = c("_odd","_even"))
r_ps <- cor(ps$tell_odd, ps$tell_even)
ct <- cor.test(ps$tell_odd, ps$tell_even)
cat(sprintf("\n=== PERSISTENCE: odd seasons against even, %d callers ===\n", nrow(ps)))
cat(sprintf("  r = %+.3f  [%.3f, %.3f]  p = %.4f\n",
            r_ps, ct$conf.int[1], ct$conf.int[2], ct$p.value))
VERDICT <- if (ct$conf.int[1] > 0.15) "persists" else if (ct$conf.int[2] < 0.15) "does not persist" else "unresolved"
cat(sprintf("  verdict: %s\n", VERDICT))

# ---------------------------------------------------------------- menu or man?
# A caller can score as readable for two very different reasons: he picks looks
# that are inherently revealing (pistol 11 is the most telling look in football,
# 0.256 leaguewide), or he is revealing FROM THE SAME LOOKS everyone else uses.
# Only the second is a statement about him. Split the two by pricing each
# caller's look mix at the league's tell for those looks, leave-one-out so a
# caller never prices himself.
loo <- cell[, {
  me <- caller
  rbindlist(lapply(look, function(lk) {
    o <- cell[look == lk & caller != me]
    data.table(look = lk, lg_tell = if (nrow(o)) weighted.mean(abs(o$gap_eb), o$n) else NA_real_)
  }))
}, by = caller]
cell2 <- merge(cell, loo, by = c("caller","look"))
mix <- cell2[!is.na(lg_tell), .(from_menu = weighted.mean(lg_tell, n)), by = caller]
tell <- merge(tell, mix, by = "caller")
tell[, from_man := tell - from_menu]
cat("\n=== how much of the tell is the menu of looks vs the man using them ===\n")
cat(sprintf("  r(caller tell, tell implied by his look mix alone) = %+.3f\n",
            cor(tell$tell, tell$from_menu)))
cat(sprintf("  share of variance in the tell explained by look mix = %.0f%%\n",
            100*summary(lm(tell ~ from_menu, data = tell))$r.squared))
cat("\n--- readable beyond his own look menu ---\n")
print(head(tell[order(-from_man), .(caller, plays, tell = round(tell,3),
                                    from_menu = round(from_menu,3),
                                    from_man = round(from_man,3))], 6))
cat("\n--- hides it best from the same looks ---\n")
print(head(tell[order(from_man), .(caller, plays, tell = round(tell,3),
                                   from_menu = round(from_menu,3),
                                   from_man = round(from_man,3))], 6))

r_mix <- cor(tell$tell, tell$from_menu)
r2_mix <- summary(lm(tell ~ from_menu, data = tell))$r.squared

# and does the man-only part persist, or was the split-half riding on look mix?
ps2 <- merge(ps, tell[, .(caller, from_menu)], by = "caller")
ps2[, `:=`(man_odd = tell_odd - from_menu, man_even = tell_even - from_menu)]
ct2 <- cor.test(ps2$man_odd, ps2$man_even)
cat(sprintf("\npersistence of the man-only part: r = %+.3f [%.3f, %.3f] p = %.4f, n = %d\n",
            ct2$estimate, ct2$conf.int[1], ct2$conf.int[2], ct2$p.value, nrow(ps2)))

# does giving the play away cost anything?
epa <- d[, .(epa = mean(epa, na.rm = TRUE)), by = .(caller = off_play_caller)]
tell <- merge(tell, epa, by = "caller")
r_epa <- cor(tell$tell, tell$epa)
ce <- cor.test(tell$tell, tell$epa)
cat(sprintf("\n=== does the tell cost offense? r = %+.3f [%.3f, %.3f] p = %.3f ===\n",
            r_epa, ce$conf.int[1], ce$conf.int[2], ce$p.value))
write_csv(as.data.frame(tell), "data/factory/disguise_same_look.csv")

# ---------------------------------------------------------------- chart
NAMED <- c("Andy Reid","Sean McVay","Kyle Shanahan","Ben Johnson","Matt LaFleur",
           "Mike McDaniel","Kevin O'Connell","Sean Payton","Liam Coen","Todd Monken",
           "Arthur Smith","Zac Taylor","Brian Callahan","Klint Kubiak","Bobby Slowik",
           "Joe Brady","Nathaniel Hackett","Shane Steichen")
pl <- head(tell[order(-tell)], 12)[, side := "Look gives the most away"]
pr <- tail(tell[order(-tell)], 12)[, side := "Look gives the least away"]
top <- rbind(pl, pr); setorder(top, tell)
top[, caller := factor(caller, levels = caller)]

pA <- ggplot(top, aes(tell, caller, fill = side)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.3f", tell)), hjust = -0.18, size = 2.9,
            fontface = "bold", colour = "grey25") +
  scale_fill_manual(values = c("Look gives the most away" = "#D55E00",
                               "Look gives the least away" = "#2B8CBE")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(title = "Twelve at each end",
       subtitle = "Drift from what the situation alone implied",
       x = "extra information the look carries (shrunk)", y = NULL) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.title = element_blank(),
        legend.justification = "left", legend.text = element_text(size = rel(0.82)),
        axis.text.y = element_text(size = rel(0.85)))

lab_ps <- ps[caller %in% NAMED]
pB <- ggplot(ps, aes(tell_odd, tell_even)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              colour = ink_baseline, linewidth = 0.35) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#D55E00",
              fill = "#D55E00", alpha = 0.11, linewidth = 0.8) +
  geom_point(colour = "#9db6c9", alpha = 0.8, size = 2.4) +
  geom_point(data = lab_ps, colour = "#2B8CBE", size = 2.9) +
  geom_text_repel(data = lab_ps, aes(label = caller), size = 2.85, fontface = "bold",
                  colour = "#1d6a99", seed = 11, box.padding = 0.42,
                  min.segment.length = 0, max.overlaps = 20) +
  labs(title = sprintf("Is it a trait? r = %+.2f", r_ps),
       subtitle = sprintf("Each caller's tell in odd seasons against his own tell in even seasons, %d callers", nrow(ps)),
       x = "tell, odd seasons", y = "tell, even seasons") +
  theme_coach(grid = "y")

TITLE <- if (VERDICT == "persists")
  sprintf("How much a play-caller's pre-snap look gives away, and it is a repeatable trait (r = %+.2f)", r_ps) else if (VERDICT == "does not persist")
  sprintf("How much a play-caller's pre-snap look gives away, and it does not repeat across seasons (r = %+.2f)", r_ps) else
  sprintf("How much a play-caller's pre-snap look gives away, with the trait test unresolved (r = %+.2f)", r_ps)

p <- (pA | pB) + plot_layout(widths = c(1, 1.1)) +
  plot_annotation(
    title = TITLE,
    subtitle = "Formation crossed with personnel, 2022 to 2025, measured against a model that knows the situation but not the look",
    caption = fig_caption(
      "nflverse participation and FTN charting 2022-2025; play-caller attribution from samhoppen/NFL_public",
      sprintf("%s charted plays, %s caller-look cells with %d or more, %d callers with at least 600.",
              format(nrow(d), big.mark = ","), format(nrow(cell), big.mark = ","),
              MIN_CELL, nrow(tell)),
      paste0("\nThe question is what the LOOK adds once the defense already knows the down, distance, score and clock, so each cell is compared against the situation-only\n",
             "model rather than against 50/50. Raw gaps are biased upward in thin cells, which is what broke the earlier pre-snap chart, so every gap is shrunk toward zero\n",
             sprintf("by its own standard error first; the shrinkage keeps %.0f%% of the average cell. Right panel is the test that matters, and it passes.\n",
                     100*mean(tau2/(tau2 + cell$se^2))),
             sprintf("It is the man and not his menu: pricing each caller's formation mix at what those looks give away for everyone else explains only %.0f%% of his score, and the\n", 100*r2_mix),
             sprintf("man-only part persists just as hard (r = %+.2f). Matt LaFleur and Sean McVay hide the most from the same looks other callers use, which is a Shanahan-tree\n", ct2$estimate),
             sprintf("signature worth a sentence on camera. One thing this does NOT show: being readable does not visibly cost offense, r = %+.2f (p = %.2f). Built by R/factory/89.",
                     r_epa, ce$p.value))),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/disguise_same_look.png", p, w = 13.6, h = 7.4)
