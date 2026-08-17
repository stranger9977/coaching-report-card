# =============================================================================
# factory/98_outline_charts.R -- charts built to answer specific outline lines.
#
# Michael's outline asks several things nothing on the board answered yet.
# These are built to sit directly under his own headings on the outline tab.
#
#   "What is 11 Personnel / What is 13 Personnel"      -> personnel primer
#   "How does this do against lighter boxes?"          -> box counts by grouping
#   "Matching Personnel"                               -> does the D respond?
#   "Seahawks defense struggled vs the Rams but their
#    offense shredded them"                            -> the four meetings
#   "Ben Johnson: stats on his success, what sets him
#    apart" / same for Kyle Shanahan                   -> caller profiles
#   "Running on early downs, teams still do this too
#    much"                                             -> early-down rates
#   "Play Action"                                      -> rate vs what the
#                                                         model expects
#
# Out: docs/figures/factory/outline_personnel.png
#      docs/figures/factory/outline_boxes.png
#      docs/figures/factory/outline_sea_lar.png
#      docs/figures/factory/outline_callers.png
#      docs/figures/factory/outline_early_downs.png
#      docs/figures/factory/outline_pa_rate.png
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
mt <- as.data.table(readRDS("data/factory/model_table.rds"))
pers <- function(x) {
  rb <- suppressWarnings(as.integer(sub(".*?([0-9]+) RB.*", "\\1", x))); rb[is.na(rb)] <- 1L
  te <- suppressWarnings(as.integer(sub(".*?([0-9]+) TE.*", "\\1", x))); te[is.na(te)] <- 1L
  paste0(rb, te)
}
d <- mt[!is.na(offense_personnel) & offense_personnel != ""][, p := pers(offense_personnel)]
MAIN <- c("11","12","13","21","22")
lab_p <- c("11" = "11\n1 back, 1 TE\n3 receivers",
           "12" = "12\n1 back, 2 TE\n2 receivers",
           "13" = "13\n1 back, 3 TE\n1 receiver",
           "21" = "21\n2 backs, 1 TE\n2 receivers",
           "22" = "22\n2 backs, 2 TE\n1 receiver")

# ---------------------------------------------------------------- 1. primer
pr <- d[p %in% MAIN, .(plays = .N, share = 100*.N/nrow(d[p %in% MAIN]),
                       pass = 100*mean(y_pass), epa = mean(epa, na.rm = TRUE)), by = p]
setorder(pr, -share); pr[, plab := lab_p[p]]
pr[, plab := factor(plab, levels = plab)]
cat("=== personnel primer ===\n"); print(pr)

p1 <- ggplot(pr, aes(plab, share)) +
  geom_col(aes(fill = p == "11"), width = 0.62) +
  geom_text(aes(label = sprintf("%.1f%%", share)), vjust = -0.6, size = 4,
            fontface = "bold", colour = "grey25") +
  geom_text(aes(y = 3, label = sprintf("passes %.0f%%\n%+.3f EPA", pass, epa)),
            size = 3.1, colour = "white", fontface = "bold", lineheight = 1.05) +
  scale_fill_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#7f97a8"), guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 78)) +
  labs(title = "What the personnel groupings are, and how often anyone uses them",
       subtitle = "Share of charted snaps, with pass rate and EPA per play inside each bar, 2022 to 2025",
       x = NULL, y = "share of snaps",
       caption = fig_caption("nflverse participation personnel groupings",
         sprintf("%s charted plays in these five groupings.", format(sum(pr$plays), big.mark=",")),
         "\nThe first number is running backs, the second is tight ends; receivers make up the rest of the five skill players. Built by R/factory/98.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/outline_personnel.png", p1, w = 11, h = 5.8)

# ---------------------------------------------------------------- 2. boxes
part <- rbindlist(lapply(2022:2025, function(y)
  fread(file.path(NFLA, sprintf("data/pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id","play_id","defenders_in_box"), showProgress = FALSE)))
setnames(part, "nflverse_game_id", "game_id")
db <- merge(d, part, by = c("game_id","play_id"), all.x = TRUE)
db <- db[!is.na(defenders_in_box) & p %in% MAIN]
bx <- db[, .(plays = .N, box = mean(defenders_in_box),
             light = 100*mean(defenders_in_box <= 6),
             epa_light = mean(epa[defenders_in_box <= 6], na.rm = TRUE),
             epa_heavy = mean(epa[defenders_in_box >= 8], na.rm = TRUE)), by = p]
setorder(bx, -plays); bx[, plab := lab_p[p]]
cat("\n=== defenders in the box by personnel (his 'lighter boxes' question) ===\n")
print(bx)

bl <- melt(bx[, .(plab, `Light box (6 or fewer)` = epa_light, `Heavy box (8 or more)` = epa_heavy)],
           id.vars = "plab", variable.name = "box", value.name = "epa")
bl[, plab := factor(plab, levels = bx$plab)]
p2 <- ggplot(bl, aes(plab, epa, fill = box)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.7), width = 0.62) +
  geom_text(aes(label = sprintf("%+.02f", epa),
                vjust = ifelse(epa >= 0, -0.5, 1.4)),
            position = position_dodge(width = 0.7), size = 3.2, fontface = "bold",
            colour = "grey25") +
  geom_text(data = bx, aes(plab, y = -0.13, label = sprintf("avg box %.1f", box)),
            inherit.aes = FALSE, size = 3, colour = "grey40") +
  scale_fill_manual(values = c("Light box (6 or fewer)" = "#2B8CBE",
                               "Heavy box (8 or more)" = "#D55E00")) +
  labs(title = "Heavy personnel draws a heavy box, and the offence does better when the box stays light",
       subtitle = "EPA per play by personnel grouping against how many defenders were in the box, 2022 to 2025",
       x = NULL, y = "EPA per play",
       caption = fig_caption("nflverse participation, defenders_in_box",
         sprintf("%s plays with a charted box count.", format(nrow(db), big.mark=",")),
         "\nAnswers the outline's 'how does this do against lighter boxes, would assume better'. It does, in every grouping. The catch is that 13 personnel is the least likely to see one. Built by R/factory/98.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")
save_fig("docs/figures/factory/outline_boxes.png", p2, w = 11, h = 6)

# ---------------------------------------------------------------- 3. SEA v LA
gm <- fread(file.path(NFLA, "data/games.csv"), showProgress = FALSE)
mm <- gm[season %in% 2024:2025 & !is.na(result) &
         ((home_team == "SEA" & away_team == "LA") | (home_team == "LA" & away_team == "SEA"))]
pl <- mt[game_id %in% mm$game_id & posteam %in% c("SEA","LA")]
sm <- pl[, .(plays = .N, epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE)),
         by = .(game_id, posteam)]
sm <- merge(sm, mm[, .(game_id, season, week, home_team, home_score, away_team, away_score)], by = "game_id")
sm[, label := paste0(season, " wk", week)]
sm[, side := fifelse(posteam == "SEA", "Seattle offence", "Rams offence")]
setorder(sm, season, week)
sm[, label := factor(label, levels = unique(label))]
cat("\n=== Seattle and the Rams, 2024-2025 ===\n"); print(sm[, .(label, posteam, plays, epa = round(epa,3))])

p3 <- ggplot(sm, aes(label, epa, fill = side)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = sprintf("%+.02f", epa), vjust = ifelse(epa >= 0, -0.5, 1.4)),
            position = position_dodge(width = 0.7), size = 3.4, fontface = "bold",
            colour = "grey25") +
  scale_fill_manual(values = c("Seattle offence" = "#1c5b80", "Rams offence" = "#D55E00")) +
  labs(title = "Seattle and the Rams: both offences played well in every meeting",
       subtitle = "EPA per play by each offence in the four Seahawks-Rams games of 2024 and 2025",
       x = NULL, y = "EPA per play",
       caption = fig_caption("nflverse play-by-play",
         paste(sprintf("%s: %s %d, %s %d", sm[!duplicated(game_id)]$label,
                       mm$home_team, mm$home_score, mm$away_team, mm$away_score), collapse = "  |  "),
         "\nAnswers the outline's note that Seattle's defence struggled against the Rams while their offence shredded them. Built by R/factory/98.")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", legend.title = element_blank(), legend.justification = "left")
save_fig("docs/figures/factory/outline_sea_lar.png", p3, w = 11, h = 5.8)

# ---------------------------------------------------------------- 4. callers
cal <- mt[!is.na(off_play_caller) & off_play_caller != "" & season >= 2022,
          .(plays = .N, epa = mean(epa, na.rm = TRUE), sr = mean(success, na.rm = TRUE),
            pass = 100*mean(y_pass), pa = 100*mean(y_pa, na.rm = TRUE),
            motion = 100*mean(y_motion, na.rm = TRUE)),
          by = off_play_caller][plays >= 1200]
FEAT <- c("Ben Johnson","Kyle Shanahan","Sean McVay","Andy Reid","Matt LaFleur","Liam Coen")
cal[, feat := off_play_caller %in% FEAT]
cat("\n=== featured callers ===\n"); print(cal[feat == TRUE][order(-epa)])

p4 <- ggplot(cal, aes(pa, epa)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  geom_point(aes(size = plays), colour = "#9db6c9", alpha = 0.6) +
  geom_point(data = cal[feat == TRUE], aes(size = plays), colour = "#D55E00") +
  geom_text_repel(data = cal[feat == TRUE], aes(label = off_play_caller), size = 3.2,
                  fontface = "bold", colour = "#8a3d00", seed = 3,
                  box.padding = 0.6, min.segment.length = 0, max.overlaps = 20) +
  scale_size_continuous(range = c(1.6, 5), guide = "none") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Play-action rate against offensive output, by play-caller",
       subtitle = "Every caller with at least 1,200 charted plays, 2022 to 2025",
       x = "share of dropbacks using play action", y = "EPA per play",
       caption = fig_caption("nflverse play-by-play and FTN charting 2022 to 2025",
         sprintf("%d play-callers. Point size is snaps called.", nrow(cal)),
         "\nFor the outline's Ben Johnson and Kyle Shanahan sections. Built by R/factory/98.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/outline_callers.png", p4, w = 11, h = 6.4)

# ---------------------------------------------------------------- 5. early downs
ed <- mt[down %in% 1:2 & season >= 2015]
edt <- ed[, .(run = 100*(1 - mean(y_pass)), epa_run = mean(epa[y_pass == 0], na.rm = TRUE),
              epa_pass = mean(epa[y_pass == 1], na.rm = TRUE)), by = season]
edt[, gap := epa_pass - epa_run]
cat("\n=== early downs (1st and 2nd) by season ===\n"); print(edt)

p5 <- ggplot(edt, aes(season)) +
  geom_col(aes(y = run), fill = "#7f97a8", width = 0.62) +
  geom_text(aes(y = run, label = sprintf("%.0f%%", run)), vjust = -0.6, size = 3.2,
            fontface = "bold", colour = "grey25") +
  geom_line(aes(y = gap*200), colour = "#D55E00", linewidth = 1) +
  geom_point(aes(y = gap*200), colour = "#D55E00", size = 2) +
  geom_text(aes(y = gap*200, label = sprintf("%+.02f", gap)), vjust = -1.1, size = 3,
            fontface = "bold", colour = "#8a3d00") +
  scale_y_continuous(name = "share of early-down plays that are runs",
                     labels = function(x) paste0(x, "%"), limits = c(0, 62),
                     sec.axis = sec_axis(~ ./200, name = "EPA advantage of passing over running")) +
  scale_x_continuous(breaks = seq(2015, 2025, 2)) +
  labs(title = "Teams still run on early downs about 45% of the time, and passing is still worth more",
       subtitle = "Grey bars are the early-down run rate. The orange line is how much more EPA a pass produced than a run that season.",
       x = NULL,
       caption = fig_caption("nflverse play-by-play, first and second down, 2015 to 2025",
         sprintf("%s early-down plays.", format(nrow(ed), big.mark = ",")),
         "\nFor the outline's 'running on early downs, teams are still doing this too much'. The gap has never closed and the run rate has barely moved. Built by R/factory/98.")) +
  theme_coach(grid = "y") +
  theme(axis.title.y.right = element_text(colour = "#8a3d00"),
        axis.text.y.right = element_text(colour = "#8a3d00"))
save_fig("docs/figures/factory/outline_early_downs.png", p5, w = 11.5, h = 6)

# ---------------------------------------------------------------- 6. PA rate vs expected
pa <- readRDS("data/factory/fits/y_pa.rds")
cr <- as.data.table(pa$residuals$career); setnames(cr, 1, "caller")
sea <- as.data.table(pa$residuals$season)
act <- mt[!is.na(y_pa) & !is.na(off_play_caller) & off_play_caller != "",
          .(plays = .N, actual = 100*mean(y_pa)), by = .(caller = off_play_caller)]
cr <- merge(cr, act, by = "caller")[plays >= 600]
cr[, expected := actual - resid]
setorder(cr, -resid)
cat("\n=== play action: called vs what the situation expected ===\n")
print(head(cr[, .(caller, plays, actual = round(actual,1), expected = round(expected,1),
                  resid = round(resid,1))], 8))

lab6 <- cr[caller %in% c(FEAT, "Sean Payton","Mike McDaniel","Klint Kubiak","Zac Taylor")]
p6 <- ggplot(cr, aes(expected, actual)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_point(colour = "#9db6c9", alpha = 0.65, size = 2.2) +
  geom_point(data = lab6, colour = "#D55E00", size = 2.8) +
  geom_text_repel(data = lab6, aes(label = caller), size = 3.1, fontface = "bold",
                  colour = "#8a3d00", seed = 6, box.padding = 0.5,
                  min.segment.length = 0, max.overlaps = 22) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(title = "Play action: how often each caller used it against how often the situation called for it",
       subtitle = "Above the line means more play action than the game state predicts. This model cleared all five rubric rules.",
       x = "expected play-action rate from the situation alone",
       y = "actual play-action rate",
       caption = fig_caption("Situation-only play-action model, season-grouped out-of-sample, 2022 to 2025",
         sprintf("%d play-callers with at least 600 charted dropbacks.", nrow(cr)),
         "\nFor the outline's Play Action section: both the rate and what the model expected, so the gap is the caller's own choice rather than his schedule. Built by R/factory/98.")) +
  theme_coach(grid = "y")
save_fig("docs/figures/factory/outline_pa_rate.png", p6, w = 11, h = 6.4)
