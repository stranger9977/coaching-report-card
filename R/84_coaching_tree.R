# =============================================================================
# 84_coaching_tree.R -- the coaching tree as a measurable thing.
#
# The ask: "is there maybe a coaching tree component here like how many people
# have branched off that coach kind of like a seniority award of sorts like a
# signal that their philosophy is being widely adopted across the league?"
#
# WHAT COUNTS AS A BRANCH. The playcaller file names three men per team-season:
# head coach, offensive play-caller, defensive play-caller. So a branch is a
# coordinator who called plays for a head coach and later became a head coach
# somewhere else. That undercounts real trees, which run through position
# coaches the file does not name, and it cannot see a coordinator who was hired
# away and failed to reach a head job. It is the tree the data can see.
#
# THE HONEST PROBLEM, tested rather than waved at: a tree grows with time in
# the chair. A coach with fourteen seasons has had more coordinators than one
# with four, so the raw count is partly a seniority award, which is what the
# question suspected. Both are reported: the raw count and the count net of
# what his tenure alone predicts.
#
# AND WHETHER IT MEANS ANYTHING: does a big tree go with winning, and does it
# predict the mentor's own next season? Both are computed, and the second is
# the one that decides whether this belongs on the report card.
#
# Out: data/derived/coaching_tree.csv, docs/figures/coaching_tree.png
# =============================================================================

suppressMessages({ library(data.table); library(readr); library(ggplot2); library(scales); library(ggrepel) })
source("R/lib/theme_coach.R")
NFLA <- "/Users/nick/stranger9977/nfl-analysis/data"

pc <- fread(file.path(NFLA, "playcallers.csv"),
            select = c("season", "week", "team", "head_coach", "off_play_caller", "def_play_caller"))
for (k in c("head_coach", "off_play_caller", "def_play_caller")) pc[, (k) := trimws(get(k))]
ts <- unique(pc[, .(season, team, head_coach, off_play_caller, def_play_caller)])[, .SD[1], by = .(season, team)]

# every head-coaching spell, so we know when a man first ran his own team
hcs <- unique(ts[, .(season, team, coach = head_coach)])
setorder(hcs, coach, season)
first_hc <- hcs[, .(first_hc = min(season), hc_seasons = uniqueN(season), teams = uniqueN(team)), by = coach]

# a staff pairing: a coordinator who called plays under a head coach
staff <- rbind(ts[off_play_caller != head_coach, .(season, team, mentor = head_coach, pupil = off_play_caller, side = "offense")],
               ts[def_play_caller != head_coach, .(season, team, mentor = head_coach, pupil = def_play_caller, side = "defense")])
staff <- staff[pupil != "" & mentor != ""]
# a branch: the pupil later became a head coach, somewhere other than under him
pu <- merge(staff, first_hc[, .(pupil = coach, pupil_first_hc = first_hc)], by = "pupil")
branch <- pu[pupil_first_hc > season, .(under_mentor_from = min(season), branched = min(pupil_first_hc)),
             by = .(mentor, pupil, side)]
tree <- branch[, .(branches = uniqueN(pupil), branch_names = paste(sort(unique(pupil)), collapse = ", ")), by = mentor]
tree <- merge(first_hc[, .(mentor = coach, hc_seasons, first_hc)], tree, by = "mentor", all.x = TRUE)
tree[is.na(branches), `:=`(branches = 0L, branch_names = "")]
cat(sprintf("%d head coaches, %d have produced at least one head coach\n", nrow(tree), tree[branches > 0, .N]))
cat("\nbiggest trees:\n"); print(tree[order(-branches)][1:12, .(mentor, hc_seasons, branches, branch_names)])

# a tree grows with time in the chair: net the count of what tenure predicts
m <- glm(branches ~ hc_seasons, family = poisson, data = tree)
tree[, expected_branches := predict(m, tree, type = "response")]
tree[, tree_resid := branches - expected_branches]
cat(sprintf("\nbranches per season in the chair: %.2f; correlation of raw count with tenure: %.2f\n",
            coef(m)["hc_seasons"], cor(tree$branches, tree$hc_seasons)))
cat("\nbiggest trees for their tenure:\n")
print(tree[order(-tree_resid)][1:12, .(mentor, hc_seasons, branches, expected = round(expected_branches, 1), net = round(tree_resid, 1))])

# does it go with winning, and does it predict the mentor's next season?
ps <- fread("data/derived/card_lines_seasons.csv")
career <- ps[, .(wat = mean(wat17, na.rm = TRUE), wins = mean(act17, na.rm = TRUE), seasons = .N), by = coach]
tw <- merge(tree, career, by.x = "mentor", by.y = "coach")
cat(sprintf("\ntree size vs career wins above talent: raw r = %.2f, net of tenure r = %.2f (n = %d)\n",
            cor(tw$branches, tw$wat, use = "complete.obs"), cor(tw$tree_resid, tw$wat, use = "complete.obs"), nrow(tw)))
# the predictive test: branches produced BEFORE season t against what he did in t
setorder(ps, coach, season)
bp <- branch[, .(mentor, branched)]
pred <- ps[, .(coach, season, wat17, act17)]
pred[, branches_so_far := sapply(seq_len(.N), function(i) bp[mentor == coach[i] & branched <= season[i], .N]), by = coach]
pred <- merge(pred, ps[, .(coach, season, prior = NULL)], by = c("coach", "season"))
pr <- pred[season >= 2015 & !is.na(wat17)]
lmt <- summary(lm(wat17 ~ branches_so_far, pr))$coefficients
cat(sprintf("branches produced so far, against the mentor's own wins above talent that season: %+.3f per branch (t = %.1f, p = %.2f, n = %d)\n",
            lmt["branches_so_far", 1], lmt["branches_so_far", 3], lmt["branches_so_far", 4], nrow(pr)))
write_csv(as.data.frame(tree[order(-branches)]), "data/derived/coaching_tree.csv")

# ---------------------------------------------------------------- figure
ACT <- unique(pc[season == 2026 & week == 1]$head_coach)
tp <- tree[branches > 0 | mentor %in% ACT][order(-branches)][1:26]
tp[, lab := sprintf("%s%s", mentor, fifelse(mentor %in% ACT, "", " (not coaching in 2026)"))]
p <- ggplot(tp, aes(x = branches, y = reorder(lab, branches))) +
  geom_segment(aes(x = 0, xend = branches, yend = reorder(lab, branches)), colour = "grey85", linewidth = 3) +
  geom_point(aes(colour = mentor %in% ACT), size = 3.2) +
  geom_point(aes(x = expected_branches), shape = 124, size = 4, colour = "grey45") +
  geom_text(aes(label = sprintf("%d in %d seasons", branches, hc_seasons)), hjust = 0, nudge_x = 0.15, size = 2.9, colour = "grey35") +
  scale_colour_manual(values = c(`TRUE` = "#2B8CBE", `FALSE` = "grey55"),
                      labels = c(`TRUE` = "head coach in 2026", `FALSE` = "not coaching in 2026"), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.45))) +
  labs(title = "The coaching tree the data can see: coordinators who called plays for a man and later ran their own team",
       subtitle = paste0("A branch is a play-caller who worked under a head coach and afterwards became a head coach himself. The tick is how many branches his\n",
                         "time in the chair alone predicts, so a mark to the right of its tick is a tree bigger than seniority explains. Position coaches are not in\n",
                         "the playcaller file, so real trees are larger than this; what is here is comparable across coaches, which is what matters for ranking."),
       x = "coordinators who became head coaches", y = NULL,
       caption = fig_caption("nflverse playcaller file, head coach and both coordinators per team-season",
         sprintf("\nTree size correlates %.2f with a coach's career wins above talent, and %.2f once tenure is netted out. Branches produced so far are worth %+.3f wins above\ntalent to the mentor in the seasons that follow (p = %.2f), which is why this is on the card as a badge rather than a graded line. Built by R/84.",
                 cor(tw$branches, tw$wat, use = "complete.obs"), cor(tw$tree_resid, tw$wat, use = "complete.obs"),
                 lmt["branches_so_far", 1], lmt["branches_so_far", 4]))) +
  theme_coach(grid = "none") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/coaching_tree.png", p, w = 12, h = 8)
cat("\nOut: coaching_tree.csv, coaching_tree.png\n")
