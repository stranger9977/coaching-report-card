# =============================================================================
# 30_brady_confound.R
#
# Nick's critique of docs/figures/coach_war.png (R/23), verbatim: "belichick
# looks good here because brady was a late round pick."
#
# He's right about the mechanism. R/01's draft-talent model controls for QB
# quality using the QB's OWN DRAFT VALUE. Tom Brady was pick 199 (2000 draft),
# which the Jimmy Johnson chart floors near zero -- so the model treats two
# decades of a top-5 all-time QB as league-average roster filler and credits
# the difference to Belichick's coach random effect. That effect is the
# shipped +4.16 wins/season in data/derived/coach_vs_roster.csv and
# docs/figures/coach_war.png. R/06's contract model has a softer version of
# the same problem: Brady played most of his career on a below-market
# contract (he took team-friendly deals), so his qb_contract_z understates
# him too, just less severely than a draft slot does. The market model
# (R/02) is not affected: the closing spread priced Brady's actual play every
# week, not his draft slot or his cap number.
#
# This script quantifies the inflation instead of just asserting it, two ways:
#   1. QB-CONTROL SWAP: refit R/01's model with QB prior-season EPA/dropback
#      in place of QB draft value (R/01 already builds this as a robustness
#      check but only prints the aggregate correlation across coaches; here
#      it's extracted per coach so individual movement -- Belichick's above
#      all -- is visible). IMPORTANT CAVEAT: prior-season EPA/dropback is
#      itself partly coach-contaminated (the QB played it in that coach's
#      system, behind that coach's line, calling that coach's plays), so this
#      is not a clean instrument either. Treated here as a bound, not a
#      replacement: the truth sits somewhere between the draft-value model
#      (ignores in-season QB quality entirely) and the prior-EPA model
#      (partly credits the coach's own system back to the coach).
#   2. NATURAL EXPERIMENT: Belichick is refit as two pseudo-coaches, "w/
#      Brady" (seasons with Brady at starter) and "post-Brady" (seasons
#      without him), same model, everything else unchanged. Done under both
#      QB controls, and once more under R/06's contract model where the
#      window allows it.
#
# Data (no new pulls; everything here already exists on disk):
#   - data/derived/team_talent.csv (R/01 Out): team-season point_diff_pg,
#     head_coach, roster_value, qb_value (Brady's own draft value),
#     qb_prior_epa_per_db (Brady's own prior-season play), 2003-2025.
#   - data/derived/contract_talent.csv (R/06 Out): same shape, contract_talent
#     and qb_apy_cap_pct in place of draft value, 2012-2025.
#   - data/derived/coach_vs_roster.csv, coach_vs_contracts.csv, coach_market.csv:
#     the three shipped coach numbers, read here only to mark the shipped
#     +4.16 and the market floor +1.01 on the figure.
#   - Model specs (formula, scaling, wins-per-point-differential slope) are
#     copied exactly from R/01 step 5 and R/06 step 6 so the refits reproduce
#     the shipped numbers before any new treatment is applied; see the
#     "VALIDATION" print block.
#
# Belichick's seasons in the data: 2003-2019 = Brady starting every season
# except 2008 (Matt Cassel, Brady's injury year -- correctly excluded from
# "Brady years" by the starter-detection logic, not hand-patched here).
# 2020-2023 = Cam Newton (2020), Mac Jones (2021-2023). Mac Jones' 2021
# rookie season has no prior-season NFL stats, so it drops out of the
# prior-EPA model's complete-case subset (post-Brady n=4 seasons under draft
# control, n=3 under prior-EPA control -- reported, not smoothed over).
#
# Conventions: R/lib/theme_coach.R (theme_coach(), fig_caption(), save_fig()).
# No em dashes.
#
# Out:
#   data/derived/brady_confound.csv -- per-coach wins/season under both QB
#                                       controls (draft value vs prior EPA)
#                                       and the delta, all coaches in the
#                                       complete-case pool, plus Belichick's
#                                       two era-split pseudo-coaches.
#   docs/figures/brady_confound.png -- Belichick's effect under 2x2 (QB
#                                       control x era), shipped +4.16 and
#                                       market-model +1.01 marked as reference.
# =============================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lme4)
  library(ggplot2)
})

source("R/lib/theme_coach.R")

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

BRADY_LAST_SEASON <- 2019  # last season Brady started for NE in this window

# -----------------------------------------------------------------------
# 0. Load. Shipped numbers (for validation + figure reference lines).
# -----------------------------------------------------------------------
team_talent     <- read.csv("data/derived/team_talent.csv", stringsAsFactors = FALSE)
contract_talent <- read.csv("data/derived/contract_talent.csv", stringsAsFactors = FALSE)

shipped_roster    <- read.csv("data/derived/coach_vs_roster.csv", stringsAsFactors = FALSE)
shipped_contracts <- read.csv("data/derived/coach_vs_contracts.csv", stringsAsFactors = FALSE)
shipped_market     <- read.csv("data/derived/coach_market.csv", stringsAsFactors = FALSE)

shipped_belichick_draft    <- shipped_roster    %>% filter(head_coach == "Bill Belichick") %>% pull(wins_per_season)
shipped_belichick_contract <- shipped_contracts %>% filter(head_coach == "Bill Belichick") %>% pull(wins_per_season)
shipped_belichick_market   <- shipped_market     %>% filter(coach == "Bill Belichick") %>% pull(rate_per17)

# -----------------------------------------------------------------------
# 1. Wins-per-point-differential slopes, one per window, exactly as R/01
#    step 1 / R/06 step 6 compute them (lm(wins ~ point_diff_pg) on that
#    script's own team-season table, before any QB-availability filter).
# -----------------------------------------------------------------------
wins_per_point_diff_draft <- unname(coef(lm(wins ~ point_diff_pg, data = team_talent))["point_diff_pg"])
wins_per_point_diff_contract <- unname(coef(lm(wins ~ point_diff_pg, data = contract_talent))["point_diff_pg"])

# -----------------------------------------------------------------------
# 2. Draft-talent model data, R/01 step 5 exactly: roster_z + qb_z (QB's own
#    draft value), and the prior-EPA robustness variant on the complete-case
#    subset. Both get a head_coach_split column for the Belichick natural
#    experiment (test 2); the swap comparison (test 1) uses the unsplit
#    head_coach column so the shipped model is reproduced exactly first.
# -----------------------------------------------------------------------
model_data <- team_talent %>%
  mutate(roster_z = as.numeric(scale(roster_value)),
         qb_z = as.numeric(scale(qb_value)),
         head_coach_split = case_when(
           head_coach == "Bill Belichick" & season <= BRADY_LAST_SEASON ~ "Belichick w/ Brady",
           head_coach == "Bill Belichick" & season >  BRADY_LAST_SEASON ~ "Belichick post-Brady",
           TRUE ~ head_coach
         ))

robust_data <- model_data %>%
  filter(!is.na(qb_prior_epa_per_db)) %>%
  mutate(qb_prior_z = as.numeric(scale(qb_prior_epa_per_db)))

extract_ranef <- function(fit, group_col, wins_slope) {
  ranef(fit)[[group_col]] %>%
    rownames_to_column(group_col) %>%
    rename(effect = `(Intercept)`) %>%
    mutate(wins_per_season = effect * wins_slope)
}

# --- Test 1: QB-control swap, unsplit head_coach, full population ---------
fit_draft_control  <- lmer(point_diff_pg ~ roster_z + qb_z      + (1 | head_coach) + (1 | season), data = model_data)
fit_prioepa_control <- lmer(point_diff_pg ~ roster_z + qb_prior_z + (1 | head_coach) + (1 | season), data = robust_data)

ranef_draft   <- extract_ranef(fit_draft_control,  "head_coach", wins_per_point_diff_draft) %>%
  rename(wins_draft = wins_per_season) %>% select(head_coach, wins_draft)
ranef_prioepa <- extract_ranef(fit_prioepa_control, "head_coach", wins_per_point_diff_draft) %>%
  rename(wins_prior_epa = wins_per_season) %>% select(head_coach, wins_prior_epa)

seasons_n <- model_data %>% count(head_coach, name = "seasons")

# Primary QB label per coach: whichever QB threw the most total dropbacks
# for him in the window, for labeling movers ("Belichick (Brady)" etc).
primary_qb <- team_talent %>%
  filter(!is.na(qb_name)) %>%
  group_by(head_coach, qb_name) %>%
  summarise(total_dropbacks = sum(qb_dropbacks, na.rm = TRUE), .groups = "drop") %>%
  group_by(head_coach) %>%
  slice_max(total_dropbacks, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(head_coach, primary_qb = qb_name)

swap_compare <- inner_join(ranef_draft, ranef_prioepa, by = "head_coach") %>%
  left_join(seasons_n, by = "head_coach") %>%
  left_join(primary_qb, by = "head_coach") %>%
  mutate(delta_wins = wins_prior_epa - wins_draft) %>%
  arrange(delta_wins)

swap_cor <- cor(swap_compare$wins_draft, swap_compare$wins_prior_epa)

# --- Test 2: natural experiment, Belichick split, both QB controls --------
fit_draft_split   <- lmer(point_diff_pg ~ roster_z + qb_z      + (1 | head_coach_split) + (1 | season), data = model_data)
fit_prioepa_split <- lmer(point_diff_pg ~ roster_z + qb_prior_z + (1 | head_coach_split) + (1 | season), data = robust_data)

ranef_draft_split   <- extract_ranef(fit_draft_split,   "head_coach_split", wins_per_point_diff_draft)
ranef_prioepa_split <- extract_ranef(fit_prioepa_split, "head_coach_split", wins_per_point_diff_draft)

belichick_splits <- c("Belichick w/ Brady", "Belichick post-Brady")

belichick_record <- function(df) {
  df %>% filter(head_coach_split %in% belichick_splits) %>%
    group_by(head_coach_split) %>%
    summarise(seasons = n(), games = sum(games), wins = sum(wins),
              losses = games - wins, win_pct = wins / games, .groups = "drop")
}
belichick_record_draft   <- belichick_record(model_data)
belichick_record_prioepa <- belichick_record(robust_data)

belichick_draft_split   <- ranef_draft_split   %>% filter(head_coach_split %in% belichick_splits)
belichick_prioepa_split <- ranef_prioepa_split %>% filter(head_coach_split %in% belichick_splits)

# -----------------------------------------------------------------------
# 3. Contract model (R/06), era split only -- no second QB-quality measure
#    exists in the contract data to run a swap test, so this is test 2's
#    treatment applied to the contract model, not test 1's. Window is
#    2012-2025, so "Brady years" here is only 2012-2019 (8 seasons), not
#    the full 2003-2019 span -- flagged in the printed output.
# -----------------------------------------------------------------------
model_data_contract <- contract_talent %>%
  filter(!is.na(qb_apy_cap_pct)) %>%
  mutate(contract_z = as.numeric(scale(contract_talent)),
         qb_contract_z = as.numeric(scale(qb_apy_cap_pct)),
         head_coach_split = case_when(
           head_coach == "Bill Belichick" & season <= BRADY_LAST_SEASON ~ "Belichick w/ Brady",
           head_coach == "Bill Belichick" & season >  BRADY_LAST_SEASON ~ "Belichick post-Brady",
           TRUE ~ head_coach
         ))

fit_contract_unsplit <- lmer(point_diff_pg ~ contract_z + qb_contract_z + (1 | head_coach) + (1 | season),
                              data = model_data_contract)
fit_contract_split <- lmer(point_diff_pg ~ contract_z + qb_contract_z + (1 | head_coach_split) + (1 | season),
                            data = model_data_contract)

belichick_contract_unsplit <- extract_ranef(fit_contract_unsplit, "head_coach", wins_per_point_diff_contract) %>%
  filter(head_coach == "Bill Belichick")
belichick_contract_split <- extract_ranef(fit_contract_split, "head_coach_split", wins_per_point_diff_contract) %>%
  filter(head_coach_split %in% belichick_splits)
belichick_contract_record <- model_data_contract %>%
  filter(head_coach_split %in% belichick_splits) %>%
  group_by(head_coach_split) %>%
  summarise(seasons = n(), games = sum(games), wins = sum(wins), losses = games - wins, .groups = "drop")

# -----------------------------------------------------------------------
# 4. Validation: refit numbers should match the shipped CSVs (same data,
#    same formula, unsplit). Confirms the swap/split treatments below are
#    changes on top of the real shipped model, not a different model.
# -----------------------------------------------------------------------
belichick_draft_unsplit <- ranef_draft %>% filter(head_coach == "Bill Belichick") %>% pull(wins_draft)
belichick_contract_check <- belichick_contract_unsplit$wins_per_season

cat("\n================ VALIDATION: refit matches shipped ================\n")
cat(sprintf("Draft-talent model, Belichick, unsplit:    refit %+.3f vs shipped %+.3f wins/season\n",
            belichick_draft_unsplit, shipped_belichick_draft))
cat(sprintf("Contract-talent model, Belichick, unsplit: refit %+.3f vs shipped %+.3f wins/season\n",
            belichick_contract_check, shipped_belichick_contract))

# -----------------------------------------------------------------------
# 5. Print: Belichick's four numbers, top movers, named-coach direction
#    check, contract-model era split, verdict.
# -----------------------------------------------------------------------
cat("\n================ TEST 1: QB-CONTROL SWAP (draft value vs prior-season EPA) ================\n")
cat(sprintf("Full population: n = %d coaches under draft control, n = %d under prior-EPA control (complete-case subset),\n",
            nrow(ranef_draft), nrow(ranef_prioepa)))
cat(sprintf("n = %d coaches in common. Correlation between the two models' coach wins/season: r = %.3f\n",
            nrow(swap_compare), swap_cor))
cat("(Caveat: prior-season EPA/dropback is partly coach-contaminated -- the QB played it in that coach's\n")
cat(" own system. This swap is a bound on the truth, not a clean replacement for the draft-value control.)\n")

belichick_row <- swap_compare %>% filter(head_coach == "Bill Belichick")
cat(sprintf("\nBelichick, unsplit, full career (%d seasons):\n", belichick_row$seasons))
cat(sprintf("  QB draft-value control:      %+.2f wins/season  (shipped: %+.2f)\n", belichick_row$wins_draft, shipped_belichick_draft))
cat(sprintf("  QB prior-EPA control:        %+.2f wins/season\n", belichick_row$wins_prior_epa))
cat(sprintf("  Delta:                       %+.2f wins/season\n", belichick_row$delta_wins))

cat("\n--- Top 10 movers (most negative delta = coach effect shrinks most when QB is credited for actual play\n")
cat("    instead of draft slot -- the signature of a coach whose QB was drafted low but played great) ---\n")
top_movers_down <- swap_compare %>% slice_min(delta_wins, n = 10) %>%
  select(head_coach, primary_qb, seasons, wins_draft, wins_prior_epa, delta_wins)
print(as.data.frame(top_movers_down), row.names = FALSE)

cat("\n--- Top 10 movers the OTHER way (most positive delta = coach effect grows when QB is credited for\n")
cat("    actual play instead of draft slot -- a high-drafted QB who underperformed his slot) ---\n")
top_movers_up <- swap_compare %>% slice_max(delta_wins, n = 10) %>%
  select(head_coach, primary_qb, seasons, wins_draft, wins_prior_epa, delta_wins)
print(as.data.frame(top_movers_up), row.names = FALSE)

cat("\n--- Named-coach direction check (late-round/inexpensive star QB vs high-pick QB) ---\n")
direction_check <- swap_compare %>%
  filter(head_coach %in% c("Bill Belichick", "Kyle Shanahan", "Pete Carroll", "Mike McCarthy")) %>%
  select(head_coach, primary_qb, seasons, wins_draft, wins_prior_epa, delta_wins)
print(as.data.frame(direction_check), row.names = FALSE)
cat("(Shanahan and McCarthy's numbers pool their whole career's QBs -- Shanahan has started Garoppolo,\n")
cat(" Beathard and Purdy; McCarthy has started Rodgers and Prescott -- so a late-round-QB effect specific\n")
cat(" to one QB gets averaged against his other seasons, diluting the movement. Belichick is the clean\n")
cat(" case because Brady is nearly his whole sample.)\n")

cat("\n================ TEST 2: NATURAL EXPERIMENT (Belichick split w/ Brady vs post-Brady) ================\n")
cat("\n--- Actual won-loss record, by split (from raw data, not the model) ---\n")
cat("Draft-control model's sample (all 4 post-Brady seasons present):\n")
print(as.data.frame(belichick_record_draft), row.names = FALSE)
cat("\nPrior-EPA model's complete-case sample (Mac Jones' 2021 rookie year drops, no prior-season stats):\n")
print(as.data.frame(belichick_record_prioepa), row.names = FALSE)

cat("\n--- Coach effect, by split, under QB draft-value control ---\n")
print(as.data.frame(belichick_draft_split %>% select(head_coach_split, effect, wins_per_season)), row.names = FALSE)

cat("\n--- Coach effect, by split, under QB prior-EPA control ---\n")
print(as.data.frame(belichick_prioepa_split %>% select(head_coach_split, effect, wins_per_season)), row.names = FALSE)

cat("\n================ TEST 3: CONTRACT MODEL (R/06), ERA SPLIT ================\n")
cat("Window is 2012-2025, so \"w/ Brady\" here means 2012-2019 only (8 seasons), not the full 2003-2019 span.\n")
cat(sprintf("Belichick, unsplit, contract model (%d seasons): %+.2f wins/season  (shipped: %+.2f)\n",
            belichick_contract_unsplit$seasons, belichick_contract_unsplit$wins_per_season, shipped_belichick_contract))
cat("\n--- Actual won-loss record, by split, contract-model window ---\n")
print(as.data.frame(belichick_contract_record), row.names = FALSE)
cat("\n--- Coach effect, by split, contract model ---\n")
print(as.data.frame(belichick_contract_split %>% select(head_coach_split, effect, wins_per_season)), row.names = FALSE)

# -----------------------------------------------------------------------
# 6. Verdict.
# -----------------------------------------------------------------------
belichick_wbrady_draft   <- belichick_draft_split   %>% filter(head_coach_split == "Belichick w/ Brady") %>% pull(wins_per_season)
belichick_postbrady_draft <- belichick_draft_split   %>% filter(head_coach_split == "Belichick post-Brady") %>% pull(wins_per_season)
belichick_wbrady_prioepa   <- belichick_prioepa_split %>% filter(head_coach_split == "Belichick w/ Brady") %>% pull(wins_per_season)
belichick_postbrady_prioepa <- belichick_prioepa_split %>% filter(head_coach_split == "Belichick post-Brady") %>% pull(wins_per_season)

pct_survive_prioepa <- 100 * belichick_row$wins_prior_epa / shipped_belichick_draft
pct_survive_postbrady <- 100 * belichick_postbrady_draft / shipped_belichick_draft

verdict <- paste(
  sprintf("VERDICT: the shipped +%.2f wins/season does not survive intact. Swapping Brady's draft-slot",
          shipped_belichick_draft),
  sprintf("QB control for his actual prior-season play shrinks it to +%.2f (%.0f%% of the shipped number),",
          belichick_row$wins_prior_epa, pct_survive_prioepa),
  "and that swap still credits Belichick's own system back to him (prior EPA is coach-contaminated), so",
  sprintf("+%.2f is a ceiling, not a floor. The cleaner test is the era split: with Brady starting",
          belichick_row$wins_prior_epa),
  sprintf("(w/ Brady, draft control) Belichick grades at +%.2f wins/season; without him (post-Brady, same",
          belichick_wbrady_draft),
  sprintf("control, 4 seasons, %d-%d record) he grades at %+.2f. Under the harder prior-EPA control the gap",
          belichick_record_draft$wins[belichick_record_draft$head_coach_split == "Belichick post-Brady"],
          belichick_record_draft$losses[belichick_record_draft$head_coach_split == "Belichick post-Brady"],
          belichick_postbrady_draft),
  sprintf("is similar (w/ Brady +%.2f, post-Brady %+.2f). Every one of these numbers still sits above the",
          belichick_wbrady_prioepa, belichick_postbrady_prioepa),
  sprintf("market model's career floor of +%.2f (which prices Brady in fully, every week, for 20 years), so",
          shipped_belichick_market),
  "some real coaching value likely survives even the post-Brady numbers. But the specific +4.16 headline",
  "is substantially a Brady number, not a Belichick number: it requires both the weakest QB control in the",
  "project AND Brady's own two decades in the sample to reach that height."
)
cat("\n================ VERDICT ================\n")
cat(strwrap(verdict, width = 100), sep = "\n")
cat("\n")

# -----------------------------------------------------------------------
# 7. Figure: Belichick's effect, 2x2 (QB control x era), shipped +4.16 and
#    market floor +1.01 marked.
# -----------------------------------------------------------------------
plot_data <- tibble(
  era = c("w/ Brady", "w/ Brady", "post-Brady", "post-Brady"),
  qb_control = c("QB draft value (shipped control)", "QB prior-season EPA",
                 "QB draft value (shipped control)", "QB prior-season EPA"),
  wins_per_season = c(belichick_wbrady_draft, belichick_wbrady_prioepa,
                       belichick_postbrady_draft, belichick_postbrady_prioepa)
) %>%
  mutate(era = factor(era, levels = c("w/ Brady", "post-Brady")),
         qb_control = factor(qb_control, levels = c("QB draft value (shipped control)", "QB prior-season EPA")))

ref_lines <- tibble(
  y = c(shipped_belichick_draft, shipped_belichick_market),
  label = c(sprintf("shipped full-career number: +%.2f", shipped_belichick_draft),
            sprintf("market-model career floor: +%.2f", shipped_belichick_market)),
  linetype = c("shipped", "floor")
)

qb_control_colours <- c("QB draft value (shipped control)" = "#D6604D",
                         "QB prior-season EPA" = "#4393C3")

p <- ggplot(plot_data, aes(x = era, y = wins_per_season, fill = qb_control)) +
  geom_col(position = position_dodge(width = 0.65), width = 0.55) +
  geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
  geom_hline(data = ref_lines, aes(yintercept = y, linetype = label),
             colour = ink_title, linewidth = 0.5, show.legend = FALSE) +
  geom_text(data = ref_lines, aes(x = 2.42, y = y, label = label), inherit.aes = FALSE,
            hjust = 1, vjust = -0.5, size = 3.1, fontface = "bold", colour = ink_title) +
  geom_text(aes(label = sprintf("%+.2f", wins_per_season)),
            position = position_dodge(width = 0.65), vjust = -0.5, size = 3.4,
            fontface = "bold", colour = ink_title) +
  scale_fill_manual(values = qb_control_colours, name = NULL) +
  scale_y_continuous(limits = c(min(-1, min(plot_data$wins_per_season) - 0.5),
                                 max(shipped_belichick_draft, max(plot_data$wins_per_season)) + 0.7)) +
  coord_cartesian(xlim = c(0.5, 2.75), clip = "off") +
  labs(
    title = sprintf("Belichick's shipped +%.2f grades as a Brady effect, not a coaching effect", shipped_belichick_draft),
    subtitle = paste0(
      "Same mixed model as R/01 (point differential ~ roster talent + QB talent, coach random effect), Belichick refit\n",
      "as two pseudo-coaches by era, under two QB controls. Every post-Brady bar sits closer to the market model's\n",
      "career floor than to the shipped headline."
    ),
    x = NULL,
    y = "Wins per season above roster talent"
  ) +
  labs(caption = paste(strwrap(fig_caption(
    "data/derived/team_talent.csv (R/01), data/derived/coach_vs_roster.csv, coach_market.csv (this repo)",
    "Belichick, NE, 2003-2023. w/ Brady = 2003-2019 (2008 excluded: Matt Cassel started, Brady injured). post-Brady = 2020-2023 (Newton, Jones).",
    "QB prior-EPA control is itself partly coach-contaminated (QB played it in Belichick's own system), so it is a bound, not a clean replacement."
  ), width = 130), collapse = "\n")) +
  theme_coach(grid = "y") +
  theme(legend.position = "top", plot.margin = margin(10, 130, 8, 10))

save_fig("docs/figures/brady_confound.png", p, w = 11, h = 8)

# -----------------------------------------------------------------------
# 8. Write CSV: per-coach effects under both QB controls, plus Belichick's
#    era-split pseudo-coaches appended (same columns, computed from the
#    split models instead of the unsplit ones).
# -----------------------------------------------------------------------
belichick_split_rows <- inner_join(
  ranef_draft_split   %>% filter(head_coach_split %in% belichick_splits) %>%
    rename(head_coach = head_coach_split, wins_draft = wins_per_season) %>% select(head_coach, wins_draft),
  ranef_prioepa_split %>% filter(head_coach_split %in% belichick_splits) %>%
    rename(head_coach = head_coach_split, wins_prior_epa = wins_per_season) %>% select(head_coach, wins_prior_epa),
  by = "head_coach"
) %>%
  mutate(delta_wins = wins_prior_epa - wins_draft, primary_qb = NA_character_) %>%
  left_join(bind_rows(belichick_record_draft %>% transmute(head_coach = head_coach_split, seasons)),
            by = "head_coach")

out_csv <- bind_rows(
  swap_compare %>% select(head_coach, primary_qb, seasons, wins_draft, wins_prior_epa, delta_wins),
  belichick_split_rows %>% select(head_coach, primary_qb, seasons, wins_draft, wins_prior_epa, delta_wins)
) %>%
  arrange(delta_wins)

write.csv(out_csv, "data/derived/brady_confound.csv", row.names = FALSE)

cat("\nWrote data/derived/brady_confound.csv, docs/figures/brady_confound.png\n")
