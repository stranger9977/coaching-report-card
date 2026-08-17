# =============================================================================
# factory/00_features.R -- the one modeling table everything else reads.
#
# Nick's spec, 15 Aug: "given a game state, predict different targets... down
# distance yardline (redzone is an important part of the field, inside the 10
# too) score differential, game clock, 2 minute and 4 minute is important too."
#
# The design rule that makes the residuals mean anything: the BASE feature set
# is situation only. Nothing about who is on the field, nothing about talent.
# That is deliberate, and it is Eric Eager's framing via Michael: "I would
# start w what would the average coach do given situation and take the
# residual." If personnel goes in the base model, the residual stops being
# "what this coach chose" and becomes "what this coach chose beyond his
# roster", which is a different and much harder quantity.
#
# Talent and quarterback controls live in `ctrl_*` columns instead. They are
# never in the base model. Experiments can add them, and stage two of the
# analysis uses them to ask whether a coach's deviation is really his.
#
# Targets are built here too so every model shares one row universe and one
# definition of a called play.
#
# Out: data/factory/model_table.rds  (one row per called play, 2015-2025)
#      data/factory/feature_spec.rds (which columns are state / ctrl / target)
# =============================================================================

suppressMessages({
  library(data.table); library(readr)
})

NFLA <- "/Users/nick/stranger9977/nfl-analysis"
D <- file.path(NFLA, "data")
OUT <- "data/factory"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

SEASONS <- 2015:2025
FTN_YRS <- 2022:2025   # charting-based targets only exist here

# ---------------------------------------------------------------- base play-by-play
keep <- c("game_id","play_id","season","week","posteam","defteam","home_team",
          "season_type","qtr","down","ydstogo","yardline_100","score_differential",
          "game_seconds_remaining","half_seconds_remaining","posteam_timeouts_remaining",
          "defteam_timeouts_remaining","vegas_wp","wp","wpa","epa","success",
          "qb_dropback","rush","qb_kneel","qb_spike","play_type","drive",
          "passer_player_name","passer_player_id","spread_line","total_line")
pbp <- as.data.table(readRDS(file.path(D, "pbp_slim.rds")))
pbp <- pbp[season %in% SEASONS, intersect(keep, names(pbp)), with = FALSE]

# A "called play" = the coach chose run or pass. Kneels and spikes are clock
# management, not play calls, so they are out.
pbp[, called := (qb_dropback == 1 | rush == 1) &
      (is.na(qb_kneel) | qb_kneel != 1) & (is.na(qb_spike) | qb_spike != 1)]

# ---------------------------------------------------------------- game state
# Field position gets explicit flags because the field is not linear: the red
# zone and the 10 change what is possible, not just how far away you are.
pbp[, `:=`(
  is_redzone      = as.integer(yardline_100 <= 20),
  is_inside10     = as.integer(yardline_100 <= 10),
  is_goal_to_go   = as.integer(yardline_100 <= ydstogo),
  is_own_deep     = as.integer(yardline_100 >= 90),
  # Clock: the two-minute and four-minute situations are their own regimes.
  is_2min         = as.integer(half_seconds_remaining <= 120),
  is_4min         = as.integer(qtr == 4 & game_seconds_remaining <= 240),
  is_half_end     = as.integer(qtr == 2 & half_seconds_remaining <= 300),
  is_second_half  = as.integer(qtr >= 3),
  # Score state, both raw and shaped. One score is the decision boundary.
  score_abs       = abs(score_differential),
  is_one_score    = as.integer(abs(score_differential) <= 8),
  is_trailing     = as.integer(score_differential < 0),
  is_blowout      = as.integer(abs(score_differential) > 16),
  # Timeout asymmetry matters more than either count alone.
  to_diff         = posteam_timeouts_remaining - defteam_timeouts_remaining,
  # Pre-game expectation, so the model knows a bad team is chasing all day.
  spread_posteam  = fifelse(posteam == home_team, -spread_line, spread_line),
  is_playoff      = as.integer(season_type == "POST")
)]

# Leverage: how much this snap can still move the outcome. Used for slicing and
# for the high-leverage question, not as a feature (it is a function of state).
pbp[, leverage := 4 * vegas_wp * (1 - vegas_wp)]

STATE <- c("down","ydstogo","yardline_100","score_differential","score_abs",
           "game_seconds_remaining","half_seconds_remaining","qtr",
           "posteam_timeouts_remaining","defteam_timeouts_remaining","to_diff",
           "is_redzone","is_inside10","is_goal_to_go","is_own_deep",
           "is_2min","is_4min","is_half_end","is_second_half",
           "is_one_score","is_trailing","is_blowout","spread_posteam","vegas_wp")

# ---------------------------------------------------------------- play callers
pc <- fread(file.path(D, "playcallers.csv"),
            select = c("season","week","team","game_id","off_play_caller",
                       "def_play_caller","head_coach"), showProgress = FALSE)
pc[, `:=`(off_play_caller = trimws(off_play_caller),
          def_play_caller = trimws(def_play_caller),
          head_coach = trimws(head_coach))]
pbp <- merge(pbp, pc, by.x = c("game_id","season","week","posteam"),
             by.y = c("game_id","season","week","team"), all.x = TRUE)
# Defensive caller is the OTHER team's defensive caller on this snap.
pcd <- pc[, .(game_id, defteam = team, def_caller_opp = def_play_caller)]
pbp <- merge(pbp, pcd, by = c("game_id","defteam"), all.x = TRUE)

# ---------------------------------------------------------------- charting targets
ftn <- rbindlist(lapply(FTN_YRS, function(y)
  fread(file.path(D, sprintf("ftn_charting_%d.csv.gz", y)),
        select = c("nflverse_game_id","nflverse_play_id","is_motion",
                   "is_play_action","is_rpo","is_no_huddle","is_screen_pass",
                   "n_blitzers"), showProgress = FALSE)))
setnames(ftn, c("nflverse_game_id","nflverse_play_id"), c("game_id","play_id"))
part <- rbindlist(lapply(FTN_YRS, function(y)
  fread(file.path(D, sprintf("pbp_participation_%d.csv.gz", y)),
        select = c("nflverse_game_id","play_id","defense_man_zone_type",
                   "offense_formation","offense_personnel","was_pressure"),
        showProgress = FALSE)))
setnames(part, "nflverse_game_id", "game_id")
pbp <- merge(pbp, ftn,  by = c("game_id","play_id"), all.x = TRUE)
pbp <- merge(pbp, part, by = c("game_id","play_id"), all.x = TRUE)

pbp[, `:=`(
  y_pass    = as.integer(qb_dropback == 1),
  y_motion  = as.integer(is_motion),
  y_pa      = as.integer(is_play_action),
  y_rpo     = as.integer(is_rpo),
  y_blitz   = fifelse(is.na(n_blitzers), NA_integer_, as.integer(n_blitzers > 0)),
  y_man     = fifelse(defense_man_zone_type == "MAN_COVERAGE", 1L,
              fifelse(defense_man_zone_type == "ZONE_COVERAGE", 0L, NA_integer_))
)]

mt <- pbp[called == TRUE & down %in% 1:4 & !is.na(yardline_100) &
          !is.na(score_differential) & !is.na(down) & !is.na(ydstogo)]
cat(sprintf("called plays %d-%d: %s\n", min(SEASONS), max(SEASONS),
            format(nrow(mt), big.mark = ",")))
for (v in c("y_pass","y_motion","y_pa","y_rpo","y_blitz","y_man")) {
  cat(sprintf("  %-8s n=%9s  rate=%.3f\n", v,
              format(sum(!is.na(mt[[v]])), big.mark = ","), mean(mt[[v]], na.rm = TRUE)))
}

saveRDS(mt, file.path(OUT, "model_table.rds"))
saveRDS(list(state = STATE,
             targets = c("y_pass","y_motion","y_pa","y_rpo","y_blitz","y_man"),
             ftn_years = FTN_YRS, seasons = SEASONS),
        file.path(OUT, "feature_spec.rds"))
cat("\nwrote", file.path(OUT, "model_table.rds"), "\n")
