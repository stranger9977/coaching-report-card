#!/usr/bin/env python3
"""
Pull Sumer play-level NFL data, one game at a time, into per-season CSVs.

The API is documented at https://data.sumersports.com/ (Swagger). Auth is an
X-API-Key header. /pro/sumer/plays takes gsisGameId, which is the same id
nflverse carries as old_game_id, so everything here joins straight onto the
play-by-play we already have on (gsis_game_id, gsis_play_id).

Why per game rather than the lastModifiedSince firehose: paging by
last_modified cannot be filtered to a season, so it would drag every season we
do not want. Per game is more requests but pulls exactly the window we need.

Politeness: one request at a time, SLEEP between them, and every finished game
is written to disk so a rerun skips it. Kill and restart freely.

Out: data/raw/sumer/plays_<season>.csv
     data/raw/sumer/games_<season>.csv
"""
import csv, json, os, sys, time, urllib.request, urllib.error

KEY = None
for line in open(os.path.expanduser("~/.Renviron")):
    if line.startswith("SUMER_API_KEY="):
        KEY = line.split("=", 1)[1].strip()
if not KEY:
    sys.exit("no SUMER_API_KEY in ~/.Renviron")

BASE = "https://data.sumersports.com"
OUT = "data/raw/sumer"
SLEEP = 0.35
SEASONS = [2022, 2023, 2024, 2025]

# Everything we might plausibly use, and nothing else. The full row is ~193
# fields and most of it is special teams accounting.
FIELDS = [
    "gsis_game_id", "gsis_play_id", "season", "sumer_game_id", "sumer_play_id",
    "sumer_offense_team_id", "sumer_defense_team_id",
    "quarter", "clock", "down", "distance", "field_position", "yards_to_goal_line",
    "offense_score_diff", "two_minute", "four_minute", "redzone", "goal_line",
    "short_yardage", "garbage_time", "no_huddle", "no_play", "outcome",
    # what was called
    "run_pass", "play_type", "is_dropback", "dropback_type", "play_action",
    "play_action_concept", "run_pass_option", "screen", "draw", "read_option_run",
    "option_run", "run_concept", "run_side", "pass_zone", "pass_width",
    "depth_of_target", "is_targeted_pass", "throwaway", "catchable_target",
    # the pre-snap picture
    "formation", "formation_unbalanced", "trick_look", "offensive_personnel_basic",
    "defensive_personnel_package", "quarterback_alignment", "back_alignment",
    "offensive_hash", "defensive_hash", "box_defenders", "coverage_defenders",
    # what the defense showed against what it did
    "middle_of_field_coverage_look", "middle_of_field_coverage_played",
    "coverage_scheme", "man_zone_coverage", "blitz", "schemed_blitz", "stunt",
    "pass_rush_count", "pressure", "time_to_pressure", "time_to_throw", "hurry",
    # outcome
    "expected_points_added", "epa_success_offense", "epa_success_defense",
    "offensive_yards", "first_down_gained", "is_sack", "is_interception",
    "is_complete_pass", "is_touchdown",
]


def get(path, params, tries=4):
    q = "&".join(f"{k}={v}" for k, v in params.items())
    url = f"{BASE}{path}?{q}"
    for a in range(tries):
        try:
            req = urllib.request.Request(url, headers={"X-API-Key": KEY})
            with urllib.request.urlopen(req, timeout=120) as r:
                return json.load(r)
        except urllib.error.HTTPError as e:
            if e.code in (429, 500, 502, 503, 504) and a < tries - 1:
                wait = 5 * (a + 1)
                print(f"  HTTP {e.code}, backing off {wait}s", flush=True)
                time.sleep(wait)
                continue
            raise
        except Exception:
            if a < tries - 1:
                time.sleep(5 * (a + 1))
                continue
            raise


os.makedirs(OUT, exist_ok=True)

for season in SEASONS:
    gpath = f"{OUT}/games_{season}.csv"
    games = get("/games", {"league": "NFL", "season": season})
    # week_type 3 is preseason; we only want games that count
    games = [g for g in games if g.get("week_type") != 3 and g.get("gsis_game_id")]
    games.sort(key=lambda g: (g.get("week") or 0, g.get("start") or ""))
    with open(gpath, "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(games[0].keys()))
        w.writeheader()
        w.writerows(games)
    print(f"season {season}: {len(games)} games that count", flush=True)

    ppath = f"{OUT}/plays_{season}.csv"
    done = set()
    if os.path.exists(ppath):
        with open(ppath) as f:
            for row in csv.DictReader(f):
                done.add(str(row["gsis_game_id"]))
        print(f"  resuming, {len(done)} games already on disk", flush=True)

    new = not os.path.exists(ppath)
    with open(ppath, "a", newline="") as f:
        w = csv.DictWriter(f, fieldnames=FIELDS, extrasaction="ignore")
        if new:
            w.writeheader()
        for i, g in enumerate(games, 1):
            gid = str(g["gsis_game_id"])
            if gid in done:
                continue
            plays = get("/pro/sumer/plays", {"gsisGameId": gid, "limit": 5000})
            for p in plays:
                p.setdefault("season", season)
            w.writerows(plays)
            f.flush()
            if i % 25 == 0:
                print(f"  {season} {i}/{len(games)} games", flush=True)
            time.sleep(SLEEP)
    print(f"season {season} complete -> {ppath}", flush=True)

print("done", flush=True)
