#!/usr/bin/env python3
"""
Pull the two Sumer endpoints the first passes never touched:

  /pro/sumer/plays/players  -> data/raw/sumer/plays_players.csv
      122 per-player-per-play fields (route, alignment, bunched_alignment,
      off_on_los_alignment, press, primary_coverage, coverage_responsibility,
      substitution_in/out, per-player EPA splits, coarse_grade, ...).
      ~22 rows per play, so expect millions of rows.
  /pro/sumer/matchups       -> data/raw/sumer/matchups.csv

Same offset-paging approach as sumer_pull_full.py and for the same reason:
every row carries one shared last_modified stamp, so the documented
lastModifiedSince cursor cannot advance; offset paging is stable (ordered by
last_modified with a key tiebreaker). Key from ~/.Renviron, never printed.
"""
import csv, gzip, json, os, sys, time, urllib.request, urllib.error

KEY = None
for line in open(os.path.expanduser("~/.Renviron")):
    if line.startswith("SUMER_API_KEY="):
        KEY = line.split("=", 1)[1].strip()
if not KEY:
    sys.exit("no SUMER_API_KEY in ~/.Renviron")

BASE = "https://data.sumersports.com"
LIMIT = 5000
SLEEP = 0.6


def get(path, offset, tries=5):
    url = (f"{BASE}{path}?lastModifiedSince=2000-01-01"
           f"&offset={offset}&limit={LIMIT}")
    for a in range(tries):
        try:
            req = urllib.request.Request(url, headers={"X-API-Key": KEY})
            with urllib.request.urlopen(req, timeout=300) as r:
                return json.load(r)
        except urllib.error.HTTPError as e:
            if e.code in (429, 500, 502, 503, 504) and a < tries - 1:
                wait = 8 * (a + 1)
                print(f"  HTTP {e.code} at offset {offset}, waiting {wait}s", flush=True)
                time.sleep(wait); continue
            raise
        except Exception:
            if a < tries - 1:
                time.sleep(8 * (a + 1)); continue
            raise


def pull(path, out, start_offset=0, header_from=None):
    """Write gzip-compressed CSV. start_offset resumes a partial pull into a
    new part file; header_from forces the column order to match part 1 so the
    parts concatenate cleanly."""
    n, offset, writer, f = 0, start_offset, None, None
    while True:
        batch = get(path, offset)
        rows = batch if isinstance(batch, list) else batch.get("data", batch.get("items", []))
        if not rows:
            break
        if writer is None:
            if header_from:
                opener = gzip.open if header_from.endswith(".gz") else open
                with opener(header_from, "rt") as h:
                    cols = h.readline().rstrip("\n").split(",")
            else:
                cols = sorted({k for r in rows for k in r})
            f = gzip.open(out, "wt", newline="")
            writer = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
            writer.writeheader()
        writer.writerows(rows)
        n += len(rows)
        offset += LIMIT
        if n % 100000 < LIMIT:
            print(f"{path}: {n:,} rows (offset {offset:,})", flush=True)
        if len(rows) < LIMIT:
            break
        time.sleep(SLEEP)
    if f:
        f.close()
    print(f"DONE {path}: {n:,} rows -> {out}", flush=True)


os.makedirs("data/raw/sumer", exist_ok=True)
# Part 1 (offsets 0 to 2,539,866) was pulled uncompressed on 2026-08-18, then
# trimmed of one kill-truncated row and gzipped in place as plays_players_p1.csv.gz.
# Resume from there; the loader reads both parts.
P1 = "data/raw/sumer/plays_players_p1.csv.gz"
RESUME_AT = 2539866
if os.path.exists(P1):
    pull("/pro/sumer/plays/players", "data/raw/sumer/plays_players_p2.csv.gz",
         start_offset=RESUME_AT, header_from=P1)
else:
    pull("/pro/sumer/plays/players", "data/raw/sumer/plays_players_p1.csv.gz")
pull("/pro/sumer/matchups", "data/raw/sumer/matchups.csv.gz")
