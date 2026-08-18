#!/usr/bin/env python3
"""
Re-pull Sumer NFL plays with ALL 193 fields, via offset paging.

The first pass (sumer_pull.py) kept a hand-picked 74-column subset and dropped
14 fields that turn out to matter for coach-level work: run_gap_intent_side
against run_gap_outcome_side, lead_run, split_run, the jet/end-around/reverse
family, dropback_side, quarterback_scramble, center_block_direction, looper.

Paging note. Every row currently carries the same last_modified stamp, so the
documented lastModifiedSince cursor cannot advance through the set. Offset
paging is the documented alternative and is stable because results are ordered
by last_modified with a key tiebreaker.

Cost: ~37 requests for the whole 2022-2025 set, against 1,140 for the per-game
version. Kinder to their gateway and gets more data.

Out: data/raw/sumer/plays_full.csv  (all fields, all seasons the API holds)
"""
import csv, json, os, sys, time, urllib.request, urllib.error

KEY = None
for line in open(os.path.expanduser("~/.Renviron")):
    if line.startswith("SUMER_API_KEY="):
        KEY = line.split("=", 1)[1].strip()
if not KEY:
    sys.exit("no SUMER_API_KEY in ~/.Renviron")

BASE = "https://data.sumersports.com"
OUT = "data/raw/sumer/plays_full.csv"
LIMIT = 5000
SLEEP = 0.6


def get(offset, tries=4):
    url = (f"{BASE}/pro/sumer/plays?lastModifiedSince=2000-01-01"
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
        except Exception as e:
            if a < tries - 1:
                print(f"  {type(e).__name__} at offset {offset}, retrying", flush=True)
                time.sleep(8 * (a + 1)); continue
            raise


offset, total, writer, fh, fields = 0, 0, None, None, None
seen = set()
while True:
    rows = get(offset)
    if not rows:
        break
    if writer is None:
        fields = list(rows[0].keys())
        fh = open(OUT, "w", newline="")
        writer = csv.DictWriter(fh, fieldnames=fields, extrasaction="ignore")
        writer.writeheader()
        print(f"{len(fields)} fields", flush=True)
    # offset paging can overlap if the tiebreaker is not perfectly stable;
    # de-duplicate on the play key rather than trusting it
    fresh = []
    for r in rows:
        k = (r.get("sumer_game_id"), r.get("sumer_play_id"))
        if k in seen:
            continue
        seen.add(k); fresh.append(r)
    writer.writerows(fresh)
    fh.flush()
    total += len(fresh)
    offset += LIMIT
    if offset % 25000 == 0 or len(rows) < LIMIT:
        print(f"  offset {offset}: {total:,} unique rows", flush=True)
    if len(rows) < LIMIT:
        break
    time.sleep(SLEEP)

if fh:
    fh.close()
print(f"done: {total:,} rows -> {OUT}", flush=True)
