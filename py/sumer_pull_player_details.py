#!/usr/bin/env python3
"""
Pull /pro/player/details once -- the player-name lookup no prior script
touched. Needed to turn sumer_player_id into readable names for the SF
receiving-leaders and Purdy-vs-Jones checks in R/29_hawks_and_kyle.R.

Same offset-paging approach as sumer_pull_players.py and for the same
reason: every row shares one last_modified stamp, so lastModifiedSince
cannot advance a cursor; offset paging is stable. Key from ~/.Renviron,
never printed.

Out: data/raw/sumer/player_details.csv.gz
"""
import csv, gzip, json, os, sys, time, urllib.request, urllib.error

KEY = None
for line in open(os.path.expanduser("~/.Renviron")):
    if line.startswith("SUMER_API_KEY="):
        KEY = line.split("=", 1)[1].strip()
if not KEY:
    sys.exit("no SUMER_API_KEY in ~/.Renviron")

BASE = "https://data.sumersports.com"
OUT = "data/raw/sumer/player_details.csv.gz"
LIMIT = 5000
SLEEP = 0.6


def get(offset, tries=5):
    url = (f"{BASE}/pro/player/details?lastModifiedSince=2000-01-01"
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


os.makedirs("data/raw/sumer", exist_ok=True)
n, offset, writer, f = 0, 0, None, None
while True:
    rows = get(offset)
    if not rows:
        break
    if writer is None:
        cols = sorted({k for r in rows for k in r})
        f = gzip.open(OUT, "wt", newline="")
        writer = csv.DictWriter(f, fieldnames=cols, extrasaction="ignore")
        writer.writeheader()
    writer.writerows(rows)
    n += len(rows)
    offset += LIMIT
    print(f"player/details: {n:,} rows (offset {offset:,})", flush=True)
    if len(rows) < LIMIT:
        break
    time.sleep(SLEEP)
if f:
    f.close()
print(f"DONE player/details: {n:,} rows -> {OUT}", flush=True)
