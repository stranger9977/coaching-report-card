#!/usr/bin/env python3
"""
fetch_staffs.py -- real coordinator titles for every team-season, from Wikipedia.

Why this exists: the nflverse playcaller file names whoever CALLS the plays, so
a head coach who calls his own offense is listed as his own coordinator and his
actual coordinators never appear. That made the coaching-tree metric biased
against exactly the coaches it should reward: Sean McVay's file shows no
offensive coordinator at all from 2017 on, hiding Matt LaFleur, Kevin O'Connell
and Liam Coen.

Every "YYYY <Team> season" article on Wikipedia carries an Infobox NFL team
season with coach, off_coach and def_coach fields, which are the TITLES. This
pulls them for every franchise-season and writes one row per team-season.

Out: data/raw/wiki_staffs.csv
"""
import csv, json, re, time, urllib.parse, urllib.request, os, sys

API = "https://en.wikipedia.org/w/api.php"
UA = "coaching-report-card/1.0 (research; contact via github.com/stranger9977)"
YEARS = range(2000, 2026)

# franchise names as Wikipedia titles them, with the era changes that matter
def team_names(year):
    t = [
        "Arizona Cardinals", "Atlanta Falcons", "Baltimore Ravens", "Buffalo Bills",
        "Carolina Panthers", "Chicago Bears", "Cincinnati Bengals", "Cleveland Browns",
        "Dallas Cowboys", "Denver Broncos", "Detroit Lions", "Green Bay Packers",
        "Houston Texans", "Indianapolis Colts", "Jacksonville Jaguars", "Kansas City Chiefs",
        "Miami Dolphins", "Minnesota Vikings", "New England Patriots", "New Orleans Saints",
        "New York Giants", "New York Jets", "Philadelphia Eagles", "Pittsburgh Steelers",
        "San Francisco 49ers", "Seattle Seahawks", "Tampa Bay Buccaneers", "Tennessee Titans",
    ]
    t.append("Los Angeles Rams" if year >= 2016 else "St. Louis Rams")
    t.append("Los Angeles Chargers" if year >= 2017 else "San Diego Chargers")
    t.append("Las Vegas Raiders" if year >= 2020 else "Oakland Raiders")
    if year >= 2022:   t.append("Washington Commanders")
    elif year >= 2020: t.append("Washington Football Team")
    else:              t.append("Washington Redskins")
    if year < 2002:
        t = [x for x in t if x != "Houston Texans"]
    return t

def api(params):
    params = dict(params, format="json", formatversion="2")
    url = API + "?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=45) as r:
        return json.load(r)

FIELD = re.compile(r"^\s*\|\s*(coach|off_coach|def_coach|st_coach)\s*=\s*(.+?)\s*$", re.M)
# the Staff section lists the whole staff: "*Offensive coordinator - [[Name]]"
STAFF = re.compile(r"^\*\s*([^\n\-\u2013\u2014]{3,60}?)\s*[\-\u2013\u2014]\s*(.+?)\s*$", re.M)
GROUP = re.compile(r"^\|\s*([A-Za-z /]+?)\s*=\s*$", re.M)
def clean(v):
    v = re.sub(r"<ref.*?(/>|</ref>)", "", v, flags=re.S)
    v = re.sub(r"\{\{[^}]*\}\}", "", v)
    v = re.sub(r"\[\[(?:[^\]|]*\|)?([^\]|]*)\]\]", r"\1", v)   # [[A|B]] -> B, [[A]] -> A
    v = re.sub(r"<br\s*/?>", "; ", v)
    v = re.sub(r"<[^>]+>", "", v)
    v = v.replace("'''", "").replace("''", "").strip(" *;")
    return v.strip()

def main():
    out, info, missing = [], [], []
    titles = [(y, t, f"{y} {t} season") for y in YEARS for t in team_names(y)]
    print(f"fetching {len(titles)} team-seasons", file=sys.stderr)
    for i in range(0, len(titles), 20):
        batch = titles[i:i + 20]
        d = None
        for attempt in range(4):
            try:
                d = api({"action": "query", "prop": "revisions", "rvprop": "content",
                         "rvslots": "main", "redirects": "1",
                         "titles": "|".join(b[2] for b in batch)})
                break
            except Exception as e:
                print(f"  retry {attempt+1}: {e}", file=sys.stderr); time.sleep(2 + 3 * attempt)
        if d is None:
            missing.extend(b[2] for b in batch); continue
        pages = {p.get("title"): p for p in d.get("query", {}).get("pages", [])}
        norm = {r["from"]: r["to"] for r in d.get("query", {}).get("normalized", [])}
        redir = {r["from"]: r["to"] for r in d.get("query", {}).get("redirects", [])}
        for y, team, title in batch:
            key = redir.get(norm.get(title, title), norm.get(title, title))
            p = pages.get(key)
            if not p or "revisions" not in p:
                missing.append(title); continue
            wt = p["revisions"][0]["slots"]["main"]["content"]
            f = {k: clean(v) for k, v in FIELD.findall(wt[:9000])}
            info.append({"season": y, "team_name": team,
                         "head_coach": f.get("coach", ""), "off_coord": f.get("off_coach", ""),
                         "def_coord": f.get("def_coach", ""), "st_coord": f.get("st_coach", "")})
            # the full staff section, position coaches included
            si = wt.find("==Staff")
            if si < 0: si = wt.find("== Staff")
            if si >= 0:
                sec = wt[si:si + 9000]
                for role, name in STAFF.findall(sec):
                    nm = clean(name)
                    rl = clean(role)
                    if not nm or len(nm) > 60 or "=" in nm: continue
                    nm = re.split(r"\s*[;(]", nm)[0].strip()
                    if not re.match(r"^[A-Z][A-Za-z.'\- ]+$", nm): continue
                    out.append({"season": y, "team_name": team, "role": rl, "name": nm})
        print(f"  {min(i+20, len(titles))}/{len(titles)}", file=sys.stderr)
        time.sleep(0.7)
    os.makedirs("data/raw", exist_ok=True)
    with open("data/raw/wiki_team_seasons.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["season", "team_name", "head_coach", "off_coord", "def_coord", "st_coord"])
        w.writeheader(); w.writerows(info)
    with open("data/raw/wiki_staffs.csv", "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["season", "team_name", "role", "name"])
        w.writeheader(); w.writerows(out)
    print(f"team-seasons: {len(info)} ({len(missing)} articles missing)")
    print(f"staff rows: {len(out)} covering {len({(r['season'], r['team_name']) for r in out})} team-seasons")
    filled = sum(1 for r in info if r["off_coord"])
    print(f"infobox offensive coordinator filled on {filled} of {len(info)}")

if __name__ == "__main__":
    main()
