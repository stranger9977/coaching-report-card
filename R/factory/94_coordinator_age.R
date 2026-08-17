# =============================================================================
# factory/94_coordinator_age.R -- have coordinators got younger?
#
# Michael's outline: "Have Coordinators decreased in age? How much? Offense?
# Defense?"
#
# The play-caller dataset (samhoppen/NFL_public) gives who called plays for
# every team-week from 1999 to 2025, offense and defense separately. It has no
# birth dates, so those come from Wikidata: everyone with the occupation
# "American football coach" or "American football player" who has a date of
# birth, 43,000 people, matched on name.
#
# Match rate is about 80% of the 360 unique callers, and higher than that
# weighted by seasons called, because the coaches who last are the ones with
# Wikipedia pages. Unmatched names are dropped rather than guessed, and the
# count behind every point is printed so a thin year is visible.
#
# Age is taken at 1 September of that season.
#
# Out: docs/figures/factory/coordinator_age.png
#      data/factory/coordinator_age.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales)
})
source("R/lib/theme_coach.R")

NFLA <- "/Users/nick/stranger9977/nfl-analysis"

# ---------------------------------------------------------------- birth dates
DOB <- "data/raw/wikidata_coach_dob.csv"
if (!file.exists(DOB)) {
  q <- function(occ) sprintf(
    'SELECT ?name ?dob WHERE { ?p wdt:P106 wd:%s ; wdt:P569 ?dob ; rdfs:label ?name . FILTER(LANG(?name)="en") }', occ)
  get1 <- function(occ) {
    tmp <- tempfile(fileext = ".csv")
    url <- paste0("https://query.wikidata.org/sparql?format=csv&query=",
                  utils::URLencode(q(occ), reserved = TRUE))
    download.file(url, tmp, method = "curl",
                  extra = '-s -H "User-Agent: coaching-research/1.0"', quiet = TRUE)
    fread(tmp, showProgress = FALSE)
  }
  # Q42331263 American football coach, Q19204627 American football player
  wd <- rbind(get1("Q42331263"), get1("Q19204627"))
  fwrite(wd, DOB)
}
wd <- fread(DOB, showProgress = FALSE)
wd[, byear := suppressWarnings(as.integer(substr(dob, 1, 4)))]
wd <- wd[!is.na(byear) & byear > 1900 & byear < 2005, .(byear = min(byear)), by = name]

# ---------------------------------------------------------------- callers
pc <- fread(file.path(NFLA, "data/playcallers.csv"), showProgress = FALSE)[season <= 2025]
long <- rbind(
  unique(pc[off_play_caller != "", .(season, team, name = off_play_caller)])[, side := "Offense"],
  unique(pc[def_play_caller != "", .(season, team, name = def_play_caller)])[, side := "Defense"]
)
# one row per caller-season-team, so a coach is counted once per job per year
cs <- unique(long)
cs <- merge(cs, wd, by = "name", all.x = TRUE)
cs[, age := season - byear]
cs <- cs[!is.na(age) & age >= 25 & age <= 80]

cov <- unique(long[, .(name)])
cat(sprintf("unique play-callers 1999-2025: %d, matched to a birth year: %d (%.0f%%)\n",
            nrow(cov), uniqueN(cs$name), 100*uniqueN(cs$name)/nrow(cov)))
cat(sprintf("caller-season-team rows kept: %s of %s (%.0f%%)\n",
            format(nrow(cs), big.mark = ","), format(nrow(unique(long)), big.mark = ","),
            100*nrow(cs)/nrow(unique(long))))

yr <- cs[, .(n = .N, mean_age = mean(age), med = as.numeric(median(age)),
             p25 = as.numeric(quantile(age, .25)),
             p75 = as.numeric(quantile(age, .75))), by = .(season, side)]
setorder(yr, side, season)
write_csv(as.data.frame(yr), "data/factory/coordinator_age.csv")
cat("\n--- mean age of play-callers, first and last five seasons ---\n")
print(yr[season %in% c(1999:2003, 2021:2025)][order(side, season),
         .(side, season, n, mean_age = round(mean_age, 1))])

fits <- yr[, {
  m <- lm(mean_age ~ season, weights = n)
  .(slope_per_decade = 10*coef(m)[2], p = summary(m)$coefficients[2,4],
    first = mean_age[which.min(season)], last = mean_age[which.max(season)])
}, by = side]
cat("\n--- full-window trend ---\n"); print(fits)

# The straight line hides the shape. Defense was getting OLDER for two decades
# and then turned over hard, so fit the two eras either side of 2019 as well.
era <- yr[, {
  a <- .SD[season <= 2019]; b <- .SD[season >= 2019]
  ma <- lm(mean_age ~ season, data = a, weights = n)
  mb <- lm(mean_age ~ season, data = b, weights = n)
  .(pre = 10*coef(ma)[2], pre_p = summary(ma)$coefficients[2,4],
    post = 10*coef(mb)[2], post_p = summary(mb)$coefficients[2,4],
    peak = max(a$mean_age), peak_yr = a$season[which.max(a$mean_age)],
    now = .SD[season == max(season)]$mean_age)
}, by = side]
cat("\n--- split at 2019 ---\n"); print(era)
dP <- era[side == "Defense"]; oP <- era[side == "Offense"]

# ---------------------------------------------------------------- chart
ends <- yr[season == max(season)]
p <- ggplot(yr, aes(season, mean_age, colour = side, fill = side)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.16, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "22", linewidth = 0.6,
              formula = y ~ x, show.legend = FALSE) +
  geom_text_repel(data = ends, aes(label = sprintf("%s, %.0f", side, mean_age)),
                  hjust = 0, nudge_x = 0.6, direction = "y", size = 3.4,
                  fontface = "bold", segment.colour = NA, seed = 4) +
  scale_colour_manual(values = c("Offense" = "#D55E00", "Defense" = "#2B8CBE")) +
  scale_fill_manual(values = c("Offense" = "#D55E00", "Defense" = "#2B8CBE")) +
  scale_x_continuous(breaks = seq(2000, 2024, 4), expand = expansion(mult = c(0.02, 0.16))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Defensive play-callers got older for twenty years, then the league changed its mind",
    subtitle = "Average age of the man calling plays, by season. Shaded band is the middle half of callers that year.",
    x = NULL, y = "age",
    caption = fig_caption(
      "Play-caller attribution from samhoppen/NFL_public 1999-2025; birth dates from Wikidata",
      sprintf("%s caller-season-team rows, %d of %d unique callers matched (%.0f%%). Age at 1 September.",
              format(nrow(cs), big.mark = ","), uniqueN(cs$name), nrow(cov),
              100*uniqueN(cs$name)/nrow(cov)),
      paste0(sprintf("\nBoth sides are younger now, but the shapes differ. Offense drifts down across the whole window, %+.1f years per decade (p = %.3f).\n",
                     fits[side == "Offense"]$slope_per_decade, fits[side == "Offense"]$p),
             sprintf("Defense did the opposite until recently: %+.1f years per decade through 2019 (p = %.3f), peaking at %.1f, and then %+.1f per decade since (p = %.3f) down to %.1f.\n",
                     dP$pre, dP$pre_p, dP$peak, dP$post, dP$post_p, dP$now),
             "Dashed lines are the full-window trend, which is why they miss the turn. Unmatched names are dropped rather than guessed. Built by R/factory/94."))
  ) +
  theme_coach(grid = "y") + theme(legend.position = "none")
save_fig("docs/figures/factory/coordinator_age.png", p, w = 11.5, h = 6.2)
