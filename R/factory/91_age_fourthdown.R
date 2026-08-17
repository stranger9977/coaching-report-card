# =============================================================================
# factory/91_age_fourthdown.R -- does a coach's age predict how aggressive he is
# on fourth down?
#
# Nick, 17 Aug: "relationship between age and 4th down aggressiveness would be a
# cool view add that."
#
# The measure of aggression is the one from R/08: on fourth downs where the
# nfl4th model says go by at least 1.5 win-probability points, how often did the
# coach actually go? Those are the spots nobody serious argues about, and they
# are the ones coaches get wrong. The league went 37.8% in 2018 and 60.5% in
# 2025.
#
# THAT TREND IS THE WHOLE PROBLEM. The league got far more aggressive over eight
# seasons, and the coaches hired during those years were younger than the ones
# they replaced. So a raw age-against-aggression plot would mostly be measuring
# the calendar. Everything here is expressed against the league average in the
# SAME season, and the regression carries season fixed effects.
#
# The second confound is that being older and being the same guy are tangled up:
# a 62-year-old in the data is usually a long-tenured coach, not a coach who
# aged. So the last thing this does is ask whether an individual coach's own
# aggression changes as HE ages, with his own average taken out. That is the
# only version that is about aging rather than about who gets hired.
#
# Birth years come from the same Wikidata pull as R/factory/94, with a small
# supplemental lookup for the head coaches the bulk query missed.
#
# Out: docs/figures/factory/age_fourthdown.png
#      data/factory/age_fourthdown.csv
# =============================================================================

suppressMessages({
  library(data.table); library(dplyr); library(readr); library(ggplot2)
  library(ggrepel); library(scales); library(patchwork)
})
source("R/lib/theme_coach.R")

# ---------------------------------------------------------------- birth years
wd <- fread("data/raw/wikidata_coach_dob.csv", showProgress = FALSE)
wd[, byear := suppressWarnings(as.integer(substr(dob, 1, 4)))]
wd <- wd[!is.na(byear) & byear > 1900 & byear < 2005, .(byear = min(byear)), by = name]

fd <- fread("data/derived/fourth_down_decisions.csv", showProgress = FALSE)
missing <- setdiff(unique(fd$coach), wd$name)

# The bulk SPARQL query filters on occupation, so a few coaches whose Wikidata
# item lacks that statement fall out. Look those up by name against the entity
# API and cache, rather than typing birth years in from memory.
EXTRA <- "data/raw/wikidata_coach_dob_extra.csv"
if (length(missing) && !file.exists(EXTRA)) {
  ua <- '-s -H "User-Agent: coaching-research/1.0"'
  got <- rbindlist(lapply(missing, function(nm) {
    t1 <- tempfile(fileext = ".json")
    download.file(paste0("https://www.wikidata.org/w/api.php?action=wbsearchentities",
                         "&format=json&language=en&limit=1&search=",
                         utils::URLencode(nm, reserved = TRUE)),
                  t1, method = "curl", extra = ua, quiet = TRUE)
    s <- jsonlite::fromJSON(t1)$search
    if (is.null(s) || !length(s)) return(NULL)
    Sys.sleep(0.4)
    t2 <- tempfile(fileext = ".json")
    download.file(paste0("https://www.wikidata.org/w/api.php?action=wbgetclaims",
                         "&format=json&property=P569&entity=", s$id[1]),
                  t2, method = "curl", extra = ua, quiet = TRUE)
    cl <- jsonlite::fromJSON(t2, simplifyVector = FALSE)$claims$P569
    if (is.null(cl) || !length(cl)) return(NULL)
    data.table(name = nm, qid = s$id[1], desc = s$description[1],
               dob = substr(cl[[1]]$mainsnak$datavalue$value$time, 2, 11))
  }))
  if (nrow(got)) fwrite(got, EXTRA)
}
if (file.exists(EXTRA)) {
  ex <- fread(EXTRA, showProgress = FALSE)
  cat("supplemental birth dates pulled from the Wikidata entity API:\n"); print(ex)
  wd <- rbind(wd, ex[, .(name, byear = as.integer(substr(dob, 1, 4)))])
}

fd <- merge(fd, wd, by.x = "coach", by.y = "name", all.x = TRUE)
fd[, age := season - byear]
still <- unique(fd[is.na(age)]$coach)
cat(sprintf("\nhead coaches with no birth year, dropped rather than guessed: %s (%d of %s decisions)\n",
            paste(still, collapse = ", "), nrow(fd[is.na(age)]), format(nrow(fd), big.mark = ",")))
fd <- fd[!is.na(age) & age >= 30 & age <= 80]

# ---------------------------------------------------------------- season-adjusted aggression
go <- fd[verdict == "should go"]
lg <- go[, .(lg_rate = mean(went)), by = season]
go <- merge(go, lg, by = "season")
go[, above := went - lg_rate]

cs <- go[, .(n = .N, rate = mean(went), above = mean(above), age = mean(age),
             team = names(sort(table(posteam), decreasing = TRUE))[1]),
         by = .(coach, season)][n >= 8]
cat(sprintf("\ncoach-seasons with at least 8 clear go-for-it spots: %d, %d coaches, %s decisions\n",
            nrow(cs), uniqueN(cs$coach), format(sum(cs$n), big.mark = ",")))

# ---------------------------------------------------------------- does age predict it?
m0 <- lm(above ~ age, data = cs, weights = n)
m1 <- lm(went ~ age + factor(season), data = go)
cat("\n--- season-adjusted rate on age (coach-seasons, weighted by opportunities) ---\n")
cat(sprintf("  %+.1f points of go rate per decade of age (p = %.4f, n = %d)\n",
            100*10*coef(m0)[2], summary(m0)$coefficients[2,4], nrow(cs)))
cat("--- play level, season fixed effects ---\n")
cat(sprintf("  %+.1f points per decade (p = %.4f, n = %s plays)\n",
            100*10*coef(m1)["age"], summary(m1)$coefficients["age",4],
            format(nrow(go), big.mark = ",")))

# within-coach: his own aggression against his own average, as he ages
wc <- cs[, if (.N >= 4) .SD, by = coach]
wc[, `:=`(age_c = age - mean(age), above_c = above - mean(above)), by = coach]
m2 <- lm(above_c ~ 0 + age_c, data = wc, weights = n)
cat(sprintf("\n--- within-coach (%d coaches with 4+ qualifying seasons, own averages removed) ---\n",
            uniqueN(wc$coach)))
cat(sprintf("  %+.1f points per decade of his own aging (p = %.4f)\n",
            100*10*coef(m2)[1], summary(m2)$coefficients[1,4]))

cat(sprintf("\ncorrelation between age and season-adjusted go rate: %+.3f | spread across coach-seasons: %.1f points sd\n",
            cor(cs$age, cs$above), 100*sd(cs$above)))

per_dec  <- 100*10*coef(m0)[2]
p_dec    <- summary(m0)$coefficients[2,4]
r_age    <- cor(cs$age, cs$above)
sd_cs    <- 100*sd(cs$above)
per_dec2 <- 100*10*coef(m1)["age"]
p_dec2   <- summary(m1)$coefficients["age",4]
w_dec    <- 100*10*coef(m2)[1]
p_w      <- summary(m2)$coefficients[1,4]

# ---------------------------------------------------------------- buckets
cs[, bucket := cut(age, c(29, 39, 44, 49, 54, 59, 80),
                   labels = c("under 40","40-44","45-49","50-54","55-59","60+"))]
bk <- cs[, {
  w <- n / sum(n)
  mu <- sum(w * above)
  se <- sqrt(sum(w^2 * (above - mu)^2) * .N/(.N-1))
  .(coaches = uniqueN(coach), seasons = .N, plays = sum(n), mu = mu, se = se)
}, by = bucket][order(bucket)]
cat("\n--- go rate above the league that season, by age bucket ---\n")
print(bk[, .(bucket, coaches, seasons, plays, above = sprintf("%+.1f pts", 100*mu))])
write_csv(as.data.frame(cs), "data/factory/age_fourthdown.csv")

# ---------------------------------------------------------------- career view
career <- go[, .(n = .N, above = mean(above), age = mean(age),
                 first = min(season), last = max(season)), by = coach][n >= 40]
setorder(career, -above)
cat("\n--- most and least aggressive careers, against the league in their own seasons ---\n")
print(head(career[, .(coach, n, age = round(age), above = sprintf("%+.1f pts", 100*above))], 6))
print(tail(career[, .(coach, n, age = round(age), above = sprintf("%+.1f pts", 100*above))], 6))

# ---------------------------------------------------------------- chart
NAMED <- c("Bill Belichick","Andy Reid","Pete Carroll","Mike Tomlin","John Harbaugh",
           "Sean McVay","Kyle Shanahan","Dan Campbell","Nick Sirianni","Doug Pederson",
           "Sean McDermott","Brandon Staley","Mike McCarthy","Bill O'Brien",
           "Kevin Stefanski","Matt LaFleur","Mike Vrabel","Sean Payton","Ben Johnson",
           # Michael's two: "Bruce Arians was lauded as big balls guy... so is
           # Josh McDaniels." And the pair that kills the age story outright,
           # Kingsbury and Ryans, one year apart at opposite extremes.
           "Bruce Arians","Josh McDaniels","Kliff Kingsbury","DeMeco Ryans","Matt Patricia")
lab <- career[coach %in% NAMED]

pA <- ggplot(career, aes(age, above)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_smooth(aes(weight = n), method = "lm", formula = y ~ x, se = TRUE,
              colour = "#D55E00", fill = "#D55E00", alpha = 0.11, linewidth = 0.8) +
  geom_point(aes(size = n), colour = "#9db6c9", alpha = 0.75) +
  geom_point(data = lab, aes(size = n), colour = "#2B8CBE") +
  geom_text_repel(data = lab, aes(label = coach), size = 2.95, fontface = "bold",
                  colour = "#1d6a99", seed = 7, box.padding = 0.42,
                  min.segment.length = 0, max.overlaps = 22) +
  scale_size_area(max_size = 6.5, guide = "none") +
  scale_y_continuous(labels = function(x) sprintf("%+.0f", 100*x)) +
  labs(title = "Every coach with 40 or more clear go-for-it spots",
       subtitle = "The vertical spread at any given age is far bigger than anything the trend line picks up",
       x = "average age when the decision came up", y = "points above the league that season") +
  theme_coach(grid = "y")

pB <- ggplot(bk, aes(bucket, mu)) +
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  geom_errorbar(aes(ymin = mu - 1.96*se, ymax = mu + 1.96*se), width = 0.14,
                colour = "grey55", linewidth = 0.5) +
  geom_point(aes(colour = mu > 0), size = 3.6) +
  geom_text(aes(label = sprintf("%+.1f", 100*mu), vjust = ifelse(mu >= 0, -1.5, 2.3)),
            size = 3.2, fontface = "bold", colour = "grey25") +
  geom_text(aes(y = min(bk$mu - 1.96*bk$se) - 0.022,
                label = sprintf("%d coaches\n%s calls", coaches, format(plays, big.mark = ","))),
            size = 2.7, colour = "grey45", lineheight = 0.95) +
  scale_colour_manual(values = c("TRUE" = "#2B8CBE", "FALSE" = "#D55E00"), guide = "none") +
  scale_y_continuous(labels = function(x) sprintf("%+.0f", 100*x),
                     expand = expansion(mult = c(0.20, 0.16))) +
  labs(title = "By age bucket, with 95% intervals",
       subtitle = "Only the 60-and-over group clears zero. Every other bar could be nothing.",
       x = "age that season", y = "points above the league that season") +
  theme_coach(grid = "y")

p <- (pA | pB) + plot_layout(widths = c(1.25, 1)) +
  plot_annotation(
    title = "Age tells you almost nothing about whether a coach goes for it",
    subtitle = "Fourth downs where the nfl4th model says go by at least 1.5 win-probability points, 2018 to 2025, always measured against the league in the same season",
    caption = fig_caption(
      "nfl4th decision model on nflverse play-by-play 2018-2025; coach birth years from Wikidata",
      sprintf("%s clear go-for-it spots, %d coaches. Regulation only, at least a minute left in the half, win probability between 5%% and 95%%.",
              format(nrow(go), big.mark = ","), uniqueN(go$coach)),
      paste0(sprintf("\nThe obvious story is that young analytics hires go for it and old-school coaches punt, and the data does not support it. Age and season-adjusted go rate correlate\n"),
             sprintf("at %+.3f. The slope is %+.1f points of go rate per decade (p = %.2f), against a spread of %.0f points between coach-seasons, so age is worth less than a tenth of what\n",
                     r_age, per_dec, p_dec, sd_cs),
             sprintf("separates two coaches. Coaches under 40 are %+.1f points BELOW the league, not above it. Track individual coaches as they age, with each man's own average removed,\n",
                     100*bk[bucket == "under 40"]$mu),
             sprintf("and the effect is %+.1f points per decade (p = %.2f). The one real gap is 60 and over, %+.1f points below the league. The season adjustment does the heavy lifting here:\n",
                     w_dec, p_w, 100*bk[bucket == "60+"]$mu),
             "the league itself went from 37.8% in 2018 to 60.5% in 2025, so without it this chart would mostly be reading the calendar. Built by R/factory/91.")),
    theme = theme_coach(grid = "none"))
save_fig("docs/figures/factory/age_fourthdown.png", p, w = 13.4, h = 6.8)
