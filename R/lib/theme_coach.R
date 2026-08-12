# =============================================================================
# theme_coach.R -- shared figure style for the coaching-report-card repo.
# Copied from ghost-town's theme_ghost.R with functions renamed.
# Tufte + CLEAR synthesis: erase non-data ink, one accent at a time,
# takeaway titles, left-aligned captions, direct labels over legends.
#
# Usage (from repo root, in every figure-producing script):
#   source("R/lib/theme_coach.R")
#   p <- ggplot(...) + theme_coach()
#   save_fig("docs/figures/foo.png", p, w = 12, h = 6.75)
#
# Rules this file encodes:
#   - theme_minimal base, minor grid always erased, x grid always erased,
#     y grid faint grey92 or absent when direct labels carry the values
#   - no axis ticks, no panel border, no plot border, white background
#   - text hierarchy: bold title rel(1.15), grey40 subtitle, grey45 caption
#     at rel(0.7) left-aligned to the PLOT edge (title.position = "plot")
#   - facet strips: no background box, bold left-aligned strip text
#   - legends off by default: direct-label instead (see direct_label note)
# =============================================================================

suppressMessages({
  library(ggplot2)
})

# ---------------------------------------------------------------------------
# Palette constants. Never invent hues per script; use these names.
# ---------------------------------------------------------------------------

# Era ramp: one hue (blue), light to dark = old to recent. Sequential because
# eras are ordered. Same vector everywhere, including filtered charts.
pal_era <- c("1990s" = "#A6BDDB",
             "2000s" = "#74A9CF",
             "2010s" = "#2B8CBE",
             "2020s" = "#045A8D")

# Pooled-era variants (NHL bins chart) reuse the ramp endpoints' neighbors.
pal_era_pooled <- c("1990s-2000s" = "#74A9CF",
                    "2010s-2020s" = "#045A8D")

# Sport identity: Okabe-Ito, colorblind-safe, fixed order NFL MLB NHL NBA.
# A sport keeps its hue on every chart on the page, no exceptions.
pal_sport <- c(NFL = "#0072B2",
               MLB = "#D55E00",
               NHL = "#009E73",
               NBA = "#CC79A7")

# Muted diverging pair + neutral mid for RATIO fills (calendar heatmaps).
# Below baseline = muted blue, at baseline = near-page grey, above = muted red.
# RdBu inner hues: colorblind-safe, low chroma so near-null cells vanish.
pal_ratio <- c(low = "#4393C3", mid = "#F7F7F7", high = "#D6604D")

# Ink greys (text and reference lines). Floors chosen for WCAG on white.
ink_title    <- "grey15"
ink_body     <- "grey30"
ink_subtitle <- "grey40"
ink_caption  <- "grey45"   # rel(0.7) caption text; do not go lighter
ink_baseline <- "grey55"   # dashed baseline-at-1 reference lines
ink_grid     <- "grey92"   # y gridlines, when kept at all

# ---------------------------------------------------------------------------
# theme_coach(): the only theme any figure may use.
#   grid = "y"    faint horizontal gridlines (default: bar/line lookup charts)
#   grid = "none" no grid at all (heatmaps, maps, direct-labeled charts)
# ---------------------------------------------------------------------------
theme_coach <- function(base_size = 13, grid = c("y", "none")) {
  grid <- match.arg(grid)
  grid_y <- if (grid == "y") {
    element_line(colour = ink_grid, linewidth = 0.3)
  } else {
    element_blank()
  }
  theme_minimal(base_size = base_size) +
    theme(
      # erase non-data ink
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = grid_y,
      panel.border       = element_blank(),
      axis.ticks         = element_blank(),
      axis.line          = element_blank(),
      plot.background    = element_rect(fill = "white", colour = NA),
      panel.background   = element_blank(),
      # text hierarchy
      plot.title           = element_text(face = "bold", size = rel(1.15),
                                          colour = ink_title),
      plot.title.position   = "plot",
      plot.subtitle         = element_text(colour = ink_subtitle,
                                           size = rel(0.9),
                                           margin = margin(b = 12)),
      plot.caption          = element_text(colour = ink_caption,
                                           size = rel(0.7), hjust = 0,
                                           lineheight = 1.25,
                                           margin = margin(t = 10)),
      plot.caption.position = "plot",
      axis.title            = element_text(colour = ink_body, size = rel(0.85)),
      axis.text             = element_text(colour = ink_body, size = rel(0.8)),
      strip.background      = element_blank(),
      strip.text            = element_text(face = "bold", hjust = 0,
                                           size = rel(0.85), colour = ink_title),
      panel.spacing         = unit(1.1, "lines"),
      # legends: off unless a scale genuinely cannot be direct-labeled
      # (county map fill ramp keeps its legend; see design spec)
      legend.position  = "none",
      legend.title     = element_text(size = rel(0.8), colour = ink_body),
      legend.text      = element_text(size = rel(0.75), colour = ink_body),
      plot.margin      = margin(10, 14, 8, 10)
    )
}

# Variant that keeps a legend (maps, continuous fill ramps only).
theme_coach_legend <- function(base_size = 13, grid = c("y", "none"),
                                  position = "right") {
  theme_coach(base_size = base_size, grid = grid) +
    theme(legend.position = position)
}

# ---------------------------------------------------------------------------
# Baseline reference line at ratio = 1 (or share = 0.25 etc.).
# Drawn FIRST so data sits on top; quiet grey per layering/separation.
# ---------------------------------------------------------------------------
geom_baseline <- function(yintercept = 1) {
  geom_hline(yintercept = yintercept, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.35)
}

# ---------------------------------------------------------------------------
# direct_label: end-of-line labels replacing legends (<= 4 series).
# Note for implementers: pass a data frame of ONE row per series (the last
# x position), label in the series' own hue at 70% darkness or ink_body,
# and remember coord_cartesian(clip = "off") + a widened right plot.margin
# (e.g. + theme(plot.margin = margin(10, 70, 8, 10))). Deterministic nudges
# beat ggrepel; only reach for repel if labels physically collide.
# ---------------------------------------------------------------------------
direct_label <- function(data, mapping, nudge_x = 0.12, size = 3.4,
                         fontface = "bold", ...) {
  geom_text(data = data, mapping = mapping, hjust = 0,
            nudge_x = nudge_x, size = size, fontface = fontface, ...)
}

# ---------------------------------------------------------------------------
# Ratio fill scale for calendar heatmaps: symmetric in log space around 1.0
# so hue faithfully means direction (a 1.5x cell is as saturated as a 0.67x
# cell). Feed it the RAW ratio; it transforms internally.
#   max_ratio: clamp, e.g. 1.6 means the scale runs 1/1.6 .. 1.6.
# ---------------------------------------------------------------------------
scale_fill_ratio <- function(max_ratio = 1.6, name = NULL) {
  m <- log2(max_ratio)
  scale_fill_gradient2(
    low = pal_ratio[["low"]], mid = pal_ratio[["mid"]],
    high = pal_ratio[["high"]], midpoint = 0,
    limits = c(-m, m), oob = scales::squish,
    trans = "identity", name = name,
    breaks = c(-m, 0, m),
    labels = c("fewer than expected", "expected", "more than expected")
  )
  # NOTE: map fill to log2(ratio) in aes(), not to ratio itself.
}

# ---------------------------------------------------------------------------
# Caption builder: one line, source + universe + baseline. No em dashes.
# ---------------------------------------------------------------------------
fig_caption <- function(source, universe, note = NULL) {
  out <- paste0("Data: ", source, ". ", universe)
  if (!is.null(note) && nzchar(note)) out <- paste0(out, " ", note)
  out
}

# ---------------------------------------------------------------------------
# save_fig: the only save path. dpi 320, white background, dir created.
# Default size keeps type at true report scale; override per figure.
# ---------------------------------------------------------------------------
save_fig <- function(path, plot, w = 12, h = 6.75) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(path, plot, width = w, height = h, dpi = 320, bg = "white")
  cat(sprintf("wrote %s\n", path))
  invisible(path)
}
