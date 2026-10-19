#!/usr/bin/env Rscript
# Generate the HistoneMod logo (PNG + SVG).
# Run from the repository root: Rscript tools/make-logo.R

library(ggplot2)

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

ellipse <- function(cx, cy, rx, ry, n = 240) {
  theta <- seq(0, 2 * pi, length.out = n)
  data.frame(x = cx + rx * cos(theta), y = cy + ry * sin(theta))
}

circle <- function(cx, cy, r, n = 240) ellipse(cx, cy, r, r, n)

bg <- "#0B1220"
blue_dark <- "#1E3A8A"
blue <- "#0EA5E9"
blue_light <- "#7DD3FC"
teal <- "#2DD4BF"
orange <- "#F59E0B"
green <- "#22C55E"
pink <- "#EC4899"
violet <- "#A78BFA"
grey <- "#94A3B8"

nucleosome <- ellipse(0.50, 0.58, 0.255, 0.165)
nucleosome_inner <- ellipse(0.50, 0.58, 0.195, 0.115)
nucleosome_mid <- ellipse(0.50, 0.58, 0.225, 0.140)

dna_x <- seq(0.18, 0.82, length.out = 400)
dna_y1 <- 0.58 + 0.075 * sin(seq(0, 7 * pi, length.out = 400))
dna_y2 <- 0.58 - 0.075 * sin(seq(0, 7 * pi, length.out = 400))

tail_x <- seq(0.50, 0.24, length.out = 200)
tail_y <- 0.74 + 0.14 * (seq(0, 1, length.out = 200))^0.8 +
  0.018 * sin(seq(0, 6 * pi, length.out = 200))

ptm <- data.frame(
  x = c(0.455, 0.405, 0.350, 0.295),
  y = c(0.790, 0.830, 0.868, 0.900),
  colour = c(green, orange, pink, violet)
)

p <- ggplot() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE, clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = bg, colour = NA),
    plot.margin = margin(0, 0, 0, 0)
  ) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = bg) +
  annotate("path", x = nucleosome$x, y = nucleosome$y,
           colour = blue_light, linewidth = 1.1, alpha = 0.9) +
  annotate("path", x = nucleosome_mid$x, y = nucleosome_mid$y,
           colour = blue_dark, linewidth = 6.5, alpha = 0.55) +
  annotate("path", x = nucleosome_inner$x, y = nucleosome_inner$y,
           colour = blue, linewidth = 3.2, alpha = 0.85) +
  annotate("path", x = dna_x, y = dna_y1, colour = teal, linewidth = 1.5) +
  annotate("path", x = dna_x, y = dna_y2, colour = teal, linewidth = 1.5, alpha = 0.75) +
  annotate("path", x = tail_x, y = tail_y, colour = grey, linewidth = 1.6) +
  annotate("point", x = ptm$x, y = ptm$y, colour = ptm$colour,
           size = c(4.2, 4.2, 4.2, 4.2)) +
  annotate("text", x = 0.5, y = 0.330, label = "HistoneMod",
           colour = "white", size = 13, fontface = "bold", family = "sans") +
  annotate("text", x = 0.5, y = 0.245, label = "HISTONE PTM ANALYSIS",
           colour = grey, size = 4.1, family = "sans") +
  annotate("text", x = 0.5, y = 0.180, label = "Skyline \u00b7 R \u00b7 Shiny",
           colour = "#64748B", size = 3.6, family = "sans")

ggsave("man/figures/histonemod-logo.png", p, width = 3.4, height = 3.4, dpi = 300, bg = bg)
ggsave("man/figures/histonemod-logo.svg", p, width = 3.4, height = 3.4, bg = bg, device = "svg")
message("Wrote man/figures/histonemod-logo.png and man/figures/histonemod-logo.svg")
