# r_scripts/34_or_interaction_explainer.R
# Compact three-column table schematic: Baseline | New Context | Interaction OR
# Fractions displayed as numerator/denominator (frac). Monochrome. Table-like layout.

library(ggplot2)

bk  <- "black"
dim <- "gray28"   # subtitles and interpretation text

# Column centers and divider x-positions
c1 <- 1.80; c2 <- 5.00; c3 <- 8.20
d1 <- 3.40; d2 <- 6.60

# Y positions — expanded to give fraction rows room
y_top   <- 2.87   # top rule
y_hdr   <- 2.69   # column headers
y_hrule <- 2.52   # header / formula divider
y_frm   <- 2.22   # formula row (center of fraction)
y_sub   <- 1.90   # subtitle row
y_bot   <- 1.72   # bottom rule of table
y_ih    <- 1.55   # Interpretation heading
y_i1    <- 1.33   # interpretation line 1
y_i2    <- 1.09   # interpretation line 2
y_i3    <- 0.85   # interpretation line 3

fig_or_explainer <- ggplot() +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(1.55, 3.00), expand = c(0, 0)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(4, 8, 4, 8)
  ) +

  # ── Table top rule ──────────────────────────────────────────
  annotate("segment", x = 0.35, xend = 9.65,
           y = y_top, yend = y_top,
           color = "gray25", linewidth = 0.45) +

  # ── Column headers (bold, uniform) ──────────────────────────
  annotate("text", x = c1, y = y_hdr, hjust = 0.5,
           label = "Baseline Condition",
           fontface = "bold", size = 2.88, color = bk) +
  annotate("text", x = c2, y = y_hdr, hjust = 0.5,
           label = "New Political Context",
           fontface = "bold", size = 2.88, color = bk) +
  annotate("text", x = c3, y = y_hdr, hjust = 0.5,
           label = "Interaction Odds Ratio",
           fontface = "bold", size = 2.88, color = bk) +

  # ── Thin rule below headers ──────────────────────────────────
  annotate("segment", x = 0.35, xend = 9.65,
           y = y_hrule, yend = y_hrule,
           color = "gray55", linewidth = 0.22) +

  # ── Light vertical column dividers ──────────────────────────
  annotate("segment", x = d1, xend = d1, y = y_bot, yend = y_top,
           color = "gray72", linewidth = 0.20) +
  annotate("segment", x = d2, xend = d2, y = y_bot, yend = y_top,
           color = "gray72", linewidth = 0.20) +

  # ── Formulas — numerator over denominator via frac() ─────────
  annotate("text", x = c1, y = y_frm, hjust = 0.5,
           label = '"GOP OR" == frac(Odds[RB], Odds[DB])',
           parse = TRUE, size = 2.78, color = bk) +
  annotate("text", x = c2, y = y_frm, hjust = 0.5,
           label = '"GOP OR" == frac(Odds[RL], Odds[DL])',
           parse = TRUE, size = 2.78, color = bk) +
  annotate("text", x = c3, y = y_frm, hjust = 0.5,
           label = '"Interaction OR" == frac(Odds[RL]~"/"~Odds[DL], Odds[RB]~"/"~Odds[DB])',
           parse = TRUE, size = 2.60, color = bk) +

  # ── Italic subtitles ─────────────────────────────────────────
  annotate("text", x = c1, y = y_sub, hjust = 0.5,
           label = "baseline Republican–Democrat odds ratio",
           fontface = "italic", size = 2.05, color = dim) +
  annotate("text", x = c2, y = y_sub, hjust = 0.5,
           label = "Republican–Democrat odds ratio in new context",
           fontface = "italic", size = 2.05, color = dim) +
  annotate("text", x = c3, y = y_sub, hjust = 0.5,
           label = "how the partisan odds ratio changes from baseline",
           fontface = "italic", size = 2.05, color = dim) +

  # ── Table bottom rule ────────────────────────────────────────
  annotate("segment", x = 0.35, xend = 9.65,
           y = y_bot, yend = y_bot,
           color = "gray25", linewidth = 0.45)

# ── Save ─────────────────────────────────────────────────────
ggsave(
  "/Users/cycoldiron/Desktop/congress-fiscal-tweets/figures/methodology/or_interaction_explainer.png",
  plot   = fig_or_explainer,
  width  = 6.5,
  height = 1.6,
  dpi    = 300,
  bg     = "white"
)
