library(ggplot2)

# ── progress & styling ─────────────────────────────────────────────────────────
p <- 0.62  # 0..1
fill_col    <- "#CDB58E"  # darker beige "bone"
outline_col <- "#3D3526"  # dark brown outline

n_vertebrae <- 24
gap <- 0.05          # tight spacing
v_height_base <- 1.6 # was 1.15 — much taller for slab-like look

# ── backbone path: stronger S-curve ───────────────────────────────────────────
t  <- seq(0, 1, length.out = n_vertebrae)
x0 <- cumsum(rep(1 + gap, n_vertebrae)) - (1 + gap)
y0 <- 0.55*sin(2*pi*(t - 0.05)) + 0.18*sin(4*pi*t)

dx <- c(diff(x0), tail(diff(x0), 1))
dy <- c(diff(y0), tail(diff(y0), 1))
angle <- atan2(dy, dx) * 180/pi

# width & height scale by vertebra position
size_scale <- 0.7 + 0.6*dnorm(seq(-2, 2, length.out = n_vertebrae),
                              mean = 0.7, sd = 0.9)
w <- 0.9 * size_scale                     # keep width modest
h <- v_height_base * size_scale           # boost height strongly

spine <- data.frame(i = seq_len(n_vertebrae), x = x0, y = y0,
                    angle = angle, w = w, h = h)

# ── fill computation ──────────────────────────────────────────────────────────
k  <- floor(p * n_vertebrae)
rem <- p * n_vertebrae - k

fill_full <- subset(spine, i <= k)

fill_part <- NULL
if (rem > 0 && k + 1 <= n_vertebrae) {
  v <- spine[k + 1, ]
  fill_part <- within(v, {
    w <- w * rem
    rad <- angle * pi/180
    x <- x - 0.5 * (spine$w[k + 1] - w) * cos(rad)
    y <- y - 0.5 * (spine$w[k + 1] - w) * sin(rad)
  })
}

# ── plot ──────────────────────────────────────────────────────────────────────
ggplot() +
  geom_tile(
    data = fill_full,
    aes(x = x, y = y, width = w, height = h, angle = angle),
    fill = fill_col, color = NA
  ) +
  { if (!is.null(fill_part))
    geom_tile(
      data = fill_part,
      aes(x = x, y = y, width = w, height = h, angle = angle),
      fill = fill_col, color = NA
    )
  } +
  geom_tile(
    data = spine,
    aes(x = x, y = y, width = w, height = h, angle = angle),
    fill = NA, color = outline_col, linewidth = 0.4
  ) +
  annotate("text",
           x = min(spine$x) - 1.2, y = spine$y[1],
           label = scales::percent(p),
           size = 5, hjust = 1, color = outline_col) +
  coord_fixed(expand = TRUE) +
  theme_void() +
  theme(plot.margin = margin(10, 10, 10, 10))
