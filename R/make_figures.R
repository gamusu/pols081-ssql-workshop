# ---------------------------------------------------------------------------
# make_figures.R
# Builds every pre-rendered figure used in slides/part1-lecture.qmd.
# All data here are SIMULATED for teaching purposes (seeded, so they rebuild
# identically). Nothing in these figures describes real countries.
#
# Run from the project root:  source("R/make_figures.R")
# Requires: ggplot2
# ---------------------------------------------------------------------------

library(ggplot2)

out_dir <- "slides/figs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- Palette & theme --------------------------------------------------------
pal <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a")
ink <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"; surface <- "#fcfcfb"

theme_ssql <- function(base_size = 18) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = surface, colour = NA),
      panel.background = element_rect(fill = surface, colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = grid_col, linewidth = 0.4),
      axis.text        = element_text(colour = ink2),
      axis.title       = element_text(colour = ink2),
      plot.title       = element_text(colour = ink, face = "bold"),
      plot.subtitle    = element_text(colour = ink2),
      plot.caption     = element_text(colour = ink2, size = rel(0.6), hjust = 0),
      strip.text       = element_text(colour = ink, face = "bold"),
      legend.position  = "top",
      legend.text      = element_text(colour = ink2),
      legend.title     = element_blank()
    )
}
sim_note <- "Simulated data for illustration."

save_fig <- function(p, name, w = 10, h = 5.6) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 200, bg = surface)
}

# Draw (x, y) with a target correlation r
sim_r <- function(n, r) {
  x <- rnorm(n); e <- rnorm(n)
  y <- r * x + sqrt(1 - r^2) * e
  data.frame(x = as.numeric(scale(x)), y = as.numeric(scale(y)))
}

# --- 1. Guess the correlation ----------------------------------------------
set.seed(81)
targets <- c(A = 0.85, B = 0.35, C = 0.0, D = -0.6)
guess <- do.call(rbind, lapply(names(targets), function(k) {
  d <- sim_r(80, targets[[k]]); d$panel <- k; d
}))
guess_r <- sapply(split(guess, guess$panel), function(d) cor(d$x, d$y))

p_guess <- ggplot(guess, aes(x, y)) +
  geom_point(colour = pal["blue"], size = 2.6, alpha = 0.85) +
  facet_wrap(~panel, nrow = 1) +
  labs(x = NULL, y = NULL, caption = sim_note) +
  theme_ssql() + theme(axis.text = element_blank())
save_fig(p_guess, "guess_corr.png", h = 3.6)

guess$label <- paste0(guess$panel, ":  r = ", sprintf("%.2f", guess_r[guess$panel]))
p_guess_rev <- p_guess %+% guess + facet_wrap(~label, nrow = 1) +
  geom_smooth(method = "lm", se = FALSE, colour = pal["orange"], linewidth = 1.2, formula = y ~ x)
save_fig(p_guess_rev, "guess_corr_reveal.png", h = 3.6)

# --- 2. No variation, no correlation ----------------------------------------
set.seed(2)
n <- 60
novar <- rbind(
  data.frame(x = runif(n, 0, 100), panel = "The explanatory variable varies"),
  data.frame(x = rep(50, n) + rnorm(n, 0, 0.6), panel = "Every case has the same value")
)
novar$y <- 20 + 0.5 * novar$x + rnorm(nrow(novar), 0, 8)
p_novar <- ggplot(novar, aes(x, y)) +
  geom_point(colour = pal["blue"], size = 2.6, alpha = 0.85) +
  facet_wrap(~panel) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(x = "Explanatory variable (X)", y = "Outcome (Y)", caption = sim_note) +
  theme_ssql()
save_fig(p_novar, "no_variation.png", h = 4.6)

# --- 3. Restricted range ----------------------------------------------------
set.seed(7)
n <- 160
rr <- data.frame(vuln = runif(n, 0.25, 0.70))
rr$fin <- 3.5 + 18 * (rr$vuln - 0.25) + rnorm(n, 0, 2.2)
rr$kept <- rr$vuln > 0.60
r_all  <- cor(rr$vuln, rr$fin)
r_kept <- cor(rr$vuln[rr$kept], rr$fin[rr$kept])
rr$group <- ifelse(rr$kept, "Kept in a narrow sample", "Dropped from the sample")

p_rr_full <- ggplot(rr, aes(vuln, fin)) +
  geom_point(colour = pal["blue"], size = 2.6, alpha = 0.85) +
  annotate("text", x = 0.25, y = Inf, hjust = 0, vjust = 1.3, size = 6, colour = ink,
           label = sprintf("All countries: r = %.2f", r_all)) +
  labs(x = "Vulnerability index", y = "Finance received per capita",
       caption = sim_note) +
  theme_ssql()
save_fig(p_rr_full, "restricted_full.png", h = 4.8)

p_rr <- ggplot(rr, aes(vuln, fin, colour = group)) +
  geom_point(size = 2.6, alpha = 0.85) +
  scale_colour_manual(values = c("Kept in a narrow sample" = unname(pal["blue"]),
                                 "Dropped from the sample" = "#c9c8c3")) +
  annotate("text", x = 0.25, y = Inf, hjust = 0, vjust = 1.2, size = 6, colour = ink,
           label = sprintf("All countries: r = %.2f\nOnly the most vulnerable: r = %.2f", r_all, r_kept)) +
  labs(x = "Vulnerability index", y = "Finance received per capita", caption = sim_note) +
  theme_ssql()
save_fig(p_rr, "restricted_range.png", h = 4.8)

# --- 4. Confounding ---------------------------------------------------------
set.seed(11)
n <- 150
cap <- sample(c("Low", "Medium", "High"), n, replace = TRUE)
cap <- factor(cap, levels = c("Low", "Medium", "High"))
base <- c(Low = 1, Medium = 2, High = 3)[as.character(cap)]
conf <- data.frame(
  capacity = cap,
  delegation = 15 * base + rnorm(n, 0, 6),
  finance    = 10 * base + rnorm(n, 0, 4)
)
r_conf <- cor(conf$delegation, conf$finance)
r_within <- sapply(split(conf, conf$capacity), function(d) cor(d$delegation, d$finance))

p_conf1 <- ggplot(conf, aes(delegation, finance)) +
  geom_point(colour = pal["blue"], size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, colour = ink, linewidth = 1, formula = y ~ x) +
  annotate("text", x = min(conf$delegation), y = max(conf$finance), hjust = 0, size = 6,
           colour = ink, label = sprintf("r = %.2f", r_conf)) +
  labs(x = "Delegation size at the COP", y = "Climate finance received", caption = sim_note) +
  theme_ssql()
save_fig(p_conf1, "confound_pooled.png", h = 4.8)

p_conf2 <- ggplot(conf, aes(delegation, finance, colour = capacity)) +
  geom_point(size = 2.6, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, formula = y ~ x) +
  scale_colour_manual(values = unname(pal), labels = function(l) paste(l, "state capacity")) +
  annotate("text", x = min(conf$delegation), y = max(conf$finance), hjust = 0, size = 5.5,
           colour = ink, label = sprintf("Within each group: r = %.2f, %.2f, %.2f",
                                         r_within[1], r_within[2], r_within[3])) +
  labs(x = "Delegation size at the COP", y = "Climate finance received", caption = sim_note) +
  theme_ssql()
save_fig(p_conf2, "confound_split.png", h = 4.8)

# --- 5. Sampling distribution -----------------------------------------------
set.seed(2026)
N <- 150
pop <- data.frame(id = 1:N, value = rlnorm(N, meanlog = 2.6, sdlog = 0.6))
mu <- mean(pop$value)

p_pop <- ggplot(pop, aes(value)) +
  geom_histogram(bins = 30, fill = pal["blue"], colour = surface, linewidth = 0.6) +
  geom_vline(xintercept = mu, colour = pal["orange"], linewidth = 1.3) +
  annotate("text", x = mu, y = Inf, vjust = 1.5, hjust = -0.05, size = 6, colour = ink,
           label = sprintf("True population mean = %.1f", mu)) +
  labs(x = "Value for each of 150 countries", y = "Number of countries", caption = sim_note) +
  theme_ssql()
save_fig(p_pop, "sampling_1_population.png", h = 4.6)

n_s <- 30
draws <- lapply(1:3, function(i) pop[sample(N, n_s), ])
samp <- do.call(rbind, Map(function(d, i) { d$draw <- paste("Sample", i); d }, draws, 1:3))
samp_means <- sapply(draws, function(d) mean(d$value))
mlab <- data.frame(draw = paste("Sample", 1:3), m = samp_means)
p_samp <- ggplot(samp, aes(value)) +
  geom_histogram(bins = 15, fill = pal["blue"], colour = surface, linewidth = 0.6) +
  geom_vline(xintercept = mu, colour = "#c9c8c3", linewidth = 1, linetype = "dashed") +
  geom_vline(data = mlab, aes(xintercept = m), colour = pal["orange"], linewidth = 1.3) +
  geom_text(data = mlab, aes(x = m, y = Inf, label = sprintf("mean = %.1f", m)),
            vjust = 1.5, hjust = -0.08, size = 5, colour = ink) +
  facet_wrap(~draw, nrow = 1) +
  labs(x = "Value (30 randomly drawn countries)", y = "Count",
       caption = paste(sim_note, "Dashed line = true population mean.")) +
  theme_ssql()
save_fig(p_samp, "sampling_2_three_samples.png", h = 4.4)

many <- replicate(2000, mean(pop$value[sample(N, n_s)]))
se <- sd(many)
p_dist <- ggplot(data.frame(m = many), aes(m)) +
  geom_histogram(bins = 40, fill = pal["blue"], colour = surface, linewidth = 0.4) +
  geom_vline(xintercept = mu, colour = pal["orange"], linewidth = 1.3) +
  annotate("text", x = Inf, y = Inf, vjust = 1.5, hjust = 1.05, size = 6, colour = ink,
           label = sprintf("Spread of the estimates\n(standard error) = %.1f", se)) +
  labs(x = "Mean from each of 2,000 random samples of 30 countries", y = "Number of samples",
       caption = sim_note) +
  theme_ssql()
save_fig(p_dist, "sampling_3_distribution.png", h = 4.6)

# --- 6. Confidence intervals ------------------------------------------------
set.seed(18)
ci <- do.call(rbind, lapply(1:20, function(i) {
  v <- pop$value[sample(N, n_s)]
  m <- mean(v); s <- sd(v) / sqrt(n_s); t <- qt(0.975, n_s - 1)
  data.frame(i = i, m = m, lo = m - t * s, hi = m + t * s)
}))
ci$covers <- ifelse(ci$lo <= mu & ci$hi >= mu, "Interval contains the true mean", "Interval misses it")
p_ci <- ggplot(ci, aes(y = factor(i), x = m, xmin = lo, xmax = hi, colour = covers)) +
  geom_vline(xintercept = mu, colour = ink2, linewidth = 0.8, linetype = "dashed") +
  geom_linerange(linewidth = 1.4) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c("Interval contains the true mean" = unname(pal["blue"]),
                                 "Interval misses it" = unname(pal["orange"]))) +
  labs(x = "Estimated mean with 95% confidence interval", y = "Sample",
       caption = paste(sim_note, "Dashed line = true population mean.")) +
  theme_ssql() + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
save_fig(p_ci, "confidence_intervals.png", h = 5.2)

message("Figures written to ", out_dir)

# ===========================================================================
# Diagrams (causal diagrams and flow charts), drawn with ggplot2 so they
# render identically everywhere.
# ===========================================================================

# nodes: data.frame(id, x, y, label, w, h, fill)
# edges: data.frame(from, to, type = "solid"/"dashed", label, curv)
edge_point <- function(x0, y0, x1, y1, w, h) {
  # point where the segment from the box centre (x0,y0) toward (x1,y1) leaves a w x h box
  dx <- x1 - x0; dy <- y1 - y0
  s <- min(ifelse(dx == 0, Inf, (w / 2) / abs(dx)), ifelse(dy == 0, Inf, (h / 2) / abs(dy)))
  c(x0 + dx * s, y0 + dy * s)
}

draw_diagram <- function(nodes, edges, xlim, ylim, file, w = 6, h = 4, text_size = 6) {
  seg <- do.call(rbind, lapply(seq_len(nrow(edges)), function(i) {
    a <- nodes[nodes$id == edges$from[i], ]; b <- nodes[nodes$id == edges$to[i], ]
    p0 <- edge_point(a$x, a$y, b$x, b$y, a$w + 0.08, a$h + 0.08)
    p1 <- edge_point(b$x, b$y, a$x, a$y, b$w + 0.08, b$h + 0.08)
    data.frame(x = p0[1], y = p0[2], xend = p1[1], yend = p1[2],
               type = edges$type[i], label = edges$label[i], curv = edges$curv[i])
  }))
  p <- ggplot() +
    geom_rect(data = nodes, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y - h / 2, ymax = y + h / 2,
                                fill = fill), colour = ink2, linewidth = 0.5) +
    scale_fill_identity() +
    geom_text(data = nodes, aes(x, y, label = label), size = text_size, colour = ink, lineheight = 0.9)
  for (i in seq_len(nrow(seg))) {
    s <- seg[i, ]
    lt <- if (s$type == "dashed") "22" else "solid"
    if (s$curv == 0) {
      p <- p + geom_segment(data = s, aes(x = x, y = y, xend = xend, yend = yend), linetype = lt,
                            colour = ink2, linewidth = 0.9,
                            arrow = arrow(length = unit(0.14, "inches"), type = "closed"))
    } else {
      p <- p + geom_curve(data = s, aes(x = x, y = y, xend = xend, yend = yend), linetype = lt,
                          colour = ink2, linewidth = 0.9, curvature = s$curv,
                          arrow = arrow(length = unit(0.14, "inches"), type = "closed"))
    }
    if (!is.na(s$label) && nzchar(s$label)) {
      p <- p + annotate("label", x = (s$x + s$xend) / 2, y = (s$y + s$yend) / 2, label = s$label,
                        size = text_size * 0.85, colour = ink, fill = surface, label.size = 0)
    }
  }
  p <- p + coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) + theme_void() +
    theme(plot.background = element_rect(fill = surface, colour = NA))
  ggsave(file.path(out_dir, file), p, width = w, height = h, dpi = 200, bg = surface)
}

grey_fill <- "#f1f0ec"; blue_fill <- "#eaf2fc"; orange_fill <- "#fdf0ea"
E <- function(from, to, type = "solid", label = "", curv = 0)
  data.frame(from = from, to = to, type = type, label = label, curv = curv)

# Confounding
draw_diagram(
  data.frame(id = c("Z", "X", "Y"), x = c(2, 0.75, 3.25), y = c(2.2, 0.6, 0.6),
             label = c("State capacity", "Delegation\nsize", "Finance\nreceived"),
             w = c(1.9, 1.3, 1.3), h = c(0.6, 0.75, 0.75), fill = c(orange_fill, grey_fill, grey_fill)),
  rbind(E("Z", "X"), E("Z", "Y"), E("X", "Y", "dashed", "?")),
  xlim = c(0, 4), ylim = c(0, 2.7), file = "dag_confounding.png", w = 4.4, h = 3)

# Reverse causality
draw_diagram(
  data.frame(id = c("X", "Y"), x = c(2, 2), y = c(2.15, 0.55),
             label = c("Strong climate\npolicy", "Climate finance\nreceived"),
             w = c(1.6, 1.6), h = c(0.75, 0.75), fill = c(grey_fill, grey_fill)),
  rbind(E("X", "Y", "dashed", "?", 0.5), E("Y", "X", "solid", "", 0.5)),
  xlim = c(0, 4), ylim = c(0, 2.7), file = "dag_reverse.png", w = 4.4, h = 3)

# Selection
draw_diagram(
  data.frame(id = c("A", "S", "D"), x = c(0.6, 2.2, 3.8), y = c(1.2, 1.2, 1.2),
             label = c("All\ncases", "Who ends\nup in data", "What we\nobserve"),
             w = c(1.0, 1.15, 1.0), h = c(0.8, 0.8, 0.8), fill = c(grey_fill, orange_fill, grey_fill)),
  rbind(E("A", "S"), E("S", "D")),
  xlim = c(0, 4.4), ylim = c(0, 2.7), file = "dag_selection.png", w = 4.4, h = 3)

# Tennant two-stage logic
draw_diagram(
  data.frame(id = c("A", "S1", "S2", "Z"), x = c(0.9, 2.9, 5, 5), y = c(1.2, 1.2, 1.9, 0.5),
             label = c("All developing\ncountries", "Stage 1:\nany climate\nfinance?", "Stage 2:\nhow much?", "Zero"),
             w = c(1.6, 1.5, 1.4, 1.4), h = c(0.8, 1.0, 0.7, 0.55),
             fill = c(grey_fill, blue_fill, blue_fill, grey_fill)),
  rbind(E("A", "S1"), E("S1", "S2", label = "yes"), E("S1", "Z", label = "no")),
  xlim = c(0, 5.9), ylim = c(0, 2.4), file = "tennant_stages.png", w = 6.4, h = 2.6)

# Tennant starting causal diagram
draw_diagram(
  data.frame(id = c("V", "I", "G", "D", "P", "F"),
             x = c(3.3, 5.2, 3.3, 3.3, 3.6, 6.0), y = c(4.0, 5.2, 3.0, 2.0, 0.85, 2.5),
             label = c("Vulnerability", "Income", "Governance /\ncapacity to apply",
                       "Donor interests\n(trade, history, votes)", "Population size", "Climate finance\nreceived"),
             w = c(1.8, 1.2, 2.0, 2.3, 1.8, 1.9), h = c(0.55, 0.55, 0.75, 0.75, 0.55, 0.85),
             fill = c(blue_fill, grey_fill, grey_fill, grey_fill, grey_fill, orange_fill)),
  rbind(E("V", "F"), E("I", "V"), E("I", "F"), E("G", "F"), E("D", "F"), E("P", "F")),
  xlim = c(1.6, 7.1), ylim = c(0.5, 5.65), file = "tennant_dag.png", w = 9, h = 5.2, text_size = 6.5)

message("Diagrams written to ", out_dir)
