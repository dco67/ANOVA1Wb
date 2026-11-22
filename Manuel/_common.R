# ===========================================================================
# _common.R — réglages et calculs partagés par les chapitres du manuel.
# Chaque chapitre le charge au début : source("_common.R").
# Les données sont simulées EXACTEMENT comme dans l'application : même graine,
# mêmes paramètres, même ordre de tirage (groupe A, puis B, puis C...). Les
# chiffres cités dans le texte sont donc ceux que l'étudiant voit à l'écran.
# ===========================================================================

suppressPackageStartupMessages({
  library(ggplot2)
})

knitr::opts_chunk$set(
  echo = FALSE, message = FALSE, warning = FALSE,
  fig.align = "center", out.width = "88%", fig.width = 6.4, fig.height = 3.6,
  dev = "ragg_png", dpi = if (knitr::is_latex_output()) 300 else 150
)

# --- Nombres à la française -------------------------------------------------
fr <- function(x, d = 2) {
  x <- as.numeric(x); x[!is.na(x) & abs(x) < 0.5 * 10^(-d)] <- 0
  sub("^-", "−", formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = " "))
}
pc <- function(x, d = 1) paste0(fr(100 * x, d), " %")
# Version pour l'intérieur d'une formule LaTeX : 9{,}75 et signe moins ASCII
frm <- function(x, d = 2) gsub(",", "{,}", sub("−", "-", fr(x, d), fixed = TRUE), fixed = TRUE)
pvm <- function(p) if (p < 0.001) "p < 0{,}001" else paste0("p = ", frm(p, 3))
# « p < 0,001 » ou « p = 0,012 » (pour le texte, hors formule)
pv <- function(p) if (p < 0.001) "< 0,001" else paste0("= ", fr(p, 3))
axe_fr <- function(x) sub("^-", "−", format(x, decimal.mark = ",", big.mark = " ",
                                                   trim = TRUE, scientific = FALSE, drop0trailing = TRUE))

# --- Couleurs (celles de l'application) et thème ------------------------------
PALETTE <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")
pal <- list(bleu = "#2980b9", rouge = "#e74c3c", vert = "#1e8449", gris = "grey45", or = "#d9a53b",
            sarcelle = "#137983")
theme_set(theme_minimal(base_size = 12) +
            theme(panel.grid.minor = element_blank(), legend.position = "bottom",
                  strip.text = element_text(face = "bold"),
                  plot.title = element_text(face = "bold", size = 12)))

# --- Simulation identique à l'application -------------------------------------
simuler <- function(graine = 2026, ns = rep(30, 3), mu = c(50, 54, 60), sd = rep(10, 3)) {
  k <- length(ns); niv <- paste("Groupe", LETTERS[seq_len(k)])
  set.seed(graine)
  do.call(rbind, lapply(seq_len(k), function(i)
    data.frame(groupe = factor(niv[i], levels = niv), y = rnorm(ns[i], mu[i], sd[i]))))
}

# --- ANOVA, tailles d'effet ---------------------------------------------------
anova_1f <- function(d) {
  k <- nlevels(d$groupe); N <- nrow(d)
  m <- tapply(d$y, d$groupe, mean); n <- tapply(d$y, d$groupe, length); v <- tapply(d$y, d$groupe, var)
  gm <- mean(d$y)
  sce <- sum(n * (m - gm)^2); scr <- sum((n - 1) * v); sct <- sce + scr
  cme <- sce / (k - 1); cmr <- scr / (N - k); F <- cme / cmr
  list(k = k, N = N, m = m, n = n, s = sqrt(v), v = v, gm = gm, sce = sce, scr = scr, sct = sct,
       ddl1 = k - 1, ddl2 = N - k, cme = cme, cmr = cmr, F = F,
       p = pf(F, k - 1, N - k, lower.tail = FALSE),
       eta2 = sce / sct, omega2 = max(0, (sce - (k - 1) * cmr) / (sct + cmr)))
}

welch <- function(d) {
  w <- oneway.test(y ~ groupe, data = d, var.equal = FALSE)
  list(F = unname(w$statistic), ddl2 = unname(w$parameter[2]), p = w$p.value)
}

levene_med <- function(d) car::leveneTest(y ~ groupe, data = d, center = median)[1, ]

# Contraste : ψ̂, erreur type, t, p, IC, SC
contraste <- function(d, cf, alpha = 0.05) {
  a <- anova_1f(d)
  psi <- sum(cf * a$m); se <- sqrt(a$cmr * sum(cf^2 / a$n)); t <- psi / se
  q <- qt(1 - alpha / 2, a$ddl2)
  list(psi = psi, se = se, t = t, ddl = a$ddl2, p = 2 * pt(-abs(t), a$ddl2),
       lo = psi - q * se, hi = psi + q * se, sc = psi^2 / sum(cf^2 / a$n))
}

# Tukey (même convention que l'application : 1er groupe − 2e groupe)
tukey <- function(d) {
  th <- TukeyHSD(aov(y ~ groupe, data = d))$groupe
  # TukeyHSD donne « B-A » = B − A ; on retourne A − B
  data.frame(paire = sub("^(.*)-(.*)$", "\\2 − \\1", rownames(th)),
             diff = -th[, "diff"], lo = -th[, "upr"], hi = -th[, "lwr"], p = th[, "p adj"])
}

games_howell <- function(d) {
  m <- tapply(d$y, d$groupe, mean); v <- tapply(d$y, d$groupe, var); n <- tapply(d$y, d$groupe, length)
  k <- length(m); pr <- t(combn(k, 2))
  do.call(rbind, lapply(seq_len(nrow(pr)), function(r) {
    i <- pr[r, 1]; j <- pr[r, 2]; vn <- v / n
    se <- sqrt(vn[i] + vn[j]); df <- (vn[i] + vn[j])^2 / (vn[i]^2 / (n[i] - 1) + vn[j]^2 / (n[j] - 1))
    dif <- m[i] - m[j]
    data.frame(paire = paste(names(m)[i], "−", names(m)[j]), diff = unname(dif), se = unname(se),
               ddl = unname(df), p = unname(ptukey(abs(dif) / se * sqrt(2), k, df, lower.tail = FALSE)))
  }))
}

puissance <- function(n, f, k, a = 0.05)
  pf(qf(1 - a, k - 1, k * (n - 1)), k - 1, k * (n - 1), ncp = f^2 * k * n, lower.tail = FALSE)
n_requis <- function(f, k, a = 0.05, cible = 0.8) { ns <- 2:2000; ns[which(puissance(ns, f, k, a) >= cible)[1]] }

# --- Les scénarios du manuel (graine 2026 partout) ----------------------------
fil   <- simuler()                                                      # cas fil rouge
fil_a <- anova_1f(fil)
nul   <- simuler(mu = c(50, 50, 50))                                    # pas d'effet
nul_a <- anova_1f(nul)
lab1  <- simuler(mu = c(50, 50, 60))                                    # laboratoire 1
lab1_a <- anova_1f(lab1)
bruit <- simuler(sd = rep(20, 3))                                       # même écart, plus de bruit
bruit_a <- anova_1f(bruit)
het   <- simuler(ns = c(20, 30, 45), sd = c(18, 10, 6))                 # variances inégales
het_a <- anova_1f(het)
ord   <- simuler(ns = rep(30, 4), mu = c(45, 50, 55, 60), sd = rep(10, 4))  # niveaux ordonnés
ord_a <- anova_1f(ord)

# Paramètres de population du cas fil rouge
mu_fil <- c(50, 54, 60); sd_fil <- 10
v_inter_th <- mean((mu_fil - mean(mu_fil))^2)                           # poids égaux
f_fil_th <- sqrt(v_inter_th) / sd_fil
eta2_fil_th <- v_inter_th / (v_inter_th + sd_fil^2)
