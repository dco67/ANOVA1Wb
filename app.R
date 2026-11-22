# =============================================================================
# APPLICATION SHINY : ILLUSTRATION PÉDAGOGIQUE DE L'ANOVA
# Auteur  : généré pour Daniel / CIQSS
# Version : 1.0
# Packages requis : shiny, ggplot2, dplyr, tidyr, car, broom, emmeans,
#                   effectsize, patchwork, DT, shinythemes, agricolae,
#                   rstatix, PMCMRplus, multcomp, dunn.test
# =============================================================================
# Installation (si nécessaire) :
# install.packages(c("shiny","ggplot2","dplyr","tidyr","car","broom",
#                    "emmeans","effectsize","patchwork","DT","shinythemes",
#                    "agricolae","rstatix","PMCMRplus","multcomp","dunn.test"))
# =============================================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(broom)
library(emmeans)
library(effectsize)
library(patchwork)
library(DT)
library(shinythemes)
library(agricolae)   # Duncan, SNK
library(rstatix)     # Games-Howell
library(PMCMRplus)   # Tamhane T2, Dunnett T3

# ── Palette daltonien-friendly ────────────────────────────────────────────────
PALETTE <- c("#E69F00","#56B4E9","#009E73","#F0E442","#CC79A7")

# ── Lisibilité : thème unique pour tous les graphiques ────────────────────────
# Les graphiques sont des objets indépendants de l'interface Shiny : la taille
# du texte du navigateur ne les agrandit pas. Ce thème évite que certains
# graphiques deviennent presque illisibles à cause de base_size = 11 ou 13.
theme_anova <- function() {
  theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 19, face = "bold", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 15, lineheight = 1.05, margin = margin(b = 8)),
      axis.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 14),
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 15, face = "bold")
    )
}

theme_anova_void <- function() {
  theme_void(base_size = 16) +
    theme(
      plot.title = element_text(size = 19, face = "bold", margin = margin(b = 6)),
      plot.subtitle = element_text(size = 15, lineheight = 1.05, margin = margin(b = 8)),
      legend.title = element_text(size = 15, face = "bold"),
      legend.text = element_text(size = 14)
    )
}

# ── Nombres à la française ────────────────────────────────────────────────────
# (Pas d'options(OutDec = ",") : Shiny écrirait alors les valeurs des curseurs
#  avec une virgule, que le navigateur ne sait pas lire. Les axes des
#  graphiques utilisent axe_fr() explicitement.)

fr <- function(x, d = 2) {
  x <- as.numeric(x)
  x[!is.na(x) & abs(x) < 0.5 * 10^(-d)] <- 0   # pas de « −0,000 »
  out <- formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = "\u00a0")
  out <- sub("^-", "\u2212", out)
  out[is.na(x)] <- "\u2014"
  out
}
fr_p <- function(p) ifelse(is.na(p), "\u2014", ifelse(p < 0.001, "< 0,001", fr(p, 3)))
fr_alpha <- function(a) fr(a, if (a < 0.01) 3 else 2)
p_eq <- function(p) ifelse(p < 0.001, "p < 0,001", paste0("p = ", fr(p, 3)))
axe_fr <- function(x) sub("^-", "\u2212", format(x, decimal.mark = ",", big.mark = "\u00a0",
                                                   trim = TRUE, scientific = FALSE, drop0trailing = TRUE))

# Tableau DT dont les colonnes numériques sont écrites à la française
dt_fr <- function(df, digits = 3, p_cols = character(0), ...) {
  for (nm in names(df)) {
    if (nm %in% p_cols) df[[nm]] <- fr_p(df[[nm]])
    else if (is.numeric(df[[nm]]) && !all(df[[nm]] == round(df[[nm]]), na.rm = TRUE))
      df[[nm]] <- fr(df[[nm]], if (nm == "ddl") 1 else digits)   # ddl de Welch : une décimale
  }
  datatable(df, options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                               columnDefs = list(list(className = "dt-right", targets = "_all"))),
            rownames = FALSE, ...)
}

# Petit tableau HTML (résultats de tests)
tab_html <- function(df) {
  tags$table(class = "table table-condensed table-striped", style = "width:auto;",
             tags$thead(tags$tr(lapply(names(df), tags$th))),
             tags$tbody(lapply(seq_len(nrow(df)), function(i)
               tags$tr(lapply(df[i, ], function(v) tags$td(HTML(as.character(v))))))))
}

# Orthogonalité de deux contrastes : Σ cᵢdᵢ/nᵢ = 0 (tient compte des effectifs)
produit_contrastes <- function(c1, c2, n) sum(c1 * c2 / n)

# Description en mots d'un contraste (groupes à coefficients positifs vs négatifs)
decrire_contraste <- function(cf, niv) {
  pos <- niv[cf > 1e-10]; neg <- niv[cf < -1e-10]
  et <- function(g) if (length(g) == 1) g else paste0(paste(g[-length(g)], collapse = ", "), " et ", g[length(g)])
  bloc <- function(g) if (length(g) == 1) g else paste0("moyenne de ", et(g))
  if (!length(pos) || !length(neg)) return("contraste incomplet")
  paste0(bloc(pos), " vs ", bloc(neg))
}

# Valeurs par défaut : le cas fil rouge du manuel (scores sur 100)
MOY_DEFAUT <- c(50, 54, 60, 56, 52)
SD_DEFAUT  <- 10

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # ── styles identiques à la version originale ──────────────────────────────
  tags$head(tags$style(HTML("
    body { font-size: 16px; }
    .nav-tabs > li > a { font-weight: 600; }
    .well  { background-color: #f8f9fa; border: 1px solid #dee2e6; }
    .info-box { background:#eaf4fb; border-left:4px solid #2980b9;
                padding:10px 14px; border-radius:4px; margin-bottom:12px; }
    .warn-box { background:#fef9e7; border-left:4px solid #f39c12;
                padding:10px 14px; border-radius:4px; margin-bottom:12px; }
    .ok-box   { background:#eafaf1; border-left:4px solid #27ae60;
                padding:10px 14px; border-radius:4px; margin-bottom:12px; }
    .formule  { background:#f4f4f4; font-family:monospace; font-size:16px;
                padding:8px 12px; border-radius:4px; margin:6px 0; }
    .info-box, .warn-box, .ok-box { font-size:15px; line-height:1.45; }
    /* Les nombreuses anciennes notes de 11–12 px doivent rester lisibles. */
    .info-box[style], .warn-box[style], .ok-box[style] { font-size:15px !important; }
    .global-sidebar label, .global-sidebar .control-label { font-size:15px; }
    .global-sidebar .help-block { font-size:14px; }
    h4 { color: #2c3e50; margin-top:18px; }
    /* Panneau latéral global : léger liseré pour le distinguer */
    .global-sidebar {
      border-right: 2px solid #dee2e6;
      padding-right: 14px;
      min-height: 90vh;
    }
    /* Étiquette de groupe colorée dans le panneau */
    .grp-label {
      color: white; padding: 3px 8px; border-radius: 4px;
      font-weight: bold; font-size: 12px; display:inline-block;
      margin-bottom: 2px;
    }
    /* Rendre les sliders du panneau un peu plus compacts */
    .global-sidebar .irs { margin-bottom: 2px; }
    .global-sidebar .form-group { margin-bottom: 6px; }
  "))),
  withMathJax(),
  
  titlePanel(
    div(style = "display:flex; align-items:center; gap:14px;",
        tags$img(src    = "https://www.r-project.org/logo/Rlogo.png",
                 height = "40px", style = "opacity:.7"),
        div(
          h2("Analyse de la variance (ANOVA)", style = "margin:0; color:#2c3e50;"),
          p("Plan complètement aléatoire — illustration conceptuelle et pratique",
            style = "margin:0; color:#7f8c8d; font-size:14px;")
        )
    )
  ),
  
  # ── Mise en page : sidebar global à gauche, onglets à droite ──────────────
  sidebarLayout(
    
    # ── PANNEAU LATÉRAL GLOBAL (persistant sur tous les onglets) ─────────────
    sidebarPanel(
      width = 3,
      div(class = "global-sidebar",
          
          h4("🔧 Paramètres généraux"),
          
          # Ligne 1 : k | n | graine
          fluidRow(
            column(6,
                   numericInput("nb_groupes",   "Groupes (k)", 3,  2,    5, 1)
            ),
            column(6,
                   numericInput("n_par_groupe", "n par groupe",  30, 5,  200, 5)
            )
          ),
          numericInput("graine", "Graine (reproduit l'échantillon)", 2026, 1, 9999, 1),
          
          # Ligne 2 : déséquilibre aléatoire ou tailles personnalisées
          sliderInput("desequilibre",
                      label = "Déséquilibre aléatoire ± n",
                      min = 0L, max = 20L, value = 0L, step = 1L,
                      width = "100%", ticks = FALSE),
          conditionalPanel("input.desequilibre > 0",
                           div(class = "info-box", style = "font-size:11px; padding:6px 10px;",
                               "Chaque groupe reçoit n plus un entier tiré au hasard entre −d et +d.
     Tailles effectives visibles dans le tableau descriptif.")
          ),
          textInput("n_perso", "Ou tailles précises (ex. 20; 30; 45)", value = "",
                    placeholder = "laisser vide : n par groupe"),
          uiOutput("ui_n_perso"),
          
          actionButton("regenerer", "🎲 Nouvel échantillon",
                       class = "btn-primary btn-sm", width = "100%"),
          div(style = "font-size:14px; color:#666; margin-top:4px;",
              "Le bouton tire une nouvelle graine. Pour retrouver un échantillon,
               inscrivez sa graine dans la case Graine."),

          
          hr(),
          
          # ── [NOUVEAU] Transformation ─────────────────────────────────────────
          h4("🔄 Transformation de Y"),
          selectInput("transformation", NULL,
                      choices = c(
                        "Aucune (Y brut)"             = "none",
                        "Racine carrée  √Y"           = "sqrt",
                        "Logarithme naturel  ln(Y)"   = "log",
                        "Logarithme base 10  log₁₀Y"  = "log10",
                        "Carré  Y²"                   = "square",
                        "Inverse racine  1/√Y"        = "invsqrt",
                        "Arcsin racine  arcsin(√Y)"   = "arcsin"
                      ),
                      selected = "none"
          ),
          uiOutput("ui_transfo_warning"),   # avertissement contextuel
          uiOutput("ui_ylabel"),            # label Y affiché partout
          
          hr(),
          
          # ── Type de VI ───────────────────────────────────────────────────────
          h4("📌 Type de VI"),
          radioButtons("type_vi", NULL,
                       c("Nominale"    = "nominal",
                         "Quantitative / ordinale" = "quanti"),
                       selected = "nominal",
                       inline = TRUE),
          
          hr(),
          
          # ── [NOUVEAU] Sliders μ / σ par groupe (toujours visibles) ──────────
          h4("🎚️ Moyennes et écarts-types"),
          div(class = "info-box", style = "font-size:12px; padding:6px 10px;",
              "Ces paramètres contrôlent la simulation et sont reflétés
             immédiatement dans tous les onglets."),
          uiOutput("ui_groupes_sidebar")   # généré dynamiquement
      )
    ),
    
    # ── CONTENU PRINCIPAL (onglets) ───────────────────────────────────────────
    mainPanel(
      width = 9,
      tabsetPanel(id = "onglets", type = "tabs",
                  
                  # ── INTRODUCTION ────────────────────────────────────────────────────
                  tabPanel("Introduction",
                           br(),
                           fluidRow(
                             column(7,
                                    h3("Qu'est-ce que l'analyse de la variance?"),
                                    p("L'analyse de la variance (ANOVA) est une méthode statistique
                 qui permet de comparer les moyennes de plusieurs groupes
                 simultanément, en décomposant la variabilité totale des
                 observations en composantes attribuables à des sources
                 identifiables."),
                                    p("Elle généralise le test t de Student à k ≥ 2 groupes tout en
                 maintenant le taux d'erreur de Type I à son niveau nominal α,
                 ce qu'une série de tests t individuels ne ferait pas."),
                                    
                                    h4("Hypothèses testées"),
                                    div(class = "formule",
                                        withMathJax(
                                          "\\( H_0 : \\mu_1 = \\mu_2 = \\cdots = \\mu_k \\)",
                                          br(),
                                          "\\( H_1 : \\text{au moins une moyenne diffère} \\)"
                                        )
                                    ),
                                    
                                    h4("Décomposition de la variance"),
                                    p("Le principe fondamental repose sur la partition de la somme
                 des carrés totale :"),
                                    div(class = "formule",
                                        withMathJax(
                                          "\\[ \\underbrace{\\sum_{i=1}^{k}\\sum_{j=1}^{n_i}
                      (Y_{ij} - \\bar{Y}_{..})^2}_{\\text{SCT}} =
                     \\underbrace{\\sum_{i=1}^{k} n_i
                      (\\bar{Y}_{i.} - \\bar{Y}_{..})^2}_{\\text{SCE}} +
                     \\underbrace{\\sum_{i=1}^{k}\\sum_{j=1}^{n_i}
                      (Y_{ij} - \\bar{Y}_{i.})^2}_{\\text{SCR}} \\]"
                                        )
                                    ),
                                    
                                    h4("Tableau de l'ANOVA"),
                                    div(class = "formule",
                                        withMathJax(
                                          "\\[
                    \\begin{array}{lcccc}
                    \\hline
                    \\text{Source} & \\text{SC} & \\text{ddl} &
                      \\text{CM} & F \\\\
                    \\hline
                    \\text{Entre groupes} & \\text{SCE} & k-1 &
                      \\frac{\\text{SCE}}{k-1} &
                      \\frac{\\text{CME}}{\\text{CMR}} \\\\[6pt]
                    \\text{Résiduelle} & \\text{SCR} & N-k &
                      \\frac{\\text{SCR}}{N-k} & \\\\[6pt]
                    \\text{Totale} & \\text{SCT} & N-1 & & \\\\
                    \\hline
                    \\end{array}
                  \\]"
                                        )
                                    ),
                                    p(withMathJax(
                                      "Sous \\(H_0\\), la statistique \\(F\\) suit une distribution
                 \\(F(k-1,\\, N-k)\\). On rejette \\(H_0\\) si
                 \\(F_{\\text{obs}} > F_{\\text{critique}}\\)."
                                    )),
                                    
                                    h4("Postulats"),
                                    tags$ul(
                                      tags$li(withMathJax(
                                        strong("Normalité : "),
                                        "Les résidus \\(\\varepsilon_{ij} \\sim
                   \\mathcal{N}(0,\\sigma^2)\\) dans chaque groupe."
                                      )),
                                      tags$li(withMathJax(
                                        strong("Homoscédasticité : "),
                                        "\\(\\sigma_1^2 = \\sigma_2^2 = \\cdots = \\sigma_k^2\\)."
                                      )),
                                      tags$li(
                                        strong("Indépendance : "),
                                        "Les observations sont indépendantes les unes des autres."
                                      )
                                    ),
                                    
                                    h4("Taille d'effet"),
                                    div(class = "formule",
                                        withMathJax(
                                          "\\[ \\eta^2 = \\frac{\\text{SCE}}{\\text{SCT}} \\qquad
                     \\omega^2 = \\frac{\\text{SCE} - (k-1)\\,\\text{CMR}}
                                      {\\text{SCT} + \\text{CMR}} \\qquad
                     f = \\sqrt{\\frac{\\eta^2}{1 - \\eta^2}} \\]"
                                        )
                                    ),
                                    p(withMathJax(
                                      "\\(\\omega^2\\) corrige le biais optimiste de \\(\\eta^2\\) : il est préférable pour estimer l'effet dans la population.
                 Conventions de Cohen (1988) : petit \\(f = 0{,}10\\),
                 moyen \\(f = 0{,}25\\), grand \\(f = 0{,}40\\)."
                                    ))
                             ),
                             
                             column(5,
                                    div(class = "info-box",
                                        h4("🗺️ Guide d'utilisation"),
                                        p(strong("Panneau latéral gauche"), " — disponible en tout temps :"),
                                        tags$ul(
                                          tags$li("Choisissez le nombre de groupes (2 à 5) et la
                           taille d'échantillon par groupe. La graine rend
                           l'échantillon reproductible; le bouton « Nouvel
                           échantillon » en tire une autre."),
                                          tags$li("Ajustez le déséquilibre, ou inscrivez des tailles
                           précises (ex. 20; 30; 45), pour simuler des groupes
                           de tailles inégales."),
                                          tags$li("Ajustez les moyennes (μ) et écarts-types (σ) de
                           chaque groupe. Les changements se propagent
                           immédiatement dans tous les onglets."),
                                          tags$li("Sélectionnez une transformation de Y si les
                           postulats sont violés (√Y, ln Y, etc.).")
                                        ),
                                        hr(),
                                        p(strong("Onglet Données")),
                                        tags$ul(
                                          tags$li(strong("Conception : "),
                                                  "distributions théoriques N(μ, σ²) avant simulation."),
                                          tags$li(strong("Données simulées : "),
                                                  "boîtes à moustaches, violin plots, nuage de points
                     et intervalles de confiance."),
                                          tags$li(strong("Postulats : "),
                                                  "normalité (Shapiro-Wilk, Q-Q), homogénéité des
                     variances (Levene, Bartlett), indépendance, et
                     alternative non paramétrique (Kruskal-Wallis).")
                                        ),
                                        hr(),
                                        p(strong("Onglet Analyse")),
                                        tags$ul(
                                          tags$li(strong("Décomposition : "),
                                                  "visualisez SCT = SCE + SCR observation par observation."),
                                          tags$li(strong("Tableau ANOVA : "),
                                                  "tableau complet, ANOVA de Welch, distribution F."),
                                          tags$li(strong("Comparaisons multiples : "),
                                                  "9 procédures (variances homogènes) ou 3 procédures
                     robustes (variances hétérogènes)."),
                                          tags$li(strong("Taille d'effet & Puissance : "),
                                                  "η², ω², f de Cohen, courbes de puissance.")
                                        ),
                                        hr(),
                                        div(class = "ok-box",
                                            "💡 ", strong("Astuce pédagogique : "),
                                            "rapprochez les moyennes jusqu'à ce que F devienne
                   non significatif, puis augmentez les écarts-types et
                   observez l'effet sur la puissance."
                                        )
                                    ),
                                    br(),
                                    div(class = "warn-box",
                                        h4("⚠️ Limites de l'application"),
                                        tags$ul(
                                          tags$li("Les données sont simulées selon des lois normales.
                           Au démarrage : 3 groupes de 30, μ = 50, 54, 60, σ = 10,
                           graine 2026 (le cas fil rouge du manuel)."),
                                          tags$li("Plan complètement aléatoire à effets fixes uniquement."),
                                          tags$li("L'ANOVA est robuste aux légères violations de
                           normalité (grand n), mais sensible à
                           l'hétéroscédasticité sévère.")
                                        )
                                    )
                             )
                           )
                  ),  # fin Introduction
                  
                  
                  # ── DONNÉES ─────────────────────────────────────────────────────────
                  tabPanel("Données",
                           tabsetPanel(type = "pills",
                                       
                                       tabPanel("Conception",
                                                br(),
                                                div(class = "info-box",
                                                    "Les distributions ci-dessous reflètent les paramètres μ et σ
                 du panneau latéral. Modifiez-les et observez l'impact
                 en temps réel."),
                                                plotOutput("plot_conception", height = "420px"),
                                                uiOutput("variance_conception"),
                                                br(),
                                                conditionalPanel("input.type_vi == 'quanti'",
                                                                 div(class = "warn-box",
                                                                     "⚠️ VI quantitative ou ordinale : l'ANOVA traite tout de même
                   les niveaux comme des catégories (c'est une régression sur
                   k − 1 variables indicatrices). Pour tenir compte de l'ordre,
                   utilisez l'onglet Analyse > Comparaisons > Tendance polynomiale.")
                                                )
                                       ),  # fin Conception
                                       
                                       tabPanel("Données simulées",
                                                br(),
                                                fluidRow(
                                                  column(3,
                                                         wellPanel(
                                                           h4("📊 Type de graphique"),
                                                           checkboxGroupInput("types_graph", NULL,
                                                                              c("Boîtes à moustaches" = "boxplot",
                                                                                "Violin plot"         = "violin",
                                                                                "Points (jitter)"     = "jitter",
                                                                                "Moyenne ± IC 95%"    = "mean_ci"),
                                                                              selected = c("boxplot", "jitter", "mean_ci"))
                                                         )
                                                  ),
                                                  column(9,
                                                         plotOutput("plot_donnees", height = "420px"),
                                                         br(),
                                                         h4("📋 Statistiques descriptives"),
                                                         DTOutput("tableau_desc"),
                                                         br(),
                                                         h4("Aperçu des données brutes"),
                                                         DTOutput("tableau_brut")
                                                  )
                                                )
                                       ),  # fin Données simulées
                                       
                                       tabPanel("Postulats",
                                                br(),
                                                div(class = "info-box",
                                                    "L'ANOVA repose sur trois postulats : (1) normalité des
                 résidus, (2) homogénéité des variances,
                 (3) indépendance des observations."),
                                                tabsetPanel(
                                                  
                                                  tabPanel("Normalité",
                                                           br(),
                                                           fluidRow(
                                                             column(6,
                                                                    h4("Histogrammes par groupe"),
                                                                    plotOutput("plot_hist", height = "410px")
                                                             ),
                                                             column(6,
                                                                    h4("Diagrammes Q-Q par groupe"),
                                                                    plotOutput("plot_qq", height = "410px")
                                                             )
                                                           ),
                                                           br(),
                                                           h4("Test de Shapiro-Wilk par groupe"),
                                                           div(class = "warn-box",
                                                               "H₀ : La distribution est normale. p < 0,05 → violation."),
                                                           DTOutput("tableau_shapiro"),
                                                           br(),
                                                           uiOutput("conclusion_normalite")
                                                  ),  # fin Normalité
                                                  
                                                  tabPanel("Homogénéité des variances",
                                                           br(),
                                                           fluidRow(
                                                             column(6,
                                                                    h4("Écarts-types observés par groupe"),
                                                                    plotOutput("plot_sd", height = "340px"),
                                                                    uiOutput("conclusion_homo")
                                                             ),
                                                             column(6,
                                                                    h4("Tests formels"),
                                                                    div(class = "warn-box",
                                                                        "H₀ : σ₁² = σ₂² = … = σₖ²"),
                                                                    br(),
                                                                    h4("Test de Levene"),
                                                                    uiOutput("test_levene"),
                                                                    h4("Test de Bartlett"),
                                                                    uiOutput("test_bartlett")
                                                             )
                                                           )
                                                  ),  # fin Homogénéité
                                                  
                                                  tabPanel("Indépendance",
                                                           br(),
                                                           div(class = "ok-box",
                                                               "✔️ Données simulées : chaque observation est tirée
                     indépendamment des autres. Avec des données réelles,
                     l'indépendance tient au plan de collecte (une mesure par
                     personne, pas de grappes, pas de contamination entre
                     conditions); aucun graphique ne peut la garantir."),
                                                           br(),
                                                           h4("Résidus dans l'ordre des observations"),
                                                           div(class = "info-box",
                                                               "Une tendance systématique indiquerait un problème
                     d'indépendance."),
                                                           plotOutput("plot_residus_ordre", height = "340px")
                                                  ),  # fin Indépendance
                                                  
                                                  tabPanel("Non paramétrique",
                                                           br(),
                                                           div(class = "info-box",
                                                               "Le test de Kruskal-Wallis est l'alternative non
                     paramétrique à l'ANOVA. Il ne suppose pas la normalité :
                     il compare les rangs plutôt que les moyennes. Pour être lu
                     comme une comparaison de médianes, il suppose des
                     distributions de même forme; des variances très inégales
                     le perturbent aussi. Seuil α : celui de l'onglet
                     Analyse > Tableau ANOVA.",
                                                               br(), br(),
                                                               "H₀ : Les distributions des groupes sont identiques.",
                                                               br(),
                                                               "H₁ : Au moins un groupe a une distribution différente."
                                                           ),
                                                           fluidRow(
                                                             column(6,
                                                                    h4("Test de Kruskal-Wallis"),
                                                                    uiOutput("kruskal_test"),
                                                                    br(),
                                                                    uiOutput("kruskal_conclusion")
                                                             ),
                                                             column(6,
                                                                    h4("Comparaisons post-hoc de Dunn"),
                                                                    div(class = "warn-box", style = "font-size:12px;",
                                                                        "À effectuer seulement si Kruskal-Wallis est significatif."),
                                                                    selectInput("dunn_method", "Correction",
                                                                                c("Bonferroni"               = "bonferroni",
                                                                                  "Holm"                     = "holm",
                                                                                  "Benjamini-Hochberg (FDR)"  = "bh",
                                                                                  "Aucune"                   = "none"),
                                                                                selected = "bonferroni"
                                                                    ),
                                                                    DTOutput("dunn_test")
                                                             )
                                                           ),
                                                           br(),
                                                           div(class = "info-box",
                                                               h4("Quand utiliser Kruskal-Wallis plutôt que l'ANOVA?"),
                                                               tags$ul(
                                                                 tags$li("Normalité clairement violée ET n petit (< 30 par groupe)"),
                                                                 tags$li("Variable dépendante ordinale"),
                                                                 tags$li("Présence de valeurs extrêmes influentes"),
                                                                 tags$li(
                                                                   "L'ANOVA est robuste aux violations légères de normalité
                         quand n est grand — les deux tests donnent alors
                         des conclusions similaires."
                                                                 )
                                                               )
                                                           )
                                                  )  # fin Non paramétrique
                                                  
                                                )  # fin tabsetPanel Postulats
                                       )  # fin tabPanel Postulats
                                       
                           )  # fin tabsetPanel Données (pills)
                  ),  # fin tabPanel Données
                  
                  
                  # ── ANALYSE ─────────────────────────────────────────────────────────
                  tabPanel("Analyse",
                           tabsetPanel(type = "pills",
                                       
                                       tabPanel("Décomposition",
                                                br(),
                                                div(class = "info-box",
                                                    "SCT = SCE + SCR. Manipulez μ et σ dans le panneau latéral
                 pour voir comment chaque composante évolue."),
                                                fluidRow(
                                                  column(5,
                                                         h4("Formule de décomposition"),
                                                         div(class = "formule",
                                                             withMathJax("\\[ \\text{SCT} = \\text{SCE} + \\text{SCR} \\]")
                                                         ),
                                                         div(class = "formule",
                                                             withMathJax(
                                                               "\\[ \\sum_{ij}(Y_{ij}-\\bar{Y}_{..})^2 =
                          \\sum_{i} n_i(\\bar{Y}_{i.}-\\bar{Y}_{..})^2 +
                          \\sum_{ij}(Y_{ij}-\\bar{Y}_{i.})^2 \\]"
                                                             )
                                                         ),
                                                         br(),
                                                         h4("Valeurs calculées"),
                                                         tableOutput("tableau_decomp"),
                                                         br(),
                                                         h4("Proportion de variance expliquée"),
                                                         plotOutput("plot_pie", height = "250px")
                                                  ),
                                                  column(7,
                                                         h4("Visualisation de la décomposition"),
                                                         sliderInput("obs_selectionnee",
                                                                     "Observation à décomposer",
                                                                     1, 100, 1, 1, width = "100%"),
                                                         plotOutput("plot_decomp", height = "440px")
                                                  )
                                                )
                                       ),  # fin Décomposition
                                       
                                       tabPanel("Tableau ANOVA",
                                                br(),
                                                fluidRow(
                                                  column(6,
                                                         h4("Tableau de l'ANOVA"),
                                                         DTOutput("tableau_anova"),
                                                         br(),
                                                         uiOutput("conclusion_anova"),
                                                         br(),
                                                         uiOutput("welch_anova"),
                                                         br(),
                                                         h4("Formules"),
                                                         div(class="formule",
                                                             withMathJax("\\( \\text{ddl}_{\\text{entre}} = k - 1 \\)")),
                                                         div(class="formule",
                                                             withMathJax("\\( \\text{ddl}_{\\text{résiduel}} = N - k \\)")),
                                                         div(class="formule",
                                                             withMathJax("\\( \\text{CME} = \\dfrac{\\text{SCE}}{k-1} \\)")),
                                                         div(class="formule",
                                                             withMathJax("\\( \\text{CMR} = \\dfrac{\\text{SCR}}{N-k} \\)")),
                                                         div(class="formule",
                                                             withMathJax(
                                                               "\\( F = \\dfrac{\\text{CME}}{\\text{CMR}}
                       \\sim F(k-1,\\, N-k) \\text{ sous } H_0 \\)"))
                                                  ),
                                                  column(6,
                                                         h4("Distribution F et région critique"),
                                                         plotOutput("plot_F", height = "380px"),
                                                         br(),
                                                         sliderInput("alpha_anova", "Seuil α",
                                                                     .001, .20, .05, .001, width = "100%")
                                                  )
                                                )
                                       ),  # fin Tableau ANOVA
                                       
                                       # ── COMPARAISONS (onglet regroupé) ───────────────────────────────
                                       tabPanel("Comparaisons",
                                                br(),
                                                div(class = "info-box",
                                                    "Cet onglet regroupe toutes les approches de comparaison
                 entre groupes : comparaisons ", strong("a posteriori"),
                                                    " (après avoir observé un F significatif) et contrastes ",
                                                    strong("a priori"), " (hypothèses formulées avant
                 la collecte des données)."
                                                ),
                                                tabsetPanel(type = "tabs",
                                                            
                                                            # ── A posteriori ─────────────────────────────────────────────
                                                            tabPanel("A posteriori",
                                                                     br(),
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                h4("🔧 Options"),
                                                                                radioButtons("famille_cm", "Famille",
                                                                                             c("Variances homogènes"   = "homo",
                                                                                               "Variances hétérogènes" = "hetero"),
                                                                                             selected = "homo"
                                                                                ),
                                                                                conditionalPanel("input.famille_cm == 'homo'",
                                                                                                 selectInput("methode_homo", "Procédure",
                                                                                                             c("Tukey HSD"                  = "tukey",
                                                                                                               "Bonferroni"                 = "bonferroni",
                                                                                                               "Holm (Bonferroni séq.)"     = "holm",
                                                                                                               "Hochberg"                   = "hochberg",
                                                                                                               "Benjamini-Hochberg (FDR)"   = "fdr",
                                                                                                               "Scheffé"                    = "scheffe",
                                                                                                               "Duncan"                     = "duncan",
                                                                                                               "Student-Newman-Keuls (SNK)" = "snk",
                                                                                                               "LSD Fisher (non corrigé)"   = "none"),
                                                                                                             selected = "tukey"
                                                                                                 ),
                                                                                                 uiOutput("ui_warn_homo")
                                                                                ),
                                                                                conditionalPanel("input.famille_cm == 'hetero'",
                                                                                                 selectInput("methode_hetero", "Procédure",
                                                                                                             c("Games-Howell (recommandé)" = "games_howell",
                                                                                                               "Tamhane T2"                = "tamhane",
                                                                                                               "Dunnett T3"                = "dunnett3"),
                                                                                                             selected = "games_howell"
                                                                                                 ),
                                                                                                 div(class = "warn-box",
                                                                                                     "⚠️ Ces procédures ne supposent pas l'égalité
                             des variances.")
                                                                                ),
                                                                                sliderInput("alpha_cm", "Seuil α",
                                                                                            .001, .20, .05, .001),
                                                                                hr(),
                                                                                h4("Levene (orientation)"),
                                                                                uiOutput("ui_levene_resume")
                                                                              )
                                                                       ),
                                                                       column(8,
                                                                              h4("IC simultanés des moyennes"),
                                                                              plotOutput("plot_cm", height = "400px"),
                                                                              br(),
                                                                              h4("Tableau des comparaisons par paires"),
                                                                              DTOutput("tableau_cm"),
                                                                              br(),
                                                                              uiOutput("ui_groupes_homogenes")
                                                                       )
                                                                     )
                                                            ),  # fin A posteriori
                                                            
                                                            # ── Contrastes standards ──────────────────────────────────────
                                                            tabPanel("Contrastes standards",
                                                                     br(),
                                                                     div(class = "info-box",
                                                                         "Les contrastes standards définissent automatiquement
                     un jeu de k−1 comparaisons selon un schéma prédéfini. Ils
                     se choisissent ", strong("a priori"), ". Seuls les contrastes
                     de Helmert et de différence sont orthogonaux (avec des
                     groupes de même taille); pour les autres, une correction
                     pour tests multiples est prudente."
                                                                     ),
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                h4("🔧 Options"),
                                                                                selectInput("contraste_std", "Type de codage",
                                                                                            c("Déviation (vs grande moyenne)"     = "deviation",
                                                                                              "Simple (vs groupe de référence)"   = "simple",
                                                                                              "Différence / Helmert inverse"      = "difference",
                                                                                              "Helmert (vs moyenne des suivants)" = "helmert",
                                                                                              "Groupes adjacents (repeated)"      = "repeated"),
                                                                                            selected = "deviation"
                                                                                ),
                                                                                conditionalPanel("input.contraste_std == 'simple'",
                                                                                                 uiOutput("ui_groupe_ref")
                                                                                ),
                                                                                sliderInput("alpha_contraste", "Seuil α",
                                                                                            .001, .20, .05, .001),
                                                                                checkboxInput("contraste_ajuste",
                                                                                              "Ajustement de Bonferroni", FALSE),
                                                                                hr(),
                                                                                uiOutput("ui_desc_contraste")
                                                                              )
                                                                       ),
                                                                       column(8,
                                                                              h4("Matrice de contrastes"),
                                                                              div(class = "info-box", style = "font-size:12px;",
                                                                                  "Chaque ligne est un contraste. Les coefficients
                         indiquent le poids de chaque groupe.
                         La somme de chaque ligne doit être 0."),
                                                                              tableOutput("tableau_matrice_c"),
                                                                              br(),
                                                                              uiOutput("ui_ortho_check")
                                                                       )
                                                                     ),
                                                                     br(),
                                                                     fluidRow(
                                                                       column(12,
                                                                              h4("Résultats des contrastes"),
                                                                              DTOutput("tableau_contrastes_std"),
                                                                              br(),
                                                                              uiOutput("conclusion_contrastes_std")
                                                                       ),
                                                                       column(12,
                                                                              h4("Visualisation"),
                                                                              plotOutput("plot_contrastes_std", height = "420px")
                                                                       )
                                                                     )
                                                            ),  # fin Contrastes standards
                                                            
                                                            # ── Tendance polynomiale ──────────────────────────────────────
                                                            tabPanel("Tendance polynomiale",
                                                                     br(),
                                                                     div(class = "info-box",
                                                                         "L'analyse de tendance décompose l'effet global en
                     composantes polynomiales orthogonales : linéaire,
                     quadratique, cubique, etc. Appropriée quand les groupes
                     représentent des niveaux ", strong("ordonnés"),
                                                                         " (doses, temps, scores)."
                                                                     ),
                                                                     fluidRow(
                                                                       column(4,
                                                                              wellPanel(
                                                                                h4("🔧 Options"),
                                                                                sliderInput("alpha_poly", "Seuil α",
                                                                                            .001, .20, .05, .001),
                                                                                hr(),
                                                                                div(class = "warn-box", style = "font-size:12px;",
                                                                                    "⚠️ Suppose des groupes ordonnés et des intervalles
                           approximativement égaux."),
                                                                                hr(),
                                                                                h4("Scores des niveaux"),
                                                                                div(class = "info-box", style = "font-size:12px;",
                                                                                    "Par défaut : 1, 2, 3, ... (équidistants)."),
                                                                                uiOutput("ui_scores_poly")
                                                                              )
                                                                       ),
                                                                       column(8,
                                                                              h4("Polynômes orthogonaux — matrice de contrastes"),
                                                                              div(class = "info-box", style = "font-size:12px;",
                                                                                  "Coefficients calculés par ", code("contr.poly()"),
                                                                                  " à partir des scores des niveaux. Ils sont orthogonaux au sens strict
                                                                                  (Σ cᵢdᵢ/nᵢ = 0) quand les groupes ont la même taille."),
                                                                              tableOutput("tableau_matrice_poly"),
                                                                              br(),
                                                                              uiOutput("ui_ortho_poly")
                                                                       )
                                                                     ),
                                                                     br(),
                                                                     fluidRow(
                                                                       column(12,
                                                                              h4("Résultats par composante"),
                                                                              DTOutput("tableau_tendance"),
                                                                              br(),
                                                                              uiOutput("conclusion_tendance")
                                                                       ),
                                                                       column(12,
                                                                              h4("Graphique de tendance"),
                                                                              plotOutput("plot_tendance", height = "440px")
                                                                       )
                                                                     )
                                                            ),  # fin Tendance polynomiale
                                                            
                                                            # ── Contrastes planifiés ──────────────────────────────────────
                                                            tabPanel("Contrastes planifiés",
                                                                     br(),
                                                                     div(class = "info-box",
                                                                         "Les contrastes planifiés sont définis ", strong("avant"),
                                                                         " la collecte des données. La somme de chaque ligne
                     doit être ", strong("0"),
                                                                         ". Jusqu'à ", strong("k−1"),
                                                                         " contrastes indépendants sont possibles."
                                                                     ),
                                                                     fluidRow(
                                                                       column(3,
                                                                              wellPanel(
                                                                                h4("🔧 Contrastes"),
                                                                                fluidRow(
                                                                                  column(6,
                                                                                         actionButton("ajouter_contraste", "+ Ajouter",
                                                                                                      class = "btn-success btn-sm",
                                                                                                      width = "100%")
                                                                                  ),
                                                                                  column(6,
                                                                                         actionButton("retirer_contraste", "− Retirer",
                                                                                                      class = "btn-danger btn-sm",
                                                                                                      width = "100%")
                                                                                  )
                                                                                ),
                                                                                br(),
                                                                                uiOutput("ui_nb_contrastes_info"),
                                                                                hr(),
                                                                                sliderInput("alpha_planifie", "Seuil α",
                                                                                            .001, .20, .05, .001),
                                                                                selectInput("ajust_planifie", "Correction",
                                                                                            c("Aucune"     = "none",
                                                                                              "Bonferroni" = "bonferroni",
                                                                                              "Holm"       = "holm"),
                                                                                            selected = "none"
                                                                                ),
                                                                                hr(),
                                                                                actionButton("reset_contrastes", "↺ Réinitialiser",
                                                                                             class = "btn-default btn-sm",
                                                                                             width = "100%")
                                                                              )
                                                                       ),
                                                                       column(9,
                                                                              h4("Grille des coefficients"),
                                                                              div(class = "info-box", style = "font-size:12px;",
                                                                                  "Chaque ligne = un contraste. Chaque colonne = un groupe.
                         La somme de chaque ligne doit être 0."),
                                                                              uiOutput("ui_grille_contrastes"),
                                                                              br(),
                                                                              uiOutput("ui_validation_planifie")
                                                                       )
                                                                     ),
                                                                     br(),
                                                                     fluidRow(
                                                                       column(12,
                                                                              h4("Résultats"),
                                                                              DTOutput("tableau_planifie"),
                                                                              br(),
                                                                              uiOutput("conclusion_planifie")
                                                                       ),
                                                                       column(12,
                                                                              h4("Visualisation"),
                                                                              plotOutput("plot_planifie", height = "440px")
                                                                       )
                                                                     )
                                                            )  # fin Contrastes planifiés — pas de virgule : dernier
                                                            
                                                )  # fin tabsetPanel Comparaisons (tabs)
                                       ),  # fin tabPanel Comparaisons
                                       
                                       tabPanel("Taille d'effet & Puissance",
                                                br(),
                                                fluidRow(
                                                  column(5,
                                                         h4("Mesures de la taille d'effet"),
                                                         div(class = "info-box",
                                                             withMathJax(
                                                               "\\(\\eta^2\\) : proportion de variance expliquée.",
                                                               br(),
                                                               "\\(\\omega^2\\) : estimation moins biaisée pour la population (préférée).",
                                                               br(),
                                                               "\\(f\\) de Cohen : référence pour le calcul de puissance."
                                                             )
                                                         ),
                                                         br(),
                                                         tableOutput("tableau_effet"),
                                                         br(),
                                                         h4("Conventions (Cohen, 1988)"),
                                                         tableOutput("tableau_conventions"),
                                                         br(),
                                                         uiOutput("conclusion_effet")
                                                  ),
                                                  column(7,
                                                         h4("Courbes de puissance"),
                                                         div(class = "info-box",
                                                             withMathJax(
                                                               "Probabilité de détecter un effet de taille \\(f\\)
                       donnée selon \\(n\\) par groupe."
                                                             )
                                                         ),
                                                         sliderInput("f_cible", "f de Cohen cible",
                                                                     .10, .80, .25, .01, width = "100%"),
                                                         sliderInput("alpha_puissance", "Seuil α",
                                                                     .001, .20, .05, .001, width = "100%"),
                                                         plotOutput("plot_puissance", height = "390px"),
                                                         uiOutput("n_recommande")
                                                  )
                                                )
                                       )  # fin Taille d'effet — pas de virgule : dernier tabPanel
                                       
                           )  # fin tabsetPanel Analyse (pills)
                  )  # fin tabPanel Analyse
                  
      )  # fin tabsetPanel principal
    )  # fin mainPanel
  )
)
    
server <- function(input, output, session) {
  
  # =========================================================================
  # UTILITAIRES (définis en premier, utilisés partout)
  # =========================================================================

  # Domaine de chaque transformation : si une valeur en sort, on n'applique
  # PAS la transformation (on garderait sinon des valeurs tronquées en silence)
  transfo_valide <- function(y, transfo) {
    switch(transfo,
           sqrt = all(y >= 0), log = all(y > 0), log10 = all(y > 0),
           invsqrt = all(y > 0), arcsin = all(y >= 0 & y <= 1), TRUE)
  }
  appliquer_transfo <- function(y, transfo) {
    if (is.null(transfo) || transfo == "none" || !transfo_valide(y, transfo)) return(y)
    switch(transfo,
           sqrt    = sqrt(y),
           log     = log(y),
           log10   = log10(y),
           square  = y^2,
           invsqrt = 1 / sqrt(y),
           arcsin  = asin(sqrt(y)),
           y
    )
  }
  
  shapiro_safe <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3)
      return(list(W = NA, p = NA, msg = "⚠️ Moins de 3 valeurs finies"))
    if (length(unique(x)) == 1)
      return(list(W = NA, p = NA, msg = "⚠️ Valeurs identiques (transformation hors domaine?)"))
    if (length(x) > 5000) x <- sample(x, 5000)
    tryCatch({
      res <- shapiro.test(x)
      list(W = round(res$statistic, 4), p = round(res$p.value, 4), msg = NULL)
    }, error = function(e) {
      list(W = NA, p = NA, msg = paste0("⚠️ ", conditionMessage(e)))
    })
  }
  
  paires_depuis_pmcmr <- function(res_pmcmr, d, alph) {
    sm      <- summary(res_pmcmr)
    p_mat   <- sm$p.value
    niveaux <- rownames(p_mat)
    stats_grp <- d %>%
      group_by(groupe_f) %>%
      summarise(m = mean(valeur_t), s = sd(valeur_t), n = n(), .groups = "drop") %>%
      mutate(groupe_c = as.character(groupe_f))
    paires <- which(!is.na(p_mat), arr.ind = TRUE)
    if (nrow(paires) == 0) return(data.frame(Erreur = "Aucune paire disponible"))
    do.call(rbind, lapply(seq_len(nrow(paires)), function(i) {
      r <- paires[i,1]; c2 <- paires[i,2]
      # même convention que Tukey : groupe de rang inférieur moins groupe de rang supérieur
      g1 <- sort(c(niveaux[r], niveaux[c2]))[1]; g2 <- sort(c(niveaux[r], niveaux[c2]))[2]
      s1 <- stats_grp[stats_grp$groupe_c == g1, ]
      s2 <- stats_grp[stats_grp$groupe_c == g2, ]
      if (nrow(s1) == 0 || nrow(s2) == 0) {
        return(data.frame(Contraste = paste(g1,"-",g2), Différence = NA_real_,
                          SE = NA_real_, ddl = NA_real_,
                          `p ajusté` = round(p_mat[r,c2],4),
                          Décision = ifelse(p_mat[r,c2] < alph,
                                            "⚠️ Significatif","✔️ Non significatif"),
                          stringsAsFactors = FALSE, check.names = FALSE))
      }
      diff_val <- s1$m - s2$m
      se_val   <- sqrt(s1$s^2/s1$n + s2$s^2/s2$n)
      ddl_val  <- (s1$s^2/s1$n + s2$s^2/s2$n)^2 /
        ((s1$s^2/s1$n)^2/(s1$n-1) + (s2$s^2/s2$n)^2/(s2$n-1))
      p_val    <- p_mat[r,c2]
      data.frame(Contraste = paste(g1,"-",g2),
                 Différence = round(diff_val,4), SE = round(se_val,4),
                 ddl = round(ddl_val,1), `p ajusté` = p_val,
                 Décision = ifelse(p_val < alph,"⚠️ Significatif","✔️ Non significatif"),
                 stringsAsFactors = FALSE, check.names = FALSE)
    }))
  }
  
  # Tests de contrastes (matrice M : une ligne par contraste), avec IC,
  # somme des carrés du contraste et description en mots
  tester_contrastes <- function(M, alph, adj = "none") {
    d   <- donnees()
    st  <- d %>% group_by(groupe_f) %>% summarise(m = mean(valeur_t), n = n(), .groups = "drop")
    N   <- nrow(d); k <- nrow(st)
    cmr <- sum((d$valeur_t - st$m[as.integer(d$groupe_f)])^2) / (N - k)
    ddl <- N - k
    out <- do.call(rbind, lapply(seq_len(nrow(M)), function(i) {
      cf  <- M[i, ]
      psi <- sum(cf * st$m)
      se  <- sqrt(cmr * sum(cf^2 / st$n))
      t   <- psi / se
      data.frame(psi = psi, se = se, t = t, ddl = ddl,
                 p = 2 * pt(abs(t), ddl, lower.tail = FALSE),
                 sc = psi^2 / sum(cf^2 / st$n))
    }))
    m <- nrow(M)
    out$p_aj <- if (adj == "none") out$p else p.adjust(out$p, adj)
    tcrit <- qt(1 - alph / (2 * if (adj == "bonferroni") m else 1), ddl)
    out$lo <- out$psi - tcrit * out$se; out$hi <- out$psi + tcrit * out$se
    out$nom <- rownames(M)
    out$desc <- vapply(seq_len(m), function(i) decrire_contraste(M[i, ], colnames(M)), character(1))
    sce <- sum(st$n * (st$m - mean(d$valeur_t))^2)
    out$pct <- out$sc / sce
    ortho <- if (m >= 2) all(combn(m, 2, function(p) abs(produit_contrastes(M[p[1], ], M[p[2], ], st$n)) < 1e-8)) else TRUE
    attr(out, "ortho") <- ortho
    attr(out, "complet") <- ortho && m == k - 1
    attr(out, "n") <- st$n
    out
  }

  # Mise en forme d'un tableau de contrastes
  afficher_contrastes <- function(r, alph, adj, premiere = "Contraste", avec_desc = TRUE) {
    df <- data.frame(r$nom, r$desc, fr(r$psi, 3), fr(r$se, 3),
                     paste0("[", fr(r$lo, 2), " ; ", fr(r$hi, 2), "]"),
                     fr(r$t, 2), r$ddl, fr_p(r$p), check.names = FALSE)
    names(df) <- c(premiere, "Compare", "ψ̂", "Erreur type",
                   paste0("IC à ", fr(100 * (1 - alph), if (alph < .01) 1 else 0), " %",
                          if (adj == "bonferroni") " (Bonferroni)" else ""),
                   "t", "ddl", "p")
    if (adj != "none") df[["p ajusté"]] <- fr_p(r$p_aj)
    if (isTRUE(attr(r, "complet"))) df[["% SCE"]] <- paste0(fr(100 * r$pct, 1), " %")
    if (!avec_desc) df$Compare <- NULL
    df$Décision <- ifelse(r$p_aj < alph, "⚠️ Significatif", "✔️ Non significatif")
    datatable(df, options = list(dom = "t", paging = FALSE, scrollX = TRUE,
                                 columnDefs = list(list(className = "dt-right", targets = 2:(ncol(df) - 2)))),
              rownames = FALSE) %>%
      formatStyle("Décision", backgroundColor = styleEqual(
        c("⚠️ Significatif", "✔️ Non significatif"), c("#fdecea", "#eafaf1")))
  }

  # Message d'orthogonalité (tient compte des effectifs)
  message_ortho <- function(M, n) {
    m <- nrow(M)
    if (m < 2) return(div(class = "ok-box", "✔️ Un seul contraste : l'orthogonalité ne se pose pas."))
    prods <- combn(m, 2, function(p) produit_contrastes(M[p[1], ], M[p[2], ], n))
    noms  <- combn(m, 2, function(p) paste0(rownames(M)[p[1]], "·", rownames(M)[p[2]]))
    egaux <- length(unique(n)) == 1
    if (all(abs(prods) < 1e-8))
      div(class = "ok-box", "✔️ Contrastes orthogonaux : Σ cᵢdᵢ/nᵢ = 0 pour chaque paire. Leurs estimations sont indépendantes.",
          if (m == length(n) - 1) " Comme il y en a k − 1, leurs sommes des carrés s'additionnent pour donner la SCE (colonne % SCE).")
    else {
      brut <- combn(m, 2, function(p) sum(M[p[1], ] * M[p[2], ]))
      div(class = "warn-box",
          "⚠️ Contrastes non orthogonaux (", paste0(noms, " = ", fr(prods, 3), collapse = " | "), "). ",
          if (!egaux && all(abs(brut) < 1e-8))
            "Ils le seraient avec des groupes de même taille (Σ cᵢdᵢ = 0), mais les effectifs diffèrent. "
          else "",
          "Leurs tests se recoupent : une correction pour tests multiples est recommandée.")
    }
  }

  k_safe <- function() {
    k <- input$nb_groupes
    k <- if (is.null(k) || is.na(suppressWarnings(as.integer(k)))) 3L
    else as.integer(k)
    max(2L, min(5L, k))
  }
  
  # Taille de base (inchangée)
  n_safe <- function() {
    n <- input$n_par_groupe
    n <- if (is.null(n) || is.na(suppressWarnings(as.integer(n)))) 30L
    else as.integer(n)
    max(5L, n)
  }
  
  # Tailles effectives par groupe (réactif — dépend de la graine)
  n_groupes <- reactive({
    g   <- graine_active()
    k   <- k_safe()
    n   <- n_safe()
    dec <- if (is.null(input$desequilibre)) 0L
    else as.integer(input$desequilibre)
    perso <- n_perso_lus()
    if (!is.null(perso)) return(rep_len(perso, k))
    set.seed(g + 1L)   # graine distincte de celle des données
    if (dec == 0L) {
      rep(n, k)
    } else {
      pmax(5L, n + sample(-dec:dec, k, replace = TRUE))
    }
  })

  # Tailles personnalisées « 20; 30; 45 » (NULL si vide ou illisible)
  n_perso_lus <- reactive({
    txt <- trimws(if (is.null(input$n_perso)) "" else input$n_perso)
    if (!nzchar(txt)) return(NULL)
    v <- suppressWarnings(as.integer(trimws(strsplit(txt, "[;,[:space:]]+")[[1]])))
    v <- v[!is.na(v)]
    if (!length(v) || any(v < 3)) return(NULL)
    v
  })

  output$ui_n_perso <- renderUI({
    txt <- trimws(if (is.null(input$n_perso)) "" else input$n_perso)
    if (!nzchar(txt)) return(NULL)
    v <- n_perso_lus()
    if (is.null(v))
      div(class = "warn-box", style = "font-size:11px; padding:6px 10px;",
          "Tailles illisibles (entiers ≥ 3, séparés par « ; ») : les réglages ci-dessus s'appliquent.")
    else
      div(class = "info-box", style = "font-size:11px; padding:6px 10px;",
          "Tailles utilisées : ", paste(rep_len(v, k_safe()), collapse = ", "),
          if (length(v) != k_safe()) " (liste répétée ou tronquée pour k groupes)" else "",
          ". Le déséquilibre aléatoire est ignoré.")
  })
  
  donnees <- reactive({
    g  <- graine_active()
    k  <- k_safe()
    ns <- n_groupes()          # vecteur de longueur k
    p  <- params()
    transfo <- if (is.null(input$transformation)) "none" else input$transformation
    vi      <- if (is.null(input$type_vi)) "nominal" else input$type_vi
    set.seed(g)
    
    df <- do.call(rbind, lapply(seq_len(k), function(i) {
      vals  <- rnorm(ns[i], p[[i]]$moy, p[[i]]$sd)
      label <- if (vi == "quanti") paste0("Niveau ", i)
      else paste("Groupe", LETTERS[i])
      data.frame(groupe    = label,
                 groupe_f  = factor(label),
                 valeur    = vals,
                 id_groupe = i,
                 stringsAsFactors = FALSE)
    }))
    df$obs_id   <- seq_len(nrow(df))
    df$valeur_t <- appliquer_transfo(df$valeur, transfo)
    attr(df, "transfo_ok") <- transfo == "none" || transfo_valide(df$valeur, transfo)
    df
  })
  
  
  # =========================================================================
  # GRAINE
  # =========================================================================
  graine_active <- reactiveVal(2026L)
  
  observeEvent(input$regenerer, {
    g <- sample.int(9999, 1)
    updateNumericInput(session, "graine", value = g)
    graine_active(g)
  })
  
  observeEvent(input$graine, {
    g <- suppressWarnings(as.integer(input$graine))
    if (!is.na(g) && g >= 1) graine_active(g)
  }, ignoreInit = FALSE)
  
  # =========================================================================
  # RÉACTIVES DE BASE
  # =========================================================================
  
  couleurs <- reactive({
    PALETTE[seq_len(k_safe())]
  })
  
  ylabel <- reactive({
    t <- if (is.null(input$transformation)) "none" else input$transformation
    if (!isTRUE(attr(donnees(), "transfo_ok"))) t <- "none"
    switch(t, none="Y", sqrt="√Y", log="ln(Y)", log10="log₁₀(Y)",
           square="Y²", invsqrt="1/√Y", arcsin="arcsin(√Y)", "Y")
  })
  
  params <- reactive({
    k <- k_safe()
    lapply(seq_len(k), function(i) {
      moy <- input[[paste0("moy_", i)]]
      sd  <- input[[paste0("sd_",  i)]]
      if (is.null(moy) || length(moy) == 0 || is.na(moy)) moy <- MOY_DEFAUT[i]
      if (is.null(sd)  || length(sd)  == 0 || is.na(sd))  sd  <- SD_DEFAUT
      list(moy = moy, sd = sd)
    })
  })
  
  modele <- reactive({
    aov(valeur_t ~ groupe_f, data = donnees())
  })
  
  df_residus <- reactive({
    d <- donnees()
    d$residus  <- residuals(modele())
    d$ajustees <- fitted(modele())
    d
  })
  
  decomp <- reactive({
    d  <- donnees()
    gd <- mean(d$valeur_t)
    gp <- d %>% group_by(groupe_f) %>%
      summarise(moy = mean(valeur_t), .groups = "drop")
    d  <- left_join(d, gp, by = "groupe_f")
    SCT <- sum((d$valeur_t - gd)^2)
    SCE <- sum((d$moy      - gd)^2)
    SCR <- sum((d$valeur_t - d$moy)^2)
    list(d=d, gd=gd, SCT=SCT, SCE=SCE, SCR=SCR, N=nrow(d), k=k_safe())
  })
  
  em_obj <- reactive({
    emmeans(modele(), ~ groupe_f)
  })
  
  # =========================================================================
  # UI DYNAMIQUE
  # =========================================================================
  
  output$ui_groupes_sidebar <- renderUI({
    k <- k_safe()
    vi <- if (is.null(input$type_vi)) "nominal" else input$type_vi
    compact_css <- tags$style(HTML("
      .grp-row .form-group { margin-bottom:0; }
      .grp-row .irs { margin-bottom:0; margin-top:2px; }
      .grp-row .irs--shiny .irs-handle { width:14px; height:14px; top:22px; }
      .grp-row .irs--shiny .irs-min,
      .grp-row .irs--shiny .irs-max { font-size:11px; }
      .grp-row .irs--shiny .irs-single { font-size:12px; }
      .grp-label-inline { color:white; padding:2px 7px; border-radius:4px;
        font-weight:bold; font-size:13px; display:inline-block; margin-top:6px; }
      .grp-row { border-bottom:1px solid #e8e8e8; padding-bottom:4px; margin-bottom:4px; }
    "))
    rows <- lapply(seq_len(k), function(i) {
      moy_id <- paste0("moy_", i); sd_id <- paste0("sd_", i)
      grp_label <- if (vi == "quanti") paste0("Niv.",i) else paste("Gr.",LETTERS[i])
      div(class="grp-row",
          fluidRow(
            column(3, div(tags$span(class="grp-label-inline",
                                    style=paste0("background:",PALETTE[i],";"),
                                    grp_label))),
            column(4, div(class="grp-row",
                          sliderInput(moy_id, label=tags$span(style="font-size:14px;","μ"),
                                      min=0, max=100,
                                      value=isolate(if(!is.null(input[[moy_id]])) input[[moy_id]] else MOY_DEFAUT[i]),
                                      step=0.5, width="100%", ticks=FALSE))),
            column(5, div(class="grp-row",
                          sliderInput(sd_id, label=tags$span(style="font-size:14px;","σ"),
                                      min=0.5, max=30,
                                      value=isolate(if(!is.null(input[[sd_id]])) input[[sd_id]] else SD_DEFAUT),
                                      step=0.5, width="100%", ticks=FALSE)))
          )
      )
    })
    tagList(compact_css, rows)
  })
  
  output$ui_transfo_warning <- renderUI({
    t <- if (is.null(input$transformation)) "none" else input$transformation
    if (t != "none" && !isTRUE(attr(donnees(), "transfo_ok"))) {
      y <- donnees()$valeur
      cond <- switch(t, sqrt = "des valeurs positives ou nulles", arcsin = "des proportions entre 0 et 1",
                     "des valeurs strictement positives")
      return(div(class = "warn-box", style = "font-size:12px;",
                 "⚠️ Cette transformation exige ", cond, " (minimum observé : ", fr(min(y), 2),
                 ", maximum : ", fr(max(y), 2), "). Les analyses utilisent donc Y brut."))
    }
    switch(t,
           none    = NULL,
           sqrt    = div(class="info-box",style="font-size:12px;",
                         "√Y : stabilise la variance quand σ ∝ μ (Poisson)."),
           log     = div(class="info-box",style="font-size:12px;",
                         "ln(Y) : distributions log-normales. Requiert Y > 0."),
           log10   = div(class="info-box",style="font-size:12px;",
                         "log₁₀(Y) : même usage que ln. Requiert Y > 0."),
           square  = div(class="info-box",style="font-size:12px;",
                         "Y² : renforce l'asymétrie droite. Rarement recommandé."),
           invsqrt = div(class="info-box",style="font-size:12px;",
                         "1/√Y : forte asymétrie droite, données de durée. Y > 0."),
           arcsin  = div(class="info-box",style="font-size:12px;",
                         "arcsin(√Y) : proportions ∈ [0,1]."),
           NULL
    )
  })
  
  output$ui_ylabel <- renderUI({
    if (!is.null(input$transformation) && input$transformation != "none")
      div(style="font-size:14px;color:#555;margin-top:-4px;",
          paste0("Axe Y affiché : ", ylabel()))
  })
  
  # =========================================================================
  # ONGLET CONCEPTION
  # =========================================================================
  output$plot_conception <- renderPlot({
    p   <- params()
    k   <- k_safe()
    col <- couleurs()
    vi  <- if (is.null(input$type_vi)) "nominal" else input$type_vi
    mus <- vapply(p, function(z) z$moy, numeric(1)); sds <- vapply(p, function(z) z$sd, numeric(1))
    x   <- seq(min(mus - 4 * sds), max(mus + 4 * sds), length.out = 600)
    
    # Construire les labels dans le même ordre que donnees()
    labels_ordonnes <- if (vi == "quanti")
      paste0("Niveau ", seq_len(k))
    else
      paste("Groupe", LETTERS[seq_len(k)])
    
    df_dens <- do.call(rbind, lapply(seq_len(k), function(i) {
      data.frame(
        x       = x,
        densite = dnorm(x, p[[i]]$moy, p[[i]]$sd),
        groupe  = factor(labels_ordonnes[i], levels = labels_ordonnes)
      )
    }))
    
    ggplot(df_dens, aes(x, densite, colour = groupe, fill = groupe,
                        group = groupe)) +
      geom_area(alpha = .15, position = "identity") +
      geom_line(linewidth = 1.2, position = "identity") +
      scale_colour_manual(values = col) +
      scale_fill_manual(values   = col) +
      geom_vline(
        data = data.frame(
          moy    = sapply(seq_len(k), function(i) p[[i]]$moy),
          groupe = factor(labels_ordonnes, levels = labels_ordonnes)
        ),
        aes(xintercept = moy, colour = groupe),
        linetype = "dashed", linewidth = .8, show.legend = FALSE
      ) +
      labs(
        title    = "Distributions théoriques N(μ, σ²) — Y brut",
        subtitle = if (!is.null(input$transformation) &&
                       input$transformation != "none")
          paste0("Transformation appliquée dans les autres onglets : ",
                 ylabel())
        else NULL,
        x = "Y brut", y = "Densité",
        colour = "Groupe", fill = "Groupe"
      ) +
      scale_x_continuous(labels = axe_fr) + scale_y_continuous(labels = axe_fr) +
      theme_anova() +
      theme(legend.position = "bottom")
  })

  variance_conception <- reactive({
    p <- params()
    n <- n_groupes()
    poids <- n / sum(n)
    moyennes <- vapply(p, function(x) x$moy, numeric(1))
    ecarts_types <- vapply(p, function(x) x$sd, numeric(1))
    moyenne_globale <- sum(poids * moyennes)
    v_inter <- sum(poids * (moyennes - moyenne_globale)^2)
    v_intra <- sum(poids * ecarts_types^2)

    list(
      moyenne = moyenne_globale,
      inter = v_inter,
      intra = v_intra,
      totale = v_inter + v_intra
    )
  })

  output$variance_conception <- renderUI({
    v <- variance_conception()
    dc <- decomp()
    ddl_inter <- dc$k - 1L
    ddl_intra <- dc$N - dc$k
    cm_inter <- dc$SCE / ddl_inter
    cm_intra <- dc$SCR / ddl_intra
    f_observe <- cm_inter / cm_intra
    # virgule décimale protégée pour MathJax ({,}), espace fine pour les milliers
    fmt <- function(x) gsub(" ", "\\\\,", gsub(",", "{,}", formatC(x, format = "f", digits = 2, decimal.mark = ",", big.mark = " "), fixed = TRUE))

    withMathJax(
      div(
        class = "info-box",
        style = "margin-top: 10px;",
        h4("Variance totale des distributions théoriques"),
        p("Les tailles de groupes définissent les poids ",
          withMathJax("\\(w_i=n_i/N\\)"), ". Pour ce mélange de distributions normales, la décomposition est exacte :"),
        fluidRow(
          column(
            7,
            div(
              class = "formule",
              style = "font-size: 1.35em;",
              HTML("$$\\sigma^2_{Y} =
              \\underbrace{\\sum_i w_i(\\mu_i-\\mu)^2}_{V_{inter}} +
              \\underbrace{\\sum_i w_i\\sigma_i^2}_{V_{intra}}$$")
            ),
            div(
              class = "formule",
              style = "font-size: 1.35em;",
              HTML(sprintf(
                "$$%s = \\underbrace{%s}_{V_{inter}} + \\underbrace{%s}_{V_{intra}}$$",
                fmt(v$totale), fmt(v$inter), fmt(v$intra)
              ))
            )
          ),
          column(
            5,
            div(
              class = "ok-box",
              style = "margin-top: 6px; padding: 8px 12px;",
              h4(style = "margin-top: 0; font-size: 1.05em;", "Lien avec le test ANOVA"),
              p(style = "font-size: 1em; margin-bottom: 5px;",
                "Dans l'échantillon simulé :"),
              div(
                class = "formule",
                style = "font-size: 1.06em; text-align: center;",
                HTML(sprintf(
                  "$$CM_{inter}=\\frac{SCE}{k-1}=\\frac{%s}{%d}=%s$$",
                  fmt(dc$SCE), ddl_inter, fmt(cm_inter)
                ))
              ),
              div(
                class = "formule",
                style = "font-size: 1.06em; text-align: center;",
                HTML(sprintf(
                  "$$CM_{intra}=\\frac{SCR}{N-k}=\\frac{%s}{%d}=%s$$",
                  fmt(dc$SCR), ddl_intra, fmt(cm_intra)
                ))
              ),
              div(
                class = "formule",
                style = "font-size: 1.16em; text-align: center; font-weight: 700;",
                HTML(sprintf(
                  "$$F(%d{,}%d)=\\frac{CM_{inter}}{CM_{intra}}=\\frac{%s}{%s}=%s$$",
                  ddl_inter, ddl_intra, fmt(cm_inter), fmt(cm_intra), fmt(f_observe)
                ))
              ),
              p(style = "font-size: 0.95em; margin: 5px 0 0;",
                "Sous H₀, les deux carrés moyens estiment la même variance résiduelle : F tend alors vers 1. Des moyennes plus éloignées augmentent surtout CMinter, donc F."),
              if (ylabel() != "Y") {
                p(style = "font-size: 0.92em; margin: 5px 0 0;",
                  paste0("Les carrés moyens et F utilisent la variable actuellement analysée : ", ylabel(), "."))
              }
            )
          )
        ),
        tags$ul(
          tags$li("Éloigner les moyennes des groupes augmente ", withMathJax("\\(V_{inter}\\)"), ", même si les écarts-types ne changent pas."),
          tags$li("Augmenter un ou plusieurs écarts-types augmente ", withMathJax("\\(V_{intra}\\)"), ", même si les moyennes ne changent pas.")
        ),
        p("Part théorique de la variance due aux groupes : ",
          strong(paste0(fr(100 * v$inter / v$totale, 1), " %")),
          ". C'est la valeur vers laquelle tend le η² observé quand les échantillons deviennent très grands."),
        p(style = "margin-bottom: 0; font-size: 1em;",
          strong("Précision : "),
          "il ne s'agit pas d'une addition non pondérée de variances de groupes. Les poids wᵢ sont essentiels; c'est cette pondération qui rend l'égalité exacte.")
      )
    )
  })
  
  # =========================================================================
  # ONGLET DONNÉES SIMULÉES
  # =========================================================================
  output$plot_donnees <- renderPlot({
    d   <- donnees(); col <- couleurs(); tg <- input$types_graph
    ylab <- ylabel()
    stat_grp <- d %>% group_by(groupe_f) %>%
      summarise(m=mean(valeur_t), se=sd(valeur_t)/sqrt(n()),
                lo=m-qt(.975,n()-1)*se, hi=m+qt(.975,n()-1)*se, .groups="drop")
    p <- ggplot(d, aes(x=groupe_f,y=valeur_t,colour=groupe_f,fill=groupe_f))
    if ("violin"  %in% tg) p <- p + geom_violin(alpha=.45,linewidth=.9)
    if ("boxplot" %in% tg) p <- p + geom_boxplot(alpha=.65,linewidth=.9,
                                                 outlier.shape=NA,width=.35)
    if ("jitter"  %in% tg) p <- p + geom_jitter(alpha=.75,width=.12,size=2.1)
    if ("mean_ci" %in% tg) p <- p +
      geom_point(data=stat_grp,aes(x=groupe_f,y=m),size=4,shape=23,
                 colour="white",fill="black") +
      geom_errorbar(data=stat_grp,aes(x=groupe_f,ymin=lo,ymax=hi),
                    colour="black",width=.12,linewidth=.9,inherit.aes=FALSE)
    p + scale_colour_manual(values=col) + scale_fill_manual(values=col) +
      scale_y_continuous(labels = axe_fr) +
      labs(title="Distribution des données par groupe",
           subtitle=if ("mean_ci" %in% tg) "Losange noir : moyenne; barre : intervalle de confiance à 95 % de la moyenne" else NULL,
           x="Groupe",y=ylab) +
      theme_anova() + theme(legend.position="none")
  })
  
  output$tableau_desc <- renderDT({
    d <- donnees(); ylab <- ylabel()
    tab <- d %>% group_by(Groupe=groupe_f) %>%
      summarise(n=n(), Moyenne=mean(valeur_t), `Écart-type`=sd(valeur_t),
                `Erreur type`=sd(valeur_t)/sqrt(n()),
                Min=min(valeur_t), Max=max(valeur_t), .groups="drop")
    tab <- rbind(tab, data.frame(Groupe="Ensemble", n=nrow(d), Moyenne=mean(d$valeur_t),
                                 `Écart-type`=sd(d$valeur_t), `Erreur type`=sd(d$valeur_t)/sqrt(nrow(d)),
                                 Min=min(d$valeur_t),
                                 Max=max(d$valeur_t), check.names=FALSE))
    dt_fr(as.data.frame(tab), digits=2,
          caption = if (ylab != "Y") paste0("Statistiques de ", ylab) else NULL)
  })
  
  output$tableau_brut <- renderDT({
    d <- donnees(); ylab <- ylabel()
    out <- d %>% dplyr::select(Observation = obs_id, Groupe = groupe_f, Y = valeur, Y_t = valeur_t)
    out$Y <- fr(out$Y, 3); out$Y_t <- fr(out$Y_t, 3)
    if (ylab == "Y") out$Y_t <- NULL else names(out)[4] <- ylab
    datatable(out, options=list(pageLength=10,scrollX=TRUE,
                                columnDefs=list(list(className="dt-right", targets="_all"))),
              rownames=FALSE)
  })
  
  # =========================================================================
  # ONGLET POSTULATS
  # =========================================================================
  output$plot_hist <- renderPlot({
    d <- df_residus(); col <- couleurs()
    ggplot(d, aes(x=residus,fill=groupe_f)) +
      geom_histogram(aes(y=after_stat(density)),bins=15,alpha=.7,colour="white") +
      stat_function(fun=dnorm,args=list(mean=0,sd=sd(d$residus)),
                    colour="black",linewidth=.9,linetype="dashed") +
      scale_fill_manual(values=col) + facet_wrap(~groupe_f,scales="free_y") +
      scale_x_continuous(labels = axe_fr) + scale_y_continuous(labels = axe_fr) +
      labs(title="Histogrammes des résidus par groupe",
           subtitle="Pointillé : loi normale de même écart-type que l'ensemble des résidus",
           x="Résidu",y="Densité") +
      theme_anova() + theme(legend.position="none")
  })
  
  output$plot_qq <- renderPlot({
    d <- df_residus(); col <- couleurs()
    ggplot(d, aes(sample=residus,colour=groupe_f)) +
      stat_qq(size=2.1,alpha=.85) + stat_qq_line(linewidth=1.1) +
      scale_colour_manual(values=col) + facet_wrap(~groupe_f) +
      scale_x_continuous(labels = axe_fr) + scale_y_continuous(labels = axe_fr) +
      labs(title="Diagrammes Q-Q par groupe",
           x="Quantiles théoriques",y="Quantiles observés") +
      theme_anova() + theme(legend.position="none")
  })
  
  output$tableau_shapiro <- renderDT({
    d <- df_residus()
    res <- d %>% group_by(Groupe=groupe_f) %>%
      summarise({
        sw <- shapiro_safe(residus)
        tibble(n=n(), W=fr(sw$W, 3), p=fr_p(sw$p),
               Décision=if(!is.null(sw$msg)) sw$msg
               else if(sw$p<.05) "⚠️ Non-normalité détectée"
               else "✔️ Normalité acceptable")
      }, .groups="drop")
    datatable(res,options=list(dom="t",paging=FALSE),rownames=FALSE) %>%
      formatStyle("Décision",backgroundColor=styleEqual(
        c("⚠️ Non-normalité détectée","✔️ Normalité acceptable"),
        c("#fdecea","#eafaf1")))
  })
  
  output$conclusion_normalite <- renderUI({
    d <- df_residus()
    res <- d %>% group_by(groupe_f) %>%
      summarise(sw_p=shapiro_safe(residus)$p, .groups="drop")
    inappl <- res %>% filter(is.na(sw_p))
    signif <- res %>% filter(!is.na(sw_p) & sw_p < .05)
    if (nrow(inappl)>0)
      div(class="warn-box",
          paste0("⚠️ Test inapplicable pour ",nrow(inappl)," groupe(s). ",
                 "Vérifiez la compatibilité de la transformation avec vos données."))
    else if (nrow(signif)>0)
      div(class="warn-box",
          sprintf("⚠️ %d groupe(s) : déviation significative (p < 0,05).",nrow(signif)))
    else
      div(class="ok-box","✔️ Normalité acceptable dans tous les groupes (p ≥ 0,05).")
  })
  
  output$test_levene <- renderUI({
    d <- donnees()
    lm_ <- leveneTest(valeur_t ~ groupe_f, data=d, center=mean)
    lmed <- leveneTest(valeur_t ~ groupe_f, data=d, center=median)
    tagList(
      tab_html(data.frame(
        Version = c("Centrée sur la médiane (Brown-Forsythe, par défaut)", "Centrée sur la moyenne (Levene d'origine)"),
        F = fr(c(lmed$`F value`[1], lm_$`F value`[1]), 3),
        ddl = paste0(lmed$Df[1], " ; ", lmed$Df[2]),
        p = fr_p(c(lmed$`Pr(>F)`[1], lm_$`Pr(>F)`[1])), check.names = FALSE)),
      tags$small("Le test de Levene fait une ANOVA sur les écarts absolus de chaque observation au centre de son groupe.
                  La version centrée sur la médiane, utilisée dans les conclusions de l'application, résiste mieux à l'asymétrie.")
    )
  })
  
  output$test_bartlett <- renderUI({
    b <- bartlett.test(valeur_t ~ groupe_f, data=donnees())
    tagList(
      tab_html(data.frame(`χ²` = fr(b$statistic, 3), ddl = b$parameter, p = fr_p(b$p.value), check.names = FALSE)),
      tags$small("Bartlett compare directement les variances. Il est puissant si les données sont normales,
                  mais il confond facilement non-normalité et variances inégales.")
    )
  })
  
  output$plot_sd <- renderPlot({
    d <- donnees(); col <- couleurs()
    st <- d %>% group_by(groupe_f) %>% summarise(sd=sd(valeur_t), n=n(), .groups="drop")
    ggplot(st,aes(x=groupe_f,y=sd,fill=groupe_f)) +
      geom_col(width=.5,alpha=.8) +
      geom_text(aes(label=paste0("n = ", n)), vjust=-0.5, size=4) +
      geom_hline(yintercept=mean(st$sd),linetype="dashed",colour="black",linewidth=.9) +
      scale_fill_manual(values=col) +
      scale_y_continuous(labels = axe_fr, expand = expansion(mult = c(0, 0.12))) +
      labs(title="Écarts-types par groupe", subtitle="Pointillé : moyenne des écarts-types",
           x="Groupe",y="Écart-type") +
      theme_anova() + theme(legend.position="none")
  })
  
  output$conclusion_homo <- renderUI({
    d  <- donnees()
    lv <- leveneTest(valeur_t~groupe_f,data=d)
    p  <- lv$`Pr(>F)`[1]
    st <- d %>% group_by(groupe_f) %>% summarise(s=sd(valeur_t), n=n(), .groups="drop")
    ratio <- max(st$s)/min(st$s)
    n_inegaux <- max(st$n)/min(st$n) > 1.5
    # le plus petit groupe est-il le plus dispersé ?
    piege <- n_inegaux && which.min(st$n) == which.max(st$s)
    tagList(
      if (p>=.05) div(class="ok-box", paste0("✔️ Levene : p = ", fr_p(p), " ≥ 0,05 : aucune hétérogénéité détectée."))
      else        div(class="warn-box", paste0("⚠️ Levene : ", p_eq(p), " < 0,05 : variances hétérogènes.")),
      div(class = if (ratio > 2 || piege) "warn-box" else "info-box",
          "Rapport du plus grand au plus petit écart-type : ", strong(fr(ratio, 2)),
          " (rapport des variances : ", fr(ratio^2, 2), "). ",
          if (piege) "Le plus petit groupe est aussi le plus dispersé : c'est la situation où le F classique devient trop libéral. Préférez Welch."
          else if (ratio > 2) "Au-delà de 2 environ, surtout avec des tailles inégales, préférez Welch."
          else "Un rapport inférieur à 2 environ est rarement problématique, surtout avec des groupes de même taille.")
    )
  })
  
  output$plot_residus_ordre <- renderPlot({
    d <- df_residus()
    ggplot(d,aes(x=obs_id,y=residus,colour=groupe_f)) +
      geom_point(alpha=.75,size=2) +
      geom_hline(yintercept=0,linetype="dashed",colour="black") +
      geom_smooth(se=FALSE,colour="black",linewidth=.7,method="loess",formula=y~x) +
      scale_colour_manual(values=couleurs()) +
      scale_y_continuous(labels = axe_fr) +
      labs(title="Résidus dans l'ordre des observations",
           x="Numéro d'observation",y="Résidu",colour="Groupe") +
      theme_anova() + theme(legend.position="bottom")
  })
  
  # =========================================================================
  # ONGLET DÉCOMPOSITION
  # =========================================================================
  output$tableau_decomp <- renderTable({
    dc <- decomp()
    data.frame(
      Composante=c("SCE (entre groupes)","SCR (résiduelle)","SCT (totale)"),
      Valeur=fr(c(dc$SCE,dc$SCR,dc$SCT),2),
      Proportion=paste0(fr(c(dc$SCE/dc$SCT,dc$SCR/dc$SCT,1)*100,1)," %"))
  }, striped=TRUE, hover=TRUE, bordered=TRUE, align="lrr")
  
  output$plot_pie <- renderPlot({
    dc <- decomp()
    ggplot(data.frame(C=c("SCE","SCR"),V=c(dc$SCE,dc$SCR)),aes(x="",y=V,fill=C)) +
      geom_col(width=1,colour="white") + coord_polar("y") +
      scale_fill_manual(values=c("#2980b9","#e74c3c")) +
      labs(title="Proportion de variance",fill=NULL) +
      theme_anova_void() + theme(legend.position="bottom")
  })
  
  # Le curseur suit le nombre d'observations (sans effet de bord dans le graphique)
  observeEvent(nrow(donnees()), {
    N <- nrow(donnees())
    updateSliderInput(session, "obs_selectionnee", max = N,
                      value = min(isolate(input$obs_selectionnee), N))
  })

  output$plot_decomp <- renderPlot({
    dc <- decomp(); d <- dc$d; gd <- dc$gd; col <- couleurs(); ylab <- ylabel()
    obs_i <- max(1,min(input$obs_selectionnee,nrow(d)))
    obs   <- d[obs_i,]
    ggplot(d,aes(x=obs_id,y=valeur_t,colour=groupe_f)) +
      geom_point(alpha=.7,size=2.2) +
      geom_hline(yintercept=gd,colour="black",linewidth=1) +
      annotate("text",x=max(d$obs_id),y=gd+diff(range(d$valeur_t))*.02,
               label=paste0("Ȳ.. = ",fr(gd,2)),hjust=1,size=4,colour="black") +
      { lapply(unique(d$groupe_f),function(g) {
        dg <- d[d$groupe_f==g,]; mg <- unique(dg$moy)
        cg <- col[unique(dg$id_groupe)]
        list(geom_segment(data=dg,aes(x=min(obs_id)-.5,xend=max(obs_id)+.5,
                                      y=mg,yend=mg),colour=cg,linewidth=1.1,
                          linetype="longdash",inherit.aes=FALSE),
             annotate("text",x=max(dg$obs_id)+.5,
                      y=mg+diff(range(d$valeur_t))*.02,
                      label=paste0("Ȳ",unique(dg$id_groupe)," = ",fr(mg,2)),
                      hjust=0,size=3.5,colour=cg))
      }) } +
      geom_point(data=obs,aes(x=obs_id,y=valeur_t),size=5,shape=21,
                 fill="yellow",colour="black",stroke=1.5,inherit.aes=FALSE) +
      geom_segment(x=obs$obs_id,xend=obs$obs_id,y=gd,yend=obs$moy,
                   colour="#2980b9",linewidth=1.4,
                   arrow=arrow(length=unit(.2,"cm"),ends="both")) +

      geom_segment(x=obs$obs_id,xend=obs$obs_id,y=obs$moy,yend=obs$valeur_t,
                   colour="#e74c3c",linewidth=1.4,
                   arrow=arrow(length=unit(.2,"cm"),ends="both")) +

      scale_colour_manual(values=col) +
      scale_y_continuous(labels = axe_fr) +
      labs(title=paste0("Décomposition — observation n° ",obs_i," (",obs$groupe_f,
                        ", ",ylab," = ",fr(obs$valeur_t,2),")"),
           subtitle=paste0("Bleu : (Ȳi − Ȳ..)² = ", fr((obs$moy-gd)^2,2),
                           ", contribution de cette observation à la SCE\n",
                           "Rouge : (Yij − Ȳi)² = ", fr((obs$valeur_t-obs$moy)^2,2),
                           ", sa contribution à la SCR"),
           x="Numéro d'observation",y=ylab,colour="Groupe") +
      theme_anova() + theme(legend.position="bottom")
  })
  
  # =========================================================================
  # ONGLET TABLEAU ANOVA
  # =========================================================================
  output$tableau_anova <- renderDT({
    dc <- decomp(); k <- dc$k; N <- dc$N
    ddl_e <- k-1; ddl_r <- N-k
    cme <- dc$SCE/ddl_e; cmr <- dc$SCR/ddl_r
    Fval <- cme/cmr; pval <- pf(Fval,ddl_e,ddl_r,lower.tail=FALSE)
    df_tab <- data.frame(
      Source=c("Entre groupes (SCE)","Résiduelle (SCR)","Total (SCT)"),
      SC=fr(c(dc$SCE,dc$SCR,dc$SCT),2),
      ddl=c(ddl_e,ddl_r,ddl_e+ddl_r),
      CM=c(fr(cme,2),fr(cmr,2),""),
      F=c(fr(Fval,2),"",""),
      p=c(fr_p(pval),"",""), check.names=FALSE)
    datatable(df_tab,options=list(dom="t",paging=FALSE,
                                  columnDefs=list(list(className="dt-right", targets=1:5))),
              rownames=FALSE) %>%
      formatStyle("Source",fontWeight=styleEqual("Total (SCT)","bold"))
  })
  
  output$conclusion_anova <- renderUI({
    dc <- decomp(); k <- dc$k; N <- dc$N
    cme <- dc$SCE/(k-1); cmr <- dc$SCR/(N-k)
    Fval <- cme/cmr; pval <- pf(Fval,k-1,N-k,lower.tail=FALSE)
    alph <- if(is.null(input$alpha_anova)) .05 else input$alpha_anova
    eta2 <- dc$SCE/dc$SCT
    omega2 <- max((dc$SCE-(k-1)*cmr)/(dc$SCT+cmr),0)
    fcrit <- qf(1-alph, k-1, N-k)
    tagList(
      if (pval<alph)
        div(class="ok-box",
            strong(paste0("F(", k-1, ", ", N-k, ") = ", fr(Fval,2), ", ", p_eq(pval), " < α = ", fr_alpha(alph))),
            br(), "On rejette H₀ : au moins une moyenne de population diffère des autres. ",
            "Le test ne dit pas lesquelles : voir l'onglet Comparaisons.")
      else
        div(class="warn-box",
            strong(paste0("F(", k-1, ", ", N-k, ") = ", fr(Fval,2), ", ", p_eq(pval), " ≥ α = ", fr_alpha(alph))),
            br(), "On ne rejette pas H₀ : les données ne permettent pas de conclure à une différence. ",
            "Ce n'est pas une preuve que les moyennes sont égales."),
      p(paste0("Valeur critique au seuil α = ", fr_alpha(alph), " : F = ", fr(fcrit,2),
               ". Taille d'effet : η² = ", fr(eta2,3), ", ω² = ", fr(omega2,3), ".")),
      p(strong("Pour rapporter : "),
        em(paste0("F(", k-1, ", ", N-k, ") = ", fr(Fval,2), ", p ", if (pval < .001) "< 0,001" else paste0("= ", fr(pval,3)),
                  ", ω² = ", fr(omega2,2), ".")))
    )
  })
  
  output$plot_F <- renderPlot({
    dc <- decomp(); k <- dc$k; N <- dc$N
    df1 <- k-1; df2 <- N-k
    cme <- dc$SCE/df1; cmr <- dc$SCR/df2
    Fobs <- cme/cmr
    alph <- if(is.null(input$alpha_anova)) .05 else input$alpha_anova
    Fcrit <- qf(1-alph,df1,df2)
    pval  <- pf(Fobs,df1,df2,lower.tail=FALSE)
    x_max <- max(Fobs*1.3,Fcrit*1.4,5)
    x <- seq(0,x_max,length.out=500)
    df_f <- data.frame(x=x,y=df(x,df1,df2))
    df_f <- df_f[is.finite(df_f$y), ]
    ytop <- max(df_f$y[df_f$x > 0.05])
    ggplot(df_f,aes(x,y)) +
      geom_area(data=subset(df_f,x>=Fcrit),aes(x,y),fill="#e74c3c",alpha=.35) +
      geom_area(data=subset(df_f,x>=Fobs),aes(x,y),fill="#2980b9",alpha=.45) +
      geom_line(linewidth=1) +
      geom_vline(xintercept=Fobs,colour="#2980b9",linewidth=1.3) +
      geom_vline(xintercept=Fcrit,colour="#e74c3c",linewidth=1.1,linetype="dashed") +
      annotate("label",x=Fobs,y=ytop*.75, hjust=if (Fobs > x_max*.6) 1.05 else -0.05,
               label=paste0("F observé = ",fr(Fobs,2)),colour="#2980b9",size=4) +
      annotate("label",x=Fcrit,y=ytop*.45, hjust=-0.05,
               label=paste0("F critique (α = ",fr_alpha(alph),") = ",fr(Fcrit,2)),
               colour="#e74c3c",size=3.8) +
      annotate("text",x=x_max*.98,y=ytop*.98, hjust=1,
               label=paste0("p ",if (pval < .001) "< 0,001" else paste0("= ",fr(pval,3))),size=4.5,colour="black") +
      coord_cartesian(ylim=c(0, ytop*1.05)) +
      scale_x_continuous(labels = axe_fr) + scale_y_continuous(labels = axe_fr) +
      labs(title=paste0("Distribution F(",df1,", ",df2,") sous H₀"),
           subtitle="Rouge : région de rejet  |  bleu : valeur p (aire au-delà du F observé)",x="F",y="Densité") +
      theme_anova()
  })
  
  output$welch_anova <- renderUI({
    d    <- donnees()
    lv   <- leveneTest(valeur_t ~ groupe_f, data = d)
    p_lv <- lv$`Pr(>F)`[1]
    
    res  <- oneway.test(valeur_t ~ groupe_f, data = d, var.equal = FALSE)
    if (!is.finite(res$statistic) || is.na(res$p.value) || is.na(p_lv))
      return(div(class = "warn-box", "Test de Welch impossible : au moins un groupe n'a aucune variabilité."))
    Fw   <- fr(res$statistic, 2)
    df1w <- res$parameter[1]
    df2w <- fr(res$parameter[2], 1)
    pw   <- res$p.value
    alph <- if (is.null(input$alpha_anova)) .05 else input$alpha_anova
    
    # Toujours afficher, mais mettre en valeur si Levene rejette
    classe <- if (p_lv < .05) "warn-box" else "info-box"
    titre  <- if (p_lv < .05)
      "⚠️ Variances hétérogènes détectées — ANOVA de Welch recommandée"
    else
      "ℹ️ ANOVA de Welch (robuste à l'hétéroscédasticité)"
    
    decision <- if (pw < alph)
      paste0("On rejette H₀ (p ", if (pw < .001) "< 0,001" else paste0("= ", fr(pw,3)), " < α = ", fr_alpha(alph), ").")
    else
      paste0("On ne rejette pas H₀ (p = ", fr_p(pw), " ≥ α = ", fr_alpha(alph), ").")
    
    div(class = classe,
        strong(titre), br(),
        paste0("F de Welch(", df1w, "; ", df2w, ") = ", Fw, ". ", decision),
        br(),
        tags$small(
          "Les degrés de liberté sont ajustés selon la correction de Welch-Satterthwaite.
       Cette statistique est valide même quand σ₁² ≠ σ₂² ≠ … ≠ σₖ²."
        )
    )
  })
  
  # =========================================================================
  # ONGLET COMPARAISONS MULTIPLES
  # =========================================================================
  output$ui_levene_resume <- renderUI({
    lv <- leveneTest(valeur_t~groupe_f,data=donnees())
    p  <- lv$`Pr(>F)`[1]
    if (p<.05)
      div(class="warn-box",style="font-size:12px;",
          paste0(p_eq(p), " < 0,05 : variances hétérogènes. Utilisez Games-Howell, Tamhane T2 ou Dunnett T3, en cohérence avec le test de Welch."))
    else
      div(class="ok-box",style="font-size:12px;",
          paste0(p_eq(p), " ≥ 0,05 : aucune hétérogénéité détectée. Les procédures à variance commune sont applicables."))
  })
  
  output$ui_warn_homo <- renderUI({
    meth <- if(is.null(input$methode_homo)) "tukey" else input$methode_homo
    switch(meth,
           tukey      = div(class="info-box",style="font-size:12px;",
                            "Recommandé par défaut. Contrôle le FWER."),
           bonferroni = div(class="info-box",style="font-size:12px;",
                            "Conservateur. Idéal pour comparaisons planifiées."),
           holm       = div(class="info-box",style="font-size:12px;",
                            "Bonferroni séquentiel — toujours plus puissant que Bonferroni."),
           hochberg   = div(class="info-box",style="font-size:12px;",
                            "Variante ascendante de Holm."),
           fdr        = div(class="warn-box",style="font-size:12px;",
                            "Contrôle le FDR, pas le FWER. Exploratoire seulement."),
           scheffe    = div(class="info-box",style="font-size:12px;",
                            "Le plus conservateur. Valide pour tout contraste."),
           duncan     = div(class="warn-box",style="font-size:12px;",
                            "Ne contrôle pas strictement le FWER. Usage agronomique."),
           snk        = div(class="warn-box",style="font-size:12px;",
                            "Puissance intermédiaire entre Tukey et Duncan."),
           none       = div(class="warn-box",style="font-size:12px;",
                            "⚠️ Aucune correction — pédagogique uniquement.")
    )
  })
  
  resultats_cm <- reactive({
    d    <- donnees()
    alph <- if(is.null(input$alpha_cm)) .05 else input$alpha_cm
    fam  <- if(is.null(input$famille_cm)) "homo" else input$famille_cm
    
    if (fam == "homo") {
      meth <- if(is.null(input$methode_homo)) "tukey" else input$methode_homo
      
      if (meth %in% c("duncan","snk")) {
        mod_lm <- lm(valeur_t~groupe_f,data=d)
        res_ag <- if(meth=="duncan")
          agricolae::duncan.test(mod_lm,"groupe_f",alpha=alph,console=FALSE)
        else
          agricolae::SNK.test(mod_lm,"groupe_f",alpha=alph,console=FALSE)
        grps   <- res_ag$groups
        niveaux <- rownames(grps)
        paires <- combn(niveaux,2,simplify=FALSE)
        tab <- do.call(rbind,lapply(paires,function(p) {
          g1 <- grps[p[1],"groups"]; g2 <- grps[p[2],"groups"]
          m1 <- grps[p[1],"valeur_t"]; m2 <- grps[p[2],"valeur_t"]
          partage <- any(strsplit(g1,"")[[1]] %in% strsplit(g2,"")[[1]])
          data.frame(Contraste=paste(p[1],"–",p[2]),
                     Différence=round(m1-m2,4),
                     `p ajusté`=ifelse(partage,"> α","< α"),
                     Décision=ifelse(partage,"✔️ Non significatif","⚠️ Significatif"),
                     stringsAsFactors=FALSE,check.names=FALSE)
        }))
        return(list(type="agricolae",tab=tab,groups=grps,
                    meth_label=toupper(meth),
                    em_ci=as.data.frame(confint(em_obj(),level=1-alph))))
      }
      
      adj <- switch(meth,tukey="tukey",bonferroni="bonferroni",holm="holm",
                    hochberg="hochberg",fdr="fdr",scheffe="scheffe",none="none")
      em    <- em_obj()
      ci_df <- as.data.frame(confint(em,adjust=adj,level=1-alph))
      cont_df <- as.data.frame(contrast(em,method="pairwise",adjust=adj))
      se_col  <- intersect(c("SE","se"),names(cont_df))[1]
      paires  <- cont_df %>%
        mutate(Contraste=contrast, Différence=round(estimate,4),
               SE=if(!is.na(se_col)) round(.data[[se_col]],4) else NA_real_,
               ddl=round(df,0), t=round(t.ratio,4),
               `p ajusté`=round(p.value,4),
               Décision=ifelse(p.value<alph,"⚠️ Significatif","✔️ Non significatif")) %>%
        dplyr::select(Contraste, Différence, SE, ddl, t, `p ajusté`, Décision)
      return(list(type="emmeans",tab=paires,groups=NULL,
                  meth_label=toupper(meth),em_ci=ci_df))
      
    } else {
      meth <- if(is.null(input$methode_hetero)) "games_howell" else input$methode_hetero
      tab_hetero <- tryCatch({
        if (meth=="games_howell") {
          # Games-Howell : erreur type et ddl de Welch pour chaque paire,
          # loi de l'étendue studentisée (même convention de signe que Tukey : 1er − 2e)
          st <- d %>% group_by(groupe_f) %>%
            summarise(m = mean(valeur_t), v = var(valeur_t), n = n(), .groups = "drop")
          k  <- nrow(st); pr <- t(combn(k, 2))
          do.call(rbind, lapply(seq_len(nrow(pr)), function(r) {
            i <- pr[r, 1]; j <- pr[r, 2]
            vn <- st$v / st$n
            se <- sqrt(vn[i] + vn[j])
            df <- (vn[i] + vn[j])^2 / (vn[i]^2 / (st$n[i] - 1) + vn[j]^2 / (st$n[j] - 1))
            dif <- st$m[i] - st$m[j]
            p <- ptukey(abs(dif) / se * sqrt(2), k, df, lower.tail = FALSE)
            data.frame(Contraste = paste(st$groupe_f[i], "-", st$groupe_f[j]),
                       Différence = round(dif, 4), SE = round(se, 4), ddl = round(df, 1),
                       `p ajusté` = p,
                       Décision = ifelse(p < alph, "⚠️ Significatif", "✔️ Non significatif"),
                       stringsAsFactors = FALSE, check.names = FALSE)
          }))
        } else if (meth=="tamhane") {
          paires_depuis_pmcmr(PMCMRplus::tamhaneT2Test(valeur_t~groupe_f,data=d),d,alph)
        } else {
          paires_depuis_pmcmr(PMCMRplus::dunnettT3Test(valeur_t~groupe_f,data=d),d,alph)
        }
      }, error=function(e) data.frame(Erreur=paste("Erreur:",conditionMessage(e)),
                                      stringsAsFactors=FALSE))
      em_ci <- tryCatch(
        as.data.frame(confint(emmeans(modele(),~groupe_f,mode="satterthwaite"),level=1-alph)),
        error=function(e) as.data.frame(confint(em_obj(),level=1-alph)))
      return(list(type="hetero",tab=tab_hetero,groups=NULL,
                  meth_label=switch(meth,games_howell="Games-Howell",
                                    tamhane="Tamhane T2",dunnett3="Dunnett T3"),
                  em_ci=em_ci))
    }
  })
  
  output$plot_cm <- renderPlot({
    res  <- resultats_cm(); col <- couleurs()
    alph <- if(is.null(input$alpha_cm)) .05 else input$alpha_cm
    ggplot(res$em_ci,aes(x=groupe_f,y=emmean,colour=groupe_f)) +
      geom_point(size=4) +
      geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL),width=.25,linewidth=1.2) +
      scale_colour_manual(values=col) +
      scale_y_continuous(labels = axe_fr) +
      labs(title=paste0("Moyennes estimées — IC à ",fr(100*(1-alph), if (alph < .01) 1 else 0)," %"),
           subtitle=paste0("Procédure : ",res$meth_label,
                           if (res$type == "emmeans") " (intervalles ajustés pour l'ensemble des moyennes)" else " (intervalles non ajustés, à titre descriptif)"),
           x="Groupe",y=ylabel()) +
      theme_anova() + theme(legend.position="none")
  })
  
  output$tableau_cm <- renderDT({
    res <- resultats_cm(); tab <- res$tab
    if ("Erreur" %in% names(tab))
      return(datatable(data.frame(Problème=tab$Erreur[1],
                                  Conseil="Essayez Games-Howell."),
                       options=list(dom="t",paging=FALSE),rownames=FALSE))
    p_cols <- if (is.numeric(tab$`p ajusté`)) "p ajusté" else character(0)
    if ("ddl" %in% names(tab)) tab$ddl <- round(tab$ddl, 1)
    dt_fr(tab, digits = 3, p_cols = p_cols) %>%
      formatStyle("Décision",backgroundColor=styleEqual(
        c("⚠️ Significatif","✔️ Non significatif"),c("#fdecea","#eafaf1")))
  })
  
  output$ui_groupes_homogenes <- renderUI({
    res  <- resultats_cm()
    fam  <- if(is.null(input$famille_cm)) "homo" else input$famille_cm
    meth <- if(is.null(input$methode_homo)) "tukey" else input$methode_homo
    if (fam != "homo") return(NULL)
    if (res$type=="agricolae" && !is.null(res$groups)) {
      grps <- res$groups
      df_g <- data.frame(Groupe=rownames(grps),
                         Moyenne=fr(grps[["valeur_t"]],2),
                         Lettres=grps[["groups"]],stringsAsFactors=FALSE)
      tagList(h4(paste0("Groupes homogènes — ",res$meth_label)),
              div(class="info-box","Groupes partageant une lettre → pas de différence significative."),
              renderTable(df_g,striped=TRUE,hover=TRUE,bordered=TRUE))
    } else if (meth=="tukey") {
      tryCatch({
        cld <- multcomp::cld(em_obj(),alpha=input$alpha_cm,Letters=letters,adjust="tukey")
        df_g <- as.data.frame(cld) %>%
          mutate(Groupe=as.character(groupe_f),Moyenne=fr(emmean,2),
                 Lettres=trimws(.group)) %>% dplyr::select(Groupe, Moyenne, Lettres)
        tagList(h4("Groupes homogènes — Tukey HSD"),
                div(class="info-box","Groupes partageant une lettre → pas de différence significative."),
                renderTable(df_g,striped=TRUE,hover=TRUE,bordered=TRUE))
      }, error=function(e) NULL)
    }
  })
  
  # =========================================================================
  # ONGLET TAILLE D'EFFET & PUISSANCE
  # =========================================================================
  output$tableau_effet <- renderTable({
    dc <- decomp(); k <- dc$k; N <- dc$N
    cmr <- dc$SCR/(N-k)
    eta2   <- dc$SCE/dc$SCT
    omega2 <- max((dc$SCE-(k-1)*cmr)/(dc$SCT+cmr),0)
    f_c    <- sqrt(eta2/max(1-eta2,1e-9))
    f_o    <- sqrt(omega2/max(1-omega2,1e-9))
    lab_eta <- function(x) ifelse(x<.01,"Négligeable",ifelse(x<.06,"Petit",ifelse(x<.14,"Moyen","Grand")))
    lab_f   <- function(x) ifelse(x<.10,"Négligeable",ifelse(x<.25,"Petit",ifelse(x<.40,"Moyen","Grand")))
    data.frame(
      Mesure=c("η² (êta carré)","ω² (oméga carré)","f de Cohen (d'après η²)","f de Cohen (d'après ω²)"),
      Valeur=fr(c(eta2,omega2,f_c,f_o),3),
      Interprétation=c(lab_eta(eta2), lab_eta(omega2), lab_f(f_c), lab_f(f_o)))
  }, striped=TRUE,hover=TRUE,bordered=TRUE,align="lrl")
  
  output$tableau_conventions <- renderTable({
    data.frame(Effet=c("Petit","Moyen","Grand"),
               `η² ou ω²`=c("0,01","0,06","0,14"),
               `f`=c("0,10","0,25","0,40"), check.names=FALSE)
  }, bordered=TRUE, align="lrr")
  
  output$conclusion_effet <- renderUI({
    dc <- decomp(); k <- dc$k; N <- dc$N
    cmr    <- dc$SCR/(N-k)
    omega2 <- max((dc$SCE-(k-1)*cmr)/(dc$SCT+cmr),0)
    eta2   <- dc$SCE/dc$SCT
    taille <- ifelse(omega2<.01,"négligeable",ifelse(omega2<.06,"petit",ifelse(omega2<.14,"moyen","grand")))
    div(class="info-box",
        paste0("η² = ", fr(eta2,3), " : dans l'échantillon, le facteur est associé à ", fr(100*eta2,1),
               " % de la variabilité de Y. ω² = ", fr(omega2,3), " : on estime cette part à ", fr(100*omega2,1),
               " % dans la population (effet ", taille, " selon les repères de Cohen). ",
               "η² est toujours un peu optimiste, surtout avec de petits groupes; ω² corrige ce biais. ",
               "Ces repères sont indicatifs : l'importance d'un effet se juge dans le domaine étudié."))
  })
  
  output$plot_puissance <- renderPlot({
    k    <- k_safe()
    alph <- if(is.null(input$alpha_puissance)) .05 else input$alpha_puissance
    f0   <- if(is.null(input$f_cible)) .25 else input$f_cible
    dc   <- decomp()
    cmr  <- dc$SCR/(dc$N-dc$k)
    om2  <- max((dc$SCE-(dc$k-1)*cmr)/(dc$SCT+cmr),0)
    f_obs <- sqrt(om2/max(1-om2,1e-9))
    ns  <- seq(5,200,by=1)
    pw_f <- function(n,f,k,a) {
      pf(qf(1-a,k-1,k*(n-1)),k-1,k*(n-1),ncp=f^2*k*n,lower.tail=FALSE)
    }
    df_pw <- data.frame(
      n=rep(ns,2),
      puissance=c(sapply(ns,pw_f,f=f0,k=k,a=alph),
                  sapply(ns,pw_f,f=f_obs,k=k,a=alph)),
      Scenario=factor(rep(c(paste0("f visé = ",fr(f0,2)),paste0("f estimé sur les données (d'après ω²) = ",fr(f_obs,2))),each=length(ns)),
                      levels=c(paste0("f visé = ",fr(f0,2)),paste0("f estimé sur les données (d'après ω²) = ",fr(f_obs,2)))))
    ggplot(df_pw,aes(x=n,y=puissance,colour=Scenario,linetype=Scenario)) +
      geom_line(linewidth=1.3) +
      geom_hline(yintercept=.80,linetype="dashed",colour="black",linewidth=.8) +
      annotate("text",x=5,y=.82,label="Puissance visée : 80 %",hjust=0,size=4) +
      scale_colour_manual(values=c("#2980b9","#e74c3c")) +
      scale_y_continuous(limits=c(0,1),labels=function(x) paste0(axe_fr(100*x), " %")) +
      labs(title=paste0("Courbes de puissance — k = ",k," groupes, α = ",fr_alpha(alph)),
           x="n par groupe",y="Puissance (1−β)",colour=NULL,linetype=NULL) +
      theme_anova() + theme(legend.position="bottom", legend.direction="vertical")
  })
  
  output$n_recommande <- renderUI({
    k    <- k_safe()
    alph <- if(is.null(input$alpha_puissance)) .05 else input$alpha_puissance
    f0   <- if(is.null(input$f_cible)) .25 else input$f_cible
    pw_f <- function(n,f,k,a)
      pf(qf(1-a,k-1,k*(n-1)),k-1,k*(n-1),ncp=f^2*k*n,lower.tail=FALSE)
    ns   <- seq(5,1000)
    n_min <- ns[which(sapply(ns,pw_f,f=f0,k=k,a=alph)>=.80)[1]]
    if (!is.na(n_min))
      div(class="ok-box",
          paste0("✔️ Pour détecter f = ", fr(f0,2), " avec α = ", fr_alpha(alph), " et k = ", k,
                 " groupes, il faut ", n_min, " observations par groupe (N = ", n_min*k,
                 ") pour une puissance de 80 %. Calcul pour des groupes de même taille."))
    else
      div(class="warn-box","Effet trop petit pour 80% de puissance dans la plage explorée.")
  })
  
  output$kruskal_test <- renderUI({
    d <- donnees()
    res <- kruskal.test(valeur_t ~ groupe_f, data = d)
    rg <- d %>% mutate(r = rank(valeur_t)) %>% group_by(Groupe = groupe_f) %>%
      summarise(n = n(), `Rang moyen` = fr(mean(r), 1), Médiane = fr(median(valeur_t), 2), .groups = "drop")
    tagList(
      tab_html(data.frame(H = fr(res$statistic, 3), ddl = res$parameter, p = fr_p(res$p.value))),
      tab_html(as.data.frame(rg)),
      tags$small("Sous H₀, chaque rang moyen serait proche de (N + 1)/2 = ", fr((nrow(d) + 1)/2, 1), ".")
    )
  })
  
  output$kruskal_conclusion <- renderUI({
    res  <- kruskal.test(valeur_t ~ groupe_f, data = donnees())
    alph <- if (is.null(input$alpha_anova)) .05 else input$alpha_anova
    p    <- res$p.value
    if (p < alph)
      div(class = "ok-box",
          paste0("✔️ Kruskal-Wallis : H(", res$parameter, ") = ", fr(res$statistic, 2), ", ", p_eq(p),
                 " < α = ", fr_alpha(alph), " : au moins un groupe tend à prendre des valeurs plus élevées ou plus faibles que les autres."))
    else
      div(class = "warn-box",
          paste0("⚠️ Kruskal-Wallis : H(", res$parameter, ") = ", fr(res$statistic, 2), ", ", p_eq(p),
                 " ≥ α = ", fr_alpha(alph), " : aucune différence de distribution détectée."))
  })
  
  output$dunn_test <- renderDT({
    alph <- if (is.null(input$alpha_anova)) .05 else input$alpha_anova
    d    <- donnees()
    
    res <- tryCatch({
      sortie <- utils::capture.output(dt <- dunn.test::dunn.test(d$valeur_t,
                                 g       = d$groupe_f,
                                 method  = input$dunn_method,
                                 alpha   = alph,
                                 kw      = FALSE,
                                 label   = TRUE,
                                 altp    = TRUE))  # valeurs p bilatérales, comparées directement à α
      data.frame(
        Contraste  = dt$comparisons,
        Z          = fr(dt$Z, 3),
        `p ajusté` = fr_p(dt$altP.adjusted),
        Décision   = ifelse(dt$altP.adjusted < alph,
                            "⚠️ Significatif", "✔️ Non significatif"),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    }, error = function(e) {
      data.frame(Erreur = conditionMessage(e), stringsAsFactors = FALSE)
    })
    
    if ("Erreur" %in% names(res))
      return(datatable(res, options = list(dom="t", paging=FALSE), rownames=FALSE))
    
    datatable(res, options = list(dom="t", paging=FALSE), rownames=FALSE) %>%
      formatStyle("Décision",
                  backgroundColor = styleEqual(
                    c("⚠️ Significatif","✔️ Non significatif"),
                    c("#fdecea","#eafaf1")))
  })
  
  # ── Groupe de référence (pour codage Simple) ─────────────────────────────────
  output$ui_groupe_ref <- renderUI({
    d  <- donnees()
    niveaux <- levels(d$groupe_f)
    selectInput("groupe_ref", "Groupe de référence",
                choices  = niveaux,
                selected = niveaux[1])
  })
  
  # ── Description pédagogique du codage choisi ─────────────────────────────────
  output$ui_desc_contraste <- renderUI({
    switch(input$contraste_std,
           deviation = div(class="info-box", style="font-size:12px;",
                           strong("Déviation :"), " chaque groupe est comparé à la moyenne
      non pondérée des k moyennes (ψ = μᵢ − moyenne des μ). Le dernier groupe est omis
      (son écart se déduit des autres). Utile sans groupe contrôle naturel. Non orthogonaux."),
           simple = div(class="info-box", style="font-size:12px;",
                        strong("Simple :"), " chaque groupe est comparé au groupe de
      référence choisi (ψ = μᵢ − μ_réf). Classique en présence d'un groupe contrôle.
      Non orthogonaux : ils partagent le groupe de référence."),
           difference = div(class="info-box", style="font-size:12px;",
                            strong("Différence (Helmert inverse) :"), " chaque groupe est
      comparé à la moyenne des groupes précédents. Utile quand les
      groupes représentent des étapes cumulatives."),
           helmert = div(class="info-box", style="font-size:12px;",
                         strong("Helmert :"), " chaque groupe est comparé à la moyenne
      des groupes suivants. Sens opposé à Différence."),
           repeated = div(class="info-box", style="font-size:12px;",
                          strong("Groupes adjacents :"), " chaque groupe est comparé au groupe
      suivant (ψ = μ₁ − μ₂, μ₂ − μ₃, etc.). Utile pour des niveaux ordonnés
      (doses, temps, intensités). Non orthogonaux.")
    )
  })
  
  # ── Matrice de contrastes ─────────────────────────────────────────────────────
  matrice_contrastes <- reactive({
    d      <- donnees()
    k      <- k_safe()
    niv    <- levels(d$groupe_f)
    
    M <- switch(input$contraste_std,
                
                deviation = {
                  # Chaque groupe vs grande moyenne — dernière catégorie = référence
                  M <- matrix(0, nrow = k-1, ncol = k)
                  for (i in seq_len(k-1)) {
                    M[i, i] <- 1 - 1/k        # groupe i
                    M[i, k] <- -1/k           # référence implicite
                    for (j in setdiff(seq_len(k), c(i, k)))
                      M[i, j] <- -1/k
                  }
                  M
                },
                
                simple = {
                  # Chaque groupe moins le groupe de référence : μᵢ − μ_réf
                  ref <- if (is.null(input$groupe_ref) || !(input$groupe_ref %in% niv)) 1L
                  else which(niv == input$groupe_ref)
                  M <- matrix(0, nrow = k-1, ncol = k)
                  non_ref <- setdiff(seq_len(k), ref)
                  for (i in seq_along(non_ref)) {
                    M[i, non_ref[i]] <-  1
                    M[i, ref]        <- -1
                  }
                  M
                },
                
                difference = {
                  # Helmert inverse : groupe i vs moyenne des i-1 précédents
                  M <- matrix(0, nrow = k-1, ncol = k)
                  for (i in 2:k) {
                    M[i-1, i]          <-  (i-1)/i
                    M[i-1, seq_len(i-1)] <- -1/i
                  }
                  M
                },
                
                helmert = {
                  # Groupe i vs moyenne des groupes i+1 ... k
                  M <- matrix(0, nrow = k-1, ncol = k)
                  for (i in seq_len(k-1)) {
                    n_apres <- k - i
                    M[i, i]               <-  n_apres / (n_apres + 1)
                    M[i, (i+1):k]         <- -1       / (n_apres + 1)
                  }
                  M
                },
                
                repeated = {
                  # Groupe i vs groupe i+1
                  M <- matrix(0, nrow = k-1, ncol = k)
                  for (i in seq_len(k-1)) {
                    M[i, i]   <-  1
                    M[i, i+1] <- -1
                  }
                  M
                }
    )
    
    rownames(M) <- paste0("C", seq_len(nrow(M)))
    colnames(M) <- niv
    M
  })
  
  # ── Affichage de la matrice ───────────────────────────────────────────────────
  output$tableau_matrice_c <- renderTable({
    M   <- matrice_contrastes()
    df  <- as.data.frame(lapply(as.data.frame(M), function(v) fr(v, 3)), check.names = FALSE)
    rownames(df) <- rownames(M)
    df$`Σ` <- fr(rowSums(M), 0)
    df$`Compare` <- vapply(seq_len(nrow(M)), function(i) decrire_contraste(M[i, ], colnames(M)), character(1))
    df
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ── Vérification orthogonalité ────────────────────────────────────────────────
  output$ui_ortho_check <- renderUI({
    M <- matrice_contrastes()
    n <- as.vector(table(donnees()$groupe_f))
    tagList(
      div(class="ok-box", style="margin-bottom:6px;",
          "✔️ Somme des coefficients = 0 pour chaque contraste : ψ = 0 quand toutes les moyennes sont égales."),
      message_ortho(M, n)
    )
  })
  
  # ── Calcul des contrastes via emmeans ─────────────────────────────────────────
  resultats_contrastes_std <- reactive({
    adj <- if (isTRUE(input$contraste_ajuste)) "bonferroni" else "none"
    tester_contrastes(matrice_contrastes(), input$alpha_contraste, adj)
  })
  
  # ── Tableau des résultats ─────────────────────────────────────────────────────
  output$tableau_contrastes_std <- renderDT({
    afficher_contrastes(resultats_contrastes_std(), input$alpha_contraste,
                        if (isTRUE(input$contraste_ajuste)) "bonferroni" else "none")
  })
  
  # ── Conclusion globale ────────────────────────────────────────────────────────
  output$conclusion_contrastes_std <- renderUI({
    df   <- resultats_contrastes_std()
    alph <- input$alpha_contraste
    sig  <- df$p_aj < alph
    adj  <- if (isTRUE(input$contraste_ajuste)) " (correction de Bonferroni)" else " (sans correction)"
    div(class = if (any(sig)) "ok-box" else "warn-box",
        paste0(sum(sig), " contraste(s) significatif(s) sur ", nrow(df), " au seuil α = ", fr_alpha(alph), adj, "."),
        if (any(sig)) tags$ul(lapply(which(sig), function(i)
          tags$li(paste0(df$nom[i], " (", df$desc[i], ") : ψ̂ = ", fr(df$psi[i], 2), ", ", p_eq(df$p_aj[i]))))))
  })
  
  # ── Graphique ─────────────────────────────────────────────────────────────────
  output$plot_contrastes_std <- renderPlot({
    d    <- donnees()
    M    <- matrice_contrastes()
    col  <- couleurs()
    alph <- input$alpha_contraste
    df_r <- resultats_contrastes_std()
    
    # Moyennes par groupe
    moy_grp <- d %>%
      group_by(groupe_f) %>%
      summarise(m  = mean(valeur_t),
                se = sd(valeur_t)/sqrt(n()),
                lo = m - qt(.975, n()-1)*se,
                hi = m + qt(.975, n()-1)*se,
                .groups = "drop")
    
    # Graphique principal : moyennes + IC
    p1 <- ggplot(moy_grp, aes(x = groupe_f, y = m, colour = groupe_f)) +
      geom_point(size = 4) +
      geom_errorbar(aes(ymin = lo, ymax = hi), width = .2, linewidth = 1) +
      scale_colour_manual(values = col) +
      scale_y_continuous(labels = axe_fr) +
      labs(x = "Groupe", y = ylabel(), colour = NULL) +
      theme_anova() +
      theme(legend.position = "none")
    
    # Graphique des coefficients par contraste
    df_coef <- as.data.frame(t(M)) %>%
      tibble::rownames_to_column("Groupe") %>%
      tidyr::pivot_longer(-Groupe, names_to = "Contraste", values_to = "Coef") %>%
      mutate(Groupe = factor(Groupe, levels = levels(d$groupe_f)))
    
    # Ajouter significativité si disponible
    if (!"Erreur" %in% names(df_r)) {
      sig_map <- setNames(df_r$p_aj < alph, df_r$nom)
      df_coef <- df_coef %>%
        mutate(sig = sig_map[Contraste],
               Contraste = paste0(Contraste,
                                  ifelse(sig, " *", "")))
    }
    
    p2 <- ggplot(df_coef, aes(x = Groupe, y = Coef, fill = Groupe)) +
      geom_col(alpha = .75, colour = "white") +
      geom_hline(yintercept = 0, linewidth = .6, colour = "black") +
      scale_fill_manual(values = col) +
      facet_wrap(~Contraste, ncol = 2) +
      scale_y_continuous(labels = axe_fr) +
      labs(title = "Coefficients des contrastes (* = significatif)",
           x = NULL, y = "Coefficient") +
      theme_anova() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1))
    
    # Assemblage avec patchwork
    p1 / p2 + patchwork::plot_layout(heights = c(1, 1.6))
  })
  
  # ── Scores des niveaux (saisie dynamique) ────────────────────────────────────
  output$ui_scores_poly <- renderUI({
    k   <- k_safe()
    d   <- donnees()
    niv <- levels(d$groupe_f)
    lapply(seq_len(k), function(i) {
      fluidRow(
        column(6,
               tags$small(niv[i], style = "color:var(--color-text-secondary);")
        ),
        column(6,
               numericInput(paste0("score_poly_", i),
                            label = NULL,
                            value = i,
                            step  = 1,
                            width = "100%")
        )
      )
    })
  })
  
  # ── Scores sous forme de vecteur ─────────────────────────────────────────────
  scores_poly <- reactive({
    k <- k_safe()
    scores <- sapply(seq_len(k), function(i) {
      v <- input[[paste0("score_poly_", i)]]
      if (is.null(v) || is.na(v)) i else v
    })
    scores
  })
  
  # ── Matrice des polynômes orthogonaux ────────────────────────────────────────
  matrice_poly <- reactive({
    k      <- k_safe()
    d      <- donnees()
    niv    <- levels(d$groupe_f)
    scores <- scores_poly()
    
    # contr.poly() calcule les polynômes orthogonaux (scores distincts requis)
    if (anyDuplicated(scores)) scores <- seq_len(k)
    M <- t(contr.poly(k, scores = scores))
    
    # Noms des composantes
    noms <- c("Linéaire", "Quadratique", "Cubique", "Quartique")[seq_len(k-1)]
    rownames(M) <- noms
    colnames(M) <- niv
    M
  })
  
  # ── Affichage de la matrice ───────────────────────────────────────────────────
  output$tableau_matrice_poly <- renderTable({
    M  <- matrice_poly()
    df <- as.data.frame(lapply(as.data.frame(M), function(v) fr(v, 3)), check.names = FALSE)
    rownames(df) <- rownames(M)
    df$`Σ` <- fr(rowSums(M), 0)
    df
  }, rownames = TRUE, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  # ── Vérification orthogonalité ────────────────────────────────────────────────
  output$ui_ortho_poly <- renderUI({
    M <- matrice_poly()
    if (nrow(M) < 1) return(NULL)
    sc <- scores_poly()
    tagList(
      if (length(unique(diff(sort(sc)))) > 1)
        div(class="info-box", style="margin-bottom:6px;",
            "Scores non équidistants (", paste(axe_fr(sc), collapse = "; "),
            ") : les coefficients respectent les distances réelles entre niveaux; ils ne sont plus les entiers des tables."),
      if (anyDuplicated(sc))
        div(class="warn-box", "⚠️ Deux niveaux ont le même score : la tendance n'est pas définie.")
      else message_ortho(M, as.vector(table(donnees()$groupe_f)))
    )
  })
  
  # ── Calcul des tendances via emmeans ──────────────────────────────────────────
  resultats_tendance <- reactive({
    tester_contrastes(matrice_poly(), input$alpha_poly, "none")
  })
  
  # ── Tableau des résultats ─────────────────────────────────────────────────────
  output$tableau_tendance <- renderDT({
    afficher_contrastes(resultats_tendance(), input$alpha_poly, "none",
                        premiere = "Composante", avec_desc = FALSE)
  })
  
  # ── Conclusion ────────────────────────────────────────────────────────────────
  output$conclusion_tendance <- renderUI({
    df   <- resultats_tendance()
    alph <- input$alpha_poly
    sig  <- df$nom[df$p < alph]
    lin  <- df[df$nom == "Linéaire", ]
    tagList(
      if (input$type_vi == "nominal")
        div(class="warn-box", "⚠️ La VI est déclarée nominale : une tendance n'a de sens que si les niveaux sont ordonnés."),
      if (length(sig) == 0)
        div(class="warn-box",
            paste0("Aucune composante polynomiale significative à α = ", fr_alpha(alph), "."))
      else
        div(class="ok-box",
            paste0("Composante(s) significative(s) à α = ", fr_alpha(alph), " : ", paste(sig, collapse = ", "), ". "),
            if (nrow(lin) && lin$p < alph)
              paste0("La pente est ", if (lin$psi > 0) "positive : la moyenne tend à augmenter avec le niveau. "
                     else "négative : la moyenne tend à diminuer avec le niveau. "),
            if (isTRUE(attr(df, "complet")))
              paste0("La plus grande part de la SCE revient à la composante ", df$nom[which.max(df$sc)],
                     " (", fr(100 * max(df$pct), 1), " %)."))
    )
  })
  
  # ── Graphique de tendance ─────────────────────────────────────────────────────
  output$plot_tendance <- renderPlot({
    d      <- donnees()
    col    <- couleurs()
    alph   <- input$alpha_poly
    scores <- scores_poly()
    df_r   <- resultats_tendance()
    ylab   <- ylabel()
    
    # Moyennes observées par groupe
    moy_grp <- d %>%
      group_by(groupe_f, id_groupe) %>%
      summarise(m  = mean(valeur_t),
                se = sd(valeur_t)/sqrt(n()),
                lo = m - qt(.975, n()-1)*se,
                hi = m + qt(.975, n()-1)*se,
                .groups = "drop") %>%
      mutate(score = scores[id_groupe])
    
    # Déterminer les composantes significatives
    sig_comp  <- df_r$nom[df_r$p < alph]
    # degré du plus haut terme significatif
    degre_max <- max(0, match(sig_comp, c("Linéaire","Quadratique","Cubique","Quartique")))
    
    # Courbe polynomiale ajustée (sur les scores)
    x_seq <- seq(min(scores), max(scores), length.out = 200)
    
    p <- ggplot(moy_grp, aes(x = score, y = m)) +
      geom_errorbar(aes(ymin = lo, ymax = hi, colour = groupe_f),
                    width = diff(range(scores))*0.03, linewidth = .8) +
      geom_point(aes(colour = groupe_f), size = 4) +
      scale_colour_manual(values = col, name = "Groupe")
    
    # Ajouter courbes selon composantes significatives
    # Courbe : polynôme de ce degré ajusté aux moyennes (pondérées par n)
    if (degre_max >= 1 && !anyDuplicated(scores)) {
      ajust <- lm(m ~ poly(score, degre_max, raw = TRUE), data = moy_grp,
                  weights = as.vector(table(d$groupe_f)))
      courbe <- data.frame(score = x_seq)
      courbe$m <- predict(ajust, courbe)
      p <- p + geom_line(data = courbe, aes(score, m), colour = "black", linewidth = 1.1)
    }
    
    # Sous-titre dynamique
    sous_titre <- if (length(sig_comp) == 0)
      "Aucune tendance polynomiale significative"
    else
      paste0("Composante(s) significative(s) : ", paste(sig_comp, collapse = " + "),
             "\nCourbe : polynôme de degré ", degre_max, " ajusté aux moyennes")
    
    p +
      scale_x_continuous(
        breaks = scores,
        labels = paste0(levels(d$groupe_f), "\n(", axe_fr(scores), ")")
      ) +
      scale_y_continuous(labels = axe_fr) +
      labs(title    = "Analyse de tendance polynomiale",
           subtitle = sous_titre,
           x        = "Groupes (scores)",
           y        = ylab) +
      theme_anova() +
      theme(legend.position = "bottom")
  })
  
  # ── Nombre de contrastes actifs ───────────────────────────────────────────────
  nb_contrastes <- reactiveVal(1L)
  
  observeEvent(input$ajouter_contraste, {
    k <- k_safe()
    nc <- nb_contrastes()
    if (nc < k - 1L) nb_contrastes(nc + 1L)
  })
  
  observeEvent(input$retirer_contraste, {
    nc <- nb_contrastes()
    if (nc > 1L) nb_contrastes(nc - 1L)
  })
  
  observeEvent(input$reset_contrastes, {
    nb_contrastes(1L)
  })
  
  # Réinitialiser quand k change
  observeEvent(k_safe(), {
    nb_contrastes(1L)
  }, ignoreInit = TRUE)
  
  # ── Info sur le nombre de contrastes ─────────────────────────────────────────
  output$ui_nb_contrastes_info <- renderUI({
    k  <- k_safe()
    nc <- nb_contrastes()
    max_c <- k - 1L
    
    classe <- if (nc >= max_c) "warn-box" else "info-box"
    div(class = classe, style = "font-size:12px;",
        sprintf("%d contraste(s) actif(s) sur %d maximum (k−1).", nc, max_c),
        if (nc >= max_c) br(),
        if (nc >= max_c) "Maximum atteint — système complet de contrastes."
    )
  })
  
  # ── Grille dynamique de saisie ────────────────────────────────────────────────
  output$ui_grille_contrastes <- renderUI({
    k   <- k_safe()
    nc  <- nb_contrastes()
    d   <- donnees()
    niv <- levels(d$groupe_f)
    col <- couleurs()
    
    # En-tête des groupes
    header <- fluidRow(
      column(2, tags$b("Contraste")),
      lapply(seq_len(k), function(j) {
        column(
          max(1, floor(8/k)),
          div(
            style = paste0(
              "background:", col[j], ";color:white;",
              "padding:3px 6px;border-radius:4px;",
              "font-weight:bold;font-size:12px;text-align:center;"
            ),
            niv[j]
          )
        )
      }),
      column(2, tags$b("Σ", style="color:var(--color-text-secondary);"))
    )
    
    # Lignes de saisie
    lignes <- lapply(seq_len(nc), function(i) {
      fluidRow(
        column(2,
               div(style="margin-top:6px;font-weight:bold;",
                   paste0("C", i))
        ),
        lapply(seq_len(k), function(j) {
          id <- paste0("cp_", i, "_", j)
          column(
            max(1, floor(8/k)),
            numericInput(id,
                         label = NULL,
                         value = isolate({
                           v <- input[[id]]
                           if (is.null(v) || is.na(v)) 0 else v
                         }),
                         step  = 1,
                         width = "100%")
          )
        }),
        column(2,
               uiOutput(paste0("ui_somme_", i))
        )
      )
    })
    
    tagList(header, lignes)
  })
  
  # ── Sommes en temps réel par contraste ───────────────────────────────────────
  observe({
    nc <- nb_contrastes()
    k  <- k_safe()
    lapply(seq_len(nc), function(i) {
      local({
        ii <- i
        output[[paste0("ui_somme_", ii)]] <- renderUI({
          coeffs <- sapply(seq_len(k), function(j) {
            v <- input[[paste0("cp_", ii, "_", j)]]
            if (is.null(v) || is.na(v)) 0 else v
          })
          s <- sum(coeffs)
          style_div <- if (abs(s) < 1e-10)
            "color:#27ae60;font-weight:bold;margin-top:8px;"
          else
            "color:#e74c3c;font-weight:bold;margin-top:8px;"
          div(style = style_div, round(s, 4))
        })
      })
    })
  })
  
  # ── Matrice des contrastes planifiés ─────────────────────────────────────────
  matrice_planifie <- reactive({
    nc  <- nb_contrastes()
    k   <- k_safe()
    d   <- donnees()
    niv <- levels(d$groupe_f)
    
    M <- matrix(0, nrow = nc, ncol = k)
    for (i in seq_len(nc)) {
      for (j in seq_len(k)) {
        v <- input[[paste0("cp_", i, "_", j)]]
        M[i, j] <- if (is.null(v) || is.na(v)) 0 else v
      }
    }
    rownames(M) <- paste0("C", seq_len(nc))
    colnames(M) <- niv
    M
  })
  
  # ── Validation ───────────────────────────────────────────────────────────────
  output$ui_validation_planifie <- renderUI({
    M  <- matrice_planifie()
    nc <- nrow(M)
    k  <- ncol(M)
    
    # 1. Somme = 0
    sommes   <- rowSums(M)
    somme_ok <- abs(sommes) < 1e-10
    noms     <- rownames(M)
    
    
    # 3. Contrastes nuls (tous 0)
    nuls <- apply(M, 1, function(r) all(r == 0))
    
    tagList(
      # Sommes
      if (all(somme_ok))
        div(class="ok-box", style="margin-bottom:6px;",
            "✔️ Somme = 0 pour tous les contrastes.")
      else
        div(class="warn-box", style="margin-bottom:6px;",
            paste0("⚠️ Somme ≠ 0 pour : ",
                   paste(noms[!somme_ok], collapse=", "),
                   ". Ce n'est pas un contraste : ψ ne serait pas nul quand toutes les moyennes sont égales.")),
      
      # Ce que compare chaque contraste
      if (all(somme_ok) && !any(nuls))
        div(class="info-box", style="margin-bottom:6px;",
            tags$b("Ce que compare chaque contraste :"),
            tags$ul(style="margin-bottom:0;", lapply(seq_len(nc), function(i)
              tags$li(paste0(noms[i], " : ", decrire_contraste(M[i, ], colnames(M))))))),
      
      # Orthogonalité
      if (nc >= 2 && all(somme_ok)) message_ortho(M, as.vector(table(donnees()$groupe_f))),
      
      # Contrastes nuls
      if (any(nuls))
        div(class="warn-box",
            paste0("⚠️ Contraste(s) nul(s) (tous coefficients = 0) : ",
                   paste(noms[nuls], collapse=", "),
                   ". Saisissez des coefficients."))
    )
  })
  
  # ── Calcul des résultats ──────────────────────────────────────────────────────
  resultats_planifie <- reactive({
    M    <- matrice_planifie()
    adj  <- if (is.null(input$ajust_planifie)) "none" else input$ajust_planifie
    if (any(abs(rowSums(M)) > 1e-8))
      return(data.frame(Avertissement = "La somme des coefficients n'est pas 0 pour tous les contrastes. Corrigez la grille.",
                        stringsAsFactors = FALSE))
    if (any(apply(M, 1, function(r) all(r == 0))))
      return(data.frame(Avertissement = "Un ou plusieurs contrastes ont tous leurs coefficients à 0.",
                        stringsAsFactors = FALSE))
    tester_contrastes(M, input$alpha_planifie, adj)
  })
  
  # ── Tableau des résultats ─────────────────────────────────────────────────────
  output$tableau_planifie <- renderDT({
    df <- resultats_planifie()
    if ("Avertissement" %in% names(df))
      return(datatable(df, options=list(dom="t",paging=FALSE), rownames=FALSE))
    afficher_contrastes(df, input$alpha_planifie, input$ajust_planifie)
  })
  
  # ── Conclusion ────────────────────────────────────────────────────────────────
  output$conclusion_planifie <- renderUI({
    df   <- resultats_planifie()
    if ("Avertissement" %in% names(df)) return(NULL)
    alph <- input$alpha_planifie
    adj  <- input$ajust_planifie
    lbl_adj <- switch(adj, none="sans correction",
                      bonferroni="correction de Bonferroni", holm="correction de Holm")
    tagList(
      lapply(seq_len(nrow(df)), function(i) {
        sig <- df$p_aj[i] < alph
        sens <- if (df$psi[i] > 0) "plus élevée" else "plus faible"
        pos <- strsplit(df$desc[i], " vs ")[[1]][1]
        div(class = if (sig) "ok-box" else "warn-box", style = "margin-bottom:6px;",
            strong(paste0(df$nom[i], " — ", df$desc[i], " : ")),
            paste0("ψ̂ = ", fr(df$psi[i], 2), ", t(", df$ddl[i], ") = ", fr(df$t[i], 2),
                   ", ", p_eq(df$p_aj[i]), " (", lbl_adj, "). "),
            if (sig) paste0("La moyenne de « ", pos, " » est significativement ", sens, ".")
            else "Les données ne permettent pas de conclure à une différence pour cette comparaison.")
      }),
      tags$small("ψ̂ = Σ cᵢȲᵢ est exprimé dans l'échelle des coefficients : avec (−1, −1, 2), ψ̂ vaut deux fois l'écart entre la moyenne du groupe 3 et celle des groupes 1 et 2. Avec (−½, −½, 1), il vaut cet écart lui-même; t et p ne changent pas.")
    )
  })
  
  # ── Graphique ─────────────────────────────────────────────────────────────────
  output$plot_planifie <- renderPlot({
    d    <- donnees()
    M    <- matrice_planifie()
    col  <- couleurs()
    alph <- input$alpha_planifie
    df_r <- resultats_planifie()
    ylab <- ylabel()
    
    # Moyennes + IC
    moy_grp <- d %>%
      group_by(groupe_f) %>%
      summarise(m  = mean(valeur_t),
                se = sd(valeur_t)/sqrt(n()),
                lo = m - qt(.975, n()-1)*se,
                hi = m + qt(.975, n()-1)*se,
                .groups = "drop")
    
    p1 <- ggplot(moy_grp, aes(x=groupe_f, y=m, colour=groupe_f)) +
      geom_point(size=4) +
      geom_errorbar(aes(ymin=lo, ymax=hi), width=.2, linewidth=1) +
      scale_colour_manual(values=col) +
      scale_y_continuous(labels = axe_fr) +
      labs(x="Groupe", y=ylab) +
      theme_anova() +
      theme(legend.position="none")
    
    # Barres des coefficients par contraste
    niv <- levels(d$groupe_f)
    df_coef <- do.call(rbind, lapply(seq_len(nrow(M)), function(i) {
      sig <- if (!"Avertissement" %in% names(df_r) && i <= nrow(df_r))
        df_r$p_aj[i] < alph
      else FALSE
      data.frame(
        Contraste = paste0("C", i, if(sig) " *" else ""),
        Groupe    = factor(niv, levels=niv),
        Coef      = M[i,],
        stringsAsFactors = FALSE
      )
    }))
    
    p2 <- ggplot(df_coef, aes(x=Groupe, y=Coef, fill=Groupe)) +
      geom_col(alpha=.75, colour="white") +
      geom_hline(yintercept=0, linewidth=.6, colour="black") +
      scale_fill_manual(values=col) +
      facet_wrap(~Contraste, ncol=2) +
      scale_y_continuous(labels = axe_fr) +
      labs(title="Coefficients (* = significatif)",
           x=NULL, y="Coefficient") +
      theme_anova() +
      theme(legend.position="none",
            axis.text.x=element_text(angle=30, hjust=1))
    
    p1 / p2 + patchwork::plot_layout(heights=c(1, 1.6))
  })
  
  
} # fin server

shinyApp(ui, server)

