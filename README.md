# ANOVA1Wb — L'analyse de variance à un facteur

**Comprendre l'ANOVA à un facteur en simulant les données : moyennes, dispersion, effectifs, et ce qu'ils font aux carrés moyens.**

ANOVA1Wb est une application pédagogique **R/Shiny**, en français. On choisit le nombre de groupes, leurs moyennes, leurs écarts-types et leurs effectifs. L'application simule les données et parcourt toute l'analyse : description, vérification des postulats, décomposition de la variance, tableau de l'ANOVA, comparaisons multiples, contrastes, taille d'effet et puissance.

- [Utiliser l'application en ligne](https://shiny.dcstats.org/ANOVA1Wb/)
- [Consulter le manuel pédagogique](https://dcstats.org/ANOVA1Wb/)

## Objectifs pédagogiques

- comprendre la décomposition de la variance totale en variance intergroupe et intragroupe;
- voir pourquoi, sous H₀, le rapport F tend vers 1;
- vérifier les postulats (normalité, homogénéité des variances, indépendance) et choisir une solution quand ils ne tiennent pas (Welch, Kruskal-Wallis);
- distinguer comparaisons a posteriori, contrastes planifiés et tendances polynomiales;
- relier taille d'effet, effectifs et puissance.

## Fonctionnalités

| Onglet | Contenu |
| --- | --- |
| Introduction | Hypothèses, décomposition de la variance, tableau de l'ANOVA, postulats, taille d'effet, guide d'utilisation. |
| Données | Conception du plan (k groupes, n par groupe ou tailles inégales, moyennes et écarts-types), données simulées, statistiques descriptives et graphiques; transformation de Y. |
| Postulats | Normalité (histogrammes, Q-Q, Shapiro-Wilk), homogénéité (Levene, Bartlett), indépendance des résidus, solution non paramétrique (Kruskal-Wallis et Dunn). |
| Analyse | Décomposition d'une observation, formules, tableau de l'ANOVA, distribution F et région critique, proportion de variance expliquée. |
| Comparaisons | Comparaisons a posteriori (avec procédures adaptées aux variances inégales), contrastes standards, tendance polynomiale, contrastes planifiés avec vérification de l'orthogonalité. |
| Taille d'effet et puissance | η², ω², f de Cohen, puissance selon les effectifs. |

## Exécuter l'application localement

```r
install.packages(c("shiny", "shinythemes", "ggplot2", "patchwork", "dplyr", "tidyr", "DT",
                   "car", "rstatix", "emmeans", "effectsize", "agricolae", "PMCMRplus", "broom"))
shiny::runApp("ANOVA1Wb")
```

## Licence

GPL-3.0. Voir le fichier `LICENSE`.

## Auteur

Daniel Coulombe, Ph.D. — <https://dcstats.org>
