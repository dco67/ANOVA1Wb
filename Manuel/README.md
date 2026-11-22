# Manuel ANOVA

Ce dossier est un projet Quarto autonome accompagnant l'application interactive **Analyse de la variance (ANOVA)**.

Pour produire les formats disponibles, ouvrez le dossier comme projet Quarto dans RStudio, puis choisissez **Render Book**, ou exécutez :

```r
quarto::quarto_render()
```

La couverture est déjà configurée : `images/cover.png` est utilisée dans le HTML et l'EPUB; `cover.tex` crée la couverture pleine page du PDF. Gardez ces chemins et la section `include-before-body: cover.tex` du fichier `_quarto.yml` ensemble.

Le manuel contient la préface, dix chapitres d'apprentissage, des laboratoires guidés, des exercices avec corrigés repliables en HTML, un glossaire et des références.
