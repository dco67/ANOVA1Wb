#!/usr/bin/env python3
"""
Captures d'écran reproductibles de l'application ANOVA (version conceptuelle).

Le script pilote l'application comme le ferait un étudiant (réglages du
panneau latéral, onglets, menus), puis enregistre les captures dans images/.
Chaque capture part d'une page fraîchement chargée : au démarrage,
l'application affiche le cas fil rouge du manuel (3 groupes de 30,
μ = 50, 54, 60, σ = 10, graine 2026).

Installation (une seule fois) :
    pip install playwright pillow
    python -m playwright install chromium

Utilisation, depuis la racine du manuel :
    python captures/captures.py --url http://127.0.0.1:8765
    python captures/captures.py --url ... --fichier cap-anova.png
    python captures/captures.py --url ... --mathjax /usr/share/javascript/mathjax
"""
import argparse, re, time
from pathlib import Path
from playwright.sync_api import sync_playwright

LARGEUR, HAUTEUR, ECHELLE = 1440, 1000, 2

def attendre(page, pause=0.8):
    page.wait_for_function("() => window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() "
                           "&& !document.documentElement.classList.contains('shiny-busy')", timeout=120000)
    time.sleep(pause)
    page.wait_for_function("() => !document.documentElement.classList.contains('shiny-busy')", timeout=120000)

def onglet(page, titre):
    page.locator(f"xpath=//a[@data-toggle='tab' and normalize-space()=\"{titre}\"]").first.click()
    attendre(page, 1.2)

def saisir(page, input_id, valeur):
    page.fill(f"#{input_id}", str(valeur)); page.locator(f"#{input_id}").dispatch_event("change"); attendre(page, 0.6)

def choisir(page, input_id, valeur):
    page.evaluate("([id, v]) => { const el=$('#'+id); if (el[0].selectize) el[0].selectize.setValue(v); "
                  "else el.val(v).trigger('change'); }", [input_id, str(valeur)]); attendre(page, 0.8)

def radio(page, nom, valeur):
    page.locator(f"input[name='{nom}'][value='{valeur}']").check(force=True); attendre(page, 0.8)

def curseur(page, input_id, valeur):
    page.evaluate("([id, v]) => $('#'+id).data('ionRangeSlider').update({from: v})", [input_id, valeur])
    attendre(page, 0.6)

def regler(page, k=None, n_perso=None, mus=None, sds=None, vi=None, graine=None):
    """Réglages du panneau latéral (dans cet ordre : k recrée les curseurs)."""
    if vi is not None: radio(page, "type_vi", vi)
    if k is not None: saisir(page, "nb_groupes", k); attendre(page, 1.5)
    if mus is not None:
        for i, v in enumerate(mus, 1): curseur(page, f"moy_{i}", v)
    if sds is not None:
        for i, v in enumerate(sds, 1): curseur(page, f"sd_{i}", v)
    if n_perso is not None: saisir(page, "n_perso", n_perso)
    if graine is not None: saisir(page, "graine", graine)
    attendre(page, 1.5)

# Scénarios du manuel
def heterogene(page):
    regler(page, n_perso="20; 30; 45", sds=[18, 10, 6])

def ordonne(page):
    regler(page, vi="quanti", k=4, mus=[45, 50, 55, 60], sds=[10, 10, 10, 10])

def elements(*ids, marge=10):
    def f(page, fichier):
        page.evaluate("() => document.querySelectorAll('.shiny-notification').forEach(n => n.remove())")
        page.evaluate("""ids => { const e = ids.map(i => document.getElementById(i)).find(e => e && e.offsetParent !== null);
                                  if (e) e.scrollIntoView({block: 'start'}); }""", list(ids)); time.sleep(0.5)
        b = page.evaluate("""ids => { const r = ids.map(i => document.getElementById(i))
                    .filter(e => e && e.offsetParent !== null).map(e => e.getBoundingClientRect());
            return [Math.min(...r.map(x => x.left)) + scrollX, Math.min(...r.map(x => x.top)) + scrollY,
                    Math.max(...r.map(x => x.right)) + scrollX, Math.max(...r.map(x => x.bottom)) + scrollY]; }""",
                          list(ids))
        page.screenshot(path=str(fichier), full_page=True, clip={
            "x": max(b[0] - marge, 0), "y": max(b[1] - marge, 0),
            "width": b[2] - b[0] + 2 * marge, "height": b[3] - b[1] + 2 * marge})
        print("  ✓", fichier.name)
    return f

def fenetre(page, fichier):
    page.evaluate("() => window.scrollTo(0, 0)"); time.sleep(0.3)
    page.screenshot(path=str(fichier)); print("  ✓", fichier.name)

def aller(*chemin):
    def f(page):
        for t in chemin: onglet(page, t)
    return f

def puis(*etapes):
    def f(page):
        for e in etapes: e(page)
    return f

def contraste_planifie(coefs):
    def f(page):
        for j, v in enumerate(coefs, 1): saisir(page, f"cp_1_{j}", v)
    return f

ZONE_ANOVA = ("tableau_anova", "conclusion_anova", "welch_anova", "plot_F")
ZONE_HOMOG = ("plot_sd", "conclusion_homo", "test_levene", "test_bartlett")
ZONE_CM = ("famille_cm", "ui_levene_resume", "plot_cm", "tableau_cm", "ui_groupes_homogenes")

CAPTURES = [
    ("cap-interface.png", lambda p: None, fenetre),
    ("cap-conception.png", aller("Données", "Conception"), elements("plot_conception", "variance_conception")),
    ("cap-donnees.png", aller("Données", "Données simulées"), elements("types_graph", "plot_donnees", "tableau_desc")),
    ("cap-decomposition.png", puis(aller("Analyse", "Décomposition"), lambda p: curseur(p, "obs_selectionnee", 75)),
     elements("tableau_decomp", "plot_pie", "obs_selectionnee", "plot_decomp")),
    ("cap-normalite.png", aller("Données", "Postulats", "Normalité"),
     elements("plot_hist", "plot_qq", "tableau_shapiro", "conclusion_normalite")),
    ("cap-homogeneite.png", aller("Données", "Postulats", "Homogénéité des variances"), elements(*ZONE_HOMOG)),
    ("cap-homog-hetero.png", puis(heterogene, aller("Données", "Postulats", "Homogénéité des variances")),
     elements(*ZONE_HOMOG)),
    ("cap-kw.png", aller("Données", "Postulats", "Non paramétrique"),
     elements("kruskal_test", "kruskal_conclusion", "dunn_method", "dunn_test")),
    ("cap-anova.png", aller("Analyse", "Tableau ANOVA"), elements(*ZONE_ANOVA)),
    ("cap-anova-hetero.png", puis(heterogene, aller("Analyse", "Tableau ANOVA")), elements(*ZONE_ANOVA)),
    ("cap-tukey.png", aller("Analyse", "Comparaisons", "A posteriori"), elements(*ZONE_CM)),
    ("cap-tukey-hetero.png", puis(heterogene, aller("Analyse", "Comparaisons", "A posteriori")),
     elements("famille_cm", "ui_levene_resume", "plot_cm", "tableau_cm")),
    ("cap-gameshowell.png", puis(heterogene, aller("Analyse", "Comparaisons", "A posteriori"),
                                 lambda p: radio(p, "famille_cm", "hetero")),
     elements("famille_cm", "ui_levene_resume", "plot_cm", "tableau_cm")),
    ("cap-simple.png", puis(aller("Analyse", "Comparaisons", "Contrastes standards"),
                            lambda p: choisir(p, "contraste_std", "simple")),
     elements("contraste_std", "tableau_matrice_c", "ui_ortho_check", "tableau_contrastes_std", "conclusion_contrastes_std")),
    ("cap-planifie.png", puis(aller("Analyse", "Comparaisons", "Contrastes planifiés"), contraste_planifie([-1, -1, 2])),
     elements("ui_grille_contrastes", "ui_validation_planifie", "tableau_planifie", "conclusion_planifie", "plot_planifie")),
    ("cap-tendance.png", puis(ordonne, aller("Analyse", "Comparaisons", "Tendance polynomiale")),
     elements("tableau_matrice_poly", "ui_ortho_poly", "tableau_tendance", "conclusion_tendance", "plot_tendance")),
    ("cap-effet.png", aller("Analyse", "Taille d'effet & Puissance"),
     elements("tableau_effet", "tableau_conventions", "conclusion_effet", "plot_puissance", "n_recommande")),
]

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--url", required=True)
    ap.add_argument("--images", default="images")
    ap.add_argument("--fichier")
    ap.add_argument("--mathjax", help="copie locale de MathJax 2 (hors ligne)")
    a = ap.parse_args()
    out = Path(a.images); out.mkdir(parents=True, exist_ok=True)
    with sync_playwright() as p:
        chrome = "/opt/pw-browsers/chromium-1194/chrome-linux/chrome"
        nav = p.chromium.launch(executable_path=chrome) if Path(chrome).exists() else p.chromium.launch()
        page = nav.new_page(viewport={"width": LARGEUR, "height": HAUTEUR}, device_scale_factor=ECHELLE, locale="fr-CA")
        if a.mathjax:
            racine = Path(a.mathjax)
            page.route(re.compile(r"https://(mathjax\.rstudio\.com/latest|cdn\.jsdelivr\.net/npm/mathjax@2[^/]*)/.*"),
                       lambda r: r.fulfill(path=str(racine / re.split(r"/latest/|mathjax@2[^/]*/", r.request.url)[1].split("?")[0])))
        # Logo R : servi depuis une copie locale (l'environnement de capture est hors ligne)
        logo = Path("/usr/share/R/doc/html/Rlogo.svg")
        if logo.exists():
            page.route("https://www.r-project.org/logo/Rlogo.png",
                       lambda r: r.fulfill(path=str(logo), content_type="image/svg+xml"))
        for nom, reglage, zone in CAPTURES:
            if a.fichier and nom != a.fichier: continue
            page.goto(a.url); attendre(page, 2.5)
            reglage(page); attendre(page, 1.0)
            zone(page, out / nom)
        nav.close()

if __name__ == "__main__":
    main()
