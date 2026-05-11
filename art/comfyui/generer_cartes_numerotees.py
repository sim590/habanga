#!/usr/bin/env python3
"""Génère les 72 cartes numérotées de Habanga (4 couleurs × 18 numéros).

Étape 1 : Génère un fond texturé par couleur via ComfyUI.
Étape 2 : Superpose les numéros 1–18 avec Pillow sur chaque fond.

Les fonds sont sauvegardés dans art/cartes/fonds/ pour éviter de les
régénérer si le script est relancé.

Usage:
    python generer_cartes_numerotees.py
    python generer_cartes_numerotees.py --seulement-composer   # sauter ComfyUI
"""

import json
import os
import random
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

# ---------------------------------------------------------------------------
# Chemins
# ---------------------------------------------------------------------------
COMFYUI_URL = "http://127.0.0.1:8188"
COMFYUI_SORTIE = Path("/var/lib/comfyui/output")
SCRIPT_DIR = Path(__file__).parent
REPO_ROOT = SCRIPT_DIR.parent.parent
SORTIE_DIR = REPO_ROOT / "art" / "cartes"
FONDS_DIR = SORTIE_DIR / "fonds"
COLONNE_DIR = REPO_ROOT / "art" / "cartes" / "colonne"
POLICE = "/usr/share/fonts/TTF/OpenSans-ExtraBold.ttf"

# Bordure blanche uniforme et rayon de coin pour les fonds traités.
BORDURE_FOND = 38  # px depuis chaque bord
RAYON_FOND   = 38  # rayon des coins arrondis

# ---------------------------------------------------------------------------
# Couleurs (noms correspondant au type Color de Cards.hs)
# ---------------------------------------------------------------------------
COULEURS = {
    "bleu": (
        "solid blue background completely covered with subtle embossed geometric tribal texture pattern, "
        "angular maze labyrinth motifs evenly distributed across entire surface, "
        "no frame, no border, no inner rectangle, no card design, "
        "flat graphic illustration style, clean vector art, no text, no numbers, no symbols"
    ),
    "jaune": (
        "solid warm golden yellow background completely covered with subtle embossed geometric tribal texture pattern, "
        "angular geometric shapes motifs evenly distributed across entire surface, "
        "no frame, no border, no inner rectangle, no card design, "
        "flat graphic illustration style, clean vector art, no text, no numbers, no symbols"
    ),
    "rouge": (
        "solid crimson red background completely covered with subtle embossed african tribal geometric texture pattern, "
        "ethnic zigzag lines triangles diamond shapes motifs evenly distributed across entire surface, "
        "no frame, no border, no inner rectangle, no card design, "
        "flat graphic illustration style, clean vector art, no text, no numbers, no symbols"
    ),
    "violet": (
        "solid deep purple background completely covered with subtle embossed african tribal geometric texture pattern, "
        "ethnic zigzag lines circles spirals motifs evenly distributed across entire surface, "
        "no frame, no border, no inner rectangle, no card design, "
        "flat graphic illustration style, clean vector art, no text, no numbers, no symbols"
    ),
}



# ---------------------------------------------------------------------------
# Post-traitement des fonds IA : recoloriage + masque propre
# ---------------------------------------------------------------------------

def echantillonner_couleur_reference(couleur: str) -> tuple:
    """Échantillonne la couleur de fond depuis la carte de référence (bords, mi-hauteur)."""
    ref = Image.open(COLONNE_DIR / f"{couleur}.png").convert("RGB")
    arr = np.array(ref, dtype=np.float32)
    larg, haut = ref.size
    zones = [
        arr[haut // 2 - 80 : haut // 2 + 80, 42:90],
        arr[haut // 2 - 80 : haut // 2 + 80, larg - 90 : larg - 42],
    ]
    pixels = np.concatenate([z.reshape(-1, 3) for z in zones])
    non_blanc = pixels[(pixels[:, 0] < 230) | (pixels[:, 1] < 230) | (pixels[:, 2] < 230)]
    return tuple(non_blanc.mean(axis=0).astype(int))


def post_traiter_fond(fond: Image.Image, couleur: str) -> Image.Image:
    """Recolorie le fond IA et applique un masque Pillow propre.

    Isolation de la texture fine :
      1. Calcule la luminance de l'image IA.
      2. Applique un flou gaussien fort (rayon 50 px) pour obtenir la
         « structure » grande-échelle (cadre interne, dégradé global, etc.).
      3. Soustrait le lissé → résidu centré sur 0 = texture fine seulement.
      4. Applique ce résidu (normalisé) à la couleur cible : ±15 % de variation.
      5. Colle le résultat dans un masque Pillow propre (rectangle arrondi).
    """
    larg, haut = fond.size
    arr = np.array(fond.convert("RGB"), dtype=np.float32)

    cible = echantillonner_couleur_reference(couleur)
    r_c, g_c, b_c = cible

    # Luminance perceptuelle de l'image IA
    lum = 0.299 * arr[:, :, 0] + 0.587 * arr[:, :, 1] + 0.114 * arr[:, :, 2]

    # Lissage fort → capte la structure grande-échelle (cadre IA, dégradé…)
    lum_img = Image.fromarray(np.clip(lum, 0, 255).astype(np.uint8))
    lum_lisse = np.array(
        lum_img.filter(ImageFilter.GaussianBlur(radius=50)), dtype=np.float32
    )

    # Résidu = variation fine (texture) seulement, centré sur 0
    residuel = lum - lum_lisse
    sigma = residuel.std() + 1          # normalisation
    facteur = 1.0 + (residuel / sigma) * 0.15   # ±15 % autour de la cible

    # Couleur cible teintée par la texture fine
    recolorie = arr.copy()
    recolorie[:, :, 0] = np.clip(r_c * facteur, 0, 255)
    recolorie[:, :, 1] = np.clip(g_c * facteur, 0, 255)
    recolorie[:, :, 2] = np.clip(b_c * facteur, 0, 255)

    # Masque Pillow propre (rectangle arrondi, bords lisses)
    masque = Image.new("L", (larg, haut), 0)
    ImageDraw.Draw(masque).rounded_rectangle(
        [BORDURE_FOND, BORDURE_FOND, larg - BORDURE_FOND, haut - BORDURE_FOND],
        radius=RAYON_FOND,
        fill=255,
    )

    texture = Image.fromarray(recolorie.clip(0, 255).astype(np.uint8))
    fond_propre = Image.new("RGB", (larg, haut), "white")
    fond_propre.paste(texture, mask=masque)
    return fond_propre


# ---------------------------------------------------------------------------
# ComfyUI — génération du fond
# ---------------------------------------------------------------------------
def construire_workflow(prompt: str, graine: int) -> dict:
    return {
        "1": {"class_type": "UNETLoader",
              "inputs": {"unet_name": "z_image_turbo_bf16.safetensors", "weight_dtype": "default"}},
        "2": {"class_type": "CLIPLoader",
              "inputs": {"clip_name": "qwen_3_4b.safetensors", "type": "lumina2", "device": "default"}},
        "3": {"class_type": "VAELoader",
              "inputs": {"vae_name": "ae.safetensors"}},
        "4": {"class_type": "LoraLoader",
              "inputs": {"model": ["1", 0], "clip": ["2", 0],
                         "lora_name": "pixel_art_style_z_image_turbo.safetensors",
                         "strength_model": 0.0, "strength_clip": 0.0}},
        "5": {"class_type": "ModelSamplingAuraFlow",
              "inputs": {"model": ["4", 0], "shift": 3.0}},
        "6": {"class_type": "CLIPTextEncode",
              "inputs": {"clip": ["4", 1], "text": prompt}},
        "7": {"class_type": "ConditioningZeroOut",
              "inputs": {"conditioning": ["6", 0]}},
        "8": {"class_type": "EmptySD3LatentImage",
              "inputs": {"width": 512, "height": 784, "batch_size": 1}},
        "9": {"class_type": "KSampler",
              "inputs": {"model": ["5", 0], "positive": ["6", 0], "negative": ["7", 0],
                         "latent_image": ["8", 0], "seed": graine,
                         "steps": 4, "cfg": 1.0, "sampler_name": "res_multistep",
                         "scheduler": "simple", "denoise": 1.0}},
        "10": {"class_type": "VAEDecode",
               "inputs": {"samples": ["9", 0], "vae": ["3", 0]}},
        "11": {"class_type": "SaveImage",
               "inputs": {"images": ["10", 0], "filename_prefix": "fond_carte"}},
    }


def soumettre(workflow: dict) -> str:
    payload = json.dumps({"prompt": workflow}).encode()
    req = urllib.request.Request(
        f"{COMFYUI_URL}/prompt", data=payload,
        headers={"Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(req) as resp:
            result = json.load(resp)
            if "error" in result:
                raise RuntimeError(f"Erreur ComfyUI : {result['error']}")
            return result["prompt_id"]
    except urllib.error.HTTPError as e:
        raise RuntimeError(f"HTTP {e.code} : {e.read().decode()}") from e


def attendre_et_recuperer(prompt_id: str, timeout: int = 300) -> Path:
    """Attend la génération et retourne le chemin de l'image produite."""
    debut = time.time()
    while True:
        if time.time() - debut > timeout:
            raise TimeoutError(f"Délai dépassé après {timeout} s")
        time.sleep(2)
        with urllib.request.urlopen(f"{COMFYUI_URL}/history/{prompt_id}") as resp:
            historique = json.load(resp)
        if prompt_id not in historique:
            continue
        statut = historique[prompt_id].get("status", {})
        if statut.get("status_str") == "error":
            raise RuntimeError(f"Génération échouée : {statut.get('messages')}")
        if statut.get("completed"):
            sorties = historique[prompt_id].get("outputs", {})
            for sortie in sorties.values():
                for img in sortie.get("images", []):
                    return COMFYUI_SORTIE / img["filename"]
    raise RuntimeError("Aucune image trouvée dans l'historique")


def generer_fond(couleur: str, prompt: str) -> Path:
    """Génère le fond texturé via ComfyUI, applique le post-traitement et le
    copie dans art/cartes/fonds/."""
    dest = FONDS_DIR / f"fond_{couleur}.png"
    if dest.exists():
        print(f"  [fond {couleur}] déjà présent, on réutilise.")
        return dest
    graine = random.randint(0, 2 ** 32 - 1)
    print(f"  [fond {couleur}] génération (graine {graine})…")
    workflow = construire_workflow(prompt, graine)
    pid = soumettre(workflow)
    chemin_src = attendre_et_recuperer(pid)
    dest.parent.mkdir(parents=True, exist_ok=True)
    fond_brut = Image.open(chemin_src)
    fond_traite = post_traiter_fond(fond_brut, couleur)
    fond_traite.save(dest)
    print(f"  [fond {couleur}] sauvegardé → {dest.name}")
    return dest


# ---------------------------------------------------------------------------
# Pillow — composition des chiffres
# ---------------------------------------------------------------------------
def trouver_taille_police(texte: str, chemin: str, larg_max: int, haut_max: int) -> ImageFont.FreeTypeFont:
    for taille in range(480, 10, -4):
        police = ImageFont.truetype(chemin, taille)
        bbox = police.getbbox(texte)
        if (bbox[2] - bbox[0]) <= larg_max and (bbox[3] - bbox[1]) <= haut_max:
            return police
    return ImageFont.truetype(chemin, 10)


def coller_texte_coin(carte: Image.Image, texte: str, police: ImageFont.FreeTypeFont,
                      x: int, y: int, retourne: bool) -> None:
    """Dessine un numéro de coin (avec contour blanc) à la position (x, y).

    Si retourne=True, l'image du texte est pivotée de 180° avant collage
    (pour les coins inférieurs).
    """
    bbox = police.getbbox(texte)
    larg = bbox[2] - bbox[0] + 8
    haut = bbox[3] - bbox[1] + 8

    img_coin = Image.new("RGBA", (larg, haut), (0, 0, 0, 0))
    d = ImageDraw.Draw(img_coin)
    d.text((-bbox[0] + 4, -bbox[1] + 4), texte, font=police,
           fill="white", stroke_width=3, stroke_fill=(30, 30, 30, 220))

    if retourne:
        img_coin = img_coin.rotate(180)

    carte.paste(img_coin, (x, y), img_coin)


def coller_points_coin(carte: Image.Image, x: int, y: int, retourne: bool,
                       n_points: int = 3, rayon: int = 5, espacement: int = 14) -> None:
    """Dessine une colonne de petits cercles blancs près d'un coin."""
    draw = ImageDraw.Draw(carte)
    for i in range(n_points):
        cy = y + i * espacement if not retourne else y - i * espacement
        draw.ellipse([x - rayon, cy - rayon, x + rayon, cy + rayon],
                     fill="white", outline=(30, 30, 30))


def composer_carte(fond: Image.Image, numero: int) -> Image.Image:
    carte = fond.convert("RGBA").copy()
    draw = ImageDraw.Draw(carte)
    larg, haut = carte.size
    texte = str(numero)

    # --- Numéro central ---
    marge_centrale_h = int(larg * 0.15)
    marge_centrale_v = int(haut * 0.25)
    larg_max = larg - 2 * marge_centrale_h
    haut_max = haut - 2 * marge_centrale_v

    police_centrale = trouver_taille_police(texte, POLICE, larg_max, haut_max)
    bbox = police_centrale.getbbox(texte)
    tw, th = bbox[2] - bbox[0], bbox[3] - bbox[1]
    cx = (larg - tw) // 2 - bbox[0]
    cy = (haut - th) // 2 - bbox[1]
    draw.text((cx, cy), texte, font=police_centrale, fill="white",
              stroke_width=8, stroke_fill=(20, 20, 20, 200))

    # --- Numéros de coin ---
    taille_coin = 50
    police_coin = ImageFont.truetype(POLICE, taille_coin)
    # Les fonds Pillow ont une bordure blanche uniforme de 38px.
    # On utilise 53px pour rester confortablement dans la zone colorée (38+15).
    marge = 53
    bbox_coin = police_coin.getbbox(texte)
    lc = bbox_coin[2] - bbox_coin[0]
    hc = bbox_coin[3] - bbox_coin[1]

    # Coin supérieur gauche
    coller_texte_coin(carte, texte, police_coin, marge, marge, retourne=False)
    # Coin supérieur droit
    coller_texte_coin(carte, texte, police_coin, larg - marge - lc - 8, marge, retourne=False)
    # Coin inférieur gauche (retourné)
    coller_texte_coin(carte, texte, police_coin, marge, haut - marge - hc - 8, retourne=True)
    # Coin inférieur droit (retourné)
    coller_texte_coin(carte, texte, police_coin, larg - marge - lc - 8,
                      haut - marge - hc - 8, retourne=True)

    # --- Points décoratifs ---
    px = marge + lc // 2 + 4
    # Sous le coin supérieur gauche
    coller_points_coin(carte, px, marge + hc + 18, retourne=False)
    # Sous le coin supérieur droit
    coller_points_coin(carte, larg - marge - lc // 2 - 4, marge + hc + 18, retourne=False)
    # Au-dessus du coin inférieur gauche (retourné → dessiné vers le haut)
    coller_points_coin(carte, px, haut - marge - hc - 22, retourne=True)
    # Au-dessus du coin inférieur droit
    coller_points_coin(carte, larg - marge - lc // 2 - 4,
                       haut - marge - hc - 22, retourne=True)

    return carte.convert("RGB")


# ---------------------------------------------------------------------------
# Programme principal
# ---------------------------------------------------------------------------
def main() -> None:
    seulement_composer = "--seulement-composer" in sys.argv

    SORTIE_DIR.mkdir(parents=True, exist_ok=True)
    FONDS_DIR.mkdir(parents=True, exist_ok=True)

    for couleur, prompt in COULEURS.items():
        print(f"\n=== {couleur.upper()} ===")

        if seulement_composer:
            chemin_fond = FONDS_DIR / f"fond_{couleur}.png"
            if not chemin_fond.exists():
                print(f"  ERREUR : fond manquant ({chemin_fond}). Relancer sans --seulement-composer.")
                continue
        else:
            chemin_fond = generer_fond(couleur, prompt)

        fond = Image.open(chemin_fond)

        for numero in range(1, 19):
            carte = composer_carte(fond, numero)
            nom = SORTIE_DIR / f"habanga_{couleur}_{numero:02d}.png"
            carte.save(nom)
            print(f"  {nom.name}")

    print(f"\nTerminé. {4 * 18} cartes dans {SORTIE_DIR}/")


if __name__ == "__main__":
    main()
