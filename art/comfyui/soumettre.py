#!/usr/bin/env python3
"""Soumet un workflow de génération de cartes-dos à ComfyUI.

Après génération, applique automatiquement une bordure blanche arrondie
via Pillow (même approche que pour les cartes numérotées).

Usage:
    python soumettre.py [couleur]

Couleurs disponibles : bleu (défaut), orange, violet, rouge
"""

import json
import random
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

from PIL import Image, ImageDraw

COMFYUI_URL    = "http://127.0.0.1:8188"
COMFYUI_SORTIE = Path("/var/lib/comfyui/output")

# Bordure blanche ajoutée par Pillow après génération
BORDURE = 38
RAYON   = 38

PROMPTS = {
    "bleu": (
        "african tribal ceremonial mask centered on solid blue background, "
        "full bleed illustration filling entire image edge to edge, "
        "flat graphic illustration style, bold flat solid colors, no gradients, "
        "blue background with subtle embossed geometric tribal texture pattern, "
        "blue painted wooden ceremonial mask with colorful decorative feathers on top, "
        "circular forehead ornament, "
        "flat clean white sticker border around mask only, no shadow, no drop shadow, "
        "no dark outline, no black border, no glow, pure white flat border only, "
        "no card border, no card frame, no white card outline, "
        "clean vector art, vibrant saturated colors"
    ),
    "orange": (
        "african tribal ceremonial mask centered on solid orange background, "
        "full bleed illustration filling entire image edge to edge, "
        "flat graphic illustration style, bold flat solid colors, no gradients, "
        "orange background with subtle embossed geometric tribal texture pattern, "
        "yellow orange painted wooden ceremonial mask with red feathers on sides, "
        "elongated oval face, "
        "flat clean white sticker border around mask only, no shadow, no drop shadow, "
        "no dark outline, no black border, no glow, pure white flat border only, "
        "no card border, no card frame, no white card outline, "
        "clean vector art, vibrant saturated colors"
    ),
    "violet": (
        "african tribal ceremonial mask centered on solid purple background, "
        "full bleed illustration filling entire image edge to edge, "
        "flat graphic illustration style, bold flat solid colors, no gradients, "
        "purple violet background with clearly visible embossed geometric tribal texture pattern, "
        "angular spirals maze labyrinths zigzag circles motifs throughout entire background, "
        "purple painted wooden ceremonial mask with horns, red dot accents, "
        "orange tongue, "
        "flat clean white sticker border around mask only, no shadow, no drop shadow, "
        "no dark outline, no black border, no glow, pure white flat border only, "
        "no card border, no card frame, no white card outline, "
        "clean vector art, vibrant saturated colors"
    ),
    "rouge": (
        "african tribal ceremonial mask centered on solid red background, "
        "full bleed illustration filling entire image edge to edge, "
        "flat graphic illustration style, bold flat solid colors, no gradients, "
        "red background with subtle embossed geometric tribal texture pattern, "
        "red orange painted wooden ceremonial mask with colorful feathers on sides, "
        "white dot decorations, smiling expression, "
        "flat clean white sticker border around mask only, no shadow, no drop shadow, "
        "no dark outline, no black border, no glow, pure white flat border only, "
        "no card border, no card frame, no white card outline, "
        "clean vector art, vibrant saturated colors"
    ),
}


def appliquer_bordure(chemin: Path) -> None:
    """Applique une bordure blanche arrondie sur l'image générée par l'IA."""
    img = Image.open(chemin).convert("RGB")
    larg, haut = img.size
    masque = Image.new("L", (larg, haut), 0)
    ImageDraw.Draw(masque).rounded_rectangle(
        [BORDURE, BORDURE, larg - BORDURE, haut - BORDURE],
        radius=RAYON,
        fill=255,
    )
    fond = Image.new("RGB", (larg, haut), "white")
    fond.paste(img, mask=masque)
    fond.save(chemin)


def construire_workflow(prompt: str, graine: int) -> dict:
    """Construit le workflow en format API ComfyUI."""
    return {
        "1": {
            "class_type": "UNETLoader",
            "inputs": {
                "unet_name": "z_image_turbo_bf16.safetensors",
                "weight_dtype": "default",
            },
        },
        "2": {
            "class_type": "CLIPLoader",
            "inputs": {
                "clip_name": "qwen_3_4b.safetensors",
                "type": "lumina2",
                "device": "default",
            },
        },
        "3": {
            "class_type": "VAELoader",
            "inputs": {"vae_name": "ae.safetensors"},
        },
        "4": {
            # LoRA pixel art désactivée (force 0.0) — style illustration plate voulu
            "class_type": "LoraLoader",
            "inputs": {
                "model": ["1", 0],
                "clip": ["2", 0],
                "lora_name": "pixel_art_style_z_image_turbo.safetensors",
                "strength_model": 0.0,
                "strength_clip": 0.0,
            },
        },
        "5": {
            "class_type": "ModelSamplingAuraFlow",
            "inputs": {"model": ["4", 0], "shift": 3.0},
        },
        "6": {
            "class_type": "CLIPTextEncode",
            "inputs": {"clip": ["4", 1], "text": prompt},
        },
        "7": {
            # Pas de prompt négatif — architecture Lumina2
            "class_type": "ConditioningZeroOut",
            "inputs": {"conditioning": ["6", 0]},
        },
        "8": {
            # Ratio ~1:1,53 — cohérent avec les cartes de référence (277×425 px)
            "class_type": "EmptySD3LatentImage",
            "inputs": {"width": 512, "height": 784, "batch_size": 1},
        },
        "9": {
            "class_type": "KSampler",
            "inputs": {
                "model": ["5", 0],
                "positive": ["6", 0],
                "negative": ["7", 0],
                "latent_image": ["8", 0],
                "seed": graine,
                "steps": 4,
                "cfg": 1.0,
                "sampler_name": "res_multistep",
                "scheduler": "simple",
                "denoise": 1.0,
            },
        },
        "10": {
            "class_type": "VAEDecode",
            "inputs": {"samples": ["9", 0], "vae": ["3", 0]},
        },
        "11": {
            "class_type": "SaveImage",
            "inputs": {"images": ["10", 0], "filename_prefix": "game_asset"},
        },
    }


def soumettre(workflow: dict) -> str:
    """Soumet le workflow à ComfyUI et retourne le prompt_id."""
    payload = json.dumps({"prompt": workflow}).encode()
    req = urllib.request.Request(
        f"{COMFYUI_URL}/prompt",
        data=payload,
        headers={"Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(req) as resp:
            result = json.load(resp)
            if "error" in result:
                raise RuntimeError(f"Erreur ComfyUI : {result['error']}")
            return result["prompt_id"]
    except urllib.error.HTTPError as e:
        corps = e.read().decode()
        raise RuntimeError(f"HTTP {e.code} : {corps}") from e


def attendre_completion(prompt_id: str, timeout: int = 300) -> list[Path]:
    """Attend la génération et retourne les chemins des images produites."""
    debut = time.time()
    print(f"  En attente de la génération (prompt_id={prompt_id})…")
    while True:
        if time.time() - debut > timeout:
            raise TimeoutError(f"Délai dépassé après {timeout} s")
        time.sleep(2)
        with urllib.request.urlopen(f"{COMFYUI_URL}/history/{prompt_id}") as resp:
            historique = json.load(resp)
        if prompt_id in historique:
            entree = historique[prompt_id]
            statut = entree.get("status", {})
            if statut.get("completed"):
                chemins = []
                for sortie in entree.get("outputs", {}).values():
                    for img in sortie.get("images", []):
                        chemins.append(COMFYUI_SORTIE / img["filename"])
                return chemins
            if statut.get("status_str") == "error":
                msgs = statut.get("messages", [])
                raise RuntimeError(f"Génération échouée : {msgs}")
    return []


def main() -> None:
    couleur = sys.argv[1].lower() if len(sys.argv) > 1 else "bleu"
    if couleur not in PROMPTS:
        print(f"Couleur inconnue : {couleur!r}")
        print(f"Couleurs disponibles : {', '.join(PROMPTS)}")
        sys.exit(1)

    graine = random.randint(0, 2**32 - 1)
    print(f"Couleur : {couleur}  |  Graine : {graine}")

    workflow = construire_workflow(PROMPTS[couleur], graine)
    prompt_id = soumettre(workflow)
    print(f"  Soumis avec succès (prompt_id={prompt_id})")

    chemins = attendre_completion(prompt_id)
    for chemin in chemins:
        appliquer_bordure(chemin)
        print(f"  Image avec bordure : {chemin.name}")

    print("  Terminé.")


if __name__ == "__main__":
    main()
