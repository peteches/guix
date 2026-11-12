"""SillyTavernReferenceSheet — collapse 1..N avatar references into ONE image.

WHY
---
`TextEncodeMOSSImageEdit`'s `images` input is a ComfyUI-v3 Autogrow slot
(min 1, max 10), so a graph can wire up to ten references. But SillyTavern
builds the workflow by *textual substitution* and there is no way to delete a
JSON key that way: a reference slot whose token was never substituted still
feeds something to the encoder. A workflow with four slots therefore cannot be
used for a one-character scene without the three empty slots contributing
blank conditioning.

This node removes that constraint, so ONE workflow serves 1..N references and
you never have to switch files per chat:

  * each reference arrives from its own `LoadImageFromBase64OrUrl`
  * that loader emits a 1x1 placeholder for anything "missing" (unsubstituted
    token, unset `{{getvar::}}`, ST's PNG_PIXEL fallback)
  * this node DROPS the blanks, tiles only the real references, and resizes the
    result to `target_size`

With exactly one reference present the output is that reference at full
`target_size` — pixel-identical to what a dedicated single-reference workflow
would produce, so the 1-ref case pays nothing for this flexibility.

LAYOUT (auto)
-------------
  1 ref  -> full frame                      (each ref  target x target)
  2 refs -> 1 row x 2 cols                  (each ref  target/2 x target)
  3 refs -> 2 x 2, bottom-right left blank  (each ref  target/2 x target/2)
  4 refs -> 2 x 2                           (each ref  target/2 x target/2)

Cells are filled left-to-right, top-to-bottom, matching the order the loaders
appear in the graph. Each input is resized to COVER its cell and center-cropped,
so panels keep their aspect ratio instead of being squashed.

The instruction text in the workflow must tell the model this may be a contact
sheet; see `sillytavern_moss_sft_4step_ref.json` node 22.

TRADEOFF
--------
Two or more references share the frame, so each character gets fewer pixels than
a dedicated full-frame reference would give them. If identity fidelity across a
multi-character scene proves too weak, switch to native Autogrow slots
(`images.image_0`..`image_N`, one full-resolution reference each) and accept
that the reference count is then fixed per workflow file.
"""

import torch
import numpy as np
from PIL import Image


def _tensor_to_pil(t: torch.Tensor) -> Image.Image:
    """First frame of a ComfyUI IMAGE tensor ([B,H,W,C], float 0..1) -> RGB PIL."""
    a = t[0].detach().cpu().numpy() if t.dim() == 4 else t.detach().cpu().numpy()
    a = np.clip(a, 0.0, 1.0)
    return Image.fromarray((a * 255.0).round().astype(np.uint8)).convert("RGB")


def _pil_to_tensor(i: Image.Image) -> torch.Tensor:
    a = np.array(i.convert("RGB"), dtype=np.float32) / 255.0
    return torch.from_numpy(a)[None,]


def _is_blank(i: Image.Image) -> bool:
    """True when an image carries no identity information.

    Two signatures:

      * **any 1x1 image**, regardless of colour. This is what
        LoadImageFromBase64OrUrl emits for a missing reference, and what
        SillyTavern's PNG_PIXEL constant is. PNG_PIXEL is a 1x1 fully
        TRANSPARENT BLACK pixel (mode LA, value (0, 0)) -- NOT white. An earlier
        revision of this function tested `getpixel() == (255, 255, 255)`, which
        never matched, so every unset slot survived as a black panel and a solo
        scene was tiled as a 2x2 sheet of one avatar plus three black squares.
        One pixel cannot describe a character, so size alone is the right test
        here; dropping a deliberate 1x1 avatar costs nothing but its
        (nonexistent) detail.
      * any larger solid-colour frame (zero standard deviation across all
        channels). A uniform panel cannot describe a character either, and this
        also catches blanks that were upscaled to target_size by an intervening
        ImageScale -- where the 1x1 test would no longer fire.
    """
    if i.size == (1, 1):
        return True
    a = np.asarray(i, dtype=np.float32)
    return float(a.std()) < 1e-6


def _cover_crop(i: Image.Image, w: int, h: int) -> Image.Image:
    """Resize to COVER w x h preserving aspect ratio, then center-crop."""
    if w <= 0 or h <= 0:
        return i
    sw, sh = i.size
    if sw <= 0 or sh <= 0:
        return Image.new("RGB", (w, h), (255, 255, 255))
    scale = max(w / sw, h / sh)
    nw, nh = max(1, round(sw * scale)), max(1, round(sh * scale))
    i = i.resize((nw, nh), Image.LANCZOS)
    left, top = (nw - w) // 2, (nh - h) // 2
    return i.crop((left, top, left + w, top + h))


class SillyTavernReferenceSheet:
    @classmethod
    def INPUT_TYPES(cls):
        return {
            "required": {
                "target_size": ("INT", {
                    "default": 1024, "min": 64, "max": 4096, "step": 64,
                    "tooltip": "Square edge length of the output sheet. MUST equal the EmptyLatentImage size: TextEncodeMOSSImageEdit uses references at the size they arrive and its VAE-encode fails on tiny inputs."}),
            },
            # All four optional, so the same node serves a solo portrait and a
            # four-character group scene without editing the graph.
            "optional": {
                "image_1": ("IMAGE",),
                "image_2": ("IMAGE",),
                "image_3": ("IMAGE",),
                "image_4": ("IMAGE",),
            },
        }

    RETURN_TYPES = ("IMAGE", "INT")
    RETURN_NAMES = ("sheet", "reference_count")
    FUNCTION = "build"
    CATEGORY = "peteches/SillyTavern"
    DESCRIPTION = "Drops blank/unset avatar slots and tiles the remaining references into one square contact sheet for TextEncodeMOSSImageEdit."

    def build(self, target_size: int = 1024, image_1=None, image_2=None,
              image_3=None, image_4=None):
        size = int(target_size)
        panels = []
        for slot, t in enumerate((image_1, image_2, image_3, image_4), start=1):
            if t is None:
                continue
            if not isinstance(t, torch.Tensor) or t.numel() == 0:
                continue
            try:
                pil = _tensor_to_pil(t)
            except Exception as e:
                print(f"[peteches/SillyTavern] slot {slot}: unreadable image ({e}); skipping")
                continue
            if _is_blank(pil):
                print(f"[peteches/SillyTavern] slot {slot}: blank/unset reference, dropped")
                continue
            panels.append(pil)

        n = len(panels)

        # Nothing usable: hand back a neutral full-size frame rather than a 1x1.
        # A 1x1 would crash TextEncodeMOSSImageEdit's VAE-encode, and the
        # workflow's instruction tells the model to ignore a blank sheet.
        if n == 0:
            print("[peteches/SillyTavern] no usable references; emitting blank sheet")
            return (_pil_to_tensor(Image.new("RGB", (size, size), (255, 255, 255))), 0)

        # 1 -> full frame; 2 -> one row of two; 3-4 -> 2x2 (3 leaves one cell empty)
        if n == 1:
            cols, rows = 1, 1
        elif n == 2:
            cols, rows = 2, 1
        else:
            cols, rows = 2, 2

        cw, ch = size // cols, size // rows
        sheet = Image.new("RGB", (cols * cw, rows * ch), (255, 255, 255))
        for idx, p in enumerate(panels[:cols * rows]):
            r, c = divmod(idx, cols)
            sheet.paste(_cover_crop(p, cw, ch), (c * cw, r * ch))

        # Keep the output exactly square even when size is not divisible by cols.
        if sheet.size != (size, size):
            sheet = sheet.resize((size, size), Image.LANCZOS)

        print(f"[peteches/SillyTavern] reference sheet: {n} panel(s) in a "
              f"{cols}x{rows} layout at {size}x{size}")
        return (_pil_to_tensor(sheet), n)


NODE_CLASS_MAPPINGS = {"SillyTavernReferenceSheet": SillyTavernReferenceSheet}
NODE_DISPLAY_NAME_MAPPINGS = {
    "SillyTavernReferenceSheet": "SillyTavern Reference Sheet (drop blanks + tile)",
}
