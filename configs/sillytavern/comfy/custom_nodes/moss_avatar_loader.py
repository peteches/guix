"""LoadImageFromBase64OrUrl — tolerant avatar loader for SillyTavern's char_avatar token.

SillyTavern's ComfyUI integration performs a *textual* substitution on the
serialized workflow: it replaces the string token `"char_avatar"` (with its
quotes) by `JSON.stringify(avatarBase64)` — i.e. the base64 of the character
card avatar WITHOUT any `data:image/png;base64,` prefix. The widget value that
arrives at ComfyUI is therefore one of:

  * bare base64 (ST injected a real avatar)       -> "iVBORw0KGgo..."
  * the literal token string, unsubstituted        -> "char_avatar"
    (e.g. testing the workflow directly in ComfyUI's Web UI)

ArtVenture's LoadImageFromUrl cannot handle case 1: it requires a
`data:image/...;base64,` prefix (it splits on "," and decodes part [1]), and
falls through to a file/URL branch otherwise, raising "Invalid url" or
binascii "Incorrect padding". This node accepts all of the above plus ordinary
URLs, so no client-side concatenation is needed: set the widget value to
exactly `char_avatar`.

Outputs match comfy's LoadImage (IMAGE + MASK) so it feeds ImageScale ->
TextEncodeMOSSImageEdit unchanged.
"""

import os
import io
import base64
import torch
import numpy as np
from PIL import Image, ImageOps

import requests
import folder_paths


def _pil_to_tensor(i: Image.Image):
    i = ImageOps.exif_transpose(i)
    has_alpha = "A" in i.getbands()
    if "RGB" not in i.mode:
        i = i.convert("RGBA") if has_alpha else i.convert("RGB")
    image_np = np.array(i.convert("RGB").crop((0, 0, i.width, i.height)), dtype=np.float32) / 255.0
    tensor = torch.from_numpy(image_np)[None,]
    if has_alpha:
        alpha_np = np.array(i.getchannel("A").crop((0, 0, i.width, i.height)), dtype=np.float32) / 255.0
        mask = torch.from_numpy(alpha_np)[None,][0]
    else:
        mask = torch.zeros((i.height, i.width), dtype=torch.float32)
    return tensor, mask


def _is_no_avatar(p: Image.Image) -> bool:
    """True when p is ST's PNG_PIXEL placeholder (1x1 white).

    Two ways to be 'no avatar': the base64 equals ST's PNG_PIXEL constant, or it
    decodes to a single pixel whose RGB is white — which is what that constant is.
    A 1x1 image is NOT sufficient evidence: real avatars can legitimately be 1x1
    when the card author uploaded one deliberately (observed in the wild), so
    size alone must never trigger this. The value check is what makes it safe.
    """
    return p.size == (1, 1) and p.convert("RGB").getpixel((0, 0)) == (255, 255, 255)


def _decode_b64(s: str) -> Image.Image:
    payload = s.strip()
    rem = len(payload) % 4
    if rem:
        payload += "=" * (4 - rem)
    raw = base64.b64decode(payload.replace("-", "/").replace("_", "+"), validate=False)
    return Image.open(io.BytesIO(raw))


# ST's PNG_PIXEL constant (1x1 px), used when a character/persona has no avatar.
# MUST match ST's own constant (/srv/sillytavern/public/scripts/extensions/
# stable-diffusion/index.js, `const PNG_PIXEL = ...`): that is the base64 ST
# substitutes for "%char_avatar%" when it cannot fetch the avatar. It is also a
# real 1x1 white pixel, so an unrelated character's avatar can be detected by
# value rather than guessed at — see _is_no_avatar() below.
PNG_PIXEL_B64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="


class LoadImageFromBase64OrUrl:
    def __init__(self):
        self.output_dir = folder_paths.get_input_directory()

    @classmethod
    def INPUT_TYPES(cls):
        return {
            "required": {
                "image": ("STRING", {"default": "", "multiline": True,
                                     "tooltip": 'Paste base64, a data: URI, an http(s) URL, or use SillyTavern\'s char_avatar token'}),
            },
        }

    RETURN_TYPES = ("IMAGE", "MASK")
    RETURN_NAMES = ("image", "mask")
    FUNCTION = "load"
    CATEGORY = "peteches/SillyTavern"

    def load(self, image: str):
        text = image.strip()
        if not text or "char_avatar" in text or "user_avatar" in text:
            # placeholder unsubstituted (Web-UI test) or empty -> inert 1x1 reference
            i = _decode_b64(PNG_PIXEL_B64)
        elif text.startswith("data:image/"):
            i = _decode_b64(text.split(",", 1)[1])
            if _is_no_avatar(i):
                raise ValueError(
                    "Avatar is a 1x1 white pixel — SillyTavern sent its PNG_PIXEL "
                    "placeholder, so this character has no usable avatar. Add one to "
                    "the character card (Character > Avatar) or switch to a workflow "
                    "without reference conditioning.")
        elif text.startswith("data:image/"):
            i = _decode_b64(text.split(",", 1)[1])
        elif text.startswith("http://") or text.startswith("https://"):
            resp = requests.get(text, timeout=10)
            resp.raise_for_status()
            i = Image.open(io.BytesIO(resp.content))
        else:
            # bare base64 (the ST case); if that fails treat as input-dir filename
            try:
                i = _decode_b64(text)
            except Exception:
                path = folder_paths.get_annotated_filepath(text)
                if not path or not os.path.isfile(path):
                    raise ValueError(f"Invalid avatar input: {text[:60]!r}")
                i = Image.open(path)

        t, m = _pil_to_tensor(i)
        return (t, m)


NODE_CLASS_MAPPINGS = {"LoadImageFromBase64OrUrl": LoadImageFromBase64OrUrl}
NODE_DISPLAY_NAME_MAPPINGS = {"LoadImageFromBase64OrUrl": "Load Image From Base64/URL/char_avatar"}
