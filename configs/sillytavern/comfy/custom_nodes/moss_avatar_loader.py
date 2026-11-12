"""LoadImageFromBase64OrUrl — tolerant avatar/reference loader for SillyTavern.

WHY THIS NODE EXISTS
--------------------
SillyTavern's ComfyUI integration performs a purely *textual* substitution on
the serialized workflow JSON (verified against ST 1.19.0,
public/scripts/extensions/stable-diffusion/index.js:4221-4290). It only ever
replaces a token that appears as a COMPLETE, STANDALONE QUOTED STRING VALUE,
and the pattern includes the percent signs and the quotes:

    workflow.replaceAll('"%prompt%"',        JSON.stringify(prompt))
    workflow.replaceAll('"%char_avatar%"',   JSON.stringify(avatarBase64))
    workflow.replaceAll('"%user_avatar%"',   JSON.stringify(avatarBase64))
    workflow.replaceAll('"%<find>%"',        JSON.stringify(substituteParams(replace)))

Two consequences that bit this repo before:

  * The widget value must be exactly  %char_avatar%  — NOT  char_avatar.
    ST gates the avatar fetch itself behind  /%char_avatar%/gi.test(workflow),
    so a percent-less token means ST never even looks up an avatar and the
    literal string reaches this node.
  * A token embedded in a longer string (e.g. "moss_xvstyle %prompt%") is
    NEVER substituted and arrives verbatim.

The injected avatar is base64 with NO `data:image/png;base64,` prefix (ST takes
`dataUrl.split(',')[1]`). ArtVenture's LoadImageFromUrl requires that prefix and
therefore cannot consume ST's output — hence this node.

WHAT THIS NODE ACCEPTS
----------------------
  * bare base64                       -> the SillyTavern case
  * a full `data:image/...;base64,` URI
  * an http(s) URL                    -> fetched server-side
  * a filename in ComfyUI's input dir -> for Web-UI testing
  * anything "missing" (see below)    -> handled per `on_missing`

"MISSING" IS A NORMAL STATE, NOT AN ERROR
-----------------------------------------
ST substitutes PNG_PIXEL (a 1x1 white PNG) whenever it cannot fetch an avatar,
and group-chat workflows deliberately leave extra reference slots unset. All of
these mean "no reference here":

  * empty / whitespace
  * an unsubstituted built-in token: %char_avatar%, %user_avatar%, char_avatar
  * an unsubstituted custom placeholder: %ref3%, %anything%
  * an unexpanded ST macro: {{getvar::foo}}, {{char}}, ... (a custom
    placeholder whose variable is unset expands to nothing or stays literal)
  * the literal strings "undefined" / "null" (what an unset {{getvar::}} can
    produce once JSON.stringify'd through a custom placeholder)
  * ST's PNG_PIXEL, by VALUE — a 1x1 pure-white pixel

The last one is deliberately a value check, not a size check: a real 1x1 avatar
uploaded on purpose is legitimate, so size alone must never trigger this.

Outputs match comfy's LoadImage (IMAGE + MASK) so it feeds ImageScale ->
TextEncodeMOSSImageEdit unchanged.
"""

import os
import io
import re
import base64
import torch
import numpy as np
from PIL import Image, ImageOps

import requests
import folder_paths


# ST's PNG_PIXEL constant, substituted for "%char_avatar%"/"%user_avatar%" when
# the avatar cannot be fetched. MUST match ST's own constant verbatim
# (public/scripts/extensions/stable-diffusion/index.js: `const PNG_PIXEL = ...`).
PNG_PIXEL_B64 = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="

# An unsubstituted ST placeholder: anything wrapped in percent signs.
# Deliberately ONLY the %-wrapped form — matching a bare "..._avatar" word would
# false-positive on legitimate input-dir filenames such as `my_avatar.png`.
_TOKEN_RE = re.compile(r"^%[^%\s]+%$")
# The two bare (percent-less) spellings that appear in older workflow files and
# in ComfyUI's own widget defaults. Exact match only.
_BARE_TOKENS = {"char_avatar", "user_avatar"}
# An unexpanded SillyTavern macro: {{getvar::x}}, {{char}}, {{// comment}}, ...
_MACRO_RE = re.compile(r"\{\{.*?\}\}", re.DOTALL)
# What an unset variable can serialize to.
_NULLISH = {"", "undefined", "null", "none", "nan"}


def _is_missing_token(text: str) -> bool:
    """True when `text` is clearly an unsubstituted placeholder rather than data."""
    t = text.strip()
    if t.lower() in _NULLISH:
        return True
    if t in _BARE_TOKENS:
        return True
    # A macro that never expanded. (A data: URI / URL cannot contain one.)
    if _MACRO_RE.search(t):
        return True
    # %prompt%, %char_avatar%, %ref3%, %any_custom_placeholder%, ...
    return bool(_TOKEN_RE.match(t))


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
    """True when p is ST's PNG_PIXEL placeholder (1x1 pure white)."""
    return p.size == (1, 1) and p.convert("RGB").getpixel((0, 0)) == (255, 255, 255)


def _decode_b64(s: str) -> Image.Image:
    payload = s.strip()
    rem = len(payload) % 4
    if rem:
        payload += "=" * (4 - rem)
    raw = base64.b64decode(payload.replace("-", "/").replace("_", "+"), validate=False)
    return Image.open(io.BytesIO(raw))


class LoadImageFromBase64OrUrl:
    def __init__(self):
        self.output_dir = folder_paths.get_input_directory()

    @classmethod
    def INPUT_TYPES(cls):
        return {
            "required": {
                "image": ("STRING", {"default": "", "multiline": True,
                                     "tooltip": 'Paste base64, a data: URI, an http(s) URL, or use SillyTavern\'s %char_avatar% / %user_avatar% token (percent signs required, and it must be the whole value).'}),
            },
            # OPTIONAL on purpose, and defaulted: a workflow that omits it stays
            # valid against the previously-deployed revision of this node (which
            # only had `image`), so the workflow-side %char_avatar% fix does not
            # have to wait on a node redeploy. ComfyUI applies `default` when the
            # key is absent from API-format JSON.
            "optional": {
                "on_missing": (["transparent_1px", "error"], {
                    "default": "transparent_1px",
                    "tooltip": "What to do when no usable reference arrived. 'transparent_1px' degrades gracefully (Web-UI testing, characters without avatars, unset group-chat slots); 'error' fails the prompt loudly so you notice a missing token."}),
            },
        }

    RETURN_TYPES = ("IMAGE", "MASK")
    RETURN_NAMES = ("image", "mask")
    FUNCTION = "load"
    CATEGORY = "peteches/SillyTavern"

    def _missing(self, why: str, on_missing: str):
        if on_missing == "error":
            raise ValueError(f"No usable reference image: {why}")
        # Inert 1x1 so the graph still executes. NOTE: a 1x1 reference must be
        # upscaled before TextEncodeMOSSImageEdit or its VAE-encode dies with
        # "Kernel size can't be greater than actual input size" — keep the
        # ImageScale node in the workflow.
        print(f"[peteches/SillyTavern] no reference ({why}); using inert 1x1")
        return _pil_to_tensor(_decode_b64(PNG_PIXEL_B64))

    def load(self, image: str, on_missing: str = "transparent_1px"):
        text = (image or "").strip()

        if not text or _is_missing_token(text):
            return self._missing(f"placeholder not substituted or unset: {text[:60]!r}", on_missing)

        if text.startswith("data:image/"):
            try:
                i = _decode_b64(text.split(",", 1)[1])
            except Exception as e:
                return self._missing(f"unparsable data: URI ({e})", on_missing)
            if _is_no_avatar(i):
                return self._missing("data: URI was ST's 1x1 PNG_PIXEL", on_missing)

        elif text.startswith("http://") or text.startswith("https://"):
            resp = requests.get(text, timeout=10)
            resp.raise_for_status()
            i = Image.open(io.BytesIO(resp.content))
            if _is_no_avatar(i):
                return self._missing("URL returned a 1x1 white pixel", on_missing)

        else:
            # Bare base64 (the SillyTavern case); fall back to an input-dir filename.
            try:
                i = _decode_b64(text)
            except Exception:
                path = folder_paths.get_annotated_filepath(text)
                if not path or not os.path.isfile(path):
                    return self._missing(f"not base64, URL, data: URI or input file: {text[:60]!r}", on_missing)
                i = Image.open(path)
            # ST sends PNG_PIXEL bare (no data: prefix) when a character has no
            # avatar — this is the branch that must catch it, and the one the
            # previous revision missed.
            if _is_no_avatar(i):
                return self._missing("ST sent its 1x1 PNG_PIXEL (character has no avatar)", on_missing)

        return _pil_to_tensor(i)


NODE_CLASS_MAPPINGS = {"LoadImageFromBase64OrUrl": LoadImageFromBase64OrUrl}
NODE_DISPLAY_NAME_MAPPINGS = {"LoadImageFromBase64OrUrl": "Load Image From Base64/URL/char_avatar"}
