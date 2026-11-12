#!/usr/bin/env python3
"""Tests for the peteches_sillytavern_nodes pack — no ComfyUI required.

Run with:

    cd configs/sillytavern/comfy
    guix shell -C python python-numpy python-pillow python-requests -- \
        python3 custom_nodes/test_nodes.py

`torch` and `folder_paths` are stubbed, so this exercises the pure image logic
(blank detection, tiling, cover-crop, token parsing) without a GPU or a ComfyUI
checkout. It does NOT prove the nodes execute inside ComfyUI — that still needs
the runtime checks in ../README.org's test plan.

These tests exist because an integration gap caused a real bug: the unit tests
for the sheet node passed while the loader→sheet chain was broken, because
SillyTavern's PNG_PIXEL is a 1x1 fully TRANSPARENT BLACK pixel (mode LA, value
(0, 0)) and both nodes tested for pure WHITE. Every "missing reference"
therefore survived as a black panel and a solo scene was tiled into a 2x2 sheet
of one avatar plus three black squares. `test_end_to_end_*` below are the
regression tests for exactly that.
"""

import base64
import io
import sys
import types

import numpy as np
from PIL import Image


# ── stubs for the ComfyUI-only imports ───────────────────────────────────────
class FakeTensor:
    """Just enough of torch.Tensor for _pil_to_tensor / _tensor_to_pil."""

    def __init__(self, a):
        self.a = np.asarray(a)

    def dim(self):
        return self.a.ndim

    def numel(self):
        return self.a.size

    def detach(self):
        return self

    def cpu(self):
        return self

    def numpy(self):
        return self.a

    def __getitem__(self, i):
        return FakeTensor(self.a[i])

    @property
    def shape(self):
        return self.a.shape


def _install_stubs():
    if "torch" not in sys.modules:
        t = types.ModuleType("torch")
        t.Tensor = FakeTensor
        t.from_numpy = lambda a: FakeTensor(a)
        t.zeros = lambda shape, dtype=None: FakeTensor(np.zeros(shape, dtype=np.float32))
        t.float32 = np.float32
        sys.modules["torch"] = t
    if "folder_paths" not in sys.modules:
        fp = types.ModuleType("folder_paths")
        fp.get_input_directory = lambda: "/tmp"
        fp.get_annotated_filepath = lambda p: p
        sys.modules["folder_paths"] = fp


_install_stubs()

# Import the pack under whatever its directory is actually called: `custom_nodes`
# in this repo, `peteches_sillytavern_nodes` once deployed. Going through the real
# __init__.py is the point — it is what proves the NODE_CLASS_MAPPINGS merge
# works instead of one module's star-import clobbering the other's.
import importlib  # noqa: E402
import os  # noqa: E402

_HERE = os.path.dirname(os.path.abspath(__file__))
_PARENT, _PKG = os.path.dirname(_HERE), os.path.basename(_HERE)
if _PARENT not in sys.path:
    sys.path.insert(0, _PARENT)
pack = importlib.import_module(_PKG)  # noqa: E402
_mod = lambda name: importlib.import_module(f"{_PKG}.{name}")  # noqa: E731

LOADER = pack.NODE_CLASS_MAPPINGS["LoadImageFromBase64OrUrl"]()
SHEET = pack.NODE_CLASS_MAPPINGS["SillyTavernReferenceSheet"]()

# ST's PNG_PIXEL, copied from index.js:71. Decodes to mode LA, value (0, 0).
PNG_PIXEL = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="

_failures = []


def check(label, cond, extra=""):
    if not cond:
        _failures.append(label)
    print(("ok   " if cond else "FAIL ") + label + (("  " + extra) if extra else ""))


def png_b64(w, h, colour):
    buf = io.BytesIO()
    Image.new("RGB", (w, h), colour).save(buf, format="PNG")
    return base64.b64encode(buf.getvalue()).decode()


def photo(w, h):
    a = np.random.default_rng(0).integers(0, 255, (h, w, 3), dtype=np.uint8)
    return Image.fromarray(a)


def T(pil):
    """PIL -> [1,H,W,C] fake tensor, i.e. ComfyUI's IMAGE convention."""
    return FakeTensor((np.asarray(pil.convert("RGB"), dtype=np.float32) / 255.0)[None,])


# ── 1. node registration ─────────────────────────────────────────────────────
def test_registration():
    print("\n== node registration ==")
    check("both nodes registered (mappings MERGED, not star-import clobbered)",
          set(pack.NODE_CLASS_MAPPINGS) == {"LoadImageFromBase64OrUrl", "SillyTavernReferenceSheet"},
          str(sorted(pack.NODE_CLASS_MAPPINGS)))
    check("display names cover the same set",
          set(pack.NODE_DISPLAY_NAME_MAPPINGS) == set(pack.NODE_CLASS_MAPPINGS))


# ── 2. loader: "missing" detection ───────────────────────────────────────────
def test_loader_missing_detection():
    print("\n== loader: what each SillyTavern-side state produces ==")
    real = png_b64(128, 128, (200, 30, 40))
    # (label, widget value, expect a usable reference)
    cases = [
        ("SOLO: %char_avatar% substituted", real, True),
        ("unset custom placeholder %ref2%", "%ref2%", False),
        ("unsubstituted built-in %char_avatar%", "%char_avatar%", False),
        ("legacy percent-less token", "char_avatar", False),
        ("character has no avatar -> PNG_PIXEL", PNG_PIXEL, False),
        ("empty widget", "", False),
        ("unset {{getvar::x}} -> 'undefined'", "undefined", False),
        ("unexpanded macro", "{{getvar::beth_b64}}", False),
    ]
    for label, value, want_usable in cases:
        img, _ = LOADER.load(image=value)
        usable = img.shape[1] > 1  # the placeholder is 1x1
        check(f"{label:38} -> {img.shape[2]}x{img.shape[1]} usable={usable}",
              usable == want_usable)


def test_loader_does_not_misfire_on_filenames():
    print("\n== loader: real filenames must NOT look like tokens ==")
    m = _mod("moss_avatar_loader")
    for name in ["my_avatar.png", "characters/amy_avatar.png", "avatar_front.png"]:
        check(f"{name!r} is not treated as an unsubstituted token",
              not m._is_missing_token(name))
    for tok in ["%ref3%", "%prompt%", "char_avatar", "user_avatar"]:
        check(f"{tok!r} IS treated as unsubstituted", m._is_missing_token(tok))


# ── 3. sheet: blank detection ────────────────────────────────────────────────
def test_is_blank():
    print("\n== sheet: _is_blank ==")
    rs = _mod("reference_sheet")
    check("1x1 white -> blank", rs._is_blank(Image.new("RGB", (1, 1), (255, 255, 255))))
    # THE REGRESSION: PNG_PIXEL is transparent BLACK, so a colour test fails.
    check("1x1 non-white -> blank (by SIZE, not colour)",
          rs._is_blank(Image.new("RGB", (1, 1), (10, 200, 30))))
    check("ST's actual PNG_PIXEL -> blank",
          rs._is_blank(Image.open(io.BytesIO(base64.b64decode(PNG_PIXEL))).convert("RGB")))
    check("solid 1024 white -> blank", rs._is_blank(Image.new("RGB", (1024, 1024), (255, 255, 255))))
    check("solid 512 black -> blank (catches an upscaled 1x1)",
          rs._is_blank(Image.new("RGB", (512, 512), (0, 0, 0))))
    check("real photo -> not blank", not rs._is_blank(photo(400, 600)))


# ── 4. sheet: geometry ───────────────────────────────────────────────────────
def test_cover_crop():
    print("\n== sheet: _cover_crop geometry ==")
    rs = _mod("reference_sheet")
    for (sw, sh), (cw, ch) in [((400, 600), (512, 1024)), ((1000, 100), (512, 512)),
                               ((10, 10), (512, 1024)), ((1024, 1024), (512, 1024))]:
        out = rs._cover_crop(photo(sw, sh), cw, ch)
        check(f"{sw}x{sh} -> {cw}x{ch}", out.size == (cw, ch), f"got {out.size}")


def test_layout_and_size():
    print("\n== sheet: layout selection + output is always square ==")
    for n in (1, 2, 3, 4):
        kw = {f"image_{i+1}": T(photo(400, 600)) for i in range(n)}
        sheet, count = SHEET.build(target_size=1024, **kw)
        check(f"{n} ref(s): count=={n}", count == n, f"got {count}")
        check(f"{n} ref(s): output 1024x1024", sheet.shape == (1, 1024, 1024, 3), f"got {sheet.shape}")
    sheet, _ = SHEET.build(target_size=832, image_1=T(photo(400, 600)),
                           image_2=T(photo(300, 500)), image_3=T(photo(500, 500)))
    check("3 refs at odd size 832 -> square 832x832", sheet.shape == (1, 832, 832, 3), f"got {sheet.shape}")


# ── 5. end-to-end: the chain SillyTavern actually drives ──────────────────────
def test_end_to_end_solo():
    print("\n== end-to-end: SOLO scene (only %char_avatar% set, ref2/3/4 unset) ==")
    real = png_b64(128, 128, (200, 30, 40))
    imgs = [LOADER.load(image=v)[0] for v in [real, "%ref2%", "%ref3%", "%ref4%"]]
    sheet, count = SHEET.build(target_size=1024, image_1=imgs[0], image_2=imgs[1],
                               image_3=imgs[2], image_4=imgs[3])
    check("exactly 1 reference survives", count == 1, f"got {count}")
    check("sheet is 1024x1024", sheet.shape == (1, 1024, 1024, 3), f"got {sheet.shape}")
    # The property that makes one workflow fit both solo and group scenes:
    solo, _ = SHEET.build(target_size=1024, image_1=imgs[0])
    check("4-slot solo output is PIXEL-IDENTICAL to a dedicated 1-slot graph",
          np.array_equal(sheet.numpy(), solo.numpy()),
          f"maxdiff={np.abs(sheet.numpy() - solo.numpy()).max():.4f}")


def test_end_to_end_group():
    print("\n== end-to-end: GROUP scene (2 real refs, 2 unset) ==")
    a, b = png_b64(128, 128, (200, 30, 40)), png_b64(96, 160, (30, 60, 200))
    imgs = [LOADER.load(image=v)[0] for v in [a, b, "%ref3%", "%ref4%"]]
    sheet, count = SHEET.build(target_size=1024, image_1=imgs[0], image_2=imgs[1],
                               image_3=imgs[2], image_4=imgs[3])
    check("exactly 2 references survive", count == 2, f"got {count}")
    check("sheet is 1024x1024", sheet.shape == (1, 1024, 1024, 3), f"got {sheet.shape}")
    # both characters' dominant colours must be present somewhere in the sheet
    px = (sheet.numpy()[0] * 255).round().astype(np.uint8).reshape(-1, 3)
    check("ref 1 colour present in sheet", bool((np.abs(px - np.array([200, 30, 40])).sum(1) < 12).any()))
    check("ref 2 colour present in sheet", bool((np.abs(px - np.array([30, 60, 200])).sum(1) < 12).any()))


def test_end_to_end_no_avatar():
    print("\n== end-to-end: character with NO avatar at all ==")
    sheet, count = SHEET.build(target_size=1024,
                               image_1=LOADER.load(image=PNG_PIXEL)[0],
                               image_2=LOADER.load(image="%ref2%")[0])
    check("0 references", count == 0, f"got {count}")
    check("full-size neutral sheet, NOT 1x1 (a 1x1 crashes the VAE encode)",
          sheet.shape == (1, 1024, 1024, 3), f"got {sheet.shape}")


if __name__ == "__main__":
    for fn in [test_registration, test_loader_missing_detection,
               test_loader_does_not_misfire_on_filenames, test_is_blank,
               test_cover_crop, test_layout_and_size, test_end_to_end_solo,
               test_end_to_end_group, test_end_to_end_no_avatar]:
        fn()
    print("\n" + ("=" * 60))
    if _failures:
        print(f"{len(_failures)} FAILURE(S):")
        for f in _failures:
            print("  - " + f)
        sys.exit(1)
    print("ALL PASS")
