"""peteches_sillytavern_nodes — SillyTavern-facing ComfyUI nodes.

ComfyUI reads NODE_CLASS_MAPPINGS / NODE_DISPLAY_NAME_MAPPINGS off this package,
so the per-module dicts must be MERGED here. A second `from .x import *` would
silently overwrite the first module's mappings and unregister its nodes — the
classic "only the last node pack module loads" bug. Hence the explicit update()
below; add new modules the same way rather than star-importing them.
"""

from . import moss_avatar_loader
from . import reference_sheet

_MODULES = (moss_avatar_loader, reference_sheet)

NODE_CLASS_MAPPINGS = {}
NODE_DISPLAY_NAME_MAPPINGS = {}

for _m in _MODULES:
    NODE_CLASS_MAPPINGS.update(getattr(_m, "NODE_CLASS_MAPPINGS", {}))
    NODE_DISPLAY_NAME_MAPPINGS.update(getattr(_m, "NODE_DISPLAY_NAME_MAPPINGS", {}))

__all__ = ["NODE_CLASS_MAPPINGS", "NODE_DISPLAY_NAME_MAPPINGS"]
