# submapHint

DMS / Quickshell plugin for Hyprland submaps.

It listens for Hyprland's `submap` IPC event. When the active submap changes, it runs:

```sh
hyprctl -j binds
```

Then it filters the live bind list by the active submap and displays each bind's key chord with its Hyprland bind description.

It intentionally does **not** maintain a second QML table of keybinds or descriptions. Your Hyprland config remains the source of truth.

## Install for live development

```sh
mkdir -p ~/.config/DankMaterialShell/plugins
ln -sfn /path/to/submapHint ~/.config/DankMaterialShell/plugins/submapHint

dms ipc call plugins reload submapHint
# or open DMS Settings -> Plugins -> Scan for Plugins -> enable submapHint
```

## Test

1. Ensure your Hyprland binds include descriptions.
2. Enter a submap, for example `SUPER + Space` if you used the command-mode config.
3. The overlay should appear at the top of the screen.
4. Exit the submap; the overlay should hide.

## Debug

Run DMS in the foreground:

```sh
dms kill
dms run
```

Then watch for logs from `submapHint` and any QML errors.

You can also inspect what the plugin sees:

```sh
hyprctl -j binds | jq '.[] | {submap, key, modmask, dispatcher, arg, description}'
```

`jq` is only for debugging; the plugin itself does not require it.

## Notes

- Hyprland's IPC event name is `submap`; empty data means the default submap.
- This plugin also accepts `keybinds.submap` in case you forward Lua-style event names into Quickshell later.
- If `description` is empty, the plugin falls back to showing the dispatcher and argument from the same `hyprctl` bind row.
