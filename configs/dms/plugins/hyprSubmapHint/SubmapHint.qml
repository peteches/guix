import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Io
import qs.Common
import qs.Services
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    // The active Hyprland submap as reported by Hyprland IPC.
    property string activeSubmap: ""

    // Derived from `hyprctl -j binds`; this is intentionally not a hand-written
    // keybind table. Each entry is { chord, description, dispatcher, arg }.
    property var activeBindings: []

    // Human-readable diagnostic if hyprctl fails or the JSON shape changes.
    property string errorText: ""

    // Show only while inside a non-default submap.
    readonly property bool hintVisible: activeSubmap !== "" && (activeBindings.length > 0 || errorText !== "")

    // -------------------------------------------------------------------------
    // Hyprland event handling
    // -------------------------------------------------------------------------

    Connections {
        target: Hyprland

        function onRawEvent(event) {
            // Hyprland IPC emits "submap". The Lua event name is
            // "keybinds.submap"; this extra branch is harmless if never seen.
            if (event.name === "submap" || event.name === "keybinds.submap") {
                root.setSubmap(String(event.data || "").trim())
                return
            }

            // Keep the overlay correct if you reload Hyprland while already
            // inside a submap.
            if (event.name === "configreloaded" && root.activeSubmap !== "") {
                root.refreshBinds()
            }
        }
    }

    function setSubmap(name) {
        if (name === "" || name === "reset") {
            activeSubmap = ""
            activeBindings = []
            errorText = ""
            return
        }

        activeSubmap = name
        activeBindings = []
        errorText = ""
        refreshBinds()
    }

    function refreshBinds() {
        if (activeSubmap === "")
            return

        bindsProcess.exec(["hyprctl", "-j", "binds"])
    }

    // -------------------------------------------------------------------------
    // hyprctl parsing
    // -------------------------------------------------------------------------

    Process {
        id: bindsProcess

        stdout: StdioCollector {
            id: bindsStdout
            onStreamFinished: root.parseBinds(text)
        }

        stderr: StdioCollector {
            id: bindsStderr
            onStreamFinished: {
                if (text.trim() !== "")
                    root.errorText = text.trim()
            }
        }

        onExited: function(exitCode, exitStatus) {
            if (exitCode !== 0 && root.errorText === "")
                root.errorText = "hyprctl -j binds exited with code " + exitCode
        }
    }

    function parseBinds(jsonText) {
        if (activeSubmap === "")
            return

        try {
            const binds = JSON.parse(jsonText)
            const entries = []

            for (let i = 0; i < binds.length; i++) {
                const bind = binds[i]
                const bindSubmap = String(bind.submap || "").trim()

                if (bindSubmap !== activeSubmap)
                    continue

                const chord = formatChord(bind)
                if (chord === "")
                    continue

                entries.push({
                    chord: chord,
                    description: formatDescription(bind),
                    dispatcher: String(bind.dispatcher || ""),
                    arg: String(bind.arg || "")
                })
            }

            entries.sort(function(a, b) {
                return a.chord.localeCompare(b.chord)
            })

            activeBindings = entries
            errorText = entries.length === 0
                ? "No binds found for submap: " + activeSubmap
                : ""
        } catch (e) {
            activeBindings = []
            errorText = "Could not parse `hyprctl -j binds`: " + e
        }
    }

    function formatDescription(bind) {
        const description = String(bind.description || "").trim()
        if (description !== "")
            return description

        // Fallback: still show something useful, but do not create a second
        // description table. The authoritative data is still hyprctl's bind row.
        const dispatcher = String(bind.dispatcher || "").trim()
        const arg = String(bind.arg || "").trim()

        if (dispatcher === "")
            return "No description"

        return arg === "" ? dispatcher : dispatcher + " " + arg
    }

    function formatChord(bind) {
        const parts = []

        const mods = formatMods(bind)
        if (mods !== "")
            parts.push(mods)

        let key = String(bind.key || "").trim()
        if (key === "" && bind.keycode !== undefined && bind.keycode !== -1)
            key = "code:" + bind.keycode

        key = normaliseKeyName(key)

        if (key !== "")
            parts.push(key)

        return parts.join("+")
    }

    function formatMods(bind) {
        // Some Hyprland JSON versions expose mods as a string; others expose only
        // modmask. Prefer the literal field if present.
        const literalMods = String(bind.mods || "").trim()
        if (literalMods !== "")
            return literalMods.replace(/ /g, "")

        let mask = Number(bind.modmask || 0)
        if (mask === 0)
            return ""

        // XKB modifier mask bits as used by Hyprland's bind output.
        const mods = []
        if (mask & 1) mods.push("Shift")
        if (mask & 4) mods.push("Ctrl")
        if (mask & 8) mods.push("Alt")
        if (mask & 64) mods.push("Super")
        if (mask & 16) mods.push("Mod2")
        if (mask & 32) mods.push("Mod3")
        if (mask & 128) mods.push("Mod5")

        return mods.join("+")
    }

    function normaliseKeyName(key) {
        if (key === "")
            return ""

        const lower = key.toLowerCase()

        if (lower === "escape") return "Esc"
        if (lower === "return") return "Enter"
        if (lower === "space") return "Space"
        if (lower === "tab") return "Tab"

        if (key.length === 1)
            return key

        return key.charAt(0).toUpperCase() + key.slice(1)
    }

    // -------------------------------------------------------------------------
    // Overlay UI
    // -------------------------------------------------------------------------

    PanelWindow {
        id: hintWindow

        visible: root.hintVisible
        aboveWindows: true
        focusable: false
        color: "transparent"
        exclusiveZone: 0
        exclusionMode: ExclusionMode.Ignore

        anchors {
            top: true
            left: true
            right: true
        }

        margins {
            top: 12
            left: 0
            right: 0
        }

        implicitHeight: hintCard.implicitHeight + 24

        Item {
            anchors.fill: parent

            Rectangle {
                id: hintCard

                anchors.top: parent.top
                anchors.horizontalCenter: parent.horizontalCenter
                width: Math.min(parent.width - 32, hintContent.implicitWidth + 32)
                implicitHeight: hintContent.implicitHeight + 20
                radius: Theme.cornerRadiusLarge
                color: Theme.withAlpha(Theme.surfaceContainerHigh, 0.96)
                border.color: Theme.outlineVariant
                border.width: 1

                ColumnLayout {
                    id: hintContent

                    anchors.fill: parent
                    anchors.margins: 10
                    spacing: Theme.spacingS

                    RowLayout {
                        Layout.alignment: Qt.AlignHCenter
                        spacing: Theme.spacingS

                        DankIcon {
                            name: "keyboard"
                            size: Theme.iconSize
                            color: Theme.primary
                            Layout.alignment: Qt.AlignVCenter
                        }

                        StyledText {
                            text: root.activeSubmap
                            color: Theme.primary
                            font.pixelSize: Theme.fontSizeMedium
                            font.weight: Font.Bold
                            Layout.alignment: Qt.AlignVCenter
                        }
                    }

                    Flow {
                        id: bindFlow
                        visible: root.errorText === ""
                        Layout.maximumWidth: hintWindow.width - 64
                        Layout.alignment: Qt.AlignHCenter
                        spacing: Theme.spacingS

                        Repeater {
                            model: root.activeBindings

                            delegate: Rectangle {
                                required property var modelData

                                radius: Theme.cornerRadius
                                color: Theme.withAlpha(Theme.surfaceContainerHighest, 0.92)
                                border.color: Theme.outlineVariant
                                border.width: 1
                                implicitWidth: bindRow.implicitWidth + 14
                                implicitHeight: bindRow.implicitHeight + 8

                                RowLayout {
                                    id: bindRow
                                    anchors.centerIn: parent
                                    spacing: Theme.spacingXS

                                    StyledText {
                                        text: modelData.chord
                                        color: Theme.primary
                                        font.family: "monospace"
                                        font.pixelSize: Theme.fontSizeSmall
                                        font.weight: Font.Bold
                                    }

                                    StyledText {
                                        text: "—"
                                        color: Theme.surfaceVariantText
                                        font.pixelSize: Theme.fontSizeSmall
                                    }

                                    StyledText {
                                        text: modelData.description
                                        color: Theme.surfaceText
                                        font.pixelSize: Theme.fontSizeSmall
                                    }
                                }
                            }
                        }
                    }

                    StyledText {
                        visible: root.errorText !== ""
                        text: root.errorText
                        color: Theme.error
                        font.pixelSize: Theme.fontSizeSmall
                        wrapMode: Text.WordWrap
                        Layout.maximumWidth: hintWindow.width - 64
                        Layout.alignment: Qt.AlignHCenter
                    }
                }
            }
        }
    }

    Component.onCompleted: {
        console.info("submapHint plugin loaded; waiting for Hyprland submap events")
    }
}
