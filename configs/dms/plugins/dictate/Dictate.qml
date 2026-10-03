import QtQuick
import QtQuick.Layouts
import QtQuick.Controls as QQC2
import Quickshell
import Quickshell.Io
import qs.Common
import qs.Services
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    // ── State ──────────────────────────────────────────────────────────────
    // idle      → not recording, no results
    // recording → actively streaming audio to Deepgram
    // stopping  → sent CloseStream, waiting for final flush
    // result    → transcription complete, showing editable text
    property string state: "idle"

    // Accumulated transcript fragments (each "final" utterance).
    property var fragments: []

    // The full combined text shown in the editor.
    property string fullText: ""

    // Live partial hypothesis from Deepgram (interim_results=true): shown
    // while recording and replaced by the final utterance on endpointing.
    property string interimText: ""

    // What the editor displays: committed finals plus the in-flight partial.
    readonly property string displayText: {
        if (interimText === "")
            return fullText
        return fullText === "" ? interimText : fullText + " " + interimText
    }

    // TTS playback state.
    property bool ttsPlaying: false

    // Error message if something went wrong.
    property string errorMessage: ""

    // Audio meter level (0..1), updated by the stream process's stderr or
    // derived from transcript timing. Simplified: we pulse it on each
    // transcript fragment arrival.
    property real meterLevel: 0.0

    // Last few stderr lines from dictate-stream.py, kept so an unexpected
    // crash can be reported in the panel instead of vanishing.
    property string stderrTail: ""

    // Absolute path of this plugin's directory, resolved through the DMS
    // PluginService (PluginComponent is instantiated with pluginId and
    // pluginService set).  Used to locate dictate-stream.py: Qt.resolvedUrl()
    // would hand python3 a "file://…" URL it cannot open.
    readonly property string pluginDir: (pluginService && pluginId)
        ? String(pluginService.getPluginPath(pluginId))
        : ""

    // ── Helpers ────────────────────────────────────────────────────────────

    function reset() {
        state = "idle"
        fragments = []
        fullText = ""
        interimText = ""
        stderrTail = ""
        ttsPlaying = false
        errorMessage = ""
        meterLevel = 0.0
        // Editing the transcript in the "result" state assigns TextArea.text
        // imperatively, which breaks its binding.  Restore it, otherwise the
        // next recording would display the previous session's text.
        try {
            textEdit.text = Qt.binding(function () {
                return root.displayText;
            });
        } catch (e) {
            // textEdit not instantiated yet (component still loading)
        }
    }

    function startRecording() {
        if (pluginDir === "") {
            errorMessage = "Could not resolve the Dictate plugin directory"
            return
        }
        reset()
        state = "recording"
        dictateProcess.running = true
    }

    function stopRecording() {
        if (state !== "recording") return
        state = "stopping"
        dictateProcess.write("stop\n")
    }

    function cancelRecording() {
        dictateProcess.write("cancel\n")
        dictateProcess.running = false
        reset()
    }

    // One JSON line from dictate-stream.py.
    function handleLine(line) {
        var msg
        try {
            msg = JSON.parse(line)
        } catch (e) {
            return
        }

        if (msg.type === "ready") {
            // Recording started successfully.
            meterPulse.start()
        } else if (msg.type === "transcript") {
            root.fragments = root.fragments.concat([msg.text])
            root.fullText = root.fragments.join(" ")
            root.interimText = ""
            // Pulse the meter on each new fragment.
            root.meterLevel = 0.8
        } else if (msg.type === "interim") {
            // Partial hypothesis — live feedback while still speaking.
            root.interimText = msg.text
            root.meterLevel = 0.6
        } else if (msg.type === "stopped") {
            meterPulse.stop()
            root.meterLevel = 0.0
            root.interimText = ""
            if (root.state === "stopping") {
                root.state = root.fullText !== "" ? "result" : "idle"
            }
        } else if (msg.type === "final") {
            // Full combined text from the helper.
            if (msg.text && msg.text !== "") {
                root.fullText = msg.text
            }
        } else if (msg.type === "error") {
            meterPulse.stop()
            root.errorMessage = msg.message || "Unknown error"
            root.state = "idle"
            dictateProcess.running = false
        }
    }

    function copyToClipboard() {
        if (fullText === "") return
        clipboardProcess.exec(["wl-copy", "--", fullText])
    }

    function speakText() {
        if (fullText === "" || ttsPlaying) return
        ttsPlaying = true
        ttsProcess.exec(["espeak-ng", "-s", "150", fullText])
    }

    function stopTts() {
        ttsProcess.running = false
        ttsPlaying = false
    }

    // ── Dictate stream process ─────────────────────────────────────────────
    // Runs dictate-stream.py which spawns rec + Deepgram WebSocket.
    // Communicates via JSON lines on stdout.

    Process {
        id: dictateProcess

        command: ["python3", root.pluginDir + "/dictate-stream.py"]

        // Quickshell has no StdioPipe type: stdin is turned on with this flag
        // and written to via Process.write().
        stdinEnabled: true

        // Line splitting is SplitParser (splitMarker), not
        // StdioCollector.splitMode; the per-line signal is `read`.
        stdout: SplitParser {
            splitMarker: "\n"
            onRead: line => root.handleLine(line)
        }

        // Python tracebacks land here.  Previously dropped entirely, which is
        // why a broken stream looked like an infinitely "Listening…" panel.
        stderr: SplitParser {
            splitMarker: "\n"
            onRead: line => {
                var text = String(line).trim()
                if (text === "")
                    return
                console.warn("Dictate stderr:", text)
                root.stderrTail = (root.stderrTail + "\n" + text).split("\n").slice(-4).join("\n")
            }
        }

        onExited: function(exitCode, exitStatus) {
            meterPulse.stop()
            root.meterLevel = 0.0
            if (root.state === "recording" || root.state === "stopping") {
                root.interimText = ""
                if (root.fullText !== "") {
                    root.state = "result"
                } else if (root.errorMessage === "") {
                    root.errorMessage = "dictate-stream.py exited (code " + exitCode + ")"
                            + (root.stderrTail !== "" ? ":\n" + root.stderrTail : "")
                    root.state = "idle"
                }
            }
        }
    }

    // Meter pulse animation — decays after each transcript fragment.
    Timer {
        id: meterPulse
        interval: 100
        repeat: true
        onTriggered: {
            root.meterLevel = Math.max(0, root.meterLevel - 0.1)
        }
    }

    // ── TTS process ────────────────────────────────────────────────────────

    Process {
        id: ttsProcess
        onExited: function() {
            root.ttsPlaying = false
        }
    }

    // ── Clipboard process ──────────────────────────────────────────────────

    Process {
        id: clipboardProcess
    }

    // ── Hyprland keybind integration ───────────────────────────────────────
    // The Hyprland keybind (SUPER+m) writes a nanosecond timestamp to a
    // signal file.  One long-lived `tail -F` watcher streams those lines back
    // here, so there is no polling and no per-tick process spawn.

    readonly property string signalFile: (Quickshell.env("XDG_RUNTIME_DIR") || "/tmp") + "/dictate-toggle"
    property string lastSignal: ""

    Process {
        id: signalWatcher

        // Create the file if the keybind has never run, then follow it.
        // `tail -n0` ignores any pre-existing content, so a stale timestamp
        // from a previous session cannot trigger dictation at login.
        command: ["sh", "-c", "f=\"$1\"; [ -e \"$f\" ] || : > \"$f\"; exec tail -n0 -F \"$f\"", "--", root.signalFile]
        running: true

        stdout: SplitParser {
            splitMarker: "\n"
            onRead: line => {
                var content = String(line).trim()
                if (content === "" || content === root.lastSignal)
                    return
                root.lastSignal = content
                root.toggleDictation()
            }
        }
    }

    function toggleDictation() {
        if (root.state === "idle") {
            root.startRecording()
        } else if (root.state === "recording") {
            root.stopRecording()
        } else if (root.state === "result") {
            root.reset()
        }
    }

    // ── UI ─────────────────────────────────────────────────────────────────

    PanelWindow {
        id: dictateWindow

        visible: root.state !== "idle"
        aboveWindows: true
        focusable: true
        color: "transparent"
        exclusiveZone: 0
        exclusionMode: ExclusionMode.Ignore

        anchors {
            top: true
            left: true
            right: true
        }

        margins {
            top: 60
            left: 0
            right: 0
        }

        implicitHeight: contentCard.implicitHeight + 24

        Item {
            anchors.fill: parent

            Rectangle {
                id: contentCard

                anchors.top: parent.top
                anchors.horizontalCenter: parent.horizontalCenter
                width: Math.min(parent.width - 32, 580)
                implicitHeight: contentLayout.implicitHeight + 24
                radius: Theme.cornerRadius
                color: Theme.withAlpha(Theme.surfaceContainerHigh, 0.97)
                border.color: root.state === "recording" ? Theme.primary : Theme.outlineVariant
                border.width: root.state === "recording" ? 2 : 1

                ColumnLayout {
                    id: contentLayout

                    anchors.fill: parent
                    anchors.margins: 12
                    spacing: Theme.spacingS

                    // ── Header row ─────────────────────────────────────
                    RowLayout {
                        Layout.fillWidth: true
                        spacing: Theme.spacingS

                        DankIcon {
                            name: root.state === "recording" ? "mic" :
                                  root.state === "stopping" ? "hourglass_empty" :
                                  root.state === "result" ? "description" : "mic_off"
                            size: Theme.iconSize
                            color: root.state === "recording" ? Theme.error : Theme.primary
                        }

                        StyledText {
                            text: root.state === "recording" ? "Listening…" :
                                  root.state === "stopping" ? "Finalizing…" :
                                  root.state === "result" ? "Transcription" : "Dictate"
                            color: Theme.surfaceText
                            font.pixelSize: Theme.fontSizeLarge
                            font.weight: Font.Bold
                            Layout.fillWidth: true
                        }

                        // Stop / Cancel buttons during recording.
                        QQC2.Button {
                            visible: root.state === "recording" || root.state === "stopping"
                            text: "Stop"
                            onClicked: root.stopRecording()
                            enabled: root.state === "recording"
                        }

                        QQC2.Button {
                            visible: root.state === "recording"
                            text: "Cancel"
                            onClicked: root.cancelRecording()
                        }
                    }

                    // ── Audio meter ────────────────────────────────────
                    Rectangle {
                        visible: root.state === "recording"
                        Layout.fillWidth: true
                        height: 4
                        radius: 2
                        color: Theme.surfaceContainerHighest

                        Rectangle {
                            width: parent.width * root.meterLevel
                            height: parent.height
                            radius: parent.radius
                            color: Theme.primary
                            Behavior on width { NumberAnimation { duration: 80 } }
                        }
                    }

                    // ── Error display ──────────────────────────────────
                    StyledText {
                        visible: root.errorMessage !== ""
                        text: root.errorMessage
                        color: Theme.error
                        font.pixelSize: Theme.fontSizeMedium
                        wrapMode: Text.WordWrap
                        Layout.fillWidth: true
                    }

                    // ── Transcript editor ──────────────────────────────
                    Rectangle {
                        visible: root.state === "result" || root.state === "recording" || root.state === "stopping"
                        Layout.fillWidth: true
                        implicitHeight: Math.max(80, Math.min(200, textEdit.contentHeight + 16))
                        radius: Theme.cornerRadius
                        color: Theme.surfaceContainerHighest
                        border.color: Theme.outlineVariant
                        border.width: 1

                        Flickable {
                            anchors.fill: parent
                            anchors.margins: 8
                            contentHeight: textEdit.contentHeight
                            clip: true
                            flickableDirection: Flickable.VerticalFlick

                            QQC2.TextArea {
                                id: textEdit
                                width: parent.width
                                text: root.displayText
                                color: Theme.surfaceText
                                font.pixelSize: Theme.fontSizeMedium
                                font.family: "monospace"
                                wrapMode: Text.WordWrap
                                readOnly: root.state !== "result"
                                background: null
                                selectByMouse: true

                                onTextChanged: {
                                    if (root.state === "result") {
                                        root.fullText = text
                                    }
                                }
                            }
                        }
                    }

                    // ── Action buttons (result state) ──────────────────
                    RowLayout {
                        visible: root.state === "result"
                        Layout.fillWidth: true
                        Layout.alignment: Qt.AlignHCenter
                        spacing: Theme.spacingM

                        QQC2.Button {
                            text: root.ttsPlaying ? "■ Stop TTS" : "🔊 Read Aloud"
                            onClicked: {
                                if (root.ttsPlaying) {
                                    root.stopTts()
                                } else {
                                    root.speakText()
                                }
                            }
                        }

                        QQC2.Button {
                            text: "📋 Copy"
                            onClicked: root.copyToClipboard()
                        }

                        QQC2.Button {
                            text: "✕ Close"
                            onClicked: root.reset()
                        }
                    }
                }
            }
        }
    }

    Component.onCompleted: {
        console.info("Dictate plugin loaded, watching " + root.signalFile)
    }
}
