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

    // TTS playback state.
    property bool ttsPlaying: false

    // Error message if something went wrong.
    property string errorMessage: ""

    // Audio meter level (0..1), updated by the stream process's stderr or
    // derived from transcript timing. Simplified: we pulse it on each
    // transcript fragment arrival.
    property real meterLevel: 0.0

    // ── Helpers ────────────────────────────────────────────────────────────

    function reset() {
        state = "idle"
        fragments = []
        fullText = ""
        ttsPlaying = false
        errorMessage = ""
        meterLevel = 0.0
    }

    function startRecording() {
        reset()
        state = "recording"
        dictateProcess.running = true
    }

    function stopRecording() {
        if (state !== "recording") return
        state = "stopping"
        dictateProcess.stdin.write("stop\n")
    }

    function cancelRecording() {
        dictateProcess.stdin.write("cancel\n")
        dictateProcess.running = false
        reset()
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

        command: ["python3", Qt.resolvedUrl("./dictate-stream.py")]

        stdin: StdioPipe {}

        stdout: StdioCollector {
            splitMode: StdioCollector.SplitMode.NewLine
            onLine: function(line) {
                try {
                    var msg = JSON.parse(line)
                } catch(e) {
                    return
                }

                if (msg.type === "ready") {
                    // Recording started successfully.
                    meterPulse.start()
                } else if (msg.type === "transcript") {
                    root.fragments = root.fragments.concat([msg.text])
                    root.fullText = root.fragments.join(" ")
                    // Pulse the meter on each new fragment.
                    root.meterLevel = 0.8
                } else if (msg.type === "stopped") {
                    meterPulse.stop()
                    root.meterLevel = 0.0
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
        }

        onExited: function(exitCode, exitStatus) {
            meterPulse.stop()
            root.meterLevel = 0.0
            if (root.state === "recording" || root.state === "stopping") {
                if (root.fullText !== "") {
                    root.state = "result"
                } else if (root.errorMessage === "") {
                    root.errorMessage = "Process exited unexpectedly (code " + exitCode + ")"
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
    // signal file. This timer polls the file every 250ms and toggles
    // dictation when the content changes. Simple, reliable, no extra deps.

    property string signalFile: (Quickshell.env("XDG_RUNTIME_DIR") || "/tmp") + "/dictate-toggle"
    property string lastSignal: ""

    Timer {
        id: signalPoller
        interval: 250
        repeat: true
        running: true
        onTriggered: readSignal.running = true
    }

    Process {
        id: readSignal
        command: ["cat", root.signalFile]

        stdout: StdioCollector {
            onStreamFinished: {
                var content = text.trim()
                if (content !== "" && content !== root.lastSignal) {
                    root.lastSignal = content
                    root.toggleDictation()
                }
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
                radius: Theme.cornerRadiusLarge
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
                                text: root.fullText
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
        console.info("Dictate plugin loaded")
    }
}
