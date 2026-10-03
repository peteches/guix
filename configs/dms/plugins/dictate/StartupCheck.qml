import QtQuick
import qs.Common

// Dependency gate for the Dictate plugin.  Without it a missing `rec`,
// `websockets` or Deepgram key only shows up as a dead panel after you press
// the keybind; with it DMS blocks activation and toasts the actual reason.
//
// The key itself is never printed — only its absence is reported.
QtObject {
    readonly property string probe: `
        missing=""
        command -v rec >/dev/null 2>&1 || missing="$missing rec (sox)"
        command -v python3 >/dev/null 2>&1 || missing="$missing python3"
        python3 -c 'import websockets' >/dev/null 2>&1 || missing="$missing python-websockets"
        key_file="\${XDG_CONFIG_HOME:-$HOME/.config}/DankMaterialShell/dictate/deepgram-api-key"
        if [ -z "\${DEEPGRAM_API_KEY:-}" ] && [ ! -s /run/secrets/deepgram-api-key ] && [ ! -s "$key_file" ]; then
            missing="$missing Deepgram API key (set DEEPGRAM_API_KEY, or write it to /run/secrets/deepgram-api-key or $key_file)"
        fi
        if [ -n "$missing" ]; then
            printf '%s\\n' "$missing"
            exit 1
        fi
        exit 0
    `

    function check(done) {
        Proc.runCommand("dictate.startupCheck", ["sh", "-c", probe], (stdout, exitCode) => {
            if (exitCode === 0) {
                done(null);
                return;
            }
            done({
                "title": "Dictate cannot start: missing dependencies",
                "details": String(stdout).trim()
            });
        });
    }
}
