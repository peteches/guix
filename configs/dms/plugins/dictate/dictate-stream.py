#!/usr/bin/env python3
"""dictate-stream — audio capture + Deepgram Nova-3 streaming STT.

Spawned by the DMS Dictate plugin (QML). Communicates via JSON lines on
stdout and accepts commands on stdin.

Protocol (stdout → QML):
  {"type": "ready"}                     — WebSocket open, recording started
  {"type": "transcript", "text": "..."}  — final utterance from Deepgram
  {"type": "error", "message": "..."}    — fatal error, process will exit
  {"type": "stopped"}                    — recording stopped, flushing finals

Protocol (stdin → script):
  stop     — stop recording, flush remaining transcripts, exit cleanly
  cancel   — discard all transcripts and exit immediately

Requires:
  - sox (provides `rec`) in PATH
  - DEEPGRAM_API_KEY environment variable
  - python3 with websockets (pip install websockets, or Guix python-websockets)
"""

import asyncio
import json
import os
import signal
import subprocess
import sys
import threading
from typing import Optional

try:
    import websockets
except ImportError:
    print(json.dumps({"type": "error", "message": "python-websockets not installed"}), flush=True)
    sys.exit(1)

DG_URL = (
    "wss://api.deepgram.com/v1/listen"
    "?model=nova-3"
    "&encoding=linear16"
    "&sample_rate=16000"
    "&channels=1"
    "&interim_results=true"
    "&smart_format=true"
    "&punctuate=true"
    "&endpointing=300"
)


def emit(msg: dict) -> None:
    """Write a JSON line to stdout."""
    print(json.dumps(msg), flush=True)


# Key lookup order.  The env var covers claude-workstation (where sops
# exports it into the shell); the files cover machines where the DMS shell
# has no such variable in its environment.
KEY_FILE_CANDIDATES = (
    "/run/secrets/deepgram-api-key",
    os.path.join(
        os.environ.get("XDG_CONFIG_HOME", os.path.expanduser("~/.config")),
        "DankMaterialShell", "dictate", "deepgram-api-key",
    ),
)


def resolve_api_key() -> str:
    """Return the Deepgram API key from the environment or a key file."""
    key = os.environ.get("DEEPGRAM_API_KEY", "").strip()
    if key:
        return key
    for path in KEY_FILE_CANDIDATES:
        try:
            with open(path, encoding="utf-8") as fh:
                key = fh.read().strip()
        except OSError:
            continue
        if key:
            return key
    return ""


# Overall deadline for flush + close after "stop", so a silent socket cannot
# leave the plugin stuck showing "Finalizing…".
FLUSH_TIMEOUT_S = 4.0


def _ws_is_open(ws) -> bool:
    """True if the websocket can still be written to.

    websockets >= 14 dropped the legacy ``.open`` attribute in favour of a
    ``.state`` enum; Guix currently ships 16.0, where ``ws.open`` raises
    AttributeError.  Support both so this keeps working either way.
    """
    if ws is None:
        return False
    state = getattr(ws, "state", None)
    if state is not None:
        return getattr(state, "name", str(state)) == "OPEN"
    return bool(getattr(ws, "open", False))


class DictationSession:
    def __init__(self) -> None:
        self.rec_proc: Optional[subprocess.Popen] = None
        self.ws = None
        self.finals: list[str] = []
        self.running = True
        self.stopping = False
        # Set once "stopped" has been emitted, so a second stop() caller (the
        # plugin's Stop button racing a natural end of stream) waits for the
        # in-flight flush instead of returning silently and skipping it.
        self._stopped_event = asyncio.Event()

    async def start(self) -> None:
        api_key = resolve_api_key()
        if not api_key:
            emit({
                "type": "error",
                "message": "No Deepgram API key: set DEEPGRAM_API_KEY or write it to "
                           + KEY_FILE_CANDIDATES[-1],
            })
            return

        # Spawn rec: 16kHz / 16-bit / mono PCM to stdout.
        try:
            self.rec_proc = subprocess.Popen(
                [
                    "rec", "-q",
                    "--buffer", "512",
                    "-r", "16000", "-c", "1", "-b", "16",
                    "-e", "signed-integer",
                    "-t", "raw", "-",
                ],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
            )
        except FileNotFoundError:
            emit({"type": "error", "message": "rec not found — install sox"})
            return
        except Exception as e:
            emit({"type": "error", "message": f"Failed to spawn rec: {e}"})
            return

        # Connect to Deepgram.
        try:
            self.ws = await websockets.connect(
                DG_URL,
                subprotocols=["token", api_key],
            )
        except Exception as e:
            emit({"type": "error", "message": f"Deepgram connection failed: {e}"})
            self._kill_rec()
            return

        emit({"type": "ready"})

        # Stream audio from rec → Deepgram.
        async def stream_audio():
            loop = asyncio.get_event_loop()
            try:
                while self.running and self.rec_proc and self.rec_proc.stdout:
                    chunk = await loop.run_in_executor(None, self.rec_proc.stdout.read, 4096)
                    if not chunk:
                        break
                    if _ws_is_open(self.ws):
                        await self.ws.send(chunk)
            except Exception as e:
                emit({"type": "error", "message": f"Audio streaming failed: {e}"})

        # Receive transcripts from Deepgram.
        async def receive_transcripts():
            try:
                async for message in self.ws:
                    self._handle_message(message)
            except Exception as e:
                emit({"type": "error", "message": f"Deepgram receive failed: {e}"})

        # Run both tasks concurrently.
        recv_task = asyncio.create_task(receive_transcripts())
        stream_task = asyncio.create_task(stream_audio())

        # Wait for either task to complete (stream ends or error).
        done, pending = await asyncio.wait(
            [recv_task, stream_task],
            return_when=asyncio.FIRST_COMPLETED,
        )
        for t in pending:
            t.cancel()
            try:
                await t
            except asyncio.CancelledError:
                pass

        # If we're still running (not cancelled), the stream ended naturally.
        if self.running and not self.stopping:
            await self.stop()

    def _handle_message(self, message) -> None:
        """Turn one Deepgram message into a transcript/interim JSON line."""
        try:
            msg = json.loads(message)
        except (TypeError, ValueError):
            return
        if msg.get("type") != "Results":
            return
        transcript = (
            msg.get("channel", {})
            .get("alternatives", [{}])[0]
            .get("transcript", "")
        )
        if not transcript:
            return
        if msg.get("is_final"):
            self.finals.append(transcript)
            emit({"type": "transcript", "text": transcript})
        else:
            # Partial hypothesis: the plugin shows it live and replaces it
            # with the final utterance when Deepgram endpoints.
            emit({"type": "interim", "text": transcript})

    async def _flush_and_close(self) -> None:
        """Send CloseStream, drain the finals Deepgram replies with, close.

        Bounded overall by the caller: for silent audio Deepgram sends nothing
        back, and cancelling the receive iterator mid-flight and then waiting
        for the closing handshake would otherwise stall for ~15s (websockets'
        default close_timeout is 10s), leaving the plugin on "Finalizing…".
        """
        if _ws_is_open(self.ws):
            try:
                await self.ws.send(json.dumps({"type": "CloseStream"}))
            except Exception:
                pass
            try:
                async for message in self.ws:
                    self._handle_message(message)
            except Exception:
                pass

        if self.ws is not None:
            try:
                await self.ws.close(close_timeout=1)
            except TypeError:       # older websockets: no close_timeout kwarg
                await self.ws.close()
            except Exception:
                pass

    async def stop(self) -> None:
        if self.stopping:
            try:
                await asyncio.wait_for(
                    self._stopped_event.wait(), timeout=FLUSH_TIMEOUT_S + 2
                )
            except asyncio.TimeoutError:
                pass
            return
        self.stopping = True
        self.running = False

        # Kill rec first, so no more audio is produced while we flush.
        self._kill_rec()

        try:
            await asyncio.wait_for(self._flush_and_close(), timeout=FLUSH_TIMEOUT_S)
        except asyncio.TimeoutError:
            pass
        except Exception as e:
            emit({"type": "error", "message": f"Flush failed: {e}"})

        emit({"type": "stopped"})
        self._stopped_event.set()

    def cancel(self) -> None:
        self.finals = []
        self.running = False
        self.stopping = True
        self._kill_rec()
        if self.ws is not None:
            asyncio.create_task(self._flush_and_close())

    def _kill_rec(self) -> None:
        if self.rec_proc:
            try:
                self.rec_proc.terminate()
                self.rec_proc.wait(timeout=2)
            except Exception:
                try:
                    self.rec_proc.kill()
                except Exception:
                    pass
            self.rec_proc = None

    def get_full_text(self) -> str:
        return " ".join(self.finals).strip()


async def main():
    session = DictationSession()
    loop = asyncio.get_running_loop()
    commands: asyncio.Queue = asyncio.Queue()

    # sys.stdin.readline() blocks, and a thread blocked inside the default
    # executor is joined at interpreter exit — that kept this process alive
    # (and the plugin stuck on "Listening…") after dictation had ended.  A
    # daemon thread feeding an asyncio.Queue does not.
    def stdin_reader():
        # readline(), not `for line in sys.stdin`: iterating a TextIOWrapper
        # over a pipe read-aheads a full buffer block, so commands from the
        # plugin's "Stop" button would not be seen until stdin closed (the
        # session then only ended when Deepgram's idle timeout fired).
        # No EOF sentinel either: a closed stdin means "no further commands",
        # not "end the session" — handle_stdin stays pending and is cancelled
        # when the session task finishes.
        try:
            while True:
                line = sys.stdin.readline()
                if not line:
                    return                  # EOF
                cmd = line.strip().lower()
                if not cmd:
                    continue
                loop.call_soon_threadsafe(commands.put_nowait, cmd)
                if cmd in ("stop", "cancel"):
                    return
        except Exception:
            pass

    threading.Thread(target=stdin_reader, daemon=True).start()

    async def handle_stdin():
        while True:
            cmd = await commands.get()
            if cmd == "stop":
                await session.stop()
                return
            if cmd == "cancel":
                session.cancel()
                return

    stdin_task = asyncio.create_task(handle_stdin())
    session_task = asyncio.create_task(session.start())

    # Whichever finishes first ends the session; the loser is cancelled rather
    # than awaited, so neither a hung socket nor a silent stdin can wedge us.
    done, pending = await asyncio.wait(
        {session_task, stdin_task},
        return_when=asyncio.FIRST_COMPLETED,
    )

    # A stop/cancel command finished first: let the session task run to
    # completion so its flush still emits "stopped"/"final" (cancelling it
    # here used to swallow them), but keep a bound in case the socket stalls.
    if stdin_task in done and session_task in pending:
        try:
            await asyncio.wait_for(
                asyncio.shield(session_task), timeout=FLUSH_TIMEOUT_S + 3
            )
            pending = set()
        except asyncio.TimeoutError:
            pass

    for task in pending:
        task.cancel()
    # Let the cancellations land before inspecting results: a just-cancelled
    # task is neither .cancelled() nor done yet, and .exception() on it raises
    # InvalidStateError.
    if pending:
        await asyncio.gather(*pending, return_exceptions=True)
    for task in (session_task, stdin_task):
        if not task.done() or task.cancelled():
            continue
        exc = task.exception()
        if exc is not None:
            # Report it in the plugin window instead of dying silently.
            emit({"type": "error", "message": f"{type(exc).__name__}: {exc}"})

    # Emit full text on clean stop (so the plugin can use it directly).
    if session.finals:
        emit({"type": "final", "text": session.get_full_text()})


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        pass
