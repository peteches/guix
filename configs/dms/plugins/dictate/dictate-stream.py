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
    "&interim_results=false"
    "&smart_format=true"
    "&punctuate=true"
    "&endpointing=300"
)


def emit(msg: dict) -> None:
    """Write a JSON line to stdout."""
    print(json.dumps(msg), flush=True)


class DictationSession:
    def __init__(self) -> None:
        self.rec_proc: Optional[subprocess.Popen] = None
        self.ws = None
        self.finals: list[str] = []
        self.running = True
        self.stopping = False

    async def start(self) -> None:
        api_key = os.environ.get("DEEPGRAM_API_KEY", "").strip()
        if not api_key:
            emit({"type": "error", "message": "DEEPGRAM_API_KEY not set"})
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
            while self.running and self.rec_proc and self.rec_proc.stdout:
                chunk = await loop.run_in_executor(None, self.rec_proc.stdout.read, 4096)
                if not chunk:
                    break
                if self.ws and self.ws.open:
                    await self.ws.send(chunk)

        # Receive transcripts from Deepgram.
        async def receive_transcripts():
            async for message in self.ws:
                try:
                    msg = json.loads(message)
                except json.JSONDecodeError:
                    continue
                if msg.get("type") == "Results" and msg.get("is_final"):
                    transcript = (
                        msg.get("channel", {})
                        .get("alternatives", [{}])[0]
                        .get("transcript", "")
                    )
                    if transcript:
                        self.finals.append(transcript)
                        emit({"type": "transcript", "text": transcript})

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

    async def stop(self) -> None:
        if self.stopping:
            return
        self.stopping = True
        self.running = False

        # Kill rec first.
        self._kill_rec()

        # Tell Deepgram to flush.
        if self.ws and self.ws.open:
            try:
                await self.ws.send(json.dumps({"type": "CloseStream"}))
            except Exception:
                pass

            # Wait briefly for final results.
            try:
                async for message in self.ws:
                    try:
                        msg = json.loads(message)
                    except json.JSONDecodeError:
                        continue
                    if msg.get("type") == "Results" and msg.get("is_final"):
                        transcript = (
                            msg.get("channel", {})
                            .get("alternatives", [{}])[0]
                            .get("transcript", "")
                        )
                        if transcript:
                            self.finals.append(transcript)
                            emit({"type": "transcript", "text": transcript})
            except Exception:
                pass

        if self.ws:
            try:
                await self.ws.close()
            except Exception:
                pass

        emit({"type": "stopped"})

    def cancel(self) -> None:
        self.finals = []
        self.running = False
        self._kill_rec()
        if self.ws:
            asyncio.create_task(self.ws.close())

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

    # Handle stdin commands.
    async def handle_stdin():
        loop = asyncio.get_event_loop()
        while session.running:
            line = await loop.run_in_executor(None, sys.stdin.readline)
            if not line:
                break
            cmd = line.strip().lower()
            if cmd == "stop":
                await session.stop()
                break
            elif cmd == "cancel":
                session.cancel()
                break

    stdin_task = asyncio.create_task(handle_stdin())
    session_task = asyncio.create_task(session.start())

    await asyncio.gather(session_task, stdin_task, return_exceptions=True)

    # Emit full text on clean stop (so the plugin can use it directly).
    if session.finals:
        emit({"type": "final", "text": session.get_full_text()})


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        pass
