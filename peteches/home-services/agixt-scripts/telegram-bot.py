import asyncio
import logging
import os
import traceback
from typing import Optional, Set

from telegram import Update
from telegram.constants import ParseMode
from telegram.ext import Application, CommandHandler, MessageHandler, ContextTypes, filters

try:
    from agixtsdk import AGiXTSDK
except Exception as e:  # pragma: no cover
    print("Missing agixtsdk. The start script should install it; check logs.")
    raise

# ---- Configuration from environment (set by agixt-telegram-bot) ----
BASE_URI: str = os.getenv("AGIXT_BASE_URI", "http://localhost:7437")
API_KEY: str = os.getenv("AGIXT_API_KEY", "")
AGENT: str = os.getenv("AGIXT_AGENT", "AGiXT")
DEFAULT_CHAIN: Optional[str] = (os.getenv("AGIXT_CHAIN", "") or "").strip() or None

_ALLOWED = (os.getenv("TELEGRAM_ALLOWED_USER_IDS", "") or "").strip()
ALLOWED_IDS: Optional[Set[int]] = (
    {int(x) for x in _ALLOWED.split(",") if x.strip().isdigit()} if _ALLOWED else None
)

BOT_TOKEN: str = os.environ["TELEGRAM_BOT_TOKEN"]

# ---- Logging ----
logging.basicConfig(level=logging.INFO)
log = logging.getLogger("agixt-tg")

# ---- AGiXT client ----
client = AGiXTSDK(base_uri=BASE_URI, api_key=API_KEY)


def _shorten(s: str, n: int = 3500) -> str:
    return s if len(s) <= n else (s[: n - 10] + "…")


def allowed(user_id: int) -> bool:
    return ALLOWED_IDS is None or user_id in ALLOWED_IDS


async def ask_agixt(user_text: str) -> str:
    """
    Try the chat-completions style API first; if not available, fall back to
    a chain call or prompt_agent variants.
    """
    # Try chat-completions style
    try:
        resp = await asyncio.to_thread(
            client.create_chat_completion,
            agent_name=AGENT,
            messages=[{"role": "user", "content": user_text}],
        )
        if isinstance(resp, dict):
            ch = (resp.get("choices") or [{}])[0]
            msg = (ch.get("message") or {}).get("content") or ch.get("text")
            if msg:
                return str(msg)
            if "response" in resp:
                return str(resp["response"])
        return str(resp)
    except Exception as e:
        log.debug("create_chat_completion failed, falling back: %r", e)

    # Optional: run a chain if configured
    if DEFAULT_CHAIN:
        for method_name in ("run_prompt_chain", "run_chain"):
            try:
                method = getattr(client, method_name)
                resp = await asyncio.to_thread(method, DEFAULT_CHAIN, user_text, AGENT)
                return str(resp)
            except Exception:
                pass

    # Fallback to prompt_agent with a few likely prompt names
    for prompt in ("Smart Chat", "Chat", "Instruct"):
        try:
            resp = await asyncio.to_thread(
                client.prompt_agent, AGENT, prompt, {"input": user_text}
            )
            return str(resp)
        except Exception:
            continue

    return "Sorry, I could not reach AGiXT or no methods succeeded."


# ---- Telegram handlers ----
async def cmd_start(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    if not allowed(update.effective_user.id):
        return
    msg = [
        "Hi! I will send your messages to your AGiXT agent.",
        f"Agent: {AGENT}",
        f"Default chain: {DEFAULT_CHAIN or '(none)'}",
        "Just type anything to begin.",
    ]
    await update.message.reply_text("\n".join(msg))


async def cmd_help(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    if not allowed(update.effective_user.id):
        return
    await update.message.reply_text("/start — greet\nAnything else — forwarded to AGiXT")


async def on_text(update: Update, context: ContextTypes.DEFAULT_TYPE) -> None:
    if not allowed(update.effective_user.id):
        return
    text = update.message.text or ""
    try:
        reply = await ask_agixt(text)
    except Exception as e:
        traceback.print_exc()
        reply = f"Error: {e!r}"
    await update.message.reply_text(
        _shorten(reply), parse_mode=ParseMode.HTML, disable_web_page_preview=True
    )


def main() -> None:
    app = Application.builder().token(BOT_TOKEN).build()
    app.add_handler(CommandHandler("start", cmd_start))
    app.add_handler(CommandHandler("help", cmd_help))
    app.add_handler(MessageHandler(filters.TEXT & ~filters.COMMAND, on_text))
    app.run_polling()


if __name__ == "__main__":
    main()
