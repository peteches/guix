-- Zoom: keep the main client on the messaging special workspace. Meeting
-- windows open as normal windows on the active workspace.
--
-- The main client and the meeting are SEPARATE class="zoom" windows: the
-- client's initialTitle is "Zoom Workplace", while a meeting window is
-- titled with the meeting name.
--
-- IMPORTANT: do NOT force zoom windows tiled (float = false). Zoom's
-- transient dialogs (e.g. the "leave meeting" confirmation, a QML
-- DraggableQuickView) must stay floating. Forcing them tiled made them lose
-- focus and close instantly, and the forced-tiling + destruction segfaulted
-- Zoom (signal 11). Meeting windows are created floating by Zoom, but since
-- they now land on a normal workspace (not the messaging tape) they still
-- receive focus.

-- Main client -> messaging special workspace (routed by the
-- workspaces/messaging.lua rule). The client's title always contains
-- "Zoom Workplace"; a meeting window is titled with the meeting name, so this
-- regex matches only the client.
hl.window_rule({
  match = {
    class = "zoom",
    title = "Zoom Workplace",
  },
  tag = "+messaging",
})
