# Kakoune-style copy mode keybindings for cy
#
# Behavior:
#   - Plain motions (h/j/k/l, w/b/e, ...) move cursor, exiting any visual selection
#   - Shift/extend motions (H/J/K/L, W/B/E, ...) auto-enter visual char select and extend
#   - x: when not selecting, selects the full current line; when already selecting,
#         extends cursor to end of line (snaps to line boundary, stays in char mode)
#   - alt+w/b/e for WORD motions; alt+W/B/E for extend-WORD
#   - g prefix: kakoune goto subcommands (plain)
#   - G prefix: extend versions of goto subcommands
#
# State is tracked per-client via cy's parameter system:
#   :kak-selecting - boolean, whether we are in visual char selection

# ----- State helpers -----

(defn- kak-selecting? []
  (param/get :kak-selecting :target :client))

(defn- kak-enter-select []
  (param/set :client :kak-selecting true)
  (replay/select))

(defn- kak-exit-select []
  (param/set :client :kak-selecting false)
  (replay/select))

# ----- Motion wrappers -----

# Plain motion: exits visual selection if active, then moves
(defn- kak-move [motion-fn]
  (fn []
    (when (kak-selecting?)
      (kak-exit-select))
    (motion-fn)))

# Extend motion: enters visual char selection if not active, then moves (extending)
(defn- kak-extend [motion-fn]
  (fn []
    (when (not (kak-selecting?))
      (kak-enter-select))
    (motion-fn)))

# ----- Basic cursor motions -----

(key/bind :copy ["h"] (kak-move replay/cursor-left))
(key/bind :copy ["j"] (kak-move replay/cursor-down))
(key/bind :copy ["k"] (kak-move replay/cursor-up))
(key/bind :copy ["l"] (kak-move replay/cursor-right))

(key/bind :copy ["H"] (kak-extend replay/cursor-left))
(key/bind :copy ["J"] (kak-extend replay/cursor-down))
(key/bind :copy ["K"] (kak-extend replay/cursor-up))
(key/bind :copy ["L"] (kak-extend replay/cursor-right))

# ----- Word motions -----

(key/bind :copy ["w"] (kak-move replay/word-forward))
(key/bind :copy ["b"] (kak-move replay/word-backward))
(key/bind :copy ["e"] (kak-move replay/word-end-forward))

# W/B/E: extend word (kakoune), previously cy's WORD motions
(key/bind :copy ["W"] (kak-extend replay/word-forward))
(key/bind :copy ["B"] (kak-extend replay/word-backward))
(key/bind :copy ["E"] (kak-extend replay/word-end-forward))

# alt+w/b/e: WORD motions (kakoune), whitespace-delimited
(key/bind :copy ["alt+w"] (kak-move replay/big-word-forward))
(key/bind :copy ["alt+b"] (kak-move replay/big-word-backward))
(key/bind :copy ["alt+e"] (kak-move replay/big-word-end-forward))

# alt+W/B/E: extend WORD (terminal-dependent)
(key/bind :copy ["alt+W"] (kak-extend replay/big-word-forward))
(key/bind :copy ["alt+B"] (kak-extend replay/big-word-backward))
(key/bind :copy ["alt+E"] (kak-extend replay/big-word-end-forward))

# ----- Line begin/end (kakoune: alt+h / alt+l) -----

(key/bind :copy ["alt+h"] (kak-move replay/start-of-line))
(key/bind :copy ["alt+l"] (kak-move replay/end-of-line))

(key/bind :copy ["alt+H"] (kak-extend replay/start-of-line))
(key/bind :copy ["alt+L"] (kak-extend replay/end-of-line))

# ----- g prefix: kakoune goto subcommands (plain motions) -----
# Note: g e rebound from cy's word-end-backward to end of buffer (kakoune)

(key/bind :copy ["g" "g"] (kak-move replay/beginning))
(key/bind :copy ["g" "e"] (kak-move replay/end))
(key/bind :copy ["g" "i"] (kak-move replay/first-non-blank))
(key/bind :copy ["g" "h"] (kak-move replay/start-of-line))
(key/bind :copy ["g" "l"] (kak-move replay/end-of-line))
(key/bind :copy ["g" "t"] (kak-move replay/screen-top))
(key/bind :copy ["g" "b"] (kak-move replay/screen-bottom))
(key/bind :copy ["g" "c"] (kak-move replay/screen-middle))

# ----- G prefix: extend versions of goto subcommands -----
# G alone: extend to end of buffer (replaces cy's bare G -> end)

(key/bind :copy ["G"] (kak-extend replay/end))
(key/bind :copy ["G" "g"] (kak-extend replay/beginning))
(key/bind :copy ["G" "e"] (kak-extend replay/end))
(key/bind :copy ["G" "i"] (kak-extend replay/first-non-blank))
(key/bind :copy ["G" "h"] (kak-extend replay/start-of-line))
(key/bind :copy ["G" "l"] (kak-extend replay/end-of-line))
(key/bind :copy ["G" "t"] (kak-extend replay/screen-top))
(key/bind :copy ["G" "b"] (kak-extend replay/screen-bottom))
(key/bind :copy ["G" "c"] (kak-extend replay/screen-middle))

# ----- x: line selection (kakoune-style, stays in char mode) -----
# Not selecting: move to line start, set anchor, move to line end -> full line selected
# Already selecting: extend cursor to end of line (snap cursor side to line boundary)
# After x, all extend keys (H/J/K/L/W/B/E/etc.) continue working normally.

(key/bind :copy ["x"]
  (fn []
    (if (kak-selecting?)
      (replay/end-of-line)
      (do
        (replay/start-of-line)
        (kak-enter-select)
        (replay/end-of-line)))))

# ----- f/t jumps: plain motions (exit selection) -----

(key/bind :copy ["f" [:re "."]]
  (fn [c]
    (when (kak-selecting?) (kak-exit-select))
    (replay/jump-forward c)))

(key/bind :copy ["F" [:re "."]]
  (fn [c]
    (when (kak-selecting?) (kak-exit-select))
    (replay/jump-backward c)))

(key/bind :copy ["t" [:re "."]]
  (fn [c]
    (when (kak-selecting?) (kak-exit-select))
    (replay/jump-to-forward c)))

(key/bind :copy ["T" [:re "."]]
  (fn [c]
    (when (kak-selecting?) (kak-exit-select))
    (replay/jump-to-backward c)))

# ----- Visual mode toggle (tracks state) -----

(key/bind :copy ["v"]
  (fn []
    (if (kak-selecting?)
      (kak-exit-select)
      (kak-enter-select))))

# ----- Yank and quit (reset state) -----

(defn- kak-quit []
  (param/set :client :kak-selecting false)
  (replay/quit) (replay/quit) (replay/quit))

(key/bind :copy ["y"]
  (fn []
    (param/set :client :kak-selecting false)
    (replay/copy-clipboard)
    (replay/quit) (replay/quit) (replay/quit)))

(key/bind :copy ["Y"]
  (fn []
    (param/set :client :kak-selecting false)
    (replay/copy-clipboard)))


(key/bind :copy ["q"] kak-quit)
(key/bind :copy ["esc"] kak-quit)
(key/bind :copy ["ctrl+c"] kak-quit)

