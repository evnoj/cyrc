# ----- ZLE BINDINGS -----
# zle (zsh line editor) does not support the kitty keyboard protocol (KKP)
# in addition, I want to support certain keybinds such as ctrl+s that are tricky
# I have set up zle mappings for custom escape codes for certain special keybinds
# these bindings should be made active when entering zle, and inactive when leaving
# this is done via zle hooks
(def zle-binding-table @{
  "shift+enter" "\x1b[1y"
  "shift+ctrl+left" "\x1b[1;1y"
  "shift+ctrl+right" "\x1b[1;2y"
  "shift+ctrl+up" "\x1b[1;3y"
  "shift+ctrl+down" "\x1b[1;4y"
  "ctrl+enter" "\x1b[1;5y"
  "shift+backspace" "\x1b[1;6y"
  "shift+alt+left" "\x1b[2;1y"
  "shift+alt+right" "\x1b[2;2y"
  "shift+alt+up" "\x1b[2;3y"
  "shift+alt+down" "\x1b[2;4y"
  "shift+alt+h" "\x1b[104;4u"
  "shift+alt+l" "\x1b[108;4u"
  "ctrl+s" "\x1b[115;5u"
})

(key/action
  action/zle-bindings-activate
  "activate zle bindings"

  (def pane (pane/current))
  # (msg/log :info (string "activation pane: " pane))
  (eachp [binding escape-sequence] zle-binding-table
    (key/bind pane [binding] (fn [] (pane/send-bytes pane escape-sequence)))
  )
)

(key/action
  action/zle-bindings-deactivate
  "deactivate zle bindings"

  (def pane (pane/current))
  # (msg/log :info (string "deactivation pane: " pane))
  (eachk binding zle-binding-table
    (key/unbind pane [binding])
  )
)
