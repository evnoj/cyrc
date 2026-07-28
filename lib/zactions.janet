(key/action
  action/jump-pane-by-title
  "Jump to a pane by title."

  (as?-> (group/leaves :root) _
    (map |(tuple (pane-display-title $ :style false) {:type :node :id $} $) _)
    (input/find _ :prompt "search: pane")
    (pane/attach _)
  )
)

