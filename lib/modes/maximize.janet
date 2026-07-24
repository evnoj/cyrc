(key/action
  action/maximize
  "maximize the attached view"

  (def layout (layout/get))
  (param/set :client :restore-layout layout)
  (def attach-id (layout/attach-id layout))

  (defn
    bar-text
    [[rows cols] layout]
    (def node (layout/attach-id layout))
    (def name (or (param/get :title :target node) "detached"))
    (def fg "22")
    (def bg "136")

    (string
      " "
      (style/text
        (pad-between (string " " name) "maximized" (+ cols 2))
        :bg foreground
        :fg background
        :bold true
      )
      " "
    )
  )

  (enter-mode "MAXIMIZED"
    :exit-binding ["ctrl+alt+m"]
    :unbind-existing true
    :unbind-keys true
    :remove-actions true
    :keep-actions {
      "action/kill-server" true
      "action/copy-mode" true
      "action/command-palette" true
      "action/thumbs-insert" true
      "action/thumbs-copy" true
    }
    :bindings [
      # [["ctrl+alt+p"] "action/command-palette"]
      # [["ctrl+alt+s"] "action/copy-mode"]
    ]
    :new-layout {:type :view :attached true :id attach-id}
    :exit-func (fn []
      (def restore-layout (param/get :restore-layout :target :client))
      (param/set :client :restore-layout nil)
      (layout/set restore-layout)
    )
    :bar-text bar-text
  )
)

