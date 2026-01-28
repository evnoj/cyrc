(key/action
  action/check-for-custom-value
  "check for custom value"

  (var path (layout/attach-path (layout/get)))
  (var found false)

  (while (> (length path) 0)
    (def node (layout/path (layout/get) path))
    (when (get node :custom)
      (msg/log :info (struct-to-string node))
      (set found true)
      (break)
    )
    (set path (trim path))
  )

  (if found
    (msg/log :info "found")
    (msg/log :info "not found")
  )
)

(key/action
  action/get-n-set
  "get the layout and set it"
  (layout/set (layout/get))
)

# using table params, you can just modify the table in janet without having to re-set the param
(key/action
  action/set-table-param
  "set a table param"

  (param/set :client :table @{:mykey :myval})
)

(key/action
  action/log-table-param
  "log the table param"
  (msg/log :info (struct-to-string (param/get :table :target :client)))
)

(key/action
  action/modify-table-param
  "modify the table param"
  (def t (param/get :table :target :client))
  (set (t :mykey) :diffval)
  (msg/log :info (struct-to-string (param/get :table :target :client)))
)

(key/action
  action/log-layout

  "log layout"

  (layout/set
    {
      :type :tabs
      :tabs @[
        {
          :name "taab 1"
          :active true
          :node {
            :type :split
            :vertical false
            :border :none
            :a (render-stack [(shell/new) (shell/new)])
            :b (new-bordered-pane (get-logs-pane) true)
          }
        }
        {
          :name "tab 2"
          :node (new-bordered-pane (shell/new))
        }
      ]
    }
  )
  # (layout/set
  #   {
  #     :type :tabs
  #     :tabs @[
  #       {
  #         :name "taab 1"
  #         :active true
  #         :node {
  #           :type :split
  #           :vertical false
  #           :border :none
  #           :a {
  #             :type :margins
  #             :node {
  #               :type :bar
  #               # :text (style/text "bar 1" :width 100 :bg "#b8b4d0")
  #               :text (style/text "╭ bar 1" :bold true)
  #               # :text (string
  #               #   (style/text "╭bar" :bg "#b8b4d0")
  #               #   (style/text " 1" :bg "#68AD99")
  #               # )
  #               :node {
  #                 :type :bar
  #                 :text (style/text "╭ bar 2" :bold true)
  #                 :node (new-bordered-pane (shell/new) true)
  #               }
  #             }
  #           }
  #           # :b (new-bordered-pane (shell/new) true)
  #           :b (new-bordered-pane (get-logs-pane))
  #         }
  #       }
  #       {
  #         :name "tab 2"
  #         :node (new-bordered-pane (shell/new))
  #       }
  #     ]
  #   }
  # )
  # (layout/set
  #   {
  #     :type :tabs
  #     :tabs @[
  #       {
  #         :name "taab 1"
  #         :active true
  #         :node {
  #           :type :split
  #           :vertical false
  #           :border :none
  #           :a (new-bordered-pane (shell/new) true)
  #           :b {
  #             :type :tabs
  #             :tabs @[
  #               {
  #                 :name "subtab 1"
  #                 :active true
  #                 :node (new-bordered-pane (shell/new))
  #               }
  #               {
  #                 :name "subtab 2"
  #                 :node (new-bordered-pane (shell/new))
  #               }
  #             ]
  #           }
  #         }
  #       }
  #       {
  #         :name "tab 2"
  #         :node (new-bordered-pane (shell/new))
  #       }
  #     ]
  #   }
  # )
  # (layout/set
  #   {
  #     :type :tabs
  #     :tabs @[
  #       {
  #         :name "tab 1"
  #         :active true
  #         :node (new-bordered-pane (shell/new) true)
  #       }
  #       {
  #         :name "tab 2"
  #         :node (new-bordered-pane (shell/new))
  #       }
  #     ]
  #   }
  # )
)

