(key/action
  action/test-layout
  "layout testing"
  (def cmd1 (shell/new))
  (def cmd2 (shell/new))

  (defn
    border-title
    [layout]
    # Get the NodeID of the node the user is attached to
    (def node (layout/attach-id layout))
    (if
      (nil? node) (style/text "detached" :bg "4" :italic true)
      (style/text (tree/path node) :bg "5")))

  (defn
    border-fg
    [layout]
    (def node (layout/attach-id layout))

    (if (nil? node) "4" "5")
  )
)

(defn set-test-layout
  []

  (layout/set
    # {
    #   :type :tabs
    #   :tabs @[
    #     {
    #       :name "taab 1"
    #       :active true
    #       :node {
    #         :type :split
    #         :vertical false
    #         :border :none
    #         :a (new-bordered-view (shell/new) :attach true)
    #         :b (new-bordered-view (get-logs-pane) :title "  cy log")
    #       }
    #     }
    #     {
    #       :name "tab 2"
    #       :node (new-bordered-view (shell/new))
    #     }
    #   ]
    # }
    (styled-tabs-node @[
      {
        # :name (style-tab-name "1")
        :name (style-tab-name "1")
        :node {
          :type :split
          :vertical false
          :border :none
          :a (new-bordered-view (shell/new))
          :b (new-bordered-view (get-logs-pane) :title "  cy log")
        }
      }
      {
        :name (style-tab-name "2")
        :active true
        :node {
          :type :split
          :vertical false
          :border :none
          :a (new-bordered-view (shell/new) :attach true)
          :b (new-bordered-view (get-logs-pane) :title "  cy log")
        }
      }
      {
        :name (style-tab-name "3")
        :node {
          :type :split
          :vertical false
          :border :none
          :a (new-bordered-view (shell/new))
          :b (new-bordered-view (get-logs-pane) :title "  cy log")
        }
      }
      {
        :name (style-tab-name "4")
        :node {
          :type :split
          :vertical false
          :border :none
          :a (new-bordered-view (shell/new))
          :b (new-bordered-view (get-logs-pane) :title "  cy log")
        }
      }
    ])
  )
)

