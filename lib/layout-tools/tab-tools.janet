(defn styled-tabs-node
  "given an array of tabs, returns a tabs node with colors set"
  [tabs]
  {
    :type :tabs
    :tabs tabs
    :active-fg tab-active-fg
    :active-bg tab-active-bg
    :inactive-fg tab-inactive-fg
    # :inactive-fg "9"
    :inactive-bg tab-inactive-bg
    # :inactive-bg "9"
  }
)

(defn style-tab-name
  "given a name for a tab, style it"
  [name]

  # (string " " name " ")
  (string " " name " ")
)

(defn renumber-tabs
  "given an array of tabs (the :tabs property of a tabs node), for any tab which has a name that is a number, change its name to be the index of that tab, starting at 1. Modifies the array and returns it."
  [tabs]

  (for i 0 (length tabs) (do
    (def tab (tabs i))
    (if (scan-number (tab :name))
      (put tabs i (assoc tab :name (string (+ i 1))))
    )
  ))

  tabs
)

(defn layout/new-tab
  "creates a new tab with the specified child. If tabs-path is not provided, adds the tab to the first tabs node found, or creates a top-level tab if none is found. If attach, attaches to child-node and sets the new tab to active."
  [layout child-node &opt &named tabs-path attach]

  (default tabs-path (layout/find
    layout
    |(layout/type? :tabs $)
  ))
  (default attach false)

  (def layout (if attach
    (def layout (layout/detach layout))
    layout
  ))

  (def child-node (if attach (layout/attach-first child-node) child-node))

  (def new-layout (if (nil? tabs-path) (do
    (if (param/get :mode :target :client) # if in a mode, tab node should be child of mode bar
      (layout/assoc layout @[:node] (styled-tabs-node @[
        {
          :name "1"
          :active (not attach)
          :node (layout/path layout @[:node])
        }
        {
          :name "2"
          :active attach
          :node child-node
        }
      ]))
      (styled-tabs-node @[
        {
          :name "1"
          :active (not attach)
          :node layout
        }
        {
          :name "2"
          :active attach
          :node child-node
        }
      ])
    )
  ) (do
    (def tabs-node (layout/path layout tabs-path))
    (def {:tabs existing-tabs} tabs-node)
    (var active-tab 0)
    (for i 0 (length existing-tabs)
      (if ((existing-tabs i) :active) (set active-tab i))
    )

    (def new-tab {
      :name "1"
      :active attach
      :node child-node
    })

    (def existing-tabs (if attach
      (map |(assoc $ :active false) existing-tabs)
      existing-tabs
    ))

    (layout/assoc
      layout
      tabs-path
      (assoc tabs-node :tabs (renumber-tabs (array/insert existing-tabs (+ 1 active-tab) new-tab)))
    )
  )))
)

(key/action
  action/move-tab-left
  "move tab left"

  (def layout (layout/get))
  (def tabs-path (layout/find layout |(layout/type? :tabs $)))
  (if (not tabs-path) (break))
  (def tabs-node (layout/path layout tabs-path))
  (def tabs (tabs-node :tabs))
  (def num-tabs (length tabs))

  (var active-tab-index (find-index |($ :active) tabs))
  (def active-tab (tabs active-tab-index))
  (array/remove tabs active-tab-index)
  (set active-tab-index (% (-- active-tab-index) num-tabs))
  (array/insert tabs active-tab-index active-tab)
  (renumber-tabs tabs)
  (layout/set (layout/assoc layout tabs-path (assoc tabs-node :tabs tabs)))
)

(key/action
  action/move-tab-right
  "move tab right"

  (def layout (layout/get))
  (def tabs-path (layout/find layout |(layout/type? :tabs $)))
  (if (not tabs-path) (break))
  (def tabs-node (layout/path layout tabs-path))
  (def tabs (tabs-node :tabs))
  (def num-tabs (length tabs))

  (var active-tab-index (find-index |($ :active) tabs))
  (def active-tab (tabs active-tab-index))
  (array/remove tabs active-tab-index)
  (set active-tab-index (% (++ active-tab-index) num-tabs))
  (array/insert tabs active-tab-index active-tab)
  (renumber-tabs tabs)
  (layout/set (layout/assoc layout tabs-path (assoc tabs-node :tabs tabs)))
)

