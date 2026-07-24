(key/action
  action/remove-attached-view
  "Remove the attached view from the layout, leaving its pane in the node tree."

  (def layout (layout/get))
  (def layout (layout/remove-node layout (layout/attach-path layout) :attach true))
  (def tabs-path (layout/find layout |(layout/type? :tabs $)))
  (def layout (if tabs-path
    (do
      (def tabs-node (layout/path layout tabs-path))
      (def tabs (tabs-node :tabs))
      (layout/assoc layout tabs-path (assoc tabs-node :tabs (renumber-tabs tabs)))
    )
    layout
  ))

  (layout/set layout)
)

(key/action
  action/kill-attached-view
  "Remove the attached view from the layout and kill its pane."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))
  (action/remove-attached-view)

  (when (not (nil? id))
    (def layout (layout/get))
    (def still-exists-at (layout/find layout |(= ($ :id) id)))

    (if still-exists-at
      (msg/toast :info (string "Didn't kill pane " id ", still exists at:\n" (array-to-string still-exists-at :sep "")))
      (tree/rm id)
    )
  )
)

(key/action
  action/break-view-new-tab
  "break the attached view into a new tab"

  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def attach-id (layout/attach-id layout))
  (def layout (layout/remove-node layout attach-path))

  # (def tabs-path (layout/find-last layout attach-path |(= ($ :type) :tabs)))
  (layout/set (layout/new-tab layout (new-bordered-view attach-id) :attach true))
)

(defn
  custom/split-right
  ```Split the currently attached view into two horizontally, replacing the right view with the given node.```
  [layout node]
  (def attach-path (layout/attach-path layout))
  (def path (or
    (layout/find-stack layout attach-path)
    (trim attach-path)
  ))

  (layout/replace layout path |(do {:type :split
                                        :border :none
                                        :vertical false
                                        :a (layout/detach $)
                                        :b node})))

(defn
  custom/split-left
  ```Split the currently attached view into two horizontally, replacing the left viewwith the given node.```
  [layout node]
  (def attach-path (layout/attach-path layout))
  (def path (or
    (layout/find-stack layout attach-path)
    (trim attach-path)
  ))

  (layout/replace layout path |(do {:type :split
                                        :border :none
                                        :vertical false
                                        :a node
                                        :b (layout/detach $)})))

(defn
  custom/split-down
  ```Split the currently attached view into two vertically, replacing the bottom view with the given node.```
  [layout node]
  (def attach-path (layout/attach-path layout))
  (def path (or
    (layout/find-stack layout attach-path)
    (trim attach-path)
  ))

  (layout/replace layout path |(do {:type :split
                                        :border :none
                                        :vertical true
                                        :a (layout/detach $)
                                        :b node})))

(defn
  custom/split-up
  ```Split the currently attached view into two vertically, replacing the top view with the given node.```
  [layout node]
  (def attach-path (layout/attach-path layout))
  (def path (or
    (layout/find-stack layout attach-path)
    (trim attach-path)
  ))

  (layout/replace layout path |(do {:type :split
                                        :border :none
                                        :vertical true
                                        :a node
                                        :b (layout/detach $)})))

(defmacro-
  new-view-creator
  [name docstring transformer]
  ~(upscope
    (key/action
      ,name
      ,docstring
      (def [ok path] (protect (cmd/path (pane/current))))
      (def shells (,group/mkdir :root "/shells"))
      (def shell (,cmd/new shells
                           :path (if ok path nil)
                           :name (,path/base path))
      )
      (,layout/set
        (,transformer
          (,layout/get)
          (new-bordered-view shell :attach true)
          # {:type :view :id shell :attached true}))))
        )
      )
    )
  )
)

(new-view-creator
  action/split-right
  "Split the current view to the right."
  custom/split-right)

(new-view-creator
  action/split-left
  "Split the current view to the left."
  custom/split-left)

(new-view-creator
  action/split-up
  "Split the current view upwards."
  custom/split-up)

(new-view-creator
  action/split-down
  "Split the current view downwards."
  custom/split-down)

(key/action
  action/grow
  "grow the half of the split the attached view is in"

  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def parent-split-path (layout/find-last layout attach-path |(= ($ :type) :split)))
  (if (nil? parent-split-path) (break))
  (def parent-split (layout/path layout parent-split-path))
  (def attached-a (layout/attached? (parent-split :a)))

  (msg/log :info (string "type: " (parent-split :type) " percent: " (parent-split :percent) ", cells: " (parent-split :cells)))
  (def new-split (cond
    (def cells (parent-split :cells)) (do
      (if attached-a
        (assoc parent-split :cells (+ cells 1))
        (assoc parent-split :cells (- cells 1))
      )
    )
    (def percent (parent-split :percent)) (do
      (if attached-a
        (assoc parent-split :percent (+ percent 2))
        (assoc parent-split :percent (- percent 2))
      )
    )
    (do
      (if attached-a
        (assoc parent-split :percent 52)
        (assoc parent-split :percent 48)
      )
    )
  ))

  (layout/set (layout/assoc layout parent-split-path new-split))
)

(key/action
  action/shrink
  "shrink the half of the split the attached view is in"

  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def parent-split-path (layout/find-last layout attach-path |(= ($ :type) :split)))
  (if (nil? parent-split-path) (break))
  (def parent-split (layout/path layout parent-split-path))
  (def attached-a (layout/attached? (parent-split :a)))

  # (msg/log :info (string "type: " (parent-split :type) " percent: " (parent-split :percent) ", cells: " (parent-split :cells)))
  (def new-split (cond
    (def cells (parent-split :cells)) (do
      (if attached-a
        (assoc parent-split :cells (- cells 1))
        (assoc parent-split :cells (+ cells 1))
      )
    )
    (def percent (parent-split :percent)) (do
      (if attached-a
        (assoc parent-split :percent (- percent 2))
        (assoc parent-split :percent (+ percent 2))
      )
    )
    (do
      (if attached-a
        (assoc parent-split :percent 48)
        (assoc parent-split :percent 52)
      )
    )
  ))

  (layout/set (layout/assoc layout parent-split-path new-split))
)

(defn layout/move-focus
  ``
  given a layout and a direction, detach the attached view and attach to the nearest view or stack in the given direction
  returns the modified layout
  if attaching to a stack, attaches to the stack's active leaf
  direction is :up, :down, :left, or :right
  ``
  [layout direction]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (def attach-path (if stack-path stack-path attach-path))
  (def nearest-path (layout/find-nearest-node layout attach-path direction [:view :stack] :wrap true))
  (if (nil? nearest-path) (break layout))

  (def nearest-node (layout/path layout nearest-path))

  (as-> layout _
    (layout/detach _)
    (cond
      (layout/type? :view nearest-node)
        (layout/attach _ nearest-path)
      (layout/type? :stack nearest-node) (do
        (def leaves (nearest-node :leaves))
        (def active-leaf-index (find-index |($ :active) leaves))
        (def active-leaf (leaves active-leaf-index))
        (def view-path (layout/find (active-leaf :node) |(layout/type? :view $)))
        (layout/attach _ [;nearest-path :leaves active-leaf-index :node ;view-path])
      )
    )
    (activate-tab (activate-stack-leaf _))
  )
)

(key/action
  action/focus-right
  "focus right"

  (def layout (layout/get))
  (layout/set (layout/move-focus layout :right))
)

(key/action
  action/focus-left
  "focus left"

  (def layout (layout/get))
  (layout/set (layout/move-focus layout :left))
)

(key/action
  action/focus-up
  "focus up"

  (def layout (layout/get))
  (layout/set (layout/move-focus layout :up))
)

(key/action
  action/focus-down
  "focus down"

  (def layout (layout/get))
  (layout/set (layout/move-focus layout :down))
)

(key/action
  action/new-tab
  "Create a new tab"

  (def layout (layout/get))
  (layout/set (layout/new-tab layout (new-bordered-view (shell/new)) :attach true))
)

(key/action
  action/swap-split-sides
  "swap split sides"
  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def parent-split-path (layout/find-last layout attach-path |(layout/type? :split $)))

  (if (nil? parent-split-path) (break))
  (def parent-split (layout/path layout parent-split-path))
  (def {:a a :b b} parent-split)
  (def parent-split (assoc (assoc parent-split :a b) :b a))
  (layout/set (layout/assoc layout parent-split-path parent-split))
)
