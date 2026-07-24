(defn layout/find-stack
```
given a layout and a path, checks if that path is inside a stack, and if so, returns the path to the stack. Returns nil otherwise.
Assumes there are no nested stacks, simply returns the path to the last stack node it finds.
```
  [layout path]
  (layout/find-last layout path |(
    = (get $ :type) :stack
  ))
)

(defn stack-border-fg
  [node]
  (if (layout/attached? node)
    view-attached-border-fg
    view-unattached-border-fg
  )
)

(defn leaf-border-fg
  [node]
  (if (layout/attached? node)
    view-attached-border-fg
    view-unattached-border-fg
  )
)

(defn layout/add-stacked-view
  ```
  given a layout, path, and pane node id, add a stacked view.
  If path is to a stack, the new view is added to the stack in front of the active leaf.
  If path is not to a stack, that node is replaced with a stack,
  and any descendant views of the node at `path` are put into the stack.
  `pane-id` is placed at the front (bottom) of the stack.

  If `attach`, the view is attached to and the new stack leaf made active
  Other views will be detached and other stack leaves made inactive
  ```
  [layout path pane-id &opt &named attach]

  (def layout (if attach (layout/detach layout) layout))
  (def node (layout/path layout path))
  (def new-leaf {
    :active attach
    :title border-title
    # :title "blorp"
    # :border-fg "3"
    :border-fg border-fg
      :node {
      :type :view
      :attached attach
      :id pane-id
    }
  })

  (def stack (if (layout/type? :stack node) (do
    (def active-index (find-index |($ :active) (node :leaves)))
    (def node (if attach
      (def node (assoc node :leaves (map |(assoc $ :active false) (node :leaves))))
      node
    ))
    (array/insert (node :leaves) (+ 1 active-index) new-leaf)
    node
  ) (do
    (def views (map
      |(layout/path node $)
      (layout/search node |(layout/type? :view $))
     ))

    (def leaves
      (map
        |(identity {
          :title border-title
          :border-fg border-fg
          :node {
            :type :view
            :id ($ :id)
          }
        })
        views
      )
    )

    (array/push leaves new-leaf)

    {
      :type :stack
      :leaves leaves
      # :border-fg border-fg
    }
  )))
  (layout/assoc layout path stack)
)

(defn action/add-stacked-view-cmd
  "add a stacked view with a new pane that runs the passed cmd, when the cmd exits it drops to zsh"
  [cmd &opt &named path]

  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def stack (layout/find-stack layout attach-path))
  (def path (or path (cmd/path (layout/attach-id layout))))
  # (def pwd (cmd/path (layout/attach-id layout)))
  (def new-pane (cmd/new :root :path path :command "sh" :args @["-c" (string cmd "; zsh")]))

  (def new-layout (if stack (do
    (layout/add-stacked-view layout stack new-pane :attach true)
  ) (do
    # the view has a border around it, the path to that is what we'll replace
    (layout/add-stacked-view layout (trim attach-path) new-pane :attach true)
  )))
  (layout/set new-layout)
)

(key/action
  action/add-stacked-view
  "add a stacked view at a new shell in the current directory to the focused view"

  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def stack (layout/find-stack layout path))
  (def node (layout/path layout path))
  (def new-pane (shell/new))

  (def new-layout (if stack (do
    (layout/add-stacked-view layout stack new-pane :attach true)
  ) (do
    # the pane has a border around it, the path to that is what we'll replace
    (layout/add-stacked-view layout (trim path) new-pane :attach true)
  )))
  (layout/set new-layout)
)

(key/action
  action/add-stacked-view-empty
  "add a stacked view that is empty to the focused pane"

  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def stack (layout/find-stack layout path))
  (def node (layout/path layout path))
  (def new-pane nil)

  (def new-layout (if stack (do
    (layout/add-stacked-view layout stack new-pane :attach true)
  ) (do
    # the pane has a border around it, the path to that is what we'll replace
    (layout/add-stacked-view layout (trim path) new-pane :attach true)
  )))
  (layout/set new-layout)
)

(defn stack/rotate
  ``
  given a stack node, rotate it.
  If there is an active leaf, maintains its same index
  returns the modified stack node
  direction is :forward or :backward
  if attach, attach to the first view in the active leaf
  ``
  [stack direction &opt &named attach]

  (def stack (if attach (layout/detach stack) stack))
  (def leaves (stack :leaves))

  (def active-leaf-index (find-index |($ :active) leaves))
  (if active-leaf-index (upscope
    (set (leaves active-leaf-index) (assoc (leaves active-leaf-index) :active false))
  ))

  (cond
    (= direction :forward) (do
      (def front-leaf (array/pop leaves))
      (array/insert leaves 0 front-leaf)
    )
    (= direction :backward) (do
      (def back-leaf (get leaves 0))
      (array/remove leaves 0)
      (array/push leaves back-leaf)
    )
  )

  (if active-leaf-index (do
    (def active-leaf (assoc (leaves active-leaf-index) :active true))
    (set (leaves active-leaf-index) (if attach
                                      (assoc active-leaf :node (layout/attach-first (active-leaf :node)))
                                      active-leaf))
  ))

  (assoc stack :leaves leaves)
)

(key/action
  action/rotate-stack-forward
  "rotate the current stack forward"

  (def layout (layout/get))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def stack (layout/path layout stack-path))
  (layout/set (layout/assoc layout stack-path (stack/rotate stack :forward :attach true)))
)

(key/action
  action/rotate-stack-backward
  "rotate the current stack backward"

  (def layout (layout/get))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def stack (layout/path layout stack-path))
  (layout/set (layout/assoc layout stack-path (stack/rotate stack :backward :attach true)))
)

(key/action
  action/reorder-stack-forward
  "reorder the current stack forward. Like rotate-stack-forward, but leaves the active leaf in place. A way to reposition the active leaf in the stack ordering."

  (def layout (layout/get))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))

  (def stack (layout/path layout stack-path))
  (def leaves (stack :leaves))
  (def active-leaf-index (find-index |($ :active) leaves))
  (def active-leaf (leaves active-leaf-index))
  (array/remove leaves active-leaf-index)

  (def stack (stack/rotate {:type :stack :leaves leaves} :forward))
  (def leaves (stack :leaves))
  (array/insert leaves active-leaf-index active-leaf)

  (layout/set (layout/assoc layout stack-path stack))
)

(key/action
  action/reorder-stack-backward
  "reorder the current stack backward. Like rotate-stack-forward, but leaves the active leaf in place. A way to reposition the active leaf in the stack ordering."

  (def layout (layout/get))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))

  (def stack (layout/path layout stack-path))
  (def leaves (stack :leaves))
  (def active-leaf-index (find-index |($ :active) leaves))
  (def active-leaf (leaves active-leaf-index))
  (array/remove leaves active-leaf-index)

  (def stack (stack/rotate {:type :stack :leaves leaves} :backward))
  (def leaves (stack :leaves))
  (array/insert leaves active-leaf-index active-leaf)

  (layout/set (layout/assoc layout stack-path stack))
)

(defn layout/get-stack-panes
  "given a stack node, get an array of the pane ids of its leaves"
  [stack]
  (map |(($ :node) :id) (stack :leaves))
  # (map |((layout/path $ (layout/find $ |(= ($ :type) :view))) :id) (stack :leaves))
)

(defn layout/stack-from-panes
  ``
  given an array of pane IDs, create a stack node with leaves corresponding to those panes, where later panes are in front of/below earlier panes.
  active-leaf is the index of the leaf that should be active. Defaults to the last pane.
  attach specifies whether to attach to the pane in the active leaf. Defaults to false.
  ``
  [panes &opt &named active-leaf attach]
  (default active-leaf (- (length panes) 1))
  (default attach false)

  (def leaves (map |(break
    {
      :title border-title
      :border-fg border-fg
      :node {
        :type :view
        :id $
      }
    }
  ) panes))

  (set (leaves active-leaf) (assoc (leaves active-leaf) :active true))

  (when attach
    (var pane (panes active-leaf))
    (set (leaves active-leaf) (assoc (leaves active-leaf) :node {
      :type :view
      :id pane
      :attached true
    }))
  )

  {
    :type :stack
    :leaves leaves
  }
)

(key/action
  action/merge-split-into-stack
  "merge the parent split into a single stack"
  # if the attached pane is a child of a split, or is in a stack that is a child of a split, merge the two halves (which must each be either a pane or a stack) into a single stack, replacing the split with it. If the sibling of the attached pane is a split, will do nothing
  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  # (def parent-split-path (layout/find-last layout attach-path |(= ($ :type) :split)))
  (def parent-split-path (layout/find-last layout attach-path |(layout/type? :split $)))
  (if (nil? parent-split-path) (break))

  (def parent-split (layout/path layout parent-split-path))
  (def {:a a :b b} parent-split)
  (if (or (= (a :type) :split) (= (b :type) :split)) (break))

  (def panes @[])
  (each node [a b]
    (if (layout/type? :stack node) (do
      (array/push panes ;(layout/get-stack-panes node))
    ) (do # child is a pane
      (def pane (layout/path node (layout/find node |(= ($ :type) :view))))
      (array/push panes (pane :id))
    ))
  )
  (layout/set (layout/assoc layout parent-split-path (layout/stack-from-panes panes :attach true)))
)

(defn layout/split-attached-in-stack
  "splits the attached view into a split if it is in a stack. if downright true, the attached pane will be placed down or right in the split, if false then up or left"
  [layout vertical downright]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (if (nil? stack-path) (break layout))
  (def attach-id (layout/attach-id layout))
  (def layout (layout/remove-node layout attach-path))
  (def a (new-bordered-view attach-id :attach true))
  (def b (layout/path layout stack-path))
  
  (layout/assoc layout stack-path {
    :type :split
    :vertical vertical
    :border :none
    :a (if downright b a)
    :b (if downright a b)
  })
)

(key/action
  action/split-stacked-view-up
  "split stacked view up"
  # splits the currently attached stack into a vertical split, where the active leaf becomes the top child
  (layout/set (layout/split-attached-in-stack (layout/get) true false))
)

(key/action
  action/split-stacked-view-down
  "split stacked view down"
  # splits the currently attached stack into a vertical split, where the front pane becomes the bottom child
  (layout/set (layout/split-attached-in-stack (layout/get) true true))
)

(key/action
  action/split-stacked-view-left
  "split stacked view left"
  # splits the currently attached stack into a horizontal split, where the front pane becomes the left child
  (layout/set (layout/split-attached-in-stack (layout/get) false false))
)

(key/action
  action/split-stacked-view-right
  "split stacked view right"
  # splits the currently attached stack into a horizontal split, where the front pane becomes the right child
  (layout/set (layout/split-attached-in-stack (layout/get) false true))
)

(defn layout/move-stacked-view
  ``
  moves the attached view, if it is in a stack, directionally to another view or stack
  if moving to a view, creates a new stack
  if moving to a stack, the view is placed in the front (bottom)
  direction is :up, :down, :left, or :right
  ``
  [layout direction]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (if (nil? stack-path) (break layout))

  (def pane (as-> (layout/path layout stack-path) _
    (_ :leaves)
    (find |($ :active) _)
    (_ :node)
    (_ :id)
  ))

  (def move-path (layout/find-nearest-node layout stack-path direction [:view :stack] :wrap true))
  # we want to replace a view's borders node if it exists rather than the pane
  (def move-node (layout/path layout move-path))
  (when (layout/type? :view move-node)
    (def parent-path (trim move-path))
    (def parent-node (layout/path layout parent-path))
    (if (layout/type? :borders parent-node) (array/pop move-path))
  )

  (-> layout
    (layout/remove-node attach-path)
    (layout/add-stacked-view move-path pane :attach true)
  )
)

(key/action
  action/move-stacked-view-left
  "move stacked view left"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-view layout :left)))
)

(key/action
  action/move-stacked-view-right
  "move stacked view right"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-view layout :right)))
)

(key/action
  action/move-stacked-view-up
  "move stacked view up"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-view layout :up)))
)

(key/action
  action/move-stacked-view-down
  "move stacked view down"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-view layout :down)))
)
