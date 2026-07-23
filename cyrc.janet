# ----- VARIABLES -----
(def tab-active-fg "21")
(def tab-active-bg "20")

(def tab-inactive-fg "23")
(def tab-inactive-bg "22")
(def view-attached-border-fg "24")
(def view-unattached-border-fg "25")
(def stack-attached-border-fg "23")
(def stack-unattached-border-fg "25")
(def background "26")
(def foreground "27")

# ----- HELPER FUNCTIONS -----
(defn get-logs-pane
  "Get the NodeID of the logs pane"
  []

  (find |(= (tree/path $) "/logs") (group/leaves :root))
)

(defn pane-display-title
  [pane &opt &named style attached]
  (default style true)

  (def title (cond
    (nil? pane) "󰆢  empty"
    (or (param/get :title :target pane) (cmd/title pane))
  ))
  (if (zero? (length title)) (break title))

  (if style
    (do
      (def [fg bg] (if attached
        [background foreground]
        [foreground background]
      ))
      (style/text (string " " title " ") :bg bg :fg fg :bold true)
      # (style/text (string " " title " ") :bg "#1F1F28" :bold true)
    )
    (string " " title " ")
  )
)

(defn border-title
  [dimensions node]

  (pane-display-title (node :id) :attached (node :attached))
)

(defn border-fg
  [node]

  (if (layout/attached? node)
    view-attached-border-fg
    view-unattached-border-fg
  )
)

(defn new-bordered-view
  "Create a view with a border. Takes the node ID that should be in the view."
  [node &opt &named attach title]

  (default attach false)
  (if title
    (param/set node :title title)
  )

  {
    :type :borders
    # :title (style/text " sample title  " :bg "#1F1F28" :bold true)
    :title border-title
    :border :rounded
    :border-fg border-fg
    :node {
      :type :view
      :id node
      :attached attach
    }
  }
)

(defn array-to-string
  [arr &opt &named sep]
  (default sep "\n")
  
  (var str "")
  (each item arr (set str (string str item sep)))
  (string/slice str 0 (- -1 (length sep)))
)

(defn struct-to-string
  [st]
  (var str "")
  (eachp [k v] st (set str (string str k "->" v " ")))
  str
)

(defn array-types-to-string
  [arr]
  (var str "")
  (each item arr (set str (string str (type item) "\n")))
  str
)

(defn array/rotate
  ``
  rotate an array. Modifies the input array, returning it.
  direction is :left or :right
  n is the number of times to rotate, default is 1
  ``
  [arr direction &opt n]
  (default n 1)

  (cond
    (= direction :left) (do
      (for i 0 n
        (def first (arr 0))
        (array/remove arr 0)
        (array/push arr first)
      )
    )
    (= direction :right) (do
      (for i 0 n
        (array/insert arr 0 (array/pop arr))
      )
    )
  )
  arr
)

(defn rotate-while
  ``
  rotate an array while (pred array) is true. Modifies the input array, returning it.
  if the array reaches a full rotation, stops evaluation and returns the array
  direction is :left or :right
  ``
  [arr pred direction]

  (var i 0)
  (var len (length arr))
  (while (pred arr)
    (array/rotate arr direction)
    (++ i)
    (if (= i len) (break))
  )
  arr
)

(defn trim
  "trim the last n elements from an array or tuple, returning a new tuple. n defaults to 1"
  [arrtup &opt n]
  (default n 1)

  (tuple/slice arrtup 0 (- -1 n))
)

(defn pad-between
  [left right width]

  (def left-len (length left))
  (def right-len (length right))
  (def padding (- width left-len right-len))
  (if (pos? padding)
    (string left (string/repeat " " padding) right)
    (string left right)
  )
)

(defn set-title
  [title]
  (def pane (pane/current))
  (if pane (do
    (param/set pane :title title)
    (layout/set (layout/get))
  ))
)

(defn get-title
  []
  (param/get :title :target (pane/current))
)

# ----- DEBUGGING TOOLS -----
(key/action
  action/print-path
  "print the attached node's path"

  (def layout (layout/get))
  (def current (pane/current))
  (def parent (tree/parent current))
  (def attach-path (layout/attach-path layout))
  (msg/log :info (string
    # "attach path: " (type (get attach-path 0)) " \n"
    # "attach path: " (get attach-path 1) " \n"
    # "current: " (tree/path current) " \n"
    # "parent: " (tree/path parent) " "
    (array-to-string attach-path) "\n"
    (array-types-to-string attach-path) "\n"
    "path len: " (length attach-path) "\n"
    "resolved: " (get (layout/path layout attach-path) :id) "\n"
    # "parent: " (get (layout/path layout (array/slice attach-path 0 (- (length attach-path) 1))) :id) "\n"
    "parent: " (struct-to-string (layout/path layout (array/slice attach-path 0 (- (length attach-path) 3)))) "\n"
    "attached id: " (layout/attach-id layout)
  ))
)

(key/action
  action/log-layout
  "log the current layout"

  (msg/log :info (struct-to-string (layout/get)))
)

(defn
  pass-->
  ``
  a function for doing side effects in a --> chain.
  Takes two arguments, the 1st is a function that will be called by passing the 2nd.
  Evaluates to the 2nd argument.
  ``
  [func arg]
  # (identity (args (- (length args) 1)))
  (func arg)
  (identity arg)
)

(defn
  pass->
  ``
  a function for doing side effects in a -> chain.
  Takes two arguments, the 2nd is a function that will be called by passing the 1st.
  Evaluates to the 1st argument.
  ``
  [arg func]
  # (identity (args (- (length args) 1)))
  (func arg)
  (identity arg)
)

(defn
  pass-first
  "Evaluates to its first argument. Intended to insert print debugging statements into -> chains."
  [& args]
  (identity (args 0))
)

(defn pretty-string
  "Pretty print a Janet value with indentation for nested structures.
  Returns a string representation suitable for printing."
  [value]
  
  (def rainbow-colors [
    "\e[31m"  # Red
    "\e[33m"  # Orange (yellow)
    "\e[93m"  # Yellow (bright yellow)
    "\e[32m"  # Green
    "\e[34m"  # Blue
    "\e[94m"  # Indigo (bright blue)
    "\e[35m"  # Violet (magenta)
  ])
  (def reset-color "\e[0m")
  
  (defn make-indent
    "Create indentation string with colored vertical bars"
    [level]
    (if (= level 0)
      ""
      (string/join
        (map (fn [i]
               (def color (get rainbow-colors (% i (length rainbow-colors))))
               (string color "|" reset-color " "))
             (range level))
        "")))
  
  (defn pp-helper
    "Recursive helper that tracks indentation level"
    [v level]
    (cond
      (nil? v)
      "nil"
      
      (boolean? v)
      (string v)
      
      (number? v)
      (string v)
      
      (string? v)
      (string "\"" v "\"")
      
      (keyword? v)
      (string ":" v)
      
      (symbol? v)
      (string v)
      
      (or (array? v) (tuple? v))
      (if (empty? v)
        "[]"
        (do
          (def indent (make-indent (+ level 1)))
          (def close-indent (make-indent level))
          (def items (map |(pp-helper $ (+ level 1)) v))
          # Add commas after all elements except the last
          (def len (length items))
          (def items-with-commas 
            (map (fn [i item]
                   (if (< i (- len 1))
                     (string item ",")
                     item))
                 (range len)
                 items))
          (string "[\n"
                  indent
                  (string/join items-with-commas (string "\n" indent))
                  "\n" close-indent "]")))
      
      (or (table? v) (struct? v))
      (if (empty? v)
        "{}"
        (do
          (def indent (make-indent (+ level 1)))
          (def close-indent (make-indent level))
          (def pairs (pairs v))
          (def items (map (fn [[k val]]
                           (string (pp-helper k (+ level 1))
                                   ": "
                                   (pp-helper val (+ level 1))))
                         pairs))
          (string "{\n"
                  indent
                  (string/join items (string "\n" indent))
                  "\n" close-indent "}")))
      
      # default case
      (describe v)))
  
  (pp-helper value 0))

(defn pretty-log
  "pretty-print every passed value to the log, with a newline after each"
  [& args]
  (def starr @[])
  (each arg args (do
    (array/push starr (pretty-string arg) "\n")
  ))
  (array/pop starr)
  (msg/log :info (string "\n" ;starr))
)

(key/action
  action/print-bindings
  "print the current bindings"

  (def bindings (key/current))
  (each binding bindings (pretty-log (describe (binding :function))))
)

# ---- LAYOUT FUNCTIONS -----
(defn
  layout/find-last-with-path
  ```Get the path to the last node in the path where (predicate node path) evaluates to true. The path passed to the predicate is the path to that node for the given layout.```
  [layout path predicate]
  # Must be a valid path and actually map to a node
  (if (nil? path) (break nil))
  (if (= (length path) 0) (break nil))
  (if (nil? (layout/path layout path)) (break nil))

  (def found-path (do
    (def result (find
      |(do
        (def node-path (array/slice path ;$))
        (predicate (layout/path layout node-path) node-path)
      )
      (->>
        (range (length path))
        (map |(tuple 0 $))
        (reverse))
    ))
    result
  ))

  (if (nil? found-path) (break nil))
  (if (= (length found-path) 0) (break @[]))
  (array/slice path ;found-path))

(defn
  layout/find-first
  ```Get the path to the first node in the path where (predicate node path) evaluates to true. The path passed to the predicate is the path to that node for the given layout.```
  [layout path predicate]
  # Must be a valid path and actually map to a node
  (if (nil? path) (break nil))
  (if (= (length path) 0) (break nil))
  (if (nil? (layout/path layout path)) (break nil))

  (def found-path (do
    (def result (find
      |(do
        (def node-path (array/slice path ;$))
        (predicate (layout/path layout node-path))
      )
      (->>
        (range (length path))
        (map |(tuple 0 $))
      )
    ))
    result
  ))

  (if (nil? found-path) (break nil))
  (if (= (length found-path) 0) (break @[]))
  (array/slice path ;found-path))

(defn
  layout/find-first-with-path
  ```Get the path to the first node in the path where (predicate node path) evaluates to true. The path passed to the predicate is the path to that node for the given layout.```
  [layout path predicate]
  # Must be a valid path and actually map to a node
  (if (nil? path) (break nil))
  (if (= (length path) 0) (break nil))
  (if (nil? (layout/path layout path)) (break nil))

  (def found-path (do
    (def result (find
      |(do
        (def node-path (array/slice path ;$))
        (predicate (layout/path layout node-path) node-path)
      )
      (->>
        (range (length path))
        (map |(tuple 0 $))
      )
    ))
    result
  ))

  (if (nil? found-path) (break nil))
  (if (= (length found-path) 0) (break @[]))
  (array/slice path ;found-path))

(defn layout/path-extends?
  ```checks if the first layout path is an extension or "within" the second path, assuming they are both paths from the same node```
  [path-a path-b]

  (defn extends
    [a b &opt i]
    (default i 0)

    (if (>= i (length b)) (break true))

    (if (not= (a i) (b i))
      (break false)
      (extends a b (+ i 1))
    )
  )
  (def result (extends path-a path-b))
  
  # (msg/log :info (string "path-extends?\n does this path: " (array-to-string path-a) "\nextend this path: " (array-to-string path-b) "\nresult: " result))
  result
)

(defn layout/search
  ```given a layout node and a predicate function that takes a layout node,
  return an array of every path to a node where the predicate evaluated to true
  ```
  [layout pred]

  (def paths @[])
  (def children (layout/successors layout))
  (each child children (do
    (def found-paths (map |(identity @[;child ;$]) (layout/search (layout/path layout child) pred)))
    (array/concat paths found-paths)
  ))

  (if (pred layout) (array/push paths @[]))
  paths
)

(key/action
  action/layout-search-test
  "layout search test"

  (def layout (layout/get))
  (def views (layout/search layout |(layout/type? :view $)))
  (each view views
    (msg/log :info (array-to-string view))
  )
)

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

(defn layout/remove-node
  ```Remove the node from the layout, simplifying the nearest ancestor with children.```
  [layout path &opt &named attach]
  # (def path (layout/attach-path layout))
  (if (nil? path) (break layout))
  (def parent-path (layout/find-last
                    layout path |(> (length (layout/successors $)) 1)
  ))

  # If there are no parents with other children, it's game over, just set the
  # layout to a disconnected view
  (if (nil? parent-path)
    (break {:type :view :attached true}))

  (def parent (layout/path layout parent-path))

  (def new-parent
    (cond
      (layout/type? :tabs parent) (do
        # (def {;parent-path tab-index} path)
        (var tab-index (path (+ 1 (length parent-path))))
        (def existing-tabs (parent :tabs))
        (def remaining-tabs @[])
        (for i 0 (length existing-tabs)
          (if (not= i tab-index)
            (array/push remaining-tabs (existing-tabs i))
          )
        )
        (def num-tabs (length remaining-tabs))

        (if (= num-tabs 1)
          (if attach
            (layout/attach-first ((remaining-tabs 0) :node))
            ((remaining-tabs 0) :node)
          )
          (do
            (if attach (do
              (if (= tab-index num-tabs) (-- tab-index))
              (def attached-tab (remaining-tabs tab-index))
              (set (remaining-tabs tab-index) (as?-> (remaining-tabs tab-index) _
                (assoc _ :node (layout/attach-first (_ :node)))
                (assoc _ :active true)
              ))
            ))
            (assoc parent :tabs remaining-tabs)
          )
        )
      )

      (layout/type? :split parent) (do
        (def split-child (path (length parent-path)))
        (def remaining-child (if (= split-child :a) (parent :b) (parent :a)))
        (if attach
          (layout/attach-first remaining-child)
          remaining-child
        )
      )

      (layout/type? :stack parent) (do
        (var leaf-index (path (+ 1 (length parent-path))))
        (def existing-leaves (parent :leaves))
        (def remaining-leaves @[])
        (for i 0 (length existing-leaves)
          (if (not= i leaf-index)
            (array/push remaining-leaves (existing-leaves i))
          )
        )
        (def num-leaves (length remaining-leaves))

        (if (= num-leaves 1)
          (do
            (def node ((remaining-leaves 0) :node))
            (if attach
              (if (layout/type? :view node)
                (new-bordered-view (node :id) :attach true)
                (layout/attach-first node)
              )
              (if (layout/type? :view node)
                (new-bordered-view (node :id))
                node
              )
            )
          )
          (do
            (if (= leaf-index num-leaves) (-- leaf-index))
            (def attached-leaf (remaining-leaves leaf-index))
            (set (remaining-leaves leaf-index) (as?-> (remaining-leaves leaf-index) _
              (assoc _ :active true)
              (assoc _ :node (if attach (layout/attach-first (_ :node)) (_ :node)))
            ))

            (assoc parent :leaves remaining-leaves)
          )
        )
      )
    )
  )

  (layout/assoc layout parent-path new-parent)
)

# follows implementation of layout/move in the source (`pkg/cy/boot/layout.janet`)
(defn
  layout/find-nearest-node
  ```for the given layout and path, finds the path from the layout to the nearest view or multi-view container (stack, split, tabs) to the node at the given path along the given direction

  returns nil if none was found

  direction is :up, :down, :left, or :right
  node-types is an array of layout node types to look for (ex. [:view :stack] will find the nearest view or stack)
  if wrap, and no regular nearest node was found, will search for a node at the opposite end of the highest ancestor that is arranged along the given axis
  ```
  [layout path direction node-types &opt &named wrap]
  (default wrap false)

  # a unary function that, given a node, returns a boolean that indicates whether the
  # node is arranged ale axis in question.
  # ex. when moving vertically, a vertical split would return true
  (def is-axis (cond
    (has-value? [:up :down] direction)
      |(or
        (and (layout/type? :split $) ($ :vertical))
        (layout/type? :stack $))
    (has-value? [:left :right] direction)
      |(or
        (and (layout/type? :split $) (not ($ :vertical)))
        (layout/type? :tabs $))
  ))

  # a unary function that, given a node where is-axis was true, returns the paths of
  # all of the child nodes accessible from the node in the order of their appearance
  # along the axis.
  (def axis-successors (cond
    (has-value? [:up :left] direction)
      |(identity (reverse (layout/successors $)))
    (has-value? [:down :right] direction)
      |(identity (layout/successors $))
  ))

  (defn successors
    [node]
    (if (is-axis node)
      (axis-successors node)
      (layout/successors node)))

  # We look for a path in the opposite direction of movement.
  # Consider the case where a node has successors :a, :b:, and :c arranged
  # along the axis of motion; if we're attached to a node on :b and moving in
  # the direction of :a, we want `detached-successors` to return just [:a],
  # since [:c] is "after" or "below" us.
  # node-path is the path to the passed node from the given layout
  (defn detached-successors [node node-path]
    (->>
      (successors node)
      (reverse)
      (take-while |(not (layout/path-extends? path @[;node-path ;$])))
      (reverse)
    )
  )

  (defn detached-successors-reverse [node node-path]
    (->>
      (successors node)
      (take-while |(not (layout/path-extends? path @[;node-path ;$])))
    )
  )

  (defn check-node [node node-path]
    (and (is-axis node) (> (length (detached-successors node node-path)) 0)))

  # (defn check-node-wrap [node node-path]
  #   (and (is-axis node) (> (length (detached-successors-wrap node node-path)) 0)))

  # We first find the most recent ancestor to the node we're attached to that
  # has a child tree that we can move to.
  (var branch-path (layout/find-last-with-path layout path check-node))
  (var wrapped false)

  (cond
    # if we didn't find an ancestor with a node to move to, done if not wrapping
    (and (nil? branch-path) (not wrap))
      (break)
    # if wrapping, find the oldest ancestor arranged along the axis
    (and (nil? branch-path) wrap)
      (do
        (set branch-path (layout/find-first layout path is-axis))
        # no ancestor arranged along the axis was found
        (if (nil? branch-path) (break))
        (set wrapped true)
      )
  )

  # if we are wrapping, we reverse the direction of detached successors
  (def [next-path] (if wrapped
    (detached-successors-reverse (layout/path layout branch-path) branch-path)
    (detached-successors (layout/path layout branch-path) branch-path)
  ))
  (def full-path @[;branch-path ;next-path])

  # Find the closest node of the specified types
  (defn
    find-nearest
    [node]
    (if (has-value? node-types (node :type)) (break @[]))
    (def [nearest] (successors node))
    @[;nearest ;(find-nearest (layout/path node nearest))])

  @[;full-path ;(find-nearest (layout/path layout full-path))]
)

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
  custom-view-creator
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

(custom-view-creator
  action/split-right
  "Split the current view to the right."
  custom/split-right)

(custom-view-creator
  action/split-left
  "Split the current view to the left."
  custom/split-left)

(custom-view-creator
  action/split-up
  "Split the current view upwards."
  custom/split-up)

(custom-view-creator
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

# ----- RUN IN PLACE IMPLEMENTATION -----
# call a command from cy exec to run a different command in the current pane that returns when it exits
# ex. cy exec -c '(run-in-place "echo hey | less")'
(defn swap-pane
  "find the view in the current layout with pane id current, and change its pane id to new"
  [current new]

  (def layout (layout/get))
  (def path (layout/find layout |(= ($ :id) current)))
  (if (nil? path) (break))

  (def pane (assoc (layout/path layout path) :id new))
  (layout/set (layout/assoc layout path pane))
)

(defn run-in-place
  [command &opt title]
  (def current-pane (pane/current))
  (def group-path (string "/" current-pane "/cmds"))
  (def group (group/mkdir :root group-path))
  (def name (string "0" (math/round (* 100 (math/random)))))
  (def run-cmd (cmd/new group :name name :command "bash" :args @[
    "-c"
    (string/format
      "%s; cy exec -c '(do
                           (def pane (tree/id %i \"/%s\"))
                           (swap-pane pane %i)
                           (tree/rm pane))'"
      command
      group
      name
      current-pane
    )
  ]))

  (param/set run-cmd :title title)
  (swap-pane current-pane run-cmd)
)

# ----- MODE IMPLEMENTATION -----
# entering a mode puts a bar at the top of the screen displaying the mode
# all keys are unbound and a new set of keybinds is created
# the original keybinds are restored when exiting the mode
# in general, ctrl+alt+q should leave the mode

(defn key-conv
  "takes an array representing a keybind sequence as returned by a func like key/get, and converts it into an array suitable to be passed as the sequence to key/bind"
  [seq]
  (map
    |(
      (if (and (> (length $) 2) (= "re:" (string/slice $ 0 3)))
        (break [:re (string/slice $ 3)])
        (break $)
      )
    )
    seq
  )
)

(defn exit-mode
  []

  (if (nil? (param/get :mode :target :client)) (break)) # not in a mode
  (def restore-bindings (param/get :restore-bindings :target :client))
  (def restore-actions (param/get :restore-actions :target :client))
  (def exit-func (param/get :exit-mode-func :target :client))
  (param/set :client :restore-bindings nil)
  (param/set :client :restore-actions nil)
  (param/set :client :exit-mode-func nil)
  (param/set :client :mode nil)

  (key/unbind :root [])
  (each binding restore-bindings
    (key/bind :root (key-conv (get binding :sequence)) (get binding :function))
  )

  (key/clear-actions)
  (eachp [name value] restore-actions
    (key/register-action name ;value)
  )

  (def layout (layout/get))
  (layout/set (get layout :node))

  (exit-func)
)

(defn enter-mode
  ``
  bindings: an arrtup of arrtups, where each nested arrtup has 2 elements: the 1st is the key sequence (also an arrtup), the 2nd the function to call
  actions: a table where keys are action names and values are tuples where the first element is the docstring and the second is the function of the action
  unbind-keys removes existing keybinds (restored when leaving mode), default true
  remove-actions removes existing actions (restored when leaving mode), default false
  keep-actions is an arrtup of action names that shouldn't be removed, has no effect if remove-actions is false. Each element can be either a function, or a string. If it is a string, that string is used to look up the function from the actions table *after the actions table is modified*. A string should be used for actions that are created when the mode initializes and so are not bound to a symbol during cy initialization.
  exit-func is a function that will be executed when exiting the mode
  bar-text will be passed to the bar's :text property, defaults to the mode name
  ``
  [name &named &opt exit-binding bindings unbind-keys actions remove-actions keep-actions exit-func new-layout bar-text]
  (default unbind-keys true)
  (default remove-actions false)
  (default exit-func (fn []))
  (default bar-text name)

  (param/set :client :mode name)

  # change actions
  (def existing-actions (key/get-actions))
  (param/set :client :restore-actions existing-actions)
  (when remove-actions
    (def kept @{})
    (each name (or keep-actions []) (put kept name (existing-actions name)))
    (key/clear-actions)
    (key/merge-actions kept)
  )
  (when actions
    (eachp [name value] actions
      (key/register-action name ;value)
    )
  )

  (key/action
    action/exit-mode
    "exit the current mode"

    (exit-mode)
  )

  # change bindings
  (param/set :client :restore-bindings (key/current))
  (when unbind-keys
    (key/unbind :root [])
  )
  (when bindings
    (each binding bindings
      (def [keys action] binding)
      (if (= (type action) :string) (do
        # look up the action function
        (key/bind :root keys (((key/get-actions) action) 1))
      ) (do
        (key/bind :root keys action)
      ))
    )
  )
  (key/bind :root exit-binding action/exit-mode)

  (param/set :client :exit-mode-func exit-func)

  (layout/set {
    :type :bar
    :text bar-text
    :node (or new-layout (layout/get))
  })
)

(key/action
  action/test-mode
  "mode test"

  (enter-mode "TEST MODE"
    :exit-binding ["esc"]
    :bindings [
      [["n"] action/add-stacked-view]
      [["p"] action/command-palette]
    ]
    :unbind-keys false
    :remove-actions true
    :actions {
      "action/ploop" ["ploop docstring" (fn [] (pretty-log "I plooped"))]
    }
    :exit-func (fn [] (pretty-log "exited test mode"))
  )
)

# ----- ACTIONS -----
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

(defn swap-split-sides
  ""
  [layout path]
)

# copy mode keybinds
(key/action
  action/copy-mode
  "open copy mode"

  (replay/open (pane/current) :copy true)
  # (pane/send-keys (pane/current) @["v"])
  # (replay/select)
)

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
    :keep-actions ["action/kill-server"]
    :bindings [
      [["f8"] "action/exit-mode"] # for compatibility while transitioning from zellij
      [["ctrl+alt+p"] action/command-palette]
      [["ctrl+alt+s"] action/copy-mode]
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

(key/bind :root ["ctrl+alt+m"] action/maximize)
(key/bind :root ["f8"] action/maximize)


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

# ----- GENERAL CONFIG -----
(def prefix-key "ctrl+m")
# use ctrl-m as prefix key instead of ctrl-a
(key/remap :root ["ctrl+a"] [prefix-key])

(defn hook/init []
  (param/set (tree/id :root "/logs") :title "  cy log")

  # (set-test-layout)
  (layout/set (new-bordered-view (shell/new) :attach true))
  # (param/set :client :replay-selection-style
  #   {
  #     :fg "#c8c093"
  #     :bg "#2d4f67"
  #     # :bold true
  #   }
  # )
)

# keybinds
(key/unbind :root ["ctrl+l"])

(key/bind :root ["ctrl+alt+p"] action/command-palette)

(key/bind :root ["ctrl+alt+h"] action/focus-left)
(key/bind :root ["ctrl+alt+j"] action/focus-down)
(key/bind :root ["ctrl+alt+k"] action/focus-up)
(key/bind :root ["ctrl+alt+l"] action/focus-right)

(key/bind :root ["ctrl+alt+shift+h"] action/move-stacked-view-left)
(key/bind :root ["ctrl+alt+shift+j"] action/move-stacked-view-down)
(key/bind :root ["ctrl+alt+shift+k"] action/move-stacked-view-up)
(key/bind :root ["ctrl+alt+shift+l"] action/move-stacked-view-right)

(key/bind :root ["ctrl+alt+/"] action/merge-split-into-stack)
(key/bind :root ["ctrl+alt+,"] action/split-stacked-view-left)
(key/bind :root ["ctrl+alt+."] action/split-stacked-view-right)
(key/bind :root ["ctrl+alt+shift+up"] action/split-stacked-view-up)
(key/bind :root ["ctrl+alt+shift+down"] action/split-stacked-view-down)
(key/bind :root ["ctrl+alt+="] action/grow)
(key/bind :root ["ctrl+alt+-"] action/shrink)

(key/bind :root ["ctrl+alt+shift+,"] action/move-tab-left)
(key/bind :root ["ctrl+alt+shift+."] action/move-tab-right)
(key/bind :root ["ctrl+alt+b"] action/break-view-new-tab)
(key/bind :root ["ctrl+alt+a"] action/jump-pane)
(key/bind :root [prefix-key "r"] action/rename-tab)

(key/bind :root ["ctrl+alt+u"] action/rotate-stack-backward)
(key/bind :root ["ctrl+alt+o"] action/rotate-stack-forward)
(key/bind :root ["ctrl+alt+shift+u"] action/reorder-stack-backward)
(key/bind :root ["ctrl+alt+shift+o"] action/reorder-stack-forward)

(key/bind :root ["ctrl+alt+n"] action/add-stacked-view)
(key/bind :root ["ctrl+alt+shift+n"] action/add-stacked-view-empty)

(key/bind :root ["ctrl+alt+d"] action/remove-attached-view)
(key/bind :root ["ctrl+alt+x"] action/kill-attached-view)

(key/action
  action/send-text-test
  "send text test"

  (pane/send-text (pane/current) "\e[1;6y")
)

# custom escape sequences for programs that don't support KKP, ex. zsh
# (key/bind :root ["shift+backspace"] (fn [] (pane/send-text (pane/current) "\x1b[1;6y")))

# for intial compatibility while I transition from zellij
(key/bind :root ["f1"] action/focus-left)
(key/bind :root ["f2"] action/focus-down)
(key/bind :root ["f3"] action/focus-up)
(key/bind :root ["f4"] action/focus-right)
(key/bind :root ["f5"] action/add-stacked-view)
(key/bind :root ["f6"] action/break-view-new-tab)
(key/bind :root ["f7"] action/kill-attached-view)
(key/bind :root ["f10"] action/rotate-stack-backward)
(key/bind :root ["f11"] action/rotate-stack-forward)
(key/bind :root ["ctrl+7"] action/remove-attached-view)

(key/bind :root ["ctrl+alt+s"] action/copy-mode)

(key/bind :copy ["esc"] (fn []
  (replay/quit)
  (replay/quit)
  (replay/quit)
))
(key/bind :copy ["y"] (fn [] (replay/copy-clipboard)))
(key/bind :copy ["g" "h"] replay/first-non-blank)
(key/bind :copy ["g" "l"] replay/end-of-line)
(key/bind :copy ["x"] (fn []
  replay/first-non-blank
  (replay/select)

))
(key/bind :copy ["ctrl+b"] replay/select-block)
(key/bind :copy ["l"] replay/cursor-right)

# theming kanagawa
# (color-maps/set :root :kanagawa)
(param/set-many :root
  :replay-selection-style {
    :bg "18"
    :fg "19"
  }
  :animate false
  :data-directory ""
  :use-system-clipboard true
)

(key/action
  action/thumbs-copy
  "thumbs copy to clipboard"
  # uses go regex, RE2 syntax, does not support lookaheads or lookbehinds
  (def patterns @[
    `(?:^|\s)(?P<match>[^\s│─<>]*[.\/][^\s│─<>]*)(?:\s|$)` # files
  ])
  (array/join patterns (input/thumbs/default-patterns))
  (var choice (input/thumbs :patterns patterns))
  (when (nil? choice) (break))
  (clipboard/set choice)
)
(key/action
  action/thumbs-insert
  "thumbs insert"
  # uses go regex, RE2 syntax, does not support lookaheads or lookbehinds
  (def patterns @[
    `(?:^|\s)(?P<match>[^\s│─<>]*[.\/][^\s│─<>]*)(?:\s|$)` # files
  ])
  (array/join patterns (input/thumbs/default-patterns))
  (var choice (input/thumbs :patterns patterns))
  (when (nil? choice) (break))
  (pane/send-text (pane/current) choice)
)
(key/bind :root [prefix-key "s"] action/thumbs-copy)
(key/bind :root [prefix-key "ctrl+s"] action/thumbs-copy)
(key/bind :root ["ctrl+alt+i"] action/thumbs-insert)
(key/bind :root ["ctrl+alt+y"] action/thumbs-copy)
(key/bind :root ["f12"] action/thumbs-insert) # compat during zellij transition

# ----- AUTOLOAD -----
# recursively alphabetically traverse ~/.config/cy/autoload/ and load .janet files
(defn autoload
  ``recursively evaluate every .janet file under dir.
  depth-first recursive alphabetical traversal``
  [dir]
  (when (os/stat dir)
    (each entry (sort (os/dir dir))
      (def path (string dir "/" entry))
      (cond
        (= :directory (os/stat path :mode))
        (autoload path)

        (string/has-suffix? ".janet" entry)
        (do
          (def [ok err] (protect (dofile path :env (curenv))))
          (unless ok
            (msg/log :error (string "autoload failed for " path ": " err))
          )
        )
      )
    )
  )
)

(autoload (string (os/getenv "HOME") "/.config/cy/autoload"))

# ----- SANDBOX -----
