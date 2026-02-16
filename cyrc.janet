# ----- HELPER FUNCTIONS -----
(defn get-logs-pane
  "Get the NodeID of the logs pane"
  []

  (find |(= (tree/path $) "/logs") (group/leaves :root))
)

(defn new-border
  "Create a border. Takes the node struct that should be in the pane."
  [node]

  {
    :type :borders
    :title (style/text " sample title  " :bg "#1F1F28" :bold true)
    :border :rounded
    :border-fg "#b8b4d0"
    :border-bg "#b8b4d0"
    :node node
  }
)

(defn pane-display-title
  [pane &opt style]
  (default style true)

  (def title (cond
    (nil? pane) "󰆢  empty"
    (or (param/get :title :target pane) (cmd/title pane))
  ))

  (if style
    (style/text (string " " title " ") :bg "#1F1F28" :bold true)
    (string " " title " ")
  )
)

(defn pane-border-title
  [dimensions node]

  (def pane (get node :id))
  (pane-display-title pane)
)

(defn new-bordered-pane
  "Create a pane with a border. Takes the NodeID that should be in the pane."
  [node &opt &named attach title]

  (default attach false)
  (if title
    (param/set node :title title)
  )

  {
    :type :borders
    # :title (style/text " sample title  " :bg "#1F1F28" :bold true)
    :title pane-border-title
    :border :rounded
    :border-fg "#b8b4d0"
    :border-bg "#b8b4d0"
    :node {
      :type :pane
      :id node
      :attached attach
    }
  }
)

(defn array-to-string
  [arr]
  (var str "")
  (each item arr (set str (string str item "\n")))
  str
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

(defn set-title
  [title]
  (def pane (pane/current))
  (if pane (param/set pane :title title))
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
  (def panes (layout/search layout |(layout/type? :pane $)))
  (each pane panes
    (msg/log :info (array-to-string pane))
  )
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

  (def new-layout (if (nil? tabs-path) (do
    (if (param/get :mode :target :client) # if in a mode, tab node should be child of mode bar
      (layout/assoc layout @[:node] {
        :type :tabs
        :tabs @[
          {
            :name "1"
            :active false
            :node (layout/path layout @[:node])
          }
          {
            :name "2"
            :active true
            :node (assoc child-node :attached true)
          }
        ]
      })
      {
        :type :tabs
        :tabs @[
          {
            :name "1"
            :active false
            :node layout
          }
          {
            :name "2"
            :active true
            :node (assoc child-node :attached true)
          }
        ]
      }
    )
  ) (do
    (def tabs-node (layout/path layout tabs-path))
    (def {:tabs existing-tabs} tabs-node)
    (var active-tab 0)
    (for i 0 (length existing-tabs)
      (if ((existing-tabs i) :active) (set active-tab i))
    )

    (defn tab-name-used [name tabs]
      (var found false)
      (each tab tabs
        (if (= name (tab :name)) (set found true))
      )
      found
    )
    (var name 1)
    (while (tab-name-used (string name) existing-tabs)
      (set name (+ name 1))
    )
    (set name (string name))

    (def new-tab {
      :name name
      :active attach
      :node (if attach (layout/attach-first child-node) child-node)
    })

    (def existing-tabs (if attach
      (map |(assoc $ :active false) existing-tabs)
      existing-tabs
    ))

    (layout/assoc
      layout
      tabs-path
      (assoc tabs-node :tabs (array/insert existing-tabs (+ 1 active-tab) new-tab))
    )
  )))
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
  # layout to a disconnected pane
  (if (nil? parent-path)
    (break {:type :pane :attached true}))

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
              (if (layout/type? :pane node)
                (new-bordered-pane (node :id) :attach true)
                (layout/attach-first node)
              )
              (if (layout/type? :pane node)
                (new-bordered-pane (node :id))
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
  ```for the given layout and path, finds the path from the layout to the nearest pane or multi-pane container (stack, split, tabs) to the node at the given path along the given direction

  returns nil if none was found

  direction is :up, :down, :left, or :right
  node-types is an array of layout pane types to look for (ex. [:pane :stack] will find the nearest pane or stack)
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

# ----- STACK IMPLEMENTATION -----
# a stack stores an ordered list of panes, displaying the pane at the "front"
# panes not in the front are each given a bar showing the pane's title above the front pane
# a margins node (with no margins set) is used as the layout node type that contains a stack
# the border-fg and border-bg properties on the node can store arbitrary strings
# a stack node has :border-fg "stack" and :border-bg is a newline-separated list of the pane IDs

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

(defn get-stack-pane-title
  [pane]
  (style/text (string "╭" (pane-display-title pane false)) :bold true)
)

(defn stack-bar-title
  [dimensions node]
  # TODO needs updating for new stack implementation
  # follow the child down to the pane tracking how many levels deep it goes
  # using the panes id get the stack id param
  # get the pane id that the bar corresponds to
  # get that pane's title param if it exists, or the terminal title if it doesn't

  (var child node)
  (var i 0)

  (while (not= (get child :type) :pane)
    (set child (get child :node))
    (set i (+ i 1))
  )
  # subtract 1 from index for the border node around the pane
  # not sure why I don't need this actually
  # (set i (- i 1))

  (def stack-id (param/get :stack :target (get child :id)))
  (def stacks (param/get :stacks :target :client))
  (def stack (get stacks stack-id))
  (def pane (get (get stack :panes) i))
  # (def title (param/get :title :target pane))
  # (default title (cmd/title pane))
  # (string "stack: " stack-id " pane: " pane " title: " title)
  # (style/text (string "╭ " title) :bold true)
  (get-stack-pane-title pane)

  # (def layout (layout/get))
  # (def stack-path (layout/find-stack layout))
)

(defn layout/add-stacked-pane
  ```
  given a layout, path, and pane node id, add a stacked pane.
  If path is to a stack, the new pane is added to the stack in front of the active leaf.
  If path is not to a stack, that node is replaced with a stack,
  and any descendant panes of the node at `path` are put into the stack.
  `pane-id` is placed at the front of the stack.

  If `attach`, the pane is attached to and the new stack leaf made active
  Other panes will be detached and other stack leaves made inactive
  ```
  [layout path pane-id &opt &named attach]

  (def layout (if attach (layout/detach layout) layout))
  (def node (layout/path layout path))
  (def new-leaf {
    :active attach
    :title pane-border-title
    :node {
      :type :pane
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
    (def panes (map
                |(layout/path node $)
                (layout/search node |(layout/type? :pane $))))

    (def leaves (map
                 |(identity {
                  :title pane-border-title
                  :node {
                    :type :pane
                    :id ($ :id)
                  }
                 }) panes))

    (array/push leaves new-leaf)

    {
      :type :stack
      :leaves leaves
    }
  )))
  (layout/assoc layout path stack)
)

(key/action
  action/layout-path-test
  "layout path test"
  (def layout (layout/get))
  (def node (layout/path layout @[]))
)

(key/action
  action/add-stacked-pane
  "add a stacked pane at a new shell in the current directory to the focused pane"

  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def stack (layout/find-stack layout path))
  (def node (layout/path layout path))
  (def new-pane (shell/new))

  (def new-layout (if stack (do
    (layout/add-stacked-pane layout stack new-pane :attach true)
  ) (do
    # the pane has a border around it, the path to that is what we'll replace
    (layout/add-stacked-pane layout (trim path) new-pane :attach true)
  )))
  (layout/set new-layout)
)

(key/action
  action/add-stacked-pane-empty
  "add a stacked pane that is empty in the current directory to the focused pane"

  (def layout (layout/get))
  (def path (layout/attach-path layout))
  (def stack (layout/find-stack layout path))
  (def node (layout/path layout path))
  (def new-pane nil)

  (def new-layout (if stack (do
    (layout/add-stacked-pane layout stack new-pane :attach true)
  ) (do
    # the pane has a border around it, the path to that is what we'll replace
    (layout/add-stacked-pane layout (trim path) new-pane :attach true)
  )))
  (layout/set new-layout)
)

(defn stack/rotate
  ``
  given a stack node, rotate it.
  If there is an active leaf, maintains its same index
  returns the modified stack node
  direction is :forward or :backward
  if attach, attach to the pane in the active leaf
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
  "reorder the current stack forward. Like shift-stack-forward, but leaves the active leaf in place. A way to reposition the active leaf in the stack ordering."

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
  "reorder the current stack backward. Like shift-stack-forward, but leaves the active leaf in place. A way to reposition the active leaf in the stack ordering."

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
  action/remove-layout-pane
  "Remove the current pane from the layout."

  (def layout (layout/get))
  (def layout (layout/remove-node layout (layout/attach-path layout) :attach true))
  (layout/set layout)
  # (def attach-path (layout/attach-path layout))
  # (def attach-id (layout/attach-id layout))
  # # ensure the pane has a border around it when it isn't in a stack, necessary when simplifying a stack
  # (def stack (layout/find-stack layout attach-path))
  # (when (not stack)
  #   (def parent-path (trim attach-path))
  #   (def parent (layout/path layout parent-path))
  #   (when (not (layout/type? :borders parent))
  #     (def layout (layout/assoc layout attach-path (new-bordered-pane attach-id :attach true)))
  #     (layout/set layout)
  #     (break)
  #   )
  # )
  # (layout/set layout)
)

(key/action
  action/kill-layout-pane
  "Remove the current pane from the layout and the node tree."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))
  (action/remove-layout-pane)

  (if (not (nil? id)) (tree/rm id))
)

(defn layout/stack-from-panes
  ``create and return a stack node with leaves matching the passed array of pane ids
  if attach, attaches to the vertical bottom pane in the stack
  ``
  [panes &opt &named attach]

  (def leaves @[])
  (each pane panes
    (array/push leaves {
      :title pane-border-title
      :node {
        :type :pane
        :id pane
      }
    })
  )

  (def size (length leaves))
  (if attach (set (leaves (- size 1))
    (as-> (leaves (- size 1)) _
      (assoc _ :active true)
      # (layout/attach-first _)) # TODO figure out why this isn't working UPDATE: I know why, it's because attach-first doesn't work on leaves directly (because layout/successors doesn't), so instead of passing the leaf directly to layout/attach-first pass the leaf's :node
      (assoc _ :node {:type :pane :id ((_ :node) :id) :attached true}))
  ))

  {
    :type :stack
    :leaves leaves
  }
)

(defn layout/get-stack-panes
  "given a stack node, get an array of the pane ids of its leaves"
  [stack]
  (map |(($ :node) :id) (stack :leaves))
  # (map |((layout/path $ (layout/find $ |(= ($ :type) :pane))) :id) (stack :leaves))
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

  (def leaves (map |(
    {
      :title pane-border-title
      :node {
        :type :pane
        :id $
      }
    }
  ) panes))

  (set (leaves active-leaf) (assoc (leaves active-leaf) :active true))

  (when attach
    (var pane (panes active-leaf))
    (set (leaves active-leaf) (assoc (leaves active-leaf) :node {
      :type :pane
      :id :pane
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
  (def parent-split-path (layout/find-last layout attach-path |(= ($ :type) :split)))
  (if (nil? parent-split-path) (break))

  (def parent-split (layout/path layout parent-split-path))
  (def {:a a :b b} parent-split)
  (if (or (= (a :type) :split) (= (b :type) :split)) (break))

  (def panes @[])
  (each node [a b]
    (if (layout/type? :stack node) (do
      (array/push panes ;(layout/get-stack-panes node))
    ) (do # child is a pane
      (def pane (layout/path node (layout/find node |(= ($ :type) :pane))))
      (array/push panes (pane :id))
    ))
  )
  (layout/set (layout/assoc layout parent-split-path (layout/stack-from-panes panes :attach true)))
)

(defn layout/split-attached-in-stack
  "splits the attached pane into a split if it is in a stack. if downright true, the attached pane will be placed down or right in the split, if false then up or left"
  [layout vertical downright]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (if (nil? stack-path) (break layout))
  (def attach-id (layout/attach-id layout))
  (def layout (layout/remove-node layout attach-path))
  (def a (new-bordered-pane attach-id :attach true))
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
  action/split-stacked-pane-up
  "split stacked pane up"
  # splits the currently attached stack into a vertical split, where the active pane becomes the top child
  (layout/set (layout/split-attached-in-stack (layout/get) true false))
)

(key/action
  action/split-stacked-pane-down
  "split stacked pane down"
  # splits the currently attached stack into a vertical split, where the front pane becomes the bottom child
  (layout/set (layout/split-attached-in-stack (layout/get) true true))
)

(key/action
  action/split-stacked-pane-left
  "split stacked pane left"
  # splits the currently attached stack into a horizontal split, where the front pane becomes the left child
  (layout/set (layout/split-attached-in-stack (layout/get) false false))
)

(key/action
  action/split-stacked-pane-right
  "split stacked pane right"
  # splits the currently attached stack into a horizontal split, where the front pane becomes the right child
  (layout/set (layout/split-attached-in-stack (layout/get) false true))
)

(defn layout/move-stacked-pane
  ``
  moves the active pane in the current stack directionally to another pane or stack
  if moving to a pane, creates a new stack
  if moving to a stack, the pane is placed in front of the currently active stack
  direction is :up, :down, :left, or :right
  ``
  [layout direction]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (if (nil? stack-path) (break))

  (def pane (as-> (layout/path layout stack-path) _
    # (layout/get-stack-panes _)
    # (get _ (- (length _) 1))
    (_ :leaves)
    (find |($ :active) _)
    (_ :node)
    (_ :id)
  ))

  (def move-path (layout/find-nearest-node layout stack-path direction [:pane :stack]))
  # we want to replace a pane's borders node if it exists rather than the pane
  (def move-node (layout/path layout move-path))
  (when (layout/type? :pane move-node)
    (def parent-path (trim move-path))
    (def parent-node (layout/path layout parent-path))
    (if (layout/type? :borders parent-node) (array/pop move-path))
  )

  (-> layout
    (layout/remove-node attach-path)
    (layout/add-stacked-pane move-path pane :attach true)
  )
)

(key/action
  action/move-stacked-pane-left
  "move stacked pane left"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-pane layout :left)))
)

(key/action
  action/move-stacked-pane-right
  "move stacked pane right"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-pane layout :right)))
)

(key/action
  action/move-stacked-pane-up
  "move stacked pane up"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-pane layout :up)))
)

(key/action
  action/move-stacked-pane-down
  "move stacked pane down"

  (def layout (layout/get))
  (layout/set (activate-tab (layout/move-stacked-pane layout :down)))
)

(key/action
  action/break-pane-new-tab
  "break the attached pane into a new tab"

  (def layout (layout/get))
  (def attach-path (layout/attach-path layout))
  (def attach-id (layout/attach-id layout))
  (def layout (layout/remove-node layout attach-path))

  # (def tabs-path (layout/find-last layout attach-path |(= ($ :type) :tabs)))
  (layout/set (layout/new-tab layout (new-bordered-pane attach-id) :attach true))
)

(defn
  custom/split-right
  ```Split the currently attached pane into two horizontally, replacing the right pane with the given node.```
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
  ```Split the currently attached pane into two horizontally, replacing the left pane with the given node.```
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
  ```Split the currently attached pane into two vertically, replacing the bottom pane with the given node.```
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
  ```Split the currently attached pane into two vertically, replacing the top pane with the given node.```
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
  custom-pane-creator
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
          (,new-bordered-pane shell :attach true)
          # {:type :pane :id shell :attached true}))))
        )
      )
    )
  )
)

(custom-pane-creator
  action/split-right
  "Split the current pane to the right."
  custom/split-right)

(custom-pane-creator
  action/split-left
  "Split the current pane to the left."
  custom/split-left)

(custom-pane-creator
  action/split-up
  "Split the current pane upwards."
  custom/split-up)

(custom-pane-creator
  action/split-down
  "Split the current pane downwards."
  custom/split-down)

(key/action
  action/grow-pane
  "grow pane"

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
  action/shrink-pane
  "shrink pane"

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

# ----- MODE IMPLEMENTATION -----
# entering a mode puts a bar at the top of the screen displaying the mode
# all keys are unbound and a new set of keybinds is created
# the original keybinds are restored when exiting the mode
# in general, ctrl+alt+/ should show the available actions in the mode, and ctrl+alt+q should leave the mode

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

(key/action
  action/exit-mode
  "exit the current mode"

  (if (nil? (param/get :mode :target :client)) (break)) # not in a mode
  (def restore-bindings (param/get :restore-bindings :target :client))
  (param/set :client :restore-bindings nil)
  (param/set :client :mode nil)

  (key/unbind :root [])
  (each binding restore-bindings
    (key/bind :root (key-conv (get binding :sequence)) (get binding :function))
  )
  (def layout (layout/get))
  (layout/set (get layout :node))
)

(defn enter-mode
  "bindings: an arrtup of arrtups, where each nested arrtup has 2 elements: the 1st is the key sequence (also an arrtup), the 2nd the function to call"
  [name &named exit-binding bindings &opt unbind-existing]
  (default unbind-existing true)

  (param/set :client :mode name)
  (param/set :client :restore-bindings (key/current))
  (if unbind-existing
    (key/unbind :root [])
  )
  (each binding bindings
    (key/bind :root ;binding)
  )
  (key/bind :root exit-binding action/exit-mode)

  (layout/set {
    :type :bar
    :text name
    :node (layout/get)
  })
)

(key/action
  action/test-mode
  "mode test"

  (enter-mode "TEST MODE"
    :exit-binding ["esc"]
    :bindings [
      [["n"] action/add-stacked-pane]
    ]
    :unbind-existing false
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
    #         :a (new-bordered-pane (shell/new) :attach true)
    #         :b (new-bordered-pane (get-logs-pane) :title "  cy log")
    #       }
    #     }
    #     {
    #       :name "tab 2"
    #       :node (new-bordered-pane (shell/new))
    #     }
    #   ]
    # }
    {
      :type :tabs
      :tabs @[
        {
          :name "1"
          :node {
            :type :split
            :vertical false
            :border :none
            :a (new-bordered-pane (shell/new))
            :b (new-bordered-pane (get-logs-pane) :title "  cy log")
          }
        }
        {
          :name "2"
          :active true
          :node {
            :type :split
            :vertical false
            :border :none
            :a (new-bordered-pane (shell/new) :attach true)
            :b (new-bordered-pane (get-logs-pane) :title "  cy log")
          }
        }
        {
          :name "3"
          :node {
            :type :split
            :vertical false
            :border :none
            :a (new-bordered-pane (shell/new))
            :b (new-bordered-pane (get-logs-pane) :title "  cy log")
          }
        }
        {
          :name "4"
          :node {
            :type :split
            :vertical false
            :border :none
            :a (new-bordered-pane (shell/new))
            :b (new-bordered-pane (get-logs-pane) :title "  cy log")
          }
        }
      ]
    }
  )
)

(defn layout/move-focus
  ``
  given a layout and a direction, detach the attached pane and attach to the nearest pane or stack in the given direction
  returns the modified layout
  if attaching to a stack, attaches to the stack's active leaf
  direction is :up, :down, :left, or :right
  ``
  [layout direction]

  (def attach-path (layout/attach-path layout))
  (def stack-path (layout/find-stack layout attach-path))
  (def attach-path (if stack-path stack-path attach-path))
  (def nearest-path (layout/find-nearest-node layout attach-path direction [:pane :stack]))
  (if (nil? nearest-path) (break layout))

  (def nearest-node (layout/path layout nearest-path))

  (as-> layout _
    (layout/detach _)
    (cond
      (layout/type? :pane nearest-node)
        (layout/attach _ nearest-path)
      (layout/type? :stack nearest-node) (do
        (def leaves (nearest-node :leaves))
        (def active-leaf-index (find-index |($ :active) leaves))
        (def active-leaf (leaves active-leaf-index))
        (def pane-path (layout/find (active-leaf :node) |(layout/type? :pane $)))
        (layout/attach _ [;nearest-path :leaves active-leaf-index :node ;pane-path])
      )
    )
    (activate-tab (activate-stack-leaf _))
  )
)

(key/action
  action/focus-right
  "focus the pane to the right, inc. across tabs"

  (def layout (layout/get))
  (layout/set (layout/move-focus layout :right))
)

(key/action
  action/focus-left
  "focus the pane to the left inc. across tabs"

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
  (layout/set (layout/new-tab layout (new-bordered-pane (shell/new)) :attach true))
)

(defn swap-pane-left
  ""
  [layout path]
)

# ----- GENERAL CONFIG -----
(param/set :root :animate false)

# keybinds
(key/unbind :root ["ctrl+l"])

(key/bind :root ["ctrl+alt+p"] action/command-palette)

(key/bind :root ["ctrl+alt+h"] action/focus-left)
(key/bind :root ["ctrl+alt+j"] action/focus-down)
(key/bind :root ["ctrl+alt+k"] action/focus-up)
(key/bind :root ["ctrl+alt+l"] action/focus-right)

(key/bind :root ["ctrl+alt+shift+h"] action/move-stacked-pane-left)
(key/bind :root ["ctrl+alt+shift+j"] action/move-stacked-pane-down)
(key/bind :root ["ctrl+alt+shift+k"] action/move-stacked-pane-up)
(key/bind :root ["ctrl+alt+shift+l"] action/move-stacked-pane-right)

(key/bind :root ["ctrl+alt+u"] action/rotate-stack-backward)
(key/bind :root ["ctrl+alt+o"] action/rotate-stack-forward)
(key/bind :root ["ctrl+alt+shift+u"] action/reorder-stack-backward)
(key/bind :root ["ctrl+alt+shift+o"] action/reorder-stack-forward)

(key/bind :root ["ctrl+alt+n"] action/add-stacked-pane)
(key/bind :root ["ctrl+alt+shift+n"] action/add-stacked-pane-empty)

(key/bind :root ["ctrl+alt+d"] action/remove-layout-pane)
(key/bind :root ["ctrl+alt+x"] action/kill-layout-pane)

# for intial compatibility while I transition from zellij
(key/bind :root ["f1"] action/focus-left)
(key/bind :root ["f2"] action/focus-down)
(key/bind :root ["f3"] action/focus-up)
(key/bind :root ["f4"] action/focus-right)
(key/bind :root ["ctrl+7"] action/remove-layout-pane)
(key/bind :root ["f7"] action/kill-layout-pane)
(key/bind :root ["f5"] action/add-stacked-pane)
(key/bind :root ["f10"] action/rotate-stack-backward)
(key/bind :root ["f11"] action/rotate-stack-forward)

(key/action
  action/init-client
  "should be run when a client initializes"

  # (param/set :client :pusheon-exit true)
  (set-test-layout)
)

(defn hook/init []
  (set-test-layout)
)

# ----- SANDBOX -----
