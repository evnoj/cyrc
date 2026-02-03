# helper functions
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

(defn pane-border-title
  [dimensions node]

  (def pane (get node :id))
  (def title (param/get :title :target pane))
  (default title (cmd/title pane))
  (style/text (string " " title " ") :bg "#1F1F28" :bold true)
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

# ----- UTILITY FUNCTIONS -----
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

(defn trim
  "trim the last n elements from an array or tuple, returning a new tuple. n defaults to 1"
  [arrtup &opt n]
  (default n 1)

  (tuple/slice arrtup 0 (- -1 n))
)

(defn set-title
  [title]
  (param/set (pane/current) :title title)
)

(defn get-title
  []
  (param/get :title :target (pane/current))
)

# ----- DEBUGGING ACTIONS -----
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

# ----- STACK IMPLEMENTATION -----
(defn layout/find-stack
```
given a layout and a path, checks if that path is inside a stack, and if so, returns the path to the stack. Returns nil otherwise.
Since we are appropriating the margin layout node as the container that indicates a stack,
it really checks for the first margin node that has the border-fg property set to the string "stack".
Assumes there are no nested stacks, simply returns the path to the last stack node it finds.
```
  [layout path]
  (layout/find-last layout path |(
    and (= (type $) :struct) (= ($ :type) :margins) (= ($ :border-fg) "stack")
    # = ($ :type) :margins
  ))
)

(defn panes-to-string
"takes an arrtup of pane ids (nums) and returns a string of newline-separated pane ids"
[panes]
  (string/join (map |(string $) panes) "\n")
)

(defn string-to-panes
"takes a string of newline-separated pane ids and returns an array of pane ids (nums)"
[panes]
  (map |(scan-number $) (string/split "\n" panes))
)

(defn get-stack-pane-title
  [pane]
  (def title (param/get :title :target pane))
  (default title (cmd/title pane))
  (style/text (string "╭ " title) :bold true)
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

(defn create-stack-node
  "Creates and returns a margin layout node that contains other layout nodes that represent a stack of panes. Takes a list of the node IDs of the panes in the stack (strings). 0th element is the front of the stack, going backwards (up) from there."
  [panes &opt attach]

  (default attach false)

  {
    :type :margins
    :border-fg "stack"
    :border-bg (panes-to-string panes)
    :node (do
      (var stack (new-bordered-pane (get panes 0) :attach attach))
      (if (<= (length panes) 1) (break stack))

      (for i 1 (length panes)
        (set stack {
          :type :bar
          # :text (style/text (string "╭ " "placeholder") :bold true)
          # :text stack-bar-title
          :text (get-stack-pane-title (get panes i))
          :node stack
        })
      )
      stack
    )
  }
)

(defn render-stack
  "Takes a layout and a stack table, and returns the layout modified so the path at (stack :path) contains the created stack node hierarchy"
  [layout stack &opt &named attach]
  (default attach false)

  (def stack-node (create-stack-node (get stack :panes) attach))
  (layout/assoc layout (get stack :path) stack-node)
)

(key/action
  action/add-stacked-pane
  "add a stacked pane at a new shell in the current directory to the focused pane"

  (def layout (layout/get))
  (def current-pane (layout/attach-id layout))
  (def new-pane (shell/new))
  (def current-path (layout/attach-path layout))
  (var stack-path (layout/find-stack layout current-path))

  (if stack-path (do
    # (msg/log :info "adding to stack")
    (def panes (string-to-panes (get (layout/path layout stack-path) :border-bg)))
    (array/insert panes 0 new-pane)
    (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
  ) (do
    # (msg/log :info "new stack")
    (def panes @[new-pane current-pane])
    # the pane has a border around it, the path to that is what we'll replace
    (set stack-path @[;(trim current-path)])
    (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
  ))
)

(key/action
  action/shift-stack-forward
  "shift the current stack forward"

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def panes (string-to-panes (get (layout/path layout stack-path) :border-bg)))
  (def front-pane (get panes 0))
  (array/remove panes 0)
  (array/push panes front-pane)
  (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
)

(key/action
  action/reorder-stack-forward
  "reorder the current stack forward. Like shift-stack-forward, but leaves the front pane at the front. A way to reposition the front pane in the stack ordering."

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def panes (string-to-panes (get (layout/path layout stack-path) :border-bg)))
  (def second-front-pane (get panes 1))
  (array/remove panes 1)
  (array/push panes second-front-pane)
  (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
)

(key/action
  action/shift-stack-backward
  "shift the current stack backward"

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def panes (string-to-panes (get (layout/path layout stack-path) :border-bg)))
  (def back-pane (array/pop panes))
  (array/insert panes 0 back-pane)
  (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
)

(key/action
  action/reorder-stack-backward
  "reorder the current stack backward. Like shift-stack-backward, but leaves the front pane at the front. A way to reposition the front pane in the stack ordering."

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))
  (if (nil? stack-path) (break))
  (def panes (string-to-panes (get (layout/path layout stack-path) :border-bg)))
  (def back-pane (array/pop panes))
  (array/insert panes 1 back-pane)
  (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
)

(defn layout/remove-stacked-pane
  [layout stack-path]

  (def stack-node (layout/path layout stack-path))
  (def panes (string-to-panes (get stack-node :border-bg)))
  (var new-node nil)

  (array/remove panes 0)

  (if (= (length panes) 1)
    (do
      (def last-pane (get panes 0))
      (set new-node (new-bordered-pane last-pane :attach true))
    ) (do
      (set new-node (create-stack-node panes true))
    )
  )

  (layout/assoc layout stack-path new-node)
)

(key/action
  action/remove-layout-pane
  "Remove the current pane from the layout."
  (def layout (layout/get))
  (def stack-path (layout/find-stack layout (layout/attach-path layout)))

  (if stack-path
    (layout/set (layout/remove-stacked-pane layout stack-path))
    (layout/set (layout/remove-attached layout))
  )
)

(key/action
  action/kill-layout-pane
  "Remove the current pane from the layout and the node tree."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))
  # (def stack-path (layout/find-stack layout (layout/attach-path layout))

  # (if stack-path
  #   (layout/set (remove-stacked-pane layout stack-path)
  #   (layout/set (layout/remove-attached layout))
  # )
  (action/remove-layout-pane)

  (if (not (nil? id)) (tree/rm id))
)

(key/action
  action/merge-split-into-stack
  "if the attached pane is a child of a split, or is in a stack that is a child of a split, merge the two halves (which will each be either a pane or a stack) into a single stack, replacing the split with it"
  # TODO
)

(defn
  custom/split-right
  ```Split the currently attached pane into two horizontally, replacing the right pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(layout/split (layout/detach $) node)))

(defn
  custom/split-left
  ```Split the currently attached pane into two horizontally, replacing the left pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(layout/split node (layout/detach $))))

(defn
  custom/split-down
  ```Split the currently attached pane into two vertically, replacing the bottom pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :vertical true
                                        :a (layout/detach $)
                                        :b node})))

(defn
  custom/split-up
  ```Split the currently attached pane into two vertically, replacing the top pane with the given node.```
  [layout node]
  (layout/replace-attached layout |(do {:type :split
                                        :vertical true
                                        :b (layout/detach $)
                                        :a node})))

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
  layout/split-right)

(custom-pane-creator
  action/split-left
  "Split the current pane to the left."
  layout/split-left)

(custom-pane-creator
  action/split-up
  "Split the current pane upwards."
  layout/split-up)

(custom-pane-creator
  action/split-down
  "Split the current pane downwards."
  layout/split-down)

(key/action
  action/grow-pane
  "grow pane"

  # get NodeID of current pane
  (def current (pane/current))
  # get the NodeID of current pane's parent
  (def parent (tree/parent current))
  # (msg/log :info (string parent))
  (msg/log :info (tree/path current))
  # (def layout (layout/get))
  
  # (when (layout/type? :split parent)
  #   # Get the parent split node from the layout
  #   (def parent-path (tree/path parent))
  #   (def parent-node (layout/path layout parent-path))
    
  #   # Check which child we are (`:a` or `:b`)
  #   (def children (group/children parent))
  #   (def is-first (= current (first children)))
    
  #   # Modify the cells, adjusting based on which child we are
  #   (def current-cells (get parent-node :cells 0))
  #   (def new-cells
  #     (if is-first
  #       (+ current-cells 1)
  #       (- current-cells 1)
  #     )
  #   )

  #   # Update the layout with new cell count
  #   (def new-parent (assoc parent-node :cells 5))
  #   (def new-layout (layout/assoc layout parent-path new-parent))
    
  #   (layout/set new-layout)
  # )
)

# ----- MODE IMPLEMENTATION -----
# entering a mode puts a bar at the top of the screen displaying the mode
# all keys are unbound and a new set of keybinds is created
# the original keybinds are restored when exiting the mode
# in general, ctrl+alt+/ should show the available actions in the mode, and ctrl+alt+q should leave the mode
# (defn key-conv
#   "takes a string that represents an element in a keybind sequence. If the string starts with re:, it converts it to the proper format to pass to key/bind and returns the array. Otherwise, returns the same string."
#   [key]
#   (if (= "re:" (string/slice key 0 3))
#     (break [:re (string/slice key 3)])
#   )
#   key
# )

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

  (def restore-bindings (param/get :restore-bindings :client))
  (if (nil? restore-bindings) (break)) # not in a mode
  (param/set :client :restore-bindings nil)

  (key/unbind :root [])
  (each binding restore-bindings
    # (msg/log :info (string "function: " (get binding :function) " seq: " (array-to-string (get binding :sequence))))
    (key/bind :root (key-conv (get binding :sequence)) (get binding :function))
  )
  (def layout (layout/get))
  (layout/set (get layout :node))
)

(defn enter-mode
  "bindings: an arrtup of arrtups, where each nested arrtup has 2 elements: the 1st is the key sequence (also an arrtup), the 2nd the function to call"
  [name &named exit-binding bindings &opt unbind-existing]
  (default unbind-existing true)

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

  # (enter-mode "TEST MODE" ["ctrl+alt+q"])
  (enter-mode "TEST MODE"
    :exit-binding ["esc"]
    :bindings [
      [["n"] action/add-stacked-pane]
    ]
    :unbind-existing false
  )
)

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
            :a (new-bordered-pane (shell/new) :attach true)
            :b (new-bordered-pane (get-logs-pane) :title "  log")
          }
        }
        {
          :name "tab 2"
          :node (new-bordered-pane (shell/new))
        }
      ]
    }
  )
)

# ----- ACTIONS -----
(key/action
  action/custom-move-right
  "move right, inc. across tabs"

  (def layout-current (layout/get))
  (def layout-moved (layout/move-right layout-current))

  # if layout unchanged, try to switch tabs
  (if (= layout-current layout-moved)
    (do
      (def layout-pre-tab-switch (layout/get))
      (action/next-tab)
      (var layout-post-tab-switch (layout/get))

      # if tabs switched, move to the leftmost pane in the new tab
      (if (not= layout-pre-tab-switch layout-post-tab-switch)
        (while true
          (def new-layout-post-tab-switch (layout/move-left layout-post-tab-switch))
          (if (= layout-post-tab-switch new-layout-post-tab-switch)
            (break)
            (set layout-post-tab-switch new-layout-post-tab-switch)
          )
        )
      )
      (layout/set layout-post-tab-switch)
    )
    (layout/set layout-moved)
  )
)

(key/action
  action/custom-move-left
  "move left, inc. across tabs"

  (def layout-current (layout/get))
  (def layout-moved (layout/move-left layout-current))

  (if (= layout-current layout-moved)
    (do
      (def layout-pre-tab-switch (layout/get))
      (action/prev-tab)
      (var layout-post-tab-switch (layout/get))

      (if (not= layout-pre-tab-switch layout-post-tab-switch)
        (while true
          (def new-layout-post-tab-switch (layout/move-right layout-post-tab-switch))
          (if (= layout-post-tab-switch new-layout-post-tab-switch)
            (break)
            (set layout-post-tab-switch new-layout-post-tab-switch)
          )
        )
      )
      (layout/set layout-post-tab-switch)
    )
    (layout/set layout-moved)
  )
)

# ----- GENERAL CONFIG -----
(param/set :root :animate false)

# keybinds
(key/unbind :root ["ctrl+l"])

(key/bind :root ["ctrl+alt+p"] action/command-palette)

(key/bind :root ["ctrl+alt+h"] action/custom-move-left)
(key/bind :root ["ctrl+alt+j"] action/move-down)
(key/bind :root ["ctrl+alt+k"] action/move-up)
(key/bind :root ["ctrl+alt+l"] action/custom-move-right)

(key/bind :root ["ctrl+alt+u"] action/shift-stack-backward)
(key/bind :root ["ctrl+alt+o"] action/shift-stack-forward)
(key/bind :root ["ctrl+alt+shift+u"] action/reorder-stack-backward)
(key/bind :root ["ctrl+alt+shift+o"] action/reorder-stack-forward)

(key/bind :root ["ctrl+alt+n"] action/add-stacked-pane)

(key/bind :root ["ctrl+alt+d"] action/remove-layout-pane)
(key/bind :root ["ctrl+alt+x"] action/kill-layout-pane)

# for intial compatibility while I transition from zellij
(key/bind :root ["f1"] action/custom-move-left)
(key/bind :root ["f2"] action/move-down)
(key/bind :root ["f3"] action/move-up)
(key/bind :root ["f4"] action/custom-move-right)
(key/bind :root ["f7"] action/remove-layout-pane)
(key/bind :root ["ctrl+7"] action/kill-layout-pane)
(key/bind :root ["f5"] action/add-stacked-pane)
(key/bind :root ["f10"] action/shift-stack-backward)
(key/bind :root ["f11"] action/shift-stack-forward)

(key/action
  action/init-client
  "should be run when a client initializes"

  (param/set :client :stacks @{})
  (set-test-layout)
)

# ----- SANDBOX -----
