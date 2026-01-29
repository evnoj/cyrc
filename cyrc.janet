# helper functions
(defn get-logs-pane
  "Get the NodeID of the logs pane"
  []

  (find |(= (tree/path $) "/logs") (group/leaves :root))
)

(defn get-stack
  "get a struct with all the stacks in the client, or a specific stack if specified"
  [&opt stack-id]
  (def stacks (param/get :stacks :target :client))

  (if stack-id
    (break (get stacks stack-id))
    (break stacks)
  )
)

(defn new-stack-id
  "get an unused stack id"
  []

  (def stacks (get-stack))
  (var id 1)
  (while (get stacks id)
    (set id (+ id 1))
  )
  id
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

  (tuple/slice arrtup 0 (- (length arrtup) n))
)

(defn set-title
  [title]
  (param/set (pane/current) :title title)
)

(defn get-title
  []
  (param/get :title :target (pane/current))
)

(defn render-pane-title
  [pane]
  (def title (param/get :title :target pane))
  (default title (cmd/title pane))
  (style/text (string "╭ " title) :bold true)
)

(defn stack-bar-title
  [dimensions node]
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
  (render-pane-title pane)
)

(defn create-stack-node
  "Creates and returns a margin layout node that contains other layout nodes that represent a stack of panes. Takes a list of the node IDs of the panes in the stack. 0th element is the front of the stack, going backwards (up) from there."
  [panes &opt attach]

  (default attach false)
  {
    :type :margins
    :custom :custom-value
    :node (do
      (var stack (new-bordered-pane (get panes 0) :attach attach))
      (if (<= (length panes) 1) (break stack))

      (for i 1 (length panes)
        (set stack {
          :type :bar
          # :text (style/text (string "╭ " "placeholder") :bold true)
          # :text stack-bar-title
          :text (render-pane-title (get panes i))
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
  "add a stacked pane"

  (def layout (layout/get))
  (def current-pane (layout/attach-id layout))
  (def new-pane (shell/new))
  (def current-path (layout/attach-path layout))
  (def stacks (get-stack))
  (var stack-id (param/get :stack))

  (if stack-id (do
    (msg/log :info "adding to stack")
    (def stack (get stacks stack-id))
    (def panes (get stack :panes))
    (array/insert panes 0 new-pane)
    (param/set new-pane :stack stack-id)
    (layout/set (layout/assoc layout (get stack :path) (create-stack-node panes true)))
  ) (do
    (msg/log :info "new stack")
    (set stack-id (new-stack-id))
    (def panes @[new-pane current-pane])
    # the pane has a border around it, the path to that is what we'll replace
    (def stack-path (trim current-path))
    (def stack {
      :path stack-path
      :panes panes
    })
    (set (stacks stack-id) stack)
    (param/set current-pane :stack stack-id)
    (param/set new-pane :stack stack-id)
    (layout/set (layout/assoc layout stack-path (create-stack-node panes true)))
  ))
)

(key/action
  action/shift-stack-forward
  "shift the current stack forward"

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-id (param/get :stack :target pane))
  (if (nil? stack-id) (break))
  (def stack (get-stack stack-id))
  (def panes (get stack :panes))
  (def front-pane (get panes 0))
  (array/remove panes 0)
  (array/push panes front-pane)
  (layout/set (render-stack layout stack :attach true))
)

(key/action
  action/reorder-stack-forward
  "reorder the current stack forward. Like shift-stack-forward, but leaves the front pane at the front. A way to reposition the front pane in the stack ordering."

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-id (param/get :stack :target pane))
  (if (nil? stack-id) (break))
  (def stack (get-stack stack-id))
  (def panes (get stack :panes))
  (def second-front-pane (get panes 1))
  (array/remove panes 1)
  (array/push panes second-front-pane)
  (layout/set (render-stack layout stack :attach true))
)

(key/action
  action/shift-stack-backward
  "shift the current stack backward"
  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-id (param/get :stack :target pane))
  (if (nil? stack-id) (break))
  (def stack (get-stack stack-id))
  (def panes (get stack :panes))
  (def back-pane (array/pop panes))
  (array/insert panes 0 back-pane)
  (layout/set (render-stack layout stack :attach true))
)

(key/action
  action/reorder-stack-backward
  "reorder the current stack backward. Like shift-stack-backward, but leaves the front pane at the front. A way to reposition the front pane in the stack ordering."

  (def layout (layout/get))
  (def pane (layout/attach-id layout))
  (def stack-id (param/get :stack :target pane))
  (if (nil? stack-id) (break))
  (def stack (get-stack stack-id))
  (def panes (get stack :panes))
  (def back-pane (array/pop panes))
  (array/insert panes 1 back-pane)
  (layout/set (render-stack layout stack :attach true))
)

(defn remove-stacked-pane
  [pane &opt &named kill]

  (def stacks (get-stack))
  (def stack-id (param/get :stack :target pane))
  (def stack (get stacks stack-id))
  (def panes (get stack :panes))
  (def stack-path (get stack :path))
  (var new-node nil)

  (array/remove panes 0)

  (if (= (length panes) 1)
    (do
      (def last-pane (get panes 0))
      (set (stacks stack-id) nil)
      (param/set last-pane :stack nil)
      (set new-node (new-bordered-pane last-pane :attach true))
    ) (do
      (set new-node (create-stack-node panes true))
    )
  )

  (layout/set (layout/assoc (layout/get) stack-path new-node))
  (param/set pane :stack nil)
  (if (not (nil? kill)) (tree/rm pane))
)

(key/action
  action/kill-layout-pane
  "Remove the current pane from the layout and the node tree."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))

  (if (param/get :stack :target id)
    (remove-stacked-pane id :kill true)
    (do
      (layout/set (layout/remove-attached layout))
      (if (not (nil? id)) (tree/rm id))
    )
  )
)

(key/action
  action/remove-layout-pane
  "Remove the current pane from the layout."
  (def layout (layout/get))
  (def {:id id} (layout/path layout (layout/attach-path layout)))

  (if (param/get :stack :target id)
    (remove-stacked-pane id)
    (layout/set (layout/remove-attached layout))
  )
)

(key/action
  action/merge-split-into-stack
  "if the attached pane is a child of a split, or is in a stack that is a child of a split, merge the two halves (which will each be either a pane or a stack) into a single stack, replacing the split with it"
  # TODO
)

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

(key/action
  action/log-layout

  "log layout"

  (set-test-layout)
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
  #           # :a (create-stack-node [(shell/new) (shell/new)])
  #           :a (new-bordered-pane (shell/new) true)
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
)

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

# config
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
