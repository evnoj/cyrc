(defn get-logs-pane
  "Get the NodeID of the logs pane"
  []

  (find |(= (tree/path $) "/logs") (group/leaves :root))
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
  "insert whitespace padding between the strings left and right so the concatenated string has the specified length"
  [left right len]

  (def left-len (length left))
  (def right-len (length right))
  (def padding (- len left-len right-len))
  (if (pos? padding)
    (string left (string/repeat " " padding) right)
    (string left right)
  )
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

(key/action
  action/print-commands
  "print the command state of the attached pane."

  (def {:id id} (layout/path (layout/get) (layout/attach-path (layout/get))))
  (def cmds (cmd/commands id))
  (pretty-log
    (string "n=" (length cmds) " last-pending=" (when (not (empty? cmds))
      ((last cmds) :pending))
    )
  )
)


# ----- TITLE STUFF -----
# title is derived from a param :title on a pane that is a table
# the table has 3 fields: :prefix, :body, :postfix
# the values of these tables are arrays
# each element of the array is either a string, function, or strable
# if string, it is text that will be styled according to the default styling
# if function
# if strable, it has 2 keys :style and :text
# :text is either a string or function that returns a string
# it will be rendered with style :style via style/render
(defn render-text-array
  [text-array &named default-style &opt style]
  (default style true)

  (var buff @"")
  (each elem text-array (do
    (def t (type elem))
    (buffer/push-string buff (cond
      (= :string t) (if style
        (style/render default-style elem)
        elem
      )
      (or (= :struct t) (= :table t)) (do
        (if style
          (style/render (elem :style) (elem :text))
          (elem :text)
        )
      )
      ""
    ))
  ))

  (string buff)
)

(defn pane-display-title
  [pane &opt &named style attached dimensions]
  (default style true)

  (if (or (nil? pane) (not (tree/pane? pane))) (break))

  # (def title (cond
  #   (nil? pane) "󰆢  empty"
  #   (or (param/get :title :target pane) (cmd/title pane))
  # ))
  # (if (zero? (length title)) (break title))

  (def title (or (param/get :title :target pane) @{}))
  (def theme (param/get :theme :target pane))
  (def attach-str (if attached "attached" "unattached"))

  # when building the string to set as the title,
  # must also style leading/trailing spaces,
  # bare whitespace is automatically trimmed
  # (def prefix (if (title :prefix)
  #   (render-text-array
  #     [" " ;(title :prefix)]
  #     :style style
  #     :default-style (theme (keyword "title-prefix-" attach-str))
  #   )
  # ))
  (def prefix (case (type (title :prefix))
    :nil nil
    :string (if style
      (style/render
        (theme (keyword "title-prefix-" attach-str))
        (string " " (title :prefix)))
      (string " " (title :prefix))
    )
    (render-text-array
      [" " ;(title :prefix)]
      :style style
      :default-style (theme (keyword "title-prefix-" attach-str))
    )
  ))

  # (def body (render-text-array
  #   [" " ;(or (title :body) [(cmd/title pane)]) " "]
  #   :style style
  #   :default-style (theme (keyword "title-body-" attach-str))
  # ))

  (def body (case (type (title :body))
    :nil (if style
      (style/render (theme (keyword "title-prefix-" attach-str)) (string " " (cmd/title pane) " "))
      (string " " (cmd/title pane) " ")
    )
    :string (if style
      (style/render
        (theme (keyword "title-body-" attach-str))
        (string " " (title :body) " "))
      (string " " (title :body) " ")
    )
    (render-text-array
      [" " ;(title :body) " "]
      :style style
      :default-style (theme (keyword "title-body-" attach-str))
    )
  ))

  (def bell (if (param/get :bell-rung :target pane)
    (if style
      (style/render (theme :title-bell) "  ")
      "  "
    )
    ""
  ))

  # (def postfix (if (title :postfix)
  #   (string " " (render-text-array
  #      # (style/render (theme (keyword "view-border-" attach-str)) "─") " "
  #     (title :postfix)
  #     :style style
  #     :default-style (theme (keyword "title-postfix-" attach-str))
  #   ))
  # ))

  (string bell prefix body)
)

(defn border-title
  [dimensions node]

  (if (node :attached) (param/set (node :id) :bell-rung false))
  (pane-display-title (node :id) :attached (node :attached) :dimensions dimensions)
)

(defn border-fg
  [node]

  (if (layout/attached? node)
    view-attached-border-fg
    view-unattached-border-fg
  )
)

(defn new-bordered-view
  "Create a view with a border. Takes the pane ID that should be in the view."
  [pane &opt &named attach]

  (default attach false)

  {
    :type :borders
    # :title (style/text " sample title  " :bg "#1F1F28" :bold true)
    :title (pane-display-title pane :attached attach)
    :border :rounded
    :border-fg border-fg
    :node {
      :type :view
      :id pane
      :attached attach
    }
  }
)

# pass :none for a key to set it to nil
(defn set-title
  [&opt &named body prefix postfix pane]

  (default pane (pane/current))
  (if (or (nil? pane) (not (tree/pane? pane))) (break))

  (def title (param/get :title :target pane))
  (def need-persist (nil? title))
  (default title @{})

  (if prefix (put title :prefix (if (= :none prefix) nil prefix)))
  (if body (put title :body (if (= :none body) nil body)))

  (if need-persist (param/set pane :title title))

  (var layout (layout/get))
  (def view-path (layout/find layout |(= ($ :id) pane)))
  (if (nil? view-path) (break))
  (def view (layout/path layout view-path))

  (def border-path (layout/find-last layout view-path |(layout/type? :borders $)))
  (when border-path
    (def border (as?-> border-path x
      (layout/path layout x)
      (assoc x :title (pane-display-title pane :attached (view :attached)))
    ))
    (set layout (layout/assoc layout border-path border))
  )

  (def leaf-path (as?-> view-path x
    (layout/find-last layout x |(layout/type? :stack $))
    (array/slice view-path 0 (+ (length x) 2))
  ))
  (when leaf-path
    (def leaf (as?-> leaf-path x
      (layout/path layout x)
      (assoc x :title (pane-display-title pane :attached (view :attached)))
    ))
    (set layout (layout/assoc layout leaf-path leaf))
  )

  (layout/set layout)
)
