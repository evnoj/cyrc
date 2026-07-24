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
  keep-actions is a strable where keys are strings corresponding to action names that shouldn't be removed, has no effect if remove-actions is false. values are bools that if true will cause keybinds bound to that action not to be removed even if unbind-keys is true.
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
    (eachk name (or keep-actions {}) (put kept name (existing-actions name)))
    (each [sequence name] (or bindings []) (do
      (def existing-action (existing-actions name))
      (if existing-action (put kept name existing-action))
    ))
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
  (def current-bindings (key/current))
  (param/set :client :restore-bindings current-bindings)

  (when unbind-keys
    (if keep-actions (do
      (each binding current-bindings
        (if (not (keep-actions (disasm (binding :function) :name)))
          (key/unbind :root (key-conv (binding :sequence)))
        )
      )
    ) (do
      (key/unbind :root [])
    ))
  )

  (when bindings
    (each binding bindings
      (def [keys action] binding)
      (if (= (type action) :string) (do
        # look up the action function
        (def act ((key/get-actions) action))
        (if (nil? act)
          (msg/log :error (string "unable to find action " action " to bind while entering mode " name))
          (key/bind :root keys (act 1)) # act is a tuple like [docstring func]
        )
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

