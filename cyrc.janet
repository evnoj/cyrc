# ----- VARIABLES -----
(def tab-active-fg "21")
(def tab-active-bg "20")

(def tab-inactive-fg "23")
(def tab-inactive-bg "22")
(def view-attached-border-fg "24")
(def view-unattached-border-fg "25")
(def background "26")
(def foreground "27")

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

# ----- ACTIONS -----
# copy mode keybinds
(key/action
  action/copy-mode
  "open copy mode"

  (replay/open (pane/current) :copy true)
  # (pane/send-keys (pane/current) @["v"])
  # (replay/select)
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

# ----- GENERAL CONFIG -----
(defn hook/init []
  # (param/set (tree/id :root "/logs") :title "  cy log")
  (param/set (tree/id :root "/logs") :title @{:body "  cy log"})

  (def theme @{
    :tab-active {
      :bg "20"
      :fg "21"
    }
    :tab-inactive {
      :bg "22"
      :fg "23"
    }
    :view-border-attached {
      :fg "24"
    }
    :view-border-unattached {
      :fg "25"
    }
    :title-prefix-attached {
      :fg "26"
      :bg "27"
      :bold true
    }
    :title-prefix-unattached {
      :fg "27"
      :bg "26"
      :bold true
    }
    :title-body-attached {
      :fg "26"
      :bg "27"
      :bold true
    }
    :title-body-unattached {
      :fg "27"
      :bg "26"
      :bold true  
    }
    :title-postfix-attached {
      :fg "26"
      :bg "27"
      :bold true   
    }
    :title-postfix-unattached {
      :fg "27"
      :bg "26"
      :bold true     
    }
    :title-bell {
      :fg "3"
      :bg "26"
    }
  })
  (param/set :root :theme theme)

  (layout/set (new-bordered-view (shell/new) :attach true))
)

(defn hook/bell [pane]
  (when (not= (layout/attach-id (layout/get)) pane)
    (msg/toast :info (string "  from " (pane-display-title pane :style false)))
    (param/set pane :bell-rung true)
    (set-title :pane pane)

    # set tab bell icon
    (def layout (layout/get))
    (def view-path (layout/find layout |(= ($ :id) pane)))
    (def tab-path (as?-> view-path x
      (layout/find-last layout x |(layout/type? :tabs $))
      (array/slice view-path 0 (+ (length x) 2))
    ))
    (when tab-path
      (def tab (layout/path layout tab-path))
      (var tab-name (tab :name))
      (when (not (string/has-prefix? "  " tab-name))
        (set tab-name (string "  " tab-name))
        (layout/set (layout/assoc layout tab-path (assoc tab :name tab-name)))
      )
    )

  )
)

(defn hook/title [pane title]
  (def title (param/get :title :target pane))
  (when (nil? (title :body))
    (set-title :pane pane)
  )
)

(defn hook/attach [prev-pane attach-pane layout]
  (def attach-path (layout/attach-path layout))
  (def layout (or (when (param/get :bell-rung :target attach-pane)
    (param/set attach-pane :bell-rung false)

    (def tab-path (as?-> attach-path x
      (layout/find-last layout x |(layout/type? :tabs $))
      (array/slice attach-path 0 (+ (length x) 2))
    ))
    (or (when tab-path
      (def tab (layout/path layout tab-path))
      (var tab-name (tab :name))
      (or (when (string/has-prefix? "  " tab-name)
        (set tab-name (string/slice tab-name (length "  ")))
        (layout/assoc layout tab-path (assoc tab :name tab-name))
      ) layout)
    ) layout)
  ) layout))

  (layout/set (as-> layout _
    (set-title :pane attach-pane :layout _)
    (or (set-title :pane prev-pane :layout _) _)
  ))
)

# ----- THEMING -----
# (color-maps/set :root :kanagawa)
# (def theme "kanagawa")

# (param/set :root :theme (eval-string (slurp (string (os/getenv "HOME") "/.config/cy/themes/") theme ".janet") (make-env)))
# (def theme @{
#   :tab-active {
#     :bg "20"
#     :fg "21"
#   }
#   :tab-inactive {
#     :bg "22"
#     :fg "23"
#   }
#   :view-border-attached {
#     :fg "24"
#   }
#   :view-border-unattached {
#     :fg "25"
#   }
#   :title-prefix-attached {
#     :fg "26"
#     :bg "27"
#     :bold true
#   }
#   :title-prefix-unattached {
#     :fg "27"
#     :bg "26"
#     :bold true
#   }
#   :title-body-attached {
#     :fg "26"
#     :bg "27"
#     :bold true
#   }
#   :title-body-unattached {
#     :fg "27"
#     :bg "26"
#     :bold true  
#   }
#   :title-postfix-attached {
#     :fg "26"
#     :bg "27"
#     :bold true   
#   }
#   :title-postfix-unattached {
#     :fg "27"
#     :bg "26"
#     :bold true     
#   }
# })
# (param/set :root :theme theme)

# ----- USED VIA CY EXEC -----
# (defn get-title
#   []
#   (param/get :title :target (pane/current))
# )

# ----- KEYBINDS -----
# use ctrl-m as prefix key instead of ctrl-a
(def prefix-key "ctrl+m")
(key/remap :root ["ctrl+a"] [prefix-key])

(key/bind :root ["ctrl+alt+m"] action/maximize)

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
(key/bind :root ["ctrl+alt+a"] action/jump-pane-by-title)
(key/bind :root [prefix-key "r"] action/rename-tab)

(key/bind :root ["ctrl+alt+u"] action/rotate-stack-backward)
(key/bind :root ["ctrl+alt+o"] action/rotate-stack-forward)
(key/bind :root ["ctrl+alt+shift+u"] action/reorder-stack-backward)
(key/bind :root ["ctrl+alt+shift+o"] action/reorder-stack-forward)

(key/bind :root ["ctrl+alt+n"] action/add-stacked-view)
(key/bind :root ["ctrl+alt+shift+n"] action/add-stacked-view-empty)

(key/bind :root ["ctrl+alt+d"] action/remove-attached-view)
(key/bind :root ["ctrl+alt+x"] action/kill-attached-view-confirm)

# for intial compatibility while I transition from zellij
# (key/bind :root ["f1"] action/focus-left)
# (key/bind :root ["f2"] action/focus-down)
# (key/bind :root ["f3"] action/focus-up)
# (key/bind :root ["f4"] action/focus-right)
# (key/bind :root ["f5"] action/add-stacked-view)
# (key/bind :root ["f6"] action/break-view-new-tab)
# (key/bind :root ["f7"] action/kill-attached-view)
# (key/bind :root ["f10"] action/rotate-stack-backward)
# (key/bind :root ["f11"] action/rotate-stack-forward)
# (key/bind :root ["ctrl+7"] action/remove-attached-view)

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
(param/set-many :root
  :replay-selection-style {
    :bg "18"
    :fg "19"
  }
  :animate false
  :data-directory ""
  :use-system-clipboard true
)

(key/bind :root [prefix-key "s"] action/thumbs-copy)
(key/bind :root [prefix-key "ctrl+s"] action/thumbs-copy)
(key/bind :root ["ctrl+alt+i"] action/thumbs-insert)
(key/bind :root ["ctrl+alt+y"] action/thumbs-copy)
(key/bind :root ["f12"] action/thumbs-insert) # compat during zellij transition
