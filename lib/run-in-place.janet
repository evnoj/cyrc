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
