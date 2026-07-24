(param/set :root :animate false)

(defn
  set-title
  [title]
  (def pane (pane/current))
  (if pane (do
             (param/set pane :title title) (layout/set (layout/get)))))

(defn
  border-title-dynamic
  [dimensions node]
  (def pane (node :id))
  (def title (or (param/get :title :target pane) (cmd/title pane)))
  (style/text (string " " title " ") :bg "#1F1F28" :bold true))

(defn
  hook/init
  []
  (layout/set {:type :borders
               :title border-title-dynamic
               :node {:type :pane
                      :attached true
                      :id (shell/new)}}))
