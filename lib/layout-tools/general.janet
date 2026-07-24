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

