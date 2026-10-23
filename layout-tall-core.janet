# Pure layout logic — no FFI, no jwno/jw32 imports.
# layout-tall.janet sets :close-frame-hook and :async-retile-hook on the layout object.
#
# Tree shape produced by this layout (jwno semantics):
#   1 window : top holds the window directly
#   2 windows: top = [master(win), stack(win)]          <- stack holds a window
#   3+       : top = [master(win), stack[sub0, sub1, ...]]

(defn- frame-node? [node]
  (and node (= :frame (in node :type))))

(defn- first-leaf
  "Follow the first child down to a frame that holds windows (or nothing)."
  [fr]
  (var f fr)
  (while (frame-node? (get-in f [:children 0]))
    (set f (get-in f [:children 0])))
  f)

(defn tall-on-window-created [self win uia-win exe-path desktop-info]
  (def filter-result
    (:call-filter-hook
       (in self :hook-manager)
       :and
       :filter-auto-layout-window
       win uia-win exe-path desktop-info))
  (unless filter-result
    (break))

  (def {:window-manager window-man
        :async-retile-hook retile-hook} self)
  (def cur-frame
    (and desktop-info
         (:get-current-frame-on-desktop
           (in window-man :root)
           (in desktop-info :id))))

  (unless (or (nil? cur-frame)
              (empty? (in cur-frame :children)))

    (def top-frame (:get-top-frame cur-frame))
    (def children (in top-frame :children))

    (cond
      # top still holds windows directly
      (not (frame-node? (first children)))
      (when (= (length children) 1)
        (:split top-frame :horizontal)
        (put (in win :tags) :frame (get-in top-frame [:children 1]))
        (retile-hook window-man top-frame))

      # top is split into master / stack
      (frame-node? (get-in top-frame [:children 1]))
      (do
        (def stack (get-in top-frame [:children 1]))
        (def target
          (if (frame-node? (get-in stack [:children 0]))
            (do
              (:insert-sub-frame stack -1)
              (last (in stack :children)))
            (do
              # stack holds window(s) directly: split it, existing
              # windows go to the first sub-frame
              (:split stack :vertical)
              (get-in stack [:children 1]))))
        (put (in win :tags) :frame target)
        (retile-hook window-man stack)))))

(defn- close-empty-frame [window-man fr close-hook retile-hook]
  (def to-retile (in fr :parent))
  (close-hook window-man (fn [] (:close fr)))
  (:layouts-changed window-man [(:get-layout to-retile)])
  (retile-hook window-man to-retile)
  to-retile)

(defn tall-cleanup-layout [window-man win close-hook retile-hook]
  # jwno has already removed `win` from its frame; :parent is left
  # pointing at the old frame.
  (var frame (in win :parent))
  (unless (and frame (:attached? frame))
    (break))

  (def top-frame (:get-top-frame frame))
  (def master-frame (get-in top-frame [:children 0]))
  (def stack-frame  (get-in top-frame [:children 1]))

  (unless (and (frame-node? master-frame) (frame-node? stack-frame))
    (break))

  # Master became empty: promote the first stack window.
  # (The stack may hold windows directly in the 2-window layout.)
  (when (and (= frame master-frame)
             (empty? (in master-frame :children)))
    (def src (first-leaf stack-frame))
    (def stack-win (or (:get-current-window src)
                       (get-in src [:children 0])))
    (when stack-win
      (:remove-child src stack-win)
      (:add-child master-frame stack-win)
      (:retile window-man top-frame)
      (set frame src)))

  # Close the frame left empty. Each frame is closed at most once:
  # jwno's :close merges the remaining sibling into the parent, so the
  # loop normally stops after one step.
  (while (and (frame-node? frame)
              (not= frame top-frame)
              (:attached? frame)
              (empty? (in frame :children))
              (nil? (:get-current-window frame)))
    (set frame (close-empty-frame window-man frame close-hook retile-hook))))

(defn tall-on-window-removed [self win]
  (def {:window-manager window-man
        :close-frame-hook close-hook
        :async-retile-hook retile-hook} self)
  (tall-cleanup-layout window-man win close-hook retile-hook))

(defn tall-swap-master
  ``Swap the focused window with the master window.
  `focused-win` is optional; when given (e.g. the window found from the
  real foreground hwnd) it is used instead of jwno's current frame, which
  can lag behind the actual focus for some apps such as Firefox.``
  [window-man &opt focused-win]
  (def cur-frame
    (if focused-win
      (in focused-win :parent)
      (:get-current-frame (in window-man :root))))
  (unless (and cur-frame (:attached? cur-frame))
    (break))

  (def top-frame (:get-top-frame cur-frame))
  (def master-frame (get-in top-frame [:children 0]))
  (def stack-frame (get-in top-frame [:children 1]))

  (unless (and (frame-node? master-frame) (frame-node? stack-frame))
    (break))

  (def focused (or focused-win (:get-current-window cur-frame)))
  (unless focused
    (break))

  (when (= cur-frame master-frame)
    (break))

  (def master-win (:get-current-window master-frame))
  (unless master-win
    (break))

  (:remove-child cur-frame focused)
  (:remove-child master-frame master-win)
  (:add-child master-frame focused)
  (:add-child cur-frame master-win)

  (:retile window-man top-frame)
  (:activate focused))
