# Pure layout logic — no FFI, no jwno/jw32 imports.
# layout-tall.janet sets :close-frame-hook and :async-retile-hook on the layout object.

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
    (def wins (in top-frame :children))

    (cond
      (= (length wins) 0)
      (put (in win :tags) :frame (get-in top-frame [:children 0]))

      (= (length wins) 1)
      (do
        (:split top-frame :horizontal)
        (put (in win :tags) :frame (get-in top-frame [:children 1]))
        (retile-hook window-man top-frame))

      :else
      (do
        (var stack (get-in top-frame [:children 1]))
        (def target
          (cond
            (nil? (get-in stack [:children 1]))
            (do
              (:split stack :vertical)
              (get-in stack [:children 1]))
            :else
            (do
              (:insert-sub-frame stack -1)
              (last (in stack :children)))))

        (put (in win :tags) :frame target)
        (retile-hook window-man stack)))))

(defn tall-cleanup-layout [window-man win close-hook retile-hook]
  (var frame (in win :parent))
  (unless (:attached? frame)
    (break))

  (var top-frame (:get-top-frame frame))
  (var master-frame (get-in top-frame [:children 0]))
  (var stack-frame  (get-in top-frame [:children 1]))

  (unless stack-frame
    (break))

  (when (= frame master-frame)
    (var second-frame (get-in stack-frame [:children 0]))
    (var stack-win (:get-current-window second-frame))
    (when stack-win
      (:remove-child second-frame stack-win)
      (:add-child master-frame stack-win)
      (:retile window-man top-frame)
      (set frame second-frame)))

  (when (and (nil? (:get-current-window frame))
             (empty? (in frame :children))
             (nil? (in frame :monitor)))
    (def to-retile (in frame :parent))
    (close-hook window-man (fn [] (:close frame)))
    (:layouts-changed window-man [(:get-layout to-retile)])
    (retile-hook window-man to-retile))

  (when (empty? (in stack-frame :children))
    (def to-retile (in stack-frame :parent))
    (close-hook window-man (fn [] (:close stack-frame)))
    (:layouts-changed window-man [(:get-layout to-retile)])
    (retile-hook window-man to-retile)))

(defn tall-on-window-removed [self win]
  (def {:window-manager window-man
        :close-frame-hook close-hook
        :async-retile-hook retile-hook} self)
  (tall-cleanup-layout window-man win close-hook retile-hook))

(defn tall-swap-master [window-man]
  (var cur-frame (:get-current-frame (in window-man :root)))
  (unless cur-frame
    (break))

  (var top-frame (:get-top-frame cur-frame))
  (var master-frame (get-in top-frame [:children 0]))
  (var stack-frame (get-in top-frame [:children 1]))

  (unless (and master-frame stack-frame)
    (break))

  (var focused (:get-current-window cur-frame))
  (unless focused
    (break))

  (var master-win (:get-current-window master-frame))
  (unless master-win
    (break))

  (when (= focused master-win)
    (break))

  (:remove-child cur-frame focused)
  (:remove-child master-frame master-win)
  (:add-child master-frame focused)
  (:add-child cur-frame master-win)

  (:retile window-man top-frame)
  (:activate focused))
