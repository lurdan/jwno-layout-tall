#
# xmonad like Tall layout
#
# To use it:
#
#     (def layout-tall (layout-tall/tall jwno/context))
#     (:enable layout-tall)
#
# To stop it:
#
#     (:disable layout-tall)
#

(use jw32/_uiautomation)

(import jwno/util)
(import jwno/auto-layout)
(import jwno/log)

(defn tall-on-window-created [self win uia-win exe-path desktop-info]
  (def filter-result
    (:call-filter-hook
       (in self :hook-manager)
       :and
       :filter-auto-layout-window
       win uia-win exe-path desktop-info))
  (unless filter-result
    (break))

  (def {:window-manager window-man} self)
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
      # 0 window - full screen master
      (= (length wins) 0)
      (put (in win :tags) :frame (get-in top-frame [:children 0]))
      # 1 window → master / stack split
      (= (length wins) 1)
      (do
        (:split top-frame :horizontal) # 左 master / 右 stack
        (put (in win :tags) :frame (get-in top-frame [:children 1]))
        (ev/spawn
         (:retile window-man top-frame)))
      # else →split stack
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
         (ev/spawn
          (:retile window-man stack))))))

(defn tall-on-window-removed [self win]
  (def {:window-manager window-man} self)

  (var frame (in win :parent))
  (unless (:attached? frame)
    (break))

  (var top-frame (:get-top-frame frame))
  (var master-frame (get-in top-frame [:children 0]))
  (var stack-frame  (get-in top-frame [:children 1]))

  (unless stack-frame
    (break))

  # when master window closed
  (when (= frame master-frame)
    # move stack window to master
    (var second-frame (get-in stack-frame[:children 0]))
    (var stack-win (:get-current-window second-frame))
    (when stack-win
      (:remove-child stack-frame stack-win)
      (:add-child master-frame stack-win)
      (:retile window-man top-frame)
      (set frame second-frame)
      ))

  # delete unused frame
  (when (and (empty? (in frame :children))
             # Don't touch the top-level frame
             (nil? (in frame :monitor)))
    (util/with-activation-hooks window-man
                                (:close frame))
    (def to-retile (in frame :parent))
    (:layouts-changed window-man [(:get-layout to-retile)])
    # ev/spawn to put the :retile call in the event queue
    (ev/spawn
     (:retile window-man to-retile))))

(defn tall-enable [self]
  (:disable self)

  (def {:hook-manager hook-man} self)

  (unless auto-layout/auto-layout-default-filter-hook-fn
    (set auto-layout/auto-layout-default-filter-hook-fn
         (:add-hook hook-man
                    :filter-auto-layout-window
                    auto-layout/auto-layout-default-filter)))

  (put self :hook-fns
     [(:add-hook hook-man :window-created
         (fn [& args] ((in self :on-window-created) self ;args)))
      (:add-hook hook-man :window-removed
         (fn [& args] ((in self :on-window-removed) self ;args)))
     ]))

(defn tall-disable [self]
  (def {:hook-manager hook-man
        :hook-fns hook-fns} self)

  (each hook hook-fns
    (:remove-hook hook-man hook))
  (put self :hook-fns []))

(def tall-proto
  @{:on-window-created tall-on-window-created
    :on-window-removed tall-on-window-removed
    :enable tall-enable
    :disable tall-disable
   })

(defn tall [context]
  (def {:window-manager window-man
        :hook-manager hook-man} context)

  (table/setproto
   @{:window-manager window-man
     :hook-manager hook-man
     :hook-fns @{}
    }
   tall-proto))
