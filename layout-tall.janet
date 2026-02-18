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

### Auto layout hooks

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

(defn tall-cleanup-frame [window-man win]
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
    (var second-frame (get-in stack-frame [:children 0]))
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

(defn tall-on-window-removed [self win]
  (def {:window-manager window-man} self)
  (tall-cleanup-frame window-man win))

### Window control functions

(ffi/context "user32.dll")
(ffi/defbind IsZoomed :uint32 [hwnd :ptr])
(ffi/defbind ShowWindow :uint32 [hwnd :ptr cmd :uint32])
(defn tall-toggle-maximize [window-man]
  (when-let [cur-fr (:get-current-frame (in window-man :root))
             cur-win (:get-current-window cur-fr)
             hwnd (in cur-win :hwnd)]
    (if (= 0 (IsZoomed hwnd))
      (ShowWindow hwnd 3)
      (ShowWindow hwnd 9))))

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

### Workspace manipulation

(ffi/context "VirtualDesktopAccessor.dll")
(ffi/defbind GoToDesktopNumber :uint32 [n :uint32])
(ffi/defbind MoveWindowToDesktopNumber :uint32 [hwnd :ptr n :uint32])
(ffi/defbind GetCurrentDesktopNumber :uint32 [])
(ffi/defbind GetDesktopCount :uint32 [])
(ffi/defbind CreateDesktop :uint32 [])

(while (< (GetDesktopCount) 9)
  (CreateDesktop))

(defn tall-set-desktop [n]
  (cond
    (= (GetCurrentDesktopNumber) n)
    (log/info (string "Already on Desktop " (+ n 1)))
    (< (GetDesktopCount) (+ n 1))
    (log/info (string "Desktop " (+ n 1) " unavailable"))
    (do
      (GoToDesktopNumber n)
      (log/info (string "Desktop " (+ n 1))))))

(defn tall-move-to-desktop [n window-man]
  (cond
    (= (GetCurrentDesktopNumber) n)
    (log/info (string "Already on Desktop " (+ n 1)))
    (< (GetDesktopCount) (+ n 1))
    (log/info (string "Desktop " (+ n 1) " unavailable"))
    (when-let [cur-fr (:get-current-frame (in window-man :root))
               cur-win (:get-current-window cur-fr)
               hwnd (in cur-win :hwnd)]
      (MoveWindowToDesktopNumber hwnd n)
      (:remove-child cur-fr cur-win)
      (tall-cleanup-frame window-man cur-win)
      (def vd-info (:get-hwnd-virtual-desktop window-man hwnd))
      (def new-fr (:get-current-frame-on-desktop (in window-man :root) vd-info))
      (:add-child new-fr cur-win)
      (:activate cur-win)
      (:retile window-man (:get-top-frame  new-fr))
      # (GoToDesktopNumber n)
      (log/info (string "Desktop " (+ n 1))))))

### Layout management

(defn tall-enable [self]
  (:disable self)

  (def {:window-manager window-man
        :command-manager command-man
        :hook-manager hook-man} self)

  (:add-command command-man :toggle-maximize
     (fn [] (tall-toggle-maximize window-man)))
  (:add-command command-man :swap-master
     (fn [] (tall-swap-master window-man)))
  (:add-command command-man :set-desktop
     (fn [n] (tall-set-desktop n)))
  (:add-command command-man :move-to-desktop
     (fn [n] (tall-move-to-desktop n window-man)))

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
  (def {:window-manager window-man
        :command-manager command-man
        :hook-manager hook-man
        :hook-fns hook-fns} self)

  (:remove-command command-man :toggle-maximize)
  (:remove-command command-man :swap-master)
  (:remove-command command-man :set-desktop)
  (:remove-command command-man :move-to-desktop)

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
        :command-manager command-man
        :hook-manager hook-man} context)

  (table/setproto
   @{:window-manager window-man
     :command-manager command-man
     :hook-manager hook-man
     :hook-fns @{}
    }
   tall-proto))
