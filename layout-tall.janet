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
(import layout-tall-core :as core)

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

(defn tall-move-to-desktop [n window-man close-hook retile-hook]
  (cond
    (= (GetCurrentDesktopNumber) n)
    (log/info (string "Already on Desktop " (+ n 1)))
    (< (GetDesktopCount) (+ n 1))
    (log/info (string "Desktop " (+ n 1) " unavailable"))
    (when-let [cur-fr (:get-current-frame (in window-man :root))
               cur-win (:get-current-window cur-fr)
               hwnd (in cur-win :hwnd)]
      (MoveWindowToDesktopNumber hwnd n)
      # FIXME: old frame be left, call tall-on-window-removed?
      (core/tall-cleanup-layout window-man cur-win close-hook retile-hook)
      (:retile window-man (:get-top-frame cur-fr))
      # FIXME: moved window should be controlled
      # (GoToDesktopNumber n)
      (def vd-info (:get-hwnd-virtual-desktop window-man hwnd))
      (def new-fr (:get-current-frame-on-desktop (in window-man :root) vd-info))
      (:add-child new-fr cur-win)
      (:activate cur-win)
      (log/info (string "Desktop " (+ n 1))))))

### Layout management

(defn tall-enable [self]
  (:disable self)

  (def {:window-manager window-man
        :command-manager command-man
        :hook-manager hook-man
        :close-frame-hook close-hook
        :async-retile-hook retile-hook} self)

  (:add-command command-man :toggle-maximize
     (fn [] (tall-toggle-maximize window-man)))
  (:add-command command-man :swap-master
     (fn [] (core/tall-swap-master window-man)))
  (:add-command command-man :set-desktop
     (fn [n] (tall-set-desktop n)))
  (:add-command command-man :move-to-desktop
     (fn [n] (tall-move-to-desktop n window-man close-hook retile-hook)))

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
  @{:on-window-created core/tall-on-window-created
    :on-window-removed core/tall-on-window-removed
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
     :close-frame-hook (fn [wm close-fn]
                         (util/with-activation-hooks wm (close-fn)))
     :async-retile-hook (fn [wm frame]
                          (ev/spawn (:retile wm frame)))
    }
   tall-proto))
