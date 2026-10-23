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
(ffi/defbind GetForegroundWindow :ptr [])
(defn tall-toggle-maximize [window-man]
  (when-let [cur-fr (:get-current-frame (in window-man :root))
             cur-win (:get-current-window cur-fr)
             hwnd (in cur-win :hwnd)]
    (if (= 0 (IsZoomed hwnd))
      (ShowWindow hwnd 3)
      (ShowWindow hwnd 9))))

(defn tall-swap-master [window-man]
  # jwno's current frame can lag behind the real focus (seen with
  # Firefox), so prefer the window that actually owns the foreground.
  (def fg-hwnd (GetForegroundWindow))
  (def fg-win (when fg-hwnd (:find-hwnd window-man fg-hwnd)))
  (core/tall-swap-master window-man fg-win))

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
      # 1. Detach the window from the tree. jwno fires :window-removed,
      #    so the tall layout cleans up the old frame exactly as it does
      #    for a closed window.
      (:remove-hwnd window-man hwnd)
      # 2. Move the real window.
      (MoveWindowToDesktopNumber hwnd n)
      # 3. Re-register it. jwno reads the window's new virtual desktop and
      #    fires :window-created, so the tall layout places it on desktop n.
      (:add-hwnd window-man hwnd)
      (log/info (string "Moved window to Desktop " (+ n 1))))))

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
     :hook-fns @[]
     :close-frame-hook (fn [wm close-fn]
                         (util/with-activation-hooks wm (close-fn)))
     :async-retile-hook (fn [wm frame]
                          (ev/spawn (:retile wm frame)))
    }
   tall-proto))
