(import jwno/auto-layout)
(import jwno/indicator)
(import jwno/util)
(import jwno/log)

(use jw32/_uiautomation)

(def mod-key "Win")
(def keyboard-layout :qwerty)

(def dir-keys
  (case keyboard-layout
    :qwerty
    (case (string/ascii-lower mod-key)
      "win"
      {:left  "Y"
       :down  "U"
       :up    "I"
       :right "O"}

      "alt"
      {:left  "H"
       :down  "J"
       :up    "K"
       :right "L"})

    (errorf "unsupported layout: %n" keyboard-layout)))

(def {:key-manager key-man
      :command-manager command-man
      :window-manager window-man
      :virtual-desktop-manager vd-man
      :ui-manager ui-man
      :hook-manager hook-man
      :repl-manager repl-man}
  jwno/context)

(defmacro $ [str]
  ~(->> ,str
        (peg/replace-all "${MOD}"  ,mod-key)
        (string)))

(defmacro k [key-seq cmd &opt doc]
  ~(:define-key keymap ($ ,key-seq) ,cmd ,doc))

(def current-frame-area
  (indicator/current-frame-area jwno/context))
(:enable current-frame-area)

(defn match-exe-name [exe-name]
  (fn [win]
    (if-let [win-exe (:get-exe-path win)]
      (string/has-suffix? (string "\\" (string/ascii-lower exe-name))
                          (string/ascii-lower win-exe))
      false)))

(defn build-keymap [key-man]
  (let [keymap (:new-keymap key-man)]

    (k "${MOD} + Shift + Q" :quit)
    (k "${MOD} + N"         :retile)
    (k "${MOD} + Shift + C" :close-window-or-frame)
    (k "${MOD} + Enter"     :tall-swap-master)

    (k (string "${MOD} + " (in dir-keys :down))  [:enum-frame :next])
    (k (string "${MOD} + " (in dir-keys :up))    [:enum-frame :prev])
    (k (string "${MOD} + " (in dir-keys :left))  [:enum-window-in-frame :prev])
    (k (string "${MOD} + " (in dir-keys :right)) [:enum-window-in-frame :next])

    (each dir [:down :up :left :right]
      (k (string "${MOD} + Ctrl + "  (in dir-keys dir)) [:adjacent-frame dir])
      (k (string "${MOD} + Shift + " (in dir-keys dir)) [:move-window dir]))

    (k "${MOD} + P"         :describe-window)
    (k "${MOD} + T"         :manage-window)
    (k "${MOD} + Shift + T" :ignore-window)
    (k "${MOD} + Shift + Enter"
       [:summon (match-exe-name "wt.exe") true "wt.exe"])

    # switch workspace
    (k "${MOD} + 1" [:set-desktop 0])
    (k "${MOD} + 2" [:set-desktop 1])
    (k "${MOD} + 3" [:set-desktop 2])
    (k "${MOD} + 4" [:set-desktop 3])
    (k "${MOD} + 5" [:set-desktop 4])
    (k "${MOD} + 6" [:set-desktop 5])
    (k "${MOD} + 7" [:set-desktop 6])
    (k "${MOD} + 8" [:set-desktop 7])
    (k "${MOD} + 9" [:set-desktop 8])

    # send window to workspace
    (k "${MOD} + Shift + 1" [:move-to-desktop 0])
    (k "${MOD} + Shift + 2" [:move-to-desktop 1])
    (k "${MOD} + Shift + 3" [:move-to-desktop 2])
    (k "${MOD} + Shift + 4" [:move-to-desktop 3])
    (k "${MOD} + Shift + 5" [:move-to-desktop 4])
    (k "${MOD} + Shift + 6" [:move-to-desktop 5])
    (k "${MOD} + Shift + 7" [:move-to-desktop 6])
    (k "${MOD} + Shift + 8" [:move-to-desktop 7])
    (k "${MOD} + Shift + 9" [:move-to-desktop 8])

    keymap))

(def root-keymap (build-keymap key-man))
(:set-keymap key-man root-keymap)

# xmonad-like workspace
# cf. https://github.com/agent-kilo/jwno/discussions/10
(ffi/context "VirtualDesktopAccessor.dll")

(ffi/defbind GoToDesktopNumber :uint32 [n :uint32])
(ffi/defbind MoveWindowToDesktopNumber :uint32 [hwnd :ptr n :uint32])
(ffi/defbind GetCurrentDesktopNumber :uint32 [])
(ffi/defbind GetDesktopCount :uint32 [])
(ffi/defbind CreateDesktop :uint32 [])

(while (< (GetDesktopCount) 9)
  (CreateDesktop))

(:add-command command-man :set-desktop
  (fn [n]
    (cond
      (= (GetCurrentDesktopNumber) n)
        (log/info (string "Already on Desktop " (+ n 1)))
      (< (GetDesktopCount) (+ n 1))
        (log/info (string "Desktop " (+ n 1) " unavailable"))
      (do
        (GoToDesktopNumber n)
        (log/info (string "Desktop " (+ n 1)))))))

(:add-command command-man :move-to-desktop
  (fn [n]
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
        (:retile window-man (:get-top-frame cur-fr))
        # FIXME: moved window should be controlled
        # (GoToDesktopNumber n)
        (def vd-info (:get-hwnd-virtual-desktop window-man hwnd))
        (def new-fr (:get-current-frame-on-desktop (in window-man :root) vd-info))
        (:add-child new-fr cur-win)
        (:activate cur-win)
        (log/info (string "Desktop " (+ n 1)))))))

# xmonad-like Tall layout
(import layout-tall)
(def layout-tall (layout-tall/tall jwno/context))
(:enable layout-tall)

(:add-command command-man :tall-swap-master
   (fn []
     (def {:window-manager window-man} jwno/context)

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
     (:activate focused)))
