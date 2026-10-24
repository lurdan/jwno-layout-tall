#
# Debug log for the Tall layout
#
# Records focus changes (what Windows reports vs. what jwno thinks is
# current), window creation/removal with the resulting layout, and every
# run of the layout's commands (before/after state). Meant for chasing
# problems that only show up on a particular machine.
#
# To use it (xmonad-like.janet does this):
#
#     (import tall-debug)
#     (def tall-debug (tall-debug/tall-debug jwno/context))
#     (:enable tall-debug)       # or bind the :tall-debug command to a key
#
# The log goes to tall-debug.log next to the config file; it is rotated
# to tall-debug.log.old when it grows past 2 MB.
#

(use jw32/_winuser)
(use jwno/util)

(def- max-log-size (* 2 1024 1024))
(def- wrapped-commands [:swap-master :tall-arrange :move-to-desktop])

### Formatting helpers

(defn- exe-name [w]
  (def p (try (:get-exe-path w) ([_] nil)))
  (if p (last (string/split "\\" p)) "?"))

(defn- class-of [wm hwnd]
  (try
    (with-uia [u (:get-hwnd-uia-element wm hwnd)]
      (when u (:get_CachedClassName u)))
    ([_] nil)))

(defn- desc-hwnd [wm hwnd]
  (if (nil? hwnd)
    "nil"
    (let [w (:find-hwnd wm hwnd)]
      (string/format "%s[%s %s]"
                     (string hwnd)
                     (or (class-of wm hwnd) "?")
                     (if w (exe-name w) "UNMANAGED")))))

(defn- desc-win [w]
  (if w (string/format "%s[%s]" (string (in w :hwnd)) (exe-name w)) "nil"))

(defn- layout-line
  ``One-line summary of a top frame, e.g. M[firefox.exe] S[notepad.exe | explorer.exe]``
  [top]
  (defn names [fr] (string/join (map exe-name (:get-all-windows fr)) ","))
  (def cs (in top :children))
  (cond
    (empty? cs) "(empty)"
    (= :window (get-in cs [0 :type])) (string "[" (names top) "]")
    (let [stack (get cs 1)
          slots (if (and stack (= :frame (get-in stack [:children 0 :type])))
                  (in stack :children)
                  (if stack [stack] []))]
      (string "M[" (names (cs 0)) "] S["
              (string/join (map names slots) " | ")
              "]"
              (if (> (length cs) 2) (string " +" (- (length cs) 2) " extra top-level frames") "")))))

(defn- layout-problems [top]
  (def acc @[])
  (defn walk [fr]
    (def cs (in fr :children))
    (def kinds (distinct (map |(in $ :type) cs)))
    (when (> (length kinds) 1) (array/push acc "mixed children"))
    (when (and (not= fr top) (empty? cs)) (array/push acc "empty frame left behind"))
    (when (and (= [:frame] kinds) (< (length cs) 2)) (array/push acc "split frame with 1 child"))
    (each c cs (when (= :frame (in c :type)) (walk c))))
  (walk top)
  (distinct acc))

(defn- all-top-frames [wm]
  (def tops @[])
  (each lo (get-in wm [:root :children] [])
    (each top (in lo :children)
      (array/push tops top)))
  tops)

(defn- master-win [top]
  (def m (get-in top [:children 0]))
  (when (and m (= :frame (in m :type)))
    (:get-current-window m)))

### Log output

(defn- write-log [self & parts]
  (def path (in self :log-path))
  (when-let [size (os/stat path :size)]
    (when (> size max-log-size)
      (try (os/rm (string path ".old")) ([_] nil))
      (try (os/rename path (string path ".old")) ([_] nil))))
  (def line (string (os/strftime "%Y-%m-%d %H:%M:%S" nil true) " " ;parts "\n"))
  (try
    (with [f (file/open path :a)]
      (file/write f line))
    ([_] nil)))

(defn- log-layouts [self tag]
  (def wm (in self :window-manager))
  (each top (all-top-frames wm)
    (def ps (layout-problems top))
    (write-log self "  " tag " " (layout-line top)
               (if (empty? ps) "" (string "  !! " (string/join ps ", "))))))

### Hooks

(defn- on-focus-changed [self hwnd]
  (def wm (in self :window-manager))
  (def fg (GetForegroundWindow))
  # The hook runs before jwno updates its own state; compare afterwards.
  (ev/spawn
    (def cur (:get-current-window (in wm :root)))
    (write-log self "FOCUS hook=" (desc-hwnd wm hwnd)
               " fg=" (desc-hwnd wm fg)
               " jwno=" (desc-win cur)
               (if (and cur (= fg (in cur :hwnd))) "" "  <-- fg != jwno"))))

(defn- on-window-created [self win & _]
  (write-log self "CREATED " (desc-win win))
  # the window is attached to its frame after this hook returns
  (ev/spawn (log-layouts self "after-create")))

(defn- on-window-removed [self win]
  (write-log self "REMOVED " (string (in win :hwnd)))
  (ev/spawn (log-layouts self "after-remove")))

### Command wrappers

(defn- wrap-command [self cmd-name orig-fn]
  (fn [& args]
    (def wm (in self :window-manager))
    (def fg (GetForegroundWindow))
    (def fg-win (:find-hwnd wm fg))
    (def cur (:get-current-window (in wm :root)))
    (def top (when-let [w (or fg-win cur) p (in w :parent)] (:get-top-frame p)))
    (def before (when top (master-win top)))
    (write-log self "CMD " cmd-name " " (string/format "%q" args)
               " fg=" (desc-hwnd wm fg) " jwno=" (desc-win cur)
               " master-before=" (desc-win before))
    (def res (try (do (orig-fn ;args) :ok) ([e] (string "ERROR " e))))
    (ev/spawn
      (ev/sleep 0.3)
      (def after (when top (master-win top)))
      (write-log self "  result=" res " master-after=" (desc-win after)
                 (if (= cmd-name :swap-master)
                   (cond
                     (nil? fg-win) "  => foreground window not managed by jwno"
                     (= after fg-win) "  => PROMOTED"
                     "  => NOT promoted")
                   ""))
      (log-layouts self "after-cmd"))))

### Enable / disable

(defn tall-debug-enable [self]
  (when (in self :enabled) (break))
  (def {:hook-manager hook-man :command-manager cmd-man} self)

  (put self :hook-fns
     @[[:focus-changed (:add-hook hook-man :focus-changed |(on-focus-changed self $))]
       [:window-created (:add-hook hook-man :window-created (fn [& args] (on-window-created self ;args)))]
       [:window-removed (:add-hook hook-man :window-removed |(on-window-removed self $))]])

  (def saved @{})
  (each c wrapped-commands
    (when-let [entry (:get-command cmd-man c)]
      (def wrapper (wrap-command self c (in entry :fn)))
      (put saved c [entry wrapper])
      (:add-command cmd-man c wrapper (in entry :doc))))
  (put self :saved-commands saved)

  (put self :enabled true)
  (write-log self "==== tall-debug ON  log=" (in self :log-path))
  (log-layouts self "now"))

(defn tall-debug-disable [self]
  (unless (in self :enabled) (break))
  (def {:hook-manager hook-man :command-manager cmd-man} self)

  (each [hook-name hook-fn] (in self :hook-fns)
    (:remove-hook hook-man hook-name hook-fn))
  (put self :hook-fns @[])

  (eachp [c [entry wrapper]] (in self :saved-commands)
    # Only restore if nobody (e.g. a reloaded layout) replaced our wrapper
    (when (= wrapper (get (:get-command cmd-man c) :fn))
      (:add-command cmd-man c (in entry :fn) (in entry :doc))))
  (put self :saved-commands @{})

  (write-log self "==== tall-debug OFF")
  (put self :enabled false))

(defn tall-debug-toggle [self]
  (if (in self :enabled)
    (:disable self)
    (:enable self))
  (:show-tooltip (in self :ui-manager) :tall-debug
     (if (in self :enabled)
       (string "Tall debug log ON\n" (in self :log-path))
       "Tall debug log OFF")))

(def tall-debug-proto
  @{:enable tall-debug-enable
    :disable tall-debug-disable
    :toggle tall-debug-toggle})

(defn- default-log-path [context]
  (def dir (or (in context :data-dir) "."))
  (string (string/trimr dir "\\/") "\\tall-debug.log"))

(defn tall-debug
  ``Create the debug logger. Registers the :tall-debug command (toggle);
  logging itself starts only after :enable / :tall-debug.``
  [context &opt log-path]
  (def self
    (table/setproto
      @{:window-manager (in context :window-manager)
        :hook-manager (in context :hook-manager)
        :command-manager (in context :command-manager)
        :ui-manager (in context :ui-manager)
        :log-path (or log-path (default-log-path context))
        :enabled false
        :hook-fns @[]
        :saved-commands @{}}
      tall-debug-proto))
  (:add-command (in context :command-manager) :tall-debug
     (fn [] (:toggle self))
     ```
     (:tall-debug)

     Toggles the Tall layout debug log (tall-debug.log next to the config).
     ```)
  self)
