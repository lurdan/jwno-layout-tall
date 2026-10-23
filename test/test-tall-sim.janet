# Scenario tests against a fake frame tree that mimics jwno's real
# tree semantics (src/win.janet):
#   - :split moves the existing windows into the first sub-frame
#   - :remove-child does NOT clear the child's :parent
#   - :close on a frame whose parent has only 2 children merges the
#     sibling into the parent (the closed frame keeps a stale :parent)
#   - :attached? walks up the tree and checks :has-child? on each level
#
# Run from this directory:  janet test-tall-sim.janet

(import ../layout-tall-core :as core)

### Fake tree

(var make-frame nil)

(defn- frame? [n] (= :frame (in n :type)))
(defn- window? [n] (= :window (in n :type)))

(def frame-proto
  @{:attached?
    (fn [self]
      (cond
        (in self :monitor) true
        (nil? (in self :parent)) false
        (not (find |(= $ self) (get-in self [:parent :children]))) false
        (:attached? (in self :parent))))

    :get-top-frame
    (fn [self]
      (var cur self)
      (while (not (in cur :monitor)) (set cur (in cur :parent)))
      cur)

    :get-current-window
    (fn [self]
      (def c (in self :current-child))
      (cond
        (nil? c) nil
        (window? c) c
        (:get-current-window c)))

    :get-current-frame
    (fn [self]
      (def children (in self :children))
      (cond
        (empty? children) self
        (window? (first children)) self
        (:get-current-frame (or (in self :current-child) (first children)))))

    :split
    (fn [self _dir]
      (when (and (not (empty? (in self :children)))
                 (frame? (get-in self [:children 0])))
        (error "frame is already split"))
      (def wins (in self :children))
      (def cur (in self :current-child))
      (def a (make-frame self))
      (def b (make-frame self))
      (put a :children (array/slice wins))
      (each w wins (put w :parent a))
      (put a :current-child (if (empty? wins) nil (or cur (first wins))))
      (put self :children @[a b])
      (put self :current-child a)
      self)

    :insert-sub-frame
    (fn [self n]
      (def child (make-frame self))
      (if (= n -1)
        (array/push (in self :children) child)
        (array/insert (in self :children) n child))
      child)

    :add-child
    (fn [self child]
      (def children (in self :children))
      (def old-parent (in child :parent))
      (when (and (not (empty? children))
                 (not= (in child :type) (in (first children) :type)))
        (error "cannot mix different types of children"))
      (when (empty? children) (put self :current-child child))
      (put child :parent self)
      (array/push children child)
      (when (and old-parent (not= old-parent self))
        (:remove-child old-parent child)))

    :remove-child
    (fn [self child]
      (when (window? self) (error "remove-child called on a window"))
      (def children (in self :children))
      (when (= child (in self :current-child))
        (def rest (filter |(not= $ child) children))
        (put self :current-child (if (empty? rest) nil (first rest))))
      (put self :children (filter |(not= $ child) children)))

    :close
    (fn [self]
      (def parent (in self :parent))
      (cond
        (in self :monitor) nil # top-level frame is never closed
        (and (not (empty? (in self :children)))
             (frame? (get-in self [:children 0])))
        (error "cannot close frames containing sub-frames")
        (> (length (in parent :children)) 2)
        (do
          (def wins (array/slice (in self :children)))
          (:remove-child parent self)
          (def to (:get-current-frame parent))
          (each w wins (put w :parent nil) (:add-child to w)))
        # else: merge the remaining sibling into the parent
        (do
          (def sibling (find |(not= $ self) (in parent :children)))
          (unless sibling (error "inconsistent states for frame tree"))
          (def wins (array/slice (in self :children)))
          (def to (:get-current-frame sibling))
          (each w wins (put w :parent nil) (:add-child to w))
          (put parent :children @[])
          (each c (in sibling :children)
            (put c :parent parent)
            (array/push (in parent :children) c))
          (put parent :current-child (in sibling :current-child)))))

    :get-layout (fn [self] :fake-layout)})

(set make-frame
  (fn [parent]
    (table/setproto @{:type :frame :parent parent :children @[] :current-child nil}
                    frame-proto)))

(var win-counter 0)
(defn make-win [name]
  (++ win-counter)
  @{:type :window :name name :parent nil :tags @{}
    :activate (fn [self] nil)})

### Fake window manager + layout object

(defn make-env []
  (def top (make-frame nil))
  (put top :monitor :fake-monitor)
  (def calls @[])
  (def root @{:get-current-frame-on-desktop (fn [_ _id] (:get-current-frame top))
              :get-current-frame (fn [_] (:get-current-frame top))})
  (def wm @{:root root :calls calls})
  (put wm :retile (fn [_ fr] (array/push calls [:retile fr])))
  (put wm :layouts-changed (fn [_ los] (array/push calls [:layouts-changed los])))
  (def self @{:window-manager wm
              :hook-manager @{:call-filter-hook (fn [& _] true)}
              :close-frame-hook (fn [_wm f] (f))
              :async-retile-hook (fn [wm fr] (:retile wm fr))})
  {:top top :wm wm :self self})

# what jwno's wm-add-hwnd does around the :window-created hook
(defn open-window [env name]
  (def {:self self :top top} env)
  (def w (make-win name))
  (core/tall-on-window-created self w nil nil @{:id 1})
  (def target
    (if-let [fr (get-in w [:tags :frame])]
      (do (put (in w :tags) :frame nil) (:get-current-frame fr))
      (:get-current-frame top)))
  (:add-child target w)
  w)

# what jwno's wm-remove-hwnd / purge does around the :window-removed hook
(defn close-window [env w]
  (:remove-child (in w :parent) w)
  (core/tall-on-window-removed (in env :self) w))

(defn focus [w]
  (var child w)
  (var p (in w :parent))
  (while p
    (put p :current-child child)
    (set child p)
    (set p (in p :parent))))

### Invariants

(defn all-frames [fr &opt acc]
  (default acc @[])
  (array/push acc fr)
  (each c (in fr :children)
    (when (frame? c) (all-frames c acc)))
  acc)

(defn all-windows [fr &opt acc]
  (default acc @[])
  (each c (in fr :children)
    (if (window? c) (array/push acc c) (all-windows c acc)))
  acc)

(defn tree-problems [top]
  (def probs @[])
  (each fr (all-frames top)
    (def cs (in fr :children))
    (unless (or (all window? cs) (all frame? cs))
      (array/push probs "mixed children"))
    (when (and (not= fr top) (empty? cs))
      (array/push probs "empty non-top frame left behind"))
    (when (and (not (empty? cs)) (frame? (first cs)) (< (length cs) 2))
      (array/push probs "split frame with a single child"))
    (each c cs
      (unless (= (in c :parent) fr)
        (array/push probs "stale parent pointer"))))
  (distinct probs))

### Runner

(var num-tests 0)
(var num-failures 0)

(defn check [name ok &opt detail]
  (++ num-tests)
  (unless ok
    (++ num-failures)
    (eprintf "FAIL: %s%s" name (if detail (string " -- " detail) ""))))

(defmacro scenario [name & body]
  ~(try
     (do ,;body)
     ([err fib]
       (++ num-tests)
       (++ num-failures)
       (eprintf "FAIL: %s -- error: %v" ,name err))))

(defn master-win [top]
  (def m (get-in top [:children 0]))
  (and m (frame? m) (:get-current-window m)))

### Layout building

(scenario "build 1..4 windows"
  (def env (make-env))
  (def top (env :top))
  (def a (open-window env "a"))
  (check "1 win: lives in top" (= (in a :parent) top))
  (def b (open-window env "b"))
  (check "2 win: a is master" (= (master-win top) a))
  (check "2 win: tree ok" (empty? (tree-problems top)) (string/join (tree-problems top) ", "))
  (def c (open-window env "c"))
  (def d (open-window env "d"))
  (check "4 win: a still master" (= (master-win top) a))
  (check "4 win: stack has 3 slots" (= 3 (length (get-in top [:children 1 :children]))))
  (check "4 win: tree ok" (empty? (tree-problems top)) (string/join (tree-problems top) ", ")))

### Symptom 2: closing a window leaves a frame behind

(each n [2 3 4]
  (for i 0 n
    (def lbl (string/format "close win %d of %d" i n))
    (scenario lbl
      (def env (make-env))
      (def top (env :top))
      (def wins (seq [k :range [0 n]] (open-window env (string k))))
      (close-window env (wins i))
      (def remaining (filter |(not= $ (wins i)) wins))
      (check (string lbl ": tree ok")
             (empty? (tree-problems top)) (string/join (tree-problems top) ", "))
      (check (string lbl ": all other windows still in tree")
             (deep= (sorted (map |(in $ :name) (all-windows top)))
                    (sorted (map |(in $ :name) remaining))))
      (when (> n 1)
        (check (string lbl ": master is filled")
               (or (= n 2) (master-win top)))))))

(scenario "close all windows one by one from master"
  (def env (make-env))
  (def top (env :top))
  (def wins (seq [k :range [0 4]] (open-window env (string k))))
  (each w wins
    (close-window env w)
    (check (string "close-from-master " (in w :name) ": tree ok")
           (empty? (tree-problems top)) (string/join (tree-problems top) ", ")))
  (check "close-from-master: top empty" (empty? (in top :children))))

### Symptom 1: swap-master

(each [n idx] [[2 1] [3 1] [3 2] [4 3]]
  (def lbl (string/format "swap-master %d of %d" idx n))
  (scenario lbl
    (def env (make-env))
    (def top (env :top))
    (def wins (seq [k :range [0 n]] (open-window env (string k))))
    (def target (wins idx))
    (focus target)
    (core/tall-swap-master (env :wm))
    (check (string lbl ": focused window became master") (= (master-win top) target))
    (check (string lbl ": tree ok") (empty? (tree-problems top)) (string/join (tree-problems top) ", "))))

# jwno's notion of the current frame can lag behind the real foreground
# window (e.g. Firefox). Passing the foreground window explicitly must win.
(scenario "swap-master with explicit focused window"
  (def env (make-env))
  (def top (env :top))
  (def wins (seq [k :range [0 3]] (open-window env (string k))))
  (focus (wins 0)) # jwno thinks master is focused
  (core/tall-swap-master (env :wm) (wins 2)) # but the foreground window is 2
  (check "explicit focus: window 2 is master" (= (master-win top) (wins 2))))

### Symptom 3: moving a window to another desktop
# layout-tall.janet now removes the window from the tree (as jwno's
# :remove-hwnd does) before moving it; the result must equal a close.

(scenario "move-to-desktop leaves no frame"
  (def env (make-env))
  (def top (env :top))
  (def wins (seq [k :range [0 3]] (open-window env (string k))))
  (close-window env (wins 1))
  (check "move: tree ok" (empty? (tree-problems top)) (string/join (tree-problems top) ", ")))

### Result

(if (= num-failures 0)
  (printf "%d tests passed" num-tests)
  (do
    (eprintf "%d/%d tests FAILED" num-failures num-tests)
    (os/exit 1)))
