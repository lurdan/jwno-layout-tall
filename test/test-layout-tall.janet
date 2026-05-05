(import ../layout-tall-core :as core)

### Test hooks (synchronous, no ev/spawn, no util/with-activation-hooks)

(def sync-close-hook (fn [_wm f] (f)))
(def sync-retile-hook (fn [wm frame] (:retile wm frame)))

### Fake frame/window objects

(defn make-frame [parent]
  (def f @{:parent parent :children @[] :monitor nil :current-window nil})

  (put f :attached?
    (fn [self] (not (nil? (in self :parent)))))

  (put f :get-top-frame
    (fn [self]
      (var cur self)
      (while (in cur :parent) (set cur (in cur :parent)))
      cur))

  (put f :get-current-window
    (fn [self] (in self :current-window)))

  (put f :split
    (fn [self _dir]
      (def child (make-frame self))
      (array/push (in self :children) child)
      child))

  (put f :insert-sub-frame
    (fn [self n]
      (def child (make-frame self))
      (if (= n -1)
        (array/push (in self :children) child)
        (array/insert (in self :children) n child))
      child))

  (put f :add-child
    (fn [self child]
      (def old (in child :parent))
      (when old
        (def idx (find-index |(= $ child) (in old :children)))
        (when idx (array/remove (in old :children) idx)))
      (put child :parent self)
      (array/push (in self :children) child)))

  (put f :remove-child
    (fn [self child]
      (def idx (find-index |(= $ child) (in self :children)))
      (when idx (array/remove (in self :children) idx))
      (put child :parent nil)
      (when (= (in self :current-window) child)
        (put self :current-window nil))))

  (put f :close
    (fn [self]
      (def p (in self :parent))
      (when p
        (def idx (find-index |(= $ self) (in p :children)))
        (when idx (array/remove (in p :children) idx))
        (put self :parent nil))))

  (put f :get-layout (fn [self] :fake-layout))

  (put f :activate (fn [self] nil))

  f)

(defn make-win [parent]
  (def w @{:parent parent :tags @{}})
  (put w :activate (fn [self] nil))
  w)

(defn make-wm [root]
  (def calls @[])
  (def wm @{:root root :calls calls})
  (put wm :retile
    (fn [self frame] (array/push calls [:retile frame])))
  (put wm :layouts-changed
    (fn [self layouts] (array/push calls [:layouts-changed layouts])))
  wm)

# Standard 2-window tree:
#   top [monitor=:m]
#     master  (children[0])
#       win-m
#     stack   (children[1])
#       sub0
#         win-s
(defn make-tall-tree []
  (def top (make-frame nil))
  (put top :monitor :fake-monitor)

  (def master (make-frame top))
  (array/push (in top :children) master)

  (def stack (make-frame top))
  (array/push (in top :children) stack)

  (def sub0 (make-frame stack))
  (array/push (in stack :children) sub0)

  (def win-m (make-win master))
  (array/push (in master :children) win-m)
  (put master :current-window win-m)

  (def win-s (make-win sub0))
  (array/push (in sub0 :children) win-s)
  (put sub0 :current-window win-s)

  {:top top :master master :stack stack :sub0 sub0 :win-m win-m :win-s win-s})

### Minimal test runner

(var num-tests 0)
(var num-failures 0)

(defmacro expect [label cond]
  ~(do
     (++ num-tests)
     (when (not ,cond)
       (++ num-failures)
       (eprintf "FAIL: %s\n" ,label))))

###
### tall-swap-master
###

# focused != master → swaps
(let [tree (make-tall-tree)
      {:top top :master master :stack stack :sub0 sub0 :win-m win-m :win-s win-s} tree
      root @{:get-current-frame (fn [self] sub0)}
      wm (make-wm root)]

  (put sub0 :current-window win-s)

  (core/tall-swap-master wm)

  (expect "swap: win-s moved to master children"
          (some |(= $ win-s) (in master :children)))
  (expect "swap: win-m moved to sub0 children"
          (some |(= $ win-m) (in sub0 :children)))
  (expect "swap: win-s parent updated"
          (= (in win-s :parent) master))
  (expect "swap: win-m parent updated"
          (= (in win-m :parent) sub0)))

# focused == master → no-op
(let [tree (make-tall-tree)
      {:top top :master master :sub0 sub0 :win-m win-m :win-s win-s} tree
      root @{:get-current-frame (fn [self] master)}
      wm (make-wm root)]

  (core/tall-swap-master wm)

  (expect "swap-noop: win-m stays in master"
          (= (get-in master [:children 0]) win-m))
  (expect "swap-noop: win-s stays in sub0"
          (= (get-in sub0 [:children 0]) win-s)))

###
### tall-cleanup-layout
###

# stack window closes → sub0 and stack both cleaned up
(let [tree (make-tall-tree)
      {:top top :master master :stack stack :sub0 sub0 :win-m win-m :win-s win-s} tree
      wm (make-wm @{})]

  (array/clear (in sub0 :children))
  (put sub0 :current-window nil)
  # win-s :parent still points to sub0

  (core/tall-cleanup-layout wm win-s sync-close-hook sync-retile-hook)

  (expect "stack-close: sub0 removed from stack"
          (empty? (in stack :children)))
  (expect "stack-close: stack removed from top"
          (= (length (in top :children)) 1))
  (expect "stack-close: master remains"
          (= (get-in top [:children 0]) master))
  (expect "stack-close: win-m intact"
          (= (get-in master [:children 0]) win-m)))

# master window closes → stack window promoted, then sub0 + stack cleaned up
(let [tree (make-tall-tree)
      {:top top :master master :stack stack :sub0 sub0 :win-m win-m :win-s win-s} tree
      wm (make-wm @{})]

  (array/clear (in master :children))
  (put master :current-window nil)
  # win-m :parent still points to master

  (core/tall-cleanup-layout wm win-m sync-close-hook sync-retile-hook)

  (expect "master-close: win-s promoted to master"
          (some |(= $ win-s) (in master :children)))
  (expect "master-close: only master remains in top"
          (= (length (in top :children)) 1))
  (expect "master-close: master is top child[0]"
          (= (get-in top [:children 0]) master)))

# Regression: frame with current-window set must NOT be deleted even if children empty.
# This is the "fix closing frame" case: get-current-window returns non-nil
# even though :children is empty (focus tracking and children are independent).
(let [tree (make-tall-tree)
      {:top top :master master :stack stack :sub0 sub0 :win-s win-s} tree
      wm (make-wm @{})]

  # children emptied but current-window still set
  (array/clear (in sub0 :children))
  # sub0 :current-window is still win-s — NOT cleared

  (core/tall-cleanup-layout wm win-s sync-close-hook sync-retile-hook)

  (expect "regression/fix-closing-frame: sub0 not deleted when current-window is set"
          (not (nil? (in sub0 :parent)))))

# no stack-frame → early exit, master untouched
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      win-m (make-win master)
      _ (array/push (in master :children) win-m)
      _ (put master :current-window win-m)
      wm (make-wm @{})

      win-other (make-win master)]

  (array/clear (in master :children))
  (put master :current-window nil)

  (core/tall-cleanup-layout wm win-other sync-close-hook sync-retile-hook)

  (expect "no-stack: top still has 1 child"
          (= (length (in top :children)) 1)))

###
### tall-on-window-created
###

(defn make-self [root &opt filter-result]
  (default filter-result true)
  (def hook-man @{:call-filter-hook (fn [self & _] filter-result)})
  (def wm (make-wm root))
  @{:window-manager wm
    :hook-manager hook-man
    :close-frame-hook sync-close-hook
    :async-retile-hook sync-retile-hook})

# filter returns false → window not placed, no tree changes
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      root @{:get-current-frame-on-desktop (fn [self id] top)}
      self (make-self root false)
      new-win @{:tags @{}}]

  (core/tall-on-window-created self new-win nil nil @{:id 1})

  (expect "filter-false: top unchanged"
          (= (length (in top :children)) 1))
  (expect "filter-false: new-win has no frame"
          (nil? (get-in new-win [:tags :frame]))))

# desktop-info nil → cur-frame nil → break
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      root @{:get-current-frame-on-desktop (fn [self id] top)}
      self (make-self root)
      new-win @{:tags @{}}]

  (core/tall-on-window-created self new-win nil nil nil)

  (expect "nil-desktop: top unchanged"
          (= (length (in top :children)) 1)))

# 1 win in top-frame → split horizontal, new win goes to stack slot
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      win-existing (make-win master)
      _ (array/push (in master :children) win-existing)
      root @{:get-current-frame-on-desktop (fn [self id] top)}
      self (make-self root)
      new-win @{:tags @{}}]

  (core/tall-on-window-created self new-win nil nil @{:id 1})

  (expect "1win: top split to 2 children"
          (= (length (in top :children)) 2))
  (expect "1win: new win assigned to stack slot"
          (= (get-in new-win [:tags :frame]) (get-in top [:children 1]))))

# 2 wins (stack has 1 sub-frame) → split stack vertical
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      stack (make-frame top)
      _ (array/push (in top :children) stack)
      sub0 (make-frame stack)
      _ (array/push (in stack :children) sub0)
      root @{:get-current-frame-on-desktop (fn [self id] top)}
      self (make-self root)
      new-win @{:tags @{}}]

  (core/tall-on-window-created self new-win nil nil @{:id 1})

  (expect "2win: stack split to 2 sub-frames"
          (= (length (in stack :children)) 2))
  (expect "2win: new win assigned to new sub-frame"
          (= (get-in new-win [:tags :frame]) (get-in stack [:children 1]))))

# 3+ wins (stack has 2 sub-frames) → insert-sub-frame at end
(let [top (make-frame nil)
      _ (put top :monitor :fake-monitor)
      master (make-frame top)
      _ (array/push (in top :children) master)
      stack (make-frame top)
      _ (array/push (in top :children) stack)
      sub0 (make-frame stack)
      _ (array/push (in stack :children) sub0)
      sub1 (make-frame stack)
      _ (array/push (in stack :children) sub1)
      root @{:get-current-frame-on-desktop (fn [self id] top)}
      self (make-self root)
      new-win @{:tags @{}}]

  (core/tall-on-window-created self new-win nil nil @{:id 1})

  (expect "3win: stack has 3 sub-frames"
          (= (length (in stack :children)) 3))
  (expect "3win: new win assigned to last sub-frame"
          (= (get-in new-win [:tags :frame]) (last (in stack :children)))))

###
### close-frame-hook is called for frame deletion
###

# Spy: verify close-frame-hook is invoked with the correct closed frame
(let [tree (make-tall-tree)
      {:top top :master master :stack stack :sub0 sub0 :win-s win-s} tree
      closed-frames @[]
      spy-close-hook (fn [_wm close-fn]
                       (array/push closed-frames close-fn)
                       nil)  # do NOT call close-fn — just record it
      wm (make-wm @{})]

  (array/clear (in sub0 :children))
  (put sub0 :current-window nil)

  (core/tall-cleanup-layout wm win-s spy-close-hook sync-retile-hook)

  (expect "close-hook called at least once"
          (> (length closed-frames) 0))
  # Because close-fn was NOT called, sub0 and stack should still have parent
  (expect "close-hook: frame not removed when hook skips close-fn"
          (not (nil? (in sub0 :parent)))))

### Result

(if (= num-failures 0)
  (printf "%d tests passed\n" num-tests)
  (do
    (eprintf "%d/%d tests FAILED\n" num-failures num-tests)
    (os/exit 1)))
