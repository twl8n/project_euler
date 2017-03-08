
;; (require '[clojure.java.io :as io]
;;          '[clojure.core.reducers :as r]
;;          '[clojure.string :as str])

;; Speed of demo and demo-get seems identical. The difference is #(get first-vals ...) vs #(first-vals ...)

(defn demo [list-size]
"Create two lists of random but unique values. Must use set to get unique values. Remove duplicates from
second-list and concat with first-list to create one larger list, also with unique values."
  (let [working-size (or list-size 100)
        first-list (set (map (fn [xx] {:foo xx}) (take working-size (repeatedly #(rand-int working-size)))))
        second-list (set (map (fn [xx] {:foo xx}) (take working-size (repeatedly #(rand-int working-size)))))
        first-vals (set(map :foo first-list))]
    (concat first-list (remove #(first-vals (:foo %)) second-list))))

(defn demo-get [list-size]
"Create two lists of random but unique values. Must use set to get unique values. Remove duplicates from
second-list and concat with first-list to create one larger list, also with unique values."
  (let [working-size (or list-size 100)
        first-list (set (map (fn [xx] {:foo xx}) (take working-size (repeatedly #(rand-int working-size)))))
        second-list (set (map (fn [xx] {:foo xx}) (take working-size (repeatedly #(rand-int working-size)))))
        first-vals (set(map :foo first-list))]
    (concat first-list (remove #(get first-vals (:foo %)) second-list))))

(defn demo-find-one [list-size]
  "Find a map from the seq where :foo has a certain value."
  (let [working-size (or list-size 100)
        demo-list (demo working-size)
        last-val (last demo-list)] 
    (filter #(= (:foo %) (:foo last-val)) demo-list)))

(def ulist (demo))

(def mm [{:foo "foo1", :baz "baz1"} {:foo "foo2", :baz "baz2"}])

(def mx [{:foo "foo1", :baz "baz1"} {:foo "foo3", :baz "baz2"}])

(def mm-vals (set(map :foo mm)))

(concat mm (filter #(not (mm-vals (:foo %))) mx))

(concat mm (remove  #(mm-vals (:foo %)) mx))

(filter #(not (get mm-vals (:foo %))) mx)

(filter #(not (mm-vals (:foo %))) mx)

(filter #(not (get mm-vals (:foo %))) mx)
;; ({:foo "foo3", :baz "baz2"})

(remove #(get mm-vals (:foo %)) mx)
;; ({:foo "foo3", :baz "baz2"})

;; remove from mx where :foo is equivalent
(remove #(get (set (map :foo mm)) (:foo %)) mx)
;; ({:foo "foo3", :baz "baz2"})

(concat mm (remove #(get (set (map :foo mm)) (:foo %)) mx))
;; ({:foo "foo1", :baz "baz1"} {:foo "foo2", :baz "baz2"} {:foo "foo3", :baz "baz2"})

(def mm {:foo "bar" :baz "boz"})

(apply assoc mm  '(:pie "cake" :car "red"))

(apply dissoc mm '(:baz :foo :pie))

(apply dissoc (assoc mm  :car "red") '(:baz :pie))

(apply dissoc (assoc mm  :car "red") [:baz :pie])

(apply dissoc (assoc mm  :car "red") #{:baz :pie})

((juxt :a :b) {:a 1 :b 2 :c 3 :d 4})
;; [1 2]
((juxt :a :b keys) {:a 1 :b 2 :c 3 :d 4})
;; [1 2 (:a :b :c :d)]

((juxt #(filter even? %1)) (range 10))

(defn lts [xx](< xx 7))
((juxt #(filter lts %1)) (range 10))
;; [(0 1 2 3 4 5 6)]

;; use partial to implicitly call lts with an arg
((juxt (partial filter lts)) (range 10))
;; [(0 1 2 3 4 5 6)]

((juxt (partial filter #(< %1 7))) (range 10))
;; [(0 1 2 3 4 5 6)]

((juxt (partial filter #(< %1 7)) (partial filter #(>= %1 7))) (range 10))
;; [(0 1 2 3 4 5 6) (7 8 9)]

(filter #(< %1 7) (range 10))
;; (0 1 2 3 4 5 6)

[(filter #(< %1 7) (range 10)) (filter #(>= %1 7) (range 10))]
;; [(0 1 2 3 4 5 6) (7 8 9)]

(defn gts [xx](>= xx 7))
((juxt #(filter lts %1) #(filter gts %1)) (range 10))
;; [(0 1 2 3 4 5 6) (7 8 9)]

;; nope, can't next #()
;; ((juxt #(filter #(< %1 7))) (range 10))
(def bfn 
  (comp (fn [xx] (remove nil? xx)) 
        #(map (fn [yy] (if (= "pie" yy) (format "bar %s" yy) nil)) %)))


(filter (fn [xx] (= (:foo xx) "cake")) [{:foo "pie"} {:foo "cake"} {:bar "choc"}])
;;({:foo "cake"})

;; http://stackoverflow.com/questions/1651927/how-to-unit-test-for-laziness

(instance? clojure.lang.LazySeq (lazy-seq '(1 2 3 4)))
;; true

(instance? clojure.lang.LazySeq '(1 2 3 4))
;; false

(instance? clojure.lang.LazySeq (iterate inc 10))
;; false, bue we know it is lazy

(realized? (iterate inc 10))
;; true

(try (do (realized? (iterate inc 10)) true) (catch Exception foo false))
;; true

(try (do (realized? '(1 2 3)) true) (catch Exception foo false))
;; false, as expected

(try (do (realized? (lazy-seq '(1 2 3))) true) (catch Exception foo false))
;; true, as expected

(realized? '(1 2 3 4))
;; ClassCastException clojure.lang.PersistentList cannot be cast to clojure.lang.IPending  clojure.core/realized? (core.clj:7235)

(class (seq '(1 2 3 4)))
;; clojure.lang.PersistentList

(class (iterate inc 10))
;; clojure.lang.Iterate

(try (realized? (lazy-seq '(1 2 3))) (catch Exception foo false))
;; false and false, but that's probably not right
