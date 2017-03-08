
;; (require '[clojure.java.io :as io]
;;          '[clojure.core.reducers :as r]
;;          '[clojure.string :as str])

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
