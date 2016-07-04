(ns problem_1)

;; https://projecteuler.net/problem=1

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of
;; these multiples is 23.  Find the sum of all the multiples of 3 or 5 below 1000.

;; The answer is 233168

;; Idiomatic solution to problem 1 from stack exchance is a nice example of the thread last macro and
;; reduce.

;; http://stackoverflow.com/questions/24576431/clojure-sum-up-a-bunch-of-numbers

;; (->> (range 1000)
;;      (filter #(or (= (rem % 3) 0) (= (rem % 5) 0)))
;;      (reduce +))

;; One can't un-know something this simple, and I can't see anything more efficient, so it seems like a
;; good idea to change the problem to be a bit more interesting.

;; Re-imagine the problem to require us to report the sum of 3s and 5s, as well as the sums of 3 multiples and
;; 5 multiples.  A map is reasonable way to return the results. The solution is (solve_1)

;; lein repl
;; (load-file "problem_1.clj")
;; (problem_1/solution)
;;=> {:3 166833, :neither 266332, :5 66335, :both 233168}


(defn solution []
  (let [mm (reduce #(let [map %1
                         key (cond (= 0 (mod %2 3)) :3
                                   (= 0 (mod %2 5)) :5
                                   :else :neither)
                         val %2] 
                         (assoc map key  
                                (+ val (get map key 0)))) {} (range 1000))]
                                (merge mm {:both (+ (:3 mm) (:5 mm))})))                          

;; Support the native clojure, also. It doesn't behave like the normal repl, so prn is required
;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_1

(defn -main [] (prn (solution)))

