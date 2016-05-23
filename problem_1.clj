(ns problem_1)

;; https://projecteuler.net/problem=1

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_1

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of
;; these multiples is 23.  Find the sum of all the multiples of 3 or 5 below 1000.

;; The answer is 23168

;; Idiomatic solution to problem 1 from stack exchance is a nice example of the thread last macro and
;; reduce. This is the first seach result for "clojure sum". This idiom seems to be the most common on for
;; getting sums in Clojure, and examples for reduce nearly always show a sum.

;; http://stackoverflow.com/questions/24576431/clojure-sum-up-a-bunch-of-numbers

;; (->> (range 1000)
;;      (filter #(or (= (rem % 3) 0) (= (rem % 5) 0)))
;;      (reduce +))

;; However, if we re-imagine the problem to require us to report the sum as well as knowing the break down of
;; 3 and 5 multiples, and if we decide to use a map, we have this solution that gives us a chance to use let,
;; assoc, get, and a map as well as a sequence.


(defn -main []
  (let [final-map
    (reduce #(let [map %1
                  key (cond (= 0 (mod %2 3)) 3
                            (= 0 (mod %2 5)) 5
                            :else 1)
                  val %2] 
                  (assoc map key  
                         (+ val (get map key 0)))) {} (range 1000))
    ]
    (prn 'All-three-and-five (+ (get final-map 3 0) (get final-map 5 0)))
    (prn 'Three-only (get final-map 3 0))
    (prn 'Five-only (get final-map 5 0))
    )
  )

