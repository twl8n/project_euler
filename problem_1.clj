(ns problem_1)

;; https://projecteuler.net/problem=1

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_1

;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of
;; these multiples is 23.  Find the sum of all the multiples of 3 or 5 below 1000.

(def sum 0)
(defn tf [arg]
  (if (or (= 0 (mod arg 3))
          (= 0 (mod arg 5)))
      (do
          ;; (prn "ok "  arg)
          (def sum (+ arg sum))
          1)
    '1
    ))

(defn -main []
  ;; Hmmm. (range) is zero based, good for indexes, less good for counting.
  ;; Use doseq to trick clojure into evaling a statement that doesn't print
  (doseq [x (range 1000)] 
         (tf x)
         )
  
  (printf "Sum: %s%n" sum)
  )
