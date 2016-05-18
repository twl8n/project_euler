
(ns problem_7)

;; https://projecteuler.net/problem=7

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.  What
;; is the 10001st prime number?

;; We know about 2 as the first prime, so initialize our list of found primes with 2

(def foundp [2])
(def wanted 100)

(defn checkp [possib fpcheck]
  (do
      (prn 'possib possib 'fpcheck fpcheck)
      ;; Exclude 0, 1, and any prime we've already found
      (if (or 
           (= 0 possib)
           (= 1 possib)
           (some #(= possib %) foundp))
          (do
              ());; implicit return
        (if (= 1
               (loop [fpc (first fpcheck)
                     tail (rest fpcheck)]
                     ;; (def var (first fpcheck))
                     ;; (def fpcheck (rest fpcheck))
                     (prn 'checking fpc)
                     (if (nil? fpc)
                         1
                       (do
                           (if (and 
                                (not= fpc possib)
                                (= 0 (mod possib fpc)))
                               0)
                           (recur (first tail) (rest tail))))))
        (do
            ;; Add the prime we found to the global foundp list.
            (def foundp (conj foundp possib))))
    )))
  
(defn -main []
  (prn 'Looking 'for 'the wanted 'th 'prime)
  ;; We already have 2 as the first, so begin looping with 3
  (loop [checkme 3]
        (when (< (count foundp) wanted)
          (checkp checkme foundp)
          (if (= 0 (mod checkme 50))
              (prn 'Working 'on checkme 'Found (count foundp) 'primes))
          (recur (+ checkme 1))))
  (prn (nth foundp (- wanted 1)) 'is 'the wanted 'prime))



