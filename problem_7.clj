
(ns problem_7)

;; https://projecteuler.net/problem=7

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.  What
;; is the 10001st prime number?

;; 541 is the 100 th prime
;; 104743 is the 10001 th prime

;; We know about 2 as the first prime, so initialize our list of found primes with 2

(def wanted 10001)

;; Return possib if it is prime, else return nil
(defn checkp [possib fpcheck]
  (loop [fpc (first fpcheck)
        fprest (rest fpcheck)]
        (cond (nil? fpc) possib ; Return a found prime
              (and (not= fpc possib) (= 0 (mod possib fpc))) nil ; Not a prime, return nil
              true (recur (first fprest) (rest fprest))) ; Keep working
        ))

(defn -main []
  (prn 'Looking 'for 'the wanted 'th 'prime)
  ;; We already have 2 as the first prime, so begin looping with 3
  (let [final
    (loop [checkme 3
          foundp [2]]
          (if (>= (count foundp) wanted)
              (nth foundp (- wanted 1)) ;; Done, final prime is the returned value of the loop
            (do
                ;; Print a status message every few checks
                (if (= 0 (mod checkme 2000))
                    (prn 'Working 'on checkme 'Found (count foundp) 'primes))
                (recur (+ checkme 1)
                       ;; If we get back our checkme, it must be prime so conj it to the foundp seq which will
                       ;; be used in the next recur through the loop, else use foundp is unchanged.
                       (if (= checkme (checkp checkme foundp))
                           (conj foundp checkme)
                         foundp)
                       )
              )
            )
          )]
           (prn final 'is 'the wanted 'th 'prime)
           )
  )



