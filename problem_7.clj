
(ns problem_7)

;; https://projecteuler.net/problem=7

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.  What
;; is the 10001st prime number?

;; 541 is the 100 th prime
;; 104743 is the 10001 th prime

;; We know about 2 as the first prime, so initialize our list of found primes with 2

(def foundp [2])
(def wanted 100)

;; Return possib if it is prime, else return nil
(defn checkp [possib fpcheck]
  (do
      ;; Exclude 0, 1, and any prime we've already found Unlikely to get these, so if you want more speed, get
      ;; rid of this test.
      (if (or 
           (= 0 possib)
           (= 1 possib)
           (some #(= possib %) foundp))
          nil ; Not prime, return nil
        (loop [fpc (first fpcheck)
              fprest (rest fpcheck)]
              (cond (nil? fpc) possib ; Return a found prime
                    (and (not= fpc possib) (= 0 (mod possib fpc))) nil ; Not a prime, return nil
                    true (recur (first fprest) (rest fprest))) ; Keep working
              ))))

(defn -main []
  (prn 'Looking 'for 'the wanted 'th 'prime)
  ;; We already have 2 as the first, so begin looping with 3
  (loop [checkme 3]
        (when (< (count foundp) wanted)

          ;; If we get back our checkme, it must be prime so add it to the foundp seq
          (if (= checkme (checkp checkme foundp))
              (def foundp (conj foundp checkme)))

          ;; Print a status message every few checks
          (if (= 0 (mod checkme 2000))
              (prn 'Working 'on checkme 'Found (count foundp) 'primes))
          (recur (+ checkme 1))))
  (prn (nth foundp (- wanted 1)) 'is 'the wanted 'th 'prime))



