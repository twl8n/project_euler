
(ns problem_7)

;; https://projecteuler.net/problem=7

;; java -cp ~/Library/Clojure/clojure-1.8.0.jar:. clojure.main --main problem_7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.  What
;; is the 10001st prime number?

;; We know about 2 as the first prime, so initialize our list of found primes with 2

(def foundp [2])
(def wanted 10001)

(defn checkp [possib]
  (do
      ;; Exclude 0, 1, and any prime we've already found
      (if (or 
           (= 0 possib)
           (= 1 possib)
           (some #(= possib %) foundp))
          (do
              ());; implicit return
        (do
            ;; Check factors against fpc, which gets consumed in the process.
            (def fpc foundp)
            ;; Using a globals as a loop controller and state var is less than ideal.
            (def go 1)
          (def is_prime 1)
          (while (and (= go 1) (not (empty? fpc)))
            (do
                (def var (first fpc))
                (def fpc (rest fpc))
              (if (and 
                   (not= var possib)
                   (= 0 (mod possib var)))
                  (do
                      (def go 0)
                    (def is_prime 0)
                    ))))
          (if (= is_prime 1)
              (do
                  ;; Add the prime we found to the global foundp list.
                  (def foundp (conj foundp possib))))
        ))))
  
(defn -main []
  (prn 'Lookin 'for 'the wanted 'th 'prime)
  ;; Use icky vars rather than figuring out how to bind locally with let
  (with-local-vars 
   ;; We already have 2 as the first, so begin looping with 3
   [checkme 3]
   (while (< (count foundp) wanted)
     (checkp (var-get checkme))
     (if (= 0 (mod (var-get checkme) 5000))
         (prn 'Working 'on (var-get checkme) 'Found (count foundp) 'primes))
     (do
         (var-set checkme (+ 1 (var-get checkme)))
       )))
  (prn (nth foundp (- wanted 1)) 'is 'the wanted 'prime)
  )



