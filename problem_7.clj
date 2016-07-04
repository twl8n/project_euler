
(ns problem_7)

;; https://projecteuler.net/problem=7

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.  What
;; is the 10001st prime number?

;; 541 is the 100 th prime
;; 104743 is the 10001 th prime

;; We know about 2 as the first prime, so initialize our sequence of found primes with the sequence [2]

;; lein repl
;; (load-file "problem_7.clj")
;; (problem_7/solution)
;;=> {:wanted 100, :prime 541}
;; (problem_7/solution 100)
;;=> {:wanted 100, :prime 541}
;; (problem_7/solution 10001)
;;=> {:wanted 10001, :prime 104743}

;; Return tru if possib is prime, else false.
(defn checkp [possib fpcheck]
  (loop [fpc (first fpcheck)
        fprest (rest fpcheck)]
        (cond (nil? fpc) true ; Found a prime
              (and (not= fpc possib) (= 0 (mod possib fpc))) false ; Not a prime
              true (recur (first fprest) (rest fprest)))))

;; loop-recur is somewhat the more obvious way to loop until we have the wanted prime.
(defn solution [ & wanted]
  (let [ wanted (or (first wanted) 100)]
       (loop [checkme 3
             foundp [2]
             cf (count foundp)]
             (if (>= cf wanted)
                 {:wanted cf :prime (last foundp)}
                 (recur (+ checkme 1) 
                        (if (checkp checkme foundp)
                            (conj foundp checkme)
                          foundp) 
                        (count foundp))))))

;; A second solution using reduce-reduced and lazy sequence may be a bit faster, and more functional
;; lein repl
;; (load-file "problem_7.clj")
;; (problem_7/s2)
;;=> {:wanted 100, :prime 541}

(defn s2 [& wanted]
  (let [ wanted (or (first wanted) 100)]
       (reduce 
        (fn [foundp checkme]
            (let [cf (count foundp)]
                 (if-not (>= cf wanted)
                         (if (checkp checkme foundp)
                             (conj foundp checkme)
                           foundp)
                         (reduced {:wanted cf :prime (last foundp)})))) [2] (iterate inc 3))))


