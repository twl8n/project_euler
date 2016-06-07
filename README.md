### Project Euler problems and Deft-ish programming

See the comments inside deftish_1.clj which is intended to run under Leiningen. Deft is table oriented
programming, and a few key fundamentals are in place in deftish_1.clj.

The two project euler solutions do work, and are intended to be run from the clojure.jar repl which is quite a
bit simpler (more primitive) than Leiningen and requires some kind of print to force evaluation.

Both problems need some stylistic improvements, but problem_7.clj is more common Lisp style.

The primes code (problem 7) mostly brute forces prime discovery. Somewhat less efficient than a sieve, but
simple code. Probably uses less memory than the standard sieve of Eratosthenes. This solution has a slight
optimization in that it only checks known primes as factors, and checks starting with the smallest number,
rather that simply brute force checking all possible numbers less than the candidate.

It would be a nifty idea to take the wanted nth prime as a command line arg.

Problem 1 is so simple that the solution isn't exciting. So, I've imagined it as a slightly expanded problem,
which gives a chance to use a map.






