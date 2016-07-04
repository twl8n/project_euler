### Project Euler problems and Deft-ish programming

See the comments inside deftish_1.clj which is intended to run under Leiningen. Deft is table oriented
programming, and a few key fundamentals are in place in deftish_1.clj.

The two Project Euler solutions work, and are intended to be run under Leiningen's repl. problem_1.clj
additionally has a (main) so that it supports the native clojure.jar from the command line, which is somewhat
more limited than Leiningen and requires some kind of print to force evaluation.

The Euler problems have been solved with fairly idiomatic functional Clojure.

The primes code (problem 7) mostly brute forces prime discovery. It is somewhat less efficient than a sieve,
but is simple code. It probably uses less memory than the standard sieve of Eratosthenes. This solution has a
slight optimization in that it only checks known primes as factors, and checks starting with the smallest
number, rather that simply brute force checking all possible numbers less than the candidate.

It would be a nifty idea to take the wanted nth prime as a command line arg.

Problem 1 is so simple that the solution isn't exciting.  So, I've imagined problem
1 as a slightly expanded problem, accumulating the results in a map.

The standard solution to problem 1 is a tidy demonstration of ->> the thread last macro. See deftish_1.clj for
a nice demo of -> the thread first macro.






