# project_euler
Some solutions to Project Euler problems

The two solutions provided do work. problem_7.clj is perhaps not a bad example of function Lisp style.

The primes code (problem 7) mostly brute forces prime discovery. Somewhat less efficient than a sieve, but
simple code. Probably uses less memory than the standard sieve of Eratosthenes. This solution has a slight
optimization in that it only checks known primes as factors, and checks starting with the smallest number,
rather that simply brute force checking all possible numbers less than the candidate.

It would be a nifty idea to take the wanted nth prime as a command line arg.




