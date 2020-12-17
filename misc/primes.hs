module Primes
( primes
) where

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x:(sieve [n | n <- xs, mod n x > 0])

primes :: [Integer]
primes = sieve [2..]
