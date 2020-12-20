# Haskell

Simple projects in Haskell:
- [List concatenation](misc/list.hs): ```concat'```
- [```map```](misc/list.hs): ```map'```
- [Sine via McLaren formula](misc/sin.hs): ```sin'```
- [Folds](misc/list.hs): ```foldrX```, ```foldlX```
- [Helpers for ```unfoldr```](misc/list.hs): ```binary``` (for binary representation of a number), ```factor``` (for factorizing into primes)
- [Sieve of Eratosthenes](misc/primes.hs): ```primes```
- [Permutations (перестановки)](misc/perm.hs): ```permutate```
- [```filter``` via ```fix```](misc/fixfilter.hs): ```filter'```
- [Trie](misc/trie.hs)
  ```
  *Trie> mytrie = trieBuild "hello world, world"
  *Trie> trieGet mytrie "world"
  2
  ```
- [Interpretator](interpr/Interpr.hs)
  - Fixed lexical analyser [here](https://github.com/oeml/haskell/commit/a6c29017bb0fb46996059afe7fb87979a2bf9201)
  - Added natural logarithm and root [here](https://github.com/oeml/haskell/commit/ae5b31777b2021cbd6b3e49fdbc808956ee51e28)
  
