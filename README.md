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
  - [Stack machine](interpr/rpn.hs).
    The way I've interpreted the task was to create a separate container (list) for commands, which is "random-access".
    The stack is only for data now.
    - Unconditional jump
      ```
      *Main> runRpn [RpnJmpOp 3, RpnBinOp BinOpAdd, RpnJmpOp 3, RpnBinOp BinOpMul, RpnJmpOp (-3)] (0, [1,2,3])
      [5.0]
      ```
    - Conditional jump
      ```
      *Main> runRpn [RpnJmpIfOp 3, RpnBinOp BinOpAdd, RpnJmpOp 2, RpnBinOp BinOpMul] (0, [0,1,2])
      [2.0]
      *Main> runRpn [RpnJmpIfOp 3, RpnBinOp BinOpAdd, RpnJmpOp 2, RpnBinOp BinOpMul] (0, [1,1,2])
      [3.0]
      ```
    - Unconditional jump using stack
      ```
      *Main> runRpn [RpnBinOp BinOpAdd, RpnCtxJmpOp, RpnBinOp BinOpMul, RpnBinOp BinOpAdd] (0, [1,1,2,3])
      [5.0]
      ```
    - Conditional jump using stack
      ```
      *Main> runRpn [RpnBinOp BinOpAdd, RpnCtxJmpIfOp, RpnPush 1, RpnJmpOp 2, RpnPush 2, RpnBinOp BinOpMul] (0, [-1,1,3,5])
      [10.0]
      *Main> runRpn [RpnBinOp BinOpAdd, RpnCtxJmpIfOp, RpnPush 1, RpnJmpOp 2, RpnPush 2, RpnBinOp BinOpMul] (0, [-1,2,3,5])
      [5.0]
      ```
- [Huffman archiver](huffman). Encodes bytes from a file via Huffman encoding. You can use it like so:
  - To archive:
    ```
    ./huffman -c input.bin archive.bin 
    ```
  - To extract from archive:
    ```
    ./huffman -x archive.bin output.bin
    ```
- [Register Machine](register/RegisterMachine.hs) via ```State``` monad. Usage example [here](register/main.hs).
  
