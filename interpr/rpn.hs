import Interpr (BinOp(..), UnOp(..), unOpSemantics, binOpSemantics)

import Data.Map as M ((!))

data RpnCommand = RpnPush       Double
                | RpnUnOp       UnOp
                | RpnBinOp      BinOp
                | RpnJmpOp      Int
                | RpnJmpIfOp    Int
                | RpnCtxJmpOp
                | RpnCtxJmpIfOp
                deriving (Show)

jmp :: Int -> Int -> Int
jmp ptr n = ptr + n
 
execRpn :: RpnCommand -> (Int, [Double]) -> (Int, [Double])
execRpn (RpnPush x)    (ptr, stack)      = (ptr + 1, x:stack)
execRpn (RpnUnOp op)   (ptr, (z:zs))     = (ptr + 1, ((unOpSemantics ! op) z) : zs)
execRpn (RpnBinOp op)  (ptr, (u:(v:zs))) = (ptr + 1, ((binOpSemantics ! op) u v) : zs)
execRpn (RpnJmpOp n)   (ptr, stack)      = (jmp ptr n, stack)
execRpn (RpnJmpIfOp n) (ptr, (z:zs))     = if z == 0 then (jmp ptr n, zs) else (ptr + 1, zs)
execRpn RpnCtxJmpOp    (ptr, (z:zs))     = (jmp ptr $ round z, zs)
execRpn RpnCtxJmpIfOp  (ptr, (u:(v:zs))) = if u == 0 then (jmp ptr $ round v, zs) else (ptr + 1, zs)
 
runRpn :: [RpnCommand] -> (Int, [Double]) -> [Double]
runRpn commands (ptr, stack)
  | length commands == ptr = stack
  | otherwise              = runRpn commands state
  where state = execRpn (commands !! ptr) (ptr, stack)