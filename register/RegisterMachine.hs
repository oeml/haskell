module RegisterMachine (
    RegisterState,
    runReg,
    load,
    write,
    rread,
    negt,
    sign,
    inc,
    dec,
    add,
    sub,
    mul,
    rdiv,
    rmod
) where

import Control.Monad (guard)
import Control.Monad.State (State, get, put, runStateT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M

type Registers a = M.Map Int a
type RegisterState a b = MaybeT (State (Registers a)) b

runReg :: RegisterState a b -> Registers a -> Maybe a
runReg state = (M.lookup 0) . snd . runIdentity . runStateT (runMaybeT state)

load :: a -> RegisterState a ()
load x = do
    registers <- get
    put (M.insert 0 x registers)

write :: Int -> RegisterState a ()
write regn = do
    registers <- get
    let reg0val = (M.lookup 0 registers)
    guard $ isJust reg0val
    let x = fromJust reg0val
    put (M.insert regn x registers)

rread :: Int -> RegisterState a ()
rread regn = do
    registers <- get
    let regnval = (M.lookup regn registers)
    guard $ isJust regnval
    let x = fromJust regnval
    put (M.insert 0 x registers)

unOp :: (a -> a) -> RegisterState a ()
unOp f = do
    registers <- get
    let reg0val = (M.lookup 0 registers)
    guard $ isJust reg0val
    let x = fromJust reg0val
    put (M.insert 0 (f x) registers)

negt = unOp (negate)
sign = unOp (signum)
inc = unOp (+ 1)
dec = unOp (+ (-1))

binOp :: (a -> a -> a) -> Int -> RegisterState a ()
binOp = conditionalBinOp defined

conditionalBinOp :: (Maybe a -> Maybe a -> Bool) -> (a -> a -> a) -> Int -> RegisterState a ()
conditionalBinOp p f regn = do
    registers <- get
    let regnval = (M.lookup regn registers)
        reg0val = (M.lookup 0 registers)
    guard $ p regnval reg0val
    let x = fromJust reg0val
        y = fromJust regnval
    put (M.insert 0 (f x y) registers)

defined a b = (isJust a) && (isJust b)

nonZero a b = (defined a b) && ((/=0) . (maybe 0 id) $ b)

add = binOp (+)
sub = binOp (-)
mul = binOp (*)
rdiv = conditionalBinOp nonZero div
rmod = conditionalBinOp nonZero mod
