import RegisterMachine

import System.IO
import qualified Data.Map as M

calc = do
    load 3     -- 0:  3
    write 1    -- 0:  3, 1:  3
    load 2     -- 0:  2, 1:  3
    sub 1      -- 0: -1, 1:  3
    sign       -- 0: -1, 1:  3
    write 2    -- 0: -1, 1:  3, 2: -1
    load 4     -- 0:  4, 1:  3, 2: -1
    mul 2      -- 0: -4, 1:  3, 2: -1
    inc        -- 0: -3, 1:  3, 2: -1
    write 3    -- 0: -3, 1:  3, 2: -1, 3: -3
    load 2     -- 0:  2, 1:  3, 2: -1, 3: -3
    add 2      -- 0:  1, 1:  3, 2: -1, 3: -3
    sub 3      -- 0:  4, 1:  3, 2: -1, 3: -3
    mul 1      -- 0: 12, 1:  3, 2: -1, 3: -3
    write 1    -- 0: 12, 1: 12, 2: -1, 3: -3
    rread 3    -- 0: -3, 1: 12, 2: -1, 3: -3
    dec        -- 0: -4, 1: 12, 2: -1, 3: -3
    negt       -- 0:  4, 1: 12, 2: -1, 3: -3
    write 3    -- 0:  4, 1: 12, 2: -1, 3:  4
    rread 1    -- 0: 12, 1: 12, 2: -1, 3:  4
    rdiv 3     -- 0:  3, 1: 12, 2: -1, 3:  4
    write 1    -- 0:  3, 1:  3, 2: -1, 3:  4
    rread 3    -- 0:  4, 1:  3, 2: -1, 3:  4
    rmod 1     -- 0:  1, 1:  3, 2: -1, 3:  4

main = do
    putStrLn $ "Result: " ++ (show (runReg calc (M.fromList [])))
