import System.Environment (getArgs)
import Data.Text
import qualified Data.Text.IO as TIO

main = do
    (inf:outf:p:s:[]) <- getArgs
    contents <- TIO.readFile inf
    let pattern = pack p
        substitute = pack s
        processed = replace pattern substitute contents
    TIO.writeFile outf processed

