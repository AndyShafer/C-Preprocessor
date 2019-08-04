import System.Environment

import CPreprocessor

main = do
    args <- getArgs
    preprocessFile $ head args
