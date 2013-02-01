module Main where
import System.Environment

rInt :: String -> Int
rInt = read

main :: IO ()
main = do args <- getArgs
          putStrLn ("The sum is " ++ show ((rInt $ args !! 0) + (rInt  $ args !! 1)))
