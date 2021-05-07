module Main where

import Data.List.Split
import System.IO
import Control.Parallel.Strategies
import GHC.Conc
import System.Environment
import System.Console.GetOpt

transpose :: [[a]] -> [[a]]
transpose mat = do
    let numRows = (length mat) - 1
        numCols = (length (mat !! 0))
    transposeIntern 0 numCols mat where
        transposeIntern :: Int -> Int -> [[a]] -> [[a]]
        transposeIntern currentCol numCols _ | numCols == currentCol = []
        transposeIntern currentCol numCols mat = (transposeIntern2 currentCol mat) : (transposeIntern (currentCol + 1) numCols mat)

        transposeIntern2 :: Int -> [[a]] -> [a]
        transposeIntern2 index mat = map (\x -> x !! index) mat

data Options = Options { optVersion :: Bool, optHelp :: Bool, optDelim :: String}

version :: String
version = "0.1.0"

defaultOptions :: Options
defaultOptions = Options { optHelp = False, optVersion = False, optDelim = "," }

options :: [OptDescr (Options -> IO Options)]
options = [ Option ['v'] ["version"] (NoArg (\opts -> return opts {optVersion = True})) "Print version and exit.",
    Option ['h'] ["help"] (NoArg (\opts -> return opts {optHelp = True})) "Print help message.",
    Option ['d'] ["delim"] (ReqArg (\d -> \opts -> return opts {optDelim = d}) ",") "Set the delimiter (defaults to \",\"" ]

main :: IO ()
main = do
    c <- getContents
    args <- getArgs
    hSetBuffering stdout (BlockBuffering (Just 2048))
    let (actions, _, errors) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options {optVersion = vers, optDelim = delim, optHelp = h} = opts
    if vers
    then
        putStrLn (version)
    else
        if h
        then
            putStrLn (usageInfo "Usage: csvTranspose [Options]..." options)
        else do
            let lines = endBy "\n" c
                mat = map (\x -> endBy delim x) lines
                mat2 = transpose mat
                mat3 = map (\x -> map (\y -> delim ++ y) x) mat2
                mat4 = map (\x -> foldr (++) "" x) mat3
                mat5 = (map (\x -> (drop (length delim) x) ++ "\n") mat4) `using` parListChunk numCapabilities rdeepseq
                mat6 = foldr (++) "" mat5
            putStr mat6

