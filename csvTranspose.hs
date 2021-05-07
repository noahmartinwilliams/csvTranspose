module Main where

import Data.List.Split
import System.IO
import Control.Parallel.Strategies
import GHC.Conc

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

main :: IO ()
main = do
    c <- getContents
    hSetBuffering stdout (BlockBuffering (Just 2048))
    let lines = endBy "\n" c
        mat = map (\x -> endBy "," x) lines
        mat2 = transpose mat
        mat3 = map (\x -> map (\y -> "," ++ y) x) mat2
        mat4 = map (\x -> foldr (++) "" x) mat3
        mat5 = (map (\x -> (drop 1 x) ++ "\n") mat4) `using` parListChunk numCapabilities rdeepseq
        mat6 = foldr (++) "" mat5
    putStr mat6

