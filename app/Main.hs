module Main where

import Lib

main :: IO ()
main = getContents >>= print . parseSql
