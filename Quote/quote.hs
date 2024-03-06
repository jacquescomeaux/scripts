#!/usr/bin/env stack
-- stack --resolver lts-21.3 script

import System.Environment (getArgs)

main = getArgs
    >>= pure . head
    >>= readFile
    >>= pure . quote
    >>= putStrLn

quote = unlines . fmap show . lines
