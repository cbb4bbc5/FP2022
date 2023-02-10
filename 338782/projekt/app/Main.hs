
{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Lib
import Interface ( mainLoop ) 



main :: IO ()
main = putStr ">" >> evalFull mainLoop
