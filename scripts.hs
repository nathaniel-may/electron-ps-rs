#!/usr/bin/env stack
-- stack --resolver lts-21.24 --no-install-ghc --system-ghc runghc --package "./script_modules"

module Main where

import Scripts

main :: IO ()
main = Scripts.run