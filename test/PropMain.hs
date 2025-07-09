{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point for running property-based tests
module Main where

import PropertyTests (runPropertyTests)

main :: IO ()
main = runPropertyTests
