{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Golden.Test as Golden
import Test.Tasty (defaultMain, testGroup)
import qualified Tests

main :: IO ()
main = do
  goldens <- Golden.tests
  discoveredTests <- Tests.tests
  defaultMain . testGroup "ALL" $
    [ testGroup "GOLDEN" goldens,
      discoveredTests
    ]
