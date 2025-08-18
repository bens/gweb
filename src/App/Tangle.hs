{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Tangle (tangle, alg) where

import App.Abutting (Abutting (abut, indent, wrap), flatten)
import App.FixN (AlgNE, FixNE (..), cataNE)
import App.Parse (Literate (..))
import App.Types (ParsedCode (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText

tangle :: FixNE Literate -> LText.Text
tangle lit = flatten (cataNE alg lit)

-- Simplify a tree of Literates into a list of lines.
alg :: (Abutting a) => AlgNE Literate a
alg = snd . foldr (flip go) (False, wrap "") . foldMap litCode
  where
    -- Code to the left and right of included code is abutted.
    go :: (Abutting a) => (Bool, a) -> ParsedCode Text a -> (Bool, a)
    go (forceAbut, rest) = \case
      Code t
        | forceAbut -> (False, wrap (LText.fromStrict t) `abut` rest)
        | otherwise -> (False, wrap (LText.fromStrict t) <> rest)
      Include n lt -> (True, indent n lt `abut` rest)
