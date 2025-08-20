{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module App.Tangle (tangle, alg) where

import App.Abutting (Abut, abut, flatten, indent, wrap)
import App.FixN (AlgNE, FixNE (..), cataNE)
import App.Parse (Literate (..))
import App.Types (CodeChunk (..))
import Data.Text (Text)
import qualified Data.Text.Lazy as LText

tangle :: FixNE Literate -> LText.Text
tangle lit = flatten (cataNE alg lit)

-- Simplify a tree of Literates into a list of lines.
alg :: AlgNE Literate Abut
alg = snd . foldr step (False, wrap "") . foldMap litCodeChunks
  where
    -- Code to the left and right of included code is abutted -- when there's an
    -- include, the code block afterwards also must abut.
    step :: CodeChunk Text Abut -> (Bool, Abut) -> (Bool, Abut)
    step code (must_abut, rest) = case code of
      Code (wrap . LText.fromStrict -> x)
        | must_abut -> (False, x `abut` rest)
        | otherwise -> (False, x <> rest)
      Include n lt -> (True, indent n lt `abut` rest)
