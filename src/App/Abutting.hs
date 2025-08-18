{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Abutting (Abut, abut, wrap, indent, flatten) where

import Data.DList (DList)
import qualified Data.DList as DL
import qualified Data.Text.Lazy as LText

data Line = Line
  { lineIndent :: !Int,
    lineText :: !LText.Text
  }
  deriving (Show)

-- | The 'Semigroup' instance on lines does the abutting.
instance Semigroup Line where
  Line i a <> Line _ b = Line i (a <> b)

-- | For collecting lines of lazy text together, where the last line of one
-- segment can be joined to the first line of another segment. Also, collections
-- of lines can be indented by some number of spaces.
data Abut
  = Nil
  | One !Line
  | -- | First line, middle lines, last line.
    Many !Line (DList Line) !Line
  deriving (Show)

-- | The 'Semigroup' instance collects lines together without abutting.
instance Semigroup Abut where
  Nil <> y = y
  x <> Nil = x
  One x <> One y = Many x DL.empty y
  One x <> Many b bs b' = Many x (DL.cons b bs) b'
  Many a as a' <> One y = Many a (DL.snoc as a') y
  Many a as a' <> Many b bs b' = Many a (as <> DL.fromList [a', b] <> bs) b'

instance Monoid Abut where
  mempty = Nil
  mappend = (<>)

-- | Split into lines.
wrap :: LText.Text -> Abut
wrap lt =
  case LText.breakOn "\n" lt of
    ("", "") -> Nil
    (x, "") -> One (e x)
    (x, lt') -> case LText.breakOnEnd "\n" (LText.drop 1 lt') of
      ("", y) -> Many (e x) DL.empty (e y)
      (lt'', y) -> Many (e x) (DL.fromList (map e (LText.lines lt''))) (e y)
  where
    e = Line 0

-- | Combine two segments of lines, joining the last of one line with the first
-- line of the other.
abut :: Abut -> Abut -> Abut
abut = go
  where
    go Nil y = y
    go x Nil = x
    go (One x) (One y) = One (x <> y)
    go (One x) (Many y ys y') = Many (x <> y) ys y'
    go (Many x xs x') (One y) = Many x xs (x' <> y)
    go (Many x xs x') (Many y ys y') = Many x (xs <> DL.cons (x' <> y) ys) y'

-- | Increase the indent by 'n' columns.
indent :: Int -> Abut -> Abut
indent i = \case
  Nil -> Nil
  One x -> One (incr x)
  Many x xs x' -> Many (incr x) (fmap incr xs) (incr x')
  where
    incr (Line i' x) = Line (i + i') x

-- | Extract into a LazyText.
flatten :: Abut -> LText.Text
flatten = \case
  Nil -> ""
  One x -> flattenLine x <> "\n"
  Many x xs x' ->
    (LText.unlines . map flattenLine) $
      DL.toList (DL.cons x (DL.snoc xs x'))
  where
    flattenLine (Line n x) =
      LText.replicate (fromIntegral n) " " <> x
