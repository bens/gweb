{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module App.Types where

import Data.Functor.Classes (Show1 (..), showsBinaryWith)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Merge.Lazy as Map
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Quiet (Quiet (..))
import qualified Text.Pandoc as PD

-- * Tangle

-- | A description of source that needs to be tangled. 'tangle'name' must be a
-- code block name.
data Tangle = Tangle
  { tangle'name :: Text,
    tangle'path :: FilePath,
    tangle'language :: Text
  }
  deriving (Eq, Ord, Show, Read)

-- * BlockName

-- | A wrapper for code block names.
newtype BlockName = BlockName Text
  deriving (Eq, Ord, Show, Read)

-- * ParsedCode

-- | Either code text or an include reference. Polymorphic in the text type
-- ('t') and the reference values ('a').
data ParsedCode t a = Code t | Include Int a
  deriving (Eq, Show, Read)

instance Functor (ParsedCode t) where
  fmap f = \case
    Code t -> Code t
    Include n x -> Include n (f x)

instance Foldable (ParsedCode t) where
  foldMap f = \case
    Code _ -> mempty
    Include _ x -> f x

instance (Show t) => Show1 (ParsedCode t) where
  liftShowsPrec _ _ d (Code t) =
    showString "Code " . showsPrec d t
  liftShowsPrec sp _ d (Include i x) =
    showsBinaryWith showsPrec sp "Include" d i x

-- * Metadata

data Metadata = Metadata
  { metadata'title :: Text,
    metadata'genToC :: Bool,
    metadata'tangles :: Set Tangle
  }
  deriving (Show)

-- * Literate

-- | A node in the graph of source code blocks. Has a source-level identifier,
-- some attributes, and a list of code chunks.
data Literate a = Literate
  { litId :: Int,
    litAttr :: PD.Attr,
    litCode :: [ParsedCode Text a]
  }
  deriving (Generic)
  deriving (Show, Read) via (Quiet (Literate a))

instance Functor Literate where
  fmap f lit =
    Literate
      { litId = litId lit,
        litAttr = litAttr lit,
        litCode = map (fmap f) (litCode lit)
      }

litBlockName :: Literate a -> BlockName
litBlockName (Literate _ (k, _, _) _) = BlockName k

instance Show1 Literate where
  liftShowsPrec sp sl d (Literate i attr cb) =
    shows3With sI sAttr sCB "Literate" d i attr cb
    where
      sI = showsPrec
      sAttr = showsPrec
      sCB _ = liftShowList sp sl

-- * MapMonoid

-- | A type that automatically uses the Semigroup instance on values when maps
-- are unioned with the Semigroup operator.
newtype MapMonoid k v = MapMonoid {unMapMonoid :: Map k v} deriving (Show)

mapMonoid :: k -> v -> MapMonoid k v
mapMonoid k v = MapMonoid (Map.singleton k v)

instance (Ord k, Semigroup v) => Semigroup (MapMonoid k v) where
  MapMonoid x <> MapMonoid y = MapMonoid (Map.merge f g h x y)
    where
      f = Map.mapMissing (const id)
      g = Map.mapMissing (const id)
      h = Map.zipWithAMatched (\_ a b -> pure (a <> b))

instance (Ord k, Semigroup v) => Monoid (MapMonoid k v) where
  mempty = MapMonoid mempty
  mappend = (<>)

-- * Other helpers

shows3With ::
  (Int -> a -> ShowS) ->
  (Int -> b -> ShowS) ->
  (Int -> c -> ShowS) ->
  (String -> Int -> a -> b -> c -> ShowS)
shows3With sp1 sp2 sp3 name d x y z =
  showParen (d > 10) $
    showString name
      . (showChar ' ' . sp1 11 x)
      . (showChar ' ' . sp2 11 y)
      . (showChar ' ' . sp3 11 z)
