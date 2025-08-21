{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module App.Types where

import Data.Bifunctor (Bifunctor (bimap))
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
  { tangleName :: Text,
    tanglePath :: FilePath,
    tangleLanguage :: Text
  }
  deriving (Eq, Ord, Show, Read)

-- * NodeID

-- | A unique node ID in the graph of all blocks of code with names.
newtype NodeID = NodeID Int
  deriving (Eq, Ord)
  deriving (Show, Read) via Int

firstNode :: NodeID
firstNode = NodeID 0

nextNode :: NodeID -> NodeID
nextNode (NodeID i) = NodeID (succ i)

-- * BlockName

-- | A wrapper for code block names.
newtype BlockName = BlockName Text
  deriving (Eq, Ord, Show, Read)

-- * CodeChunk

-- | Either code text or an include reference. Polymorphic in the text type and
-- the include type.
data CodeChunk text a = Code text | Include Int a
  deriving (Eq, Show, Read)

instance Functor (CodeChunk text) where
  fmap f = \case
    Code t -> Code t
    Include n x -> Include n (f x)

instance Bifunctor CodeChunk where
  bimap f g = \case
    Code t -> Code (f t)
    Include n x -> Include n (g x)

instance Foldable (CodeChunk text) where
  foldMap f = \case
    Code _ -> mempty
    Include _ x -> f x

instance (Show text) => Show1 (CodeChunk text) where
  liftShowsPrec _ _ d (Code t) =
    showString "Code " . showsPrec d t
  liftShowsPrec sp _ d (Include i x) =
    showsBinaryWith showsPrec sp "Include" d i x

-- * Metadata

data Metadata = Metadata
  { metadataTitle :: Text,
    metadataGenToC :: Bool,
    metadataTangles :: Set Tangle
  }
  deriving (Show)

-- * Literate

-- | A node in the graph of source code blocks. Has a unique node ID, some
-- attributes, and a list of code chunks.
data Literate a = Literate
  { litNodeId :: NodeID,
    litAttr :: PD.Attr,
    litCodeChunks :: [CodeChunk Text a]
  }
  deriving (Generic)
  deriving (Show, Read) via (Quiet (Literate a))

instance Functor Literate where
  fmap f lit =
    Literate
      { litNodeId = litNodeId lit,
        litAttr = litAttr lit,
        litCodeChunks = map (fmap f) (litCodeChunks lit)
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
