{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Graph
  ( Graph,
    Edge (..),
    toGraph,

    -- * The Algebra
    GraphGen,
    runGraphGen,
    alg,

    -- * Visualisation
    toDot,
  )
where

import App.FixN (AlgNE, FixNE, cataNE)
import App.Parse (Literate (..))
import App.Types (BlockName (..), CodeChunk (..), NodeID (..), litBlockName)
import Control.Monad.State.Strict (State, execState, modify)
import Data.Foldable (traverse_)
import qualified Data.Graph.Inductive as G
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (First (First), sconcat)
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import Text.Printf (printf)

data Edge = Link | Next
  deriving (Eq, Show)

type Graph = G.Gr (BlockName, Literate BlockName) Edge

-- | A graph-generating action which can return a value. We can produce these
-- actions independently of one another because all of the node ids have already
-- been allocated. No state threading of IDs is needed, just accumulating graph
-- structure.
newtype GraphGen a = GraphGen {unGraphGen :: State Graph a}
  deriving (Functor, Applicative, Monad)

runGraphGen :: GraphGen a -> Graph
runGraphGen = flip execState G.empty . unGraphGen

instance (Semigroup a) => Semigroup (GraphGen a) where
  GraphGen m <> GraphGen n = GraphGen (liftA2 (<>) m n)

instance (Monoid a) => Monoid (GraphGen a) where
  mempty = pure mempty
  mappend = (<>)

-- | Convert every node in the source graph tree into a computation that
-- generates graph fragments, then run all of those computations one after the
-- other.
toGraph :: (Foldable f) => f (FixNE Literate) -> Graph
toGraph = runGraphGen . traverse_ (cataNE alg)

-- | Convert a 'Literate' source code tree node into a computation which will
-- generate a graph fragment. The return value is the graph node ID and the
-- source block ID from the document of the first encountered block with that
-- block ID. This is so that the next level up can reference that first block.
alg :: AlgNE Literate (GraphGen (First (NodeID, BlockName)))
alg lits = do
  firsts <-
    for (zipPrevNE lits) $ \(lit@(Literate i attr code), m_prev) -> do
      -- Add cross-block links to the graph. Links to a chain of appending
      -- blocks must link to the head of the list, hence using 'First'.
      code_with_include_actions <- for code $ \case
        Code t -> pure (Code t, pure ())
        Include n getInclude -> do
          First (j, include_name :: BlockName) <- getInclude
          pure (Include n include_name, addEdge Link i j)
      let (code', add_include_actions) = unzip code_with_include_actions
      -- Add the current node to the graph.
      addNode i (litBlockName lit, Literate i attr code')
      -- Add the edges to the included nodes.
      sequence_ add_include_actions
      -- Add an edge from the preceding node in the append-chain if it exists.
      -- This makes 'AlgNE' necessary as 'Alg' doesn't give us the neighbour
      -- information.
      case m_prev of
        Nothing -> pure ()
        Just prev -> addEdge Next (litNodeId prev) i
      pure (First (i, litBlockName lit))
  pure (sconcat firsts)
  where
    addNode (NodeID i) x = GraphGen . modify $ \g ->
      let already_exists = i `G.gelem` g
       in if already_exists then g else G.insNode (i, x) g
    addEdge e (NodeID i) (NodeID j) = GraphGen . modify $ \g ->
      let already_exists = g `G.hasEdge` (i, j)
       in if already_exists then g else G.insEdge (i, j, e) g

-- | Zip the elements of a list with the previous element, or with 'Nothing' for
-- the first value.
zipPrevNE :: NonEmpty a -> NonEmpty (a, Maybe a)
zipPrevNE (x :| xs) = (x, Nothing) :| zip xs (map Just (x : xs))

toDot :: Graph -> LText.Text
toDot gr =
  mconcat
    [ "digraph G {\n",
      LText.unlines nodes,
      LText.unlines edges,
      "}\n"
    ]
  where
    nodes =
      [ LText.pack (msg :: String)
      | (i, (BlockName nm, _)) <- G.labNodes gr,
        let msg =
              printf "  u%d [label=%s];" i (show nm :: String)
      ]
    edges =
      [ LText.pack (msg :: String)
      | (i, j, ty) <- G.labEdges gr,
        let msg =
              printf "  u%d -> u%d [label=\"%s\"];" i j (show ty :: String)
      ]
