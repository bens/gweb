{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Graph
  ( Graph,
    Edge (..),
    graph,
    toDot,

    -- * The Algebra
    alg,
    Gen,
    runGen,
  )
where

import App.FixN (AlgNE, FixNE, cataNE)
import App.Parse (Literate (..))
import App.Types (BlockName (..), ParsedCode (..))
import Control.Applicative (liftA3)
import Control.Monad.State.Strict (State, execState, modify)
import Data.Bool (bool)
import Data.Foldable (traverse_)
import qualified Data.Graph.Inductive as G
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Semigroup (First (First), sconcat)
import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import Text.Printf (printf)

type Graph = G.Gr (Text, Literate BlockName) Edge

data Edge = Link | Next
  deriving (Eq, Show)

newtype Gen a = Gen {runGenG :: State Graph a}
  deriving (Functor, Applicative, Monad)

runGen :: Gen a -> Graph
runGen = flip execState G.empty . runGenG

instance (Semigroup a) => Semigroup (Gen a) where
  Gen m <> Gen n = Gen (liftA2 (<>) m n)

instance (Monoid a) => Monoid (Gen a) where
  mempty = pure mempty
  mappend = (<>)

-- | Convert every node in the source graph tree into a computation that
-- generates graph fragments, then run all of those computations one after
graph :: (Foldable f) => f (FixNE Literate) -> Graph
graph = runGen . traverse_ (cataNE alg)

-- | Convert a 'Literate' source code tree node into a computation which will
-- generate a graph fragment.
alg :: AlgNE Literate (Gen (First (Int, Text)))
alg lits = do
  firsts <-
    for (zipPrevNE lits) $ \(Literate i attr@(name, _, _) code, prev) -> do
      -- Add cross-block links to the graph. Links to a chain of appending
      -- blocks must link to the head of the list, hence using 'First'.
      code_with_link_edge_actions <- for code $ \case
        Code t -> pure (Code t, pure ())
        Include n (x :: Gen (First (Int, Text))) -> do
          First (j, nm) <- x
          pure (Include n (BlockName nm), addEdge Link (Just i) (Just j))
      let (code', add_link_edge_actions) = unzip code_with_link_edge_actions
      -- Add the current node to the graph.
      addNode i (name, Literate i attr code')
      -- Add an edge from the preceding node in the append-chain if it exists.
      -- This makes 'AlgNE' necessary as 'Alg' doesn't give us the neighbour
      -- information.
      addEdge Next (litId <$> prev) (Just i)
      -- Add the edges to the linked nodes.
      sequence_ add_link_edge_actions
      pure (pure (i, name))
  pure (sconcat firsts)
  where
    addNode i x =
      Gen . modify $ liftA3 bool (G.insNode (i, x)) id (G.gelem i)
    addEdge e (Just i) (Just j) =
      Gen . modify $ liftA3 bool (G.insEdge (i, j, e)) id (`G.hasEdge` (i, j))
    addEdge _ _ _ = pure ()

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
      | (i, (nm, _)) <- G.labNodes gr,
        let msg =
              printf "  u%d [label=%s];" i (show nm :: String)
      ]
    edges =
      [ LText.pack (msg :: String)
      | (i, j, ty) <- G.labEdges gr,
        let msg =
              printf "  u%d -> u%d [label=\"%s\"];" i j (show ty :: String)
      ]
