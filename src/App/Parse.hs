{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module App.Parse (Literate (..), Metadata (..), parse) where

import App.FixN (FixNE (..))
import App.Types
  ( BlockName (..),
    Literate (..),
    Metadata (..),
    ParsedCode (..),
    Tangle (..),
    litBlockName,
  )
import Control.Monad (guard)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (get, put, runState)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Traversable (for)
import qualified Text.Pandoc as PD
import Text.Pandoc.Walk (Walkable (query, walkM))
import Text.Printf (printf)
import Validation (Validation (Failure, Success), failure, validation)

-- | A mapping from block names to parsed code blocks which we get from parsing
-- a Pandoc document.
type CodeBlocks =
  Map BlockName (NonEmpty (Literate BlockName))

-- | Parse out:
--   - a Pandoc representation of the document,
--   - the file header metadata,
--   - and a representation of the source code blocks and their links.
parse ::
  (MonadError Text m) =>
  Text -> m (PD.Pandoc, Metadata, [(Tangle, FixNE Literate)])
parse input = do
  r <- handleEither . PD.runPure $ PD.readMarkdown mdOpts input
  handleValidation (parsePandoc r)
  where
    mdOpts = PD.def {PD.readerExtensions = PD.pandocExtensions}
    handleEither =
      either (throwError . Text.pack . show) pure
    handleValidation =
      validation (throwError . Text.pack . show . NE.toList) pure

parsePandoc ::
  PD.Pandoc ->
  Validation (NonEmpty Text) (PD.Pandoc, Metadata, [(Tangle, FixNE Literate)])
parsePandoc doc = case extractInfo doc of
  Failure errs -> Failure errs
  Success (doc', metadata@(Metadata _ _ roots), blocks) -> do
    -- For each tangle root in the metadata, extract the source code blocks.
    tangles <- for (toList roots) $ \root -> do
      fixn <- buildSourceGraph blocks root
      pure (root, fixn)
    pure (doc', metadata, tangles)

type Accum a = [a] -> [a]

runAccum :: Accum a -> [a]
runAccum acc = acc []

accumulate :: Accum a -> a -> Accum a
accumulate acc lit = (lit :) . acc

-- | Process a 'PD.Pandoc' document, giving each code block a unique ID, and at
-- the same time collecting the code blocks found. Code blocks with the same
-- source level identifier are collected together into (non-empty) lists.
extractInfo ::
  PD.Pandoc -> Validation (NonEmpty Text) (PD.Pandoc, Metadata, CodeBlocks)
extractInfo doc@(PD.Pandoc meta _) = fn <$> parseMetadata meta
  where
    fn metadata =
      let (doc', (_, code_accum)) = runState (walkM findLinks doc) (0 :: Int, id)
       in (doc', metadata, groupBy litBlockName (runAccum code_accum))

    -- Assign a unique ID to each source code block, while also collecting the
    -- parsed code blocks.
    findLinks = \case
      PD.CodeBlock attr@(blkId, cls, kv) body | blkId /= "" -> do
        (i, acc) <- get
        let kv' = kv ++ [("literate-id", Text.pack (show i))]
        put (succ i, accumulate acc (Literate i attr (parseCodeBlock body)))
        pure (PD.CodeBlock (blkId, cls, kv') body)
      blk -> pure blk

-- | Given a root source block for a tangle, dereference the block names in the
-- 'CodeBlocks' mapping and produce a tree of 'Literate' nodes. Checking for
-- cycles and invalid references is done at this point.
buildSourceGraph ::
  CodeBlocks -> Tangle -> Validation (NonEmpty Text) (FixNE Literate)
buildSourceGraph blocks root = findLinks Set.empty (BlockName (tangle'name root))
  where
    findLinks seen b@(BlockName nm) =
      case (Set.member b seen, Map.lookup b blocks) of
        (_, Nothing) -> failure (nm <> " not found")
        (True, _) -> failure (nm <> " is in an infinite loop")
        (False, Just xs) -> do
          lits <- for xs $ \(Literate k attr bs) -> do
            chunks <- for bs $ \case
              Code t -> pure (Code t)
              Include ind b' -> Include ind <$> findLinks (Set.insert b seen) b'
            pure (Literate k attr chunks)
          pure (FixNE lits)

--
-- PARSING METADATA
--

parseMetadata :: PD.Meta -> Validation (NonEmpty Text) Metadata
parseMetadata (PD.Meta meta) = do
  metadata'title <- case Map.lookup "title" meta of
    Nothing -> failure "No title for document"
    Just x -> pure $ extractText x
  metadata'genToC <- case Map.lookup "generate-toc" meta of
    Nothing -> pure False
    Just (PD.MetaBool b) -> pure b
    Just x -> failure . Text.pack $ do
      printf "Expected boolean for generate-toc (%s)" (show x)
  metadata'tangles <- case Map.lookup "tangles" meta of
    Nothing -> mempty
    Just (PD.MetaList ts) -> Set.fromList <$> traverse parseTangle ts
    Just _ -> failure "Failed to parse tangle roots"
  pure (Metadata {..})

parseTangle :: PD.MetaValue -> Validation (NonEmpty Text) Tangle
parseTangle = \case
  PD.MetaMap m -> do
    name <- q "name" m
    path <- q "path" m
    lang <- q "language" m
    pure (Tangle name (Text.unpack path) lang)
  _ -> failure "Failed to parse tangle roots"
  where
    q k = maybe (missingField k) (pure . extractText) . Map.lookup k
    missingField k = failure $ do
      "Extracting tangle roots: field not found: " <> Text.pack (show k)

extractText :: (Walkable PD.Inline a) => a -> Text
extractText = query $ \case
  PD.Str x -> x
  PD.Space -> " "
  _ -> ""

--
-- PARSING CODE BLOCKS
--

data Chunk = LBra Int64 | RBra Int64 | Literal LazyText.Text
  deriving (Show)

parseCodeBlock :: Text -> [ParsedCode Text BlockName]
parseCodeBlock =
  map (first LazyText.toStrict) . matchChunks . simplifyChunks . toChunks
  where
    -- Find all the angle bracket symbols in the input.
    toChunks t = case Text.break (`elem` ['<', '>']) t of
      (pre, Text.uncons -> Just ('<', t')) ->
        Literal (LazyText.fromStrict pre) : LBra 1 : toChunks t'
      (pre, Text.uncons -> Just ('>', t')) ->
        Literal (LazyText.fromStrict pre) : RBra 1 : toChunks t'
      (pre, t') ->
        [Literal (LazyText.fromStrict pre <> LazyText.fromStrict t')]

    -- Coalesce all the runs of LBra and RBra values. All remaining LBra and
    -- RBra have the length "2".
    simplifyChunks = \case
      [] -> []
      -- Eliminate empty strings so they don't get between angle brackets.
      x : Literal "" : xs -> simplifyChunks (x : xs)
      LBra n : LBra m : xs -> simplifyChunks (LBra (n + m) : xs)
      LBra n : xs -> langles n ++ simplifyChunks xs
      RBra n : RBra m : xs -> simplifyChunks (RBra (n + m) : xs)
      RBra n : xs -> rangles n ++ simplifyChunks xs
      x : xs -> x : simplifyChunks xs

    -- Match patterns of <<, name, >>. Also coalesces literals and converts LBra
    -- and RBra that don't match the pattern back into literal text.
    matchChunks = \case
      [] -> []
      Literal t : LBra _ : Literal (asBlockName -> Just name) : RBra _ : xs ->
        Code t : Include (indent t) name : matchChunks xs
      Literal t : Literal t' : xs -> matchChunks (Literal (t <> t') : xs)
      Literal t : LBra _ : xs -> matchChunks (Literal (t <> "<<") : xs)
      Literal t : RBra _ : xs -> matchChunks (Literal (t <> ">>") : xs)
      Literal "" : [] -> []
      Literal t : [] -> [Code t]
      LBra _ : Literal (asBlockName -> Just name) : RBra _ : xs ->
        Include 0 name : matchChunks xs
      LBra _ : xs -> Code "<<" : matchChunks xs
      RBra _ : xs -> Code ">>" : matchChunks xs

    asBlockName name =
      BlockName (LazyText.toStrict name)
        <$ guard (LazyText.all (\c -> isAlphaNum c || c `elem` ['-', '_']) name)
    indent =
      fromIntegral . LazyText.length . LazyText.takeWhileEnd (/= '\n')

    -- These functions turn any solitary angle brackets back into text, and runs
    -- of more than two brackets return the excess as text, leaving just a
    -- double-angle bracket on the side appropriate for the direction.
    langles n
      | 1 == n = [Literal "<"]
      | 2 < n = [Literal (LazyText.replicate (n - 2) "<"), LBra 2]
      | otherwise = [LBra 2]
    rangles n
      | 1 == n = [Literal ">"]
      | 2 < n = [RBra 2, Literal (LazyText.replicate (n - 2) ">")]
      | otherwise = [RBra 2]

groupBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f =
  Map.unionsWith (flip (<>)) . foldMap (\x -> [Map.singleton (f x) (x :| [])])
