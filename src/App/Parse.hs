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
    CodeChunk (..),
    Literate (..),
    Metadata (..),
    NodeID (..),
    Tangle (..),
    litBlockName,
  )
import Control.Monad.State.Strict (get, put, runState)
import Data.Char (isAlphaNum)
import qualified Data.DList as DL
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Text.Pandoc as PD
import Text.Pandoc.Walk (Walkable (query, walkM))
import Text.Printf (printf)
import Validation (Validation (Failure, Success), failure, validation)

-- | A mapping from block names to parsed code blocks which we get from parsing
-- a Pandoc document.
type LiteratesTable =
  Map BlockName (NonEmpty (Literate BlockName))

type V = Validation (NonEmpty Text)

-- | Parse out:
--   - a Pandoc representation of the document,
--   - the file header metadata,
--   - and a representation of the source code blocks and their links.
parse :: Text -> Either Text (PD.Pandoc, Metadata, [(Tangle, FixNE Literate)])
parse input = do
  r <- handleE . PD.runPure $ PD.readMarkdown mdOpts input
  handleV $ parsePandoc r
  where
    mdOpts = PD.def {PD.readerExtensions = PD.pandocExtensions}
    handleE = either (Left . Text.pack . show) Right
    handleV = validation (Left . Text.pack . show . NE.toList) Right

parsePandoc :: PD.Pandoc -> V (PD.Pandoc, Metadata, [(Tangle, FixNE Literate)])
parsePandoc doc = case extractInfo doc of
  Failure errs -> Failure errs
  Success (doc', metadata@(Metadata _ _ roots), blocks) -> do
    -- For each tangle root in the metadata, extract the source code blocks into
    -- a 'FixNE Literate' tree.
    tangles <- for (toList roots) $ \root -> do
      fixn <- buildSourceGraph blocks root
      pure (root, fixn)
    pure (doc', metadata, tangles)

-- * Extracting Info from Document

-- | Process a 'PD.Pandoc' document, giving each code block a unique ID, and at
-- the same time collecting the code blocks found. Code blocks with the same
-- source level identifier are collected together into (non-empty) lists.
extractInfo :: PD.Pandoc -> V (PD.Pandoc, Metadata, LiteratesTable)
extractInfo doc@(PD.Pandoc raw_metadata _) = do
  metadata <- parseMetadata raw_metadata
  let (doc', (_, code_accum)) = runState (walkM assignNodeID doc) (0, mempty)
  pure (doc', metadata, groupBy litBlockName (DL.apply code_accum []))
  where
    -- Assign a unique ID to each source code block, while also collecting the
    -- parsed code blocks.
    assignNodeID = \case
      PD.CodeBlock attr@(name, cls, kv) body | name /= "" -> do
        (i, acc) <- get
        let acc' = DL.cons (Literate (NodeID i) attr (parseCodeBlock body)) acc
        let kv' = kv ++ [("literate-id", Text.pack (show i))]
        put (succ i, acc')
        pure (PD.CodeBlock (name, cls, kv') body)
      pd_block -> pure pd_block

groupBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f =
  Map.unionsWith (flip (<>)) . foldMap (\x -> [Map.singleton (f x) (x :| [])])

-- * Resolving references

-- | Given a root source block for a tangle, dereference the block names in
-- 'LiteratesTable' and produce a tree of 'Literate' nodes. Checking for cycles
-- and invalid references is done at this point.
buildSourceGraph :: LiteratesTable -> Tangle -> V (FixNE Literate)
buildSourceGraph all_literates root =
  resolveBlockName rootContext (BlockName (tangleName root))
  where
    resolveBlockName ctx name@(BlockName nm)
      | Just path <- hasSeen name ctx =
          failure ("infinite loop discovered: " <> path)
      | Just lits <- Map.lookup name all_literates =
          FixNE <$> for lits (resolveLiterate (addToContext name ctx))
      | path <- getPath ctx = failure $ do
          if path == ""
            then "root source block " <> nm <> " not found"
            else path <> ": " <> nm <> " not found"

    resolveLiterate ctx (Literate k attr code_chunks) =
      fmap (Literate k attr) . for code_chunks $ \case
        Code t -> pure (Code t)
        Include ind ref -> Include ind <$> resolveBlockName ctx ref

    rootContext = (Set.empty, [])
    getPath (_seen, path) =
      Text.intercalate " -> " (reverse path)
    hasSeen name@(BlockName t) (seen, path)
      | Set.member name seen = Just (getPath (seen, t : path))
      | otherwise = Nothing
    addToContext name@(BlockName t) (seen, path) =
      (Set.insert name seen, t : path)

-- * Parsing Metadata

parseMetadata :: PD.Meta -> V Metadata
parseMetadata (PD.Meta meta) = do
  metadataTitle <- case Map.lookup "title" meta of
    Nothing -> failure "No title for document"
    Just x -> pure $ extractText x
  metadataGenToC <- case Map.lookup "generate-toc" meta of
    Nothing -> pure False
    Just (PD.MetaBool b) -> pure b
    Just x -> failure . Text.pack $ do
      printf "Expected boolean for generate-toc (%s)" (show x)
  metadataTangles <- case Map.lookup "tangles" meta of
    Nothing -> mempty
    Just (PD.MetaList ts) -> Set.fromList <$> traverse parseTangle ts
    Just _ -> failure "Failed to parse tangle roots"
  pure (Metadata {..})

parseTangle :: PD.MetaValue -> V Tangle
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

-- * Parsing Source Code Blocks

data Chunk = LBra Int | RBra Int | Literal Text
  deriving (Show)

parseCodeBlock :: Text -> [CodeChunk Text BlockName]
parseCodeBlock = matchChunks . simplifyChunks . toChunks
  where
    -- Find all the angle bracket symbols in the input.
    toChunks (Text.break (`elem` ['<', '>']) -> (pre, post)) = case post of
      (Text.uncons -> Just ('<', lt)) -> Literal pre : LBra 1 : toChunks lt
      (Text.uncons -> Just ('>', lt)) -> Literal pre : RBra 1 : toChunks lt
      lt -> [Literal (pre <> lt)]

    -- Coalesce all the runs of LBra and RBra values. All remaining LBra and
    -- RBra values have the length "2", extras are returned to normal text.
    simplifyChunks = \case
      -- Eliminate empty strings so they don't get between angle excessBrackets.
      x : Literal "" : xs -> simplifyChunks (x : xs)
      -- Collect adjacent LBra values, then emit excess text before the LBra.
      LBra n : LBra m : xs -> simplifyChunks (LBra (n + m) : xs)
      LBra 1 : xs -> Literal "<" : simplifyChunks xs
      LBra n : xs -> Literal (excessBrackets n "<") : LBra 2 : simplifyChunks xs
      -- Collect adjacent RBra values, then emit excess text after the RBra.
      RBra n : RBra m : xs -> simplifyChunks (RBra (n + m) : xs)
      RBra 1 : xs -> Literal ">" : simplifyChunks xs
      RBra n : xs -> RBra 2 : simplifyChunks (Literal (excessBrackets n ">") : xs)
      x : xs -> x : simplifyChunks xs
      [] -> []
      where
        excessBrackets n t = Text.replicate (n - 2) t

    -- Match patterns of <<, name, >>. Also coalesces literals and converts LBra
    -- and RBra that don't match the pattern back into literal text.
    matchChunks = \case
      Literal t : LBra _ : Literal (asBlockName -> Just name) : RBra _ : xs ->
        Code t : Include (calculateIndent t) name : matchChunks xs
      Literal t : Literal t' : xs -> matchChunks (Literal (t <> t') : xs)
      Literal t : LBra _ : xs -> matchChunks (Literal (t <> "<<") : xs)
      Literal t : RBra _ : xs -> matchChunks (Literal (t <> ">>") : xs)
      Literal t : xs -> [Code t | t /= ""] ++ matchChunks xs
      LBra _ : Literal (asBlockName -> Just name) : RBra _ : xs ->
        Include 0 name : matchChunks xs
      LBra _ : xs -> matchChunks (Literal "<<" : xs)
      RBra _ : xs -> matchChunks (Literal ">>" : xs)
      [] -> []

calculateIndent :: Text -> Int
calculateIndent = Text.length . Text.takeWhileEnd (/= '\n')

asBlockName :: Text -> Maybe BlockName
asBlockName name
  | Just (a, mid, b) <- textEnds name,
    isAlphaNum a && isAlphaNum b,
    Text.all (\c -> isAlphaNum c || c `elem` ['-', '_']) mid =
      Just (BlockName name)
  | otherwise = Nothing

-- | Get the first, last, and middle text of a text value. If the text is empty
-- return Nothing, if it's one char then the first and last are the same char.
textEnds :: Text -> Maybe (Char, Text, Char)
textEnds t = case Text.uncons t of
  Just (a, as) -> case Text.unsnoc as of
    Just (mid, b) -> Just (a, mid, b)
    Nothing -> Just (a, "", a)
  Nothing -> Nothing
