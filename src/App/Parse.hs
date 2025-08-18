{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Applicative (liftA3)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State.Strict (get, put, runState)
import Data.Char (isAlphaNum)
import Data.Foldable (toList)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Text.Pandoc as PD
import Text.Pandoc.Walk (Walkable (query, walkM))
import Text.Printf (printf)
import Validation (Validation (Failure, Success), validation)

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
    handleEither = either (throwError . Text.pack . show) pure
    handleValidation = validation (throwError . Text.pack . show) pure

parsePandoc ::
  PD.Pandoc ->
  Validation [Text] (PD.Pandoc, Metadata, [(Tangle, FixNE Literate)])
parsePandoc doc = case extractInfo doc of
  Failure errs -> Failure errs
  Success (doc', metadata@(Metadata _ _ roots), blocks) -> do
    -- For each tangle root in the metadata, extract the source code blocks.
    tangles <- for (toList roots) $ \root -> do
      fixn <- buildSourceGraph blocks root
      pure (root, fixn)
    pure (doc', metadata, tangles)

-- | Process a 'PD.Pandoc' document, giving each code block a unique ID, and at
-- the same time collecting the code blocks found. Code blocks with the same
-- source level identifier are collected together into (non-empty) lists.
extractInfo ::
  PD.Pandoc -> Validation [Text] (PD.Pandoc, Metadata, CodeBlocks)
extractInfo doc@(PD.Pandoc meta _) = fn <$> parseMetadata meta
  where
    fn metadata =
      let (doc', (_, allCode)) = runState (walkM assignIds doc) (0 :: Int, id)
       in (doc', metadata, groupBy litBlockName (allCode []))

    -- Assign a unique ID to each source code block, while also collecting the
    -- parsed code blocks.
    assignIds = \case
      PD.CodeBlock attr@(blkId, cls, kv) body | blkId /= "" -> do
        (i, acc) <- get
        let kv' = kv ++ [("literate-id", Text.pack (show i))]
        put (succ i, (Literate i attr (parseCodeBlock body) :) . acc)
        pure (PD.CodeBlock (blkId, cls, kv') body)
      blk -> pure blk

-- | Given a root source block for a tangle, dereference the block names in the
-- 'CodeBlocks' mapping and produce a tree of 'Literate' nodes. Checking for
-- cycles and invalid references is done at this point.
buildSourceGraph :: CodeBlocks -> Tangle -> Validation [Text] (FixNE Literate)
buildSourceGraph blocks root = assignIds Set.empty (BlockName (tangle'name root))
  where
    assignIds seen b@(BlockName nm) =
      case (Set.member b seen, Map.lookup b blocks) of
        (_, Nothing) -> Failure [nm <> " not found"]
        (True, _) -> Failure [nm <> " is in an infinite loop"]
        (False, Just xs) -> do
          lits <- for xs $ \(Literate k attr bs) -> do
            chunks <- for bs $ \case
              Code t -> pure (Code t)
              Include ind b' -> Include ind <$> assignIds (Set.insert b seen) b'
            pure (Literate k attr chunks)
          pure (FixNE lits)

--
-- PARSING METADATA
--

parseMetadata :: PD.Meta -> Validation [Text] Metadata
parseMetadata (PD.Meta meta) = liftA3 Metadata titleV genToCV tanglesV
  where
    q :: Text -> Map Text PD.MetaValue -> Validation [Text] Text
    q k = maybe (Failure [err k]) (pure . extractText) . Map.lookup k
    err k = "Extracting tangle roots: field not found: " <> Text.pack (show k)
    titleV =
      case Map.lookup "title" meta of
        Nothing -> Failure ["No title for document"]
        Just x -> pure $ extractText x
    genToCV =
      case Map.lookup "generate-toc" meta of
        Nothing -> pure False
        Just (PD.MetaBool b) -> pure b
        Just x ->
          Failure
            [ Text.pack $
                printf "Expected boolean for generate-toc (%s)" (show x)
            ]
    tanglesV =
      case Map.lookup "tangles" meta of
        Nothing -> mempty
        Just (PD.MetaList ts) -> flip foldMap ts $ \case
          PD.MetaMap m ->
            (\nm path lang -> Set.singleton (Tangle nm (Text.unpack path) lang))
              <$> q "name" m
              <*> q "path" m
              <*> q "language" m
          _ -> Failure ["Failed to parse tangle roots"]
        Just _ -> Failure ["Failed to parse tangle roots"]

extractText :: (Walkable PD.Inline a) => a -> Text
extractText = query $ \case
  PD.Str x -> x
  PD.Space -> " "
  _ -> ""

--
-- PARSING CODE BLOCKS
--

parseCodeBlock :: Text -> [ParsedCode Text BlockName]
parseCodeBlock = assignIds []
  where
    -- I should probably use megaparsec for this rather than hand-rolling it...
    assignIds acc t =
      case Text.breakOn "<<" t of
        ("", "") -> case acc of
          [] -> []
          _ -> [Code (finish acc "")]
        (c, "") -> [Code (finish acc c)]
        (c, t') -> case Text.break (not . isBlockName) (Text.drop 2 t') of
          (_, "") -> [Code (finish acc t)]
          (bname, t'') -> case Text.splitAt 2 t'' of
            (">>", t''')
              | not (Text.null bname) ->
                  Code (finish acc c)
                    : Include (indent c) (BlockName bname)
                    : assignIds [] t'''
            (_, _) ->
              let len = Text.length c + Text.length bname + 2
               in assignIds (Text.take len t : acc) t''
    isBlockName c = isAlphaNum c || c `elem` ['-', '_']
    indent c = Text.length (Text.takeWhileEnd (/= '\n') c)
    finish acc t = List.foldr1 (flip (<>)) (t : acc)

groupBy :: (Ord k, Foldable f) => (a -> k) -> f a -> Map k (NonEmpty a)
groupBy f =
  Map.unionsWith (flip (<>)) . foldMap (\x -> [Map.singleton (f x) (x :| [])])
