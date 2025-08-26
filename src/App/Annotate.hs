{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module is a bit messy, it does the actual updates to the document
-- structure to reflect the derived information in the output.
--
-- I have some ideas for cleaning it up and reducing the number of traversals,
-- but at least it's good to have something that works to improve later.
module App.Annotate (Annotator, devDocs, userDocs) where

import App.Annotate.Headers
  ( Header (..),
    Headers,
    appendix,
    getHeaders,
    getLastHeader,
    section,
  )
import App.Diagram (diagrams)
import App.Graph (Edge (..), Graph)
import App.Parse (Literate (..), Metadata (..))
import App.Types (BlockName (..), CodeChunk (..), MapMonoid (..), mapMonoid)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, gets, modify, runStateT)
import Data.Bifunctor (bimap)
import Data.Foldable (fold, toList)
import qualified Data.Graph.Inductive as G
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Walk (Walkable (query, walk, walkM))
import Text.Read (readMaybe)

type Annotator m = FilePath -> Metadata -> Graph -> PD.Pandoc -> m PD.Pandoc

-- | Prepare document to produce developer documentation.
devDocs :: (MonadError Text m, MonadIO m) => Annotator m
devDocs dir metadata gr doc = do
  -- Include review-remark and dev-only blocks in developer docs.
  let annReview = \case
        PD.Div (ident, cls, kv) body
          | "review-remark" `elem` cls ->
              PD.Div (ident, cls, kv) $ do
                [ PD.Div ("", ["reviewer"], []) $ do
                    [PD.Plain [PD.Str ("-- " <> r)]]
                  | ("reviewer", r) <- kv
                  ]
                  ++ body
        blk -> blk
  -- Collect header information for table of contents and decorate code blocks.
  (doc_annotated, headers) <-
    runStateT (annotate dir metadata gr . walk annReview $ doc) mempty
  -- Add a table of contents if requested.
  doc_annotated_toc <-
    if metadataGenToC metadata
      then pure (annotateTableOfContents headers doc_annotated)
      else pure doc_annotated
  -- Process any diagram code blocks with external tools.
  diagrams dir doc_annotated_toc

-- | Prepare document to produce user documentation.
userDocs :: (MonadError Text m, MonadIO m) => Annotator m
userDocs dir metadata gr doc = do
  -- Exclude review-remark and dev-only blocks in developer docs.
  let annReview = \case
        PD.Div (_, cls, _) _
          | "dev-only" `elem` cls -> PD.Plain []
          | "review-remark" `elem` cls -> PD.Plain []
        blk -> blk
  -- Collect header information for table of contents and decorate code blocks.
  (doc_annotated, headers) <-
    runStateT (annotate dir metadata gr . walk annReview $ doc) mempty
  -- Add a table of contents if requested.
  doc_annotated_toc <-
    if metadataGenToC metadata
      then pure (annotateTableOfContents headers doc_annotated)
      else pure doc_annotated
  -- Process any diagram code blocks with external tools.
  diagrams dir doc_annotated_toc

-- | Collect header information for the document structure and add linking
-- decorations to code blocks that take part in literate programming.
annotate ::
  (MonadError Text m, MonadState (Headers (Text, Text)) m) =>
  Annotator m
annotate _dir _metadata gr =
  walkM $ \case
    blk@(PD.CodeBlock attr x)
      | Just (ident, ctx) <- matchCodeBlock gr attr ->
          pure (annotateCodeBlock gr ident ctx attr blk)
      | otherwise -> pure (PD.CodeBlock (noLitId attr) x)
    -- Collect all the headers to build the table of contents.
    PD.Header lvl (ident, classes, _kv) title -> do
      if lvl <= 1 && "appendix" `elem` classes
        then modify (<> appendix (ident, (extractText title)))
        else modify (<> section (ident, (extractText title)) lvl)
      gets getLastHeader >>= \case
        Just hdr -> pure (renderBodyHeader hdr)
        Nothing -> error "impossible, just added a header"
    blk -> pure blk
  where
    noLitId (blkId, cls, kv) =
      (blkId, cls, filter ((/= "literate-id") . fst) kv)

matchCodeBlock ::
  Graph ->
  PD.Attr ->
  Maybe (Text, G.Context (BlockName, Literate BlockName) Edge)
matchCodeBlock gr (_, _, kv) = do
  ident <- List.lookup "literate-id" kv
  i <- readMaybe (Text.unpack ident)
  ctx <- fst (G.match i gr)
  pure (ident, ctx)

annotateCodeBlock ::
  Graph ->
  Text ->
  G.Context (BlockName, Literate BlockName) Edge ->
  PD.Attr ->
  (PD.Block -> PD.Block)
annotateCodeBlock gr ident ctx (blkId, cls, _kv) _orig =
  PD.Div ("src-" <> ident, "literate" : cls, []) $
    [ PD.Div ("", ["header"], []) $
        ( PD.Plain . concat $
            [ renderNeighbour ("pred", "<<", blkId) (G.inn', fst) ctx,
              renderNeighbour ("succ", ">>", blkId) (G.out', snd) ctx,
              [renderTitle blkId]
            ]
        )
          : renderInclusions
            ( do
                -- Find the head of the chain, then find all the blocks that
                -- include it.
                let (BlockName nm, _) = G.lab' ctx
                hd <- toList (Map.lookup nm heads)
                j <- Map.findWithDefault [] hd incoming
                name <- toList (Map.lookup j names)
                pure ((j, name) :: (G.Node, Text))
            ),
      renderCodeBlock heads (snd (G.lab' ctx))
    ]
  where
    (names, (incoming, _outgoing)) = allLinks gr
    heads = headNodes gr

annotateTableOfContents :: Headers (Text, Text) -> PD.Pandoc -> PD.Pandoc
annotateTableOfContents headers (PD.Pandoc meta doc) =
  PD.Pandoc meta (toc ++ doc)
  where
    tocInfo = filter (trim 2) (getHeaders headers)
    toc = [PD.Div ("table-of-contents", [], []) [PD.BulletList (map f tocInfo)]]
      where
        f hdr =
          [ PD.Plain
              [ PD.Link ("", [], []) [renderContentsHeader hdr] $ do
                  ("#" <> target hdr, "")
              ]
          ]
    trim n = \case
      Section _ path -> NE.length path <= n
      Appendix _ path -> NE.length path <= n
    target = \case
      Section (ident, _) _ -> ident
      Appendix (ident, _) _ -> ident

renderBodyHeader :: Header (Text, Text) -> PD.Block
renderBodyHeader = \case
  Section (ident, title) path ->
    PD.Header (NE.length path) (ident, [], []) $ do
      leader : PD.Space : [PD.Str title]
    where
      leader = PD.Span ("", ["section-no"], []) $ do
        [PD.Str (Text.intercalate "." (map showText (NE.toList path)))]
  Appendix (ident, title) (x :| xs) ->
    PD.Header (length xs + 1) (ident, ["appendix"], []) $ do
      leader : PD.Str ":" : PD.Space : [PD.Str title]
    where
      leader = PD.Str (if null xs then "Appendix " <> ts else ts)
      ts = Text.intercalate "." (letters List.!! (x - 1) : map showText xs)
  where
    showText = Text.pack . show
    letters = concat (iterate (zipWith (<>) single_letters) single_letters)
    single_letters = map Text.singleton ['A' .. 'Z']

renderContentsHeader :: Header (Text, Text) -> PD.Inline
renderContentsHeader = \case
  Section (_, title) path ->
    PD.Span ("", ["section"], [("toc-level", showText (NE.length path))]) $ do
      [ PD.Span ("", ["toc-number"], []) $ do
          [PD.Str (Text.intercalate "." (map showText (NE.toList path)))],
        PD.Span ("", ["toc-item"], []) $ do
          [PD.Space, PD.Str title]
        ]
  Appendix (_, title) path@(x :| xs) ->
    PD.Span ("", ["appendix"], [("toc-level", showText (NE.length path))]) $ do
      [ PD.Span ("", ["toc-number"], []) $ do
          [PD.Str (if null xs then "Appendix " <> ts else ts)],
        PD.Span ("", ["toc-item"], []) $ do
          [PD.Space, PD.Str title]
        ]
    where
      ts = Text.intercalate "." (letters List.!! (x - 1) : map showText xs)
  where
    showText = Text.pack . show
    letters = concat (iterate (zipWith (<>) single_letters) single_letters)
    single_letters = map Text.singleton ['A' .. 'Z']

renderTitle :: Text -> PD.Inline
renderTitle blkId = PD.Span ("", ["title"], []) [PD.Str blkId]

renderNeighbour ::
  (Show a) =>
  (Text, Text, Text) ->
  (G.Context node Edge -> [(a, a, Edge)], (a, a) -> a) ->
  (G.Context node Edge -> [PD.Inline])
renderNeighbour (lbl, txt, blkId) (edges, prj) ctx =
  case prj3 <$> List.find (\(_, _, x) -> x == Next) (edges ctx) of
    Nothing -> [PD.Span ("", [lbl], []) [PD.Str txt]]
    Just j -> do
      [ PD.Span ("", [lbl], []) $ do
          [ PD.Link ("", [], []) [PD.Str txt] $ do
              ( "#src-" <> Text.pack (show j),
                blkId <> ":" <> Text.pack (show j)
                )
            ]
        ]
  where
    prj3 (i, j, _) = prj (i, j)

renderInclusions :: [(G.Node, Text)] -> [PD.Block]
renderInclusions incls =
  [ PD.Plain [PD.Str " | ", PD.Span ("", ["inclusions"], []) items]
  | not (null items)
  ]
  where
    items = List.intersperse PD.Space $ do
      (j, lbl) <- List.sort incls
      [ PD.Link ("", [], []) [PD.Str lbl] $ do
          ( "#src-" <> Text.pack (show j),
            lbl <> ":" <> Text.pack (show j)
            )
        ]

renderCodeBlock :: Map Text G.Node -> Literate BlockName -> PD.Block
renderCodeBlock heads lit =
  PD.Div ("", ["src"], []) . (: []) . PD.Plain $ do
    litCodeChunks lit >>= \case
      Code t -> [PD.Str t]
      Include _ (BlockName nm) ->
        case Map.lookup nm heads of
          Nothing ->
            [ PD.Str "<<",
              PD.Link ("", [], []) [PD.Str nm] ("#", ""),
              PD.Str ">>"
            ]
          Just j ->
            [ PD.Str "<<",
              PD.Link
                ("", [], [])
                [PD.Str nm]
                ("#src-" <> Text.pack (show j), ""),
              PD.Str ">>"
            ]

--
-- HELPERS
--

extractText :: (Walkable PD.Inline a) => a -> Text
extractText = query $ \case
  PD.Str x -> x
  PD.Space -> " "
  _ -> ""

-- Find the nodes that are at the head of each named chain of blocks.
headNodes :: Graph -> Map Text G.Node
headNodes gr =
  Map.fromList
    [ (nm, i)
    | (i, (BlockName nm, _)) <- G.labNodes gr,
      null [() | (_, _, Next) <- G.inn gr i]
    ]

-- Collect all the incoming and outgoing inclusion links between nodes.
allLinks ::
  Graph -> (Map G.Node Text, (Map G.Node [G.Node], Map G.Node [G.Node]))
allLinks = fmap (bimap unMapMonoid unMapMonoid) . G.ufold fn mempty
  where
    fn ctx m =
      fold
        ( m
            : (Map.singleton i lbl, mempty)
            : [ (mempty, (mapMonoid j [k], mapMonoid k [j]))
              | (j, k) <- inn ++ out
              ]
        )
      where
        (i, (BlockName lbl, _)) = G.labNode' ctx
        inn = [(i, j) | (j, _, Link) <- G.inn' ctx]
        out = [(j, i) | (_, j, Link) <- G.out' ctx]
