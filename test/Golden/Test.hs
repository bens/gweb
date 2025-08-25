{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Golden.Test (tests) where

import qualified App.Annotate as SUT (Annotator, devDocs, userDocs)
import qualified App.Graph as SUT (toDot, toGraph)
import qualified App.Parse as SUT (Metadata (..), parse)
import qualified App.Render as SUT (render)
import qualified App.Tangle as SUT (tangle)
import App.Types (Tangle (..))
import Control.Monad.Except
  ( ExceptT,
    MonadError (throwError),
    liftEither,
    runExceptT,
  )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.Functor.Identity (runIdentity)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import qualified System.FilePath as Path
import System.IO (Handle, IOMode (WriteMode), hPutStr, hPutStrLn, withFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFileDiff)
import qualified Text.Pandoc as PD
import Text.Printf (printf)
import qualified Text.Show.Pretty as PP

tests :: IO [TestTree]
tests = sequence $ do
  [ findGoldens "test-golden/00_standard" ".md" $ \k -> sequence $ do
      [ k "parsed" (runGolden goldenParsed),
        k "tangled" (runGolden goldenTangled),
        k "graph" (runGolden goldenGraph),
        k "pandoc-dev" (runGolden (goldenPandoc SUT.devDocs)),
        k "pandoc-user" (runGolden (goldenPandoc SUT.userDocs)),
        k "html-dev" (runGolden (goldenHtml SUT.devDocs)),
        k "html-user" (runGolden (goldenHtml SUT.userDocs))
        ],
    findGoldens "test-golden/01_diagrams" ".md" $ \k -> sequence $ do
      [ k "diagram" (runGolden goldenDiagram)
        ]
    ]

type GoldenTest m = FilePath -> Text -> Handle -> m ()

goldenParsed :: (MonadIO m, MonadError Text m) => GoldenTest m
goldenParsed _path md h = do
  (doc, metadata, tangles) <- liftEither (SUT.parse md)
  prn h $ printf "title: %s\n========" (SUT.metadataTitle metadata)
  pretty h tangles
  prn h "========"
  pretty h doc

goldenTangled :: (MonadIO m, MonadError Text m) => GoldenTest m
goldenTangled _path md h = do
  (_, _, tangles) <- liftEither (SUT.parse md)
  for_ tangles $ \(Tangle name path lang, t) -> do
    prn h $ printf "=== %s [%s/%s] ===" path name lang
    pr h $ LText.unpack (SUT.tangle t)

goldenGraph :: (MonadIO m, MonadError Text m) => GoldenTest m
goldenGraph _path md h = do
  (_, _, tangles) <- liftEither (SUT.parse md)
  pr h $ LText.unpack $ SUT.toDot $ SUT.toGraph (snd <$> tangles)

goldenPandoc :: (MonadIO m, MonadError Text m) => SUT.Annotator m -> GoldenTest m
goldenPandoc fn path md h = do
  let dir = Path.takeDirectory path
  (doc, metadata, tangles) <- liftEither (SUT.parse md)
  doc' <- fn dir metadata (SUT.toGraph (snd <$> tangles)) doc
  pretty h doc'

goldenHtml :: (MonadIO m, MonadError Text m) => SUT.Annotator m -> GoldenTest m
goldenHtml fn path md h = do
  let dir = Path.takeDirectory path
  (doc, metadata, tangles) <- liftEither (SUT.parse md)
  doc' <- fn dir metadata (SUT.toGraph (snd <$> tangles)) doc
  tmpl <- getTemplate
  html <- SUT.render tmpl (SUT.metadataTitle metadata) doc'
  pr h (LText.unpack html)

goldenDiagram :: (MonadIO m, MonadError Text m) => GoldenTest m
goldenDiagram path md h = do
  pure ()

--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------

prn :: (MonadIO m) => Handle -> String -> m ()
prn h = liftIO . hPutStrLn h

pr :: (MonadIO m) => Handle -> String -> m ()
pr h = liftIO . hPutStr h

pretty :: (Show a, MonadIO m) => Handle -> a -> m ()
pretty h = liftIO . hPutStrLn h . PP.ppShow

findGoldens ::
  FilePath ->
  FilePath ->
  ((String -> GoldenTest IO -> IO TestTree) -> IO [TestTree]) ->
  IO TestTree
findGoldens dir extnIn f = do
  let basename = Path.takeBaseName dir
  inputs <- List.sort <$> findByExtension [extnIn] dir
  fmap (testGroup basename) . for inputs $ \inp -> do
    let filename = Path.makeRelative dir inp
    let testGroupName = printf "%s" (Path.dropExtensions filename) :: String
    md <- Text.IO.readFile inp
    fmap (testGroup testGroupName) . f $ \extnGolden g -> do
      let golden = inp `Path.replaceExtension` extnGolden
          diff ref _new = ["git", "diff", "--exit-code", "-u", ref]
      pure $
        goldenVsFileDiff extnGolden diff golden golden $
          withFile golden WriteMode (g inp md)

runGolden :: (MonadIO m) => GoldenTest (ExceptT Text m) -> GoldenTest m
runGolden fn path md h = do
  runExceptT (fn path md h) >>= \case
    Left err -> liftIO (Text.IO.hPutStrLn h err)
    Right ok -> pure ok

getTemplate :: (MonadError Text m) => m (PD.Template LText.Text)
getTemplate =
  either (throwError . Text.pack) pure . runIdentity $
    PD.compileTemplate
      ""
      "<html><head><title>$title$</title></head>\n\
      \<body>\n$body$\n</body>\n\
      \</html>\n"
