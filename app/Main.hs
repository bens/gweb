{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified App.Abutting as App (AbutLazyText, flatten)
import qualified App.Annotate as App (devDocs, userDocs)
import qualified App.Config as Config (Config (..), Input (..), loadConfig)
import App.FixN (bothNE, cataNE)
import qualified App.Graph as Graph (Gen, alg, runGen, toDot)
import qualified App.Options as Opt (Options (..), Output (..), parseOptions)
import qualified App.Parse as App (Metadata (..), parse)
import qualified App.Render as App (render)
import qualified App.Tangle as Tangle (alg)
import App.Types (Tangle (..))
import Control.Monad (when, (>=>))
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_, traverse_)
import qualified Data.Map.Lazy as Map
import Data.Semigroup (First)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO (putStrLn, readFile)
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText.IO (writeFile)
import System.Directory (createDirectoryIfMissing)
import qualified System.Environment as Env
import System.FilePath (takeDirectory, (</>))
import qualified Text.Pandoc as PD
import Prelude hiding (readFile)

main :: IO ()
main = do
  opts <- Opt.parseOptions <$> Env.getArgs
  case opts of
    Left (errs, msg) -> mapM_ putStr errs >> putStr msg
    Right (Opt.Help msg) -> putStr msg
    Right (Opt.Run buildCfg outputs) -> do
      cfg <-
        Config.loadConfig buildCfg >>= \case
          Left err -> putStrLn err *> fail "Failed to read config file"
          Right ok -> pure ok

      for_ (Config.config'inputs cfg) $ \input' ->
        (runExceptT >=> either Text.IO.putStrLn pure) $ do
          (doc, metadata, roots) <-
            readFile (Config.input'inPath input') >>= App.parse
          tmpl <- getTemplate cfg "default"

          -- Compose two algebras into one, use them in a catamorphism.
          let alg = bothNE Graph.alg Tangle.alg
          let ( results ::
                  [(Tangle, (Graph.Gen (First (Int, Text)), App.AbutLazyText))]
                ) = [(root, cataNE alg fixn) | (root, fixn) <- roots]

          let dir = takeDirectory (Config.input'inPath input')
          let graph = Graph.runGen (traverse_ (fst . snd) results)
          let tangles = map (\(root, (_, tangled)) -> (root, tangled)) results

          when (Opt.OutputTangles `elem` outputs) $ do
            for_ tangles $ \(root, tangled) -> do
              let path = Config.config'tangleDir cfg </> tangle'path root
              wr path (App.flatten tangled)

          when (Opt.OutputDevDocs `elem` outputs) $ do
            -- Use the graph of links and appends to generate docs
            doc' <- App.devDocs dir metadata graph doc
            html <- App.render tmpl (App.metadata'title metadata) doc'
            wr (Config.input'outPathDev input') html

          when (Opt.OutputUserDocs `elem` outputs) $ do
            -- Use the graph of links and appends to generate docs
            doc' <- App.userDocs dir metadata graph doc
            html <- App.render tmpl (App.metadata'title metadata) doc'
            wr (Config.input'outPathUser input') html

          when (Opt.OutputGraphViz `elem` outputs) $ do
            wr (Config.input'outPathGraphViz input') $ do
              Graph.toDot graph

--------------------------------------------------------------------------------
-- HELPERS ---------------------------------------------------------------------
--------------------------------------------------------------------------------

readFile :: (MonadIO m) => FilePath -> m Text
readFile = liftIO . Text.IO.readFile

wr :: (MonadIO m) => FilePath -> LText.Text -> m ()
wr path ltxt = liftIO $ do
  createDirectoryIfMissing True (takeDirectory path)
  LText.IO.writeFile path ltxt

getTemplate ::
  (MonadError Text m) => Config.Config -> Text -> m (PD.Template LText.Text)
getTemplate cfg nm =
  case Map.lookup nm (Config.config'templates cfg) of
    Nothing -> throwError ("template not found: " <> Text.pack (show nm))
    Just tmpl -> pure tmpl
