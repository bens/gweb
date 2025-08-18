{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Config (Config (..), Input (..), loadConfig) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAlpha)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO (readFile)
import qualified Data.Text.Lazy as LText
import Data.Traversable (for)
import Data.Void (Void)
import System.Directory (listDirectory)
import System.FilePath
  ( dropExtension,
    replaceExtension,
    takeDirectory,
    takeExtension,
    (</>),
  )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Error as PE
import qualified Text.Pandoc.Templates as PD
import qualified Text.Parser.Permutation as PP

-- | Preliminary config data type, just holds directory paths.
data ConfigRead = ConfigRead
  { cr'docsDir :: FilePath,
    cr'templatesDir :: FilePath,
    cr'devOutputDir :: FilePath,
    cr'userOutputDir :: FilePath,
    cr'tangleDir :: FilePath,
    cr'graphvizDir :: FilePath
  }
  deriving (Show)

-- | Configuration, collected from finding and reading files in configured
-- directories.
data Config = Config
  { config'inputs :: [Input],
    config'templates :: Map Text (PD.Template LText.Text),
    config'devOutputDir :: FilePath,
    config'userOutputDir :: FilePath,
    config'tangleDir :: FilePath,
    config'graphvizDir :: FilePath
  }
  deriving (Show)

-- | An input Markdown file, along with the corresponding output paths.
data Input = Input
  { input'inPath :: FilePath,
    input'outPathDev :: FilePath,
    input'outPathUser :: FilePath,
    input'outPathGraphViz :: FilePath
  }
  deriving (Show)

loadConfig :: FilePath -> IO (Either String Config)
loadConfig path = runExceptT $ do
  let dir = takeDirectory path
  -- Need to check the file exists!
  t <- liftIO (Text.IO.readFile path)
  case P.parse (parseConfigRead dir) path t of
    Left err -> throwError (PE.errorBundlePretty err)
    Right cfg_read -> loadConfigRead cfg_read

parseConfigRead :: FilePath -> P.Parsec Void Text ConfigRead
parseConfigRead dir = PP.permute $ do
  ConfigRead
    PP.<$$> filepath_opt "DOCS_PATH"
    PP.<||> filepath_opt "TEMPLATES_PATH"
    PP.<||> filepath_opt "DEV_OUTPUT_PATH"
    PP.<||> filepath_opt "USER_OUTPUT_PATH"
    PP.<||> filepath_opt "TANGLE_PATH"
    PP.<||> filepath_opt "GRAPHVIZ_PATH"
  where
    filepath_opt name = do
      P.string name *> spaces *> P.single '=' <* spaces
      path <- filepath <* (void P.eol <|> P.eof)
      pure (dir </> Text.unpack path)
    filepath = scanner "FILEPATH" (\x -> isAlpha x || x `elem` ['_', '-', '/'])
    scanner nm = P.label nm . P.takeWhile1P Nothing
    spaces = P.skipMany (P.single (' ' :: Char))

loadConfigRead :: ConfigRead -> ExceptT String IO Config
loadConfigRead cfg_read = do
  let config'devOutputDir = cr'devOutputDir cfg_read
  let config'userOutputDir = cr'userOutputDir cfg_read
  let config'graphvizDir = cr'graphvizDir cfg_read
  let config'tangleDir = cr'tangleDir cfg_read

  let docsDir = cr'docsDir cfg_read
  inputs <- liftIO (filter isMarkdown <$> listDirectory docsDir)
  let config'inputs = do
        x <- inputs
        let devOut = config'devOutputDir </> replaceExtension x "html"
        let userOut = config'userOutputDir </> replaceExtension x "html"
        let gvOut = config'graphvizDir </> replaceExtension x "dot"
        [Input (docsDir </> x) devOut userOut gvOut]

  let tmplDir = cr'templatesDir cfg_read
  tmpls <- liftIO (filter isTemplate <$> listDirectory tmplDir)
  config'templates <- fmap Map.unions . for tmpls $ \name -> do
    t <- liftIO (Text.IO.readFile (tmplDir </> name))
    liftIO (PD.compileTemplate "" t) >>= \case
      Right tmpl -> do
        let key = Text.pack (dropExtension name)
        pure (Map.singleton key tmpl)
      Left err -> throwError err

  pure (Config {..})

isMarkdown :: FilePath -> Bool
isMarkdown = (== ".md") . takeExtension

isTemplate :: FilePath -> Bool
isTemplate = (== ".template") . takeExtension
