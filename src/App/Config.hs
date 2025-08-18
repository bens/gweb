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
  { readDocsDir :: FilePath,
    readTemplatesDir :: FilePath,
    readDevOutputDir :: FilePath,
    readUserOutputDir :: FilePath,
    readTangleDir :: FilePath,
    readGraphvizDir :: FilePath
  }
  deriving (Show)

-- | Configuration, collected from finding and reading files in configured
-- directories.
data Config = Config
  { configInputs :: [Input],
    configTemplates :: Map Text (PD.Template LText.Text),
    configDevOutputDir :: FilePath,
    configUserOutputDir :: FilePath,
    configTangleDir :: FilePath,
    configGraphvizDir :: FilePath
  }
  deriving (Show)

-- | An input Markdown file, along with the corresponding output paths.
data Input = Input
  { inputInPath :: FilePath,
    inputOutPathDev :: FilePath,
    inputOutPathUser :: FilePath,
    inputOutPathGraphViz :: FilePath
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
  let configDevOutputDir = readDevOutputDir cfg_read
  let configUserOutputDir = readUserOutputDir cfg_read
  let configGraphvizDir = readGraphvizDir cfg_read
  let configTangleDir = readTangleDir cfg_read

  let docsDir = readDocsDir cfg_read
  inputs <- liftIO (filter isMarkdown <$> listDirectory docsDir)
  let configInputs = do
        x <- inputs
        let devOut = configDevOutputDir </> replaceExtension x "html"
        let userOut = configUserOutputDir </> replaceExtension x "html"
        let gvOut = configGraphvizDir </> replaceExtension x "dot"
        [Input (docsDir </> x) devOut userOut gvOut]

  let tmplDir = readTemplatesDir cfg_read
  tmpls <- liftIO (filter isTemplate <$> listDirectory tmplDir)
  configTemplates <- fmap Map.unions . for tmpls $ \name -> do
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
