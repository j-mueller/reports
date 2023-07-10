{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
{-| CLI Options for report builder
-}
module Reports.Cli(
  runMain,
) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, void)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text.IO
import qualified Data.YAML.Aeson        as Yaml
import           Development.Shake      ((%>))
import qualified Development.Shake      as Shake
import qualified Dhall.Yaml             as Dhall
import           Options.Applicative    (Parser, customExecParser, disambiguate,
                                         help, helper, idm, info, long, prefs,
                                         short, showHelpOnEmpty,
                                         showHelpOnError, strOption, switch)
import           Reports.Build          (ContentSpec (..), ReportBuild (..),
                                         SQLVisualisation (..))
import qualified Reports.Core           as Report
import           Reports.Core           (Content (..))
import           Reports.SqlQuery       (addBuiltinSqlQueryRule, sqlQuery)
import           Reports.Visualisation  (addBuiltinVisualisationRule,
                                         needVisualisation, visualisation)
import           System.FilePath        (dropExtension, takeDirectory,
                                         takeFileName)
import qualified System.FSNotify        as FSNotify
import           WithHash               (withHash)

data CliOpts =
  CliOpts
    { inFile :: FilePath
    , watch  :: Bool
    }

optionsParser :: Parser CliOpts
optionsParser =
  CliOpts
    <$> strOption (short 'f' <> long "file" <> help "Input file with tasks")
    <*> switch (short 'w' <> long "watch" <> help "Watch the input file for changes and regenerate the output every time the file is changed")

runMain :: IO ()
runMain = do
  CliOpts{inFile, watch} <- customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> optionsParser) idm)
  let build = buildReport' inFile
  if watch
    then FSNotify.withManager $ \mgr -> do
      let fileModified (FSNotify.Modified fp' _ _) = takeFileName fp' == takeFileName inFile
          fileModified _                           = False
      build -- build once
      void $ FSNotify.watchDir
              mgr
              (takeDirectory inFile)
              fileModified
              (const $ threadDelay 500 >> build)
      forever $ threadDelay 1000000
    else
      build

buildReport' ::
  FilePath -- ^ Input file (.yaml or .dhall)
  -> IO ()
buildReport' fp = Shake.shake Shake.shakeOptions $ do
  addBuiltinSqlQueryRule
  addBuiltinVisualisationRule
  Shake.want [dropExtension fp <> ".html"]
  dhallToYamlRule
  yamlToReportRule

dhallToYamlRule :: Shake.Rules ()
dhallToYamlRule =
  "*.yaml" %> \out -> do
    let inFile = dropExtension out <> ".dhall"
    Shake.need [inFile]
    liftIO $ do
      Text.IO.readFile inFile >>=
        Dhall.dhallToYaml Dhall.defaultOptions (Just $ takeDirectory inFile) >>=
          BS.writeFile out

yamlToReportRule :: Shake.Rules ()
yamlToReportRule =
  "*.html" %> \out -> do
    let inFile = dropExtension out <> ".yaml"
    Shake.need [inFile]
    (Yaml.decode1 @ReportBuild <$> liftIO (BSL.readFile inFile)) >>= \case
      Right reportBuild -> buildReport out reportBuild
      Left err          -> do
        fail ("Reading report failed with: " <> show err)

buildReport :: FilePath -> ReportBuild -> Shake.Action ()
buildReport out ReportBuild{rpDatabase, rpReport} = do
  let hashedDb = withHash rpDatabase
  rep' <- flip traverse rpReport $ \case
    VisContentSpec SQLVisualisation{cfgQuery, cfgVegaliteVisSpec, cfgTitle} -> do
      let queryVal = sqlQuery cfgQuery hashedDb
      vis <- needVisualisation (visualisation (Text.pack cfgTitle) cfgVegaliteVisSpec queryVal)
      pure (VegaLiteContent vis)
    TextContentSpec text -> pure (TextContent text)
  liftIO (Report.renderToFile out rep')

-- TODO:
-- use postgres-simple package for connections
-- share a connection
