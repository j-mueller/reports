{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Types for managing the build process of a report
-}
module Reports.Build(
    SQLVisualisation(..),
    ReportBuild(..),
    ContentSpec(..))
    where

import           Data.Aeson            (FromJSON (..), Value (Object),
                                        withObject, (.:))
import           Data.Text             (Text)
import           Reports.Core          (Report)
import           Reports.SqlQuery      (PostgresConfig, SqlQueryText)
import           Reports.Visualisation (VisSpec (..))

{-| A SQL query and a vega-lite specification file
-}
data SQLVisualisation = SQLVisualisation
    { cfgQuery           :: SqlQueryText
    , cfgVegaliteVisSpec :: VisSpec
    , cfgTitle           :: String
    }

instance FromJSON SQLVisualisation where
    parseJSON = withObject "SQLVisualisation" $ \object ->
        SQLVisualisation
            <$> object .: "query"
            <*> object .: "visualisation"
            <*> object .: "title"

data ReportBuild =
    ReportBuild
        { rpDatabase :: PostgresConfig
        , rpReport   :: Report () ContentSpec
        }

instance FromJSON ReportBuild where
    parseJSON = withObject "ReportBuild" $ \object ->
        ReportBuild
            <$> object .: "database"
            <*> object .: "report"

data ContentSpec =
    VisContentSpec SQLVisualisation
    | TextContentSpec Text

instance FromJSON ContentSpec where
    parseJSON = withObject "ContentSpec" $ \object -> do
        (tag :: String) <- object .: "type"
        case tag of
            "vega" -> VisContentSpec <$> parseJSON (Object object)
            "text" -> TextContentSpec <$> object .: "text"
            k      -> fail ("Unknown tag: " <> k)
