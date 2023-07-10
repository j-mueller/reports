{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-| Build rules for visualisations
-}
module Reports.Visualisation(
  VisSpec(..),
  Visualisation(..),
  visualisation,
  needVisualisation,
  addBuiltinVisualisationRule,
  VegaLiteGraph(..)
  ) where

import           Codec.Serialise        (Serialise (..))
import           Control.DeepSeq        (NFData)
import           Control.Lens           ((&))
import           Data.Aeson             (FromJSON, Object, ToJSON)
import qualified Data.Aeson             as JSON
import           Data.Aeson.KeyMap      (KeyMap, insert)
import           Data.Binary            (Binary (..))
import qualified Data.ByteString.Lazy   as BSL
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Development.Shake      (Action, RuleResult, Rules)
import           Development.Shake.Rule (BuiltinRun, RunChanged (..),
                                         RunMode (..), RunResult (..),
                                         addBuiltinRule, apply1, noLint)
import           GHC.Generics           (Generic)
import           NiceHash               (HasNiceHash (..), NiceHash (..),
                                         NiceHashable (..))
import           Reports.SqlQuery       (SqlQuery, needSqlQuery)
import           WithHash               (WithHash (..), withHash, withHashKey)

newtype VisSpec = VisSpec { getVisSpec :: KeyMap JSON.Value }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON, NFData)
  deriving HasNiceHash via (NiceHashable "vis_spec" VisSpec)

instance Serialise VisSpec where
  encode = encode . JSON.encode
  decode = JSON.decode <$> decode >>= \case
    Just x  -> pure x
    Nothing -> fail "Failed to decode JSON from bytestring"

instance Binary VisSpec where
  put = put . JSON.encode
  get = JSON.decode <$> get >>= \case
    Just x  -> pure x
    Nothing -> fail "Failed to decode JSON from bytestring"

data Visualisation =
  Visualisation
    { visSpec  :: WithHash VisSpec
    , visTitle :: WithHash Text
    , visData  :: WithHash SqlQuery
    } deriving stock (Generic)
      deriving anyclass (Serialise, Binary, NFData)

instance HasNiceHash Visualisation where
  type Name Visualisation = "visualisation"

instance Show Visualisation where
  show Visualisation{visTitle, visSpec, visData} =
    "Visualisation{visSpec="
      <> show (whHash visSpec)
      <> ",visTitle="
      <> Text.unpack (whValue visTitle)
      <> ",visData="
      <> show (whHash visData)

{-| Build a 'Visualisation' with the given title and vega lite spec
-}
visualisation :: Text -> VisSpec -> WithHash SqlQuery -> WithHash Visualisation
visualisation (withHash -> visTitle) (withHash -> visSpec) visData =
  withHash Visualisation{visSpec, visData, visTitle}

data VegaLiteGraph = VegaLiteGraph{ vlGraph :: Object, vlTitle :: Text }
  deriving stock (Generic, Show)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Serialise VegaLiteGraph where
  encode = encode . JSON.encode
  decode = JSON.decode <$> decode >>= \case
    Just x  -> pure x
    Nothing -> fail "Failed to decode JSON from bytestring"

instance Binary VegaLiteGraph where
  put = put . JSON.encode
  get = JSON.decode <$> get >>= \case
    Just x  -> pure x
    Nothing -> fail "Failed to decode JSON from bytestring"

needVisualisation :: WithHash Visualisation -> Action VegaLiteGraph
needVisualisation = apply1

type instance RuleResult (WithHash Visualisation) = VegaLiteGraph

addBuiltinVisualisationRule :: Rules ()
addBuiltinVisualisationRule = addBuiltinRule noLint (const . Just . withHashKey) run
  where
    run :: BuiltinRun (WithHash Visualisation) VegaLiteGraph
    run key old mode = do
      let WithHash{whValue = Visualisation{visSpec, visTitle, visData}} = key
      case old of
        Just oldValue | mode == RunDependenciesSame -> do
          vl <- decodeResponse (BSL.fromStrict oldValue)
          pure $ RunResult ChangedNothing oldValue vl
        _ -> do
          rows <- needSqlQuery visData
          let VisSpec json = whValue visSpec
              vlTitle  = whValue visTitle
              vlGraph = json
                          & insert "data" (JSON.object ["values" JSON..= rows])
                          & insert "title" (JSON.toJSON vlTitle)
              result = VegaLiteGraph{vlGraph, vlTitle}
              answer = BSL.toStrict (JSON.encode result)
              changed = if Just answer == old then ChangedRecomputeSame else ChangedRecomputeDiff
          pure $ RunResult changed answer result

decodeResponse :: forall a m. (FromJSON a, MonadFail m) => BSL.ByteString -> m a
decodeResponse = either fail pure . JSON.eitherDecode
