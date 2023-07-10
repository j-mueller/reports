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
{-| Build rules for SQL queries
-}
module Reports.SqlQuery(
  PostgresConfig(..),
  SqlQuery(..),
  SqlQueryText,
  sqlQuery,
  addBuiltinSqlQueryRule,
  needSqlQuery
  ) where

import           Codec.Serialise        (Serialise)
import           Control.DeepSeq        (NFData)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON, ToJSON, Value, withObject,
                                         (.:))
import qualified Data.Aeson             as JSON
import           Data.Binary            (Binary (..))
import qualified Data.ByteString.Lazy   as BSL
import           Data.Maybe             (listToMaybe)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Development.Shake      (Action, RuleResult, Rules)
import qualified Development.Shake      as Shake
import           Development.Shake.Rule (BuiltinRun, RunChanged (..),
                                         RunMode (..), RunResult (..),
                                         addBuiltinRule, apply1, noLint)
import           GHC.Generics           (Generic)
import           NiceHash               (HasNiceHash (..), NiceHash (..),
                                         NiceHashable (..))
import           WithHash               (WithHash (..), withHash, withHashKey)

data PostgresConfig =
    PostgresConfig
        { pcPostgresHost      :: String
        , pcPostgresPort      :: Int
        , pcPostgresDB        :: String
        , pcPostgressUsername :: String
        , pcPostgresPassword  :: String
        }
        deriving stock (Eq, Ord, Show, Generic)
        deriving anyclass (Serialise, Binary, NFData)

instance HasNiceHash PostgresConfig where
    type Name PostgresConfig = "postgres_config"

instance FromJSON PostgresConfig where
    parseJSON = withObject "PostgresConfig" $ \object ->
        PostgresConfig
            <$> object .: "host"
            <*> object .: "port"
            <*> object .: "database"
            <*> object .: "username"
            <*> object .: "password"

-- Shake.apply
newtype SqlQueryText = SqlQueryText{ getSqlQueryText :: Text }
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, FromJSON, Serialise, Binary, NFData)
  deriving HasNiceHash via (NiceHashable "sql_query_text" SqlQueryText)

data SqlQuery =
  SqlQuery
    { sqlQueryDefinition :: WithHash SqlQueryText
    , sqlQueryBackend    :: WithHash PostgresConfig
    } deriving stock (Generic)
      deriving anyclass (Serialise, Binary, NFData)

sqlQuery :: SqlQueryText -> WithHash PostgresConfig -> WithHash SqlQuery
sqlQuery (withHash -> sqlQueryDefinition) sqlQueryBackend =
  withHash SqlQuery{sqlQueryDefinition, sqlQueryBackend}

instance Show SqlQuery where
  show =
    maybe "(empty)" (take 20 . Text.unpack)
    . listToMaybe
    . filter (not . Text.null)
    . Text.lines
    . getSqlQueryText
    . whValue
    . sqlQueryDefinition

-- 2. shake rules
instance HasNiceHash SqlQuery where
  type Name SqlQuery = "sql_query"

type instance RuleResult (WithHash SqlQuery) = [Value]

needSqlQuery :: WithHash SqlQuery -> Action [Value]
needSqlQuery = apply1

addBuiltinSqlQueryRule :: Rules ()
addBuiltinSqlQueryRule = addBuiltinRule noLint (const . Just . withHashKey) run
  where
    run :: BuiltinRun (WithHash SqlQuery) [Value]
    run key old mode = do
      let WithHash{whValue = SqlQuery{sqlQueryDefinition, sqlQueryBackend}} = key
      case old of
        Just oldValue | mode == RunDependenciesSame -> do
          vl <- decodeResponse (BSL.fromStrict oldValue)
          pure $ RunResult ChangedNothing oldValue vl
        _ -> Shake.withTempFile $ \fp -> do
          let PostgresConfig{pcPostgresHost, pcPostgresPort, pcPostgresDB, pcPostgresPassword, pcPostgressUsername} = whValue sqlQueryBackend
              qry = Text.unpack (getSqlQueryText $ whValue sqlQueryDefinition)
              command = "\\copy (" <> qry <> ") TO '" <> fp <> "'"
          () <-
            Shake.cmd
              (Shake.AddEnv "PGPASSWORD" pcPostgresPassword)
              (s "psql")
              (s "--host") [pcPostgresHost]
              (s "--port") [show pcPostgresPort]
              (s "--dbname") [pcPostgresDB]
              (s "--username") [pcPostgressUsername]
              (s "--command") [command]
          answer <- liftIO (BSL.readFile fp)
          value <- decodeResponse answer
          let answer' = BSL.toStrict answer
              changed = if Just answer' == old then ChangedRecomputeSame else ChangedRecomputeDiff
          pure $ RunResult changed answer' value

decodeResponse :: MonadFail m => BSL.ByteString -> m [Value]
decodeResponse answer =
  case (traverse JSON.eitherDecode . filter (not . BSL.null) $ BSL.split (fromIntegral $ fromEnum '\n') answer) of
    Left err -> fail err
    Right x  -> pure x

s :: String -> String
s = id
