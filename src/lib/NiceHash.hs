{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
module NiceHash(
    HasNiceHash(..),
    NiceHashable(..),
    NiceHash(..),
    TestData(..),
    mkNiceHash
    ) where
import           Codec.Serialise                  (Serialise (..), serialise)
import           Control.DeepSeq                  (NFData)
import qualified Crypto.Sodium.Hash               as Hash
import           Data.Aeson                       (FromJSON (..), ToJSON (..))
import           Data.Binary                      (Binary (..))
import           Data.ByteArray.Sized             (unSizedByteArray)
import qualified Data.ByteString.Lazy             as BSL
import           Data.Hashable                    (Hashable (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Database.Beam                    (FromBackendRow,
                                                   HasSqlEqualityCheck,
                                                   HasSqlQuantifiedEqualityCheck)
import           Database.Beam.Backend.SQL.SQL92  (HasSqlValueSyntax)
import           Database.Beam.Postgres           (Postgres)
import           Database.Beam.Postgres.Syntax    (PgValueSyntax)
import           Database.Beam.Sqlite.Connection  (Sqlite)
import           Database.Beam.Sqlite.Syntax      (SqliteValueSyntax)
import           Database.SQLite.Simple.FromField (FromField (..))
import           Database.SQLite.Simple.ToField   (ToField (..))
import           GHC.Generics                     (Generic)
import           GHC.TypeLits                     (KnownSymbol, Symbol,
                                                   symbolVal)
import           Prettyprinter                    (Pretty (..))
import qualified Text.Hex

newtype NiceHash a = NiceHash { unNiceHash :: Text }
  deriving stock (Eq, Ord)
  deriving newtype
    ( ToField,
      Pretty,
      FromField,
      HasSqlValueSyntax SqliteValueSyntax,
      FromBackendRow Sqlite,
      HasSqlEqualityCheck Sqlite,
      HasSqlQuantifiedEqualityCheck Sqlite,
      HasSqlValueSyntax PgValueSyntax,
      FromBackendRow Postgres,
      HasSqlEqualityCheck Postgres,
      HasSqlQuantifiedEqualityCheck Postgres,
      Serialise,
      ToJSON,
      FromJSON,
      Hashable,
      Binary,
      NFData
    )

instance Show (NiceHash a) where
  show = Text.unpack . unNiceHash

mkNiceHash :: forall (s :: Symbol) a. KnownSymbol s => Serialise a => a -> NiceHash a
mkNiceHash =
  let prefix = Text.pack (symbolVal $ Proxy @s) <> "_" in
  NiceHash . (prefix <>) . Text.Hex.encodeHex . unSizedByteArray . Hash.blake2b @20 . BSL.toStrict . serialise

newtype NiceHashable (name :: Symbol) a = NiceHashable{ unNiceHashable :: a }
  deriving newtype Serialise

instance (KnownSymbol name, Serialise a) => HasNiceHash (NiceHashable name a) where
  type Name (NiceHashable name a) = name

class (Serialise a, KnownSymbol (Name a)) => HasNiceHash a where
    type Name a :: Symbol

    niceHash :: a -> NiceHash a
    niceHash = mkNiceHash @(Name a)

data TestData = TestData{ a :: String, b :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)
  deriving HasNiceHash via (NiceHashable "test" TestData)

deriving via (NiceHashable "text" Text) instance HasNiceHash Text
