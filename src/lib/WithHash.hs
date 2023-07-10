{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-| Hashed values for caching builds
-}
module WithHash(
    -- * Hashed values
    WithHash(..),
    withHash,
    withHashKey,
    mapWithHash,
  ) where

import           Codec.Serialise      (Serialise (..), serialise)
import           Control.DeepSeq      (NFData)
import           Data.Aeson           (FromJSON (..), ToJSON (..))
import           Data.Binary          (Binary (..))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Hashable        (Hashable (..))
import           GHC.Generics         (Generic)
import           NiceHash             (HasNiceHash (..), NiceHash)

{-| A value with its hash
-}
data WithHash a =
  WithHash
    { whHash  :: NiceHash a
    , whValue :: a
    } deriving stock (Generic)
      deriving anyclass (Binary, NFData)

instance Eq (WithHash a) where
  l == r = whHash l == whHash r

instance ToJSON a => ToJSON (WithHash a) where
  toJSON = toJSON . whValue

instance (HasNiceHash a) => Serialise (WithHash a) where
  encode = encode . whValue
  decode = withHash <$> decode

instance (FromJSON a, HasNiceHash a) => FromJSON (WithHash a) where
  parseJSON = fmap withHash . parseJSON

instance Show a => Show (WithHash a) where
  show (WithHash _ v) = "hashed(" <> show v <> ")"

instance Hashable (WithHash a) where
  hashWithSalt i = hashWithSalt i . whHash

mapWithHash :: HasNiceHash b => (a -> b) -> WithHash a -> WithHash b
mapWithHash f (WithHash _ old) = withHash (f old)

withHash :: HasNiceHash a => a -> WithHash a
withHash a' = WithHash (niceHash a') a'

withHashKey :: WithHash a -> ByteString
withHashKey = BSL.toStrict . serialise . whHash
