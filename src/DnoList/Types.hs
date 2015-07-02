module DnoList.Types where

import Control.Monad
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket (PortNumber)
import Database.Persist.Postgresql (ConnectionString)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Time.Clock

type Email = Text
type Domain = Text
type Password = ByteString

type SMTPServer = Domain

instance FromJSON ByteString where
  parseJSON = liftM encodeUtf8 . parseJSON

instance ToJSON ByteString where
  toJSON = toJSON . decodeUtf8

instance FromJSON PortNumber where
  parseJSON = liftM (fromIntegral :: Int -> PortNumber) . parseJSON

instance ToJSON PortNumber where
  toJSON = toJSON . (fromIntegral :: PortNumber -> Int)

instance FromJSON NominalDiffTime where
  parseJSON = liftM fromRational . parseJSON

instance ToJSON NominalDiffTime where
  toJSON = toJSON . toRational

data Settings = Settings { domain :: Domain
                         , smtpAddress :: Domain
                         , smtpPort :: PortNumber
                         , database :: ConnectionString
                         , expiration :: NominalDiffTime
                         }
              deriving (Show, Eq, Generic)

instance FromJSON Settings
instance ToJSON Settings
