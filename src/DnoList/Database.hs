module DnoList.Database where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.TH
import Data.Time.Clock
import Servant.Common.Text

import DnoList.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json
  name Text
  email Email
  password Password
  admin Bool
  UniqueUser name
  UniqueEmail email

List json
  name Text
  title Text
  owner UserId
  UniqueList name

Subscription json
  list ListId
  user UserId
  Primary list user

Order json
  list ListId
  from UserId
  body ByteString

Token json
  user UserId
  expiration UTCTime
|]

instance ToBackendKey SqlBackend User => FromText (Key User) where
  fromText = fmap toSqlKey . fromText

instance ToBackendKey SqlBackend User => ToText (Key User) where
  toText = toText . fromSqlKey

instance ToBackendKey SqlBackend List => FromText (Key List) where
  fromText = fmap toSqlKey . fromText

instance ToBackendKey SqlBackend List => ToText (Key List) where
  toText = toText . fromSqlKey

instance ToBackendKey SqlBackend Subscription => FromText (Key Subscription) where
  fromText = fmap toSqlKey . fromText

instance ToBackendKey SqlBackend Subscription => ToText (Key Subscription) where
  toText = toText . fromSqlKey

instance ToBackendKey SqlBackend Order => FromText (Key Order) where
  fromText = fmap toSqlKey . fromText

instance ToBackendKey SqlBackend Order => ToText (Key Order) where
  toText = toText . fromSqlKey

instance ToBackendKey SqlBackend Token => FromText (Key Token) where
  fromText = fmap toSqlKey . fromText

instance ToBackendKey SqlBackend Token => ToText (Key Token) where
  toText = toText . fromSqlKey
