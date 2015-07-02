module DnoList.OutgoingQueue where

import Control.Monad
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Logger
import Network.HaskellNet.SMTP (doSMTPPort, SMTPConnection)
import qualified Network.HaskellNet.SMTP as SMTP
import Control.Monad.Loops
import Control.Concurrent
import Control.Monad.IO.Class
import Database.Persist.Postgresql (withPostgresqlPool, tableName, fieldName)
import Database.Esqueleto

import DnoList.Types
import DnoList.Database

clearQueue :: ConnectionPool -> SMTPConnection -> IO Bool
clearQueue pool smtp = flip runSqlPool pool $ do
  -- can be replaced by "SELECT SKIP LOCKED" in psql 9.5; would need raw SQL anyway
  let query = "select pg_advisory_xact_lock('%TABLE%'::regclass::integer, \"%COL%\"), ?? from "
           <> "(select * from \"%TABLE%\" where \"%COL%\" not in "
           <> " (select objid from pg_locks where objsubid = 2 "
           <> "  and locktype = 'advisory' and classid = '%TABLE%'::regclass::integer "
           <> " ) "
           <> " order by \"%COL%\" limit 1 "
           <> ")"
  res <- rawSql ( T.replace "%TABLE%" (tableName (undefined :: Order))
               $ T.replace "%COL%" (fieldName OrderId)
               $ query
               ) []
  case res of
   [] -> return False
   [Entity key order] -> do
     Just sender <- get $ orderFrom order
     Just list <- get $ orderList order
     recvs <- select $ from $ \(subscr `InnerJoin` user) -> do
       on $ subscr^.SubscriptionUser ==. user^.UserId
       where_ $ subscr^.SubscriptionList ==. val (orderList order)
       return $ user
     liftIO $ SMTP.sendMail
       (T.unpack $ userEmail sender)
       (map (T.unpack . userEmail . entityVal) recvs)
       (orderBody order)
       smtp
     delete $ from $ \ord -> where_ $ ord^.OrderId ==. val key
     return True

runOutgoingQueue :: Settings -> IO ()
runOutgoingQueue settings =
  runStderrLoggingT $
  withPostgresqlPool (database settings) 4 $ \pool -> liftIO $
  doSMTPPort (T.unpack $ smtpAddress settings) (smtpPort settings) $ \smtp ->
  forever $ do
    r <- whileM (clearQueue pool smtp) (return ())
    when (null r) $ void $ SMTP.sendCommand smtp SMTP.NOOP
    threadDelay 100000
