module DnoList.Sysop where

import Data.Proxy
import Data.Monoid
import Control.Monad
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Client
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Logger
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Esqueleto
import Control.Monad.Trans.Control

import DnoList.Types
import DnoList.Database

type SysopAPI = "lists" :> Get '[JSON] [(ListId, Text)]
           :<|> "lists" :> ReqBody '[JSON] List :> Post '[JSON] ListId
           :<|> "lists" :> Capture "listid" ListId :> Get '[JSON] List
           :<|> "lists" :> Capture "listid" ListId :> "subscribers" :> Get '[JSON] [(UserId, Text)]
           :<|> "lists" :> Capture "listid" ListId :> "delete" :> ReqBody '[JSON] UserId :> Post '[JSON] ()
           :<|> "lists" :> Capture "listid" ListId :> "subscribe" :> Capture "clientid" UserId :> Post '[JSON] ()
           :<|> "lists" :> Capture "listid" ListId :> "unsubscribe" :> Capture "clientid" UserId :> Post '[JSON] ()
           :<|> "users" :> Get '[JSON] [(UserId, Text)]

sysopServer :: ConnectionPool -> Server SysopAPI
sysopServer pool =
  listList
  :<|> listCreate
  :<|> listShow
  :<|> listSubscribers
  :<|> listDelete
  :<|> listSubscribe
  :<|> listUnsubscribe
  :<|> userList

  where listList = run $ do
          r <- select $ from $ \list -> return list
          return $ map (\(Entity id l) -> (id, listName l)) r
        listCreate list = run $ insert list
        listShow lid = run $ maybe (lift $ left err404) return =<< get lid
        listSubscribers lid = run $ do
          r <- select $ from $ \(subscr `InnerJoin` user) -> do
                on $ subscr^.SubscriptionUser ==. user^.UserId
                where_ $ subscr^.SubscriptionList ==. val lid
                return user
          return $ map (\(Entity id u) -> (id, userName u)) r
        listDelete lid uid = run $ do
          list <- maybe (lift $ left err404) return =<< get lid
          user <- maybe (lift $ left err404) return =<< get uid
          lift $ unless (listOwner list == uid || userAdmin user) $ left err403
          delete $ from $ \order -> where_ $ order^.OrderList ==. val lid
          delete $ from $ \list -> where_ $ list^.ListId ==. val lid
        listSubscribe lid uid = run $ insert_ $ Subscription lid uid
        listUnsubscribe lid uid = run $ do
          r <- deleteCount $ from $ \subscr ->
            where_ $ subscr^.SubscriptionUser ==. val uid &&. subscr^.SubscriptionList ==. val lid
          lift $ when (r == 0) $ left err404
        userList = run $ do
          r <- select $ from $ \user -> return user
          return $ map (\(Entity id u) -> (id, userName u)) r

        run :: MonadBaseControl IO m => SqlPersistT m a -> m a
        run = flip runSqlPool pool

(     getAllLists
 :<|> createList
 :<|> getList
 :<|> getListSubscribers
 :<|> deleteList
 :<|> subscribeList 
 :<|> unsubscribeList
 :<|> getAllUsers
 ) = client (Proxy :: Proxy SysopAPI) $ BaseUrl Http "sysop" 80

runSysop :: Settings -> IO ()
runSysop settings =
  runStderrLoggingT $
  withPostgresqlPool (database settings) 4 $ \pool -> liftIO $ do
    run 80 $ serve (Proxy :: Proxy SysopAPI) (sysopServer pool)
