module DnoList.Session where

import Control.Monad
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Database.Persist.Postgresql (withPostgresqlPool)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class
import Database.Esqueleto
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Concurrent
import Control.Monad.Trans.Control
import Data.Time.Clock

import DnoList.Types
import DnoList.Database

type SessionAPI = "users" :> ReqBody '[JSON] User :> Post '[JSON] UserId
             :<|> "users" :> Capture "userid" UserId :> "login" :> ReqBody '[JSON] Password :> Post '[JSON] (TokenId, UTCTime)
             :<|> "users" :> Capture "userid" UserId :> "logout" :> Put '[JSON] ()
             :<|> "token" :> ReqBody '[JSON] TokenId :> Put '[JSON] UTCTime
             :<|> "token" :> ReqBody '[JSON] TokenId :> Delete '[JSON] ()

sessionServer :: NominalDiffTime -> ConnectionPool -> Server SessionAPI
sessionServer expire pool =
  userRegister
  :<|> userLogin
  :<|> userLogout
  :<|> tokenUpdate
  :<|> tokenRemove
  
  where userRegister user = run $ insert user
        userLogin uid pwd = run $ do
          us <- select $ from $ \user -> do
            where_ $ user^.UserId ==. val uid &&. user^.UserPassword ==. val pwd
            return user
          when (null us) $ lift $ left err403
          etime <- liftIO $ addUTCTime expire <$> getCurrentTime
          tid <- insert $ Token uid etime
          return (tid, etime)
        userLogout uid = run $ delete $ from $ \token -> where_ $ token^.TokenUser ==. val uid
        tokenUpdate tid = run $ do
          etime <- liftIO $ addUTCTime expire <$> getCurrentTime
          n <- updateCount $ \token -> do
            set token [ TokenExpiration =. val etime ]
            where_ $ token^.TokenExpiration <. val etime &&. token^.TokenId ==. val tid
          when (n == 0) $ lift $ left err403
          return etime
        tokenRemove tid = run $ delete $ from $ \token -> where_ $ token^.TokenId ==. val tid

        run :: MonadBaseControl IO m => SqlPersistT m a -> m a
        run = flip runSqlPool pool

cleanWorker :: ConnectionPool -> IO ()
cleanWorker pool = forever $ do
  ctime <- getCurrentTime
  flip runSqlPool pool $ delete $ from $ \token -> where_ $ token^.TokenExpiration >=. val ctime
  threadDelay 100000

runSession :: Settings -> IO ()
runSession settings =
  runStderrLoggingT $
  withPostgresqlPool (database settings) 4 $ \pool -> liftIO $ do
    void $ forkIO $ cleanWorker pool
    run 8082 $ serve (Proxy :: Proxy SessionAPI) $ sessionServer (expiration settings) pool
