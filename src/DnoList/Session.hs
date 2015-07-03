module DnoList.Session where

import Control.Monad
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.API
import Crypto.PasswordStore
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
             :<|> "token" :> ReqBody '[JSON] TokenId :> Put '[JSON] (UserId, UTCTime)
             :<|> "token" :> ReqBody '[JSON] TokenId :> Delete '[JSON] ()

sessionServer :: NominalDiffTime -> ConnectionPool -> Server SessionAPI
sessionServer expire pool =
  userRegister
  :<|> userLogin
  :<|> userLogout
  :<|> tokenUpdate
  :<|> tokenRemove
  
  where userRegister user = run $ do
          pwd <- liftIO $ makePassword (userPassword user) 17
          let user' = user { userPassword = pwd }
          insert user'
        userLogin uid pwd = run $ do
          u <- maybe (lift $ left err404) return =<< get uid
          unless (verifyPassword pwd (userPassword u)) $ lift $ left err403
          etime <- liftIO $ addUTCTime expire <$> getCurrentTime
          tid <- insert $ Token uid etime
          return (tid, etime)
        userLogout uid = run $ delete $ from $ \token -> where_ $ token^.TokenUser ==. val uid
        tokenUpdate tid = run $ do
          r <- maybe (lift $ left err404) return =<< get tid
          etime <- liftIO $ getCurrentTime
          lift $ when (tokenExpiration r >= etime) $ left err403
          let ntime = addUTCTime expire etime
          update $ \token -> do
            set token [ TokenExpiration =. val ntime ]
            where_ $ token^.TokenId ==. val tid
          return (tokenUser r, ntime)
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
