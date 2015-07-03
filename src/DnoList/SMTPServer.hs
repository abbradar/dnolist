module DnoList.SMTPServer where

import Data.Monoid
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Monad.Loops
import Control.Monad.Trans.Either
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Concurrent.MVar
import qualified Data.Attoparsec.ByteString.Lazy as AP
import Data.Conduit.Network
import Database.Persist.Postgresql (withPostgresqlPool)
import Database.Esqueleto
import qualified Data.Attoparsec.Text.Lazy as TP

import qualified Network.Email.Parser as P
import Network.Email.Types
import Network.Email.Read (decodeText)
import qualified Network.Email.Render as R
import Network.SMTP.Server
import Network.SMTP.Protocol hiding (Domain)
import Network.SMTP.Address

import DnoList.Types
import DnoList.Database
import DnoList.SMTPCommands
import DnoList.Sysop

smtpServer :: ConnectionPool -> ServerSettings -> Domain -> IO ()
smtpServer pool server smtpDomain = do
  let checkSender ci (Just adr) | Left domain <- domainPart adr = flip runSqlPool pool $ do
        r <- select $ from $ \user -> do
          where_ $ user^.UserEmail ==. val (localPart adr <> "@" <> domain)
          return user
        if null r
          then return $ Just $ smtpReply CArgSyntax "User not registered"
          else return Nothing
      checkSender ci _ = return $ Just $ smtpReply CArgSyntax "Invalid sender address"
      checkRcpt ci Postmaster =
        if null $ clientTo ci
        then return Nothing
        else return $ Just $ smtpReply CArgSyntax "Postmaster must be sole receiver"
      checkRcpt ci (Rcpt adr)
        | Left domain <- domainPart adr, domain == smtpDomain = flip runSqlPool pool $ do
            r <- getListId $ localPart adr
            case r of
             Nothing -> return $ Just $ smtpReply CArgSyntax "Mailing list not registered"
             Just _ -> return Nothing
      checkRcpt ci _ = return $ Just $ smtpReply CArgSyntax "Invalid recipient"
      checkData ci bs = do
        case AP.parse (P.mail <* AP.endOfInput) bs of
         AP.Fail _ stk e -> return $ Just $ smtpReply CArgSyntax "Cannot parse email"
         AP.Done _ r -> flip runSqlPool pool $ do
           let Just adr = clientFrom ci
           [Entity uid _] <- select $ from $ \user -> do
             where_ $ user^.UserEmail ==. val (toText adr)
             return user
           mapM_ (processMail uid r) $ clientTo ci
           return Nothing
  let pars = smtpParameters { extraExts = [("MIMEUTF8", Nothing)]
                            , smtpDomain, checkSender, checkRcpt, checkData
                            }
  runSMTPServer pars server id

getListId :: Text -> SqlPersistT IO (Maybe ListId)
getListId name = do
  r <- select $ from $ \list -> do
    where_ $ list^.ListName ==. val name
    return list
  return $ listToMaybe $ map entityKey r

processMail :: UserId -> Mail -> RcptAddress -> SqlPersistT IO (Maybe SMTPReply)
processMail uid dat Postmaster = do
  txt <- decodeText dat
  case TP.parse (sysop <* TP.endOfInput) txt of
   TP.Fail _ _ _ -> return $ Just $ smtpReply CArgSyntax "Invalid sysop command"
   TP.Done _ r -> case r of
     CreateList name title -> do
       checkCall $ createList $ List { listName = name
                                     , listTitle = title
                                     , listOwner = uid
                                     }
     DeleteList name -> do
       Just lid <- getListId name
       checkCall $ deleteList lid uid
     Subscribe name -> do
       Just lid <- getListId name
       checkCall $ subscribeList lid uid
     Unsubscribe name -> do
       Just lid <- getListId name
       checkCall $ unsubscribeList lid uid

  where checkCall c = do
          r <- liftIO $ runEitherT c
          case r of
           Left _ -> return $ Just $ smtpReply CArgSyntax "Sysop error"
           Right _ -> return Nothing

processMail uid dat (Rcpt adr) = do
  Just lid <- getListId $ localPart adr
  insert_ $ Order lid uid (BL.toStrict $ R.mail dat)
  return Nothing

runSmtpServer :: Settings -> IO ()
runSmtpServer settings =
  runStderrLoggingT $
  withPostgresqlPool (database settings) 4 $ \pool -> liftIO $ do
    let server = serverSettings 8025 "*"
    smtpServer pool server (domain settings)
