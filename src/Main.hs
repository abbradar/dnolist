import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
import Data.Conduit.Network

import Network.SMTP.Server

main :: IO ()
main = do
  pars' <- smtpParameters
  let pars = pars' { smtpDomain = "penis.io"
                   , extraExts = [("MIMEUTF8", Nothing)]
                   }
  let server = serverSettings 1488 "*"
  _ <- forkIO $ forever $ do
    r <- atomically $ readTQueue $ mailQueue pars
    print r
  runSMTPServer pars server
