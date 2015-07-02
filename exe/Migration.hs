import Control.Monad.Logger
import Database.Persist
import Database.Persist.Postgresql

import DnoList.Types
import DnoList.Wrapper
import DnoList.Database

main :: IO ()
main =
  wrapMain $ \settings ->
  runStderrLoggingT $
  withPostgresqlConn (database settings) $ \conn ->
  flip runSqlConn conn $ do
    runMigration migrateAll
