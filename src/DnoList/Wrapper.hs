module DnoList.Wrapper where

import System.Environment
import Data.Yaml

import DnoList.Types

wrapMain :: (Settings -> IO ()) -> IO ()
wrapMain run = do
  Just file <- lookupEnv "DNOLIST_SETTINGS"
  Just settings <- decodeFile file
  run settings
