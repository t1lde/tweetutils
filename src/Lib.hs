module Lib (runWithOptions) where

import Web.Twitter.Conduit
import Data.Time.Clock

import Options
import ApiOptions
import Delete
import Auth

runWithOptions :: CliOptions UTCTime -> IO ()
runWithOptions opts@(CliOptions _ _ mode) = do
  apiManager <- newManager tlsManagerSettings
  apiMode (mkInfo opts proxySettings) apiManager mode
  return ()
  where
    apiMode :: TWInfo -> Manager -> CliModeOptions UTCTime -> IO ()
    apiMode (TWInfo (TWToken oauth _) _) mgr (CliAuthMode) = apiAuth oauth mgr
    apiMode info mgr (CliDeleteMode deleteOpts) = apiDelete info mgr deleteOpts

proxySettings :: TWInfo
proxySettings = def
