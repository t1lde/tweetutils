module TweetUtils.Commands.GetAuth (apiAuth) where

import Web.Authenticate.OAuth qualified as OA
import Web.Twitter.Conduit

import Data.ByteString.Char8 qualified as C8

import Control.Monad.Except
import Control.Monad.Trans.Maybe

import System.IO (hFlush, stdout)

apiAuth ∷ OAuth → Manager → IO ()
apiAuth oauth mgr = do
  temp_creds <- OA.getTemporaryCredential oauth mgr
  putStr $ "Go to " <> OA.authorizeUrl oauth temp_creds <> " to recieve auth PIN\nEnter PIN:"
  hFlush stdout
  pin <- C8.getLine
  new_creds <- OA.getAccessToken oauth (OA.insert "oauth_verifier" pin temp_creds) mgr
  runExceptT $ reportAccessKeys new_creds `catchError` reportErr
  return ()
  where
    reportAccessKeys ∷ Credential → ExceptT String IO ()
    reportAccessKeys (Credential creds) = do
      access_pub <- maybeToExceptT "Error obtaining access keys from acquired oauth creds." $ MaybeT $ pure $ lookup "oauth_token" creds
      access_priv <- maybeToExceptT "Error obtaining access keys from acquired oauth creds." $ MaybeT $ pure $ lookup "oauth_token_secret" creds
      lift $ C8.putStrLn $ "Keep these somewhere safe!\nAccess Token:\n\tPublic: "<>access_pub<>"\n\tPrivate: "<>access_priv
      lift $ hFlush stdout

    reportErr ∷ String → ExceptT String IO ()
    reportErr err = lift $ putStrLn err
