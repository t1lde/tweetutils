module ApiOptions where

import Options
import Web.Twitter.Conduit

mkOauth :: ApiKey 'Consumer 'Private -> ApiKey 'Consumer 'Public -> OAuth
mkOauth (ApiKey priv) (ApiKey pub) = twitterOAuth {oauthConsumerKey = pub, oauthConsumerSecret = priv}

mkAccess :: ApiKey 'Access 'Private -> ApiKey 'Access 'Public -> Credential
mkAccess (ApiKey priv) (ApiKey pub) = Credential [("oauth_token", pub), ("oauth_token_secret", priv)]

mkInfo :: CliOptions a -> TWInfo -> TWInfo
mkInfo (CliOptions priv pub CliAuthMode) = setCredential (mkOauth priv pub) (Credential [])
mkInfo (CliOptions priv pub (CliDeleteMode (CliDeleteOptions a_priv a_pub _ _ _))) =
  setCredential (mkOauth priv pub) (mkAccess a_priv a_pub)
