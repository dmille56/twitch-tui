module Client where

import Data.Aeson
import qualified Network.HTTP.Req as R
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception

import Records

testClientId :: ByteString
testClientId = "phiay4sq36lfv9zu7cbqwz2ndnesfd8"
apiBaseUrl = "api.twitch.tv"
apiVersion = "helix"
  
instance R.MonadHttp IO where
  handleHttpException = throwIO

testCommunityIds, testGameIds, testList :: [Text]
testCommunityIds = []
testGameIds = ["21779"] -- league of legends
testList = ["ninja"]

getStreamsDataWithExceptions :: ByteString -> [Text] -> [Text] -> IO (Maybe TwitchStreamsData)
getStreamsDataWithExceptions clientId gameIds usernames = do
  let options = (R.header "Client-ID" clientId) <> usersQuery <> gameIdsQuery
      usersQuery = mconcat $ map ("user_login" R.=:) usernames
      gameIdsQuery = mconcat $ map ("game_id" R.=:) gameIds
  request <- R.req R.GET (R.https apiBaseUrl R./: apiVersion R./: "streams") R.NoReqBody R.bsResponse options
  let maybe = decodeStrict' (R.responseBody request) :: Maybe TwitchStreamsData
  return maybe

getStreamsData :: ByteString -> [Text] -> [Text] -> IO (Either Text TwitchStreamsData)
getStreamsData clientId gameIds usernames = do
  query <- try (getStreamsDataWithExceptions clientId gameIds usernames) :: IO (Either SomeException (Maybe TwitchStreamsData))
  case query of
    Left error -> return . Left . T.pack $ show error
    Right (Just sData) -> return $ Right sData
    Right (Nothing) -> return $ Left "Error couldn't deserialize json into TwitchStreamsData"

getTopGamesDataWithExceptions :: ByteString -> IO (Maybe TwitchTopGamesData)
getTopGamesDataWithExceptions clientId = do
  let options = (R.header "Client-ID" clientId)
  request <- R.req R.GET (R.https apiBaseUrl R./: apiVersion R./: "games" R./: "top") R.NoReqBody R.bsResponse options
  let maybe = decodeStrict' (R.responseBody request) :: Maybe TwitchTopGamesData
  return maybe

getTopGamesData :: ByteString -> IO (Either Text TwitchTopGamesData)
getTopGamesData clientId = do
  query <- try (getTopGamesDataWithExceptions clientId) :: IO (Either SomeException (Maybe TwitchTopGamesData))
  case query of
    Left error -> return . Left . T.pack $ show error
    Right (Just sData) -> return $ Right sData
    Right (Nothing) -> return $ Left "Error couldn't deserialize json into TwitchTopGamesData"
