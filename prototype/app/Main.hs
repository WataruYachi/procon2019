{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as B8
import GHC.Generics
import Data.Default.Class (def)

import Network.HTTP.Req

type Token = B8.ByteString

data Match = Match
             { id :: Int
             , intervalMillis :: Int
             , matchTo :: T.Text
             , myTeamID :: Int
             , turnMillis :: Int
             , turns :: Int
             } deriving Show

instance FromJSON Match where
    parseJSON (Object v) = Match <$> v .: "id"
                                 <*> v .: "intervalMillis"
                                 <*> v .: "matchTo"
                                 <*> v .: "teamID"
                                 <*> v .: "turnMillis"
                                 <*> v .: "turns"

data MatchInfo = MatchInfo [Match] deriving (Show, Generic)

instance FromJSON MatchInfo
data MatchState = MatchState
             { width :: Int
             , height :: Int
             , points :: [[Int]]
             , startedAtUnixTime :: Int
             , turn :: Int
             , tiled :: [[Int]]
             , teams :: [Team]
             , actions :: [Action]
             } deriving (Show, Generic)

data Team = Team
             { teamID :: Int
             , agents :: [Agent]
             , tilePoint :: Int
             , areaPoint :: Int
             } deriving (Show, Generic)

data Agent = Agent
              { agentID :: Int
              , x :: Int
              , y :: Int
              } deriving (Show, Generic)

data Action = Action
              { actionAgentID :: Int
              , actionType :: T.Text
              , dx :: Int
              , dy :: Int
              , actionTurn :: Int
              , apply :: Int
              } deriving Show

instance FromJSON MatchState

instance FromJSON Team
instance ToJSON Team

instance FromJSON Agent
instance ToJSON Agent

instance FromJSON Action where
    parseJSON (Object v) = Action <$> v .: "agentID"
                                  <*> v .: "type"
                                  <*> v .: "dx"
                                  <*> v .: "dy"
                                  <*> v .: "turn"
                                  <*> v .: "apply"

instance ToJSON Action where
    toJSON (Action actionAgentID actionType dx dy actionTurn apply) =
        object [ "agentID" .= actionAgentID
               , "type" .= actionType
               , "dx" .= dx
               , "dy" .= dy
               , "turn" .= actionTurn
               , "apply" .= apply
               ]

data Status = Status {status :: T.Text} deriving (Show, Generic)
instance FromJSON Status

rawJSON :: B.ByteString
rawJSON = "{\"agentID\":1, \"x\":2, \"y\":3}"

rawJSON' :: B.ByteString
rawJSON' = "{\"teamID\":1, \"agents\":{\"agentID\":1, \"x\":2, \"y\":3}, \"tilePoint\":32, \"areaPoint\":3}"

{-baseUrl :: Url Https
baseUrl = https "localhost:60133"

mkHeader :: Token -> Option Https
mkHeader token = header "Authorization" token
-}
getMatchInfo :: (MonadHttp m) => Token -> m (JsonResponse MatchInfo)
getMatchInfo token = req GET (http "localhost" /: "matches") NoReqBody jsonResponse (header "Authorization" token)

showText :: Int -> Text
showText x = T.pack (show x)

getMatchState :: (MonadHttp m) => Token -> Int -> m (JsonResponse MatchState)
getMatchState token id = req GET (http "localhost" /: "matches" /: (showText id)) NoReqBody jsonResponse (header "Authorization" token)


sendPing :: (MonadHttp m) => Token -> m (JsonResponse Status)
sendPing token = req GET (http "localhost" /: "ping") NoReqBody jsonResponse (header "Authorization" token)

token :: Token
token = "procon30_example_token"
main :: IO ()
main = do
    {-jsonData <- B.readFile "data.json"
    let fieldData = decode jsonData :: Maybe Field
    let filedPoints = actions <$> fieldData
    print filedPoints-}
    ping <- runReq def (sendPing token)
    print $ responseBody ping
    response <- runReq def (getMatchInfo token)
    print $ responseBody response
    state <- runReq def (getMatchState token 1)
    print $ responseBody state
