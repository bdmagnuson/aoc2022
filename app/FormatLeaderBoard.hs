{-# LANGUAGE DeriveGeneric #-}

module FormatLeaderBoard
  (
  )
where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Text.Printf

data StarTime = StarTime
  { _get_star_ts :: Int,
    _star_index :: Int
  }
  deriving (Show, Generic)

makeLenses ''StarTime
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''StarTime

data Member = Member
  { _local_score :: Int,
    _global_score :: Int,
    _id :: Int,
    _completion_day_level :: M.Map Int (M.Map Int StarTime),
    _last_star_ts :: Int,
    _name :: Maybe String,
    _stars :: Int
  }
  deriving (Show, Generic)

makeLenses ''Member
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''Member

data LeaderBoard = LeaderBoard
  { _event :: String,
    _owner_id :: Int,
    _members :: M.Map Int Member
  }
  deriving (Show, Generic)

makeLenses ''LeaderBoard
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''LeaderBoard

solveTime :: Int -> Int -> String
solveTime d f =
  let s = UTCTime (fromGregorian 2022 12 d) (3600 * 5)
      e = posixSecondsToUTCTime (fromIntegral f)
   in formatTime defaultTimeLocale "%3h:%02M:%02S" (diffUTCTime e s)

printBoard b = mapM_ printMember (b ^. members)
  where
    printMember m = do
      case m ^. name of
        Just m' -> printf "%s (%d)\n" m' (m ^. local_score)
        Nothing -> printf "Anonymous user #%d\n" (m ^. FormatLeaderBoard.id)
      mapM_ printDay [(d, getTs d 1, getTs d 2) | d <- [1 .. 25]]
      putStrLn ""
      where
        getTs :: Int -> Int -> Maybe StarTime
        getTs dd s = m ^? completion_day_level . ix dd . ix s
        printDay = \case
          (_, Nothing, Nothing) -> return ()
          (d, Just t1, Nothing) -> printf "%d %s\n" d (solveTime d (t1 ^. get_star_ts))
          (d, Just t1, Just t2) -> do
            printf "%d %s, %s" d (solveTime d (t1 ^. get_star_ts)) (solveTime d (t2 ^. get_star_ts)) >> putStrLn ""

main = do
  b <- B.readFile "534723.json"
  case decode b :: Maybe LeaderBoard of
    Nothing -> putStrLn "Failed to parse"
    Just json -> printBoard json
