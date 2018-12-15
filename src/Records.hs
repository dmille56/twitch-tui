{-# LANGUAGE DeriveGeneric #-}

module Records where

import Brick
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text (Text)
import Data.Default.Class
import Data.Maybe (mapMaybe)
import qualified Data.FuzzySet as FuzzySet
import Data.Aeson
import GHC.Generics

type Tags = [Text]

data Event = QuitEvent | PreviousItemEvent | NextItemEvent | FilterItemsEvent Char | FilterItemsBackspaceEvent | FilterItemsApplyEvent | FilterItemsQuitEvent | RemoveFilterEvent | PlayStreamEvent

data DefaultFilterer = DefaultFilterer { _DefaultFilterer_filterTags :: Bool
                                         }

instance Default DefaultFilterer where
  def = DefaultFilterer True

data TwitchStreamInfo = TwitchStreamInfo { _TwitchStreamInfo_name :: Text,
                                           _TwitchStreamInfo_tags :: Tags
                                         } deriving (Show)

class TextShow a where
  getText :: a -> Text

class Tagged a where
  getTags :: a -> Tags
 
class (Tagged a, TextShow a) => Filterable a

class Filterer a where
  getMatches :: (Filterable b) => a -> Text -> [b] -> [b] 

class SelectableWidget a where
  drawSelectableWidget :: Bool -> Bool -> a -> Widget AppName

instance Tagged TwitchStreamInfo where
  getTags = _TwitchStreamInfo_tags

instance TextShow TwitchStreamInfo where
  getText = _TwitchStreamInfo_name

instance Filterable TwitchStreamInfo
  
instance Filterer DefaultFilterer where
  getMatches filterer filterText [] = []
  getMatches filterer filterText items = matches
    where fuzzy = FuzzySet.fromList $ map getText tagFilteredItems
          results = FuzzySet.getWithMinScore 0.01 fuzzy tagLessText
          matches = case (tagLessText, results) of
                      ("", _) -> tagFilteredItems
                      (_, []) -> []
                      (_, _) -> mapMaybe (getItem getText tagFilteredItems . snd) results

          getItem f list item = List.find (\x -> f x == item) list

          --Filter for tags
          
          splitText = T.words filterText

          textTagsToInclude = map T.tail $ filter (\x -> T.head x == '+') splitText
          textTagsToExclude = map T.tail $ filter (\x -> T.head x == '-') splitText

          doTagsMatch textList item = any (\x -> match x) $ getTags item
            where match x = any (\y -> x == y) textList

          includedItems = case textTagsToInclude of
                            [] -> items
                            _ -> filter (doTagsMatch textTagsToInclude) items

          tagFilteredItems = case textTagsToExclude of
                               [] -> includedItems
                               _ -> filter (not . doTagsMatch textTagsToExclude) includedItems
                               
          startsWithPlusOrMinus x = case T.head x of
                                      '+' -> True
                                      '-' -> True
                                      _ -> False

          tagLessText = T.unwords $ filter (not . startsWithPlusOrMinus) splitText

data AppState = AppState { _AppState_streams :: [TwitchStreamInfo],
                           _AppState_visibleStreams :: [TwitchStreamInfo],
                           _AppState_filterText :: Text,
                           _AppState_selectedItem :: Int,
                           _AppState_view :: AppView
                         } deriving (Show)

data AppView = MainView | FilterView deriving (Show)

data AppName = ViewportMain | ViewportHeader | ViewportFooter deriving (Ord, Show, Eq)

data TwitchStreamsData = TwitchStreamsData { _TwitchStreamsData_data :: [TwitchStreamsDataUser],
                                             _TwitchStreamsData_pagination :: TwitchPagination
                                           } deriving (Generic, Show)

data TwitchPagination = TwitchPagination { _TwitchPagination_cursor :: Text } deriving (Generic, Show)

data TwitchStreamsDataUser = TwitchStreamsDataUser { _TwitchStreamsDataUser_id :: Text,
                                                     _TwitchStreamsDataUser_user_id :: Text,
                                                     _TwitchStreamsDataUser_user_name :: Text,
                                                     _TwitchStreamsDataUser_game_id :: Text,
                                                     _TwitchStreamsDataUser_community_ids :: [Text],
                                                     _TwitchStreamsDataUser_type :: Text,
                                                     _TwitchStreamsDataUser_title :: Text,
                                                     _TwitchStreamsDataUser_viewer_count :: Int,
                                                     _TwitchStreamsDataUser_started_at :: Text,
                                                     _TwitchStreamsDataUser_language :: Text,
                                                     _TwitchStreamsDataUser_thumbnail_url :: Text
                                                   } deriving (Generic, Show)

instance FromJSON TwitchStreamsData where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 19 }
  
instance ToJSON TwitchStreamsData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 19 }
  
instance FromJSON TwitchStreamsDataUser where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 23 }
  
instance ToJSON TwitchStreamsDataUser where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 23 }
  
instance FromJSON TwitchPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 18 }
  
instance ToJSON TwitchPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 18 }

data TwitchTopGamesData = TwitchTopGamesData { _TwitchTopGamesData_data :: [TwitchTopGamesDataGame],
                                               _TwitchTopGamesData_pagination :: TwitchPagination
                                             } deriving (Generic, Show)

data TwitchTopGamesDataGame = TwitchTopGamesDataGame { _TwitchTopGamesDataGame_id :: Text,
                                                       _TwitchTopGamesDataGame_name :: Text,
                                                       _TwitchTopGamesDataGame_box_art_url :: Text
                                                     } deriving (Generic, Show)

instance FromJSON TwitchTopGamesData where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 20 }
  
instance ToJSON TwitchTopGamesData where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 20 }
  
instance FromJSON TwitchTopGamesDataGame where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 24 }
  
instance ToJSON TwitchTopGamesDataGame where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 24 }
