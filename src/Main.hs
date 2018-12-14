{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Graphics.Vty as Vty

import qualified Data.List as List
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad (void)
import Data.Maybe (mapMaybe)
import System.Process(system)

import Data.Aeson
import qualified Network.HTTP.Req as R
import qualified Data.ByteString as ByteString
import Control.Monad.IO.Class
import Data.Default.Class
import GHC.Generics
import Control.Exception

import qualified Data.FuzzySet as FuzzySet

type Tags = [Text]

data Event = QuitEvent | PreviousItemEvent | NextItemEvent | FilterItemsEvent Char | FilterItemsBackspaceEvent | FilterItemsApplyEvent | FilterItemsQuitEvent | RemoveFilterEvent | PlayStreamEvent

data DefaultFilterer = DefaultFilterer { _DefaultFilterer_filterTags :: Bool
                                         }

data TwitchStreamInfo = TwitchStreamInfo { _TwitchStreamInfo_name :: Text,
                                           _TwitchStreamInfo_tags :: Tags
                                         } deriving (Show)

drawTwitchStreamInfo :: Bool -> Bool -> TwitchStreamInfo -> Widget AppName
drawTwitchStreamInfo isSelected isAlternate info = withAttr attr $ widget
  where name = _TwitchStreamInfo_name info
        tags = _TwitchStreamInfo_tags info
        widget = txt $ T.concat ["Name: ", name, ". Tags: '", T.intercalate ", " tags, "'"]
        attr = case (isSelected, isAlternate) of
                 (True, _) -> itemSelectedAttr 
                 (_, True) -> itemAlternateAttr 
                 (_, _)-> itemAttr

playTwitchStream :: TwitchStreamInfo -> IO ()
playTwitchStream info = void . system $ T.unpack command
  where name = _TwitchStreamInfo_name info
        command = T.concat ["streamlink twitch.tv/", name, " 360p"]

handlePlayStreamEvent :: AppState -> EventM AppName (Next AppState)
handlePlayStreamEvent state = suspendAndResume $ do
  let selectedItem = _AppState_selectedItem state
      streams = _AppState_visibleStreams state
      
  case (length streams > selectedItem, selectedItem >= 0) of
    (True, True) -> playTwitchStream $ streams !! selectedItem
    (_, _) -> return ()
    
  return state

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
  
instance SelectableWidget TwitchStreamInfo where
  drawSelectableWidget = drawTwitchStreamInfo

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

itemSelectedAttr, itemAlternateAttr, itemAttr :: AttrName
itemSelectedAttr = "itemSelected"
itemAlternateAttr = "itemAlternate"
itemAttr = "item"

theMap :: AttrMap
theMap = attrMap Vty.defAttr [ (itemSelectedAttr, fg Vty.green),
                               (itemAlternateAttr, fg Vty.blue),
                               (itemAttr, Vty.defAttr)
                             ]

drawUI :: AppState -> [Widget AppName]
drawUI state = [withBorderStyle unicode $ ui]
  where ui = case view of
               MainView -> vBox [
                 vLimit 2 $ viewport ViewportHeader Vertical $ vBox [txt "Streams", hBorder],
                 viewport ViewportMain Vertical $ vBox widgets
                 ]
               FilterView -> vBox [
                 vLimit 2 $ viewport ViewportHeader Vertical $ vBox [txt "Streams", hBorder],
                 viewport ViewportMain Vertical $ vBox widgets,
                 vLimit 2 $ viewport ViewportFooter Vertical $ vBox [hBorder, txt (T.concat ["filter: ", filterText]) ]
                 ]
  
        selectedItem = _AppState_selectedItem state
        nItems = length streams
        filterText = _AppState_filterText state
        view = _AppState_view state
  
        streams = _AppState_visibleStreams state

        (widgets, _) = foldr drawWidget ([], nItems) streams

        drawWidget :: TwitchStreamInfo -> ([Widget AppName], Int) -> ([Widget AppName], Int)
        drawWidget info (list, acc) = (widget:list, acc-1)
          where widget = drawSelectableWidget isSelected isAlternate info
                isSelected = (acc-1) == selectedItem
                isAlternate = (acc `mod` 2) == 0
                
handleEventFilterView :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEventFilterView state (AppEvent FilterItemsApplyEvent) = continue $ state { _AppState_view = FilterView, _AppState_visibleStreams = filteredStreams, _AppState_selectedItem = 0 }
  where streams = _AppState_streams state
        filterText = _AppState_filterText state
        
        filteredStreams = getMatches filterer filterText streams 
          where filterer = DefaultFilterer { _DefaultFilterer_filterTags = True }
                unfiltered = _AppState_streams state

handleEventFilterView state (AppEvent (FilterItemsEvent char)) = handleEventFilterView state { _AppState_filterText = filterText} $ AppEvent FilterItemsApplyEvent
  where filterText = T.snoc (_AppState_filterText state) char
  
handleEventFilterView state (AppEvent FilterItemsBackspaceEvent) = handleEventFilterView state { _AppState_filterText = filterText} $ AppEvent FilterItemsApplyEvent
  where filterText = case (T.unsnoc (_AppState_filterText state)) of
                       Just (text, _) -> text
                       Nothing -> ""
                       
handleEventFilterView state (AppEvent FilterItemsQuitEvent) = continue $ state { _AppState_view = MainView }
handleEventFilterView state (AppEvent RemoveFilterEvent) = continue $ state { _AppState_filterText = "", _AppState_visibleStreams = streams, _AppState_view = MainView }
  where streams = _AppState_streams state

handleEventFilterView state (VtyEvent (Vty.EvKey (Vty.KChar char) [])) = handleEventFilterView state $ AppEvent $ FilterItemsEvent char
handleEventFilterView state (VtyEvent (Vty.EvKey Vty.KBS [])) = handleEventFilterView state $ AppEvent FilterItemsBackspaceEvent
handleEventFilterView state (VtyEvent (Vty.EvKey Vty.KEnter [])) = handleEventFilterView state $ AppEvent FilterItemsQuitEvent
handleEventFilterView state (VtyEvent (Vty.EvKey (Vty.KChar 'g') [ Vty.MCtrl ])) = handleEventFilterView state $ AppEvent RemoveFilterEvent
handleEventFilterView state (VtyEvent (Vty.EvKey Vty.KEsc [])) = handleEvent state $ AppEvent QuitEvent
handleEventFilterView state _ = continue state

handleEventMainView :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEventMainView state (AppEvent FilterItemsApplyEvent) = handleEventFilterView state $ AppEvent FilterItemsApplyEvent
handleEventMainView state (AppEvent RemoveFilterEvent) = handleEventFilterView state $ AppEvent RemoveFilterEvent
handleEventMainView state (AppEvent PlayStreamEvent) = handlePlayStreamEvent state
handleEventMainView state (AppEvent PreviousItemEvent) = continue $ getNextItemState state False
handleEventMainView state (AppEvent NextItemEvent) = continue $ getNextItemState state True

handleEventMainView state (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = handleEventMainView state $ AppEvent RemoveFilterEvent
handleEventMainView state (VtyEvent (Vty.EvKey (Vty.KChar 'f') [])) = handleEventMainView state $ AppEvent FilterItemsApplyEvent
handleEventMainView state (VtyEvent (Vty.EvKey Vty.KDown [])) = handleEventMainView state $ AppEvent NextItemEvent
handleEventMainView state (VtyEvent (Vty.EvKey Vty.KUp [])) = handleEventMainView state $ AppEvent PreviousItemEvent
handleEventMainView state (VtyEvent (Vty.EvKey (Vty.KChar 'j') [])) = handleEventMainView state $ AppEvent NextItemEvent
handleEventMainView state (VtyEvent (Vty.EvKey (Vty.KChar 'k') [])) = handleEventMainView state $ AppEvent PreviousItemEvent
handleEventMainView state (VtyEvent (Vty.EvKey Vty.KEnter [])) = handleEventMainView state $ AppEvent PlayStreamEvent
handleEventMainView state (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = handleEvent state $ AppEvent QuitEvent
handleEventMainView state (VtyEvent (Vty.EvKey Vty.KEsc [])) = handleEvent state $ AppEvent QuitEvent
handleEventMainView state _ = continue state

handleEvent :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEvent state (AppEvent QuitEvent) = halt state
handleEvent state event = case (_AppState_view state) of
                            MainView -> handleEventMainView state event
                            FilterView -> handleEventFilterView state event

getNextItemState :: AppState -> Bool -> AppState
getNextItemState state next = state { _AppState_selectedItem = newSelectedItem }
  where selectedItem = _AppState_selectedItem state
        nItems = max 0 $ length (_AppState_visibleStreams state) - 1
        newSelectedItem = min nItems $ max 0 value

        value = case next of
                  True -> selectedItem + 1
                  False -> selectedItem - 1

runApp :: AppState -> IO AppState
runApp = defaultMain app
  where app :: App AppState Event AppName
        app = App { appDraw = drawUI 
                  , appHandleEvent = handleEvent
                  , appStartEvent = return
                  , appAttrMap = const theMap
                  , appChooseCursor = neverShowCursor
                  }

main :: IO ()
main = void $ runApp initialState
  where initialState = AppState { _AppState_streams = streams, _AppState_visibleStreams = streams, _AppState_filterText = "" , _AppState_selectedItem = 0, _AppState_view = MainView }
        streams = [s1, s2, s3]
        s1 = TwitchStreamInfo { _TwitchStreamInfo_name = "placeholder", _TwitchStreamInfo_tags = [ "fortnite", "holder" ] }
        s2 = TwitchStreamInfo { _TwitchStreamInfo_name = "placeholder2", _TwitchStreamInfo_tags = [ "holder" ] }
        s3 = TwitchStreamInfo { _TwitchStreamInfo_name = "ninja", _TwitchStreamInfo_tags = [ "fortnite" ] }


-- API Test stuff

testClientId = "phiay4sq36lfv9zu7cbqwz2ndnesfd8"
apiBaseUrl = "api.twitch.tv"
apiVersion = "helix"

data TwitchStreamsData = TwitchStreamsData { _TwitchStreamsData_data :: [TwitchStreamsDataUser],
                                             _TwitchStreamsData_pagination :: TwitchStreamsDataPagination
                                           } deriving (Generic, Show)

data TwitchStreamsDataPagination = TwitchStreamsDataPagination { _TwitchStreamsDataPagination_cursor :: Text } deriving (Generic, Show)

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
  
instance FromJSON TwitchStreamsDataPagination where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 29 }
  
instance ToJSON TwitchStreamsDataPagination where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 29 }
  
instance R.MonadHttp IO where
  handleHttpException = throwIO

testList = ["ninja"]
api = apiTest testClientId testList
api2 = apiWithExceptionHandling testClientId testList

apiTest :: ByteString.ByteString -> [Text] -> IO ()
apiTest clientId usernames = R.runReq def $ do
  let options = (R.header "Client-ID" clientId) <> usersQuery
      usersQuery = mconcat $ map ("user_login" R.=:) usernames
  r <- R.req R.GET (R.https apiBaseUrl R./: apiVersion R./: "streams") R.NoReqBody R.jsonResponse options
  liftIO $ print (R.responseBody r :: Value)

apiTest2 :: ByteString.ByteString -> [Text] -> IO (Maybe TwitchStreamsData)
apiTest2 clientId usernames = do
  let options = (R.header "Client-ID" clientId) <> usersQuery
      usersQuery = mconcat $ map ("user_login" R.=:) usernames
  request <- R.req R.GET (R.https apiBaseUrl R./: apiVersion R./: "streams") R.NoReqBody R.bsResponse options
  let maybe = decodeStrict' (R.responseBody request) :: Maybe TwitchStreamsData
  return maybe

apiWithExceptionHandling :: ByteString.ByteString -> [Text] -> IO (Either Text TwitchStreamsData)
apiWithExceptionHandling clientId usernames = do
  query <- try (apiTest2 clientId usernames) :: IO (Either SomeException (Maybe TwitchStreamsData))
  case query of
    Left error -> return . Left . T.pack $ show error
    Right (Just sData) -> return $ Right sData
    Right (Nothing) -> return $ Left "Error couldn't deserialize json into TwitchStreamsData"
