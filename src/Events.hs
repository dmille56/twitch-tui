module Events where

import Brick
import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Control.Monad (void)
import System.Process (system)
import Data.Default.Class

import Records

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
  
handleEventFilterView :: AppState -> BrickEvent AppName Event -> EventM AppName (Next AppState)
handleEventFilterView state (AppEvent FilterItemsApplyEvent) = continue $ state { _AppState_view = FilterView, _AppState_visibleStreams = filteredStreams, _AppState_selectedItem = 0 }
  where streams = _AppState_streams state
        filterText = _AppState_filterText state
        
        filteredStreams = getMatches filterer filterText streams 
          where filterer = def :: DefaultFilterer
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
