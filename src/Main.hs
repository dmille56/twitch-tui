module Main where

import Brick
import Control.Monad (void)

import Records
import Events (handleEvent)
import Themes (theMap)
import Draw (drawUI)

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
