{-# LANGUAGE OverloadedStrings #-}

module Draw where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import qualified Data.Text as T

import Records
import Themes

instance SelectableWidget TwitchStreamInfo where
  drawSelectableWidget = drawTwitchStreamInfo

drawTwitchStreamInfo :: Bool -> Bool -> TwitchStreamInfo -> Widget AppName
drawTwitchStreamInfo isSelected isAlternate info = withAttr attr $ widget
  where name = _TwitchStreamInfo_name info
        tags = _TwitchStreamInfo_tags info
        widget = txt $ T.concat ["Name: ", name, ". Tags: '", T.intercalate ", " tags, "'"]
        attr = case (isSelected, isAlternate) of
                 (True, _) -> itemSelectedAttr 
                 (_, True) -> itemAlternateAttr 
                 (_, _)-> itemAttr


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
