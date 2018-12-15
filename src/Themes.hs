module Themes where

import Brick
import qualified Graphics.Vty as Vty

itemSelectedAttr, itemAlternateAttr, itemAttr :: AttrName
itemSelectedAttr = "itemSelected"
itemAlternateAttr = "itemAlternate"
itemAttr = "item"

theMap :: AttrMap
theMap = attrMap Vty.defAttr [ (itemSelectedAttr, fg Vty.green),
                               (itemAlternateAttr, fg Vty.blue),
                               (itemAttr, Vty.defAttr)
                             ]
