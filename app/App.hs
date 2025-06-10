{-# LANGUAGE TemplateHaskell #-}

module App where

import Brick.AttrMap (AttrMap, AttrName, attrMap, attrName)
import Brick.Main (App (..), defaultMain, halt, showCursorNamed)
import Brick.Types (BrickEvent (..), EventM, Viewport, Widget, zoom)
import Brick.Util (fg, on)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicode)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
    hLimit,
    str,
    vBox,
    vLimit,
    viewport,
    visible,
    withBorderStyle,
    withDefAttr,
    (<+>),
 )
import Brick.Widgets.Edit (
    Editor,
    editFocusedAttr,
    editor,
    handleEditorEvent,
    renderEditor,
 )
import Control.Monad (void)
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

data Name
    = Edit
    | Output
    deriving (Ord, Show, Eq)

data St
    = St
    { _edit :: Editor String Name
    }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [editorUi]
  where
    editorUi = withBorderStyle unicode $ border drawEditor
      where
        label = str "Editor"
        drawEditor = renderEditor (str . unlines) True (st ^. edit)

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent ev = do
    zoom edit $ handleEditorEvent ev

initialState :: St
initialState =
    St (editor Edit Nothing "")

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (editFocusedAttr, V.defAttr)
        ]

theApp :: App St e Name
theApp =
    App
        { appDraw = drawUI
        , appChooseCursor = const $ showCursorNamed Edit
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

runApp :: IO ()
runApp = do
    void $ defaultMain theApp initialState
