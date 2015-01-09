-- This file is part of Goatee.
--
-- Copyright 2015 Bryan Gardiner
--
-- Goatee is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- Goatee is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with Goatee.  If not, see <http://www.gnu.org/licenses/>.

module Game.Goatee.Ui.Gtk.Tool.Visibility (VisibilityTool, create) where

import Control.Applicative ((<$>), (<|>))
import Control.Monad (void, when)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Set as Set
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad (
  AnyEvent (AnyEvent),
  deleteProperty,
  execGo,
  modifyPropertyCoords,
  navigationEvent,
  propertiesModifiedEvent,
  putProperty,
  )
import Game.Goatee.Lib.Property
import Game.Goatee.Lib.Tree
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Latch
import Graphics.UI.Gtk (
  AttrOp ((:=)),
  Button,
  ButtonsType (ButtonsOk),
  ComboBox,
  DialogFlags (DialogDestroyWithParent, DialogModal),
  HBox,
  ListStore,
  MessageType (MessageInfo),
  Packing (PackGrow, PackNatural),
  boxPackStart,
  buttonActivated, buttonNewWithLabel,
  cellLayoutAddColumnAttribute, cellLayoutPackStart,
  cellRendererTextNew, cellText,
  changed,
  comboBoxActive, comboBoxNewWithModel,
  dialogRun,
  get,
  hBoxNew,
  listStoreAppend, listStoreGetSize, listStoreNew, listStoreRemove,
  makeColumnIdString,
  messageDialogNewWithMarkup,
  on,
  set,
  toWidget,
  treeModelSetColumn,
  widgetDestroy, widgetSetSensitive, widgetTooltipText,
  )

-- | A 'UiTool' that toggles visibility properties in rectangles on the board.
data VisibilityTool ui = VisibilityTool
  { myUi :: ui
  , myViewState :: ViewState
  , myToolState :: ToolState
  , myDescriptor :: ValuedPropertyInfo CoordList
  , myBoardHasPointsPredicate :: BoardState -> Bool

    -- Widgets:
  , myBox :: HBox
  , myCombo :: ComboBox
  , myModel :: ListStore Mode
  , myCopyButton :: Button
  , myViewUpdateLatch :: Latch
    -- ^ This latch should be held on whenever updating the radio buttons above.
  }

-- | Describes the state of this tool's property on the current node.  The
-- 'Show' instance is used in UI text.
data Mode =
  ModeInherited
  -- ^ The property is not present on the current node, so the current value is
  -- inherited from the closest ancestor node to have the property.
  | ModeReset
    -- ^ The property is present on the current node, resetting all points back
    -- to default (visible or undimmed, depending on the property).
  | ModeAssigned
    -- ^ The property is present on the current node, with a new set of points.
    -- The set of points from the closest ancestor is abandoned.
  deriving (Bounded, Enum, Eq, Ord)

instance Show Mode where
  show mode = case mode of
    ModeInherited -> "Inherited"
    ModeReset -> "Reset"
    ModeAssigned -> "Assigned"

instance UiCtrl go ui => UiView go ui (VisibilityTool ui) where
  viewName me = "VisibilityTool(" ++ propertyName (myDescriptor me) ++ ")"

  viewCtrl = myUi

  viewState = myViewState

  viewUpdate = update

instance UiCtrl go ui => UiTool go ui (VisibilityTool ui) where
  toolState = myToolState

  toolPanelWidget = Just . toWidget . myBox

  toolGobanClickComplete me (Just from) (Just to) = do
    modifier <- getModifierForRegion me from to
    doUiGo (myUi me) $ modifyPropertyCoords (myDescriptor me) modifier

  toolGobanClickComplete _ _ _ = return ()

  toolGobanRenderGetBoard me cursor = do
    state <- toolGetGobanState me
    case (toolGobanStateStartCoord state, toolGobanStateCurrentCoord state) of
      (Just startCoord, Just endCoord) -> do
        modifier <- getModifierForRegion me startCoord endCoord
        return $ cursorBoard $ flip execGo cursor $
          modifyPropertyCoords (myDescriptor me) modifier
      _ -> return $ cursorBoard cursor

-- | Creates a 'VisibilityTool' that will modify regions of a type of visibility
-- on the board.
create :: UiCtrl go ui
       => ui
       -> ValuedPropertyInfo CoordList
       -> (BoardState -> Bool)
       -> String
       -> ToolState
       -> IO (VisibilityTool ui)
create ui descriptor boardHasPointsPrediate nounPlural toolState = do
  viewState <- viewStateNew

  box <- hBoxNew False 0
  latch <- newLatch

  -- Create a dropdown that will allow selecting between the different modes.
  -- Assigned mode may not be selected directly; it is activated by drawing on
  -- the board or clicking Copy, and will be added to the ListStore when (and
  -- only as long as) it is active.
  model <- listStoreNew [ModeInherited, ModeReset]
  combo <- comboBoxNewWithModel model
  let column = makeColumnIdString 0
  treeModelSetColumn model column show
  renderer <- cellRendererTextNew
  cellLayoutPackStart combo renderer True
  cellLayoutAddColumnAttribute combo renderer cellText column
  boxPackStart box combo PackGrow 0

  copyButton <- buttonNewWithLabel "Copy"
  boxPackStart box copyButton PackNatural 0

  helpButton <- buttonNewWithLabel "?"
  boxPackStart box helpButton PackNatural 0

  let me = VisibilityTool
        { myUi = ui
        , myViewState = viewState
        , myToolState = toolState
        , myDescriptor = descriptor
        , myBoardHasPointsPredicate = boardHasPointsPrediate

        , myBox = box
        , myCombo = combo
        , myModel = model
        , myCopyButton = copyButton
        , myViewUpdateLatch = latch
        }

  on combo changed $ whenLatchOff latch $ do
    mode <- toEnum <$> get combo comboBoxActive
    let descriptor = myDescriptor me
    case mode of
      ModeInherited -> doUiGo ui $ deleteProperty descriptor
      ModeReset -> doUiGo ui $ putProperty $ propertyBuilder descriptor emptyCoordList
      -- Shouldn't get here, since we only programatically give access to
      -- Assigned in the dropdown.
      ModeAssigned -> return ()

  on copyButton buttonActivated $ do
    let findAncestorProperty cursor =
          findProperty descriptor (cursorNode cursor) <|>
          (findAncestorProperty =<< cursorParent cursor)
    cursor <- readCursor ui
    forM_ (findAncestorProperty =<< cursorParent cursor) $ doUiGo ui . putProperty

  on helpButton buttonActivated $ showHelp ui nounPlural

  register me
    [ AnyEvent navigationEvent
    , AnyEvent propertiesModifiedEvent
    ]

  viewUpdate me
  return me

-- | Displays a help dialog explaining this tool's controls in the side panel.
showHelp :: UiCtrl go ui => ui -> String -> IO ()
showHelp ui nounPlural = do
  let message =
        intercalate "\n"
        [ "The dropdown displays the " ++ nounPlural ++ " of points on the current node."
        , ""
        , "<b>" ++ show ModeInherited ++ ":</b> Points inherit their values from the parent " ++
          "node.  This is the default for all nodes."
        , "<b>" ++ show ModeReset ++ ":</b> This node resets all points to their default values."
        , "<b>" ++ show ModeAssigned ++ ":</b> There is a custom set of points for this node " ++
          "and its descendents."
        , ""
        , "Drawing on the board with this tool will change the mode to " ++ show ModeAssigned ++
          ".  If there is an ancestor node with " ++ show ModeAssigned ++ " then you can click " ++
          "<b>Copy</b> to copy those values to the current node."
        ]
  window <- getMainWindow ui
  dialog <- messageDialogNewWithMarkup
            (Just window)
            [DialogModal, DialogDestroyWithParent]
            MessageInfo
            ButtonsOk
            message
  dialogRun dialog
  widgetDestroy dialog

update :: UiCtrl go ui => VisibilityTool ui -> IO ()
update me = do
  let ui = myUi me
  cursor <- readCursor ui

  -- Set the combo box to reflect the state of VW/DD on the current node.
  withLatchOn (myViewUpdateLatch me) $ setCombo me $
    case findPropertyValue (myDescriptor me) $ cursorNode cursor of
      Nothing -> ModeInherited
      Just coords | coords == emptyCoordList -> ModeReset
                  | otherwise -> ModeAssigned

  -- Enable the Copy button only if the parent has a non-empty VW/DD[...] that
  -- we can copy.  Copying reset properties isn't super important.
  let hasParentWithPoints =
        maybe False (myBoardHasPointsPredicate me . cursorBoard) $ cursorParent cursor
  setCopyButtonEnabled me hasParentWithPoints

-- | Sets the combo box to display the given 'Mode'.
setCombo :: UiCtrl go ui => VisibilityTool ui -> Mode -> IO ()
setCombo me mode = do
  let combo = myCombo me
      model = myModel me
  modelSize <- listStoreGetSize model
  let modelHasAssigned = modelSize == fromEnum (maxBound :: Mode) + 1
  when (mode == ModeAssigned && not modelHasAssigned) $
    void $ listStoreAppend model ModeAssigned
  set combo [comboBoxActive := fromEnum mode]
  when (mode /= ModeAssigned && modelHasAssigned) $
    listStoreRemove model $ fromEnum ModeAssigned

-- | Sets the Copy button's sensitivity and tooltip text.
setCopyButtonEnabled :: UiCtrl go ui => VisibilityTool ui -> Bool -> IO ()
setCopyButtonEnabled me enabled = do
  let button = myCopyButton me
  widgetSetSensitive button enabled
  set button [widgetTooltipText :=
              if enabled
              then Nothing
              else Just "The parent has no assigned points to copy."]

-- | @getModifierForRegion me from to@ can be called when a drag has happened
-- over the rectangle with corners @from@ and @to@ to get a function suitable
-- for passing into 'modifyPropertyCoords' that will make the drag take effect.
getModifierForRegion :: UiCtrl go ui
                     => VisibilityTool ui
                     -> Coord
                     -> Coord
                     -> IO ([Coord] -> [Coord])
getModifierForRegion me from to = do
  currentlySet <- cursorHasPointSet me from
  return $
    Set.toList .
    (if currentlySet then flip Set.difference else Set.union) (Set.fromList $ coordRange from to) .
    Set.fromList

-- | Determines whether the tool's property is set on the current node, and the
-- given point is contained in the property's coord list.
cursorHasPointSet :: UiCtrl go ui => VisibilityTool ui -> Coord -> IO Bool
cursorHasPointSet me coord =
  maybe False ((coord `elem`) . expandCoordList) .
  findPropertyValue (myDescriptor me) .
  cursorNode <$>
  readCursor (myUi me)
