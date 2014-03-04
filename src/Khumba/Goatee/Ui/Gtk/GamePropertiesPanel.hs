-- | A panel that displays a vertical list of controls for editing a game's
-- 'GameInfo'.
module Khumba.Goatee.Ui.Gtk.GamePropertiesPanel (
  GamePropertiesPanel
  , create
  , initialize
  , destruct
  , myWidget
  ) where

import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk hiding (Cursor)
import Khumba.Goatee.Sgf.Board
import Khumba.Goatee.Sgf.Monad hiding (on)
import Khumba.Goatee.Sgf.Property
import Khumba.Goatee.Sgf.Tree
import Khumba.Goatee.Sgf.Types
import Khumba.Goatee.Ui.Gtk.Common

data GamePropertiesPanel ui =
  GamePropertiesPanel { myUi :: UiRef ui
                      , myRegistrations :: ViewRegistrations
                      , myWidget :: Widget
                      , myBlackName :: Entry
                      , myBlackRank :: Entry
                      , myBlackTeam :: Entry
                      , myWhiteName :: Entry
                      , myWhiteRank :: Entry
                      , myWhiteTeam :: Entry
                      , myComment :: TextView
                      }

instance UiCtrl ui => UiView (GamePropertiesPanel ui) ui where
  viewName = const "GamePropertiesPanel"
  viewUiRef = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => UiRef ui -> IO (GamePropertiesPanel ui)
create uiRef = do
  let rows = 8
      cols = 2
  box <- vBoxNew False 0
  table <- tableNew rows cols False
  boxPackStart box table PackNatural 0

  let addSeparator row = do sep <- hSeparatorNew
                            tableAttachDefaults table sep 0 cols row (row + 1)
                            tableSetRowSpacing table (row - 1) 6
                            tableSetRowSpacing table row 6

  let blackRow = 0
  blackNameLabel <- labelNewWithMnemonic "_Black"
  blackNameEntry <- entryNew
  labelSetMnemonicWidget blackNameLabel blackNameEntry
  widgetSetSensitive blackNameEntry False
  tableAttachDefaults table blackNameLabel 0 1 blackRow (blackRow + 1)
  tableAttachDefaults table blackNameEntry 1 2 blackRow (blackRow + 1)

  blackRankLabel <- labelNewWithMnemonic "_Rank"
  blackRankEntry <- entryNew
  labelSetMnemonicWidget blackRankLabel blackRankEntry
  widgetSetSensitive blackRankEntry False
  tableAttachDefaults table blackRankLabel 0 1 (blackRow + 1) (blackRow + 2)
  tableAttachDefaults table blackRankEntry 1 2 (blackRow + 1) (blackRow + 2)

  blackTeamLabel <- labelNewWithMnemonic "T_eam"
  blackTeamEntry <- entryNew
  labelSetMnemonicWidget blackTeamLabel blackTeamEntry
  widgetSetSensitive blackTeamEntry False
  tableAttachDefaults table blackTeamLabel 0 1 (blackRow + 2) (blackRow + 3)
  tableAttachDefaults table blackTeamEntry 1 2 (blackRow + 2) (blackRow + 3)

  addSeparator 3

  let whiteRow = 4
  whiteNameLabel <- labelNewWithMnemonic "_White"
  whiteNameEntry <- entryNew
  labelSetMnemonicWidget whiteNameLabel whiteNameEntry
  widgetSetSensitive whiteNameEntry False
  tableAttachDefaults table whiteNameLabel 0 1 whiteRow (whiteRow + 1)
  tableAttachDefaults table whiteNameEntry 1 2 whiteRow (whiteRow + 1)

  whiteRankLabel <- labelNewWithMnemonic "Ran_k"
  whiteRankEntry <- entryNew
  labelSetMnemonicWidget whiteRankLabel whiteRankEntry
  widgetSetSensitive whiteRankEntry False
  tableAttachDefaults table whiteRankLabel 0 1 (whiteRow + 1) (whiteRow + 2)
  tableAttachDefaults table whiteRankEntry 1 2 (whiteRow + 1) (whiteRow + 2)

  whiteTeamLabel <- labelNewWithMnemonic "Te_am"
  whiteTeamEntry <- entryNew
  labelSetMnemonicWidget whiteTeamLabel whiteTeamEntry
  widgetSetSensitive whiteTeamEntry False
  tableAttachDefaults table whiteTeamLabel 0 1 (whiteRow + 2) (whiteRow + 3)
  tableAttachDefaults table whiteTeamEntry 1 2 (whiteRow + 2) (whiteRow + 3)

  addSeparator 7

  comment <- textViewNew
  textViewSetWrapMode comment WrapWord
  commentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy commentScroll PolicyAutomatic PolicyAutomatic
  containerAdd commentScroll comment
  boxPackStart box commentScroll PackGrow 0

  registrations <- viewNewRegistrations

  return GamePropertiesPanel { myUi = uiRef
                             , myRegistrations = registrations
                             , myWidget = toWidget box
                             , myBlackName = blackNameEntry
                             , myBlackRank = blackRankEntry
                             , myBlackTeam = blackTeamEntry
                             , myWhiteName = whiteNameEntry
                             , myWhiteRank = whiteRankEntry
                             , myWhiteTeam = whiteTeamEntry
                             , myComment = comment
                             }

initialize :: UiCtrl ui => GamePropertiesPanel ui -> IO ()
initialize me = do
  ui <- readUiRef $ myUi me

  -- Watch for game info changes.
  viewRegister me gameInfoChangedEvent $ \_ newInfo ->
    afterGo $ updateUiGameInfo me newInfo

  -- Watch for node changes.
  let onNodeChange = do cursor <- getCursor
                        afterGo $ updateUiNodeInfo me cursor
  viewRegister me navigationEvent $ const onNodeChange
  viewRegister me propertiesChangedEvent $ const $ const onNodeChange

  updateUi me =<< readCursor ui

  -- TODO Connect black/white name/rank/team, but first generalize the structure
  -- that binds the comment field to the model.

  commentBuffer <- textViewGetBuffer $ myComment me
  void $ on commentBuffer bufferChanged $ do
    newComment <- get commentBuffer textBufferText
    runUiGo ui $ modifyComment $ const newComment

destruct :: UiCtrl ui => GamePropertiesPanel ui -> IO ()
destruct = viewUnregisterAll

updateUi :: GamePropertiesPanel ui -> Cursor -> IO ()
updateUi me cursor = do
  updateUiGameInfo me $ boardGameInfo $ cursorBoard cursor
  updateUiNodeInfo me cursor

updateUiGameInfo :: GamePropertiesPanel ui -> GameInfo -> IO ()
updateUiGameInfo me info =
  forM_ [(gameInfoBlackName, myBlackName),
         (gameInfoBlackRank, myBlackRank),
         (gameInfoBlackTeamName, myBlackTeam),
         (gameInfoWhiteName, myWhiteName),
         (gameInfoWhiteRank, myWhiteRank),
         (gameInfoWhiteTeamName, myWhiteTeam)] $ \(getter, entry) ->
    entrySetText (entry me) $ fromMaybe "" $ getter info

updateUiNodeInfo :: GamePropertiesPanel ui -> Cursor -> IO ()
updateUiNodeInfo me cursor = do
  let newText = maybe "" fromText $ getProperty propertyC $ cursorNode cursor
  buf <- textViewGetBuffer $ myComment me
  oldText <- get buf textBufferText
  when (oldText /= newText) $ textBufferSetText buf newText
