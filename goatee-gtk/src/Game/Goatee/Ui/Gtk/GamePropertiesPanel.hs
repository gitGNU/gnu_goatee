-- This file is part of Goatee.
--
-- Copyright 2014 Bryan Gardiner
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

-- | A panel that displays a vertical list of controls for editing a game's
-- 'GameInfo'.
module Game.Goatee.Ui.Gtk.GamePropertiesPanel (
  GamePropertiesPanel,
  create,
  destroy,
  myWidget,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (forM_, void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad hiding (on)
import Game.Goatee.Sgf.Property
import Game.Goatee.Sgf.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Utils
import Graphics.UI.Gtk (
  AttachOptions (Expand, Fill),
  AttrOp ((:=)),
  CheckButton,
  Entry,
  Label,
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic, PolicyNever),
  SpinButton,
  TextView,
  Widget,
  boxPackStart,
  checkButtonNewWithLabel,
  containerAdd,
  entryNew, entrySetText, entryWidthChars,
  hBoxNew,
  hSeparatorNew,
  labelNew, labelNewWithMnemonic, labelSetMnemonicWidget, labelSetText,
  onValueSpinned,
  scrolledWindowAddWithViewport, scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  spinButtonNewWithRange, spinButtonSetDigits, spinButtonSetValue,
  tableAttach, tableAttachDefaults, tableNew, tableSetRowSpacing,
  textViewNew,
  toWidget,
  vBoxNew,
  widgetSetSizeRequest,
  )

data GamePropertiesPanel ui = GamePropertiesPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget

    -- Black's info.
  , myBlackNameEntry :: Entry
  , myBlackRankEntry :: Entry
  , myBlackTeamEntry :: Entry

    -- White's info.
  , myWhiteNameEntry :: Entry
  , myWhiteRankEntry :: Entry
  , myWhiteTeamEntry :: Entry

    -- Game rules.
  , myRulesetEntry :: Entry
  , myMainTimeSpin :: SpinButton
  , myMainTimeLabel :: Label
  , myOvertimeEntry :: Entry
  , myGameResultDisplayCheck :: CheckButton
  , myGameResultEntry :: Entry

    -- Game editors.
  , myGameAnnotatorEntry :: Entry
  , myGameEntererEntry :: Entry

    -- Game context.
  , myEventNameEntry :: Entry
  , myGamePlaceEntry :: Entry
  , myGameRoundEntry :: Entry
  , myGameDatesEntry :: Entry
  , myGameNameEntry :: Entry
  , myGameSourceEntry :: Entry
  , myGameCopyrightEntry :: Entry

    -- Further commentary.
  , myGameOpeningEntry :: Entry
  , myGameCommentTextView :: TextView
  , myGameCommentTextViewSetter :: String -> IO ()
  }

instance UiCtrl ui => UiView (GamePropertiesPanel ui) ui where
  viewName = const "GamePropertiesPanel"
  viewCtrl = myUi
  viewRegistrations = myRegistrations

create :: UiCtrl ui => ui -> IO (GamePropertiesPanel ui)
create ui = do
  let rows = 27
      cols = 2
  scroll <- scrolledWindowNew Nothing Nothing
  box <- vBoxNew False 0
  table <- tableNew rows cols False
  scrolledWindowSetPolicy scroll PolicyNever PolicyAutomatic
  scrolledWindowAddWithViewport scroll box
  boxPackStart box table PackNatural 0

  nextRowRef <- newIORef 0
  let nextRow = do
        row <- readIORef nextRowRef
        writeIORef nextRowRef $ row + 1
        return row
      addSeparator = do
        row <- nextRow
        sep <- hSeparatorNew
        tableAttachDefaults table sep 0 cols row (row + 1)
        tableSetRowSpacing table (row - 1) 6
        tableSetRowSpacing table row 6
      addWidget labelText widget = do
        row <- nextRow
        label <- labelNewWithMnemonic labelText
        labelSetMnemonicWidget label widget
        tableAttach table label 0 1 row (row + 1)
                    [Fill] [] 0 0
        widgetSetSizeRequest widget 0 (-1)
        tableAttach table widget 1 2 row (row + 1)
                    [Expand, Fill] [] 0 0
        return widget
      addWideWidget widget = do
        row <- nextRow
        widgetSetSizeRequest widget 0 (-1)
        tableAttachDefaults table widget 0 2 row (row + 1)
        return widget
      tableEntryNew = do
        entry <- entryNew
        set entry [entryWidthChars := 0]
        return entry

  blackNameEntry <- addWidget "_Black" =<< tableEntryNew
  blackRankEntry <- addWidget "_Rank" =<< tableEntryNew
  blackTeamEntry <- addWidget "T_eam" =<< tableEntryNew
  addSeparator
  whiteNameEntry <- addWidget "_White" =<< tableEntryNew
  whiteRankEntry <- addWidget "Ran_k" =<< tableEntryNew
  whiteTeamEntry <- addWidget "Te_am" =<< tableEntryNew
  addSeparator
  rulesetEntry <- addWidget "Ruleset" =<< tableEntryNew

  mainTimeBox <- hBoxNew True 0
  mainTimeSpin <- spinButtonNewWithRange 0 3155692600 {- 100 years -} 1
  spinButtonSetDigits mainTimeSpin 1
  mainTimeLabel <- labelNew Nothing
  boxPackStart mainTimeBox mainTimeSpin PackGrow 0
  boxPackStart mainTimeBox mainTimeLabel PackNatural 0
  addWidget "Time" mainTimeBox

  overtimeEntry <- addWidget "Overtime" =<< tableEntryNew
  gameResultDisplayCheck <- addWideWidget =<< checkButtonNewWithLabel "Show game result"
  gameResultEntry <- addWidget "Result" =<< tableEntryNew
  addSeparator
  gameAnnotatorEntry <- addWidget "Annotator" =<< tableEntryNew
  gameEntererEntry <- addWidget "Enterer" =<< tableEntryNew
  addSeparator
  eventNameEntry <- addWidget "Event" =<< tableEntryNew
  gamePlaceEntry <- addWidget "Place" =<< tableEntryNew
  gameRoundEntry <- addWidget "Round" =<< tableEntryNew
  gameDatesEntry <- addWidget "Dates" =<< tableEntryNew
  gameNameEntry <- addWidget "Name" =<< tableEntryNew
  gameSourceEntry <- addWidget "Source" =<< tableEntryNew
  gameCopyrightEntry <- addWidget "Copyright" =<< tableEntryNew
  addSeparator
  gameOpeningEntry <- addWidget "Opening" =<< tableEntryNew
  addWideWidget =<< labelNew (Just "Game comment:")

  gameCommentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  gameCommentTextView <- textViewNew
  -- Set a minimum height on the text view, otherwise it's too short.
  widgetSetSizeRequest gameCommentTextView 0 175
  containerAdd gameCommentScroll gameCommentTextView
  boxPackStart box gameCommentScroll PackNatural 0

  gameCommentTextViewSetter <- textViewConfigure gameCommentTextView $ \value ->
    runUiGo ui $ void $ modifyGameInfo $ \info ->
    info { gameInfoGameComment = if null value then Nothing else Just value }

  addedRowCount <- readIORef nextRowRef
  when (addedRowCount /= rows) $ fail $
    "GamePropertiesPanel: Table expected " ++ show rows ++ " rows, got " ++
    show addedRowCount ++ "."

  registrations <- viewNewRegistrations

  let me = GamePropertiesPanel {
        myUi = ui
        , myRegistrations = registrations
        , myWidget = toWidget scroll

        , myBlackNameEntry = blackNameEntry
        , myBlackRankEntry = blackRankEntry
        , myBlackTeamEntry = blackTeamEntry

        , myWhiteNameEntry = whiteNameEntry
        , myWhiteRankEntry = whiteRankEntry
        , myWhiteTeamEntry = whiteTeamEntry

        , myRulesetEntry = rulesetEntry
        , myMainTimeSpin = mainTimeSpin
        , myMainTimeLabel = mainTimeLabel
        , myOvertimeEntry = overtimeEntry
        , myGameResultDisplayCheck = gameResultDisplayCheck
        , myGameResultEntry = gameResultEntry

        , myGameAnnotatorEntry = gameAnnotatorEntry
        , myGameEntererEntry = gameEntererEntry

        , myEventNameEntry = eventNameEntry
        , myGamePlaceEntry = gamePlaceEntry
        , myGameRoundEntry = gameRoundEntry
        , myGameDatesEntry = gameDatesEntry
        , myGameNameEntry = gameNameEntry
        , myGameSourceEntry = gameSourceEntry
        , myGameCopyrightEntry = gameCopyrightEntry

        , myGameOpeningEntry = gameOpeningEntry
        , myGameCommentTextView = gameCommentTextView
        , myGameCommentTextViewSetter = gameCommentTextViewSetter
        }

  initialize me
  return me

initialize :: UiCtrl ui => GamePropertiesPanel ui -> IO ()
initialize me = do
  -- Watch for game info changes.
  viewRegister me gameInfoChangedEvent $ \_ newInfo ->
    afterGo $ updateUiGameInfo me newInfo

  updateUi me =<< readCursor ui

  connectEntry myBlackNameEntry $ \x info -> info { gameInfoBlackName = strToMaybe x }
  connectEntry myBlackRankEntry $ \x info -> info { gameInfoBlackRank = strToMaybe x }
  connectEntry myBlackTeamEntry $ \x info -> info { gameInfoBlackTeamName = strToMaybe x }

  connectEntry myWhiteNameEntry $ \x info -> info { gameInfoWhiteName = strToMaybe x }
  connectEntry myWhiteRankEntry $ \x info -> info { gameInfoWhiteRank = strToMaybe x }
  connectEntry myWhiteTeamEntry $ \x info -> info { gameInfoWhiteTeamName = strToMaybe x }

  connectEntry myRulesetEntry $ \x info -> info { gameInfoRuleset = toRuleset <$> strToMaybe x }
  connect (onValueSpinned $ myMainTimeSpin me)
          (spinButtonGetValueAsRational $ myMainTimeSpin me) $ \x info ->
    info { gameInfoBasicTimeSeconds = if x == 0 then Nothing else Just x }
  connectEntry myOvertimeEntry $ \x info -> info { gameInfoOvertime = strToMaybe x }
  -- TODO Game result display checkbox.
  connectEntry myGameResultEntry $ \x info ->
    info { gameInfoResult = parseGameResult . toSimpleText <$> strToMaybe x }

  connectEntry myGameAnnotatorEntry $ \x info -> info { gameInfoAnnotatorName = strToMaybe x }
  connectEntry myGameEntererEntry $ \x info -> info { gameInfoEntererName = strToMaybe x }

  connectEntry myEventNameEntry $ \x info -> info { gameInfoEvent = strToMaybe x }
  connectEntry myGamePlaceEntry $ \x info -> info { gameInfoPlace = strToMaybe x }
  connectEntry myGameRoundEntry $ \x info -> info { gameInfoRound = strToMaybe x }
  connectEntry myGameDatesEntry $ \x info -> info { gameInfoDatesPlayed = strToMaybe x }
  connectEntry myGameNameEntry $ \x info -> info { gameInfoGameName = strToMaybe x }
  connectEntry myGameSourceEntry $ \x info -> info { gameInfoSource = strToMaybe x }
  connectEntry myGameCopyrightEntry $ \x info -> info { gameInfoCopyright = strToMaybe x }

  connectEntry myGameOpeningEntry $ \x info -> info { gameInfoOpeningComment = strToMaybe x }

  where ui = myUi me
        --connectEntry :: (ui' ~ ui)
        --             => (GamePropertiesPanel ui' -> Entry)
        --             -> (String -> GameInfo -> GameInfo)
        --             -> IO ()
        connectEntry entryAccessor updater =
          onEntryChange (entryAccessor me) $ \value ->
          runUiGo ui $ void $ modifyGameInfo $ updater value
        --connect :: (IO () -> IO ()) -- ^ Function to install a handler.
        --        -> IO a             -- ^ Getter for the widget value.
        --        -> (a -> GameInfo -> GameInfo)
        --        -> IO ()
        connect connectFn getter updater =
          connectFn $ do
            newValue <- getter
            runUiGo ui $ void $ modifyGameInfo $ updater newValue
        strToMaybe str = if null str then Nothing else Just str

destroy :: UiCtrl ui => GamePropertiesPanel ui -> IO ()
destroy = viewUnregisterAll

updateUi :: UiCtrl ui => GamePropertiesPanel ui -> Cursor -> IO ()
updateUi me cursor = updateUiGameInfo me $ boardGameInfo $ cursorBoard cursor

updateUiGameInfo :: GamePropertiesPanel ui -> GameInfo -> IO ()
updateUiGameInfo me info = do
  forM_ [ (gameInfoBlackName, myBlackNameEntry)
        , (gameInfoBlackRank, myBlackRankEntry)
        , (gameInfoBlackTeamName, myBlackTeamEntry)

        , (gameInfoWhiteName, myWhiteNameEntry)
        , (gameInfoWhiteRank, myWhiteRankEntry)
        , (gameInfoWhiteTeamName, myWhiteTeamEntry)

        , (fmap fromRuleset . gameInfoRuleset, myRulesetEntry)
        , (gameInfoOvertime, myOvertimeEntry)
        , (fmap renderGameResultPretty' . gameInfoResult, myGameResultEntry)

        , (gameInfoAnnotatorName, myGameAnnotatorEntry)
        , (gameInfoEntererName, myGameEntererEntry)

        , (gameInfoEvent, myEventNameEntry)
        , (gameInfoPlace, myGamePlaceEntry)
        , (gameInfoRound, myGameRoundEntry)
        , (gameInfoDatesPlayed, myGameDatesEntry)
        , (gameInfoGameName, myGameNameEntry)
        , (gameInfoSource, myGameSourceEntry)
        , (gameInfoCopyright, myGameCopyrightEntry)

        , (gameInfoOpeningComment, myGameOpeningEntry)
        ] $ \(getter, entry) ->
    entrySetText (entry me) $ fromMaybe "" $ getter info

  spinButtonSetValue (myMainTimeSpin me) $ maybe 0 fromRational $
    gameInfoBasicTimeSeconds info
  labelSetText (myMainTimeLabel me) $ maybe "" renderSeconds $
    gameInfoBasicTimeSeconds info

  myGameCommentTextViewSetter me $ fromMaybe "" $ gameInfoGameComment info

renderSeconds :: Rational -> String
renderSeconds totalSeconds =
  let isNegative = totalSeconds < 0
      totalSeconds' = abs totalSeconds
      wholeSeconds = truncate totalSeconds' :: Integer
      fractionalSeconds = totalSeconds' - fromIntegral wholeSeconds
      (totalMinutes, seconds) = wholeSeconds `divMod` 60
      (hours, minutes) = totalMinutes `divMod` 60
  in (if isNegative then ('-':) else id) $
     (if hours > 0
      then show hours ++ ':' : show2 minutes ++ ':' : show2 seconds
      else show minutes ++ ':' : show2 seconds) ++
     (if fractionalSeconds > 0
      then '.' : tail (dropWhile (/= '.') $ show $ fromRational fractionalSeconds)
      else [])
  where show2 n = if n < 10 then '0' : show n else show n
