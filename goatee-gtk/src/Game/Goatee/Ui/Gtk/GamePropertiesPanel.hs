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

import Control.Arrow (first)
import Control.Monad (forM_, void, when)
import Control.Monad.Trans (liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Game.Goatee.Common.Bigfloat as BF
import Game.Goatee.Sgf.Board
import Game.Goatee.Sgf.Monad hiding (on)
import Game.Goatee.Sgf.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Latch
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
  entryNew, entryText, entryWidthChars,
  focusOutEvent,
  hBoxNew,
  hSeparatorNew,
  labelNew, labelSetMnemonicWidget, labelSetText,
  on, onValueSpinned,
  scrolledWindowAddWithViewport, scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  spinButtonNewWithRange, spinButtonSetDigits, spinButtonSetValue,
  tableAttach, tableAttachDefaults, tableNew, tableSetRowSpacing,
  textViewNew,
  toWidget,
  vBoxNew,
  widgetSetSizeRequest,
  )

-- A getter for 'Stringlike' 'GameInfo' fields.
data InfoGetter = forall a. Stringlike a => InfoGetter (GameInfo -> Maybe a)

data GamePropertiesPanel ui = GamePropertiesPanel {
  myUi :: ui
  , myRegistrations :: ViewRegistrations
  , myWidget :: Widget
  , myLatch :: Latch
    -- ^ A latch to be held when updating the model, to prevent the
    -- view from re-updating.

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
        label <- labelNew $ Just labelText
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

  latch <- newLatch

  blackNameEntry <- addWidget "Black" =<< tableEntryNew
  blackRankEntry <- addWidget "Rank" =<< tableEntryNew
  blackTeamEntry <- addWidget "Team" =<< tableEntryNew
  addSeparator
  whiteNameEntry <- addWidget "White" =<< tableEntryNew
  whiteRankEntry <- addWidget "Rank" =<< tableEntryNew
  whiteTeamEntry <- addWidget "Team" =<< tableEntryNew
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
    info { gameInfoGameComment = if null value then Nothing else Just $ stringToSgf value }

  addedRowCount <- readIORef nextRowRef
  when (addedRowCount /= rows) $ fail $
    "GamePropertiesPanel: Table expected " ++ show rows ++ " rows, got " ++
    show addedRowCount ++ "."

  registrations <- viewNewRegistrations

  let me = GamePropertiesPanel {
        myUi = ui
        , myLatch = latch
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
  let ui = myUi me

  -- Watch for game info changes.
  viewRegister me gameInfoChangedEvent $ \_ newInfo ->
    afterGo $ updateUiGameInfo me newInfo

  updateUi me =<< readCursor ui

  connectEntry me (myBlackNameEntry me) gameInfoBlackName $ \x info ->
    info { gameInfoBlackName = x }
  connectEntry me (myBlackRankEntry me) gameInfoBlackRank $ \x info ->
    info { gameInfoBlackRank = x }
  connectEntry me (myBlackTeamEntry me) gameInfoBlackTeamName $ \x info ->
    info { gameInfoBlackTeamName = x }

  connectEntry me (myWhiteNameEntry me) gameInfoWhiteName $ \x info ->
    info { gameInfoWhiteName = x }
  connectEntry me (myWhiteRankEntry me) gameInfoWhiteRank $ \x info ->
    info { gameInfoWhiteRank = x }
  connectEntry me (myWhiteTeamEntry me) gameInfoWhiteTeamName $ \x info ->
    info { gameInfoWhiteTeamName = x }

  connectEntry me (myRulesetEntry me) gameInfoRuleset $ \x info -> info { gameInfoRuleset = x }
  connect me
          (void . onValueSpinned (myMainTimeSpin me))
          (spinButtonGetValueAsBigfloat $ myMainTimeSpin me) $ \x info ->
    info { gameInfoBasicTimeSeconds = if x == 0 then Nothing else Just x }
  connectEntry me (myOvertimeEntry me) gameInfoOvertime $ \x info -> info { gameInfoOvertime = x }
  -- TODO Game result display checkbox.
  connectEntry me (myGameResultEntry me) gameInfoResult $ \x info -> info { gameInfoResult = x }

  connectEntry me (myGameAnnotatorEntry me) gameInfoAnnotatorName $ \x info ->
    info { gameInfoAnnotatorName = x }
  connectEntry me (myGameEntererEntry me) gameInfoEntererName $ \x info ->
    info { gameInfoEntererName = x }

  connectEntry me (myEventNameEntry me) gameInfoEvent $ \x info -> info { gameInfoEvent = x }
  connectEntry me (myGamePlaceEntry me) gameInfoPlace $ \x info -> info { gameInfoPlace = x }
  connectEntry me (myGameRoundEntry me) gameInfoRound $ \x info -> info { gameInfoRound = x }
  connectEntry me (myGameDatesEntry me) gameInfoDatesPlayed $ \x info ->
    info { gameInfoDatesPlayed = x }
  connectEntry me (myGameNameEntry me) gameInfoGameName $ \x info -> info { gameInfoGameName = x }
  connectEntry me (myGameSourceEntry me) gameInfoSource $ \x info -> info { gameInfoSource = x }
  connectEntry me (myGameCopyrightEntry me) gameInfoCopyright $ \x info ->
    info { gameInfoCopyright = x }

  connectEntry me (myGameOpeningEntry me) gameInfoOpeningComment $ \x info ->
    info { gameInfoOpeningComment = x }

-- | @connectEntry me entry getter setter@ binds an 'Entry' to a field
-- in the current 'GameInfo' so that changes in one affect the other.
-- Empty strings are coerced to 'Nothing' and vice versa.
--
-- When an 'Entry' is changed, we immediately write the value back to
-- the model: we can't wait until focus out because e.g. opening menus
-- doesn't fire focus-out events.  We also inhibit this widget's model
-- change handler assigning back to the entry, because e.g. we don't
-- want to canonicalize "B+1.0" to "B+1" as soon as it's typed.
-- Instead, we do the model-to-view canonicalizing update on
-- focus-out.
connectEntry :: (UiCtrl ui, Stringlike a)
             => GamePropertiesPanel ui
             -> Entry
             -> (GameInfo -> Maybe a)
             -> (Maybe a -> GameInfo -> GameInfo)
             -> IO ()
connectEntry me entry getter setter = do
  let ui = myUi me
  onEntryChange entry $ \value ->
    withLatchOn (myLatch me) $ runUiGo ui $ void $ modifyGameInfo $ setter $
    if null value then Nothing else Just $ stringToSgf value
  on entry focusOutEvent $ liftIO $ do
    cursor <- readCursor ui
    set entry [entryText := maybe "" sgfToString $ getter $ boardGameInfo $
               cursorBoard cursor]
    return False
  return ()

-- | @connect me connectFn getter setter@ binds a widget to a field in
-- the current 'GameInfo' so that changes in one affect the other.
-- @connect@ constructs an @IO ()@ handler for changes to the widget's
-- state, and immediately gives it to @connectFn@, which is in charge
-- of registering the handler.  @getter@ should read the current value
-- from the view.  @setter@ should update the value in a 'GameInfo'.
connect :: UiCtrl ui
        => GamePropertiesPanel ui
        -> (IO () -> IO ()) -- ^ Function to install a handler.
        -> IO a             -- ^ Getter for the widget value.
        -> (a -> GameInfo -> GameInfo)
        -> IO ()
connect me connectFn getter setter =
  let ui = myUi me
  in connectFn $ do
    newValue <- getter
    runUiGo ui $ void $ modifyGameInfo $ setter newValue

destroy :: UiCtrl ui => GamePropertiesPanel ui -> IO ()
destroy = viewUnregisterAll

updateUi :: UiCtrl ui => GamePropertiesPanel ui -> Cursor -> IO ()
updateUi me cursor = updateUiGameInfo me $ boardGameInfo $ cursorBoard cursor

updateUiGameInfo :: GamePropertiesPanel ui -> GameInfo -> IO ()
updateUiGameInfo me info = whenLatchOff (myLatch me) $ do
  forM_ [ (InfoGetter gameInfoBlackName, myBlackNameEntry)
        , (InfoGetter gameInfoBlackRank, myBlackRankEntry)
        , (InfoGetter gameInfoBlackTeamName, myBlackTeamEntry)

        , (InfoGetter gameInfoWhiteName, myWhiteNameEntry)
        , (InfoGetter gameInfoWhiteRank, myWhiteRankEntry)
        , (InfoGetter gameInfoWhiteTeamName, myWhiteTeamEntry)

        , (InfoGetter gameInfoRuleset, myRulesetEntry)
        , (InfoGetter gameInfoOvertime, myOvertimeEntry)
        , (InfoGetter gameInfoResult, myGameResultEntry)

        , (InfoGetter gameInfoAnnotatorName, myGameAnnotatorEntry)
        , (InfoGetter gameInfoEntererName, myGameEntererEntry)

        , (InfoGetter gameInfoEvent, myEventNameEntry)
        , (InfoGetter gameInfoPlace, myGamePlaceEntry)
        , (InfoGetter gameInfoRound, myGameRoundEntry)
        , (InfoGetter gameInfoDatesPlayed, myGameDatesEntry)
        , (InfoGetter gameInfoGameName, myGameNameEntry)
        , (InfoGetter gameInfoSource, myGameSourceEntry)
        , (InfoGetter gameInfoCopyright, myGameCopyrightEntry)

        , (InfoGetter gameInfoOpeningComment, myGameOpeningEntry)
        ] $ \(InfoGetter getter, entry) ->
    set (entry me) [entryText := maybe "" sgfToString $ getter info]

  spinButtonSetValue (myMainTimeSpin me) $ maybe 0 BF.toDouble $
    gameInfoBasicTimeSeconds info
  labelSetText (myMainTimeLabel me) $ maybe "" renderSeconds $
    gameInfoBasicTimeSeconds info

  myGameCommentTextViewSetter me $ maybe "" fromText $ gameInfoGameComment info

renderSeconds :: BF.Bigfloat -> String
renderSeconds totalSecondsFloat =
  let isNegative = totalSecondsFloat < 0
      (wholeSeconds, fractionalSecondsStr) = first abs $ splitFloat totalSecondsFloat
      (totalMinutes, seconds) = wholeSeconds `divMod` 60
      (hours, minutes) = totalMinutes `divMod` 60
  in (if isNegative then ('-':) else id) $
     (if hours > 0
      then show hours ++ ':' : show2 minutes ++ ':' : show2 seconds
      else show minutes ++ ':' : show2 seconds) ++
     fractionalSecondsStr
  where show2 n = if n < 10 then '0' : show n else show n

-- | Returns a pair containing the signed whole part of a 'BF.Bigfloat', plus a
-- string containing a decimal place and everything after it, if the float has a
-- fractional part, otherwise an empty string.
splitFloat :: BF.Bigfloat -> (Integer, String)
splitFloat x =
  let xs = show x
      (addNeg, xs') = case xs of
        '-':xs' -> (('-':), xs')
        _ -> (id, xs)
      (hd, tl) = break (== '.') xs'
  in (read $ addNeg hd, tl)
