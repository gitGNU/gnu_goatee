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
import Data.Maybe (fromMaybe)
import qualified Game.Goatee.Common.Bigfloat as BF
import Game.Goatee.Lib.Board
import Game.Goatee.Lib.Monad hiding (on)
import Game.Goatee.Lib.Types
import Game.Goatee.Ui.Gtk.Common
import Game.Goatee.Ui.Gtk.Utils
import Game.Goatee.Ui.Gtk.Widget
import Graphics.UI.Gtk (
  AttachOptions (Expand, Fill),
  AttrOp ((:=)),
  CheckButton,
  Label,
  Packing (PackGrow, PackNatural),
  PolicyType (PolicyAutomatic, PolicyNever),
  TextView,
  Widget,
  WidgetClass,
  boxPackStart,
  checkButtonNewWithLabel,
  containerAdd,
  entryWidthChars,
  focusOutEvent,
  get,
  hBoxNew,
  hSeparatorNew,
  labelNew, labelSetMnemonicWidget, labelSetText,
  on,
  scrolledWindowAddWithViewport, scrolledWindowNew, scrolledWindowSetPolicy,
  set,
  spinButtonSetDigits,
  tableAttach, tableAttachDefaults, tableNew, tableSetRowSpacing,
  textViewNew,
  toggleButtonActive, toggled,
  toWidget,
  vBoxNew,
  widgetSetSensitive, widgetSetSizeRequest,
  )

-- A getter for 'Stringlike' 'GameInfo' fields.
data InfoGetter = forall a. Stringlike a => InfoGetter (GameInfo -> Maybe a)

data GamePropertiesPanel ui = GamePropertiesPanel
  { myUi :: ui
  , myState :: ViewState
  , myWidget :: Widget

    -- Black's info.
  , myBlackNameEntry :: GoateeEntry
  , myBlackRankEntry :: GoateeEntry
  , myBlackTeamEntry :: GoateeEntry

    -- White's info.
  , myWhiteNameEntry :: GoateeEntry
  , myWhiteRankEntry :: GoateeEntry
  , myWhiteTeamEntry :: GoateeEntry

    -- Game rules.
  , myRulesetEntry :: GoateeEntry
  , myMainTimeSpin :: GoateeSpinButton
  , myMainTimeLabel :: Label
  , myOvertimeEntry :: GoateeEntry
  , myGameResultDisplayCheck :: CheckButton
  , myGameResultEntry :: GoateeEntry

    -- Game editors.
  , myGameAnnotatorEntry :: GoateeEntry
  , myGameEntererEntry :: GoateeEntry

    -- Game context.
  , myEventNameEntry :: GoateeEntry
  , myGamePlaceEntry :: GoateeEntry
  , myGameRoundEntry :: GoateeEntry
  , myGameDatesEntry :: GoateeEntry
  , myGameNameEntry :: GoateeEntry
  , myGameSourceEntry :: GoateeEntry
  , myGameCopyrightEntry :: GoateeEntry

    -- Further commentary.
  , myGameOpeningEntry :: GoateeEntry
  , myGameCommentTextView :: TextView
  , myGameCommentTextViewSetter :: String -> IO ()
  }

instance UiCtrl go ui => UiView go ui (GamePropertiesPanel ui) where
  viewName = const "GamePropertiesPanel"
  viewCtrl = myUi
  viewState = myState
  viewUpdate = update

create :: UiCtrl go ui => ui -> IO (GamePropertiesPanel ui)
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
      addWidget :: WidgetClass widget => String -> widget -> IO widget
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
      addWideWidget :: WidgetClass widget => widget -> IO widget
      addWideWidget widget = do
        row <- nextRow
        widgetSetSizeRequest widget 0 (-1)
        tableAttachDefaults table widget 0 2 row (row + 1)
        return widget
      addEntry :: String -> IO GoateeEntry
      addEntry labelText = do
        entry <- goateeEntryNew
        let widget = goateeEntryWidget entry
        set widget [entryWidthChars := 0]
        addWidget labelText widget
        return entry

  blackNameEntry <- addEntry "Black"
  blackRankEntry <- addEntry "Rank"
  blackTeamEntry <- addEntry "Team"
  addSeparator
  whiteNameEntry <- addEntry "White"
  whiteRankEntry <- addEntry "Rank"
  whiteTeamEntry <- addEntry "Team"
  addSeparator
  rulesetEntry <- addEntry "Ruleset"

  mainTimeBox <- hBoxNew True 0
  mainTimeSpin <- goateeSpinButtonNewWithRange 0 3155692600 {- 100 years -} 1
  let mainTimeSpinWidget = goateeSpinButtonWidget mainTimeSpin
  spinButtonSetDigits mainTimeSpinWidget 1
  mainTimeLabel <- labelNew Nothing
  boxPackStart mainTimeBox mainTimeSpinWidget PackGrow 0
  boxPackStart mainTimeBox mainTimeLabel PackNatural 0
  addWidget "Time" mainTimeBox

  overtimeEntry <- addEntry "Overtime"
  gameResultDisplayCheck <- addWideWidget =<< checkButtonNewWithLabel "Show game result"
  gameResultEntry <- addEntry "Result"
  addSeparator
  gameAnnotatorEntry <- addEntry "Annotator"
  gameEntererEntry <- addEntry "Enterer"
  addSeparator
  eventNameEntry <- addEntry "Event"
  gamePlaceEntry <- addEntry "Place"
  gameRoundEntry <- addEntry "Round"
  gameDatesEntry <- addEntry "Dates"
  gameNameEntry <- addEntry "Name"
  gameSourceEntry <- addEntry "Source"
  gameCopyrightEntry <- addEntry "Copyright"
  addSeparator
  gameOpeningEntry <- addEntry "Opening"
  addWideWidget =<< labelNew (Just "Game comment:")

  gameCommentScroll <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scroll PolicyAutomatic PolicyAutomatic
  gameCommentTextView <- textViewNew
  -- Set a minimum height on the text view, otherwise it's too short.
  widgetSetSizeRequest gameCommentTextView 0 175
  containerAdd gameCommentScroll gameCommentTextView
  boxPackStart box gameCommentScroll PackNatural 0

  gameCommentTextViewSetter <- textViewConfigure gameCommentTextView $ \value ->
    doUiGo ui $ void $ modifyGameInfo $ \info ->
    info { gameInfoGameComment = if null value then Nothing else Just $ stringToSgf value }

  addedRowCount <- readIORef nextRowRef
  when (addedRowCount /= rows) $ fail $
    "GamePropertiesPanel: Table expected " ++ show rows ++ " rows, got " ++
    show addedRowCount ++ "."

  state <- viewStateNew

  let me = GamePropertiesPanel {
        myUi = ui
        , myState = state
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

initialize :: UiCtrl go ui => GamePropertiesPanel ui -> IO ()
initialize me = do
  register me [AnyEvent gameInfoChangedEvent]

  viewUpdate me

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
  connect me (goateeSpinButtonOnSpinned $ myMainTimeSpin me) $ \x info ->
    info { gameInfoBasicTimeSeconds = if x == 0 then Nothing else Just x }
  connectEntry me (myOvertimeEntry me) gameInfoOvertime $ \x info -> info { gameInfoOvertime = x }
  let gameResultCheck = myGameResultDisplayCheck me
  on gameResultCheck toggled $ viewUpdate me
  connectEntry' me (myGameResultEntry me) (get gameResultCheck toggleButtonActive) gameInfoResult $
    \x info -> info { gameInfoResult = x }

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
connectEntry :: (UiCtrl go ui, Stringlike a)
             => GamePropertiesPanel ui
             -> GoateeEntry
             -> (GameInfo -> Maybe a)
             -> (Maybe a -> GameInfo -> GameInfo)
             -> IO ()
connectEntry me entry = connectEntry' me entry (return True)

-- | This is like 'connectEntry'.  The additional @IO Bool@ can return false to
-- indicate that a change in the entry should not be written to the model;
-- returning true writes the change.
connectEntry' :: (UiCtrl go ui, Stringlike a)
              => GamePropertiesPanel ui
              -> GoateeEntry
              -> IO Bool
              -> (GameInfo -> Maybe a)
              -> (Maybe a -> GameInfo -> GameInfo)
              -> IO ()
connectEntry' me entry propagateChangesToModel modelGetter modelSetter = do
  let ui = myUi me
  goateeEntryOnChange entry $ \value -> do
    propagate <- propagateChangesToModel
    when propagate $ doUiGo ui $ void $ modifyGameInfo $ modelSetter $
      if null value then Nothing else Just $ stringToSgf value
  on (goateeEntryWidget entry) focusOutEvent $ liftIO $ do
    cursor <- readCursor ui
    goateeEntrySetText entry $ maybe "" sgfToString $ modelGetter $
      boardGameInfo $ cursorBoard cursor
    return False
  return ()

-- | @connect me connectFn setter@ binds a widget to a field in the current
-- 'GameInfo' so that changes in one affect the other.  @connect@ constructs an
-- @a -> IO ()@ handler that takes a value and puts it into the model using
-- @setter@, and immediately gives it to @connectFn@, which is in charge of
-- registering the handler.
connect :: UiCtrl go ui
        => GamePropertiesPanel ui
        -> ((a -> IO ()) -> IO ())
        -> (a -> GameInfo -> GameInfo)
        -> IO ()
connect me connectFn modelSetter =
  let ui = myUi me
  in connectFn $ \newValue -> doUiGo ui $ void $ modifyGameInfo $ modelSetter newValue

destroy :: UiCtrl go ui => GamePropertiesPanel ui -> IO ()
destroy = viewDestroy

update :: UiCtrl go ui => GamePropertiesPanel ui -> IO ()
update me = do
  cursor <- readCursor $ myUi me
  let info = boardGameInfo $ cursorBoard cursor
  forM_ [ (InfoGetter gameInfoBlackName, myBlackNameEntry)
        , (InfoGetter gameInfoBlackRank, myBlackRankEntry)
        , (InfoGetter gameInfoBlackTeamName, myBlackTeamEntry)

        , (InfoGetter gameInfoWhiteName, myWhiteNameEntry)
        , (InfoGetter gameInfoWhiteRank, myWhiteRankEntry)
        , (InfoGetter gameInfoWhiteTeamName, myWhiteTeamEntry)

        , (InfoGetter gameInfoRuleset, myRulesetEntry)
        , (InfoGetter gameInfoOvertime, myOvertimeEntry)

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
    goateeEntrySetText (entry me) $ extractStringlike $ getter info

  -- Update the "main time" spinner.
  goateeSpinButtonSetValue (myMainTimeSpin me) $ fromMaybe 0 $
    gameInfoBasicTimeSeconds info
  labelSetText (myMainTimeLabel me) $ maybe "" renderSeconds $
    gameInfoBasicTimeSeconds info

  -- Update the game result entry.
  let gameResultEntry = myGameResultEntry me
  displayGameResult <- get (myGameResultDisplayCheck me) toggleButtonActive
  widgetSetSensitive (goateeEntryWidget gameResultEntry) displayGameResult
  goateeEntrySetText gameResultEntry $
    if displayGameResult
    then extractStringlike $ gameInfoResult info
    else "(hidden)"

  -- Update the game comment TextView.
  myGameCommentTextViewSetter me $ maybe "" fromText $ gameInfoGameComment info

extractStringlike :: Stringlike a => Maybe a -> String
extractStringlike = maybe "" sgfToString

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
