-- | GTK+ 'Action' definitions.
module Khumba.Goatee.Ui.Gtk.Actions (
  Actions
  , create
  , initialize
  , myFileNewAction
  , myFileOpenAction
  , myToolActions
  ) where

import Control.Monad
import Data.Maybe
import Graphics.UI.Gtk
import Khumba.Goatee.Ui.Gtk.Common

data Actions = Actions { myFileNewAction :: Action
                       , myFileOpenAction :: Action
                       , myToolActions :: ActionGroup
                       }

create :: UiCtrl ui => UiRef ui -> IO Actions
create uiRef = do
  let tools = enumFrom minBound

  -- File actions.
  fileActions <- actionGroupNew "File"

  -- TODO Accelerators aren't working.
  fileNewAction <- actionNew "FileNew" "New file" Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileNewAction $ Just "<Control>n"
  on fileNewAction actionActivated $ do
    ui <- readUiRef uiRef
    void $ openNewBoard ui Nothing

  fileOpenAction <- actionNew "FileOpen" "Open file..." Nothing Nothing
  actionGroupAddActionWithAccel fileActions fileOpenAction $ Just "<Control>o"
  on fileOpenAction actionActivated $ do
    ui <- readUiRef uiRef
    dialog <- fileChooserDialogNew (Just "Open a file")
                                   Nothing
                                   FileChooserActionOpen
                                   [(stockOk, ResponseOk),
                                    (stockCancel, ResponseCancel)]
    mapM_ (fileChooserAddFilter dialog) =<< fileFiltersForSgf
    response <- dialogRun dialog
    widgetHide dialog
    when (response == ResponseOk) $ do
      maybePath <- fileChooserGetFilename dialog
      when (isJust maybePath) $ do
        let path = fromJust maybePath
        loadResult <- openFile ui path
        case loadResult of
          Left parseError -> do
            errorDialog <- messageDialogNew
                           Nothing
                           []
                           MessageError
                           ButtonsOk
                           ("Error loading " ++ path ++ ".\n\n" ++ show parseError)
            dialogRun errorDialog
            widgetDestroy errorDialog
          Right _ -> return ()
    widgetDestroy dialog

  -- Tool actions.
  toolActions <- actionGroupNew "Tools"
  actionGroupAddRadioActions toolActions
    (flip map tools $ \tool ->
      RadioActionEntry { radioActionName = show tool
                       , radioActionLabel = toolLabel tool
                       , radioActionStockId = Nothing
                       , radioActionAccelerator = Nothing
                       , radioActionTooltip = Nothing
                       , radioActionValue = fromEnum tool
                       })
    (fromEnum initialTool)
    (\radioAction -> do ui <- readUiRef uiRef
                        setTool ui =<< fmap toEnum (radioActionGetCurrentValue radioAction))

  return Actions { myFileNewAction = fileNewAction
                 , myFileOpenAction = fileOpenAction
                 , myToolActions = toolActions
                 }

initialize :: Actions -> IO ()
initialize actions =
  -- Activate 'initialTool' (requires the controller, so we can't do it in the
  -- construction phase).
  actionActivate =<<
    fmap (fromMaybe $ error $ "Could not find the initial tool " ++ show initialTool ++ ".")
         (actionGroupGetAction (myToolActions actions) $ show initialTool)
