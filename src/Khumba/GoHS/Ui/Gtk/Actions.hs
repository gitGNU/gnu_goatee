module Khumba.GoHS.Ui.Gtk.Actions ( Actions
                                  , create
                                  , myToolActions
                                  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Graphics.UI.Gtk
import Khumba.GoHS.Ui.Gtk.Common

data Actions = Actions { myToolActions :: ActionGroup }

create :: UiCtrl ui => UiRef ui -> IO Actions
create uiRef = do
  let tools = enumFrom minBound

  -- Tool actions
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

  return Actions { myToolActions = toolActions }
