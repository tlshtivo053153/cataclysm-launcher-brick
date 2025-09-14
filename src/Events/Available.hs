module Events.Available (handleAvailableEvents, getDownloadAction) where

import Brick
import Brick.Widgets.List (listSelectedElement)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V

import Events.List (handleListEvents)
import qualified GameManager as GM
import Types

-- | Pure function to determine the IO action for a download.
getDownloadAction :: AppState -> Maybe (IO ())
getDownloadAction st =
  case listSelectedElement (appAvailableVersions st) of
    Nothing -> Nothing
    Just (_, gv) -> Just $ do
      let chan = appEventChannel st
          h = appHandle st
          cfg = appConfig st
      result <- GM.downloadAndInstall h cfg chan gv
      hWriteBChan h chan $ InstallFinished result

-- | Event handler for the available versions list.
handleAvailableEvents :: V.Event -> EventM Name AppState ()
handleAvailableEvents (V.EvKey V.KEnter []) = do
    st <- get
    case getDownloadAction st of
        Nothing -> return ()
        Just action -> liftIO $ void $ forkIO action
handleAvailableEvents ev = handleListEvents ev AvailableList