{-# LANGUAGE RankNTypes #-}

module Types.Handles.Http (
    HttpHandle(..)
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Time (UTCTime)
import Types.Error (ManagerError)

data HttpHandle m = HttpHandle
    { hDownloadAsset        :: T.Text -> m (Either ManagerError B.ByteString)
    , hDownloadFile         :: T.Text -> m (Either ManagerError L.ByteString)
    , hFetchReleasesFromAPI :: String -> Maybe UTCTime -> m (Either String L.ByteString)
    }