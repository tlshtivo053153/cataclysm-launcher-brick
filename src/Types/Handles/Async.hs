{-# LANGUAGE RankNTypes #-}

module Types.Handles.Async (
    AsyncHandle(..)
) where

import Brick.BChan (BChan)
import Types.Event (UIEvent)

data AsyncHandle m = AsyncHandle
    { hWriteBChan          :: BChan UIEvent -> UIEvent -> m ()
    }