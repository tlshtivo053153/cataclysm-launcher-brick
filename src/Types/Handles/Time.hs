{-# LANGUAGE RankNTypes #-}

module Types.Handles.Time (
    TimeHandle(..)
) where

import Data.Time (UTCTime)

data TimeHandle m = TimeHandle
    { hGetCurrentTime      :: m UTCTime
    }