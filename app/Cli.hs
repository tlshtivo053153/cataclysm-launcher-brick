{-# LANGUAGE OverloadedStrings #-}

module Cli
    ( Options(..)
    , parseOptions
    ) where

import Options.Applicative

-- | Command line options
data Options = Options
    { optDebug :: Bool  -- ^ Enable debug logging
    }
    deriving (Show, Eq)

-- | Parse command line options
parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
        ( fullDesc
        <> progDesc "Cataclysm: Dark Days Ahead Launcher"
        <> header "cataclysm-launcher - A TUI launcher for CDDA" )

-- | Options parser
optionsParser :: Parser Options
optionsParser = Options
    <$> switch
        ( long "debug"
        <> short 'd'
        <> help "Enable debug logging to ~/.local/share/cataclysm-launcher/debug.log" )
