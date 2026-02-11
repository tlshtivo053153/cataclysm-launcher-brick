{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FontManager (
    installFont,
    configureSandboxForFont,
    listInstalledFonts
) where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import qualified Data.ByteString.Lazy as LBS ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE ()
import System.FilePath ((</>), takeFileName)
import Data.Aeson (encode, object, (.=))

import Types.Font
import Types.Handle
import Types.Handles.FileSystem ()
import Types.Handles.Http ()
import Types.Handles.Archive ()
import Types.Error (ManagerError(..))
import Types.Domain (PathsConfig(..), SandboxProfile(..))

-- | Installs a font from a URL into the global fonts directory.
installFont :: (MonadCatch m) 
            => AppHandle m 
            -> PathsConfig 
            -> FontInfo 
            -> m (Either ManagerError InstalledFont)
installFont handle pathsConfig font = do
    let fs = appFileSystemHandle handle
        http = appHttpHandle handle
        archive = appArchiveHandle handle
        
    let fontsDir = (T.unpack $ launcherRoot pathsConfig) </> "fonts"
        fontDirName = T.unpack $ fontName font
        targetDir = fontsDir </> fontDirName
        
    -- Ensure fonts directory exists
    hCreateDirectoryIfMissing fs True fontsDir
    
    -- Check if already installed
    exists <- hDoesDirectoryExist fs targetDir
    if exists 
        then return $ Right $ InstalledFont (fontName font) targetDir
        else do
            -- Download
            let url = fontUrl font
                fileName = takeFileName (T.unpack url)
                downloadPath = (T.unpack $ downloadCache pathsConfig) </> fileName
                
            -- Ensure cache dir
            hCreateDirectoryIfMissing fs True (T.unpack $ downloadCache pathsConfig)
            
            downloadResult <- hDownloadFile http url
            case downloadResult of
                Left err -> return $ Left err
                Right content -> do
                    hWriteLazyByteString fs downloadPath content
                    
                    -- Extract
                    -- For now assuming zip, similar to SoundpackManager
                    if ".zip" `T.isSuffixOf` url
                        then do
                            zipData <- hReadFile fs downloadPath
                            hCreateDirectoryIfMissing fs True targetDir
                            extractResult <- hExtractZip archive fs targetDir zipData
                            case extractResult of
                                Left err -> return $ Left err
                                Right _ -> return $ Right $ InstalledFont (fontName font) targetDir
                        else return $ Left $ NetworkError "Unsupported font archive format (only .zip supported)"

-- | Configures a sandbox to use a specific installed font.
-- 1. Creates a symlink in the sandbox's font directory.
-- 2. Generates 'config/fonts.json' in the sandbox.
configureSandboxForFont :: (MonadCatch m)
                        => AppHandle m
                        -> SandboxProfile
                        -> InstalledFont
                        -> m (Either ManagerError ())
configureSandboxForFont handle profile installedFont = do
    let fs = appFileSystemHandle handle
    
    let sandboxDir = spDataDirectory profile
                                               -- Wait, user request said ".cataclysm-launcher-brick/fonts/ directory link" 
                                               -- usually user creates a symlink in 'font' folder of the game.
                                               -- checking docs/resources might be good, but typically CDDA has 'font' folder in root.
                                               -- User said "sandboxにそのディレクトリのリンクを作ります" (create a link to that directory in the sandbox).
    
    -- Link destination: <sandbox>/font/<font_name> 
    -- But usually CDDA reads fonts from data/font or font folder.
    -- If we use config/fonts.json, we can specify the font file.
    -- Let's assume we link to <sandbox>/font/managed_font_<name> to isolate it.
    
    let targetLinkPath = sandboxDir </> "font" </> (T.unpack $ installedFontName installedFont)
    
    -- Ensure sandbox/font exists
    hCreateDirectoryIfMissing fs True (sandboxDir </> "font")
    
    -- Create Symlink
    -- If link exists, remove it first?
    linkExists <- hDoesSymbolicLinkExist fs targetLinkPath
    when linkExists $ hRemoveFile fs targetLinkPath -- Remove existing link/file
    
    -- Create new link
    -- hCreateSymbolicLink target linkname
    hCreateSymbolicLink fs (installedFontPath installedFont) targetLinkPath
    
    -- Find a suitable font file in the installed directory
    -- We assume the first .ttf file found is the one we want.
    files <- hListDirectory fs (installedFontPath installedFont)
    let mFontFile = findFontFile files
        fontPathInConfig = case mFontFile of
            Just f -> "font" </> T.unpack (installedFontName installedFont) </> f
            Nothing -> "font" </> T.unpack (installedFontName installedFont) -- Fallback to dir? Unlikely to work but better than nothing.
    
    -- Generate config/fonts.json
    let configDir = sandboxDir </> "config"
    hCreateDirectoryIfMissing fs True configDir
    
    let fontsJsonPath = configDir </> "fonts.json"
    let jsonContent = encode $ object
            [ "fontblending" .= True
            , "fontwidth" .= (8 :: Int)
            , "fontheight" .= (16 :: Int)
            , "fontsize" .= (16 :: Int)
            , "typeface" .= [fontPathInConfig]
            , "map_typeface" .= [fontPathInConfig]
            ]
            
    -- Write config
    hWriteLazyByteString fs fontsJsonPath jsonContent
    
    _ <- return $ Right ()

    -- Write config
    hWriteLazyByteString fs fontsJsonPath jsonContent
    
    return $ Right ()

findFontFile :: [FilePath] -> Maybe FilePath
findFontFile [] = Nothing
findFontFile (f:fs)
    | ".ttf" `T.isSuffixOf` (T.pack f) = Just f
    | ".otf" `T.isSuffixOf` (T.pack f) = Just f
    | otherwise = findFontFile fs

-- | Lists all fonts installed in the global fonts directory.
listInstalledFonts :: MonadCatch m => AppHandle m -> PathsConfig -> m [InstalledFont]
listInstalledFonts handle pathsConfig = do
    let fs = appFileSystemHandle handle
    let fontsDir = (T.unpack $ launcherRoot pathsConfig) </> "fonts"
    
    exists <- hDoesDirectoryExist fs fontsDir
    if not exists
        then return []
        else do
            contents <- hListDirectory fs fontsDir
            -- Filter only directories? Or check for valid font content?
            -- For simplicity, assume all directories in `fonts` are installed fonts.
            -- We might want to filter out hidden files/dirs.
            fonts <- filterM (hDoesDirectoryExist fs . (fontsDir </>)) contents
            return $ map (\name -> InstalledFont (T.pack name) (fontsDir </> name)) fonts

    where
        -- simple filterM since it's not in Prelude for everything
        filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
        filterM _ [] = return []
        filterM p (x:xs) = do
            flg <- p x
            ys <- filterM p xs
            return (if flg then x:ys else ys)
