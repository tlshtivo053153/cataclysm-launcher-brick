{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module FontManager (
    installFont,
    configureSandboxForFont,
    linkFontsDirToSandbox,
    listInstalledFonts
) where

import Control.Exception (SomeException)
import Control.Monad (filterM, forM_, when)
import Control.Monad.Catch (MonadCatch, try)
import qualified Data.ByteString.Lazy as LBS ()
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE ()
import System.FilePath ((</>), takeFileName, takeDirectory)
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
        
    let fontsDir = T.unpack (launcherRoot pathsConfig) </> "fonts"
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
                downloadPath = T.unpack (downloadCache pathsConfig) </> fileName
                
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

-- | Links the global fonts directory to the sandbox's font directory.
-- Creates a symlink: sandbox/font -> .cataclysm-launcher-brick/fonts
linkFontsDirToSandbox :: (MonadCatch m)
                      => AppHandle m
                      -> SandboxProfile
                      -> PathsConfig
                      -> m (Either ManagerError ())
linkFontsDirToSandbox handle profile pathsConfig = do
    let fs = appFileSystemHandle handle
    let sandboxDir = spDataDirectory profile
    let globalFontsDir = T.unpack (launcherRoot pathsConfig) </> "fonts"
    let sandboxFontLink = sandboxDir </> "font"

    -- Ensure global fonts directory exists
    hCreateDirectoryIfMissing fs True globalFontsDir
    
    -- Convert to absolute path for symlink target
    absGlobalFontsDir <- hMakeAbsolute fs globalFontsDir

    -- Check if sandbox/font exists
    -- Use try because pathIsSymbolicLink throws an exception if the path doesn't exist
    isSymlinkEither <- try $ hDoesSymbolicLinkExist fs sandboxFontLink
    let isSymlink = case isSymlinkEither of
                         Left (_ :: SomeException) -> False
                         Right b -> b
    
    when isSymlink $ hRemoveFile fs sandboxFontLink
    
    -- Re-check existence after potential symlink removal
    stillExists <- hDoesDirectoryExist fs sandboxFontLink
    when stillExists $ do
        -- It's a real directory. 
        -- Copy its contents to globalFontsDir to preserve default fonts.
        -- We only copy files, assuming flat structure for fonts or simple recurrence?
        -- Game fonts usually are just .ttf or .txt files in font/ directory.
        contents <- hListDirectory fs sandboxFontLink
        forM_ contents $ \item -> do
            let srcPath = sandboxFontLink </> item
            let destPath = globalFontsDir </> item
            -- Check if it is a file
            isFile <- hDoesFileExist fs srcPath
            when isFile $ do
                 -- Read and write to global
                 -- We don't overwrite if exists? Or do we?
                 -- Let's overwrite to ensure we have valid files.
                 fileContent <- hReadFile fs srcPath
                 hWriteFile fs destPath fileContent
        
        -- After copying, remove the directory
        hRemoveDirectoryRecursive fs sandboxFontLink

    hCreateSymbolicLink fs absGlobalFontsDir sandboxFontLink
    return $ Right ()

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
    -- Find a suitable font file in the installed directory
    -- Since we use directory linking, the font should be accessible at sandbox/font/<fontName>
    -- We need to find the specific .ttf/.otf file relative to the sandbox root.
    
    -- The path where the font is physically located (global repo)
    let globalFontDir = installedFontPath installedFont
    
    -- We assume the relative path in sandbox is font/<fontName>
    -- because sandbox/font -> global/fonts
    -- and global/fonts/<fontName> is where files are.
    
    -- Find a suitable font file in the installed directory
    -- We assume the first .ttf file found is the one we want.
    files <- hListDirectory fs globalFontDir
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
    
    return $ Right ()

findFontFile :: [FilePath] -> Maybe FilePath
findFontFile [] = Nothing
findFontFile (f:fs)
    | ".ttf" `T.isSuffixOf` T.pack f = Just f
    | ".otf" `T.isSuffixOf` T.pack f = Just f
    | otherwise = findFontFile fs

-- | Lists all fonts installed in the global fonts directory.
listInstalledFonts :: MonadCatch m => AppHandle m -> PathsConfig -> m [InstalledFont]
listInstalledFonts handle pathsConfig = do
    let fs = appFileSystemHandle handle
    let fontsDir = T.unpack (launcherRoot pathsConfig) </> "fonts"
    
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
