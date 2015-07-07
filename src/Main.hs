{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import Control.Monad.IO.Class
import System.Directory
import Data.Monoid (mconcat)
import Data.Text.Lazy hiding(map)
import System.FilePath.Posix
import Control.Conditional
import Control.Monad


main = scotty 3000 $ do
  get "/" $ do
    files <- liftIO $ getCurrentDirectory >>= getDirectoryContents
    html $ intercalate "\n" $ map pack files

  get "/:path" $ do
    path <- param "path"
    currDir <- liftIO getCurrentDirectory
    let fullPath = joinPath [currDir, path]
    isDir <- liftIO $ doesDirectoryExist fullPath
    isFile <- liftIO $ doesFileExist fullPath
    if isDir then do
      files <- liftIO $ getDirectoryContents fullPath
      html $ intercalate "\n" $ map pack files
    else do
      content <- liftIO $ readFile fullPath
      html $ pack content 
