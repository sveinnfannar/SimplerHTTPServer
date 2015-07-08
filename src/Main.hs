{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import System.Directory
import System.FilePath.Posix
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy hiding(map)

import Debug.Trace

main :: IO ()
main = scotty 3000 $ do
    get "/" $ liftIO (getCurrentDirectory >>= listDir) >>= either raise html

    get "/:path" $ do
        path <- param "path"
        liftIO (getPath path) >>= either raise html


getPath :: FilePath -> IO (Either Text Text)
getPath path | trace ("Called getPath " ++ path) False = undefined
getPath path = do
    curdir <- getCurrentDirectory
    let fullpath = joinPath [curdir, path]
    liftIO $ putStrLn fullpath
    isDir <- doesDirectoryExist fullpath
    isFile <- doesFileExist fullpath
    case (isDir, isFile) of
        (True, _) -> listDir fullpath
        (_, True) -> getFile fullpath
        otherwise -> return $ Left "File not found"

listDir :: FilePath -> IO (Either Text Text)
listDir path | trace ("Called listdir " ++ path) False = undefined
listDir path = do
    exists <- doesDirectoryExist path
    if exists then do
        files <- getDirectoryContents path
        return $ Right $ intercalate "\n" $ map pack files
    else
        return $ Left "No such directory"

getFile :: FilePath -> IO (Either Text Text)
getFile path = do
    exists <- doesFileExist path
    if exists then do
        content <- readFile path
        return $ Right $ pack content
    else
        return $ Left "No such file"
