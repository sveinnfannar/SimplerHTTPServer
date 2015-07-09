{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty

import System.Directory
import System.FilePath.Posix
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Text.Lazy hiding(map, empty, singleton)
import Control.Applicative ((<$>))
import Prelude hiding (concat)
import Data.Map.Strict hiding(map)

import Debug.Trace


data FileInfo = FileInfo
              { fileName :: Text
              , path :: Text
              , relativePath :: Text
              }
main :: IO ()
main = scotty 3000 $ do
    get "/" $ do
        curdir <- liftIO getCurrentDirectory
        dirs <- liftIO $ listDir curdir
        either raise html $ renderIndex curdir . map (toFileInfo curdir "/")<$> dirs

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
        (True, _) -> do
            dirs <- listDir fullpath
            return $ renderIndex path . map (toFileInfo curdir fullpath) <$> dirs
        (_, True) -> getFile fullpath
        otherwise -> return $ Left "File not found"


toFileInfo :: FilePath -> FilePath -> FilePath -> FileInfo
toFileInfo root parent file = FileInfo fname path relpath
    where fname = pack file
          path = pack $ joinPath [parent, file]
          relpath = pack $ joinPath [makeRelative root parent, file]

htmlTag :: Text -> Text -> Text
htmlTag = flip htmlTagWithProps empty

htmlTagWithProps :: Text -> Map Text Text -> Text -> Text
htmlTagWithProps tag propsMap content =
    let props = foldrWithKey f "" propsMap
        f k v acc = concat [k, "=\"", v, "\" ", acc]
    in concat ["<", tag, " ", props, ">", content, "</", tag, ">"]

a href = htmlTagWithProps "a" (singleton "href" href)
th = htmlTag "th"
td = htmlTag "td"
tr = htmlTag "tr"
thead = htmlTag "thead"
tbody = htmlTag "tbody"
table = htmlTag "table"
h1 = htmlTag "h1"

mkTable :: [Text] -> [[Text]] -> Text
mkTable columns rows =
    let content = concat [thead headers, tbody allRows]
        headers = concat . map (tr . th) $ columns
        allRows = concat . map (tr . concat . map td) $ rows
    in table content

renderIndex :: FilePath -> [FileInfo] -> Text
renderIndex dir files =
    let
        header = concat ["<h1>Index of ", pack dir, "</h1>"]
        filesHtml = map (\f -> a (relativePath f) (fileName f)) files
        tbl = mkTable ["Name"] $ map (:[]) filesHtml
    in intercalate "\n"
        [ header
        , tbl
        ]

listDir :: FilePath -> IO (Either Text [FilePath])
listDir path | trace ("Called listdir " ++ path) False = undefined
listDir path = do
    exists <- doesDirectoryExist path
    if exists then do
        files <- getDirectoryContents path
        return $ Right files
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
