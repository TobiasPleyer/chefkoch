module Chefkoch.Http where


import qualified Data.Text as T
import Network.HTTP
import System.Process
import System.IO
import Chefkoch.DataTypes
import Chefkoch.DataFunctions


openURL :: String -> IO T.Text
openURL url = fmap T.pack $ getResponseBody =<< simpleHTTP (getRequest url)


wgetURL :: String -> IO T.Text
wgetURL url = do
    let
      wgetPath = "/usr/bin/wget"
      wgetArgs = [url, "-qO-"]
    htmlString <- readProcess wgetPath wgetArgs ""
    return (T.pack htmlString)


fileURL :: String -> IO T.Text
fileURL f = do
    filecontent <- readFile f
    return (T.pack filecontent)


fetchRecipeOverview :: (String -> IO T.Text) -> Year -> Month -> IO T.Text
fetchRecipeOverview grabber year month = do
    let url = ("https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year)
    grabber url
