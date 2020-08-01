module Chefkoch.Http
  ( Grabber (..),
    download,
    downloadAndTag,
    downloadWith,
  )
where

import Chefkoch.DataFunctions
import Chefkoch.DataTypes
import Chefkoch.Html.Parser
import Chefkoch.Util
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HTTP
import Network.Wreq
import System.IO
import System.Process

data Grabber
  = SimpleHTTP
  | WGet
  | WReq
  | File
  deriving (Eq, Ord, Show)

type GrabberFunc = String -> IO Text

openURL :: GrabberFunc
openURL url = fmap T.pack $ getResponseBody =<< simpleHTTP (getRequest url)

wgetURL :: GrabberFunc
wgetURL url = do
  let wgetPath = "/usr/bin/wget"
      wgetArgs = [url, "-qO-"]
  htmlString <- readProcess wgetPath wgetArgs ""
  return (T.pack htmlString)

wreqURL :: GrabberFunc
wreqURL url = TL.toStrict . TL.decodeUtf8 . flip (^.) responseBody <$> get url

fileURL :: GrabberFunc
fileURL = TIO.readFile

grabberFunc :: Grabber -> GrabberFunc
grabberFunc SimpleHTTP = openURL
grabberFunc WGet = wgetURL
grabberFunc WReq = wreqURL
grabberFunc File = fileURL

downloadUrl :: GrabberFunc -> String -> IO Text
downloadUrl f url = do
  sayLoud $ "Downloading URL: " ++ url
  f url

download :: [String] -> IO [Text]
download = downloadWith WReq

downloadAndTag :: [String] -> IO [(String, Text)]
downloadAndTag urls = zip urls <$> downloadWith WReq urls

downloadWith :: Grabber -> [String] -> IO [Text]
downloadWith _ [] = return []
downloadWith grabber [url] = (: []) <$> downloadUrl (grabberFunc grabber) url
downloadWith grabber urls = do
  sayLoud "Starting parallel download..."
  mapConcurrently (downloadUrl (grabberFunc grabber)) urls
