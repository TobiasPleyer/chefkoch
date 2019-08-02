{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception.Base                (bracket)
import           Control.Monad
import           Control.Monad.IO.Class                (MonadIO (..))
import qualified Data.ByteString.Char8                 as BC
import           Data.Either                           (partitionEithers)
import           Data.List                             (partition)
import qualified Data.Text.IO                          as TIO
import           Debug.Trace
import           Options.Applicative
import           System.Console.CmdArgs.Verbosity
import           System.IO                             (IOMode (..), hClose,
                                                        hPutStrLn, openFile)
import           Text.HTML.TagSoup
import qualified Text.Megaparsec                       as M

import           Chefkoch.CmdLine
import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes
import           Chefkoch.Format
import           Chefkoch.Html.Megaparsec
import           Chefkoch.Html.Parser
import           Chefkoch.Http
import           Chefkoch.Util

import qualified Database.Couch.Explicit.Configuration as DbC
import qualified Database.Couch.Explicit.Database      as DbDb
import qualified Database.Couch.Explicit.Design        as DbDes
import qualified Database.Couch.Explicit.Server        as DbS
import qualified Database.Couch.Types                  as DbT

import qualified Data.Aeson                            as A
import qualified Network.HTTP.Client                   as Http


run :: Options -> IO [Recipe]
run opts@Options{..} = do
    setVerbosity Quiet
    sayLoud "Starting execution"
    sayLoud $ "Options: " ++ show opts
    let month = fmap unsafeInt2Month optionMonth
    sayLoud $ "Month: " ++ show month
    if optionRandom
     then do
       sayLoud "Choosing at random..."
       (year,month,day) <- getRandomYearMonthDay
       wreqDownloadRecipesByDate optionUrlsOnly (Just year, Just month, Just day)
     else case optionUrl of
       Just url -> do
         sayLoud $ "Using URL: " ++ url
         wreqDownloadRecipesByUrl [url]
       Nothing -> do
         sayLoud $ "Using (year,month,day): " ++ show (optionYear, month, optionDay)
         wreqDownloadRecipesByDate optionUrlsOnly (optionYear, month, optionDay)


defaultOptions = Options
  { optionYear = Nothing
  , optionMonth = Nothing
  , optionDay = Nothing
  , optionUrl = Nothing
  , optionUrlsOnly = False
  , optionRandom = False
  , optionOutput = "-"
  , optionFormat = "json"
  }


main = do
    manager <- Http.newManager Http.defaultManagerSettings
    let ctx = DbT.Context
              { ctxManager = manager
              , ctxHost = DbT.Host "127.0.0.1"
              , ctxPort = DbT.Port 5984
              , ctxCred = Just DbT.Basic
                                 { credUser = DbT.User "tobple"
                                 , credPass = DbT.Password "foobar" }
              , ctxCookies = Http.createCookieJar []
              , ctxDb = Just (DbT.Db "recipes")
              }
    --putStrLn "Database does not exist - will create it now..."
    --c :: DbT.Result A.Value <- DbDb.create ctx
    --print c
    putStrLn "Starting download of recipes..."
    forM_ [2018,2019] $ \year -> do
      putStrLn $ "year: " ++ show year
      forM_ [1..12] $ \month -> do
        putStrLn $ "  month: " ++ show month
        let options = defaultOptions { optionYear = Just year
                                     , optionMonth = Just month }
        recipes <- run options
        forM_ recipes $ \recipe -> do
          putStrLn $ "    Uploading " ++ recipeName recipe
          r :: DbT.Result A.Value <- DbDb.createDoc False recipe ctx
          return ()
    putStrLn "Done"
