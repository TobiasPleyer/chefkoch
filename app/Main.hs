{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception.Base                (bracket)
import           Control.Monad
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


run :: Options -> IO ()
run opts@Options{..} = do
    setVerbosity Quiet
    sayLoud "Starting execution"
    sayLoud $ "Options: " ++ show opts
    let month = fmap unsafeInt2Month optionMonth
    sayLoud $ "Month: " ++ show month
    recipes <- if optionRandom
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
    sayLoud $ "Found " ++ show (length recipes) ++ " recipes"
    unless (null recipes) $ do
      let maybeFormatter = lookup optionFormat formatterMap
      formatter <- case maybeFormatter of
                   Just fm -> return fm
                   Nothing -> do
                     putStrLn ("Unknown format '" ++ optionFormat ++ "', defaulting to 'raw'")
                     return rawFormatter
      let formattedRecipes = formatter recipes
      if optionOutput == "-"
      then BC.putStrLn formattedRecipes
      else BC.writeFile optionOutput formattedRecipes


myOptions = Options
  { optionYear = Nothing
  , optionMonth = Nothing
  , optionDay = Nothing
  , optionUrl = Just "https://www.chefkoch.de/rezepte/1841351298407440/Haferflocken-Kaese-Bratling.html"
  , optionUrlsOnly = False
  , optionRandom = False
  , optionOutput = "-"
  , optionFormat = "json"
  }


main = do
    --options <- execParser optionParser
    --run myOptions
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
    v :: DbT.Result A.Value <- DbS.allDbs ctx
    print v
    e :: DbT.Result Bool <- DbDb.exists ctx
    print e
    c :: DbT.Result A.Value <- DbDb.create ctx
    print c
    v2 :: DbT.Result A.Value <- DbS.allDbs ctx
    print v2
    d :: DbT.Result A.Value <- DbDb.delete ctx
    print d
    v3 :: DbT.Result A.Value <- DbS.allDbs ctx
    print v3
