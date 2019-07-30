module Chefkoch.Http where


import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens
import           Control.Monad
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Network.HTTP
import           Network.Wreq
import           System.IO
import           System.Process

import           Chefkoch.DataFunctions
import           Chefkoch.DataTypes
import           Chefkoch.Html.Parser
import           Chefkoch.Util


openURL :: String -> IO T.Text
openURL url = fmap T.pack $ getResponseBody =<< simpleHTTP (getRequest url)


wgetURL :: String -> IO T.Text
wgetURL url = do
    let
      wgetPath = "/usr/bin/wget"
      wgetArgs = [url, "-qO-"]
    htmlString <- readProcess wgetPath wgetArgs ""
    return (T.pack htmlString)


wreqURL :: String -> IO T.Text
wreqURL url = TL.toStrict . TL.decodeUtf8 . flip (^.) responseBody <$> get url


fileURL :: String -> IO T.Text
fileURL = TIO.readFile


downloadMonthlyRecipeListing :: (String -> IO T.Text) -> Year -> Month -> IO T.Text
downloadMonthlyRecipeListing grabber year month = do
    let url = "https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year
    sayLoud $ "Grabbing URL: " ++ url
    grabber url


downloadRecipePage :: (String -> IO T.Text) -> String -> IO T.Text
downloadRecipePage grabber url = do
    sayLoud $ "Downloading URL: " ++ url
    grabber url


downloadRecipesByDate :: (String -> IO T.Text)                -- ^ the grabber to use for download
                      -> Bool                                 -- ^ sparse or full download
                      -> (Maybe Year, Maybe Month, Maybe Day) -- ^ date selection criteria
                      -> IO [Recipe]
downloadRecipesByDate grabber sparse (my,mm,md) = do
    sayLoud "Downloading recipes by date"
    (year,month) <- yearMonthFromMaybe (my,mm)
    monthlyListing <- downloadMonthlyRecipeListing grabber year month
    sayLoud "Parsing monthly listing..."
    let partialRecipesParseResult = parseMonthlyRecipeListing monthlyListing
    case partialRecipesParseResult of
      Left err -> do
        sayLoud "Unable to parse monthly recipe listing. Reason:"
        putStrLn err
        return []
      Right recipeInfos -> do
        sayLoud "Parse succeeded"
        let recipeInfoSelection = selectRecipesByDay md recipeInfos
        if sparse
        then return $ map mkPartialRecipe recipeInfoSelection
        else reverse <$> foldM (\rs (day, weekday, name, url) -> do
          recipeDetails <- parseRecipePage <$> downloadRecipePage grabber url
          case recipeDetails of
            Left err -> do
              putStrLn $ "Error! Url: " ++ url ++ "\n" ++ err
              return rs
            Right (title,ingredients,instructions) ->
              return $ Recipe
                       { recipeDay = Just day
                       , recipeWeekday = Just weekday
                       , recipeMonth = Just month
                       , recipeYear = Just year
                       , recipeName = name
                       , recipeUrl = url
                       , recipeIngredients = ingredients
                       , recipeInstruction = instructions
                       } : rs
            ) [] recipeInfoSelection


downloadRecipeByUrl :: (String -> IO T.Text) -> String -> IO (Either String Recipe)
downloadRecipeByUrl grabber url = do
    sayLoud $ "Downloading recipe by url: " ++ url
    recipePage <- grabber url
    let parseRecipePageParseResult = parseRecipePage recipePage
    case parseRecipePageParseResult of
      Left err -> return $ Left err
      Right (title, ingr, inst) ->
        return $ Right emptyRecipe{ recipeUrl = url
                                  , recipeName = title
                                  , recipeIngredients = ingr
                                  , recipeInstruction = inst}


downloadRecipesByUrl :: (String -> IO T.Text) -> [String] -> IO [Recipe]
downloadRecipesByUrl grabber urls = do
  let download = downloadRecipeByUrl grabber
  sayLoud "Starting parallel download..."
  eRecipes <- mapConcurrently download urls
  foldM (\rs eRecipe -> case eRecipe of
      Left err -> do
        putStrLn err
        return rs
      Right r -> return (r:rs)
    ) [] eRecipes


-- Simple helpers

wgetDownloadRecipePage = downloadRecipePage wgetURL
wreqDownloadRecipePage = downloadRecipePage wreqURL
openDownloadRecipePage = downloadRecipePage openURL
fileDownloadRecipePage = downloadRecipePage fileURL

wgetDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing wgetURL
wreqDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing wreqURL
openDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing openURL
fileDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing fileURL

wgetDownloadRecipesByDate = downloadRecipesByDate wgetURL
wreqDownloadRecipesByDate = downloadRecipesByDate wreqURL
openDownloadRecipesByDate = downloadRecipesByDate openURL
fileDownloadRecipesByDate = downloadRecipesByDate fileURL

wgetDownloadRecipeByUrl = downloadRecipeByUrl wgetURL
wreqDownloadRecipeByUrl = downloadRecipeByUrl wreqURL
openDownloadRecipeByUrl = downloadRecipeByUrl openURL
fileDownloadRecipeByUrl = downloadRecipeByUrl fileURL

wgetDownloadRecipesByUrl = downloadRecipesByUrl wgetURL
wreqDownloadRecipesByUrl = downloadRecipesByUrl wreqURL
openDownloadRecipesByUrl = downloadRecipesByUrl openURL
fileDownloadRecipesByUrl = downloadRecipesByUrl fileURL
