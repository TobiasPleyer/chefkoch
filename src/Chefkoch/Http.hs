module Chefkoch.Http where


import           Control.Concurrent.Async (mapConcurrently)
import           Control.Monad
import qualified Data.Text as T
import           Network.HTTP
import           System.Process
import           System.IO

import Chefkoch.DataTypes
import Chefkoch.DataFunctions
import Chefkoch.Util
import Chefkoch.Html.Parser


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


downloadMonthlyRecipeListing :: (String -> IO T.Text) -> Year -> Month -> IO T.Text
downloadMonthlyRecipeListing grabber year month = do
    let url = ("https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year)
    grabber url


downloadRecipePage :: (String -> IO T.Text) -> Recipe -> IO T.Text
downloadRecipePage grabber = grabber . recipeUrl


downloadRecipesByDate :: (String -> IO T.Text)                -- ^ the grabber to use for download
                      -> Bool                                 -- ^ sparse or full download
                      -> (Maybe Year, Maybe Month, Maybe Day) -- ^ date selection criteria
                      -> IO [Recipe]
downloadRecipesByDate grabber sparse (my,mm,md) = do
    (year,month) <- yearMonthFromMaybe (my,mm)
    monthlyListing <- downloadMonthlyRecipeListing grabber year month
    let
      partialRecipes = parseMonthlyRecipeListing monthlyListing
      recipes = (map ( modifyRecipeYear (Just year)
                     . modifyRecipeMonth (Just month))) partialRecipes
      recipeSelection = selectRecipesByDay md recipes
    if sparse
    then
      return recipeSelection
    else do
      recipeDetails <- downloadRecipesByUrl grabber (map recipeUrl recipeSelection)
      forM (zip recipeDetails recipes)
           (\(detail,recipe) ->
             ( return
             . modifyRecipeIngredients (recipeIngredients detail)
             . modifyRecipeInstruction (recipeInstruction detail)) recipe)


downloadRecipeByUrl :: (String -> IO T.Text) -> String -> IO Recipe
downloadRecipeByUrl grabber url = do
    recipePage <- grabber url
    let (ingr, inst) = parseRecipePage recipePage
    return emptyRecipe{ recipeUrl = url
                      , recipeIngredients = ingr
                      , recipeInstruction = inst}


downloadRecipesByUrl :: (String -> IO T.Text) -> [String] -> IO [Recipe]
downloadRecipesByUrl grabber urls =
  let download = downloadRecipeByUrl grabber
  in mapConcurrently download urls


-- Simple helpers

wgetDownloadRecipePage = downloadRecipePage wgetURL
openDownloadRecipePage = downloadRecipePage openURL
fileDownloadRecipePage = downloadRecipePage fileURL

wgetDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing wgetURL
openDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing openURL
fileDownloadMonthlyRecipeListing = downloadMonthlyRecipeListing fileURL

wgetDownloadRecipesByDate = downloadRecipesByDate wgetURL
openDownloadRecipesByDate = downloadRecipesByDate openURL
fileDownloadRecipesByDate = downloadRecipesByDate fileURL

wgetDownloadRecipeByUrl = downloadRecipeByUrl wgetURL
openDownloadRecipeByUrl = downloadRecipeByUrl openURL
fileDownloadRecipeByUrl = downloadRecipeByUrl fileURL

wgetDownloadRecipesByUrl = downloadRecipesByUrl wgetURL
openDownloadRecipesByUrl = downloadRecipesByUrl openURL
fileDownloadRecipesByUrl = downloadRecipesByUrl fileURL
