module Chefkoch.Http where


import Control.Monad
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Network.HTTP
import System.Process
import System.IO

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
downloadRecipePage grabber recipe = grabber (recipeUrl recipe)


getRecipeIngredientsAndInstructions :: (String -> IO T.Text) -> Recipe -> IO ([String],String)
getRecipeIngredientsAndInstructions grabber recipe = do
  recipePage <- downloadRecipePage grabber recipe
  return $ parseRecipePage recipePage


downloadRecipesByDate :: (String -> IO T.Text) -> (Maybe Year, Maybe Month, Maybe Day) -> IO [Recipe]
downloadRecipesByDate grabber (y,m,d) = do
    (currYear,currMonth,currDay) <- getCurrentYearMonthDay
    let year = fromMaybe currYear y
        month = fromMaybe currMonth m
    monthlyListing <- downloadMonthlyRecipeListing grabber year month
    let
      partialRecipes = parseMonthlyRecipeListing monthlyListing
      recipes = (map ( modifyRecipeYear (Just year)
                     . modifyRecipeMonth (Just month))) partialRecipes
    go recipes year month d
    where
      go :: [Recipe] -> Year -> Month -> Maybe Day -> IO [Recipe]
      go recipes year month Nothing = do
        recipeDetails <- forM recipes (getRecipeIngredientsAndInstructions grabber)
        forM (zip recipeDetails recipes) (\((ingr,inst),recipe)
            -> ( return
               . modifyRecipeIngredients ingr
               . modifyRecipeInstruction inst) recipe)
      go recipes year month day = do
        let
          maybeRecipe = find (\r -> recipeDay r == day) recipes
        if isNothing maybeRecipe
        then
          return []
        else do
          let
            recipe = fromJust maybeRecipe
          (ingr, inst) <- getRecipeIngredientsAndInstructions grabber recipe
          return [( modifyRecipeInstruction inst
                  . modifyRecipeIngredients ingr) recipe]


downloadRecipeByLink :: (String -> IO T.Text) -> String -> IO Recipe
downloadRecipeByLink grabber url = do
    recipePage <- grabber url
    let (ingr, inst) = parseRecipePage recipePage
    return emptyRecipe{ recipeUrl = url
                      , recipeIngredients = ingr
                      , recipeInstruction = inst}


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

wgetDownloadRecipeByLink = downloadRecipeByLink wgetURL
openDownloadRecipeByLink = downloadRecipeByLink openURL
fileDownloadRecipeByLink = downloadRecipeByLink fileURL
