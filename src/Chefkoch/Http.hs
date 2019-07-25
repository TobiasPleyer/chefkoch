module Chefkoch.Http where


import           Control.Concurrent.Async (mapConcurrently)
import           Control.Lens
import           Control.Monad
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
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
wreqURL url = (TL.toStrict . TL.decodeUtf8 . flip (^.) responseBody) <$> get url


fileURL :: String -> IO T.Text
fileURL f = do
    filecontent <- readFile f
    return (T.pack filecontent)


downloadMonthlyRecipeListing :: (String -> IO T.Text) -> Year -> Month -> IO T.Text
downloadMonthlyRecipeListing grabber year month = do
    let url = "https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year
    grabber url


downloadRecipePage :: (String -> IO T.Text) -> Recipe -> IO T.Text
downloadRecipePage grabber = grabber . recipeUrl


downloadRecipesByDate :: (String -> IO T.Text)                -- ^ the grabber to use for download
                      -> Bool                                 -- ^ sparse or full download
                      -> (Maybe Year, Maybe Month, Maybe Day) -- ^ date selection criteria
                      -> IO (Either String [Either String Recipe])
downloadRecipesByDate grabber sparse (my,mm,md) = do
    (year,month) <- yearMonthFromMaybe (my,mm)
    monthlyListing <- downloadMonthlyRecipeListing grabber year month
    let partialRecipesParseResult = parseMonthlyRecipeListing monthlyListing
    case partialRecipesParseResult of
      Left err -> return $ Left err
      Right partialRecipes -> do
        let
          recipes = map ( modifyRecipeYear (Just year)
                        . modifyRecipeMonth (Just month)) partialRecipes
          recipeSelection = selectRecipesByDay md recipes
        if sparse
        then
          return $ Right $ map Right recipeSelection
        else do
          recipeDetails <- downloadRecipesByUrl grabber (map recipeUrl recipeSelection)
          updatedRecipes <- forM (zip recipeDetails recipes)
               (\(detail,recipe) ->
                 case detail of
                   Left err -> return $ Left err
                   Right detail' ->
                     ( return . Right
                     . modifyRecipeIngredients (recipeIngredients detail')
                     . modifyRecipeInstruction (recipeInstruction detail')) recipe)
          return $ Right updatedRecipes


downloadRecipeByUrl :: (String -> IO T.Text) -> String -> IO (Either String Recipe)
downloadRecipeByUrl grabber url = do
    recipePage <- grabber url
    let parseRecipePageParseResult = parseRecipePage recipePage
    case parseRecipePageParseResult of
      Left err -> return $ Left err
      Right (title, ingr, inst) ->
        return $ Right emptyRecipe{ recipeUrl = url
                                  , recipeName = title
                                  , recipeIngredients = ingr
                                  , recipeInstruction = inst}


downloadRecipesByUrl :: (String -> IO T.Text) -> [String] -> IO [Either String Recipe]
downloadRecipesByUrl grabber urls =
  let download = downloadRecipeByUrl grabber
  in mapConcurrently download urls


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
