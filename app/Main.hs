import Control.Monad
import Data.Char
import qualified Data.Text as T
import Data.Foldable
import Network.HTTP
import System.Process
import Text.HTML.TagSoup
import Text.StringLike


data Weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday
             deriving (Eq, Show)


data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Eq, Show)


type Year = Int


data Unit = None
          | Liter
          | Milliliter
          | Gram
          | Kilogram
          | SmallSpoon
          | BigSpoon
          | Package
          | Slice
          | Unknown
          deriving (Eq, Show)


data Recipe = Recipe
    { day :: Int
    , weekday :: Weekday
    , month :: Month
    , year :: Year
    , name :: String
    , url :: String
    , ingredients :: [( String  -- ^ The name of the ingredient
                      , Float   -- ^ The quantity
                      , Unit    -- ^ The unit corresponding to the quantity
                      )]
    , instruction :: String
    , portions :: Int
    } deriving (Eq, Show)


month2Int :: Month -> Int
month2Int January = 1
month2Int February = 2
month2Int March = 3
month2Int April = 4
month2Int May = 5
month2Int June = 6
month2Int July = 7
month2Int August = 8
month2Int September = 9
month2Int October = 10
month2Int November = 11
month2Int December = 12


str2Weekday :: String -> Weekday
str2Weekday "(Mo)" = Monday
str2Weekday "(Di)" = Tuesday
str2Weekday "(Mi)" = Wednesday
str2Weekday "(Do)" = Thursday
str2Weekday "(Fr)" = Friday
str2Weekday "(Sa)" = Saturday
str2Weekday "(So)" = Sunday


--openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)


wgetURL :: String -> IO T.Text
wgetURL url = do
    let
      wgetPath = "/usr/bin/wget"
      wgetArgs = [url, "-qO-"]
    htmlString <- readProcess wgetPath wgetArgs ""
    return (T.pack htmlString)


fetchRecipeOverview :: (String -> IO T.Text) -> Year -> Month -> IO T.Text
fetchRecipeOverview grabber year month = do
    let url = ("https://www.chefkoch.de/rezept-des-tages.php?month="
               ++ show (month2Int month)
               ++ "&year="
               ++ show year)
    grabber url


normalize :: [Tag T.Text] -> [Tag T.Text]
normalize (x:xs) =
  case x of
    TagText t -> TagText (T.unwords (T.words t)) : normalize xs
    _         -> x : normalize xs
normalize [] = []


extractRecipeTable :: [Tag T.Text] -> [Tag T.Text]
extractRecipeTable = takeWhile (~/= TagClose "table")
                   . tail
                   . dropWhile (~/= TagOpen "table" [("class", "table-day")])


extractRecipeInfo :: T.Text -> [[T.Text]]
extractRecipeInfo = map (map clearTag)
                  . splitList 4
                  . filter (\t -> notEmptyText t && notTableTag t)
                  . normalize
                  . extractRecipeTable
                  . parseTags
    where
      notEmptyText (TagText t)
        | t == T.empty = False
      notEmptyText _   = True

      notTableTag (TagOpen  t _)
        | t == T.pack "tr"   = False
        | t == T.pack "td"   = False
        | t == T.pack "span" = False
      notTableTag (TagClose t)
        | t == T.pack "tr"   = False
        | t == T.pack "td"   = False
        | t == T.pack "span" = False
        | t == T.pack "a"    = False
      notTableTag _          = True

      splitList n xs
        | length xs < n = []
        | otherwise     = (take n xs) : splitList n (drop n xs)

      --clearTag :: Tag T.Text -> T.Text
      clearTag (TagText t) = t
      clearTag tag@(TagOpen aTag attrs) = fromAttrib (T.pack "href") tag
      clearTag _ = T.empty


extractCookingInstructions :: T.Text -> ([(String,Float,Unit)], String, Int)
extractCookingInstructions website = (ingredients,instructions,portions)
  where
    portions = 4
    ingredients = []
    instructions = ( T.unpack
                   . innerText
                   . normalize
                   . takeWhile (~/= "</div>")
                   . dropWhile (~/= "<div id=rezept-zubereitung")) tags
    tags = parseTags website


mkRecipe year month [day, weekday, relative_url, name] = do
  let base_url = "https://www.chefkoch.de"
      recipe_url = base_url ++ (T.unpack relative_url)
  webcontent <- wgetURL recipe_url
  let (ingredients,instructions,portions) = extractCookingInstructions webcontent
  return (Recipe
           (read (T.unpack (T.init day)))
           (str2Weekday (T.unpack weekday))
           month
           year
           (T.unpack name)
           recipe_url
           ingredients
           instructions
           portions)


fetchRecipes :: Year -> Month -> IO [Recipe]
fetchRecipes year month = do
    webcontent <- fetchRecipeOverview wgetURL year month
    let raw_info = extractRecipeInfo webcontent
    mapM (mkRecipe year month) [head raw_info]


main = do
    let
      year = 2018
      month = February
    recipes <- fetchRecipes year month
    forM_ recipes print
