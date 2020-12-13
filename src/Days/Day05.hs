module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import qualified Data.Attoparsec.Text as Atto
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
rowToInt :: Char -> Int
rowToInt 'B' = 1
rowToInt 'F' = 0

colToInt :: Char -> Int
colToInt 'R' = 1
colToInt 'L' = 0

textToEnconding :: Parser SitEnconding
textToEnconding = do
    rs <- Atto.take 7
    cs <- Atto.take 3
    let sr = T.unpack (T.reverse rs)
    let sc = T.unpack (T.reverse cs)
    let row = sum $ zipWith (\c e -> rowToInt c * 2 ^ e)  sr [0..]
    let column = sum $ zipWith (\c e -> colToInt c * 2 ^ e)  sc [0..]
    return $ Sit {..}

inputParser :: Parser Input
inputParser = textToEnconding `sepBy` endOfLine

------------ TYPES ------------
data SitEnconding = Sit {row :: Int , column :: Int} deriving (Show, Eq)

type Input = [SitEnconding]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
getSitId :: SitEnconding -> Int
getSitId sit = row sit * 8 + column sit

partA :: Input -> OutputA
partA = maximum . fmap getSitId

------------ PART B ------------
partB :: Input -> OutputB
partB sits = case idx of 
    Nothing -> 0 
    Just i -> (t !! i) + 1
    where t = sort . fmap getSitId $ sits 
          diffs = zipWith (-) t (tail t)
          idx = elemIndex (-2) diffs




