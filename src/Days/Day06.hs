module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Text ( Text )
import Data.Char
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
lineParser :: Parser Answer
lineParser = many1 (satisfy isLetter)
groupParser :: Parser Group
groupParser = many1 $ lineParser <* endOfLine
inputParser :: Parser Input
inputParser = groupParser  `sepBy` endOfLine

------------ TYPES ------------
type Answer = [Char]
type Group  = [Answer]  

type Input = [Group]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
yesanswers :: Group -> Set Char
yesanswers g = Set.fromList $ concat g

yesOnly :: Group -> Set Char
yesOnly = foldl1 Set.intersection . fmap Set.fromList 

partA :: Input -> OutputA
partA = sum . fmap (Set.size . yesanswers)
------------ PART B ------------
partB :: Input -> OutputB
partB = sum . fmap (Set.size . yesOnly)
