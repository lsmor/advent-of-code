module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List.PointedList.Circular
    ( fromList, singleton, moveN, PointedList(_focus) )
import Control.Applicative ( Alternative((<|>)) )
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
--inputParser :: Parser Input
inputParser :: Parser [PointedList Square]
inputParser = lineToPointed `sepBy` endOfLine
  where 
    charToSquare :: Parser Square 
    charToSquare = (char '.' >> return Open) <|> (char '#' >> return Tree)
    lineToPointed :: Parser (PointedList Square)
    lineToPointed = do
        xs <- many' charToSquare
        case fromList xs of
            Nothing -> return $ singleton Open
            Just p  -> return p

------------ TYPES ------------
data Square = Open | Tree  deriving (Show, Eq, Enum)

type Input = [PointedList Square]

type OutputA = Int

type OutputB = Int

moveInput :: Int -> Int -> Input -> Input
moveInput a b =  drop b . fmap (moveN a)

countTrees :: Int -> Int -> Input -> OutputA
countTrees _ _ [] = 0
countTrees a b (x:xs) = fromEnum (_focus x) + countTrees a b (moveInput a (b-1) xs)

------------ PART A ------------
partA :: Input -> OutputA
partA = countTrees 3 1

------------ PART B ------------
partB :: Input -> OutputB
partB i = product $ ($ i) <$> [countTrees 1 1, countTrees 3 1, countTrees 5 1, countTrees 7 1, countTrees 1 2]