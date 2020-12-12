module Days.Day01 (runDay) where

import Data.Attoparsec.Text
import Control.Applicative ((<|>))
import qualified Data.Map.Strict as Map
import qualified Program.RunDay as R (runDay)

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Integer]

type OutputA = Integer

type OutputB = Integer

aux :: Integer -> Map.Map Integer Integer -> [Integer] -> Maybe (Integer, Integer)
aux _ _ [] = Nothing
aux l m (x:xs) = sequenceA (x, Map.lookup x m) <|> aux l newm xs
  where newm = Map.insert (l - x) x m

------------ PART A ------------
partA :: Input -> OutputA
partA xs = case aux 2020 Map.empty xs of
  Nothing -> 0
  Just (a,b) -> a * b

------------ PART B ------------
partB :: Input -> OutputB
partB [] = 0
partB (x:xs) = case aux (2020 - x) Map.empty xs of
  Nothing -> partB xs
  Just (a,b) -> x * a * b
