module Days.Day02 (runDay) where

import qualified Data.Vector as Vec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text


runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = flip sepBy endOfLine $ do
    lowBound <- decimal
    char '-'
    upperBound <- decimal
    space
    character <- anyChar
    string ": "
    passw <- takeTill isEndOfLine
    let policy = Policy {..}
    return PassW {..}

------------ TYPES ------------
data Policy = Policy {lowBound :: Int, upperBound :: Int, character :: Char} deriving (Eq, Show)
data PassW  = PassW {policy :: Policy, passw :: Text} deriving (Eq, Show)
type Input = [PassW]

type OutputA = Int

type OutputB = Int

isValidPassWA :: PassW -> Bool 
isValidPassWA PassW {..} = 
    let characterOccurrences = T.length $ T.filter (character policy == ) passw  
     in lowBound policy <= characterOccurrences && characterOccurrences <= upperBound policy

isValidPassWB :: PassW -> Bool 
isValidPassWB PassW {..} = 
    let vectorOfChars = Vec.fromList (T.unpack passw) 
        characterAtLow = vectorOfChars Vec.!? (lowBound policy - 1)
        characterAtUp  = vectorOfChars Vec.!? (upperBound policy - 1)
        c = character policy
        isInLow = characterAtLow == Just c
        isInUp = characterAtUp == Just c
     in (isInLow && not isInUp) ||(isInUp && not isInLow)
------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter isValidPassWA

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter isValidPassWB