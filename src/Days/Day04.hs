{-# LANGUAGE TypeApplications #-}
module Days.Day04 (runDay) where

import Data.Attoparsec.Text
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Void
import Control.Applicative
import Control.Monad
import Data.Char
import qualified Program.RunDay as R (runDay)
import qualified Util.Util as U
import Data.Maybe

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
keylist :: [Text]
keylist = ["pid", "cid", "eyr", "byr", "iyr", "ecl", "hcl", "hgt"]

keyValueParser :: Text -> Parser a -> Parser (Text, a)
keyValueParser k p = do 
    let parseKey = string k *> ":" *> skipSpace
    v <- parseKey *> p
    return (k, v)

passPortFieldParser :: Parser (Text, Text)
passPortFieldParser = choice $ flip keyValueParser word <$> keylist
  where word = takeWhile1 $ not . isSpace

fieldsParser :: Parser [(Text, Text)]
fieldsParser = passPortFieldParser `sepBy` satisfy isSpace

validate :: (a -> Bool) -> a -> Maybe a
validate f t = if f t then Just t else Nothing

passPortFromMap :: Map.Map Text Text -> Maybe PassPort 
passPortFromMap m = do
    
    pid <- "pid" `Map.lookup` m >>= validate ((== 9) . T.length)
    readMaybe @Int (unpack pid)
    
    eyr <- "eyr" `Map.lookup` m 
                >>= validate ((== 4) . T.length)
                >>= readMaybe . unpack 
                >>= validate (2020 <=)
                >>= validate (<= 2030)

    byr <- "byr" `Map.lookup` m
                >>= validate ((== 4) . T.length)
                >>= readMaybe . unpack 
                >>= validate (1920 <=)
                >>= validate (<= 2002)

    iyr <- "iyr" `Map.lookup` m
                >>= validate ((== 4) . T.length)
                >>= readMaybe . unpack 
                >>= validate (2010 <=)
                >>= validate (<= 2020)

    ecl <- "ecl" `Map.lookup` m 
                >>= validate (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])

    hcl <- "hcl" `Map.lookup` m
                >>= \t -> case T.uncons t of 
                            Just ('#', ts) -> validate (T.all isHexDigit) ts
                            _              -> Nothing

    let hgtparse :: Parser (Maybe Text)
        hgtparse = do
            n <- decimal
            u <- takeText
            case u of
                "cm" -> if 150 <= n && n <= 193 then return (Just $ T.pack $ show n) else return Nothing
                "in" -> if 59 <= n && n <= 76 then return (Just $ T.pack $ show n) else return Nothing

    mhgt <- "hgt" `Map.lookup` m >>= maybeResult . parse hgtparse
    hgt <- mhgt
    let cid = "cid" `Map.lookup` m

    return PassPort {..}

passPortParser :: Parser (Maybe PassPort)
passPortParser = do
    kv <- fieldsParser
    let m = Map.fromList kv
    return $ passPortFromMap m

inputParser :: Parser Input
inputParser = catMaybes <$> passPortParser `sepBy` endOfLine

------------ TYPES ------------
data PassPort = PassPort { pid :: Text
                         , cid :: Maybe Text
                         , eyr :: Int
                         , byr :: Int
                         , iyr :: Int
                         , ecl :: Text
                         , hcl :: Text
                         , hgt :: Text
                         } deriving (Show, Eq)

type Input = [PassPort]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = length

------------ PART B ------------
partB :: Input -> OutputB
partB = length
