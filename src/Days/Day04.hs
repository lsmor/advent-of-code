{-# LANGUAGE TypeApplications #-}
module Days.Day04 (runDay) where

import Data.Attoparsec.Text
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Char
import qualified Program.RunDay as R (runDay)

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

    pid <- Pid <$> "pid" `Map.lookup` m
    eyr <- Eyr <$> ("eyr" `Map.lookup` m >>= readMaybe . unpack)
    byr <- Byr <$> ("byr" `Map.lookup` m >>= readMaybe . unpack)
    iyr <- Iyr <$> ("iyr" `Map.lookup` m >>= readMaybe . unpack)
    ecl <- Ecl <$> "ecl" `Map.lookup` m
    hcl <- Hcl <$> "hcl" `Map.lookup` m
    hgt <- Hgt <$> "hgt" `Map.lookup` m
    let cid = Cid $ "cid" `Map.lookup` m

    return PassPort {..}

passPortFromMapValidate :: Map.Map Text Text -> Maybe PassPort 
passPortFromMapValidate m = do
    pid <- "pid" `Map.lookup` m >>= validateField
    eyr <- "eyr" `Map.lookup` m >>= validateField
    byr <- "byr" `Map.lookup` m >>= validateField
    iyr <- "iyr" `Map.lookup` m >>= validateField
    ecl <- "ecl" `Map.lookup` m >>= validateField
    hcl <- "hcl" `Map.lookup` m >>= validateField
    hgt <- "hgt" `Map.lookup` m >>= validateField
    let cid = Cid $ "cid" `Map.lookup` m

    return PassPort {..}

passPortParser :: (Map.Map Text Text -> Maybe PassPort) -> Parser (Maybe PassPort)
passPortParser f = do
    kv <- fieldsParser
    let m = Map.fromList kv
    return $ f m

inputParser :: Parser Input
inputParser = catMaybes <$> passPortParser passPortFromMapValidate `sepBy` endOfLine

------------ TYPES ------------
class IsValidPassPortField a where
    validateField :: Text -> Maybe a

newtype Pid = Pid Text deriving (Show, Eq)
instance IsValidPassPortField Pid where
    validateField t = do 
        validate ((== 9) . T.length) t
        validate (T.all isNumber) t
        return $ Pid t

newtype Cid = Cid (Maybe Text) deriving (Show, Eq)

newtype Eyr = Eyr Int deriving (Show, Eq)
instance IsValidPassPortField Eyr where
    validateField t = do
        validate ((== 4) . T.length) t
        i <- readMaybe @Int (unpack t)
        validate (<= 2030) i
        validate (2020 <=) i
        return $ Eyr i

newtype Byr = Byr Int deriving (Show, Eq)
instance IsValidPassPortField Byr where
    validateField t = do
        validate ((== 4) . T.length) t
        i <- readMaybe @Int (unpack t)
        validate (<= 2002) i
        validate (1920 <=) i
        return $ Byr i

newtype Iyr = Iyr Int deriving (Show, Eq)
instance IsValidPassPortField Iyr where
    validateField t = do
        validate ((== 4) . T.length) t
        i <- readMaybe @Int (unpack t)
        validate (<= 2020) i
        validate (2010 <=) i
        return $ Iyr i

newtype Ecl = Ecl Text deriving (Show, Eq)
instance IsValidPassPortField Ecl where
    validateField t = do 
        validate (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) t
        return $ Ecl t

newtype Hcl = Hcl Text deriving (Show, Eq)
instance IsValidPassPortField Hcl where
    validateField t = do 
        (c, t1) <- T.uncons t
        validate (== '#') c
        validate ((== 6) . T.length) t1
        validate (T.all isHexDigit) t1
        return $ Hcl t

newtype Hgt = Hgt Text deriving (Show, Eq)
instance IsValidPassPortField Hgt where
    validateField t = do 
        (t1, c1) <- T.unsnoc t
        (t2, c0) <- T.unsnoc t1
        let u = c0 `T.cons` c1 `T.cons` ""
        i <- readMaybe @Int (unpack t2)
        case u of 
            "cm" -> do
                validate (<= 193) i
                validate (150 <=) i
                return $ Hgt t
            "in" -> do
                validate (<= 76) i
                validate (59 <=) i
                return $ Hgt t
            _    -> Nothing

data PassPort = PassPort { pid :: Pid
                         , cid :: Cid
                         , eyr :: Eyr
                         , byr :: Byr
                         , iyr :: Iyr
                         , ecl :: Ecl
                         , hcl :: Hcl
                         , hgt :: Hgt
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
