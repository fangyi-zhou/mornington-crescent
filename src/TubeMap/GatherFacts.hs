module TubeMap.GatherFacts where

import           Control.Monad     (forM_)
import           Data.List         (intercalate)
import qualified Data.Map          as M
import qualified Data.Set          as S
import           Data.String.Utils (strip)
import           Text.Parsec       (ParseError, Parsec, char, endOfLine, eof,
                                    getState, many1, noneOf, putState,
                                    runParser, skipMany)

newtype TubeLine = TubeLine String deriving (Eq, Ord, Show)
newtype Station = Station String deriving (Eq, Ord, Show)
type TubeMap = M.Map TubeLine (S.Set Station)
type TubeMapEntry = (Station, [TubeLine])

type Parser = Parsec String TubeMap

emptyTubeMap :: TubeMap
emptyTubeMap
  = M.empty

updateFact :: TubeMapEntry -> Parser ()
updateFact entry = do
  curr <- getState
  putState (updateTubeMap entry curr)
  where
    updateTubeMap :: TubeMapEntry -> TubeMap -> TubeMap
    updateTubeMap (newStation, stationLines) tubeMap
      = foldl (\map line -> (M.alter updateLine line map)) tubeMap stationLines
      where
        updateLine (Just s) = Just $ S.insert newStation s
        updateLine Nothing  = Just $ S.singleton newStation

stationNameParser :: Parser Station
stationNameParser = do
  name <- many1 (noneOf "[") -- Assume whatever before the '[' is part of station name
  return $ Station $ strip name

lineParser :: Parser TubeLine
lineParser = do
  _ <- char '['
  name <- many1 (noneOf "]")
  _ <- char ']'
  skipMany (char ' ')
  return $ TubeLine $ strip name

factParser :: Parser TubeMapEntry
factParser = do
  station <- stationNameParser
  lines <- many1 lineParser
  _ <- endOfLine
  return (station, lines)

factsParser :: Parser TubeMap
factsParser = do
  entries <- many1 factParser
  eof
  forM_ entries updateFact
  getState

parseFacts :: String -> FilePath -> Either ParseError TubeMap
parseFacts contents path
  = runParser factsParser emptyTubeMap path contents

showFacts :: TubeMap -> String
showFacts tubeMap
  = intercalate "\n" (map (uncurry showLine) (M.assocs tubeMap))

showLine :: TubeLine -> S.Set Station -> String
showLine (TubeLine line) stations
  = line ++ " Line: " ++ intercalate ", " (map show (S.toList stations))

loadFactsFromFile :: FilePath -> IO ()
loadFactsFromFile path = do
  contents <- readFile path
  case parseFacts contents path of
    Left err      -> putStrLn $ show err
    Right tubemap -> putStrLn $ showFacts tubemap
