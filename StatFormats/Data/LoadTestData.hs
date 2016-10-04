{- |
Module      :  StatFormats.Data.LoadTestData
Copyright   :  Andreas Jonsson
License     :  GPL3

Maintainer  :  andreas.jonsson@kreablo.se
Stability   :  experimental
-}

module StatFormats.Data.LoadTestData(
  loadTestData,
  loadLoadTestData,
  loadTestDataSamples,
  loadTestPoints,
  LoadTestData(..),
  LoadTestScenarioData(..),
  LoadTestStep(..)
) where

import qualified Data.Vector.Unboxed as V
import qualified Data.Attoparsec.Text as A
import Prelude                                   (Int, Num, ($), Show, Double, (||), (==))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative
import Control.Monad.Error
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Data.Either
import Data.Foldable
import qualified Data.ByteString as B
import System.IO


newtype LoadTestData n = LoadTestData (Map.Map (T.Text, T.Text) (LoadTestScenarioData n)) deriving Show

data LoadTestScenarioData n = LoadTestScenarioData {
      ltsdStep :: LoadTestStep n,
      ltsdInstances :: Map.Map Int Int
    } deriving Show

newtype LoadTestStep n = LoadTestStep (V.Vector n) deriving Show

emptyScenarioData :: V.Unbox n => LoadTestScenarioData n
emptyScenarioData = LoadTestScenarioData {
                      ltsdStep = LoadTestStep V.empty,
                      ltsdInstances = Map.empty
                    }

loadTestDataSamples :: T.Text -> T.Text -> LoadTestData n -> Maybe (V.Vector n)
loadTestDataSamples scenario step (LoadTestData m) = do
  LoadTestScenarioData { ltsdStep = LoadTestStep stepData } <- Map.lookup (scenario, step) m
  pure stepData

loadTestPoints :: LoadTestData n -> [(T.Text, T.Text)]
loadTestPoints (LoadTestData m) = Map.keys m

loadLoadTestData :: NumericData n => FilePath -> IO (LoadTestData n)
loadLoadTestData filename = do
  t <- TE.decodeUtf8 <$> B.readFile filename
  case A.parseOnly loadTestData t of
    Left m  -> throwError $ strMsg m
    Right d -> pure d


loadTestData :: NumericData n => A.Parser (LoadTestData n)
loadTestData = foldr addLineToData (LoadTestData Map.empty) <$> many line

addLineToData :: V.Unbox n => (T.Text, Int, T.Text, n) -> LoadTestData n -> LoadTestData n
addLineToData l@(scenario, _, step, _) (LoadTestData m) =
    let sd = fromMaybe emptyScenarioData (Map.lookup (scenario, step) m)
    in
      LoadTestData $ Map.insert (scenario, step) (addLine sd l) m

scenarioName :: A.Parser T.Text
scenarioName = A.takeWhile1 (\c -> isLetter c || c == '_')

scenarioInstance :: A.Parser Int
scenarioInstance = A.decimal

stepName :: A.Parser T.Text
stepName = A.takeWhile1 (\c -> isAlphaNum c || c == '_')

line :: NumericData n => A.Parser (T.Text, Int, T.Text, n)
line = do
  A.skipSpace
  scenario <- scenarioName
  n <- scenarioInstance
  A.skipSpace
  step <- stepName
  A.skipSpace
  sample <- numericData
  pure (scenario, n, step, sample)

addLine :: V.Unbox n => LoadTestScenarioData n -> (T.Text, Int, T.Text, n) -> LoadTestScenarioData n
addLine (LoadTestScenarioData {
           ltsdStep = LoadTestStep stepData,
           ltsdInstances = stepInstances
         })  (_, n, _, sample) = LoadTestScenarioData {
              ltsdStep = LoadTestStep $ stepData `V.snoc` sample,
              ltsdInstances = Map.insert n (V.length stepData) stepInstances
            }

class (Num n, V.Unbox n) => NumericData n where
    numericData :: A.Parser n

instance NumericData Int where
    numericData = A.decimal

instance NumericData Double where
    numericData = A.double

