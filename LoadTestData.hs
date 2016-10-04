{-# LANGUAGE OverloadedStrings #-}

module Main(main)

where

import Options.Applicative
import StatFormats.Data.LoadTestData
import StatFormats.Data.StatValue

import Statistics.Sample
import Statistics.Function

import qualified Data.Vector.Unboxed as V
import Prelude(Double, ($), flip, uncurry, String, min, max, (/), mapM_, (.), div, (*), (-), ceiling, fromIntegral, Int, (>), Bool, not)
import Data.Maybe
import System.IO
import Data.Csv(encodeByName)
import qualified Data.ByteString.Lazy as BL

data Options = Options {
      optInputFilename :: String
    }

options :: Parser Options
options = Options <$>
          strOption ( long "in" <> metavar "FILENAME" <> help "The filename of the logfile." )

optParser :: ParserInfo Options
optParser = info (helper <*> options) (fullDesc <> progDesc "Process the logfile from load test." <> header "load-test-data - processor for load test output.")

{-
cvTitle :: Bool -> Bool -> (String, String, (V.Vector Double -> Double), String, Double) -> IO ()
cvTitle first last (t, u, _, _, _) = do
  when (not first) $ putStr $ printf ","
  printf "\"%s (%s)\"" t u
  when last $ putStr "\n"

cvValue :: V.Vector Double -> Bool -> Bool -> (String, String, (V.Vector Double -> Double), String, Double) -> IO ()
cvValue v first last (_, _, f, format, scale) = do
  when first $ putStr $ printf ","
  printf ("\"" <> format <> "\"") $ f v / scale
  when last $ putStr "\n"
-}

main :: IO ()
main = do
  opts <- execParser optParser
  d <- loadLoadTestData $ optInputFilename opts
  let s  = (flip $ uncurry loadTestDataSamples) d
      op = [ StatValue "median"   "s"  "%.2f" 1e6 (\v -> sort v V.! (V.length v `div` 2)),
             StatValue "90th"     "s"  "%.2f" 1e6 (\v ->  sort v V.! (ceiling (fromIntegral (V.length v) * (0.9 :: Double)) - 1 )),
             StatValue "mean"     "s"  "%.2f" 1e6 mean,
             StatValue "variance" "s²" "%.2f" 1e12 variance,
             StatValue "stddev"   "s"  "%.2f" 1e6 stdDev,
             StatValue "min"      "s"  "%.2f" 1e6 V.minimum,
             StatValue "max"      "s"  "%.2f" 1e6 V.maximum,
             StatValue "num"      ""   "%.0f" 1   (fromIntegral . V.length)
           ]
    in
      BL.putStr $ encodeByName (headerOrder op) $ mapMaybe (\p -> do
                                                          v <- s p
                                                          pure $ AppStatValues op v p) $ loadTestPoints d


{-
statF :: T.Text -> (T.Text, T.Text) -> T.Text -> (V.Vector Double -> Double) -> String -> Double ->  Maybe (V.Vector Double) -> IO ()
statF t (scenario, step) unit f format scale mv =
  putStr $ T.unpack $ ((scenario <>
                      " " <>
                      step <>
                      " " <>
                      t <>
                      ": " <>
                               case mv of
                                 Nothing -> "Nothing"
                                 Just v  -> (T.pack  $ printf format $ (f v / scale))) <> unit) <> "\n"
-}