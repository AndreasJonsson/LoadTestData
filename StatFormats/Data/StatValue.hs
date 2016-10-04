{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  StatFormats.Data.StatValue
Copyright   :  Andreas Jonsson
License     :  GPL3

Maintainer  :  andreas.jonsson@kreablo.se
Stability   :  experimental
-}

module StatFormats.Data.StatValue(
  StatValue(..),
  AppStatValue(..),
  AppStatValues(..),
  headerOrder
) where

import Prelude(String, Double, (/), ($), (.), (/=))
import Data.Text(Text, pack);
import Data.Csv
import Text.Printf(printf)
import Data.Text.Encoding(encodeUtf8)
import Data.Monoid
import Data.Functor

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

data StatValue = StatValue {
      svLabel :: Text,
      svUnit  :: Text,
      svFormat :: String,
      svScale :: Double,
      svFunction :: VU.Vector Double -> Double
}

data AppStatValue = AppStatValue StatValue (VU.Vector Double)

data AppStatValues = AppStatValues [StatValue] (VU.Vector Double) (Text, Text)

instance ToField AppStatValue where
    toField (AppStatValue StatValue {
               svFormat = format,
               svScale = scale,
               svFunction = f
             } v) = encodeUtf8 $ pack $ printf format $ f v / scale

instance ToNamedRecord AppStatValues where
    toNamedRecord (AppStatValues svs v (scenario, step)) =
        namedRecord ([ "Step" .= (scenario <> " " <> step) ] <>
                    fmap (\sv -> statValueLabel sv .= AppStatValue sv v) svs)

headerOrder :: [StatValue] -> Header
headerOrder = V.fromList . ([ encodeUtf8 "Step" ] <>) . fmap statValueLabel

statValueLabel :: StatValue -> B.ByteString
statValueLabel (StatValue { svLabel = label, svUnit = unit }) = encodeUtf8 $ label <> if unit /= "" then " (" <> unit <> ")" else ""