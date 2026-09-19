{-# LANGUAGE FlexibleContexts #-}
module Jev.Patterns
  ( Band(..), band
  , compositeScore
  ) where

import qualified Data.Map.Strict as Map
import Jev.Types (ScoreAnswer(..))

-- | Three-band confidence routing: act, confirm/gather, or escalate to a human.
data Band = Act | Confirm | Escalate deriving (Eq, Show)

-- | Route a scalar (a confidence or a noul) against @(lo, hi)@ thresholds.
band :: (Double, Double) -> Double -> Band
band (lo, hi) x
  | x >= hi   = Act
  | x >= lo   = Confirm
  | otherwise = Escalate

-- | Weighted composite of several dimension scores, each normalised to [0,1]
-- by dividing by @(levels - 1)@ before weighting. Weights need not sum to 1.
compositeScore :: [(Double, ScoreAnswer)] -> Double
compositeScore [] = 0
compositeScore ws = sum [ w * norm sa | (w, sa) <- ws ] / sum (map fst ws)
  where
    norm sa = let n = Map.size sa.probabilities
              in if n <= 1 then sa.score else sa.score / fromIntegral (n - 1)
