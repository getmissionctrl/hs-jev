module TestUtil (expectRight) where

import Test.Hspec (Expectation, expectationFailure)

-- | Run an assertion on a 'Right', or fail the example with the 'Left'.
-- Keeps decode tests flat instead of nesting @case@ on 'Either'.
expectRight :: Show e => Either e a -> (a -> Expectation) -> Expectation
expectRight (Left e)  _ = expectationFailure (show e)
expectRight (Right a) f = f a
