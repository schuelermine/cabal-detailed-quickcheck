{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Tests where

import Data.Maybe (maybeToList)
import Distribution.TestSuite (Test)
import Distribution.TestSuite.QuickCheck
import System.CPUTime (getCPUTime)
import System.IO (hFlush, withFile, IOMode(WriteMode))
import Test.QuickCheck
  ( Positive,
    Testable,
    getPositive,
    ioProperty,
    (===), verbose,
  )

tests :: IO [Test]
tests =
  return [
    mkPT
      PropertyTest
        { name = "law-of-identity",
          tags = [],
          property = \(i :: Integer) -> i === i
        },
    mkPT
      PropertyTest
        { name = "monotonicity-of-addition",
          tags = [],
          property = \(getPositive -> a :: Integer) (getPositive -> b :: Integer) ->
            0 <= a + b
        }, mkPT
      PropertyTest
        { name = "monotinicity-time",
          tags = [],
          property = ioProperty do
            timeBefore <- getCPUTime
            withFile "/dev/null" WriteMode \handle -> do
              mapM_ print [1..10000]
              hFlush handle
            timeAfter <- getCPUTime
            return (timeBefore < timeAfter)
        }
  ]
  where
    mkPT :: Testable prop => PropertyTest prop -> Test
    mkPT = getPropertyTestWith (stdTestArgs {verbosity = Verbose})
