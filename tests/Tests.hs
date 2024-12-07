{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    (===),
  )

tests :: IO [Test]
tests = return tests'

normalSettings :: TestArgs
normalSettings = stdTestArgs {verbosity = Verbose}

getNormalPT :: (Testable prop) => PropertyTest prop -> Test
getNormalPT = getPropertyTestWith normalSettings

tests' :: [Test]
tests' =
  [ getNormalPT
      PropertyTest
        { name = "tautology",
          tags = [],
          property = \i -> (===) @Int i i
        },
    getNormalPT
      PropertyTest
        { name = "monotonicity-addition",
          tags = [],
          property = \(args@(ap, bp) :: (Positive Int, Positive Int)) ->
            let a = getPositive ap
                b = getPositive bp
                c = a + b
            in 0 <= c
        },
    getNormalPT
      PropertyTest
        { name = "monotinicity-time",
          tags = [],
          property = ioProperty do
            a <- getCPUTime
            withFile "/dev/null" WriteMode $ \handle -> mapM_ print [1..10000] >> hFlush handle
            b <- getCPUTime
            return (a < b)
        }
  ]
