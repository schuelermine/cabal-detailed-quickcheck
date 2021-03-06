{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- NoFieldSelectors is implemented in GHC 9.2.2, but HLS doesn’t support it
-- {-# LANGUAGE NoFieldSelectors #-}

-- |
-- Module:       Distribution.TestSuite.QuickCheck
-- Description:  Convert QuickCheck properties into Cabal tests
-- Copyright:    ⓒ Anselm Schüler 2022
-- License:      MIT
-- Maintainer:   Anselm Schüler <mail@anselmschueler.com>
-- Stability:    stable
-- Portability:  Portable
--
-- This module allows you to easily make Cabal tests for the @detailed-0.9@ interface. ([docs](https://cabal.readthedocs.io/en/3.6/cabal-package.html#example-package-using-detailed-0-9-interface))
-- It sets sensible option declarations for the tests.
--
-- This module re-uses record names from "Distribution.TestSuite" and "Test.QuickCheck".
-- It is recommended that you enable the [@DisambiguateRecordFields@](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/disambiguate_record_fields.html) extension in GHC and/or import the module qualified.
-- For basic tests, you don’t need to import "Distribution.TestSuite".
module Distribution.TestSuite.QuickCheck
  ( -- * Create tests
    getPropertyTest,
    getPropertyTestWith,
    getPropertyTestUsing,
    getPropertyTestWithUsing,
    getPropertyTests,
    propertyTestGroup,

    -- * Argument data types
    PropertyTest (..),
    TestArgs (..),
    Verbosity (..),

    -- * Functions for using arguments
    argsToTestArgs,
    testArgsToArgs,
    stdTestArgs,
  )
where

import Data.Bool (bool)
import Data.Functor ((<&>))
import qualified Distribution.TestSuite as T
import qualified Test.QuickCheck as QC
import Text.Read (readMaybe)

-- | Datatype for setting the verbosity of tests
data Verbosity
  = -- | QuickCheck prints nothing. This sets @'QC.chatty' = 'False'@.
    Silent
  | -- | Print basic statistics. This sets @'QC.chatty' = 'True'@.
    Chatty
  | -- | Print every test case. This applies 'QC.verbose'.
    Verbose
  deriving
    ( Eq,
      -- | 'Silent' < 'Chatty' < 'Verbose'
      Ord,
      Show,
      Read,
      Enum,
      Bounded
    )

-- ! [PARTIAL] This function fails when passed Silent
switchVerbosity :: Verbosity -> Bool -> Verbosity -> Verbosity
switchVerbosity v' q v = bool max min q v $ bool id pred q v'

-- | Arguments for altering property test behaviour.
--   These can be altered in the final Cabal 'T.Test' using 'T.setOption'.
data TestArgs = TestArgs
  { -- | Verbosity for tests. See 'QC.verbose' and 'QC.chatty'.
    verbosity :: Verbosity,
    -- TODO Consider joining verboseShrinking back into verbosity

    -- | Whether QuickCheck should print shrinks. See 'QC.verboseShrinking'.
    verboseShrinking :: Bool,
    -- | Maximum discarded tests per successful test. See 'QC.maxDiscardRatio'.
    maxDiscardRatio :: Int,
    -- | Disable shrinking. See 'QC.noShrinking'.
    noShrinking :: Bool,
    -- | Maximum number of shrink attempts. See 'QC.maxShrinks'.
    maxShrinks :: Int,
    -- | Maximum number of successful checks before passing. See 'QC.maxSuccess'.
    maxSuccess :: Int,
    -- | Maximum size of test cases. See 'QC.maxSize'.
    maxSize :: Int,
    -- | Scale size by an integer using 'QC.mapSize'.
    sizeScale :: Int
  }

-- | Transform a QuickCheck 'QC.Args' value to a 'TestArgs' value, defaulting all missing properties
argsToTestArgs :: QC.Args -> TestArgs
argsToTestArgs QC.Args {..} =
  TestArgs
    { verbosity = if chatty then Chatty else Silent,
      verboseShrinking = False,
      maxDiscardRatio,
      noShrinking = False,
      maxShrinks,
      maxSuccess,
      maxSize,
      sizeScale = 1
    }

-- | Default arguments for property tests
stdTestArgs :: TestArgs
stdTestArgs = argsToTestArgs QC.stdArgs

-- | Recover arguments passed to 'QC.quickCheck' from a 'TestArgs'
testArgsToArgs :: TestArgs -> QC.Args
testArgsToArgs
  TestArgs
    { verbosity,
      maxDiscardRatio,
      maxShrinks,
      maxSuccess,
      maxSize
    } =
    QC.Args
      { replay = Nothing,
        maxSuccess,
        maxDiscardRatio,
        maxSize,
        chatty = verbosity >= Chatty,
        maxShrinks
      }

useModifiers :: QC.Testable a => TestArgs -> a -> QC.Property
useModifiers TestArgs {verbosity, noShrinking, verboseShrinking, sizeScale} =
  foldr (.) QC.property $
    snd
      <$> filter
        fst
        [ (verbosity == Verbose, QC.verbose),
          (verboseShrinking, QC.verboseShrinking),
          (noShrinking, QC.noShrinking),
          (sizeScale /= 1, QC.mapSize (* sizeScale))
        ]

qcTestArgs :: QC.Testable a => TestArgs -> a -> IO QC.Result
qcTestArgs args property = QC.quickCheckWithResult (testArgsToArgs args) (useModifiers args property)

switchVIn :: Verbosity -> Bool -> TestArgs -> TestArgs
switchVIn v' q args@TestArgs {verbosity} = args {verbosity = switchVerbosity v' q verbosity}

setArgStr :: String -> String -> Maybe (TestArgs -> TestArgs)
setArgStr "silent" str =
  readMaybe str <&> \val args@TestArgs {verbosity} ->
    if val
      then args {verbosity = Silent}
      else args {verbosity = max Chatty verbosity}
setArgStr "chatty" str = readMaybe str <&> switchVIn Chatty
setArgStr "verbose" str = readMaybe str <&> switchVIn Verbose
setArgStr "verboseShrinking" str =
  readMaybe str <&> \val args ->
    args {verboseShrinking = val}
setArgStr "verbosity" str =
  readMaybe str <&> \val args ->
    args {verbosity = val}
setArgStr "maxDiscardRatio" str =
  readMaybe str <&> \val args ->
    args {maxDiscardRatio = val}
setArgStr "noShrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = val}
setArgStr "shrinking" str =
  readMaybe str <&> \val args ->
    args {noShrinking = not val}
setArgStr "maxShrinks" str =
  readMaybe str <&> \val args ->
    args {maxShrinks = val}
setArgStr "maxSuccess" str =
  readMaybe str <&> \val args ->
    args {maxSuccess = val}
setArgStr "maxSize" str =
  readMaybe str <&> \val args ->
    args {maxSize = val}
setArgStr "sizeScale" str =
  readMaybe str <&> \val args ->
    args {sizeScale = val}
setArgStr _ _ = Nothing

positiveIntType :: T.OptionType
positiveIntType =
  T.OptionNumber
    { optionNumberIsInt = True,
      optionNumberBounds = (Just "1", Nothing)
    }

getOptionDescrs :: TestArgs -> [T.OptionDescr]
getOptionDescrs TestArgs {..} =
  [ T.OptionDescr
      { optionName = "silent",
        optionDescription = "Suppress QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity == Silent
      },
    T.OptionDescr
      { optionName = "chatty",
        optionDescription = "Print QuickCheck output",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Chatty
      },
    T.OptionDescr
      { optionName = "verbose",
        optionDescription = "Print checked values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verbosity > Verbose
      },
    T.OptionDescr
      { optionName = "verboseShrinking",
        optionDescription = "Print all checked and shrunk values",
        optionType = T.OptionBool,
        optionDefault = Just . show $ verboseShrinking
      },
    T.OptionDescr
      { optionName = "verbosity",
        optionDescription = "Verbosity level",
        optionType = T.OptionEnum ["Silent", "Chatty", "Verbose", "VerboseShrinking"],
        optionDefault = Just $ show verbosity
      },
    T.OptionDescr
      { optionName = "maxDiscardRatio",
        optionDescription = "Maximum number of discarded tests per successful test before giving up",
        optionType = positiveIntType,
        optionDefault = Just $ show maxDiscardRatio
      },
    T.OptionDescr
      { optionName = "noShrinking",
        optionDescription = "Disable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just $ show noShrinking
      },
    T.OptionDescr
      { optionName = "shrinking",
        optionDescription = "Enable shrinking",
        optionType = T.OptionBool,
        optionDefault = Just . show $ not noShrinking
      },
    T.OptionDescr
      { optionName = "maxShrinks",
        optionDescription = "Maximum number of shrinks to before giving up or zero to disable shrinking",
        optionType = positiveIntType,
        optionDefault = Just $ show maxShrinks
      },
    T.OptionDescr
      { optionName = "maxSuccess",
        optionDescription = "Maximum number of successful tests before succeeding",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSuccess
      },
    T.OptionDescr
      { optionName = "maxSize",
        optionDescription = "Size to use for the biggest test cases",
        optionType = positiveIntType,
        optionDefault = Just $ show maxSize
      },
    T.OptionDescr
      { optionName = "sizeScale",
        optionDescription = "Scale all sizes by a number",
        optionType = positiveIntType,
        optionDefault = Just $ show sizeScale
      }
  ]

-- | Property test declaration with metadata
data PropertyTest prop = PropertyTest
  { -- | Name of the test, for Cabal. See See Cabal’s 'T.name'.
    name :: String,
    -- | Tags of the test, for Cabal. See Cabal’s 'T.tags'.
    tags :: [String],
    -- | Property to check. This should usually be or return an instance of 'QC.Testable'.
    property :: prop
  }

-- | Get a Cabal 'T.Test' with custom 'TestArgs' from a 'PropertyTest' that takes the test arguments and returns a 'QC.testable' value
getPropertyTestWithUsing ::
  QC.Testable prop =>
  -- | The arguments for the test
  TestArgs ->
  -- | A property test whose 'property' takes a 'TestArgs' argument
  PropertyTest (TestArgs -> prop) ->
  T.Test
getPropertyTestWithUsing originalArgs PropertyTest {..} =
  let withArgs args =
        T.TestInstance
          { -- TODO Consider using 'T.Progress' to allow intermediate results
            run = do
              result <- qcTestArgs args (property args)
              let resultStr = "\n" ++ show result
              return $ T.Finished case result of
                QC.Success {} -> T.Pass
                QC.GaveUp {} -> T.Error $ "GaveUp: QuickCheck gave up" ++ resultStr
                QC.Failure {} -> T.Fail $ "Failure: A property failed" ++ resultStr
                QC.NoExpectedFailure {} ->
                  T.Fail $
                    "NoExpectedFailure: A property that should have failed did not"
                      ++ "\n"
                      ++ show result,
            name,
            tags,
            options = getOptionDescrs originalArgs,
            setOption = \opt str -> case setArgStr opt str of
              Nothing -> Left "Parse error"
              Just f -> Right . withArgs $ f args
          }
   in T.Test $ withArgs originalArgs

discardingTestArgs :: PropertyTest prop -> PropertyTest (TestArgs -> prop)
discardingTestArgs test@PropertyTest {property} = test {property = const property}

-- | Get a Cabal 'T.Test' from a 'PropertyTest' that takes the test arguments and returns a 'QC.Testable' value
getPropertyTestUsing ::
  QC.Testable prop =>
  -- | A property test whose 'property' takes a 'TestArgs' argument
  PropertyTest (TestArgs -> prop) ->
  T.Test
getPropertyTestUsing = getPropertyTestWithUsing stdTestArgs

-- | Get a Cabal 'T.Test' from a 'PropertyTest' with custom 'TestArgs'
getPropertyTestWith ::
  QC.Testable prop =>
  -- | The arguments for the test
  TestArgs ->
  PropertyTest prop ->
  T.Test
getPropertyTestWith args = getPropertyTestWithUsing args . discardingTestArgs

-- | Get a Cabal 'T.Test' from a 'PropertyTest'
getPropertyTest :: QC.Testable prop => PropertyTest prop -> T.Test
getPropertyTest = getPropertyTestWithUsing stdTestArgs . discardingTestArgs

-- | Get a list of 'T.Test's from a list of 'PropertyTest's
getPropertyTests :: QC.Testable prop => [PropertyTest prop] -> [T.Test]
getPropertyTests = (getPropertyTest <$>)

-- | Get a named test group from a list of 'PropertyTest's. These are assumed to be able to run in parallel. See 'T.testGroup' and 'T.Group'.
propertyTestGroup :: QC.Testable prop => String -> [PropertyTest prop] -> T.Test
propertyTestGroup name = T.testGroup name . getPropertyTests
