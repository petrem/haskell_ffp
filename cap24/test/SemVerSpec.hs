{-# LANGUAGE OverloadedStrings #-}
module SemVerSpec where

import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import Text.Trifecta (parseString, Result(..))

import SemVer

semVerSpecs :: Spec
semVerSpecs =
  describe "SemVer parsing" $ do
    for_ goodCases test
    for_ badCases test
  where
    test (SuccessCase di e) = it (description di) $ do
      parseString parseSemVer mempty (input di) `shouldBeSuccess` e
    test (FailureCase di) = it (description di) $ shouldBeFailure $
      parseString parseSemVer mempty (input di)

    shouldBeFailure (Failure _) = return ()
    shouldBeFailure (Success r) =
      expectationFailure $ "expected failure, not success:" ++ show r
    shouldBeSuccess (Failure f) e =
      expectationFailure $ "expected success: " ++ show e ++ " not:\n" ++ show f
    shouldBeSuccess (Success v) e = v `shouldBe` e


data Case = SuccessCase { describedInput :: DescribedInput
                        , expected :: Expected
                        }
            | FailureCase { describedInput :: DescribedInput }

type Expected = SemVer 

data DescribedInput = DescribedInput { description :: String
                                     , input :: String
                                     }

goodCases :: [Case]
goodCases =
  [ SuccessCase
      { describedInput =
          DescribedInput { description = "Version Only, full zeros"
                         , input = "0.0.0"
                         }
      , expected = SemVer 0 0 0 [] []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Version Only, one-two-three"
                         , input = "1.2.3"
                         }
      , expected = SemVer 1 2 3 [] []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease, single component"
                         , input = "1.2.3-4"
                         }
      , expected = SemVer 1 2 3 [NumLabel 4] []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease, some components"
                         , input = "1.0.0-x.7.z.92"
                         }
      , expected = SemVer 1 0 0 [NonNumLabel "x", NumLabel 7, NonNumLabel "z", NumLabel 92] []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease and metadata, simple"
                         , input = "1.0.0-gamma+002"
                         }
      , expected = SemVer 1 0 0 [NonNumLabel "gamma"] ["002"]
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease and metadata, multiple components"
                         , input = "1.0.0-beta.1+oof.sha.41af286"
                         }
      , expected = SemVer 1 0 0 [NonNumLabel"beta", NumLabel 1] ["oof", "sha", "41af286"]
      }
  ]

badCases :: [Case]
badCases =
  [ FailureCase
      { describedInput =
          DescribedInput { description = "Version Only, just two components"
                         , input = "0.0"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Version Only, non-numeric component"
                         , input = "1.two.3"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Version Only, space in version"
                         , input = "1. 2.3"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Version Only, space in version"
                         , input = "1 .2.3"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Version Only, no leading zeroes"
                         , input = "0.1.02"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Version followed by non - or +"
                         , input = "1.2.3;3"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Prerelease, leading zero in numeric id"
                         , input = "1.2.3-a.01"
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Empty components in prerelease"
                         , input = "1.2.3-."
                         }
      }
  , FailureCase
      { describedInput =
          DescribedInput { description = "Empty components in metadata"
                         , input = "1.2.3+.."
                         }
      }
  ]
