{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module SemVerSpec where

import Data.List (sort)
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import Text.Trifecta (parseString, Result(..))

import SemVer

semVerParseSpecs :: Spec
semVerParseSpecs =
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


data ParseCase = SuccessCase { describedInput :: DescribedInput
                             , expected :: Expected
                             }
               | FailureCase { describedInput :: DescribedInput }

type Expected = SemVer

data DescribedInput = DescribedInput { description :: String
                                     , input :: String
                                     }

goodCases :: [ParseCase]
goodCases =
  [ SuccessCase
      { describedInput =
          DescribedInput { description = "Version Only, full zeros"
                         , input = "0.0.0"
                         }
      , expected = SemVer 0 0 0 (prerelease []) []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Version Only, one-two-three"
                         , input = "1.2.3"
                         }
      , expected = SemVer 1 2 3 (prerelease []) []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease, single component"
                         , input = "1.2.3-4"
                         }
      , expected = SemVer 1 2 3 (prerelease ["4"]) []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease, some components"
                         , input = "1.0.0-x.7.z.92"
                         }
      , expected = SemVer 1 0 0 (prerelease ["x", "7", "z", "92"]) []
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease and metadata, simple"
                         , input = "1.0.0-gamma+002"
                         }
      , expected = SemVer 1 0 0 (prerelease ["gamma"]) ["002"]
      }
  , SuccessCase
      { describedInput =
          DescribedInput { description = "Prerelease and metadata, multiple components"
                         , input = "1.0.0-beta.1+oof.sha.41af286"
                         }
      , expected = SemVer 1 0 0 (prerelease ["beta", "1"]) ["oof", "sha", "41af286"]
      }
  ]

badCases :: [ParseCase]
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


semVerOrdSpecs :: Spec
semVerOrdSpecs =
  describe "SemVer ordering" $ for_ ordCases test
  where
    shouldBeSorted semVers = sort semVers `shouldBe` semVers
    test OrdCase{..} = it desc $ shouldBeSorted orderedSemvers


data OrdCase = OrdCase { desc :: String
                       , orderedSemvers :: [SemVer]
                       }

ordCases :: [OrdCase]
ordCases = [ OrdCase { desc = "Precedence is determined by the first difference when comparing each of these identifiers from left to right as follows: Major, minor, and patch versions are always compared numerically."
                     , orderedSemvers = [ semver 1 0 0
                                        , semver 2 0 0
                                        , semver 2 1 0
                                        , semver 2 1 1
                                        ]
                     }
           , OrdCase { desc = "When major, minor, and patch are equal, a pre-release version has lower precedence than a normal version"
                     , orderedSemvers = [ semverPre 1 0 0 ["alpha"]
                                        , semver 1 0 0
                                        ]
                     }
           , OrdCase { desc = "Precedence for two pre-release versions with the same major, minor, and patch version MUST be determined by comparing each dot separated identifier from left to right until a difference is found. Identifiers consisting of only digits are compared numerically. Identifiers with letters or hyphens are compared lexically in ASCII sort order. Numeric identifiers always have lower precedence than non-numeric identifiers. A larger set of pre-release fields has a higher precedence than a smaller set, if all of the preceding identifiers are equal."
                     , orderedSemvers = [ semverPre 1 0 0 ["alpha"]
                                        , semverPre 1 0 0 ["alpha", "1"]
                                        , semverPre 1 0 0 ["alpha", "beta"]
                                        , semverPre 1 0 0 ["beta"]
                                        , semverPre 1 0 0 ["beta", "2"]
                                        , semverPre 1 0 0 ["beta", "11"]
                                        , semverPre 1 0 0 ["rc", "1"]
                                        , semver 1 0 0
                                        ]
                     }
           ]
