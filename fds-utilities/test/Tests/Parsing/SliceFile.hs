module Tests.Parsing.SliceFile (tests) where

import FDSUtilities.Parsing
import Test.HUnit


assertRight :: Show a => Either a b -> IO b
assertRight (Left s) = assertFailure (show s) >> fail "assertion failed"
assertRight (Right a) = return a

tests = TestList
    [ test1
    -- , test2
    ]

test1 = TestLabel "parse a slice file" $ TestCase $ do
    res <- parseSliceFile testPath
    print res
    assertRight res *> pure ()
    where
        testPath = "test-data/test_slice.sf"

-- test2 = TestLabel "Current Date Parser" $ TestCase $ do
--     let Right res = parse currentDateParser "testString" testString
--     assertEqual "currentDate" "January 26, 2016  00:16:16" res
--     where
--         testString = " Current Date     : January 26, 2016  00:16:16\n"