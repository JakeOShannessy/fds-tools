{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module FDSUtilities.Verification.Display where

import Data.Monoid
import Data.Tree
import qualified Data.Text as T

import Text.Blaze.Html
import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import FDSUtilities.Types.Assess


instance ToMarkup Assessment where
    toMarkup (Assessment resultTree) = H.ul H.! A.class_ ("verification-list") $ testTreeToHtml resultTree

instance ToMarkup (Either String Assessment) where
    toMarkup (Left err) = do
        H.string $ ("Simulation Verification Assessment could not be completed due to:" :: String)
        H.br
        H.pre $ H.string err
    toMarkup (Right ass) = H.toHtml ass

testTreeToHtml :: Tree CompletedTest -> H.Html
testTreeToHtml (Node result subResults) = H.li H.! A.class_ ("test" <> " " <> theClass <> " " <> showStyle) H.! A.onclick visCode $ do
    H.div $ H.toHtml result
    case subResults of
      [] -> return ()
      _ -> do
        H.ul $ do
          mapM_ testTreeToHtml subResults
    where
        visCode = "toggle_visibility(arguments[0],this);"
        theClass = case testResult of
            Success _ -> "successTest"
            Warning _ -> "warningTest"
            Failure _ -> "failureTest"
        showStyle = (if isSuccess result then "hidden-test" else "shown-test")
        (CompletedTest _ testResult) = result

testTreeToTextTree :: Tree CompletedTest -> Tree T.Text
testTreeToTextTree (Node result subResults) = Node (T.pack $ show result) (map testTreeToTextTree subResults)

-- testTreeToText :: Tree CompletedTest -> Text
-- testTreeToText (Node result subResults) =

--     H.li H.! A.class_ ("test" <> " " <> theClass <> " " <> showStyle) H.! A.onclick visCode $ do
--     H.div $ H.toHtml result
--     case subResults of
--         [] -> return ()
--         _ -> do
--         H.ul $ do
--             mapM_ testTreeToHtml subResults
--     where
--         visCode = "toggle_visibility(arguments[0],this);"
--         theClass = case testResult of
--             Success _ -> "successTest"
--             Warning _ -> "warningTest"
--             Failure _ -> "failureTest"
--         showStyle = (if isSuccess result then "hidden-test" else "shown-test")
--         (CompletedTest _ testResult) = result

instance ToMarkup CompletedTest where
    toMarkup t@(CompletedTest name result) = do
        H.strong $ H.toHtml $ name
        H.span H.! A.class_ style $ H.toHtml $ (pr :: String)
        H.toHtml (":" :: String)
        H.ul $ do
            mapM_ (\l->H.li $ H.toHtml l) (lines str)
        where
            (str, pr, style) = case result of
                (Success str) -> (str, "[OK]", "success")
                (Warning str) -> (str, "[Warning]", "warning")
                (Failure str) -> (str, "[Failed]", "failure")

instance ToMarkup TestResult where
    toMarkup result = do
        H.span H.! A.class_ style $ H.toHtml $ (pr :: String)
        H.toHtml (":" :: String)
        H.div $ do
            mapM_ (\l->H.li $ H.toHtml l) (lines str)
        where
            (str, pr, style) = case result of
                (Success str) -> (str, "[OK]", "success")
                (Warning str) -> (str, "[Warning]", "warning")
                (Failure str) -> (str, "[Failed]", "failure")
