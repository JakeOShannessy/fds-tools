{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module FDSUtilities.CompileCompress.Render where

import qualified Text.Blaze.XHtml5 as H
import qualified Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed

-- TODO: the page (to be a new type) is a monoid! if treated correctly
-- |Transform a HTML compilation into a full page.
toPage :: String -> String -> String -> H.Html -> H.Html
toPage heading style script content = H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml heading
        H.style $ H.toHtml style
        H.script H.! A.type_ "text/javascript" $ H.preEscapedToHtml $ script
    H.body $ do
        H.h1 $ H.toHtml heading
        H.br
        content


jscript :: String
jscript = unlines
    [ "<!--\nfunction toggle_visibility(ev,el) {"
    , "  if (el.classList.contains(\"test\")) {"
    , "    el.classList.toggle(\"shown-test\");"
    , "    el.classList.toggle(\"hidden-test\");"
    , "    ev.stopPropagation();\n  };"
    , "}//-->"
    ]

css :: String
css = C8.unpack
    $(makeRelativeToProject "data/CompileCompress.css" >>= embedFile)
