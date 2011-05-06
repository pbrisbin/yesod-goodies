{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Goodies.Markdown
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Rewrite/simplification of yesod-markdown written by ajdunlap.
--
-- <https://github.com/ajdunlap/yesod-markdown>
--
-------------------------------------------------------------------------------
module Yesod.Goodies.Markdown
  ( Markdown(..)
  -- * Conversions
  , parseMarkdown
  , writePandoc
  -- * Wrappers
  , markdownToHtml
  , markdownFromFile
  -- * Option sets
  , yesodDefaultWriterOptions
  , yesodDefaultParserState
  -- * Form helpers
  , markdownField
  , maybeMarkdownField
  )
  where


import Yesod
import Yesod.Form.Core
import Yesod.Goodies.Shorten

import Text.Pandoc
import Text.Pandoc.Shared

import Data.Monoid      (Monoid)
import Data.String      (IsString)
import System.Directory (doesFileExist)

import qualified Data.Text as T

newtype Markdown = Markdown String
    deriving (Eq, Ord, Show, Read, PersistField, IsString, Monoid)

instance Shorten Markdown where
    shorten n (Markdown s) = Markdown $ shorten n s

instance ToFormField Markdown y where
    toFormField = markdownField

instance ToFormField (Maybe Markdown) y where
    toFormField = maybeMarkdownField

markdownField :: (IsForm f, FormType f ~ Markdown) => FormFieldSettings -> Maybe Markdown -> f
markdownField = requiredFieldHelper markdownFieldProfile

maybeMarkdownField :: FormFieldSettings -> FormletField sub y (Maybe Markdown)
maybeMarkdownField = optionalFieldHelper markdownFieldProfile

markdownFieldProfile :: FieldProfile sub y Markdown
markdownFieldProfile = FieldProfile
    { fpParse = Right . Markdown . unlines . lines' . T.unpack
    , fpRender = \(Markdown m) -> T.pack m
    , fpWidget = \theId name val _isReq -> addHamlet
#if __GLASGOW_HASKELL__ >= 700
        [hamlet|
#else
        [$hamlet|
#endif
            <textarea id="#{theId}" name="#{name}" .markdown>#{val}
            |]
    }

    where
        lines' :: String -> [String]
        lines' = map go . lines

        go []        = []
        go ('\r':xs) = go xs
        go (x:xs)    = x : go xs

-- | Converts markdown directly to html using the yesod default option 
--   sets
markdownToHtml :: Markdown -> Html
markdownToHtml = writePandoc yesodDefaultWriterOptions
               . parseMarkdown yesodDefaultParserState

-- | Reads markdown in from the specified file; returns the empty string 
--   if the file does not exist
markdownFromFile :: FilePath -> IO Markdown
markdownFromFile f = do
    exists <- doesFileExist f
    content <- do
        if exists
            then readFile f
            else return ""

    return $ Markdown content

writePandoc :: WriterOptions -> Pandoc -> Html
writePandoc wo = preEscapedString . writeHtmlString wo

parseMarkdown :: ParserState -> Markdown -> Pandoc
parseMarkdown ro (Markdown m) = readMarkdown ro m

yesodDefaultWriterOptions :: WriterOptions
yesodDefaultWriterOptions = defaultWriterOptions
  { writerEmailObfuscation = JavascriptObfuscation
  , writerSectionDivs      = False
  , writerWrapText         = False
  }

yesodDefaultParserState :: ParserState
yesodDefaultParserState = defaultParserState
    { stateSmart    = True
    , stateParseRaw = True
    }
