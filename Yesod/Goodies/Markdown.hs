{-# LANGUAGE CPP                        #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  -- * Form helper
  , markdownField
  )
  where


import Yesod
import Yesod.Form.Types
import Yesod.Goodies.Shorten

import Text.Blaze (preEscapedString)
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

instance ToField Markdown master where
    toField = areq markdownField

instance ToField (Maybe Markdown) master where
    toField = aopt markdownField

markdownField :: RenderMessage master FormMessage => Field sub master Markdown
markdownField = Field
    { fieldParse = blank $ Right . Markdown . unlines . lines' . T.unpack
    , fieldView  = \theId name val _isReq -> addHamlet
#if __GLASGOW_HASKELL__ >= 700
        [hamlet|
#else
        [$hamlet|
#endif
            <textarea id="#{theId}" name="#{name}">#{either id unMarkdown val}
            |]
     }

     where
        unMarkdown :: Markdown -> T.Text
        unMarkdown (Markdown s) = T.pack s

        lines' :: String -> [String]
        lines' = map go . lines

        go []        = []
        go ('\r':xs) = go xs
        go (x:xs)    = x : go xs


blank :: (Monad m, RenderMessage master FormMessage)
      => (T.Text -> Either FormMessage a)
      -> [T.Text]
      -> m (Either (SomeMessage master) (Maybe a))
blank _ []     = return $ Right Nothing
blank _ ("":_) = return $ Right Nothing
blank f (x:_)  = return $ either (Left . SomeMessage) (Right . Just) $ f x

{-
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
        -}

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
