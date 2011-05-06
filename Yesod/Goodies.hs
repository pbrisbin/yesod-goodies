-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Goodies
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A collection of various small helpers that are general enough to be 
-- useful in any yesod application.
--
-------------------------------------------------------------------------------
module Yesod.Goodies 
    ( 
    -- | Automatically lookup a gravatar img url by email
      module Yesod.Goodies.Gravatar

    -- | Create concise link widgets for use throughout your app
    , module Yesod.Goodies.Links

    -- | Useful wrappers around the pandoc library specific to markdown 
    --   manipulation
    , module Yesod.Goodies.Markdown

    -- | A simple framework for searching the data on your site
    , module Yesod.Goodies.Search

    -- | Shorten a variety of string-like types to a certian length and 
    --   add ellipsis
    , module Yesod.Goodies.Shorten

    -- | Print time values in human readable ways
    , module Yesod.Goodies.Time
    ) where

import Yesod.Goodies.Gravatar
import Yesod.Goodies.Links
import Yesod.Goodies.Markdown
import Yesod.Goodies.Search
import Yesod.Goodies.Shorten
import Yesod.Goodies.Time
