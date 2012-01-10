{-# LANGUAGE CPP          #-}
{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE TypeFamilies #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Links
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Links
    ( Destination(..)
    , Link(..)
    , YesodLinked(..)
    , IsLink(..)
    , link
    , link'
    ) where

import Yesod.Core (Route, GWidget, whamlet)
import qualified Data.Text as T

-- | An internal route or external url
data Destination m = Internal (Route m) | External T.Text

-- | A link to a 'Destination' with supplied titles and text to be used 
--   when showing the html.
data Link m = Link
    { linkDest  :: Destination m
    , linkTitle :: T.Text
    , linkText  :: T.Text
    }

-- | A type family class used to generalize widgets printing routes that 
--   are internal to your site
--
--   > instance YesodLinked MySite where
--   >     type Linked = MySite
--
class YesodLinked m where
    type Linked

-- | Any type can represent a link.
--
--   > instance IsLink MyAppRoute where
--   >     toLink RootR  = Link (Internal RootR)  "go home"         "home"
--   >     toLink AboutR = Link (Internal AboutR) "about this site" "about"
--   >     ...
--   >
--   > getRootR :: Handler RepHtml
--   > getRootR = defaultLayout $ do
--   >     [hamlet|
--   >
--   >         be sure to visit our ^{link AboutR} page.
--   >
--   >         |]
--
class IsLink a where
    toLink :: a -> Link Linked

-- | Link to any @'IsLink'@ type. This is simply @'link'' . 'toLink'@.
link :: IsLink a => a -> GWidget s Linked ()
link = link' . toLink

-- | Link to a raw @'Link'@. Can be used even if your site is not an
--   instance of 'YesodLinked'.
link' :: Link m -> GWidget s m ()
#if __GLASGOW_HASKELL__ >= 700
link' (Link (Internal i) t x) = [whamlet|<a title="#{t}" href="@{i}">#{x}|]
link' (Link (External e) t x) = [whamlet|<a title="#{t}" href="#{e}">#{x}|]
#else
link' (Link (Internal i) t x) = [$whamlet|<a title="#{t}" href="@{i}">#{x}|]
link' (Link (External e) t x) = [$whamlet|<a title="#{t}" href="#{e}">#{x}|]
#endif
