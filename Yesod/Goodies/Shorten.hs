{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Goodies.Shorten
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-------------------------------------------------------------------------------
module Yesod.Goodies.Shorten (Shorten(..)) where

import qualified Data.Text as T

class Shorten a where
    shorten :: Int -> a -> a

instance Shorten String where
    shorten n s = if length s > n then take (n - 3) s ++ "..." else s

instance Shorten T.Text where
    shorten n t = if T.length t > n then T.take (n - 3) t `T.append` "..." else t
