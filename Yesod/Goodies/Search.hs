-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Goodies.Search
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a transparent method for searching a list of values.
--
-------------------------------------------------------------------------------
module Yesod.Goodies.Search
    ( SearchResult(..)
    , Search(..)
    , search
    , search_
    , weightedSearch
    , weightedSearch_
    ) where

import Data.List  (sortBy)
import Data.Ord   (Ordering(..), compare, comparing)
import Data.Maybe (catMaybes)

import qualified Data.Text as T

-- | A ranked search result
data SearchResult a = SearchResult
    { searchRank   :: Double
    , searchResult :: a
    }

class Search a where
    -- | If two results have the same rank, lend preference to one, the 
    --   sorting includes a reversal so this ordering should return GT 
    --   for values that should appear above
    preference :: SearchResult a -> SearchResult a -> Ordering
    preference _ _ = EQ

    -- | Given a search term and some @a@, provide @Just@ a ranked 
    --   result or @Nothing@.
    match :: T.Text -> a -> Maybe (SearchResult a)

-- | Excute a search on a list of @a@s, and rank the results
search :: Search a => T.Text -> [a] -> [SearchResult a]
search t = rankResults . catMaybes . map (match t)

-- | Identical but discards the rank values.
search_ :: Search a => T.Text -> [a] -> [a]
search_ t = map searchResult . search t

-- | Perform a normal search but add (or remove) weight from items that 
--   have certian properties.
weightedSearch :: Search a => (a -> Double) -> T.Text -> [a] -> [SearchResult a]
weightedSearch f t = rankResults . map (applyFactor f) . catMaybes . map (match t)

    where
        applyFactor :: (a -> Double) -> SearchResult a -> SearchResult a
        applyFactor f (SearchResult d v) = SearchResult (d * f v) v

-- | Identical but discards the rank values.
weightedSearch_ :: Search a => (a -> Double) -> T.Text -> [a] -> [a]
weightedSearch_ f t = map searchResult . weightedSearch f t

-- | Reverse sort the results by rank and then preference.
rankResults :: Search a => [SearchResult a] -> [SearchResult a]
rankResults = reverse . sortBy (comparing searchRank `andthen` preference)

-- | Compare values in a compound way
--
--   > sortBy (comparing snd `andthen` comparing fst)
--
andthen :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
andthen f g a b =
    case f a b of
        EQ -> g a b
        x  -> x
