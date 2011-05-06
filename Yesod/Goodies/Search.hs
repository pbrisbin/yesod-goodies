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
-------------------------------------------------------------------------------
module Yesod.Goodies.Search
    ( SearchResult(..)
    , Search(..)
    , search
    , search_
    , weightedSearch
    , weightedSearch_
    -- * search helpers
    , TextSearch(..)
    , keywordMatch
    ) where

import Data.List  (sortBy, intersect)
import Data.Ord   (Ordering(..), compare, comparing)
import Data.Maybe (catMaybes)

import qualified Data.Text as T

-- | A ranked search result
data SearchResult a = SearchResult
    { searchRank   :: Double
    , searchResult :: a
    }

-- | Any item can be searched by providing a @'match'@ function.
class Search a where
    -- | If two results have the same rank, optionally lend preference
    --   to one. The /greater/ value will appear first.
    preference :: SearchResult a -> SearchResult a -> Ordering
    preference _ _ = EQ

    -- | Given a search term and some @a@, provide @Just@ a ranked 
    --   result or @Nothing@.
    match :: T.Text -> a -> Maybe (SearchResult a)

-- | Excute a search on a list of @a@s and rank the results
search :: Search a => T.Text -> [a] -> [SearchResult a]
search t = rankResults . catMaybes . map (match t)

-- | Identical but discards the actual rank values.
search_ :: Search a => T.Text -> [a] -> [a]
search_ t = map searchResult . search t

-- | Add (or remove) weight from items that have certian properties.
weightedSearch :: Search a => (a -> Double) -> T.Text -> [a] -> [SearchResult a]
weightedSearch f t = rankResults . map (applyFactor f) . catMaybes . map (match t)

    where
        applyFactor :: (a -> Double) -> SearchResult a -> SearchResult a
        applyFactor f (SearchResult d v) = SearchResult (d * f v) v

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

-- | Being a member of this class means defining the way to represent 
--   your type as pure text so it can be searched by keyword, etc.
class TextSearch a where
    toText :: a -> T.Text

-- | Search term is interpreted as keywords. Results are ranked by the 
--   number of words that appear in the source text, a rank of 0 returns 
--   Nothing.
keywordMatch :: TextSearch a => T.Text -> a -> Maybe (SearchResult a)
keywordMatch t v = go $ fix (toText v) `intersect` fix t

        where
            go [] = Nothing
            go ms = Just $ SearchResult (fromIntegral $ length ms) v

            fix :: T.Text -> [T.Text]
            fix = filter (not . T.null)
                . map T.strip
                . T.words
                . T.toCaseFold
                . T.filter (`notElem` ",.-")
