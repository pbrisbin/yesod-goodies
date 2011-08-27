{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Goodies.Paginate
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- orignal concept by ajdunlap: 
--      <http://hackage.haskell.org/package/yesod-paginate>
--
-- this version does not use the subsite approach.
--
-------------------------------------------------------------------------------
module Yesod.Goodies.Paginate
    ( 
    -- * automatic
      PageOptions(..)
    , paginate

    -- * manual
    , Page(..)
    , determinePage
    ) where

import Yesod
import qualified Data.Text as T

data PageOptions a s m = PageOptions
    { itemsPerPage :: Int

    -- | How to show a single page's item listing
    , showItems :: [a] -> GWidget s m ()
    }

data Page a = Page
    { thisPage  :: (Int, [a])
    , prevPages :: [Int]
    , nextPages :: [Int]
    } 

paginate :: PageOptions a s m -> [a] -> GWidget s m ()
paginate opts xs = do
    mp <- lift $ lookupGetParam "p"
    let page = case mp of
            Nothing -> determinePage 1 (itemsPerPage opts) xs
            Just "" -> determinePage 1 (itemsPerPage opts) xs
            Just p  -> case readIntegral $ T.unpack p of
                Just i -> determinePage i (itemsPerPage opts) xs
                _      -> determinePage 1 (itemsPerPage opts) xs

    displayPage (showItems opts) page

determinePage :: Int -> Int -> [a] -> Page a
determinePage p per xs = go $ length xs `divPlus` per
    where
        go pages
            | pages <= 1 = Page (1, xs) [] []
            | pages <  p = determinePage pages per xs
            | otherwise  = let items = take per $ drop ((p - 1) * per) xs
                           in  Page (p, items) [1..p-1] [p+1..pages]

        divPlus :: Int -> Int -> Int
        x `divPlus` y = (\(n, r) -> if r == 0 then n else n + 1) $ x `divMod` y

displayPage :: ([a] -> GWidget s m ()) -> Page a -> GWidget s m ()
displayPage doShow (Page (this, items) prev next) = do
    -- make the page listing a bit more apprope
    addCassius [cassius|
        ul.pagination
            margin: 5px 0px;
            padding: 0px;
            text-align: center
        .pagination li
            display: inline
            list-style-type: none
            margin: 0px;
            padding: 0px 3px
            text-align: center
        .pagination li.this_page
            padding: 0px 5px
        |]

    -- limit how many page links are shown
    let prev' = if length prev > 10 then drop (length prev - 10) prev else prev
    let next' = if length next > 10 then take 10 next else next

    -- current GET params
    rgps <- lift $ return . reqGetParams =<< getRequest

    [whamlet|
        ^{doShow items}

        <ul .pagination>
            $if (/=) prev prev'
                <li .previous_pages_more>...

            $forall p <- prev'
                <li .previous_pages>
                    <a href="#{updateGetParam rgps $ mkParam p}">#{show p}

            <li .this_page>#{show this}

            $forall n <- next'
                <li .next_pages>
                    <a href="#{updateGetParam rgps $ mkParam n}">#{show n}

            $if (/=) next next'
                <li .next_pages_more>...

        |]

    where 
        mkParam :: Int -> (T.Text, T.Text)
        mkParam = (,) "p" . T.pack . show

        -- preserves existing get params, updates the passed key/value 
        -- or adds it if it's not there
        updateGetParam :: [(T.Text,T.Text)] -> (T.Text,T.Text) -> T.Text
        updateGetParam getParams (p, n) =
            -- prefix with ? and splice in &
            (T.cons '?') . T.intercalate "&"

            -- join the key, value pairs
            . map (\(k,v) -> k `T.append` "=" `T.append` v)

            -- add/update our key
            . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams
