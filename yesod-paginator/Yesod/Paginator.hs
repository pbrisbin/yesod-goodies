{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  Yesod.Paginator
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
-- There are two pagination functions. One for arbitrary items where you
-- provide the list of things to be paginated, and one for paginating
-- directly out of the database, you provide the same filters as you
-- would to @selectList@.
--
-- Both functions return a tuple: the first part being the list of items
-- to display on this page and the second part being a widget of the
-- list of links to previous and next pages.
--
-- A GET param "p" is handled for you to manage the pagination.
--
-------------------------------------------------------------------------------
module Yesod.Paginator
    ( paginate
    , selectPaginated
    ) where

import Yesod
import Data.Text (Text)
import qualified Data.Text as T

-- | Paginate a list of items. This is useful when the things you're 
--   paginating are complex data types that come from more than one
--   table and you already have a loader function or they are part of
--   your foundation type.
--
--   The downside is that the entire list must be held in memory at
--   once.
--
--   > getSomeRoute = do
--   >     things' <- getAllThings
--   >
--   >     (things, widget) <- paginate 10 things'
--   >
--   >     defaultLayout $ do
--   >         [whamlet|
--   >             $forall thing <- things
--   >                 ^{showThing thing}
--   >
--   >             ^{widget}
--   >             |]
--
paginate :: Int -- ^ items per page
         -> [a] -- ^ complete list of items
         -> GHandler s m ([a], GWidget s m ())
paginate per items = do
    let pages = length items `divPlus` per

    if pages <= 1
        then return $ (items, pageWidget 1 pages)
        else do
            p <- getCurrentPage pages
            let items' = take per $ drop ((p - 1) * per) items

            return (items', pageWidget p pages)

-- | Paginate directly out of the database, this uses an OFFSET and
--   LIMIT to hold only the current page's data in memory for any given
--   request.
--
--   The downside is that the select can only return data from one
--   table.
--
--   This function accepts the same arguments as @selectList@ and what
--   @selectList@ would have returned is the first component of the
--   tuple.
--
--   > getSomeRoute something = do
--   >     -- note: things is [(key, value)] just like selectList returns
--   >     (things, widget) <- selectPaginated 10 [SomeThing ==. something] []
--   >
--   >     defaultLayout $ do
--   >         [whamlet|
--   >             $forall thing <- things
--   >                 ^{showThing $ snd thing}
--   >
--   >             ^{widget}
--   >             |]
--
selectPaginated :: (YesodPersist m,
                    PersistEntity v,
                    PersistBackend (YesodPersistBackend m) (GGHandler s m IO))
                => Int           -- ^ items per page
                -> [Filter v]    -- ^ filters
                -> [SelectOpt v] -- ^ additional select opts (Asc/Desc)
                -> GHandler s m ([(Key (YesodPersistBackend m) v, v)], GWidget s m ())
selectPaginated per filters selectOpts = do
    pages <- fmap (`divPlus` per) $ runDB $ count filters

    if pages <= 1
        then do
            items <- runDB $ selectList filters selectOpts
            return (items, pageWidget 1 pages)

        else do
            p     <- getCurrentPage pages
            items <- runDB $ selectList filters (selectOpts ++ [OffsetBy ((p-1)*per), LimitTo per])

            return (items, pageWidget p pages)

-- | Parse the p GET param, guards against p > total
getCurrentPage :: Int -> GHandler s m Int
getCurrentPage tot = do
    mp <- lookupGetParam "p"
    return $
        case mp of
            Nothing -> 1
            Just "" -> 1
            Just p  ->
                case readIntegral $ T.unpack p of
                    Just i -> if i > tot then tot else i
                    _      -> 1

-- | Integer division plus one to capture remainder
divPlus :: Int -> Int -> Int
x `divPlus` y = (\(n, r) -> if r == 0 then n else n + 1) $ x `divMod` y

-- | The page selection widget with current page in middle and links to
--   previous/next pages
pageWidget :: Int -- ^ current page
           -> Int -- ^ total number of pages
           -> GWidget s m ()
pageWidget cur tot = do
    let prev = [1     .. cur-1]
    let next = [cur+1 .. tot  ]

    let limit = 9 -- don't show more than nine links on either side
    let prev' = if length prev > limit then drop ((length prev) - limit) prev else prev
    let next' = if length next > limit then take limit next else next

    -- current request GET parameters
    rgps <- lift $ return . reqGetParams =<< getRequest

    [whamlet|
        <div .pagination>
            <ul>
                $if (/=) prev prev'
                    <li .first>
                        <a href="#{updateGetParam rgps $ mkParam 1}">First

                $forall p <- prev'
                    <li>
                        <a href="#{updateGetParam rgps $ mkParam p}">#{show p}

                <li .active>
                    <a href="#">#{show cur}

                $forall n <- next'
                    <li>
                        <a href="#{updateGetParam rgps $ mkParam n}">#{show n}

                $if (/=) next next'
                    <li .last>
                        <a href="#{updateGetParam rgps $ mkParam tot}">Last

        |]

    where
        mkParam :: Int -> (Text, Text)
        mkParam = (,) "p" . T.pack . show

        -- preserves existing get params, updates the passed key/value 
        -- or adds it if it's not there
        updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
        updateGetParam getParams (p, n) =
            -- prefix with ? and splice in &
            (T.cons '?') . T.intercalate "&"

            -- join the key, value pairs
            . map (\(k,v) -> k `T.append` "=" `T.append` v)

            -- add/update our key
            . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams
