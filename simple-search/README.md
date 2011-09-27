# Simple search

Provide a generalize way for searching lists of data in-memory (hence, 
simple).

1. Define an instance of `Search` for your type
2. `search` (or `search_`) a list of that type
3. Do something with the results!

~~~ { .haskell }
import qualified Data.Text as T

data Product = Product
    { productId    :: Int
    , productDescr :: String
    }

-- if we instanciate with TextSearch we get keywordMatch for free
instance TextSearch Product where
    toText (Product theId theDescr) = T.pack $ show theId ++ theDescr

instance Search Product where
   match = keywordMatch -- but how to match could be anything your heart 
                        -- desires! 

someData :: [Product]
someData = [ Product 1 "some awesome thing"
           , Product 2 "and the other thing"
           , Product 3 "nothing"
           , ...
           ]

search "awesome thing" someData

-- returns a list with Product 1 and 2 (matched "thing") but product 1 
-- is ranked higher because it matched both "awesome" and "thing".
--
-- there's a lot more you can do but this is a short example.
~~~
