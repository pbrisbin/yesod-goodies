# Yesod paginator

~~~ { .haskell }
import Yesod.Paginator
~~~

Do you have some `[Thing]` that you've already selected out of your DB, 
maybe composed from multiple tables, and you just want to paginate this 
big list?

~~~ { .haskell }
getPageR :: Handler RepHtml
getPageR = do
    things' <- getAllThings

    -- note: things will be the same type as things'
    (things, widget) <- paginate 10 things'

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing thing}

            ^{widget}
            |]
~~~

Do you have a single table of records and you want to paginate them, 
selecting only the records needed to display the current page?

~~~ { .haskell }
getPageR :: Handler RepHtml
getPageR = do
    -- note: things is [(Key, Val)] just like selectList returns
    (things, widget) <- selectPaginated 10 [] []

    defaultLayout $ do
        [whamlet|
            $forall thing <- things
                ^{showThing $ snd thing}

            ^{widget}
            |]
~~~

The result (hideously un-styled as it were):

![Screen Shot](http://pbrisbin.com/static/screenshots/desktop_1112192057.png)

### Installation

    git clone https://github.com/pbrisbin/yesod-goodies
    cd yesod-goodies/yesod-paginator
    cabal install

You could also install all of yesod-goodies using the provided 
`bin/install` from that directory.
