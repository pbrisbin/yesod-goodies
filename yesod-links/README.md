# Yesod links

Make concise link-widgets by defining how routes can be represented as 
links.

~~~ { .haskell }
-- required to provide widgets containing your site's routes
instance YesodLinked MySite where
    type Linked = MySite

-- declare link-specific info about routes
IsLink MySiteRoute where
    toLink RootR  = Link (Internal RootR)  "go home"         "home"
    toLink AboutR = Link (Internal AboutR) "about this site" "about"

-- link to them
getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    [whamlet|

        be sure to visit our ^{link AboutR} page.

        |]

-- you can bypass the IsLink instance and use the raw link' function for 
-- external links
getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    [whamlet|
        
        be sure to checkout my ^{link' github}

        |]

    where
        github :: Link MySite
        github = Link (External "https://github.com/pbrisbin") "my github repos" "github"
~~~
