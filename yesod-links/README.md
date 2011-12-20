# Yesod links

~~~ { .haskell }
import Yesod.Links

instance YesodLinked MySite where
    type Linked = MySite
~~~

Make concise link-widgets by defining how things should be represented 
as links.

~~~ { .haskell }
--
-- * Make linking to any specific route simpler
--
instance IsLink MySiteRoute where
    toLink RootR  = Link (Internal RootR)  "go home"         "home"
    toLink AboutR = Link (Internal AboutR) "about this site" "about"

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
    [whamlet|

        be sure to visit our ^{link AboutR} page.

        |]

--
-- * Use it for more than just routes
--
data Post = Post
    { postSlug  :: Text
    , postTitle :: Text
    , postDescr :: Text
    }

instance IsLink Post where
    toLink (Post s t d) = Link (Internal $ PostR s) d t

getIndexR :: Handler RepHtml
getIndexR = defaultLayout do
    [whamlet|

        <ul>
            $forall post <- posts
                <li>^{link post}
        
        |]

--
-- * Bypass the IsLink instance and use the raw link' function for 
--   external links
--
getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ do
    [whamlet|
        
        be sure to checkout my ^{link' github} profile.

        |]

    where
        github :: Link MySite
        github = Link (External "https://github.com/pbrisbin") "my github repos" "github"
~~~
