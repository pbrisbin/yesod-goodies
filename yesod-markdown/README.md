# Yesod markdown

~~~ { .haskell }
import Yesod.Markdown
~~~

This is a fork, simplification, continuation of the [package][] that 
currently exists on hackage by Alexander Dunlap.

It is a small wrapper over pandoc's `Markdown -> Html` support with 
usage tailored for Yesod.

That package has slipped out of date and does not work with recent 
releases of pandoc or yesod.

This package differs in the following ways:

1. updated to compile with newer dependencies
2. removed `Yesod.Markdown.Macros`
3. fixed and exported form field settings for `Markdown` fields
4. uses xss-sanitize by default and provides `*Trusted` functions to 
   skip it
5. a few additional helper methods added

### Usage

~~~ { .haskell }
getPageR :: FilePath -> Handler RepHtml
getPageR filepath = do
    -- content is Html
    content <- markdownToHtml =<< liftIO (markdownFromFile filepath)

    defaultLayout do
        [shamlet|
            <div class="content">
                #{content}
            |]
~~~

[package]: http://hackage.haskell.org/package/yesod-markdown
