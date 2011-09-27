# Yesod Goodies

A collection of small helper functions useful in any yesod application.

### Details

`yesod-goodies` itself is just a meta package which re-exports the 
packages contained here. This new layout of sub-packages is not yet on 
hackage as there are a few name-clashes I need to think about dealing 
with.

Please see the individual readmes for what each package does:

[friendly-time][]
[gravatar][]
[shorten-strings][]
[simple-search][]
[yesod-links][]
[yesod-markdown][]
[yesod-paginate][]

[friendly-time]:   https://github.com/pbrisbin/yesod-goodies/blob/master/friendly-time/README.md
[gravatar]:        https://github.com/pbrisbin/yesod-goodies/blob/master/gravatar/README.md
[shorten-strings]: https://github.com/pbrisbin/yesod-goodies/blob/master/shorten-strings/README.md
[simple-search]:   https://github.com/pbrisbin/yesod-goodies/blob/master/simple-search/README.md
[yesod-links]:     https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-links/README.md
[yesod-markdown]:  https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-markdown/README.md
[yesod-paginate]:  https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-paginate/README.md


### Name-clashes

`gravatar` exists as version 0.1, 0.2, or 0.3 on hackage by Don Stewart. 
My package is a little bit more featureful (I wrote it knowing nothing 
about the existing one), so I think if I reach out to dons about it, he 
might consider merging or just letting me take over at version 0.4.

`yesod-paginate` and `yesod-markdown` exist on hackage but have not had 
much development lately. They're both by Alexander Dunlap.

My `yesod-paginate` is only inspired by the existing package, it takes a 
very different approach and shares no code.

My `yesod-markdown` is a simplification of his existing package, it has 
a lot of the same code but was trimmed down and a few helper functions 
added.

### Installation

For now, this can only be installed through git:

~~~ { .bash }
#!/bin/bash -e

git clone http://github.com/pbrisbin/yesod-goodies.git

cd yesod-goodies

pkgs=( friendly-time
       gravatar
       shorten-strings
       simple-search
       yesod-links
       yesod-markdown
       yesod-paginate
     )

# install each sub-package
for pkg in "${pkgs[@]}"; do
  (
    cd "$pkg"
    cabal install
  )
done

# install the meta package
cabal install
~~~

Some day when I get some time, I'll reach out to the other developers or 
otherwise find some way to get all this up on hackage.

Note that all of the packages can be used by themselves except for 
`yesod-markdown` which requires `shorten-strings` (so that `Markdown` 
types can be shortened.)
