# Yesod Goodies

A collection of small helper functions useful in any yesod application.

### Details

`yesod-goodies` itself is just a meta package which re-exports the 
packages contained here. This new layout of sub-packages is not yet on 
hackage as there are a few name-clashes I need to think about dealing 
with.

Please see the individual READMEs for what each package does:

[friendly-time][]

[gravatar][]

[shorten-strings][]

[simple-search][]

[yesod-links][]

[yesod-markdown][]

[yesod-paginator][]

[friendly-time]:   https://github.com/pbrisbin/yesod-goodies/blob/master/friendly-time/README.md
[gravatar]:        https://github.com/pbrisbin/yesod-goodies/blob/master/gravatar/README.md
[shorten-strings]: https://github.com/pbrisbin/yesod-goodies/blob/master/shorten-strings/README.md
[simple-search]:   https://github.com/pbrisbin/yesod-goodies/blob/master/simple-search/README.md
[yesod-links]:     https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-links/README.md
[yesod-markdown]:  https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-markdown/README.md
[yesod-paginator]: https://github.com/pbrisbin/yesod-goodies/blob/master/yesod-paginator/README.md

### Name-clashes

`gravatar` exists as version 0.1, 0.2, or 0.3 on hackage by Don Stewart. 
My package (0.4) is a little bit more featureful (I wrote it knowing 
nothing about the existing one). I've reached out to see if I can take 
over development of this module on hackage.

`yesod-markdown` exists as version 0.0, 0.1, or 0.2.1 on hackage by 
Alexander Dunlap. It has not had much development lately and does not 
compile on newer pandocs and yesods. My package (0.3) is a 
simplification of his existing package, it has a lot of the same code 
but was trimmed down and a few helper functions added.

### Installation

For now, this can only be installed through git:

~~~ { .bash }
git clone http://github.com/pbrisbin/yesod-goodies.git

cd yesod-goodies

./bin/install
~~~

Note that all of the packages can be used by themselves except for 
`yesod-markdown` which requires `shorten-strings` (so that `Markdown` 
types can be shortened). I'm not sure yet if this is a Good Thing.

Installation for a single package can be done by changing to its 
directory and executing `cabal install`.
