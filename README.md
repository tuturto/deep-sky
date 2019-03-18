Deep Sky
========

Toy simulation by Tuukka Turto

[User manual](https://tuturto.github.io/deep-sky/) explains basics of the
user interface.

Getting started
---------------

- Install [Stack](https://docs.haskellstack.org/en/stable/README/)
- Install [Elm](https://guide.elm-lang.org/install.html)
- Get [sources](https://github.com/tuturto/deep-sky)
- Build client: `cd elm && elm make src/Main.elm --output=../static/js/client.js`
- Build Yesod binaries: `stack install yesod-bin --install-ghc`
- Build sources: `stack build --flag sky:dev --flag sky:library-only`
- Run development server: `stack exec -- yesod devel`
- Connect to sky.sqlite3 and run seed.sql
- Now application can be accessed at: http://localhost:3000

Released under MIT license
Graphics from https://game-icons.net/ are licensed under [CC BY 3.0](https://creativecommons.org/licenses/by/3.0/)
