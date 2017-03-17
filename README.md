# Aelve Guide

[![Build status](https://secure.travis-ci.org/aelve/guide.svg)](https://travis-ci.org/aelve/guide)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/aelve/guide/blob/master/LICENSE)

The beta version is running at [guide.aelve.com](https://guide.aelve.com). The most complete section yet is [the one about lenses](https://guide.aelve.com/haskell/lenses-sth6l9jl).

Installation instructions and the explanation of config variables (in `config.json`) are here: [INSTALL.md](INSTALL.md). Don't be afraid to install it locally – it's very easy! You don't need to set up any databases or anything like that, and you can get a full copy of the data from the site by simply cloning it from Github.

## Contributing

If you want to contribute but don't know where to start, grep the source for
`[very-easy]` and `[easy]`, or look at these issues:

  * [“your first pull request”][first pr] – really easy things, with detailed “how to fix it” instructions
  * [“your second pull request”][second pr] – less easy things, which assume that you already know where stuff happens in the code
  * [“not-fleshed-out idea”][ideas] – discussion issues (“should we have users? what better ways are there to present pros and cons?”) which you can help with even if you don't know Haskell
  * [“design”][design] – issues about design (which I'm not good at, and so help is wanted)

[first pr]: https://github.com/aelve/guide/issues?q=is%3Aissue+is%3Aopen+label%3A%22your+first+pull+request%22
[second pr]: https://github.com/aelve/guide/issues?q=is%3Aissue+is%3Aopen+label%3A%22your+second+pull+request%22
[ideas]: https://github.com/aelve/guide/issues?q=is%3Aissue+is%3Aopen+label%3A%22not-fleshed-out+idea%22
[design]: https://github.com/aelve/guide/issues?q=is%3Aissue+is%3Aopen+label%3A%22design%22

## Overview of the code

### Folder structure

  * `lib` – actual code (as a library)
  * `src` – just a dumb `Main.hs` file to compile an executable
  * `tests` – tests
  * `static` – icons, CSS, Markdown pages, HTML pages, and some JS libraries
  * `templates` – HTML templates for pages and elements of pages
  * `scripts` – some scripts used by automatic testing
  * `favicon` – code used to generate a favicon
  * `guidejs` – client side JavaScript

### Notes

When you see something like

~~~ haskell
-- See Note [acid-state]
~~~

it means that there's an extensive comment somewhere else in the code, which you can find by grepping for `Note [acid-state]`. This convention was stolen from GHC. If you know some plugin for Emacs that would help with jumping to comments (even if those comments would have to be formatted differently), let me know.

### Main modules

There are 4 main modules – `Guide.hs`, `JS.hs`, `View.hs`, and `Types.hs`.

`Guide.hs` contains:

  * handlers for GET/POST requests (`renderMethods`, `setMethods`, etc)
  * feed generation (`itemToFeedEntry`)
  * some utility functions (`undoEdit`, `lucidWithConfig`, `createCheckpoint'`)
  * the `main` function, which starts the server

`JS.hs` contains all Javascript that is used in pages. The way it works is tricky: each Javascript function is overloaded in such a way that it can generate either a function call or a function definition. `allJSFunctions`, exported by `JS.hs`, is made by producing definitions from all functions in the module and then concatenating them; later `allJSFunction` is served by the server as `/js.js`. When you see something like `JS.foo (a, b, c)` in Haskell code, a call to `foo` is being generated (and `foo` can be found in `JS.hs`).

`View.hs` contains HTML rendering code. (It's much uglier than using templates, and we should switch to templates one day. Actually, maybe we should even switch to Node.js or Elm from Haskell.)

`Types.hs` contains almost all types used in code elsewhere – `Item`, `Category`, and so on. `GlobalState` is a type for, well, the whole database used by the site. All content, all edits, and all analytics data (i.e. users' IPs, etc) are stored there. The data is held in memory, and acid-state makes sure that data on hard drive is kept in sync with it. For a more detailed explanation, see `Note [acid-state]`.

Currently changing database schema is somewhat painful; to see how exactly painful it is, look at `Note [extending types]`. Not painful enough to never touch it again, sure, but still kinda annoying. Making it easier is possible, but for that we'd need a different `acid-state` or a real database.

### Other modules

`Markdown.hs` contains functions for rendering Markdown. There are special types for Markdown as well, coupling rendered Markdown, its text representation, and its parse tree.

`Merge.hs` contains an algorithm for merging edits – if you are editing a description of an item and someone else is editing the same description, upon submitting your edit you'll get a popup showing both versions and a merged version.

`Cache.hs` provides some helpers for caching pages. (A cache is a global variable holding a map from cache keys to rendered HTML.)

`Config.hs` has a config type and functions for reading/writing said config.

`Utils.hs` is just that – utility functions.
