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

## Testing

You need `chromedriver` and `selenium-server-standalone` installed (those are the package names on Arch Linux). Then you can do

```
$ java -jar /usr/share/selenium-server/selenium-server-standalone.jar
```

```
$ stack test
```

## Notes

When you see something like

~~~ haskell
-- See Note [acid-state]
~~~

it means that there's an extensive comment somewhere else in the code, which you can find by grepping for `Note [acid-state]`. This convention was stolen from GHC. If you know some plugin for Emacs that would help with jumping to comments (even if those comments would have to be formatted differently), let me know.
