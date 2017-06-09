# Zurihac 2017

## How to build the project

You will need [Stack][] and [npm][].

```
$ stack build         # build the project
$ ./official.sh       # download the database from https://guide.aelve.com
$ stack exec guide    # start Guide
```

After that, the site will be running at <http://localhost:8080/>.

## What to hack on

There's a bunch of issues at <https://github.com/aelve/guide/issues>, but I
recommend taking one of issues listed below.

### Non-coding issues

* You can write content. Take any of the “To be written” categories
  at <https://guide.aelve.com/haskell> and list libraries, write about
  already listed libraries, or give examples. You can look
  at <https://guide.aelve.com/haskell/lenses-sth6l9jl> for an example of a
  more-or-less finished category.

* You can improve the design. This is mostly about CSS and HTML, though, but
  it would still be appreciated. The HTML is mostly generated with [Lucid][],
  though there are some bits written as Mustache templates in the
  `templates/` folder. The styles can be found in `templates/css.widget`.

### Easy issues

* Add a special page listing all broken links (i.e. links returning 400 or
  500).

* Improve analytics. Currently we've got lots of useless referrers (like
  `encrypted.google.com` – see this [analytics screenshot][]). It'd be nice
  if duplicated links were lumped together, Google/Yandex links were parsed
  nicely, etc.

### Medium issues

* We've got search – for instance, here's how [searching for “lens”][] looks.
  However, at the moment the search is somewhat dumb (if you look
  at [`Guide.Search`][] you'll see that it simply does full-text search in
  titles and doesn't do fuzzy matching).

    * One way to improve it would be to add fuzzy matching of some kind, e.g.
      take English morphology into account so that “lenses” would find
      `lens`-the-library.

    * Another thing you can do is implement highlighting for found terms. See
      how GHC User's Guide does highlighting in this [search for “kinds”][].

* Currently, if you edit something and somebody else also edits (and saves
  it) in the process, Guide will show you a popup saying “merge conflict,
  please resolve it”. Unfortunately, it doesn't show the diff between the
  conflicting versions; it'd be nice to highlight differences in them. This
  is a pretty easy task.

* Guide also tries to resolve the merge confict by itself, but the algorithm
  it uses is pretty dumb (see <https://github.com/aelve/guide/issues/91> for
  details). If you want to research merge algorithms and implement a better
  one, it'll be cool.

* Since the database is publicly accessible, it should be possible to write
  an Electron wrapper that would download it and serve it offline. That's a
  pretty good issue in terms of power-to-weight ratio (i.e. it's useful *and*
  easy to do).

* If you look at e.g. [notes for lens][], you'll find that editing them is a
  pain because you can only edit the whole thing at the time. It'd be better
  to allow editing subsections directly (and also code snippets). This is
  somewhat complicated because if someone else edits the section that you
  were also editing, the backend would have to somehow recognize which
  subsection you were editing. This can be done, for instance, by sending two
  pieces of text to the backend – `(original text, modified text)` – and then
  the backend would find the subsection that matches the original text and
  (try to) apply the changes to it.

[Stack]: https://haskellstack.org
[npm]: https://www.npmjs.com/
[Lucid]: https://hackage.haskell.org/package/lucid

[`Guide.Search`]: src/Guide/Search.hs

[search for “kinds”]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html?highlight=kinds#ghc-flag--fprint-explicit-kinds
[searching for “lens”]: https://guide.aelve.com/haskell?q=lens
[notes for lens]: https://guide.aelve.com/haskell/lenses-sth6l9jl#item-notes-ov2yi6mf
[analytics screenshot]: https://github.com/aelve/guide/issues/85#issuecomment-307368459
