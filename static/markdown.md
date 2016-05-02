# Markdown

Markdown is supported in most places; inline Markdown in pros/cons (i.e. you can't create lists, paragraphs, etc) and ordinary Markdown elsewhere. If you haven't written Markdown before, see [this reference][Commonmark ref].

[Commonmark ref]: http://commonmark.org/help/

## Custom features

You can write `[pkg](@hk)` to get a link to package `pkg` on Hackage (the full list of shortcuts is [here][shortcuts]).

[shortcuts]: https://hackage.haskell.org/package/shortcut-links/docs/ShortcutLinks-All.html

If you mark snippets of code with `repl` (to show that they come from GHCi), they will display differently:

<table width="100%"><tr><td width="45%">

~~~~
~~~ hs repl
> 2+2
4
~~~
~~~~

</td><td width="10%">

<div style="text-align:center;font-size:400%">→</div>

</td><td width="45%">

~~~ hs repl
> 2+2
4
~~~

</td></tr></table>

If something doesn't come from GHCi, but is still Haskell code, use `hs` or `haskell` (other languages are supported too):

<table width="100%"><tr><td width="45%">

~~~~
~~~ hs
t :: Bool
t = True
~~~
~~~~

</td><td width="10%">

<div style="text-align:center;font-size: 400%">→</div>

</td><td width="45%">

~~~ hs
t :: Bool
t = True
~~~

</td></tr></table>
