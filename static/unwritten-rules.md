# Unwritten rules

If you disagree with any of these “rules”, ignore them! I don't think they are right, I just think they are better than what people would be doing otherwise if they didn't have any opinions on the matter.

For custom Markdown features we have, see [here](/markdown).

## What to include

Sort things by importance and don't try to write *everything*. The problem is that when you see 2 columns titled “pros” and “cons”, it makes people want to try and come up with as many pros/cons as possible (well, actually I don't know about *people*, but it does happen to me all the time, so it probably happens to others too). This should be avoided at all costs. Same with items themselves (e.g. libraries) – don't list all related libraries, list all libraries that could conceivably be chosen. The test is simple: if you can't say under what conditions a person would choose A over B, and B is already on the list, don't add A. I can very well relate to the vague feeling of “but A deserves to be mentioned too”, but still, don't.

On the other hand, there's another problem that should somehow be avoided. Let's take the section about books – RWH and LYAH are often recommended to people, but there's Haskell Programming From First Principles (that book at [haskellbook.com](http://haskellbook.com)) and it's regarded to be Totally Better Than LYAH And RWH Put Together. Even despite the fact that the former 2 are free and the latter one isn't, all 3 books still have to be mentioned – otherwise, how is the reader going to know that LYAH was omitted because it's bad and not because everyone forgot to write about it? That's the problem that comes up very often, with lists like [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/) and [State Of The Haskell Ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md) – there are people who wouldn't trust them not because they don't trust the author's good intentions, but because they are afraid that the author simply didn't list something worth mentioning. This leads to endless Google searches and traversals of related Hackage categories (and there are way too many packages on Hackage).

In a nutshell:

  * don't list things that aren't really distinguishable from the already listed things
  * do list things that people are likely to be *considering*, even if they aren't likely to actually choose them

## Descriptions

Unhelpful descriptions are unhelpful. For instance, here are bad [descriptions](http://www.alternative.to/category/internet/browsers) of several browsers:

> Opera is a web browser and internet suite developed by the Opera Software company. The browser handles common Internet-related tasks such as [...]

> Mozilla Firefox is a free, open source, cross-platform, graphical web browser developed by the Mozilla Corporation and hundreds of volunteers. [...]

> Google Chrome is a web-browser from Google released on September 2nd, 2008. There are three main areas on which Google intends to improve the [...]

These aren't descriptions, these are blurbs. Who the hell cares that Chrome was released on September 2nd? Why mention hundreds of volunteers? Who on earth invented the phrase “common Internet-related tasks”? The purpose of these “descriptions” has nothing to do with trying to help the reader.

A description should convey information that isn't conveyed elsewhere. For instance, the reason the library/book/etc was created – e.g. “this is a fork of X” suggests that there are some problems with X, and for someone they were important enough to bother to fork it. What's more interesting, it's a *stronger* signal than saying “X has such-and-such problems” explicitly. Everything has problems; not everything has forks.

A description should also summarise the pros/cons – “use this if X; don't use this if Y”. Yes, it doesn't give any new information, but it still helps the reader.

## Code examples

When giving long code examples, don't do this:

~~~~
Here we define data types:

~~~ hs
data X = X
data Y = Y
~~~

And here we define a conversion:

~~~ hs
xToY :: X -> Y
xToY X = Y
~~~
~~~~

This makes it harder to copy the code (e.g. if you want to play with it). Instead, write comments as comments, but still break code into sections:

~~~~
~~~ hs
-- Here we define data types
data X = X
data Y = Y
~~~

~~~ hs
-- And here we define a conversion
xToY :: X -> Y
xToY X = Y
~~~
~~~~

### “Ecosystem” fields

  * When the field becomes long, use bullet points
  * Don't forget to emphasise meaningful parts: “[**zlib**-lens](@hk)” instead of “[zlib-lens](@hk)”
  * When it's hard to understand what the package does just by its name, clarify it in parens: “[lens-properties](@hk) (for Quickcheck)”
  * When only a part of a package is relevant (e.g. one module), link to that module instead of the whole package
