{-# LANGUAGE
QuasiQuotes,
OverloadedStrings,
OverloadedLists,
NoImplicitPrelude
  #-}


module SampleState
(
  sampleState,
)
where


-- General
import BasePrelude
-- Containers
import qualified Data.Map as M
-- Text
import qualified Data.Text as T
import NeatInterpolation

-- Local
import Types
import Utils


sampleState :: GlobalState
sampleState = do
  let lensItem = Item {
        _itemUid = "12",
        _itemName = "lens",
        _itemGroup_ = Nothing,
        _itemDescription = "The Swiss army knife of lens libraries",
        _itemPros = [
           Trait "121" [text|
             The most widely used lenses library, by a huge margin.|],
           Trait "123" $ T.unwords $ T.lines [text|
             Contains pretty much everything you could want – while other
             lens libraries mostly only provide lenses for manipulating
             lists, maps, tuples, and standard types like
             `Maybe`/`Either`/etc, lens has functions for manipulating
             filepaths, Template Haskell structures, generics, complex
             numbers, exceptions, and everything else in the Haskell
             Platform.|],
           Trait "125" $ T.unwords $ T.lines [text|
             Unlike most other libraries, has prisms – a kind of lenses
             that can act both as constructors and deconstructors at once.
             They can be pretty useful when you're dealing with exceptions,
             Template Haskell, or JSON.|] ],
        _itemCons = [
           Trait "122" $ T.unwords $ T.lines [text|
             Takes a lot of time to compile, and has a lot of dependencies
             as well.|],
           Trait "124" $ T.unwords $ T.lines [text|
             Some of its advanced features are very intimidating, and the
             whole library may seem overengineered
             (see [this post](http://fvisser.nl/post/2013/okt/11/why-i-dont-like-the-lens-library.html)).|],
           Trait "126" $ T.unwords $ T.lines [text|
             Once you start using lenses for *everything* (which is easier
             to do with lens than with other libraries), your code may start
             not looking like Haskell much
             (see [this post](https://ro-che.info/articles/2014-04-24-lens-unidiomatic)).|] ],
        _itemNotes = [text|
           Get a value:

               > (1,2) ^. _1
               1

           Set a value:

               > (1,2) & _1 .~ 10
               (10, 2)
           |],
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let microlensItem = Item {
        _itemUid = "13",
        _itemName = "microlens",
        _itemGroup_ = Nothing,
        _itemDescription = "A very small lens library",
        _itemPros = [
           Trait "131" $ T.unwords $ T.lines [text|
             Very small (the base package has no dependencies at all,
             and features like Template Haskell lens generation or
             instances for `Vector`/`Text`/`HashMap` are separated into
             other packages).|] ],
        _itemCons = [
           Trait "132" $ T.unwords $ T.lines [text|
             Doesn't provide lens's more advanced features (like prisms
             or indexed traversals).|],
           Trait "134" $ T.unwords $ T.lines [text|
             Doesn't let you write code in fully “lensy” style (since it
             omits lots of operators and `*Of` functions from lens).|] ],
        _itemNotes = "",
        _itemLink = Just "https://github.com/aelve/microlens",
        _itemKind = hackageLibrary }
  let lensesCategory = Category {
        _categoryUid = "1",
        _categoryTitle = "Lenses",
        _categoryNotes = "Lenses are first-class composable accessors.",
        _categoryGroups = mempty,
        _categoryItems = [lensItem, microlensItem] }

  let parsecItem = Item {
        _itemUid = "21",
        _itemName = "parsec",
        _itemGroup_ = Just "parsec-like",
        _itemDescription = "this is Parsec",
        _itemPros = [Trait "211" "the most widely used package",
                     Trait "213" "has lots of tutorials, book coverage, etc"],
        _itemCons = [Trait "212" "development has stagnated"],
        _itemNotes = "",
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let megaparsecItem = Item {
        _itemUid = "22",
        _itemName = "megaparsec",
        _itemGroup_ = Nothing,
        _itemDescription = "this is better than Parsec",
        _itemPros = [Trait "221" "the API is largely similar to Parsec, \
                                 \so existing tutorials/code samples \
                                 \could be reused and migration is easy"],
        _itemCons = [],
        _itemNotes = "",
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let attoparsecItem = Item {
        _itemUid = "23",
        _itemName = "attoparsec",
        _itemGroup_ = Nothing,
        _itemDescription = "this is faster than Parsec",
        _itemPros = [Trait "231" "very fast, good for parsing binary formats"],
        _itemCons = [Trait "232" "can't report positions of parsing errors",
                     Trait "234" "doesn't provide a monad transformer"],
        _itemNotes = "",
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let parsingCategory = Category {
        _categoryUid = "2",
        _categoryTitle = "Parsing",
        _categoryNotes = "Parsers are parsers.",
        _categoryGroups = M.fromList [("parsec-like", Hue 1)],
        _categoryItems = [parsecItem, megaparsecItem, attoparsecItem] }

  -- As many different groups as there are different hues
  let def = Item {
        _itemUid = undefined,
        _itemName = undefined,
        _itemGroup_ = Nothing,
        _itemDescription = "",
        _itemPros = [],
        _itemCons = [],
        _itemNotes = "",
        _itemLink = Nothing,
        _itemKind = hackageLibrary }
  let item1 = def {
        _itemUid = "31",
        _itemName = "api-builder",
        _itemGroup_ = Just "group 1" }
  let item2 = def {
        _itemUid = "32",
        _itemName = "aeson",
        _itemGroup_ = Just "group 2" }
  let item3 = def {
        _itemUid = "33",
        _itemName = "unordered-containers",
        _itemGroup_ = Just "group 1" }
  let item4 = def {
        _itemUid = "34",
        _itemName = "lens",
        _itemGroup_ = Just "group 3" }
  let item5 = def {
        _itemUid = "35",
        _itemName = "bytestring",
        _itemGroup_ = Just "group 4" }
  let item6 = def {
        _itemUid = "36",
        _itemName = "microlens",
        _itemGroup_ = Nothing }
  let item7 = def {
        _itemUid = "37",
        _itemName = "parsec",
        _itemGroup_ = Nothing }
  let huesCategory = Category {
        _categoryUid = "3",
        _categoryTitle = "Testing hues",
        _categoryNotes = "Hopefully they all look good.",
        _categoryGroups =
           M.fromList [("group " <> tshow i, Hue i) | i <- [1..4]],
        _categoryItems = [item1, item2, item3, item4, item5, item6, item7] }

  GlobalState {_categories = [lensesCategory, parsingCategory, huesCategory]}

