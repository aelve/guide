module MarkdownSpec (tests) where

-- Shared imports
import Imports
-- Text
import qualified Data.Text as T
-- HTML
import Lucid (ToHtml, renderText, toHtml)
import Text.HTML.TagSoup hiding (sections)
-- Markdown
import CMark hiding (Node)
import qualified CMark as MD (Node (..))
import CMark.Sections
import Data.Tree
-- Testing
import Test.Hspec

import Guide.Markdown


tests :: Spec
tests = describe "Markdown" $ do
  allMarkdowns $ \convert -> do
    it "has the source filled accurately" $ do
      for_ mdBlockExamples $ \s ->
        s `shouldBe` fst (convert s)
    it "only has allowed tags" $ do
      for_ mdBlockExamples $ \s -> do
        let html = snd (convert s)
        let badTags = getTags html \\ (inlineTags ++ blockTags)
        unless (null badTags) $ expectationFailure $
          printf "%s got rendered as %s, but some tags (%s) are disallowed"
                 (show s) (show html) (T.intercalate "," badTags)
    it "doesn't pass bad HTML through" $ do
      let s = "<script>alert('foo');</script>"
      let html = snd (convert s)
      when ("script" `elem` getTags html) $ expectationFailure $
        printf "%s got rendered as %s, but the <script> tag is bad"
               (show s) (show html)
    it "Hackage links" $ do
      let s = "[text](@hk)"
      let html = snd (convert s)
      let l = "<a href=\"https://hackage.haskell.org/package/text\">text</a>"
      html `shouldSatisfy` (`elem` [l, "<p>"<>l<>"</p>\n"])

  describe "inline Markdown" $ do
    it "works" $ do
      let s = "a*b* `c`"
      htmlToText (toMarkdownInline s) `shouldBe`
        "a<em>b</em> <code>c</code>"
    it "doesn't pass block-level HTML tags" $ do
      let s = "<div>foo</div>"
      htmlToText (toMarkdownInline s) `shouldBe`
        "<code>&lt;div&gt;foo&lt;/div&gt;\n</code>"
    it "is never converted to a paragraph or a block-level element" $ do
      for_ mdBlockExamples $ \s -> do
        let html = htmlToText (toMarkdownInline s)
        let badTags = getTags html \\ inlineTags
        unless (null badTags) $ expectationFailure $
          printf "%s got rendered as %s, but some tags (%s) are disallowed"
                 (show s) (show html) (T.intercalate "," badTags)

  blockMarkdowns $ \convert -> do
    it "works" $ do
      let s = "a*b*\n\n* test"
      snd (convert s) `shouldBe`
        "<p>a<em>b</em></p>\n<ul>\n<li>test</li>\n</ul>\n"
    describe "features" $ do
      let highlighted =
            "<code class=\"sourceCode\">\
            \<span class=\"kw\">module</span> \
            \<span class=\"dt\">M</span> \
            \<span class=\"kw\">where</span></code>"
      it "code highlighting" $ do
        let s = "~~~ haskell\nmodule M where\n~~~\n"
        snd (convert s) `shouldBe`
          "<div class=\"sourceCode\"><pre class=\"sourceCode\">" <>
          highlighted <> "</pre></div>\n"
      it "code classes" $ do
        let s = "~~~ haskell repl\nmodule M where\n~~~\n"
        snd (convert s) `shouldBe`
          "<div class=\"sourceCode\"><pre class=\"sourceCode repl\">" <>
          highlighted <> "</pre></div>\n"
    it "is always converted to a paragraph or a block-level element" $ do
      for_ mdBlockExamples $ \s -> do
        let html = snd (convert s)
        case parseTags html of
          [] -> return ()
          (TagOpen t _ : _) | t `elem` blockTags -> return ()
          (t:_) -> expectationFailure $
            printf "%s got rendered as %s, and %s isn't a block tag"
            (show s) (show html) (show t)

  describe "block+toc Markdown" $ do
    it "renders correctly" $ do
      let s = "x\n\n# foo\n\n## foo\n\ny"
      htmlToText (toMarkdownTree "i-" s) `shouldBe`
        "<p>x</p>\n\
        \<h1><span id=\"i-foo\"></span>foo</h1>\
        \<h2><span id=\"i-foo_\"></span>foo</h2>\
          \<p>y</p>\n"
    it "parses into a tree" $ do
      let s = "x\n\n# foo\n\n## foo\n\ny"
      let prefaceMD = MD.Node (Just (PosInfo 1 1 1 1)) PARAGRAPH
                              [MD.Node Nothing (TEXT "x") []]
          headingMD = MD.Node Nothing (TEXT "foo") []
          foo2MD    = MD.Node (Just (PosInfo 7 1 7 1)) PARAGRAPH
                              [MD.Node Nothing (TEXT "y") []]
      markdownTreeStructure (toMarkdownTree "i-" s) `shouldBe` Document {
        prefaceAnn = "<p>x</p>\n",
        preface    = WithSource "x\n\n" [prefaceMD],
        sections   = [
          Node {rootLabel = Section {
                   level      = 1,
                   heading    = WithSource "# foo\n\n" [headingMD],
                   headingAnn = "i-foo",
                   content    = WithSource "" [],
                   contentAnn = ""},
                subForest = [
                   Node {rootLabel = Section {
                            level      = 2,
                            heading    = WithSource "## foo\n\n" [headingMD],
                            headingAnn = "i-foo_",
                            content    = WithSource "y\n" [foo2MD],
                            contentAnn = "<p>y</p>\n"},
                         subForest = [] }]}]}
    it "has a correct TOC" $ do
      let s = "x\n\n# foo\n\n## foo\n\ny"
      markdownTreeTOC (toMarkdownTree "i-" s) `shouldBe` [
        Node {rootLabel = Heading (toMarkdownInline "# foo\n\n") "i-foo",
              subForest = [
                 Node {rootLabel = Heading (toMarkdownInline "## foo\n\n") "i-foo_",
                       subForest = [] }]}]

getTags :: Text -> [Text]
getTags html = nub [t | TagOpen t _ <- parseTags html]

htmlToText :: ToHtml a => a -> Text
htmlToText = toText . renderText . toHtml

allMarkdowns :: ((Text -> (Text, Text)) -> Spec) -> Spec
allMarkdowns f = do
  describe "inline MD" $
    f ((markdownInlineSource &&& htmlToText) . toMarkdownInline)
  blockMarkdowns f

blockMarkdowns :: ((Text -> (Text, Text)) -> Spec) -> Spec
blockMarkdowns f = do
  describe "block MD" $
    f ((markdownBlockSource &&& htmlToText) . toMarkdownBlock)
  describe "block+toc MD" $
    f ((markdownTreeSource &&& htmlToText) . toMarkdownTree "")

mdInlineExamples :: [Text]
mdInlineExamples = [
  "", "\n", "\n\n",
  "x", "x*y*",
  "x\n", "\nx", "\nx\n", "\n\n x\n\n",
  "x\ny", "x\ny\n",
  "<http://x.com>",
  "`foo` `bar`"
  ]

mdBlockExamples :: [Text]
mdBlockExamples = mdInlineExamples ++ [
  "x\n\ny", "x\n\ny\n", "x\n\ny\n\n",
  "* x", "* x\n* y\n",
  "> blah",
  "[foo]: http://x.com", "foo\n\n[foo]: http://x.com",
  "# foo", "## foo",
  "<p>blah</p>",
  "---",
  "~~~ haskell\nfoo\n~~~"
  ]

inlineTags :: [Text]
inlineTags = T.words "a strong em code span"

blockTags :: [Text]
blockTags = T.words "p ul li blockquote h1 h2 h3 h4 h5 h6 hr div pre"
