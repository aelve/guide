module Guide.Common.CSS.Global where

import Prelude

import Bulma.Common (ClassName, ClassPart(..), joinClassParts, toClassName)
import Data.Monoid (mempty)

------------------------------------------------
-- header classes
 ------------------------------------------------

header :: ClassName
header = toGuideClassName $ ClassPart "header"

------------------------------------------------
-- category classes
------------------------------------------------

catClass :: ClassPart -> ClassName
catClass cp =
  toGuideClassName $ joinClassParts [ClassPart "category", cp]

catColumn :: ClassName
catColumn = catClass $ ClassPart "column"

catLink :: ClassName
catLink = catClass $ ClassPart "link"

catHeadline :: ClassName
catHeadline = catClass $ ClassPart "headline"

catSubheadline :: ClassName
catSubheadline = catClass $ ClassPart "subheadline"

catsClass :: ClassPart -> ClassName
catsClass cp =
  toGuideClassName $ joinClassParts [ClassPart "categories", cp]

------------------------------------------------
-- categories classes
------------------------------------------------

catsFinished :: ClassName
catsFinished = catsClass $ ClassPart "finished"

catsWip :: ClassName
catsWip = catsClass $ ClassPart "wip"

catsStub :: ClassName
catsStub = catsClass $ ClassPart "stub"

------------------------------------------------
-- footer classes
------------------------------------------------

footerClass :: ClassPart -> ClassName
footerClass cp =
  toGuideClassName $ joinClassParts [ClassPart "footer", cp]

footerContainer :: ClassName
footerContainer = footerClass $ ClassPart "container"

------------------------------------------------
-- Helpers
------------------------------------------------

toGuideClassName :: ClassPart -> ClassName
toGuideClassName cp =
  toClassName $ joinClassParts [ClassPart "guide", cp]
