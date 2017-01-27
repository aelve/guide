{-# LANGUAGE CPP #-}


-- Hack for bug in older Cabal versions
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif


module Guide.SafeCopy
(
  deriveSafeCopySorted,
)
where


import BasePrelude hiding (Version)
import Data.Serialize (getWord8, putWord8, label)
import Data.SafeCopy
import Data.SafeCopy.Internal
import Language.Haskell.TH.Syntax
#if MIN_VERSION_template_haskell(2,8,0)
import Language.Haskell.TH hiding (Kind)
#else
import Language.Haskell.TH hiding (Kind(..))
#endif

-- | Sorts fields (but not constructors), uses 'Simple' encoding, only works
-- on records.
deriveSafeCopySorted :: Version a -> Name -> Name -> Q [Dec]
deriveSafeCopySorted = internalDeriveSafeCopySorted

internalDeriveSafeCopySorted :: Version a -> Name -> Name -> Q [Dec]
internalDeriveSafeCopySorted versionId kindName tyName = do
  info <- reify tyName
  internalDeriveSafeCopySorted' versionId kindName tyName info

internalDeriveSafeCopySorted' :: Version a -> Name -> Name -> Info -> Q [Dec]
internalDeriveSafeCopySorted' versionId kindName tyName info =
  case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD context _name tyvars _kind cons _derivs)
#else
    TyConI (DataD context _name tyvars cons _derivs)
#endif
      | length cons > 255 -> fail $ "Can't derive SafeCopy instance for: " ++ show tyName ++
                                    ". The datatype must have less than 256 constructors."
      | otherwise         -> worker context tyvars (zip [0..] cons)

#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (NewtypeD context _name tyvars _kind con _derivs) ->
#else
    TyConI (NewtypeD context _name tyvars con _derivs) ->
#endif
      worker context tyvars [(0, con)]

    FamilyI _ insts -> do
      decs <- forM insts $ \inst ->
        case inst of
#if MIN_VERSION_template_haskell(2,11,0)
          DataInstD context _name ty _kind cons _derivs ->
#else
          DataInstD context _name ty cons _derivs ->
#endif
              worker' (foldl appT (conT tyName) (map return ty)) context [] (zip [0..] cons)

#if MIN_VERSION_template_haskell(2,11,0)
          NewtypeInstD context _name ty _kind con _derivs ->
#else
          NewtypeInstD context _name ty con _derivs ->
#endif
              worker' (foldl appT (conT tyName) (map return ty)) context [] [(0, con)]
          _ -> fail $ "Can't derive SafeCopy instance for: " ++ show (tyName, inst)
      return $ concat decs
    _ -> fail $ "Can't derive SafeCopy instance for: " ++ show (tyName, info)
  where
    worker = worker' (conT tyName)
    worker' tyBase context tyvars cons =
      let ty = foldl appT tyBase [ varT $ tyVarName var | var <- tyvars ]
#if MIN_VERSION_template_haskell(2,10,0)
          safeCopyClass args = foldl appT (conT ''SafeCopy) args
#else
          safeCopyClass args = classP ''SafeCopy args
#endif
      in (:[]) <$> instanceD (cxt $ [safeCopyClass [varT $ tyVarName var] | var <- tyvars] ++ map return context)
                                       (conT ''SafeCopy `appT` ty)
                                       [ mkPutCopySorted cons
                                       , mkGetCopySorted (show tyName) cons
                                       , valD (varP 'version) (normalB $ litE $ integerL $ fromIntegral $ unVersion versionId) []
                                       , valD (varP 'kind) (normalB (varE kindName)) []
                                       , funD 'errorTypeName [clause [wildP] (normalB $ litE $ StringL (show tyName)) []]
                                       ]

mkPutCopySorted :: [(Integer, Con)] -> DecQ
mkPutCopySorted cons =
    funD 'putCopy (map mkPutClause cons)
  where
    manyConstructors = length cons > 1
    mkPutClause (conNumber, RecC recName (sortFields -> fields)) = do
      arg <- newName "arg"
      let putConNumber = [|putWord8 $(lift conNumber)|]
          putField (field, _, _) = [|safePut ($(varE field) $(varE arg))|]
          putCopyBody = varE 'contain `appE` doE (
                          [ noBindS putConNumber | manyConstructors ] ++
                          [ noBindS (putField f) | f <- fields ] )
      clause [asP arg (recP recName [])] (normalB putCopyBody) []
    mkPutClause (_, con) =
      fail ("Only record constructors are supported: " ++ show (conName con))

mkGetCopySorted :: String -> [(Integer, Con)] -> DecQ
mkGetCopySorted tyName cons =
    valD (varP 'getCopy) (normalB [|contain $mkLabel|]) []
  where
    mkLabel = [|label $(lift labelString) $getCopyBody|]
    labelString = tyName ++ ":"
    getCopyBody = case cons of
      [(_, con)] -> mkGetBody con
      _ -> do
        tagVar <- newName "tag"
        let conMatch (i, con) =
              match (litP $ IntegerL i) (normalB $ mkGetBody con) []
        let noConMatch =
              match wildP (normalB [|fail $(errorMsg tagVar)|]) []
        doE [ bindS (varP tagVar) [|getWord8|]
            , noBindS $ caseE (varE tagVar)
                              (map conMatch cons ++ [noConMatch]) ]
    mkGetBody (RecC recName (sortFields -> fields)) = do
      fieldVars <- mapM newName [nameBase f | (f, _, _) <- fields]
      let getField fieldVar = bindS (varP fieldVar) [|safeGet|]
      let makeRecord = recConE recName
            [(f,) <$> varE v | ((f, _, _), v) <- zip fields fieldVars]
      doE ([ getField v | v <- fieldVars ] ++
           [ noBindS [|return $makeRecord|] ])
    mkGetBody con =
      fail ("Only record constructors are supported: " ++ show (conName con))
    errorMsg tagVar = [|$(lift s1) ++ show $(varE tagVar) ++ $(lift s2)|]
      where
        s1, s2 :: String
        s1 = "Could not identify tag \""
        s2 = concat [ "\" for type "
                    , show tyName
                    , " that has only "
                    , show (length cons)
                    , " constructors. Maybe your data is corrupted?" ]

sortFields :: [VarStrictType] -> [VarStrictType]
-- We sort by length and then lexicographically, so that relative ordering
-- would be preserved when version suffix is added – otherwise these fields
-- would be sorted in different order after adding a suffix:
--
-- foo         fooBar_v3
-- fooBar      foo_v3
sortFields = sortOn (\(n, _, _) -> (length (nameBase n), nameBase n))
