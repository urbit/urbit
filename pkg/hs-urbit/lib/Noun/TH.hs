{-
    Generate FromNoun and ToNoun instances.
-}

module Noun.TH (deriveNoun, deriveToNoun, deriveFromNoun) where

import ClassyPrelude              hiding (fromList)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Noun.Convert

import Noun.Core (textToUtf8Atom)

import qualified Data.Char as C


--------------------------------------------------------------------------------

type ConInfo = (Name, [Type])

data Shape
    = Vod
    | Tup ConInfo
    | Enu [(String, Name)]
    | Sum [(String, ConInfo)]
  deriving (Eq, Ord, Show)

typeShape :: Name -> Q ([TyVarBndr], Shape)
typeShape tyName = do
    (vars, cs) <-
        reify tyName >>= \case
            TyConI (DataD _ nm vars _ cs _)   -> pure (vars, unpackCon <$> cs)
            TyConI (NewtypeD _ nm vars _ c _) -> pure (vars, [unpackCon c])
            TyConI _                          -> fail badSynonym
            _                                 -> fail "not type"

    let allEmpty = all (null . snd) cs
        prefix   = getPrefix (nameStr . fst <$> cs)

    pure $ (vars,) $ case cs of
        []            -> Vod
        cs | allEmpty -> Enu (tagName prefix . fst <$> cs)
        [c]           -> Tup c
        cs            -> Sum (tagConInfo prefix <$> cs)

  where
    badSynonym = "deriveFunctor: tyCon may not be a type synonym."

    tagConInfo :: Int -> ConInfo -> (String, ConInfo)
    tagConInfo pre ci@(nm, _) = (tagString pre nm, ci)

    tagName :: Int -> Name -> (String, Name)
    tagName pre n = (tagString pre n, n)

    tyStr   = nameStr tyName
    tyAbbrv = filter C.isUpper tyStr

    typePrefixed  = (tyStr `isPrefixOf`)
    abbrvPrefixed = (tyAbbrv `isPrefixOf`)

    getPrefix :: [String] -> Int
    getPrefix cs | all typePrefixed cs  = length tyStr
    getPrefix cs | all abbrvPrefixed cs = length tyAbbrv
    getPrefix _                         = 0

    unpackCon :: Con -> ConInfo
    unpackCon = \case
      NormalC nm bangTypes          -> (nm, snd <$> bangTypes)
      RecC nm varBangTypes          -> (nm, varBangTypes <&> (\(_, _, t) -> t))
      InfixC bangType1 nm bangType2 -> error "Infix Cnstrs are not supported"
      ForallC tyVarBndrs ctx con    -> error "Polymorphic tys are not supported"
      GadtC nm bangTypes ty         -> error "GADTs are not supported"
      RecGadtC nm varBangTypes ty   -> error "GADTs are not supported"

--------------------------------------------------------------------------------

deriveNoun :: Name -> Q [Dec]
deriveNoun n = (<>) <$> deriveToNoun n <*> deriveFromNoun n

--------------------------------------------------------------------------------

deriveToNoun :: Name -> Q [Dec]
deriveToNoun tyName = do
    (params, shape) <- typeShape tyName

    let exp = case shape of Vod      -> vodToNoun
                            Tup con  -> tupToNoun con
                            Enu cons -> enumToAtom cons
                            Sum cons -> sumToNoun cons

    params <- pure $ zip ['a' ..] params <&> \(n,_) -> mkName (singleton n)

    let ty = foldl' (\acc v -> AppT acc (VarT v)) (ConT tyName) params

    let overlap = Nothing
        body    = NormalB exp
        ctx     = params <&> \t -> AppT (ConT ''ToNoun) (VarT t)
        inst    = AppT (ConT ''ToNoun) ty

    pure [InstanceD overlap ctx inst [ValD (VarP 'toNoun) body []]]

--------------------------------------------------------------------------------

addErrTag :: String -> Exp -> Exp
addErrTag tag exp =
    InfixE (Just $ AppE (VarE 'named) str) (VarE (mkName ".")) (Just exp)
  where
    str = LitE $ StringL tag

deriveFromNoun :: Name -> Q [Dec]
deriveFromNoun tyName = do
    (params, shape) <- typeShape tyName

    let exp = case shape of Vod      -> vodFromNoun
                            Tup con  -> tupFromNoun con
                            Enu cons -> enumFromAtom cons
                            Sum cons -> sumFromNoun cons

    params <- pure $ zip ['a' ..] params <&> \(n,_) -> mkName (singleton n)

    let ty = foldl' (\acc v -> AppT acc (VarT v)) (ConT tyName) params

    let overlap = Nothing
        body    = NormalB (addErrTag (nameStr tyName) exp)
        ctx     = params <&> \t -> AppT (ConT ''FromNoun) (VarT t)
        inst    = AppT (ConT ''FromNoun) ty

    pure [InstanceD overlap ctx inst [ValD (VarP 'parseNoun) body []]]

enumFromAtom :: [(String, Name)] -> Exp
enumFromAtom cons = LamE [VarP x] body
  where
    (x, c)    = (mkName "x", mkName "c")
    getTag    = BindS (VarP c) $ AppE (VarE 'parseNounUtf8Atom) (VarE x)
    examine   = NoBindS $ CaseE (VarE c) (matches ++ [fallback])
    matches   = mkMatch <$> cons
    fallback  = Match WildP (NormalB $ AppE (VarE 'fail) matchFail) []
    body      = DoE [getTag, examine]
    matchFail = LitE $ StringL ("Expected one of: " <> possible)
    possible  = intercalate " " (('%':) . fst <$> cons)
    mkMatch   = \(tag, nm) ->
      Match (SigP (LitP $ StringL tag) (ConT ''Text))
            (NormalB $ AppE (VarE 'pure) (ConE nm))
            []

applyE :: Exp -> [Exp] -> Exp
applyE e []     = e
applyE e (a:as) = applyE (AppE e a) as

vodFromNoun :: Exp
vodFromNoun = LamE [WildP] body
  where
    body = AppE (VarE 'fail)
         $ LitE $ StringL "Can't FromNoun on uninhabited data type"

tupFromNoun :: ConInfo -> Exp
tupFromNoun (n, tys) = LamE [VarP x] body
  where
    x       = mkName "x"
    vars    = mkName . singleton . fst <$> zip ['a'..] tys
    body    = DoE [getTup, convert]
    convert = NoBindS $ AppE (VarE 'pure) $ applyE (ConE n) (VarE <$> vars)
    getTup  = BindS (TupP $ VarP <$> vars) $ AppE (VarE 'parseNoun) (VarE x)

unexpectedTag :: [String] -> Exp -> Exp
unexpectedTag expected got =
    applyE (VarE 'mappend) [LitE (StringL prefix), AppE (VarE 'unpack) got]
  where
    possible  = intercalate " " (('%':) <$> expected)
    prefix    = "Expected one of: " <> possible <> " but got %"

sumFromNoun :: [(String, ConInfo)] -> Exp
sumFromNoun cons = LamE [VarP n] (DoE [getHead, getTag, examine])
  where
    (n, h, t, c) = (mkName "noun", mkName "hed", mkName "tel", mkName "tag")

    getHead = BindS (TupP [VarP h, VarP t])
            $ AppE (VarE 'parseNoun) (VarE n)

    getTag = BindS (SigP (VarP c) (ConT ''Text))
           $ AppE (VarE 'parseNounUtf8Atom) (VarE h)

    examine = NoBindS
            $ CaseE (VarE c) (matches ++ [fallback])

    matches = mkMatch <$> cons
    mkMatch = \(tag, (n, tys)) ->
                let body = AppE (addErrTag ('%':tag) (tupFromNoun (n, tys)))
                                (VarE t)
                in Match (LitP $ StringL tag) (NormalB body) []

    fallback  = Match WildP (NormalB $ AppE (VarE 'fail) matchFail) []
    matchFail = unexpectedTag (fst <$> cons) (VarE c)

--------------------------------------------------------------------------------

tagString :: Int -> Name -> String
tagString prefix = hsToHoon . drop prefix . nameStr

nameStr :: Name -> String
nameStr (Name (OccName n) _) = n

tagNoun :: String -> Exp
tagNoun = AppE (VarE 'textToUtf8Atom)
        . LitE
        . StringL

tagTup :: String -> [Name] -> Exp
tagTup tag args = AppE (VarE 'toNoun) $ TupE (tagNoun tag : fmap VarE args)

tup :: [Name] -> Exp
tup = AppE (VarE 'toNoun) . TupE . fmap VarE

--------------------------------------------------------------------------------

vodToNoun :: Exp
vodToNoun = enumToAtom [] -- LamE [WildP]

enumToAtom :: [(String, Name)] -> Exp
enumToAtom cons =
    LamCaseE $ cons <&> \(tag, nm) ->
                 Match (ConP nm []) (NormalB $ tagNoun tag) []

tupToNoun :: ConInfo -> Exp
tupToNoun cons = LamCaseE [mkMatch cons]
  where
    mkMatch :: ConInfo -> Match
    mkMatch (nm, tys) = Match (ConP nm params) (NormalB body) []
      where vars   = (zip tys ['a'..]) <&> (mkName . singleton . snd)
            params = VarP <$> vars
            body   = tup vars

sumToNoun :: [(String, ConInfo)] -> Exp
sumToNoun cons = LamCaseE (cons <&> mkMatch)
  where
    mkMatch :: (String, ConInfo) -> Match
    mkMatch (tag, (nm, tys)) =
        Match (ConP nm params) (NormalB body) []
      where vars   = (zip tys ['a'..]) <&> (mkName . singleton . snd)
            params = VarP <$> vars
            body   = tagTup tag vars

--------------------------------------------------------------------------------

hsToHoon :: String -> String
hsToHoon = go []
  where
    go acc []     = intercalate "-" $ reverse acc
    go acc (c:cs) = go (elem:acc) remain
      where
        head           = C.toLower c
        (tail, remain) = break C.isUpper cs
        elem           = head:tail
