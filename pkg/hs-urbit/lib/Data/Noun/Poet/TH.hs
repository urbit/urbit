{-
    Generate FromNoun and ToNoun instances
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module Data.Noun.Poet.TH where

import ClassyPrelude hiding (fromList)
import Control.Lens
import Data.Noun.Poet hiding (hsToHoon)

import Data.Noun
import Data.Noun.Atom
import Data.Noun.Pill
import Data.Void
import Data.Word
import GHC.Natural
import GHC.Generics hiding (from)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Prelude      as P
import qualified GHC.Generics as GHC

import Data.Typeable (Typeable)
import RIO           (decodeUtf8Lenient)

import qualified Data.Char          as C
import qualified Control.Monad.Fail as Fail


data Foo = Foo Int | Bar Int


-- Types For Hoon Constructs ---------------------------------------------------

-- data Deriving = Deriving { tyCon :: Name, tyVar :: Name }

data Shape
    = Tup Name Con
    | Enu Name [Name]
    | Sum Name [Con]
  deriving (Eq, Ord, Show)

nameStr :: Name -> String
nameStr (Name (OccName n) _) = n

typeShape :: Name -> Q Shape
typeShape tyName = do
    (tyConName, tyVars, cs) <- reify tyName >>= \case
       TyConI (DataD _ nm tyVars _ cs _)   -> pure (nm, tyVars, cs)
       TyConI (NewtypeD _ nm tyVars _ c _) -> pure (nm, tyVars, [c])
       TyConI _                            -> fail badSynonym
       _                                   -> fail "not type"

    allEmpty <- all id <$> (traverse emptyCon cs)

    if allEmpty
    then do
      conNames :: [Name] <- traverse conName' cs
      pure (Enu tyConName conNames)
    else
      case cs of
        []  -> pure $ Enu tyConName []
        [c] -> pure $ Tup tyConName c
        cs  -> pure $ Sum tyConName cs

  where
    badSynonym = "deriveFunctor: tyCon may not be a type synonym."

conName' :: Con -> Q Name
conName' = \case
    NormalC nm bangType           -> pure nm
    RecC nm varBangTypes          -> pure nm
    InfixC bangType1 nm bangType2 -> fail "Infix constructors are not supported"
    ForallC tyVarBndrs ctx con    -> fail "Polymorphic types are not supported"
    GadtC nm bangTypes ty         -> fail "GADTs are not supported"
    RecGadtC nm varBangTypes ty   -> fail "GADTs are not supported"

emptyCon :: Con -> Q Bool
emptyCon = \case
    NormalC nm bangType           -> pure (null bangType)
    RecC nm varBangTypes          -> pure (null varBangTypes)
    InfixC bangType1 nm bangType2 -> fail "Infix constructors are not supported"
    ForallC tyVarBndrs ctx con    -> fail "Polymorphic types are not supported"
    GadtC nm bangTypes ty         -> fail "GADTs are not supported"
    RecGadtC nm varBangTypes ty   -> fail "GADTs are not supported"


deriveNoun :: Name -> Q [Dec]
deriveNoun tyName = do
  (<>) <$> deriveToNoun tyName <*> deriveFromNoun tyName

deriveToNoun :: Name -> Q [Dec]
deriveToNoun tyName = do
    let t = conT tyName

    shape <- typeShape tyName

    traceM (show shape)

    body <- case shape of
              Tup nm con -> pure [| \_ -> Atom 0 |]
              Enu nm cons -> enumToAtom cons
              Sum nm cons -> pure [| \_ -> Atom 0 |]

    [d|
        instance ToNoun $t where
            toNoun = $body
      |]

enumToAtom :: [Name] -> Q ExpQ
enumToAtom cons = do
    matches <- traverse mkMatch cons
    pure (pure (LamCaseE matches))
  where
    mkMatch :: Name -> Q Match
    mkMatch nm = pure $ Match (ConP nm []) (NormalB body) []
      where
        body   = AppE (VarE 'toNoun) $ AppE (ConE 'Cord) strLit
        strLit = LitE $ StringL $ unpack $ hsToHoon $ nameStr nm

deriveFromNoun :: Name -> Q [Dec]
deriveFromNoun tyName =
    [d|
        instance FromNoun $t where
            parseNoun = $body
    |]
  where
    t = conT tyName

    body = [| \_ -> fail "unimplemented" |]


{-
    (tyConName, tyVars, cs) <- reify tyName >>= \case
       TyConI (DataD _ nm tyVars _ cs _)   -> pure (nm, tyVars, cs)
       TyConI (NewtypeD _ nm tyVars _ c _) -> pure (nm, tyVars, [c])
       TyConI _                            -> fail badSynonym
       _                                   -> fail "not type"

    let KindedTV tyVar StarT = P.last tyVars

    let instanceType = (conT ''ToNoun)
                       `appT`
                       varT (foldl' apply (conT tyConName) (P.init tyVars))

    putQ $ Deriving tyConName tyVar
    sequence [instanceD (pure []) instanceType [genToNoun cs]]
-}

{-
    apply t (PlainTV name)    = appT t (varT name)
    apply t (KindedTV name _) = appT t (varT name)

    badSynonym = "deriveFunctor: tyCon may not be a type synonym."

    genToNoun :: [Con] -> DecQ
    genToNoun cons = funD 'toNoun (genToNounClause <$> cons)

    genToNounClause :: Con -> Q Clause
    genToNounClause c@(NormalC name fieldTypes) = do
        f          <- newName "f"
        fieldNames <- replicateM (length fieldTypes) (newName "x")

        let pats = varP f:[conP name (map varP fieldNames)]
            body = normalB $ appsE $
              conE name : map (newField f) (zip fieldNames fieldTypes)

        clause pats body []
    genToNounClause _ = fail "wut"
-}

{-
newField :: Name -> (Name, StrictType) -> Q Exp
newField f (x, (_, fieldType)) = do
  Just (Deriving typeCon typeVar) <- getQ
  case fieldType of
    VarT typeVar' | typeVar' == typeVar ->
      [| $(varE f) $(varE x) |]
    ty `AppT` VarT typeVar' |
      leftmost ty == (ConT typeCon) && typeVar' == typeVar ->
        [| fmap $(varE f) $(varE x) |]
    _ -> [| $(varE x) |]

leftmost :: Type -> Type
leftmost (AppT ty1 _) = leftmost ty1
leftmost ty           = ty
-}

hsToHoon :: String -> Text
hsToHoon = go []
  where
    go acc []     = pack $ intercalate "-" $ reverse acc
    go acc (c:cs) = go (elem:acc) remain
      where
        head           = C.toLower c
        (tail, remain) = break C.isUpper cs
        elem           = head:tail
