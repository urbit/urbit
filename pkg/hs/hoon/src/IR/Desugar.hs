module IR.Desugar where

import ClassyPrelude hiding (union)

import IR.Ty
import Control.Lens

import Data.Foldable      (foldr1)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Char          (ord)
import Text.Show.Pretty   (pPrint)

import qualified AST.Parser  as AST
import qualified AST.Types   as AST
import qualified Data.Map    as Map
import qualified Data.Set    as Set
import qualified IR.Infer    as IR
import qualified IR.Ty       as IR
import qualified LL.Run      as LL
import qualified LL.Types    as LL
import qualified Prelude
import qualified System.Exit as Sys

--------------------------------------------------------------------------------

list :: [IR.Hoon] -> IR.Hoon
list []     = HAtom 0
list (x:xs) = HCons x (list xs)

tuple :: NonEmpty IR.Hoon -> IR.Hoon
tuple (x :| [])     = x
tuple (x :| y : zs) = HCons x (tuple (y :| zs))

baseToTy :: AST.Base -> Ty
baseToTy = \case
  AST.BVoid -> bot
  AST.BNull -> tyNull
  AST.BFlag -> tyBool
  AST.BNoun -> top
  AST.BCell -> tyAnyCell `union` tyAnyCore
  AST.BAtom -> tyAnyAtom

gateTy :: Ty -> Ty -> Ty -> Ty
gateTy ctx arg result = tyCore batt ctx
  where
    batt = mapFromList [("", result)]
    ctx  = tyCell arg ctx

specToTy :: AST.Spec -> Ty
specToTy = \case
  AST.SBase b       -> baseToTy b
  AST.SFaceOp f s   -> specToTy (AST.SBucTis f s)
  AST.STuple specs  -> specToTy (AST.SBucCol specs)
  AST.SBucCol specs -> foldr1 tyCell (specToTy <$> specs)
  AST.SBucHep a r   -> gateTy top (specToTy a) (specToTy r) -- TODO Ctx type
  AST.SBucTis s y   -> let y' = specToTy y
                       in  y' { tFace = Set.insert s (tFace y') }
  AST.SBucWut specs -> foldr1 union (specToTy <$> specs)
  AST.SBucPat x y   -> specToTy x `union` specToTy x
  AST.SBucKet x y   -> specToTy x `union` specToTy y
  AST.SBucCen specs -> foldr1 union (specToTy <$> specs)

armNm "" = "$"
armNm nm = nm

arm :: Sym -> AST.Hoon -> Either String (Ty, AST.Hoon)
arm _  (AST.KetHep s h) = pure (specToTy s, h)
arm nm _                = Left msg
  where msg = mconcat [ "Arm ", armNm nm, " needs a type declaration" ]

axisPath :: Nat -> Maybe TreePath
axisPath 0 = Nothing
axisPath 1 = Just []
axisPath 2 = Just [False]
axisPath 3 = Just [True]
axisPath n
  | 0==(n `rem` 2) = (++) <$> axisPath (n `quot` 2) <*> axisPath 2
  | otherwise      = (++) <$> axisPath (n `quot` 2) <*> axisPath 3

mbErr :: String -> Maybe a -> Either String a
mbErr err = \case Nothing -> Left err
                  Just a  -> pure a

wing :: AST.Wing -> Either String Wing
wing = traverse \case
  Left a  -> WAxis <$> mbErr "+0 is not valid" (axisPath a)
  Right n -> Right (WName n)

core :: Map Sym AST.Hoon -> Either String (Map Sym (Ty, AST.Hoon))
core = Map.traverseWithKey arm

desugar :: AST.Hoon -> Either String IR.Hoon
desugar = \case
  AST.Sig            -> pure (HAtom 0)
  AST.No             -> pure (HAtom 1)
  AST.Yes            -> pure (HAtom 0)
  AST.Bar            -> pure (HAtom 1)
  AST.Pam            -> pure (HAtom 0)
  AST.Hep            -> pure (HRef [WAxis [False]])
  AST.Lus            -> pure (HRef [WAxis [True]])
  AST.IsEqIrr x y    -> desugar (AST.IsEq x y)
  AST.IsEq x y       -> HEq <$> desugar x <*> desugar y
  AST.IncrIrr x      -> desugar (AST.Incr x)
  AST.Incr x         -> HSucc <$> desugar x
  AST.Tape t         -> pure (list (HAtom . ord <$> unpack t))
  AST.Cord _         -> pure (HAtom 1337)
  AST.Atom a         -> pure (HAtom a)
  AST.Wing ss        -> HRef <$> wing ss
  AST.FaceOp n h     -> desugar (AST.KetTis n h)
  AST.KetTis n h     -> HFace n <$> desugar h
  AST.Tupl xs        -> desugar (AST.ColTar xs)
  AST.ColOp x y      -> desugar (AST.TisGal x y)
  AST.BarCen arms    -> do bat <- core arms >>= traverse (traverseOf _2 desugar)
                           pure (HCore bat)
  AST.TisDot w x y   -> do x <- desugar x
                           y <- desugar y
                           w <- wing w
                           pure (HWith
                                  (HEdit w
                                    (HLike (HRef w) x)
                                    (HRef []))
                                   y)
  AST.BarTis s h     -> desugar (AST.TisGar
                                  (AST.Tupl [s, AST.Wing []])
                                  (AST.BarCen (mapFromList [("", h)])))
  AST.BarHep x       -> do (t, x') <- arm "" x
                           x'' <- desugar x'
                           pure (HWith
                                  (HCore (mapFromList [("", (t, x''))]))
                                  (HRef [WName ""]))
  AST.TisGal x y     -> desugar (AST.TisGar y x)
  AST.TisGar x y     -> HWith <$> desugar x <*> desugar y
  AST.KetHep s x     -> HCast (specToTy s) <$> desugar x
  AST.ColSig xs      -> list <$> traverse desugar xs
  AST.ColTar []      -> Left "empty tuple"
  AST.ColTar (x:xs)  -> tuple <$> traverse desugar (x :| xs)
  AST.ColHep x y     -> desugar (AST.ColTar [x, y])
  AST.ColLus x y z   -> desugar (AST.ColTar [x, y, z])
  AST.ColKet x y z a -> desugar (AST.ColTar [x, y, z, a])
  AST.WutTis s h     -> do h <- desugar h
                           pure (HNest (Pat $ specToTy s) h)
  AST.WutKet x y z   -> desugar (AST.WutPat x z y)
  AST.WutPat x y z   -> do x <- desugar x
                           y <- desugar y
                           z <- desugar z
                           pure (HIf (HNest (Pat tyAnyAtom) x) y z)
  AST.WutCol x y z -> HIf <$> desugar x <*> desugar y <*> desugar z

getRightAndShow :: Show r => (l -> Text) -> Either l r -> IO r
getRightAndShow err = \case
  Left  e -> putStrLn (err e) >> Sys.exitFailure
  Right x -> pPrint x >> putStrLn "" >> pure x

main :: IO ()
main = do
  ex  <- Prelude.head <$> getArgs
  putStrLn "== Parsing =="
  ast <- getRightAndShow id (AST.parse ex)
  putStrLn "== Desugaring =="
  ir  <- getRightAndShow pack (desugar ast)
  putStrLn "== Type Inferring =="
  ty  <- getRightAndShow pack (IR.infer tyNull ir)
  putStrLn "== Compiling =="
  ll  <- getRightAndShow pack (IR.down tyNull ir)
  putStrLn "== Result Type =="
  _   <- getRightAndShow pack (Right (ll ^. LL.llTy))
  putStrLn "== Result =="
  res <- getRightAndShow pack (LL.runLL (LL.VAtom 0) ll)
  pure ()

sugar :: LL.LL a -> AST.Hoon
sugar = \case
  LL.LWith _ x y   -> AST.ColOp (sugar y) (sugar x)
  LL.LAxis _ p     -> undefined
  LL.LEdit _ p x y -> AST.TisDot undefined undefined undefined
  LL.LFire _ s _   -> AST.Wing [Right s]
  LL.LAtom _ n     -> AST.Atom n
  LL.LPair _ x y   -> AST.Tupl [sugar x, sugar y]
  LL.LCore _ _bat  -> undefined
  LL.LSucc _ x     -> AST.IncrIrr (sugar x)
  LL.LTest _ x y z -> AST.WutCol (sugar x) (sugar y) (sugar z)
  LL.LCelQ _ x     -> AST.WutKet (sugar x) (AST.Atom 0) (AST.Atom 1)
  LL.LEqlQ _ x y   -> AST.IsEqIrr (sugar x) (sugar y)
