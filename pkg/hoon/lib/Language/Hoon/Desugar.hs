module Language.Hoon.Desugar (desugar) where

import Prelude

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Map as Map

import Language.Hoon.Nock.Types
import Language.Hoon.Types
import Language.Hoon.SpecToMold
import Language.Hoon.SpecToBunt

-- open:ap
desugar :: Bool -> Hoon -> BHoon
desugar fab = go
  where
    -- things that are already desugared
    go (HAutocons hs) = BAutocons (map go hs)
    go (HDebug s h) = BDebug s (go h)
    go (Hand t nk) = BHand t nk
    -- but open:ap also strips note
    go (Note note h) = BNote note (go h)
    go (Fits h w) = BFits (go h) w
    go (Sand n nou) = BSand n nou
    go (Rock n nou) = BRock n nou
    go (Tune t) = BTune t
    go (Lost h) = BLost (go h)
    --
    go (BarCen n b) = BBarCen n (Map.map (Map.map (\(w, h) -> (w, go h))) b)
    go (BarPat n b) = BBarPat n (Map.map (Map.map (\(w, h) -> (w, go h))) b)
    --
    go (CenTis w cs) = BCenTis w (map (\(w, h) -> (w, go h)) cs)
    --
    go (DotKet s h) = BDotKet s (go h)
    go (DotLus h) = BDotLus (go h)
    go (DotTar h j) = BDotTar (go h) (go j)
    go (DotTis h j) = BDotTis (go h) (go j)
    go (DotWut h) = BDotWut (go h)
    --
    go (KetBar h) = BKetBar (go h)
    go (KetCen h) = BKetCen (go h)
    go (KetLus h j) = BKetLus (go h) (go j)
    go (KetPam h) = BKetPam (go h)
    go (KetSig h) = BKetSig (go h)
    go (KetWut h) = BKetWut (go h)
    --
    go (SigGar hint mh j) = BSigGar hint (fmap go mh) (go j)
    go (SigZap h j) = BSigZap (go h) (go j)
    --
    go (TisGar h j) = BTisGar (go h) (go j)
    go (TisCom h j) = BTisCom (go h) (go j)
    --
    go (WutCol h j k) = BWutCol (go h) (go j) (go k)
    go (WutHax s w) = BWutHax s w
    --
    go (ZapCom h j) = BZapCom (go h) (go j)
    go (ZapMic h j) = BZapMic (go h) (go j)
    go (ZapTis h) = BZapTis (go h)
    go (ZapPat ws h j) = BZapPat ws (go h) (go j)
    go ZapZap = BZapZap


    go (H_ axis) = (BCenTis [AxisLimb axis] [])
    --
    go (HBase basetype) = go (specToMold fab (SBase basetype))
    go (Bust basetype) = go (specToBunt fab (SBase basetype))
    go (KetCol spec) = go (specToMold fab spec)
    -- writen into open:ap even though mint:ut uses handles debug case directly
    -- go (Debug h) = go h
    go (Error msg) = error ("%slog.[0 leaf/" ++ msg ++ "]")
    --
    go (Knit woofs) = error "TODO: implement %knit desugar"
    --
    go (HLeaf name atom) = go (specToMold fab (SLeaf name atom))
    go (Limb name) = (BCenTis [NameLimb name] [])
    go (Wing wing) = (BCenTis wing [])
    go (Tell hs) = go (CenCol (Limb "noah") [ZapGar (ColTar hs)]) 
    go (Yell hs) = go (CenCol (Limb "cain") [ZapGar (ColTar hs)])
    go (Xray _) = error "TODO: %xray not implemented" 
    --
    -- TODO implement bars
    --
    go (ColKet h1 h2 h3 h4) = BAutocons (map go [h1, h2, h3, h4])
    go (ColLus h1 h2 h3)    = BAutocons (map go [h1, h2, h3])
    go (ColCab h1 h2)       = BAutocons (map go [h2, h1])
    go (ColHep h1 h2)       = BAutocons (map go [h1, h2])
    go (ColSig hs) = BAutocons (map go hs ++ [BRock "n" (Atom 0)])
    go (ColTar (h :| hs)) = BAutocons (go h : map go hs)
    --
    go (KetTar spec) = BKetSig (go (specToBunt fab spec))
    --
    -- CenTis, but the product is cast to the type of the old value
    go (CenCab wing changes) = BKetLus (go (Wing wing))
                                       (BCenTis wing (desugarChanges changes))
    go (CenDot h1 h2)       = go (CenCol h2 [h1])
    go (CenKet h1 h2 h3 h4) = go (CenCol h1 [h2, h3, h4])
    go (CenLus h1 h2 h3)    = go (CenCol h1 [h2, h3])
    go (CenHep h1 h2)       = go (CenCol h1 [h2])
    -- the implementation that "probably should work, but doesn't"
    go (CenCol h hs)
        = go (CenTar [NameLimb ""] h [([AxisLimb 6], HAutocons hs)])
    -- in lieu of the "electroplating" implementation
    go (CenSig wing h hs) = go (CenTar wing h [([AxisLimb 6], HAutocons hs)])
    go (CenTar wing h changes)
        | null changes = BTisGar (go (Wing wing)) (go h)
        | otherwise = go (TisLus h (CenTis (wing ++ [AxisLimb 2]) changes))
    --
    go (KetDot h j) = BKetLus (go (CenCol h [j])) (go j)
    go (KetHep spec h) = BKetLus (go (specToBunt fab spec)) (go h)
    go (KetTis skin h) = go (grip skin h)
    --
    go (SigBar h j) = BSigGar "mean" (Just (hint h)) (go j)
      where
        hint (Sand "tas" x) = (BRock "tas" x)
        hint (HDebug _ h) = hint h
        hint h = go (BarDot (CenCol (Limb "cain") [ZapGar (TisGar (H_ 3) h)]))
    go (SigCab h j) = BSigGar "mean" (Just (go (BarDot h))) (go j)
    go (SigCen chum h tyre j) = error "desugar: TODO ~% not supported"
    go (SigFas chum h) = error "desugar: TODO ~/ not supported"
    go (SigLed n mh j) = BTisGar (go j) (BSigGar n (fmap go mh) (go (H_ 1)))
    go (SigBuc name h)
        = BSigGar "live" (Just (BRock "" (nameToAtom name))) (go h)
    go (SigLus atom h) = BSigGar "memo" (Just (BRock "" (Atom atom))) (go h)
    go (SigPam atom h j)
        = BSigGar
            "slog"
            (Just (BAutocons [BSand "" (Atom atom)
                             , go (CenCol (Limb "cain") [ZapGar j])]))
            (go j) 
    go (SigTis h j) = BSigGar "germ" (Just (go h)) (go j)
    go (SigWut atom h j k)
        = go (TisLus
            (WutDot j (Bust SNull) (HAutocons [Bust SNull, k]))
            (WutSig
                [AxisLimb 2]
                (TisGar (H_ 3) k)
                (SigPam atom (H_ 5) (TisGar (H_ 3) k))))
    --
    -- TODO mic runes
    -- 
    go (TisBar h j) = go (TisLus (specToBunt fab h) j)
    --go (TisTar name Nothing h j) = BTisGar (BTune Tone) j
    --go (TisTar name (Just spec) h j) = undefined
    go (TisCol chs h) = BTisGar (go (CenCab [AxisLimb 1] chs)) (go h)
    go (TisFas skin h j) = go (TisLus (KetTis skin h) j)
    go (TisDot w h j) = BTisGar (go (CenCab [AxisLimb 1] [(w, h)])) (go j)
    go (TisWut w h j k) = go (TisDot w (WutCol h j (Wing w)) k)
    --go (TisKet skin wing h j)
    --    = BTisGar
    --        (go (KetTis 
    go (TisLed h j) = BTisGar (go j) (go h)
    go (TisLus h j) = BTisGar (go (HAutocons [h, (H_ 1)])) (go j)
    go (TisHep h j) = go (TisLus j h)
    go (TisSig hs) = foldr BTisGar (go (H_ 1)) (map go hs)
    --
    go (WutBar hs)
        = foldr
            (\h r -> BWutCol (go h) (BRock "f" (Atom 0)) r)
            (BRock "f" (Atom 1))
            hs
    go (WutPam hs)
        = foldr
            (\h r -> BWutCol (go h) r (BRock"f" (Atom 1)))
            (BRock "f" (Atom 0))
            hs
    go (WutDot h j k) = BWutCol (go h) (go k) (go j)
    go (WutLed h j) = BWutCol (go h) BZapZap (go j)
    go (WutGar h j) = BWutCol (go h) (go j) BZapZap
    go (WutKet wing h j)
        = BWutCol (go (WutTis (SBase (SAtom "")) wing)) (go h) (go j)
    
    --go (WutKet
    --
    go (ZapWut (Left vers) h)
        | vers >= hoonVersion = go h
        | otherwise = error "hoon-version"
    go (ZapWut (Right (lower, upper)) h)
        | lower <= hoonVersion && hoonVersion <= upper = go h
        | otherwise = error "hoon-version"
    
    desugarChanges = map (\(w, h) -> (w, go h)) 

grip :: Skin -> Hoon -> Hoon
grip = error "grip not implemented"
    


