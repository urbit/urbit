module Practice.RenderDH3Orphans where

import ClassyPrelude

import Practice.DependentHoon3 hiding (Line, chip)
import qualified Practice.DependentHoon3 as DH3
import Practice.Hoon2DependentHoon3
import Practice.HoonCommon
import Practice.HoonSyntax
import Practice.Render

instance Rolling Soft where
  roll = roll . shut

instance Show a => Rolling (Code a) where
  roll = roll . rest

instance Show a => Rolling (Base a) where
  roll = roll . lock

instance Rolling Pelt where
  roll = roll . flap

instance Rolling [Act] where
  roll as = Huge $ Rose "trace:" "" $ map (tank . roll) $ reverse as

baseToHoon lvl bas = shut $ rest $ loft lvl bas

rollSL lvl sl = roll case sl of
  SeL r -> Clhp Wild (baseToHoon lvl r)
  SeR l -> Clhp (baseToHoon lvl l) Wild

rollTL lvl tl = roll case tl of
  TyL b c -> Tsgr (baseToHoon lvl b) $ Bccl Wild [shut $ rest c]
  TyR b -> Bccl (baseToHoon lvl b) [Wild]
  TyF f -> Ktts (flap $ mred f) Wild

rollSLs lvl sls =
  Huge $ Stem "" "" [] (sls <&> \sl -> ("$$", tank $ rollSL lvl sl, Leaf ""))

rollTLs lvl tls =
  Huge $ Stem "" "" [] (tls <&> \tl -> ("**", tank $ rollTL lvl tl, Leaf ""))

instance Rolling ActTree where
  roll = \case
    ActTree a cs -> Huge $ Rose "" ""
      [ tank $ roll a
      , Rose "" "" $ map (tank . roll) $ reverse cs
      ]
    ActNote n -> roll n

instance Rolling Act where
  roll = \case
    ActRoot -> leaf "root"
    ActFits f t u -> Huge $ Stem (tshow f <> ":") "" []
      [ ("have", tank $ roll t, Leaf "")
      , ("need", tank $ roll u, Leaf "")
      ]
    ActSeal Con{..} DH3.Line{lem, lyt, sez, tez} -> Huge $ Stem "seal:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("sez ", tank $ rollSLs lvl sez, Leaf "")
      , ("tez ", tank $ rollTLs lvl tez, Leaf "")
      , ("lem ", tank $ roll lem, Leaf "")
      , ("lyt ", tank $ roll lyt, Leaf "")
      ]
    ActFind Con{lvl, sut, ken} w -> Huge $ Stem "find:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("wing", tank $ roll w,    Leaf "")
      ]
    ActMeld b c -> Huge $ Stem "meld:" "" []
      [ ("base", tank $ roll b, Leaf "")
      , ("diff", tank $ roll b, Leaf "")
      ]
    ActFuse Con{lvl, sut, ken} (b, t) p -> Huge $ Stem "fuse:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("semi", tank $ roll b,    Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      , ("skin", tank $ roll p,    Leaf "")
      ]
    ActCrop Con{lvl, sut, ken} t p -> Huge $ Stem "fuse:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("type", tank $ roll $ loft lvl sut,    Leaf "")
      , ("skin", tank $ roll p,    Leaf "")
      ]
    ActFish p -> Huge $ Stem "fish:" "" []
      [ ("skin", tank $ roll p, Leaf "")
      ]
    ActToil Con{lvl, sut, ken} f p t -> Huge $ Stem "toil:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("mode", Leaf $ tshow f,   Leaf "")
      , ("skin", tank $ roll p,    Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      ]
    ActRomp Con{lvl, sut, ken} p -> Huge $ Stem "romp:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("skin", tank $ roll p,    Leaf "")
      ]
    ActWork Con{lvl, sut, ken} f c t -> Huge $ Stem "work:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("mode", Leaf $ tshow f,   Leaf "")
      , ("code", tank $ roll c,    Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      ]
    ActPlay Con{lvl, sut, ken} c -> Huge $ Stem "play:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("ken ", tank $ roll $ loft lvl ken,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("code", tank $ roll c, Leaf "")
      ]
    ActDone -> leaf "done"

instance Rolling Fail where
  roll = \case
    PareFree r b -> Huge $ Stem "pare-free:" "" []
      [ ("rump", Leaf $ tshow r, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    SealSync Con{lvl} sez tez -> Huge $ Stem "seal-sync:" "" []
      [ ("sez ", tank $ rollSLs lvl sez, Leaf "")
      , ("tez ", tank $ rollTLs lvl tez, Leaf "")
      ]
    FindFail f t -> Huge $ Stem ("find." <> printLimb f) "" []
      [ ("type", tank $ roll t, Leaf "")
      ]
    FitsFail f t u -> Huge $ Stem (tshow f <> "-fail:") "" []
      [ ("have", tank $ roll t, Leaf "")
      , ("need", tank $ roll u, Leaf "")
      ]
    MeldFail b c -> Huge $ Stem "meld-fail:" "" []
      [ ("onto", tank $ roll b, Leaf "")
      , ("unto", tank $ roll c, Leaf "")
      ]
    FuseFail (b, t) p -> Huge $ Stem "fuse-fail:" "" []
      [ ("base", tank $ roll b, Leaf "")
      , ("type", tank $ roll t, Leaf "")
      , ("skin", tank $ roll p, Leaf "")
      ]
    FuseFits f -> leaf ("fuse-" <> tshow f)
    CropFail t p -> Huge $ Stem "crop-fail:" "" []
      [ ("type", tank $ roll t, Leaf "")
      , ("skin", tank $ roll p, Leaf "")
      ]
    CropFits f -> leaf ("crop-" <> tshow f)
    FishSame s -> Huge $ Stem "crop-fail:" "" []
      [ ("hoon", tank $ roll s, Leaf "")
      ]
    FishPike p q -> Huge $ Stem "fish-pike:" "" []
      [ ("skin", tank $ roll p, Leaf "")
      , ("skin", tank $ roll q, Leaf "")
      ]
    ToilFish p t -> Huge $ Stem "toil-fish:" "" []
      [ ("skin", tank $ roll p, Leaf "")
      , ("type", tank $ roll t, Leaf "")
      ]
    RompWild p -> Huge $ Palm "romp-wild:" [tank $ roll p]
    NeedGate t -> Huge $ Palm "need-gate:" [tank $ roll t]
    WorkMiss s b -> Huge $ Stem "work-miss:" "" []
      [ ("test", tank $ roll s, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    PlayMiss s b -> Huge $ Stem "play-miss:" "" []
      [ ("test", tank $ roll s, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    BailNote t -> Huge $ Palm "bail-note:" [Leaf t]
    BailFail -> leaf "bail-fail"

instance Rolling Note where
  roll = \case
    NoteType msg t -> Huge $ Stem "note:" "" []
      [ ("text", Leaf msg,      Leaf "")
      , ("type", tank $ roll t, Leaf "")
      ]
    NoteBase msg b -> Huge $ Stem "note:" "" []
      [ ("text", Leaf msg,      Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    NoteCode msg c -> Huge $ Stem "note:" "" []
      [ ("text", Leaf msg,      Leaf "")
      , ("code", tank $ roll c, Leaf "")
      ]
