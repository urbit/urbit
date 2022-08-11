module Practice.RenderDH4Orphans where

import ClassyPrelude

import Practice.DependentHoon4
import Practice.Hoon2DependentHoon4
import Practice.HoonCommon
import Practice.HoonSyntax
import Practice.Render hiding (Line)

instance Rolling Soft where
  roll = roll . shut

instance Var a => Rolling (Code a) where
  roll = roll . rest

instance Var a => Rolling (Semi a) where
  roll = roll . lock

instance Var a => Rolling (Con a) where
  roll Con{lvl, sut} = Huge $ Stem "Con" "" []
    [ ("lvl ", Leaf $ tshow lvl, Leaf "")
    , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
    ]

instance Var a => Rolling (Line a) where
  roll Line{lev, loc, lyt, las} = Huge $ Stem "Line" "" []
    [ ("loc ", Leaf $ tshow loc, Leaf "")
    , ("lyt ", tank $ roll $ loft lev lyt,  Leaf "")
    , ("las ", tank $ rollDashes lev las,  Leaf "")
    ]

instance Rolling Stub where
  roll = leaf . tshow

instance Rolling Pelt where
  roll = roll . flap

instance Rolling Fish where
  roll = roll . pond

instance (Ord a, Rolling a) => Rolling (Set a) where
  roll fs = Huge $ Stem "" "" []
    (setToList fs <&> \f -> (">>", tank $ roll f, Leaf ""))

instance Rolling [Fish] where
  roll fs = Huge $ Stem "" "" []
    (fs <&> \f -> (">>", tank $ roll f, Leaf ""))

rollEdge lvl = \case
  Ends -> []
  Eons t j -> ("//", tank $ roll $ loft lvl t, Leaf "")
            : rollEdge lvl (j (rump (lvl, 1)))

instance Var a => Rolling (Cube a) where
  roll Cube{lvv, mat, hed} = Huge $ Stem "Cube" "" []
    [ ( "mat "
      , Stem "" "" [] (mat <&> \fs -> ("??", tank $ roll fs, Leaf ""))
      , Leaf ""
      )
    , ( "hed "
      , Stem "" "" [] (rollEdge lvv hed)
      , Leaf ""
      )
    ]

instance Rolling Warp where
  roll Warp{lax,rax,pax} = Huge $ Stem "Warp" "" []
    [ ("lax ", tank $ roll lax, Leaf "")
    , ("rax ", tank $ roll rax, Leaf "")
    , ("pax ", tank $ roll pax, Leaf "")
    ]

instance Var a => Rolling (Weft a) where
  roll Weft{seg, reg, gil} = Huge $ Stem "Weft" "" []
    [ ("seg ", tank $ roll seg, Leaf "")
    , ("reg ", tank $ roll reg, Leaf "")
    , ("gil ", tank $ roll gil, Leaf "")
    ]

instance Rolling [Act] where
  roll as = Huge $ Rose "trace:" "" $ map (tank . roll) $ reverse as

rollDash lvl = roll . \case
  DashFace f -> Ktts (flap $ mred f) Wild
  -- DashFork hs -> Bcgr Wild (map (flap . pond) $ setToList hs)
  DashSing s -> Bcts (baseToHoon lvl s) Wild
  DashMolt t -> Bcmc (baseToHoon lvl t) Wild
  DashSeal a -> Bcwt [Axis a] Wild
  DashCellLeft tr -> Clhp Wild (baseToHoon lvl tr)
  DashRailLeft jr -> Bccl Wild [shut $ rest $ luft lvl jr]
  DashCellRight tl -> Clhp (baseToHoon lvl tl) Wild
  DashCorePayload fom (s, js) -> Bcbr Wild (baseToHoon lvl fom)
                               $ fmap (shut . rest . luft lvl . (`Jamb` s)) js
rollDashes lvl ds =
  Huge $ Stem "" "" [] (ds <&> \d -> ("**", tank $ rollDash lvl d, Leaf ""))

baseToHoon lvl bas = shut $ rest $ loft lvl bas

instance Rolling ActTree where
  roll = \case
    ActTree a cs -> Huge $ Rose "" ""
      [ tank $ roll a
      , Rose "" "" $ map (tank . roll) $ reverse cs
      ]
    ActExit a r -> Huge $ Rose "exit:" ""
      [ tank $ roll a
      , tank $ roll r
      ]
    ActNote t n -> Huge $ Rose "note:" ""
      [ Leaf t
      , tank $ roll n
      ]

instance Rolling Act where
  roll = \case
    ActRoot -> leaf "root"
    ActFits f lvl t u wap -> Huge $ Stem (tshow f <> ":") "" []
      [ ("have", tank $ roll $ loft lvl t, Leaf "")
      , ("need", tank $ roll $ loft lvl u, Leaf "")
      ]
    ActDraw Line{lev, loc, lyt, las} -> Huge $ Stem "draw:" "" []
      [ ("loc ", Leaf $ tshow loc, Leaf "")
      , ("lyt ", tank $ roll lyt, Leaf "")
      , ("las ", tank $ rollDashes lev las, Leaf "")
      ]
    ActFind loc@(lvl,_) sut w -> Huge $ Stem "find:" "" []
      [ ("loc ", Leaf $ tshow loc, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("wing", tank $ roll w,    Leaf "")
      ]
    ActFuse loc@(lvl,_) t f -> Huge $ Stem "fuse:" "" []
      [ ("loc ", Leaf $ tshow loc, Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      , ("skin", tank $ roll f,    Leaf "")
      ]
    ActCrop loc@(lvl,_) t f -> Huge $ Stem "crop:" "" []
      [ ("loc ", Leaf $ tshow loc, Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      , ("skin", tank $ roll f,    Leaf "")
      ]
    ActToil Con{lvl, sut} loc pet typ -> Huge $ Stem "toil:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("loc ", Leaf $ tshow loc, Leaf "")
      , ("skin", tank $ roll pet,  Leaf "")
      , ("type", tank $ roll $ loft lvl typ,    Leaf "")
      ]
    ActTore cub    -> Huge $ Rose "tore:" "" [tank $ roll cub]
    ActTear cl cub -> Huge $ Rose "tear:" "" [tank $ roll cl, tank $ roll cub]
    ActTyre cub    -> Huge $ Rose "tyre:" "" [tank $ roll cub]
    ActTire lvl fis typ -> Huge $ Stem "tire:" "" []
      [ ("fish", tank $ roll fis, Leaf "")
      , ("type", tank $ roll $ loft lvl typ, Leaf "")
      ]
    ActThin lvl ken -> Huge $ Stem "thin:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("ken ", tank $ roll ken, Leaf "")
      ]
    ActScan Con{lvl, sut} c -> Huge $ Stem "scan:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("code", tank $ roll c,    Leaf "")
      ]
    ActWork Con{lvl, sut} f c t wap -> Huge $ Stem "work:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("mode", Leaf $ tshow f,   Leaf "")
      , ("code", tank $ roll c,    Leaf "")
      , ("type", tank $ roll $ loft lvl t,    Leaf "")
      ]
    ActPlay Con{lvl, sut} c -> Huge $ Stem "play:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("sut ", tank $ roll $ loft lvl sut,  Leaf "")
      , ("----", Leaf "",          Leaf "")
      , ("code", tank $ roll $ c, Leaf "")
      ]
    ActDone -> leaf "done"

instance Rolling Fail where
  roll = \case
    FairFore s o -> Huge $ Stem "fair-fore:" "" []
      [ ("semi", tank $ roll s, Leaf "")
      , ("fore", Leaf $ tshow o, Leaf "")
      ]
    PareFree r b -> Huge $ Stem "pare-free:" "" []
      [ ("rump", Leaf $ tshow r, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    DrawCore t -> leaf $ "draw-core: " <> t
    FindFail f t -> Huge $ Stem ("find." <> printLimb f) "" []
      [ ("type", tank $ roll t, Leaf "")
      ]
    FarmCore as bs -> Huge $ Stem "farm-core:" "" []
      [ ("arms", Leaf $ tshow as, Leaf "")
      , ("arms", Leaf $ tshow bs, Leaf "")
      ]
    FitsFail f lvl t u -> Huge $ Stem (tshow f <> "-fail:") "" []
      [ ("have", tank $ roll $ loft lvl t, Leaf "")
      , ("need", tank $ roll $ loft lvl u, Leaf "")
      ]
    ToilFish p t -> Huge $ Stem "toil-fish:" "" []
      [ ("skin", tank $ roll p, Leaf "")
      , ("type", tank $ roll t, Leaf "")
      ]
    TireFish a f -> Huge $ Stem "tire-fish:" "" []
      [ ("axis", tank $ roll a, Leaf "")
      , ("miss", tank $ roll f, Leaf "")
      ]
    ThinFree lvl ken -> Huge $ Stem "thin-free:" "" []
      [ ("lvl ", Leaf $ tshow lvl, Leaf "")
      , ("ken ", tank $ roll ken, Leaf "")
      ]
    EditPull w t -> Huge $ Stem "edit-pull:" "" []
      [ ("wing", Leaf $ tshow w, Leaf "")
      , ("type", tank $ roll t,  Leaf "")
      ]
    NeedGate t -> Huge $ Palm "need-gate:" [tank $ roll t]
    ScanMurk s -> Huge $ Palm "scan-murk:" [tank $ roll s]
    WorkMiss s b -> Huge $ Stem "work-miss:" "" []
      [ ("test", tank $ roll s, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    PlayMiss s b -> Huge $ Stem "play-miss:" "" []
      [ ("test", tank $ roll s, Leaf "")
      , ("base", tank $ roll b, Leaf "")
      ]
    SealPull w -> Huge $ Palm "seal-pull:" [tank $ roll w]
    BailNote t -> Huge $ Palm "bail-note:" [Leaf t]
    BailFail -> leaf "bail-fail"

-- | Display loft
lyft :: Var a => Level -> Semi a -> Hoon
lyft lvl = undefined
