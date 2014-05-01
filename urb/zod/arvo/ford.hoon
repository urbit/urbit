!:  ::  %ford, new execution control
!?  164
::::
|=  pit=vase
^-  vane
=>  =~
|%                                                      ::  structures
++  axle                                                ::  all %ford state
  $:  ven=%0                                            ::  version for update
      tad=[p=@ud q=(map ,@ud task)]                     ::  tasks by number
      dym=(map duct ,@ud)                               ::  duct to number
      hoc=(map role ,[p=@da q=silk]))                   ::  resolution/last used
      tiz=(map cash twig)                               ::  file hash to twig
  ==                                                    ::
++  bolt                                                ::  local product
  $%  [%0 p=(list path) q=vase]                         ::  depends / result
      [%1 p=(list path)]                                ::  blockage
      [%2 p=(list tank)]                                ::  error
  ==                                                    ::
++  plan  ,[p=@ud q=beak r=silk]                        ::  full construction
++  task                                                ::  problem in progress
  $:  wor=writ                                          ::  rights and powers
      nah=duct                                          ::  cause
      bek=beak                                          ::  context
      sic=silk                                          ::  problem
      kig=,[p=@ud q=(map ,@ud path)]                    ::  blocks
  ==                                                    ::
|%                                                      ::
++  colt                                                ::  reduce to save
  |=  lex=axle                                          
  lex
::
++  meld                                                ::  cons cages
  |=  [one=cage two=cage]
  :_  (slop q.one q.two)
  ?~  p.one  ~
  ?~  p.two  ~
  [~ p.one p.two]
::
++  za                                                  ::  per event
  =|  $:  $:  $:  wor=writ                              ::  event authority
                  tea=wire                              ::  event place
                  hen=duct                              ::  event floor
                  fav=card                              ::  event data
              ==                                        ::
              $:  now=@da                               ::  event date
                  eny=@                                 ::  unique entropy
                  sky=$+(* (unit))                      ::  system namespace
              ==                                        ::
              mow=(list move)                           ::  pending actions
          ==                                            ::
          axle                                          ::  all vane state
      ==                                                ::
  =*  lex  ->
  |%
  ++  abet 
    ^-  [(list move) axle]
    [(flop mow) lex]
  ::
  ++  apex
    |-  ^+  +
    ?^  tea  
      ?>  ?=([@ @ ~] tea)
      =+  num=(need (slaw %ud i.tea))
      ?>  ?=([%writ *] fav)
      =+  tus=(~(get by q.tad) num)
      ?~  tus
        ~&  [%ford-lost num]
        +.$
      (~(resp zo [num u.tus]) (need (slaw %ud i.t.tea)) p.fav)
    ::
    ?+    -.fav
        %exec
      =+  num=p.tad
      ?>  !(~(has by dym) hen)
      =:  p.tad  +(p.tad)
          dym    (~(put by dym) num hen) 
        ==
      abet:~(exec zo [num `task`[wor hen p.fav q.fav ~]])
    ::
        %kill
      =+  num=(need (~(get by dym) hen))
      =+  tas=(need (~(get by q.tad) num))
      ~(kill zo [num tas])
    ==
  :: 
  ++  zo
    |_  [num=@ud task]
    ++  abet  %_(..zo q.tad (~(put by q.tad) num +<+))
    ++  amok  %_(..zo q.tad (~(del by q.tad) num))
    ++  camp                                            ::  request a file
      |=  pax=path
      ^+  +>
      =+  kit=(need (tame pax))
      =+  [him=`ship`r.kit ryf=`riff`]
      =+  tik=(scot %ud p.kig)
      =:  p.kig  +(p.kig)
          q.kig  (~(put by q.kig) p.kig pax)
        ==
      %=    $
          mow  :_  mow
        :+  [~ wor]
          ~[/c [%f (scot %ud num) (scot %ud tik) ~] hen]
        [%warp `ship`r.kit [s.kit ~ %& p.kit q.kit t.kit]]
      ==
    ++  exec
      ^+  ..zo
      ?^  q.kig  ..zo
      |-  ^+  ..zo
      =^  bot  +.$  (make sic)
      ?-  -.bot
        %0  amok:+.$(mow [%made %& p.bot])
        %2  amok:+.$(mow [%made %| p.bot])
        %1  =<  abet
            |-  ^+  +.^$
            ?~  p.bot  +.^$
            $(p.bot t.p.bot, +.$ (camp i.p.bot))
      == 
    ++  find                                            ::  XX awaits nu scry
      |=  pax=path
      ^-  (unit (unit ,*))
      =+  dum=(sky pax)
      ?^  dum  [~ u.dum]
      ?:  &(?=(^ pax) =((scot %p q.wor) i.pax))
        ~   ::  assumed nonexistent
      [~ ~] ::  block on it
    ::
    ++  fuss                                            ::  load code
      |=  lav=(list path)
      ^-  [p=(unit bolt) q=_+>]
      ?~  lav  [~ +>.$]
      =+  daz=(find %cy (weld i.lav /hoon))
      ?~  daz  $(lav t.lav)
      ?~  u.daz  
    ::
    ++  make                                            ::  resolve silk
      |=  kis=silk
      ^-  [bolt _+>]
      ?-    -.kis
          ^
        =^  one  +>.$  $(kis p.kis)
        ?.  ?=(%0 -.one)  [one +>.$]
        =^  two  +>.$  $(kis q.kis)
        ?.  ?=(%0 -.two)  [two +>.$]
        [[%0 (weld p.one p.two) (meld q.one q.two)] +>.$]
      ::
          %call
        =^  one  +>.$  $(kis p.kis)
        ?.  ?=(%0 -.one)  [one +>.$]
        =^  two  +>.$  $(kis q.kis)
        ?.  ?=(%0 -.two)  [two +>.$]
        =^  tri  +>.$  (maul q.q.one q.q.two)
        [%0 :(weld p.one p.two p.tri) [~ q.tri]]
      ::
          %code 
        =^  one  +>.$  $(kis q.kis)
        ?.  ?=(%0 -.one)  [one +>.$]
        =^  twu  +>.$  (fuss p.kis)
        |-  ^-  [(unit bolt) _+>.^$]
        =+  
      ::
          %data  !!
      ::
          %noun  !!
      ::
          %pass  !!
      ::
          %reef  [%0 
      ==
    ::
    ++  maul                                            ::  slam
      |=  [gat=vase sam=vase]
      ^+  [bolt _+>]
      =+  top=(mule |.((slit gat sam)))
      ?-  -.top 
        |  [%0 
        &
      ==
    ++  maim                                            ::  slap 
    ::
    ++  kill
      ^+  ..zo
      =<  amok
      |-  ^+   k
      
    ++  resp
      |=  [tik=@ud rot=riot]
      ^+  ..zo
      ?>  (~(has by q.kig) tik)
      ?~  rot
        amok:+.$(mow [%made %| (smyt (need (~(get by q.kig)) tik))])
      exec(q.kig (~(del by q.kig) tik))
    --
  --
--
.  ==
=|  axle
=*  lex  -
|=  [now=@da eny=@ ska=$+(* (unit (unit)))]             ::  activate
^?                                                      ::  opaque core
|%                                                      ::
++  beat                                                ::  process move
  |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
  ^-  [p=(list move) q=vane]
  ?~  wru  ~|(%beat-whom !!)
  =^  mos  lex
    abet:~(apex za [[u.wru tea hen fav] [now eny sky] ~] lex)
  [mos ..^$]
::
++  come
  |=  [sam=? old=vase]
  ^-  vane
  (load old)
::
++  doze
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load
  |=  old=vase
  ^-  vane
  ?.  (~(nest ut -:!>(`axle`+>-.^$)) | p.old)
    ~&  %eyre-reset
    ..^$
  ..^$(+>- (axle q.old))
::
++  raze
  ^-  vane
  ..$(+>- *axle)
::
++  scry
  |=  [our=ship ren=@tas who=ship syd=disc lot=coin tyl=path]
  ^-  (unit)
  ~
::
++  stay
  `vase`!>((colt `axle`+>-.$))
++  vern  [164 0]
--
