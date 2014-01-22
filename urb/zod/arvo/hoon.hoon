::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Preface                               ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
?>  ?=(@ .)                                             ::  atom subject
%.  .                                                   ::  fun with subject
|=  cud=@                                               ::  call it cud
=-  ?:  =(0 cud)                                        ::  if cud is 0
      all                                               ::  then return engine
    (make:all cud)                                      ::  else simple compile
^=  all                                                 ::  assemble engine
  =~                                                    ::  volume stack
%164                                                    ::  version constant
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 0, version stub                ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~%  %k.164  ~  ~                                        ::
|%                                                      ::
++  stub  164                                           ::  version stub
--                                                      ::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 1, Hoon models                 ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~%    %mood
    +
  ~
|%                                                      ::
++  abel  typo                                          ::  original sin: type
++  axis  ,@                                            ::  tree address
++  also  ,[p=term q=wing r=type]                       ::  alias
++  base  ?([%atom p=odor] %noun %cell %bean %null)     ::
++  beer  $|(@ [~ p=twig])                              ::
++  bloq  ,@                                            ::  blockclass
++  calf  ,[p=(map ,@ud wine) q=wine]                   ::
++  char  ,@tD                                          ::
++  chop  $?  lef=term                                  ::
              [std=term kel=@]                          ::
              [ven=term pro=term kel=@]                 ::
              [ven=term pro=term ver=@ kel=@]           ::
          ==                                            ::
++  claw  $%  [%ash p=twig]                             ::
              [%elm p=twig]                             ::
              [%oak ~]                                  ::
              [%yew p=(map term claw)]                  ::
          ==                                            ::
++  coat  ,[p=path q=vase]                              ::
++  coil  $:  p=?(%gold %iron %lead %zinc)              ::
              q=type                                    ::
              r=[p=?(~ ^) q=(map term foot)]            ::
          ==                                            ::
++  coin  $%  [%$ p=dime]                               ::
              [%blob p=*]                               ::
              [%many p=(list coin)]                     ::
          ==                                            ::
++  cord  ,@t                                           ::
++  date  ,[[a=? y=@ud] m=@ud t=tarp]                   ::  parsed date
++  dime  ,[p=@ta q=@]                                  ::
++  dram  $%  [| p=(map ,@tas dram)]                    ::
              [& p=@ud q=@]                             ::
          ==                                            ::
++  edge  ,[p=hair q=(unit ,[p=* q=nail])]              ::
++  foot  $%  [%ash p=twig]                             ::
              [%elm p=twig]                             ::
              [%oak ~]                                  ::
              [%yew p=(map term foot)]                  ::
          ==                                            ::
++  gear  |*  a=_,*                                     ::  list generator
          $_                                            ::
          =|  b=*                                       ::
          |?                                            ::
          ?@  b                                         ::
            ~                                           ::
          [i=(a -.b) t=^?(..$(b +.b))]                  ::
++  hair  ,[p=@ud q=@ud]                                ::
++  hapt  (list ,@ta)                                   ::
++  like  |*  a=_,*                                     ::  generic edge
          |=  b=_`*`[(hair) ~]                          ::
          :-  p=(hair -.b)                              ::
          ^=  q                                         ::
          ?@  +.b  ~                                    ::
          :-  ~                                         ::
          u=[p=(a +>-.b) q=[p=(hair -.b) q=(tape +.b)]]  ::
++  limb  $|  term 
            $%([%& p=axis] [%| p=@ud q=term])
++  line  ,[p=[%leaf p=odor q=@] q=tile]                ::
++  list  |*  a=_,*                                     ::
          $|(~ [i=a t=(list a)])                        ::
++  odor  ,@ta                                          ::
++  tarp  ,[d=@ud h=@ud m=@ud s=@ud f=(list ,@ux)]      ::  parsed time
++  time  ,@da                                          ::  galactic time
++  tree  |*  a=_,*                                     ::  binary tree
          $|(~ [n=a l=(tree a) r=(tree a)])             ::
++  nail  ,[p=hair q=tape]                              ::
++  pass  ,@                                            ::
++  path  (list span)                                   ::
++  pint  ,[p=[p=@ q=@] q=[p=@ q=@]]                    ::
++  port  $:  p=axis                                    ::
              $=  q                                     ::
              $%  [%& p=type]                           ::
                  [%| p=axis q=(list ,[p=type q=foot])] ::
              ==                                        ::
          ==                                            ::
++  post  $:  p=axis                                    ::
              $=  q                                     ::
              $%  [0 p=type]                            ::
                  [1 p=axis q=(list ,[p=type q=foot])]  ::
                  [2 p=twin q=type]                     ::
              ==                                        ::
          ==                                            ::
++  prop  $:  p=axis                                    ::
              $=  q                                     ::
              [p=?(~ axis) q=(list ,[p=type q=foot])]   ::
          ==                                            ::
++  reef  ,[p=[p=? q=@ud] q=@ud]                        ::
++  ring  ,@                                            ::
++  rule  |=(tub=nail `edge`[p.tub ~ ~ tub])            ::
++  shoe  $%  [%hunk p=tank]                            ::
              [%lose p=term]                            ::
              [%mean p=*]                               ::
              [%spot p=spot]                            ::
          ==                                            ::
++  span  ,@ta                                          ::
++  spot  ,[p=path q=pint]                              ::
++  tank  $%  [%leaf p=tape]                            ::
              [%palm p=[p=tape q=tape r=tape s=tape] q=(list tank)]
              [%rose p=[p=tape q=tape r=tape] q=(list tank)]
          ==                                            ::
++  tape  (list char)                                   ::
++  term  ,@tas                                         ::
++  tiki                                                ::  test case
          $%  [& p=(unit term) q=wing]                  ::  simple wing
              [| p=(unit term) q=twig]                  ::  named wing
          ==                                            ::
++  tile  $&  [p=tile q=tile]                           ::  ordered pair
          $%  [%axil p=base]                            ::  base type
              [%bark p=term q=tile]                     ::  name
              [%bush p=tile q=tile]                     ::  pair/tag
              [%fern p=[i=tile t=(list tile)]]          ::  plain selection
              [%herb p=twig]                            ::  gate
              [%kelp p=[i=line t=(list line)]]          ::  tag selection
              [%leaf p=term q=@]                        ::  constant atom
              [%reed p=tile q=tile]                     ::  atom/cell
              [%weed p=twig]                            ::  example
          ==                                            ::
++  toga                                                ::  face control
          $|  p=term                                    ::  two togas
          $%  [0 ~]                                     ::  no toga
              [1 p=term q=toga]                         ::  deep toga
              [2 p=toga q=toga]                         ::  cell toga
          ==                                            ::
++  twig  $&  [p=twig q=twig]                           ::
          $%                                            ::
            [%$ p=axis]                                 ::
            [%bccb p=tile]                              ::
            [%bccm p=tile]                              ::
            [%bcpt p=wing q=tile]                       ::
            [%bctr p=tile]                              ::
            [%bczp p=base]                              ::
          ::                                            ::
            [%brcb p=tile q=(map term foot)]            ::
            [%brcn p=(map term foot)]                   ::
            [%brdt p=twig]                              ::
            [%brfs p=tile q=(map term foot)]            ::
            [%brkt p=twig q=(map term foot)]            ::
            [%brhp p=twig]                              ::
            [%brls p=tile q=twig]                       ::
            [%brtr p=tile q=twig]                       ::
            [%brts p=tile q=twig]                       ::
            [%brwt p=twig]                              ::
          ::                                            ::
            [%clcb p=twig q=twig]                       ::
            [%clcn p=tusk]                              ::
            [%clfs p=twig]                              ::
            [%clkt p=twig q=twig r=twig s=twig]         ::
            [%clhp p=twig q=twig]                       ::
            [%clls p=twig q=twig r=twig]                ::
            [%clsg p=tusk]                              ::
            [%cltr p=tusk]                              ::
            [%clzz p=tusk]                              ::
          ::                                            ::
            [%cncb p=wing q=tray]                       ::
            [%cncl p=twig q=twig]                       ::
            [%cndt p=twig q=twig]                       ::
            [%cnhp p=twig q=tusk]                       ::
            [%cntr p=wing q=twig r=tray]                ::
            [%cnkt p=twig q=twig r=twig s=twig]         ::
            [%cnls p=twig q=twig r=twig]                ::
            [%cnsg p=wing q=twig r=twig]                ::
            [%cnts p=wing q=tray]                       ::
            [%cnzy p=term]                              ::
            [%cnzz p=wing]                              ::
          ::                                            ::
            [%dtkt p=twig]                              ::
            [%dtls p=twig]                              ::
            [%dtzy p=term q=@]                          ::
            [%dtzz p=term q=*]                          ::
            [%dttr p=twig q=twig]                       ::
            [%dtts p=twig q=twig]                       ::
            [%dtwt p=twig]                              ::
          ::                                            ::
            [%hxgl p=tusk]                              ::
            [%hxgr p=tusk]                              ::
          ::                                            ::
            [%ktbr p=twig]                              ::
            [%ktdt p=twig q=twig]                       ::
            [%ktls p=twig q=twig]                       ::
            [%kthp p=tile q=twig]                       ::
            [%ktpm p=twig]                              ::
            [%ktsg p=twig]                              ::
            [%ktts p=toga q=twig]                       ::
            [%ktwt p=twig]                              ::
          ::                                            ::
            [%sgbr p=twig q=twig]                       ::
            [%sgcb p=twig q=twig]                       ::
            [%sgcn p=chop q=twig r=tyre s=twig]         ::
            [%sgfs p=chop q=twig]                       ::
            [%sggl p=$|(term [p=term q=twig]) q=twig]   ::
            [%sggr p=$|(term [p=term q=twig]) q=twig]   ::
            [%sgbc p=term q=twig]                       ::
            [%sgls p=@ q=twig]                          ::
            [%sgpm p=@ud q=twig r=twig]                 ::
            [%sgts p=twig q=twig]                       ::
            [%sgwt p=@ud q=twig r=twig s=twig]          ::
            [%sgzp p=twig q=twig]                       ::
          ::                                            ::
            [%smcl p=twig q=tusk]                       ::
            [%smdt p=twig q=tusk]                       ::
            [%smdq p=(list beer)]                       ::
            [%smsg p=twig q=tusk]                       ::
            [%smsm p=twig q=twig]                       ::
          ::                                            ::
            [%tsbr p=tile q=twig]                       ::
            [%tscl p=tray q=twig]                       ::
            [%tsdt p=wing q=twig r=twig]                ::
            [%tsgl p=twig q=twig]                       ::
            [%tsgr p=twig q=twig]                       ::
            [%tskt p=twig q=twig r=twig s=twig]         ::
            [%tsls p=twig q=twig]                       ::
            [%tstr p=term q=wing r=twig]                ::
            [%tshp p=twig q=twig]                       ::
            [%tssg p=tusk]                              ::
          ::                                            ::
            [%wtbr p=tusk]                              ::
            [%wthp p=wing q=tine]                       ::
            [%wthz p=tiki q=tine]                       ::
            [%wtcl p=twig q=twig r=twig]                ::
            [%wtdt p=twig q=twig r=twig]                ::
            [%wtkt p=wing q=twig r=twig]                ::
            [%wtkz p=tiki q=twig r=twig]                ::
            [%wtgl p=twig q=twig]                       ::
            [%wtgr p=twig q=twig]                       ::
            [%wtls p=wing q=twig r=tine]                ::
            [%wtlz p=tiki q=twig r=tine]                ::
            [%wtpm p=tusk]                              ::
            [%wtpt p=wing q=twig r=twig]                ::
            [%wtpz p=tiki q=twig r=twig]                ::
            [%wtsg p=wing q=twig r=twig]                ::
            [%wtsz p=tiki q=twig r=twig]                ::
            [%wtts p=tile q=wing]                       ::
            [%wtzp p=twig]                              ::
          ::                                            ::
            [%zpcb p=spot q=twig]                       ::
            [%zpcm p=twig q=twig]                       ::
            [%zpcn ~]                                   ::
            [%zpfs p=twig]                              ::
            [%zpgr p=twig]                              ::
            [%zpsm p=twig q=twig]                       ::
            [%zpts p=twig]                              ::
            [%zpwt p=$|(p=@ [p=@ q=@]) q=twig]          ::
            [%zpzp ~]                                   ::
          ==                                            ::
++  tine  (list ,[p=tile q=twig])                       ::
++  tusk  (list twig)                                   ::
++  tyre  (list ,[p=term q=twig])                       ::
++  tyke  (list (unit twig))                            ::
++  tray  (list ,[p=wing q=twig])                       ::
++  tone  $%  [%0 p=*]                                  ::
              [%1 p=(list)]                             ::
              [%2 p=(list ,[@ta *])]                    ::
          ==                                            ::
++  nock  $&  [p=nock q=nock]                           ::
          $%  [%0 p=@]                                  ::
              [%1 p=*]                                  ::
              [%2 p=nock q=nock]                        ::
              [%3 p=nock]                               ::
              [%4 p=nock]                               ::
              [%5 p=nock q=nock]                        ::
              [%6 p=nock q=nock r=nock]                 ::
              [%7 p=nock q=nock]                        ::
              [%8 p=nock q=nock]                        ::
              [%9 p=@ q=nock]                           ::
              [%10 p=?(@ [p=@ q=nock]) q=nock]          ::
              [%11 p=nock]                              ::
          ==                                            ::
++  toon  $%  [%0 p=*]                                  ::
              [%1 p=(list)]                             ::
              [%2 p=(list tank)]                        ::
          ==                                            ::
++  tune  $%  [%0 p=vase]                               ::
              [%1 p=(list)]                             ::
              [%2 p=(list ,[@ta *])]                    ::
          ==                                            ::
++  twin  ,[p=term q=wing r=axis s=type]                ::
++  type  $|  ?(%noun %void)                            ::
          $%  [%atom p=term]                            ::
              [%bull p=twin q=type]                     ::
              [%cell p=type q=type]                     ::
              [%core p=type q=coil]                     ::
              [%cube p=* q=type]                        ::
              [%face p=term q=type]                     ::
              [%fork p=type q=type]                     ::
              [%hold p=(list ,[p=type q=twig])]         ::
          ==                                            ::
++  typo  type                                          ::
++  udal                                                ::  atomic change (%b)
          $:  p=@ud                                     ::  blockwidth
              q=(list ,[p=@ud q=(unit ,[p=@ q=@])])     ::  indels
          ==                                            ::
++  udon                                                ::  abstract delta
          $:  p=umph                                    ::  preprocessor
              $=  q                                     ::  patch
              $%  [%a p=ulna]                           ::  trivial replace
                  [%b p=udal]                           ::  atomic indel
                  [%c p=(urge)]                         ::  list indel
                  [%d p=upas q=upas]                    ::  tree edit
              ==                                        ::
          ==                                            ::
++  ulna  ,[p=* q=*]                                    ::  from to
++  umph                                                ::  change filter
          $|  $?  %a                                    ::  no filter
                  %b                                    ::  jamfile
                  %c                                    ::  LF text
              ==                                        ::
          $%  [%d p=@ud]                                ::  blocklist
          ==                                            ::
++  unce  |*  a=_,*                                     ::  change part
          $%([%& p=@ud] [%| p=(list a) q=(list a)])     ::
++  unit  |*  a=_,*                                     ::  maybe
          $|(~ [~ u=a])                                 ::
++  upas                                                ::  tree change (%d)
          $&  [p=upas q=upas]                           ::  cell
          $%  [%0 p=axis]                               ::  copy old
              [%1 p=*]                                  ::  insert new
              [%2 p=axis q=udon]                        ::  mutate!
          ==                                            ::
++  urge  |*(a=_,* (list (unce a)))                     ::  list change
++  vase  ,[p=type q=*]                                 ::  type-value pair
++  vise  ,[p=typo q=*]                                 ::  old vase
++  wall  (list tape)                                   ::  text lines
++  wing  (list limb)                                   ::
++  wine  $|  ?(%noun %path %tank %void %wall %wool %yarn)
          $%  [%atom p=term]                            ::
              [%core p=(list ,@ta) q=wine]              ::
              [%face p=term q=wine]                     ::
              [%list p=term q=wine]                     ::
              [%pear p=term q=@]                        ::
              [%pick p=(list wine)]                     ::
              [%plot p=(list wine)]                     ::
              [%stop p=@ud]                             ::
              [%tree p=term q=wine]                     ::
              [%unit p=term q=wine]                     ::
          ==                                            ::
++  woof  (list $|(@ud [p=@ud q=@ud]))                  ::  udon transform
++  wonk  |*(veq=edge ?@(q.veq !! p.u.q.veq))           ::
::                                                      ::
++  map  |*  [a=_,* b=_,*]                              ::  associative array
         $|(~ [n=[p=a q=b] l=(map a b) r=(map a b)])    ::
++  qeu  |*  a=_,*                                      ::
         $|(~ [n=a l=(qeu a) r=(qeu a)])                ::
++  set  |*  a=_,*                                      ::
         $|(~ [n=a l=(set a) r=(set a)])                ::
--                                                      ::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 2, Hoon libraries and compiler ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
~%    %hoon
    +
  ==
    %al    al
    %ap    ap
    %ut    ut
    %seed  seed
    %show  show
  ==
|%
  :::::::::::::::::::::::::::::::::::::::::::::::::::::   ::
::::              chapter 2a, basic unsigned math       ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  add                                                 ::  add
  ~/  %add
  |=  [a=@ b=@]
  ^-  @
  ?:  =(0 a)
    b
  $(a (dec a), b +(b))
++  und  |=([a=@ b=@] (add 47 (add a b)))
::
++  cap                                                 ::  tree head
  ~/  %cap
  |=  a=@
  ^-  ?(%2 %3)
  ?-  a
    %2        %2
    %3        %3
    ?(%0 %1)  !!
    *         $(a (div a 2))
  ==
::
++  dec                                                 ::  decrement
  ~/  %dec
  |=  a=@
  ~|  %decrement-underflow
  ?<  =(0 a)
  =+  b=0
  |-  ^-  @
  ?:  =(a +(b))
    b
  $(b +(b))
::
++  div                                                 ::  divide
  ~/  %div
  |=  [a=@ b=@]
  ^-  @
  ~|  'div'
  ?<  =(0 b)
  =+  c=0
  |-
  ?:  (lth a b)
    c
  $(a (sub a b), c +(c))
::
++  gte                                                 ::  greater-equal
  ~/  %gte
  |=  [a=@ b=@]
  ^-  ?
  !(lth a b)
::
++  gth                                                 ::  greater-than
  ~/  %gth
  |=  [a=@ b=@]
  ^-  ?
  !(lte a b)
::
++  lte                                                 ::  less-equal
  ~/  %lte
  |=  [a=@ b=@]
  |(=(a b) (lth a b))
::
++  lth                                                 ::  less-than
  ~/  %lth
  |=  [a=@ b=@]
  ^-  ?
  &(!=(a b) |-(|(=(0 a) &(!=(0 b) $(a (dec a), b (dec b))))))
::
++  mas                                                 ::  tree body
  ~/  %mas
  |=  a=@
  ^-  @
  ?-  a
    1   !!
    2   1
    3   1
    *   (add (mod a 2) (mul $(a (div a 2)) 2))
  ==
::
++  max                                                 ::  maximum
  ~/  %max
  |=  [a=@ b=@]
  ^-  @
  ?:  (gth a b)
    a
  b
::
++  min                                                 ::  minimum
  ~/  %min
  |=  [a=@ b=@]
  ^-  @
  ?:  (lth a b)
    a
  b
::
++  mod                                                 ::  remainder
  ~/  %mod
  |=  [a=@ b=@]
  ^-  @
  ?<  =(0 b)
  (sub a (mul b (div a b)))
::
++  mul                                                 ::  multiply
  ~/  %mul
  |=  [a=@ b=@]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 a)
    c
  $(a (dec a), c (add b c))
::
++  peg                                                 ::  tree connect
  ~/  %peg
  |=  [a=@ b=@]
  ^-  @
  ?-  b
    1   a
    2   (mul a 2)
    3   +((mul a 2))
    *   (add (mod b 2) (mul $(b (div b 2)) 2))
  ==
::
++  sub                                                 ::  subtract
  ~/  %sub
  |=  [a=@ b=@]
  ~|  %subtract-underflow
  ^-  @
  ?:  =(0 b)
    a
  $(a (dec a), b (dec b))
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2b, basic containers          ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                Section 2bA, units                    ::
::
++  bind                                                ::  argue
  |*  [a=(unit) b=_,*]
  ?~  a
    ~
  [~ u=(b u.a)]
::
++  clap                                                ::  combine
  |*  [a=(unit) b=(unit) c=_|=(^ +<-)]
  ?~  a
    b
  ?~  b
    a
  [~ u=(c u.a u.b)]
::
++  drop                                                ::  enlist
  |*  a=(unit)
  ?~  a
    ~
  [i=u.a t=~]
::
++  fall                                                ::  default
  |*  [a=(unit) b=*]
  ?~(a b u.a)
::
++  mate                                                ::  choose
  |*  [a=(unit) b=(unit)]
  ?~  b
    a
  ?~  a
    b
  ?.(=(u.a u.b) ~|('mate' !!) a)
::
++  need                                                ::  demand
  |*  a=(unit)
  ?@  a
    !!
  u.a
::
++  some                                                ::  lift
  |*  a=*
  [~ u=a]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                Section 2bB, lists                    ::
::
++  flop                                                ::  reverse
  ~/  %flop
  |*  a=(list)
  =>  .(a (homo a))
  ^+  a
  =+  b=`_a`~
  |-
  ?@  a
    b
  $(a t.a, b [i.a b])
::
++  homo                                                ::  homogenize
  |*  a=(list)
  ^+  =<  $
    |%  +-  $  ?:(_? ~ [i=(snag 0 a) t=$])
    --
  a
::
++  limo                                                ::  listify
  |*  a=*
  ^+  =<  $
    |%  +-  $  ?@(a ~ ?:(_? [i=-.a t=$] $(a +.a)))
    --
  a
::
++  lent                                                ::  length
  ~/  %lent
  |=  a=(list)
  ^-  @
  =+  b=0
  |-
  ?@(a b $(a t.a, b +(b)))
::
++  levy
  ~/  %levy                                             ::  all of
  |*  [a=(list) b=_|=(p=* .?(p))]
  |-  ^-  ?
  ?@  a
    &
  ?:  (b i.a)
    $(a t.a)
  |
::
++  lien                                                ::  some of
  ~/  %lien
  |*  [a=(list) b=$+(* ?)]
  |-  ^-  ?
  ?~  a  |
  ?:  (b i.a)  &
  $(a t.a)
::
++  reel                                                ::  right fold
  ~/  %reel
  |*  [a=(list) b=_=|([p=* q=*] |.(q))]
  |-  ^+  q.b
  ?@  a
    q.b
  (b i.a $(a t.a))
::
++  roll                                                ::  left fold
  ~/  %roll
  |*  [a=(list) b=_=|([p=* q=*] |.(q))]
  |-
  ^+  q.b
  ?@  a
    q.b
  $(a t.a, b b(q (b i.a q.b)))
::
++  skid                                                ::  separate
  |*  [a=(list) b=$+(* ?)]
  |-  ^+  [p=a q=a]
  ?~  a  [~ ~]
  =+  c=$(a t.a)
  ?:((b i.a) [[i.a p.c] q.c] [p.c [i.a q.c]])
::
++  skim                                                ::  only
  ~/  %skim
  |*  [a=(list) b=_|=(p=* .?(p))]
  |-
  ^+  a
  ?@  a
    ~
  ?:((b i.a) [i.a $(a t.a)] $(a t.a))
::
++  skip                                                ::  except
  ~/  %skip
  |*  [a=(list) b=_|=(p=* .?(p))]
  |-
  ^+  a
  ?@  a
    ~
  ?:((b i.a) $(a t.a) [i.a $(a t.a)])
::
++  scag                                                ::  prefix
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  |(?=(~ b) =(0 a))
    ~
  [i.b $(b t.b, a (dec a))]
::
++  slag                                                ::  suffix
  |*  [a=@ b=(list)]
  |-  ^+  b
  ?:  =(0 a)
    b
  ?@  b
    ~
  $(b t.b, a (dec a))
::
++  snag                                                ::  index
  ~/  %snag
  |*  [a=@ b=(list)]
  |-
  ?~  b
    ~|('snag-fail' !!)
  ?:  =(0 a)
    i.b
  $(b t.b, a (dec a))
::
++  sort                                                ::  quicksort
  ~/  %sort
  !:
  |*  [a=(list) b=$+([* *] ?)]
  =>  .(a ^.(homo a))
  |-  ^+  a
  ?~  a  ~
  %+  weld
    $(a (skim t.a |=(c=_i.a (b c i.a))))
  ^+  t.a
  [i.a $(a (skim t.a |=(c=_i.a !(b c i.a))))]
::
++  swag                                                ::  infix
  |*  [[a=@ b=@] c=(list)]
  (scag b (slag a c))
::
++  turn                                                ::  transform
  ~/  %turn
  |*  [a=(list) b=_,*]
  |-
  ?@  a
    ~
  [i=(b i.a) t=$(a t.a)]
::
++  weld                                                ::  concatenate
  ~/  %weld
  |*  [a=(list) b=(list)]
  =>  .(a ^.(homo a), b ^.(homo b))
  |-  ^+  b
  ?~  a  b
  [i.a $(a t.a)]
::
++  wild                                                ::  concatenate
  |*  [a=(list) b=(list)]
  =>  .(a ^.(homo a), b ^.(homo b))
  |-
  ?~  a  b
  [i=i.a $(a t.a)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2bC, gears                    ::
::
++  from                                                ::  range
  |=  [a=@ b=@]
  ^-  (gear ,@)
  =+  c=0
  |?
  ?:  =(c b)
    ~
  [i=a t=^?(..$(a +(a), c +(c)))]
::
++  long                                                ::
  |*  a=(gear)
  =+  b=0
  |-  ^-  @
  =+  c=(a)
  ?~  c
    b
  $(b +(b), a t.c)
::
++  lone  |*(a=* |?([i=a t=none]))                      ::
++  mill
  |*  [a=_,* b=(gear)]
  |?
  =+  c=(b)
  ?~  c
    ~
  [i=(a i.c) t=^?(..$(b t.c))]
::
++  none  |?(~)                                         ::
++  over                                                ::
  |=  [a=@ b=@]
  ^-  (gear ,@)
  |?
  ?:  =(a b)
    [i=a t=none]
  [i=a t=^?(..$(a +(a)))]
::
++  pull                                                ::
  |*  a=(gear)
  |=  b=_^+(|-(=+(b=(a) ?~(b ~ [i=i.b t=$(a t.b)]))) ~)
  ^+  b
  =+  c=(a)
  ?~  c
    b
  $(b [i.c b], a t.c)
::
++  push                                                ::
  |*  a=(gear)
  |=  b=_^+(|-(=+(b=(a) ?~(b ~ [i=i.b t=$(a t.b)]))) ~)
  ^+  b
  =+  c=((pull a) ~)
  ((pull (spin c)) b)
::
++  spin                                                ::
  |*  a=(list)
  =>  .(a `_(homo a)`a)
  |?
  ?~  a
    ~
  [i=i.a t=^?(..$(a t.a))]
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2c, simple noun surgery       ::::
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cA, bit surgery              ::
::
++  bex                                                 ::  binary exponent
  ~/  %bex
  |=  a=@
  ^-  @
  ?:  =(0 a)
    1
  (mul 2 $(a (dec a)))
::
++  can                                                 ::  assemble
  ~/  %can
  |=  [a=bloq b=(list ,[p=@ q=@])]
  ^-  @
  ?~  b
    0
  (mix (end a p.i.b q.i.b) (lsh a p.i.b $(b t.b)))
::
++  cat                                                 ::  concatenate
  ~/  %cat
  |=  [a=bloq b=@ c=@]
  (add (lsh a (met a b) c) b)
::
++  cut                                                 ::  slice
  ~/  %cut
  |=  [a=bloq [b=@ c=@] d=@]
  (end a c (rsh a b d))
::
++  end                                                 ::  tail
  ~/  %end
  |=  [a=bloq b=@ c=@]
  (mod c (bex (mul (bex a) b)))
::
++  lsh                                                 ::  left-shift
  ~/  %lsh
  |=  [a=bloq b=@ c=@]
  (mul (bex (mul (bex a) b)) c)
::
++  met                                                 ::  measure
  ~/  %met
  |=  [a=bloq b=@]
  ^-  @
  =+  c=0
  |-
  ?:  =(0 b)
    c
  $(b (rsh a 1 b), c +(c))
::
++  rap                                                 ::  assemble nonzero
  ~/  %rap
  |=  [a=bloq b=(list ,@)]
  ^-  @
  ?@  b
    0
  (cat a i.b $(b t.b))
::
++  rep                                                 ::  assemble single
  ~/  %rep
  |=  [a=bloq b=(list ,@)]
  ^-  @
  =+  c=0
  |-
  ?@  b
    0
  (con (lsh a c (end a 1 i.b)) $(c +(c), b t.b))
::
++  rip                                                 ::  disassemble
  ~/  %rip
  |=  [a=bloq b=@]
  ^-  (list ,@)
  ?:  =(0 b)
    ~
  [(end a 1 b) $(b (rsh a 1 b))]
::
++  rsh                                                 ::  right-shift
  ~/  %rsh
  |=  [a=bloq b=@ c=@]
  (div c (bex (mul (bex a) b)))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cB, bit logic                ::
::
++  con                                                 ::  binary or
  ~/  %con
  |=  [a=@ b=@]
  =+  [c=0 d=0]
  |-  ^-  @
  ?:  ?&(=(0 a) =(0 b))
    d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   (add d (lsh 0 c ?&(=(0 (end 0 1 a)) =(0 (end 0 1 b)))))
  ==
::
++  dis                                                 ::  binary and
  ~/  %dis
  |=  [a=@ b=@]
  =|  [c=@ d=@]
  |-  ^-  @
  ?:  ?|(=(0 a) =(0 b))
    d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   (add d (lsh 0 c ?|(=(0 (end 0 1 a)) =(0 (end 0 1 b)))))
  ==
::
++  mix                                                 ::  binary xor
  ~/  %mix
  |=  [a=@ b=@]
  ^-  @
  =+  [c=0 d=0]
  |-
  ?:  ?&(=(0 a) =(0 b))
    d
  %=  $
    a   (rsh 0 1 a)
    b   (rsh 0 1 b)
    c   +(c)
    d   (add d (lsh 0 c =((end 0 1 a) (end 0 1 b))))
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cC, noun orders              ::
::
++  aor                                                 ::  a-order
  ~/  %aor
  |=  [a=* b=*]
  ^-  ?
  ?:  =(a b)
    &
  ?.  ?=(@ a)
    ?.  ?=(@ b)
      ?:  =(-.a -.b)
        $(a +.a, b +.b)
      $(a -.a, b -.b)
    |
  ?.  ?=(@ b)
    &
  |-
  =+  [c=(end 3 1 a) d=(end 3 1 b)]
  ?:  =(c d)
    $(a (rsh 3 1 a), b (rsh 3 1 b))
  (lth c d)
::
++  dor                                                 ::  d-order
  ~/  %dor
  |=  [a=* b=*]
  ^-  ?
  ?:  =(a b)
    &
  ?.  ?=(@ a)
    ?.  ?=(@ b)
      ?:  =(-.a -.b)
        $(a +.a, b +.b)
      $(a -.a, b -.a)
    |
  ?.  ?=(@ b)
    &
  (lth a b)
::
++  gor                                                 ::  g-order
  ~/  %gor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug a) d=(mug b)]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::
++  hor                                                 ::  h-order
  ~/  %hor
  |=  [a=* b=*]
  ^-  ?
  ?:  ?=(@ a)
    ?:  ?=(@ b)
      (gor a b)
    &
  ?:  ?=(@ b)
    |
  ?:  =(-.a -.b)
    (gor +.a +.b)
  (gor -.a -.b)
::
++  vor                                                 ::  v-order
  ~/  %vor
  |=  [a=* b=*]
  ^-  ?
  =+  [c=(mug (mug a)) d=(mug (mug b))]
  ?:  =(c d)
    (dor a b)
  (lth c d)
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cD, insecure hashing         ::
::
++  fnv  |=(a=@ (end 5 1 (mul 16.777.619 a)))           ::  FNV scrambler
++  mug                                                 ::  31bit nonzero FNV1a
  ~/  %mug
  |=  a=*
  ?^  a
    =+  b=[p=$(a -.a) q=$(a +.a)]
    |-  ^-  @
    =+  c=(fnv (mix p.b (fnv q.b)))
    =+  d=(mix (rsh 0 31 c) (end 0 31 c))
    ?.  =(0 c)  c
    $(q.b +(q.b))
  =+  b=2.166.136.261
  |-  ^-  @
  =+  c=b
  =+  [d=0 e=(met 3 a)]
  |-  ^-  @
  ?:  =(d e)
    =+  f=(mix (rsh 0 31 c) (end 0 31 c))
    ?.  =(0 f)  f
    ^$(b +(b))
  $(c (fnv (mix c (cut 3 [d 1] a))), d +(d))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cE, phonetic base            ::
::
++  po
  ~/  %po
  =+  :-  ^=  sis
          'dozmarbinwansamlitsighidfidlissogdirwacsabwissib\
          /rigsoldopmodfoglidhopdardorlorhodfolrintogsilmir\
          /holpaslacrovlivdalsatlibtabhanticpidtorbolfosdot\
          /losdilforpilramtirwintadbicdifrocwidbisdasmidlop\
          /rilnardapmolsanlocnovsitnidtipsicropwitnatpanmin\
          /ritpodmottamtolsavposnapnopsomfinfonbanporworsip\
          /ronnorbotwicsocwatdolmagpicdavbidbaltimtasmallig\
          /sivtagpadsaldivdactansidfabtarmonranniswolmispal\
          /lasdismaprabtobrollatlonnodnavfignomnibpagsopral\
          /bilhaddocridmocpacravripfaltodtiltinhapmicfanpat\
          /taclabmogsimsonpinlomrictapfirhasbosbatpochactid\
          /havsaplindibhosdabbitbarracparloddosbortochilmac\
          /tomdigfilfasmithobharmighinradmashalraglagfadtop\
          /mophabnilnosmilfopfamdatnoldinhatnacrisfotribhoc\
          /nimlarfitwalrapsarnalmoslandondanladdovrivbacpol\
          /laptalpitnambonrostonfodponsovnocsorlavmatmipfap'
      ^=  dex
      'zodnecbudwessevpersutletfulpensytdurwepserwylsun\
      /rypsyxdyrnuphebpeglupdepdysputlughecryttyvsydnex\
      /lunmeplutseppesdelsulpedtemledtulmetwenbynhexfeb\
      /pyldulhetmevruttylwydtepbesdexsefwycburderneppur\
      /rysrebdennutsubpetrulsynregtydsupsemwynrecmegnet\
      /secmulnymtevwebsummutnyxrextebfushepbenmuswyxsym\
      /selrucdecwexsyrwetdylmynmesdetbetbeltuxtugmyrpel\
      /syptermebsetdutdegtexsurfeltudnuxruxrenwytnubmed\
      /lytdusnebrumtynseglyxpunresredfunrevrefmectedrus\
      /bexlebduxrynnumpyxrygryxfeptyrtustyclegnemfermer\
      /tenlusnussyltecmexpubrymtucfyllepdebbermughuttun\
      /bylsudpemdevlurdefbusbeprunmelpexdytbyttyplevmyl\
      /wedducfurfexnulluclennerlexrupnedlecrydlydfenwel\
      /nydhusrelrudneshesfetdesretdunlernyrsebhulryllud\
      /remlysfynwerrycsugnysnyllyndyndemluxfedsedbecmun\
      /lyrtesmudnytbyrsenwegfyrmurtelreptegpecnelnevfes'
  |%
  ++  ind  ~/  %ind
           |=  a=@
           =+  b=0
           |-  ^-  (unit ,@)
           ?:(=(256 b) ~ ?:(=(a (tod b)) [~ b] $(b +(b))))
  ++  ins  ~/  %ins
           |=  a=@
           =+  b=0
           |-  ^-  (unit ,@)
           ?:(=(256 b) ~ ?:(=(a (tos b)) [~ b] $(b +(b))))
  ++  tod  ~/(%tod |=(a=@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] dex))))
  ++  tos  ~/(%tos |=(a=@ ?>((lth a 256) (cut 3 [(mul 3 a) 3] sis))))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cF, signed and modular ints  ::
::
++  si                                                  ::  signed integer
  |%
  ++  abs  |=(a=@s (add (end 0 1 a) (rsh 0 1 a)))
  ++  dif  |=([a=@s b=@s] (sum a (new !(syn b) (abs b))))
  ++  dul  |=([a=@s b=@s] =+(c=(old a) ?:(-.c (mod +.c b) (sub b +.c))))
  ++  mid  |=  [a=@s b=@s]
           =+  c=(old:si a)
           =+  d=(old:si b)
           =+  s=(mix -.c -.d)
           =+  m=(mod +.c +.d)
           ?:  |(=(0 m) =(0 s))
             (new:si [-.d m])
           (new:si [-.d (sub +.d m)])
  ++  fra  |=  [a=@s b=@s]
           (new =(0 (mix (syn a) (syn b))) (div (abs a) (abs b)))
  ++  new  |=([a=? b=@] `@s`?:(a (mul 2 b) ?:(=(0 b) 0 +((mul 2 (dec b))))))
  ++  old  |=(a=@s [(syn a) (abs a)])
  ++  pro  |=  [a=@s b=@s]
           (new =(0 (mix (syn a) (syn b))) (mul (abs a) (abs b)))
  ++  rem  |=([a=@s b=@s] (dif a (pro b (fra a b))))
  ++  sum  |=  [a=@s b=@s]
           ~|  %si-sum
           =+  [c=(old a) d=(old b)]
           ?:  -.c
             ?:  -.d
               (new & (add +.c +.d))
             ?:  (gte +.c +.d)
               (new & (sub +.c +.d))
             (new | (sub +.d +.c))
           ?:  -.d
             ?:  (gte +.c +.d)
               (new | (sub +.c +.d))
             (new & (sub +.d +.c))
           (new | (add +.c +.d))
  ++  sun  |=(a=@u (mul 2 a))
  ++  syn  |=(a=@s =(0 (end 0 1 a)))
  --
++  fe                                                  ::  modulo bloq
  |_  a=bloq
  ++  dif  |=([b=@ c=@] (sit (sub (add out (sit b)) (sit c))))
  ++  inv  |=(b=@ (sub (dec out) (sit b)))
  ++  net  |=  b=@  ^-  @
           =>  .(b (sit b))
           ?:  (lte a 3)
             b
           =+  c=(dec a)
           %+  con
             (lsh c 1 $(a c, b (cut c [0 1] b)))
           $(a c, b (cut c [1 1] b))
  ++  out  (bex (bex a))
  ++  rol  |=  [b=@ c=@]  ^-  @
           =+  d=(sit c)
           =+  e=(bex a)
           =+  f=(mod b e)
           =+  g=(sub e f)
           (con (lsh 0 f (end 0 g d)) (rsh 0 g d))
  ++  ror  |=  [b=@ c=@]  ^-  @
           =+  d=(sit c)
           =+  e=(bex a)
           =+  f=(mod b e)
           =+  g=(sub e f)
           (con (rsh 0 f d) (lsh 0 g (end 0 f d)))
  ++  sum  |=([b=@ c=@] (sit (add b c)))
  ++  sit  |=(b=@ (end a 1 b))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cG, floating point           ::
::
++  rlyd  |=(red=@rd ~|(%real-nyet ^-([s=? h=@ f=@] !!)))
++  rlyh  |=(reh=@rh ~|(%real-nyet ^-([s=? h=@ f=@] !!)))
++  rlyq  |=(req=@rq ~|(%real-nyet ^-([s=? h=@ f=@] !!)))
++  rlys  |=(res=@rs ~|(%real-nyet ^-([s=? h=@ f=@] !!)))
++  ryld  |=([syn=? hol=@ fac=@] ~|(%real-nyet ^-(@rd !!)))
++  rylh  |=([syn=? hol=@ fac=@] ~|(%real-nyet ^-(@rh !!)))
++  rylq  |=([syn=? hol=@ fac=@] ~|(%real-nyet ^-(@rq !!)))
++  ryls  |=([syn=? hol=@ fac=@] ~|(%real-nyet ^-(@rs !!)))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cH, urbit time               ::
::
++  year
  |=  det=date
  ^-  @d
  =+  ^=  yer
      ?:  a.det
        (add 292.277.024.400 y.det)
      (sub 292.277.024.400 (dec y.det))
  =+  day=(yawn yer m.det d.t.det)
  (yule day h.t.det m.t.det s.t.det f.t.det)
::
++  yore
  |=  now=@d
  ^-  date
  =+  rip=(yell now)
  =+  ger=(yall d.rip)
  :-  ?:  (gth y.ger 292.277.024.400)
        [a=& y=(sub y.ger 292.277.024.400)]
      [a=| y=+((sub 292.277.024.400 y.ger))]
  [m.ger d.ger h.rip m.rip s.rip f.rip]
::
++  yell
  |=  now=@d
  ^-  tarp
  =+  sec=(rsh 6 1 now)
  =+  ^=  fan
      =+  [muc=4 raw=(end 6 1 now)]
      |-  ^-  (list ,@ux)
      ?:  |(=(0 raw) =(0 muc))
        ~
      =>  .(muc (dec muc))
      [(cut 4 [muc 1] raw) $(raw (end 4 muc raw))]
  =+  day=(div sec day:yo)
  =>  .(sec (mod sec day:yo))
  =+  hor=(div sec hor:yo)
  =>  .(sec (mod sec hor:yo))
  =+  mit=(div sec mit:yo)
  =>  .(sec (mod sec mit:yo))
  [day hor mit sec fan]
::
++  yule
  |=  rip=tarp
  ^-  @d
  =+  ^=  sec  ;:  add
                 (mul d.rip day:yo)
                 (mul h.rip hor:yo)
                 (mul m.rip mit:yo)
                 s.rip
               ==
  =+  ^=  fac  =+  muc=4
               |-  ^-  @
               ?~  f.rip
                 0
               =>  .(muc (dec muc))
               (add (lsh 4 muc i.f.rip) $(f.rip t.f.rip))
  (con (lsh 6 1 sec) fac)
::
++  yall
  |=  day=@ud
  ^-  [y=@ud m=@ud d=@ud]
  =+  [era=0 cet=0 lep=_?]
  =>  .(era (div day era:yo), day (mod day era:yo))
  =>  ^+  .
      ?:  (lth day +(cet:yo))
        .(lep &, cet 0)
      =>  .(lep |, cet 1, day (sub day +(cet:yo)))
      .(cet (add cet (div day cet:yo)), day (mod day cet:yo))
  =+  yer=(add (mul 400 era) (mul 100 cet))
  |-  ^-  [y=@ud m=@ud d=@ud]
  =+  dis=?:(lep 366 365)
  ?.  (lth day dis)
    =+  ner=+(yer)
    $(yer ner, day (sub day dis), lep =(0 (end 0 2 ner)))
  |-  ^-  [y=@ud m=@ud d=@ud]
  =+  [mot=0 cah=?:(lep moy:yo moh:yo)]
  |-  ^-  [y=@ud m=@ud d=@ud]
  =+  zis=(snag mot cah)
  ?:  (lth day zis)
    [yer +(mot) +(day)]
  $(mot +(mot), day (sub day zis))
::
++  yawn
  |=  [yer=@ud mot=@ud day=@ud]
  ^-  @ud
  =>  .(mot (dec mot), day (dec day))
  =>  ^+  .
      %=    .
          day
        =+  cah=?:((yelp yer) moy:yo moh:yo)
        |-  ^-  @ud
        ?:  =(0 mot)
          day
        $(mot (dec mot), cah (slag 1 cah), day (add day (snag 0 cah)))
      ==
  |-  ^-  @ud
  ?.  =(0 (mod yer 4))
    =+  ney=(dec yer)
    $(yer ney, day (add day ?:((yelp ney) 366 365)))
  ?.  =(0 (mod yer 100))
    =+  nef=(sub yer 4)
    $(yer nef, day (add day ?:((yelp nef) 1.461 1.460)))
  ?.  =(0 (mod yer 400))
    =+  nec=(sub yer 100)
    $(yer nec, day (add day ?:((yelp nec) 36.525 36.524)))
  (add day (mul (div yer 400) (add 1 (mul 4 36.524))))
::
++  yelp
  |=  yer=@ud  ^-  ?
  &(=(0 (mod yer 4)) |(!=(0 (mod yer 100)) =(0 (mod yer 400))))
::
++  yo
  |%  ++  cet  36.524                 ::  (add 24 (mul 100 365))
      ++  day  86.400                 ::  (mul 24 hor)
      ++  era  146.097                ::  (add 1 (mul 4 cet))
      ++  hor  3.600                  ::  (mul 60 mit)
      ++  jes  106.751.991.084.417    ::  (mul 730.692.561 era)
      ++  mit  60
      ++  moh  `(list ,@ud)`[31 28 31 30 31 30 31 31 30 31 30 31 ~]
      ++  moy  `(list ,@ud)`[31 29 31 30 31 30 31 31 30 31 30 31 ~]
      ++  qad  126.144.001            ::  (add 1 (mul 4 yer))
      ++  yer  31.536.000             ::  (mul 365 day)
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2cI, almost macros            ::
::
++  hard
  |*  han=$+(* *)
  |=  fud=*  ^-  han
  ~|  %hard
  =+  gol=(han fud)
  ?>(=(gol fud) gol)
::
++  soft
  |*  han=$+(* *)
  |=  fud=*  ^-  (unit han)
  =+  gol=(han fud)
  ?.(=(gol fud) ~ [~ gol])
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2d, containers                ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dA, sets                     ::
::
++  apt                                                 ::  set invariant
  |=  a=(tree)
  ?@  a
    &
  ?&  ?@(l.a & ?&((vor n.a n.l.a) (hor n.l.a n.a)))
      ?@(r.a & ?&((vor n.a n.r.a) (hor n.a n.r.a)))
  ==
::
++  in                                                  ::  set engine
  !:
  ~/  %in
  |/  a=(set)
  +-  all
    ~/  %all
    |*  b=$+(* ?)
    |-  ^-  ?
    ?@  a
      &
    ?&((b n.a) $(a l.a) $(a r.a))
  ::
  +-  any
    ~/  %any
    |*  b=$+(* ?)
    |-  ^-  ?
    ?@  a
      |
    ?|((b n.a) $(a l.a) $(a r.a))
  ::
  +-  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b n.a)
      ?:  (hor b n.a)
        [n.a $(a l.a) r.a]
      [n.a l.a $(a r.a)]
    |-  ^-  ?(~ _a)
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor n.l.a n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
  ::
  +-  dig
    |=  b=*
    =+  c=1
    |-  ^-  (unit ,@)
    ?~  a  ~
    ?:  =(b n.a)  [~ u=(peg c 2)]
    ?:  (gor b n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  +-  gas
    ~/  %gas
    |=  b=(list ,_?>(?=(^ a) n.a))
    |-  ^+  a
    ?@  b
      a
    $(b t.b, a (put(+< a) i.b))
  ::
  +-  has
    ~/  %has
    |*  b=*
    |-  ^-  ?
    ?@  a
      |
    ?:  =(b n.a)
      &
    ?:  (hor b n.a)
      $(a l.a)
    $(a r.a)
  ::
  +-  put
    ~/  %put
    |*  b=*
    |-  ^+  a
    ?@  a
      [b ~ ~]
    ?:  =(b n.a)
      a
    ?:  (hor b n.a)
      =+  c=$(a l.a)
      ?>  ?=(^ c)
      ?:  (vor n.a n.c)
        [n.a c r.a]
      [n.c l.c [n.a r.c r.a]]
    =+  c=$(a r.a)
    ?>  ?=(^ c)
    ?:  (vor n.a n.c)
      [n.a l.a c]
    [n.c [n.a l.a l.c] r.c]
  ::
  +-  rep
    |*  [b=* c=_,*]
    |-
    ?~  a  b
    $(a r.a, b $(a l.a, b (c n.a b)))
  ::
  +-  tap
    ~/  %tap
    |=  b=(list ,_?>(?=(^ a) n.a))
    ^+  b
    ?@  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  wyt
    .+
    |-  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dB, maps                     ::
::
++  ept                                                 ::  map invariant
  |=  a=(tree ,[p=* q=*])
  ?@  a
    &
  ?&  ?@(l.a & ?&((vor p.n.a p.n.l.a) (hor p.n.l.a p.n.a)))
      ?@(r.a & ?&((vor p.n.a p.n.r.a) (hor p.n.a p.n.r.a)))
  ==
::
++  by                                                  ::  map engine
  ~/  %by
  |/  a=(map)
  +-  all
    ~/  %all
    |*  b=$+(* ?)
    |-  ^-  ?
    ?@  a
      &
    ?&((b q.n.a) $(a l.a) $(a r.a))
  ::
  +-  any
    ~/  %any
    |*  b=$+(* ?)
    |-  ^-  ?
    ?@  a
      |
    ?|((b q.n.a) $(a l.a) $(a r.a))
  ::
  +-  del
    ~/  %del
    |*  b=*
    |-  ^+  a
    ?~  a
      ~
    ?.  =(b p.n.a)
      ?:  (gor b p.n.a)
        [n.a $(a l.a) r.a]
      [n.a l.a $(a r.a)]
    |-  ^-  ?(~ _a)
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (vor p.n.l.a p.n.r.a)
      [n.l.a l.l.a $(l.a r.l.a)]
    [n.r.a $(r.a l.r.a) r.r.a]
  ::
  +-  dig
    |=  b=*
    =+  c=1
    |-  ^-  (unit ,@)
    ?~  a  ~
    ?:  =(b p.n.a)  [~ u=(peg c 2)]
    ?:  (gor b p.n.a)
      $(a l.a, c (peg c 6))
    $(a r.a, c (peg c 7))
  ::
  +-  gas
    ~/  %gas
    |*  b=(list ,[p=* q=*])
    =>  .(b `(list ,_?>(?=(^ a) n.a))`b)
    |-  ^+  a
    ?@  b
      a
    $(b t.b, a (put(+< a) p.i.b q.i.b))
  ::
  +-  get
    ~/  %get
    |*  b=*
    |-  ^-  ?(~ [~ u=_?>(?=(^ a) q.n.a)])
    ?@  a
      ~
    ?:  =(b p.n.a)
      [~ u=q.n.a]
    ?:  (gor b p.n.a)
      $(a l.a)
    $(a r.a)
  ::
  +-  has
    ~/  %has
    |*  b=*
    !=(~ (get(+< a) b))
  ::
  +-  mar
    |*  [b=_?>(?=(^ a) p.n.a) c=(unit ,_?>(?=(^ a) q.n.a))]
    ?~  c
      (del b)
    (put b u.c)
  ::
  +-  put
    ~/  %put
    |*  [b=* c=*]
    |-  ^+  a
    ?@  a
      [[b c] ~ ~]
    ?:  =(b p.n.a)
      ?:  =(c q.n.a)
        a
      [[b c] l.a r.a]
    ?:  (gor b p.n.a)
      =+  d=$(a l.a)
      ?>  ?=(^ d)
      ?:  (vor p.n.a p.n.d)
        [n.a d r.a]
      [n.d l.d [n.a r.d r.a]]
    =+  d=$(a r.a)
    ?>  ?=(^ d)
    ?:  (vor p.n.a p.n.d)
      [n.a l.a d]
    [n.d [n.a l.a l.d] r.d]
  ::
  +-  rep
    |*  [b=* c=_,*]
    |-
    ?~  a  b
    $(a r.a, b $(a l.a, b (c n.a b)))
  ::
  +-  rib
    |*  [b=* c=_,*]
    |-  ^+  [b a]
    ?~  a  [b ~]
    =+  d=(c n.a b)
    =.  n.a  +.d
    =+  e=$(a l.a, b -.d)
    =+  f=$(a r.a, b -.e)
    [-.f [n.a +.e +.f]]
  ::
  +-  run
    |*  b=_,*
    |-
    ?~  a  a
    [[p.n.a (b q.n.a)] $(a l.a) $(a r.a)]
  ::
  +-  tap
    ~/  %tap
    |=  b=(list ,_?>(?=(^ a) n.a))
    ^+  b
    ?@  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  wyt
    .+
    |-  ^-  @
    ?~(a 0 +((add $(a l.a) $(a r.a))))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dC, queues                   ::
::
++  to                                                  ::  queue engine
  |/  a=(qeu)
  +-  bal
    |-  ^+  a
    ?~  a  ~
    ?.  |(?=(~ l.a) (vor n.a n.l.a))
      $(a [n.l.a l.l.a $(a [n.a r.l.a r.a])])
    ?.  |(?=(~ r.a) (vor n.a n.r.a))
      $(a [n.r.a $(a [n.a l.a l.r.a]) r.r.a])
    a
  ::
  +-  dep
    |-  ^-  @
    ?~  a  0
    +((max $(a l.a) $(a r.a)))
  ::
  +-  gas
    |=  b=(list ,_?>(?=(^ a) n.a))
    |-  ^+  a
    ?~(b a $(b t.b, a (put(+< a) i.b)))
  ::
  +-  get
    |-  ^+  [p=?>(?=(^ a) n.a) q=a]
    ?~  a
      !!
    ?~  r.a
      [n.a l.a]
    =+  b=$(a r.a)
    :-  p.b
    ?:  |(?=(~ q.b) (vor n.a n.q.b))
      [n.a l.a q.b]
    [n.q.b [n.a l.a l.q.b] r.q.b]
  ::
  +-  nap
    ?>  ?=(^ a)
    ?:  =(~ l.a)  r.a
    =+  b=get(+< l.a)
    bal(+< ^+(a [p.b q.b r.a]))
  ::
  +-  put
    |*  b=*
    |-  ^+  a
    ?~  a
      [b ~ ~]
    bal(+< a(l $(a l.a)))
  ::
  +-  tap
    |=  b=(list ,_?>(?=(^ a) n.a))
    ^+  b
    ?~  a
      b
    $(a r.a, b [n.a $(a l.a)])
  ::
  +-  top
    |-  ^-  (unit ,_?>(?=(^ a) n.a))
    ?~  a  ~
    ?~(r.a [~ n.a] $(a r.a))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2dD, casual containers        ::
::
++  mo                                                  ::  make a map
  |*  a=(list)
  =>  .(a `_(homo a)`a)
  =>  .(a `(list ,[p=_-<.a q=_->.a])`a)
  =+  b=*(map ,_?>(?=(^ a) p.i.a) ,_?>(?=(^ a) q.i.a))
  (~(gas by b) a)
::
++  sa                                                  ::  make a set
  |*  a=(list)
  =>  .(a `_(homo a)`a)
  =+  b=*(set ,_?>(?=(^ a) i.a))
  (~(gas in b) a)
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2e, miscellaneous libs        ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eA, packing                  ::
::
++  cue                                                 ::  unpack
  ~/  %cue
  |=  a=@
  ^-  *
  =+  b=0
  =+  m=`(map ,@ ,*)`~
  =<  q
  |-  ^-  [p=@ q=* r=_m]
  ?:  =(0 (cut 0 [b 1] a))
    =+  c=(rub +(b) a)
    [+(p.c) q.c (~(put by m) b q.c)]
  =+  c=(add 2 b)
  ?:  =(0 (cut 0 [+(b) 1] a))
    =+  u=$(b c)
    =+  v=$(b (add p.u c), m r.u)
    =+  w=[q.u q.v]
    [(add 2 (add p.u p.v)) w (~(put by r.v) b w)]
  =+  d=(rub c a)
  [(add 2 p.d) (need (~(get by m) q.d)) m]
::
++  jam                                                 ::  pack
  ~/  %jam
  |=  a=*
  ^-  @
  =+  b=0
  =+  m=`(map ,* ,@)`~
  =<  q
  |-  ^-  [p=@ q=@ r=_m]
  =+  c=(~(get by m) a)
  ?@  c
    =>  .(m (~(put by m) a b))
    ?:  ?=(@ a)
      =+  d=(mat a)
      [(add 1 p.d) (lsh 0 1 q.d) m]
    =>  .(b (add 2 b))
    =+  d=$(a -.a)
    =+  e=$(a +.a, b (add b p.d), m r.d)
    [(add 2 (add p.d p.e)) (mix 1 (lsh 0 2 (cat 0 q.d q.e))) r.e]
  ?:  ?&(?=(@ a) (lte (met 0 a) (met 0 u.c)))
    =+  d=(mat a)
    [(add 1 p.d) (lsh 0 1 q.d) m]
  =+  d=(mat u.c)
  [(add 2 p.d) (mix 3 (lsh 0 2 q.d)) m]
::
++  mat                                                 ::  length-encode
  ~/  %mat
  |=  a=@
  ^-  [p=@ q=@]
  ?:  =(0 a)
    [1 1]
  =+  b=(met 0 a)
  =+  c=(met 0 b)
  :-  (add (add c c) b)
  (cat 0 (bex c) (mix (end 0 (dec c) b) (lsh 0 (dec c) a)))
::
++  rub                                                 ::  length-decode
  ~/  %rub
  |=  [a=@ b=@]
  ^-  [p=@ q=@]
  =+  c==+(c=0 |-(?.(=(0 (cut 0 [(add a c) 1] b)) c $(c +(c)))))
  ?:  =(0 c)
    [1 0]
  =+  d=(add a +(c))
  =+  e=(add (bex (dec c)) (cut 0 [d (dec c)] b))
  [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eB, parsing (tracing)        ::
::
++  last  |=  [zyc=hair naz=hair]
          ^-  hair
          ?:  =(p.zyc p.naz)
            ?:((gth q.zyc q.naz) zyc naz)
          ?:((gth p.zyc p.naz) zyc naz)
::
++  lust  |=  [weq=char naz=hair]
          ^-  hair
          ?:(=(10 weq) [+(p.naz) 1] [p.naz +(q.naz)])
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eC, parsing (custom rules)   ::
::
++  cold
  ~/  %cold
  |*  [cus=* sef=_rule]
  ~/  %fun
  |=  tub=nail
  =+  vex=(sef tub)
  ?@  q.vex
    vex
  [p=p.vex q=[~ u=[p=cus q=q.u.q.vex]]]
::
++  cook
  ~/  %cook
  |*  [poq=_,* sef=_rule]
  ~/  %fun
  |=  tub=nail
  =+  vex=(sef tub)
  ?@  q.vex
    vex
  [p=p.vex q=[~ u=[p=(poq p.u.q.vex) q=q.u.q.vex]]]
::
++  easy
  ~/  %easy
  |*  huf=*
  ~/  %fun
  |=  tub=nail
  ^-  (like ,_huf)
  [p=p.tub q=[~ u=[p=huf q=tub]]]
::
++  fail  |=(tub=nail [p=p.tub q=~])
++  full
  |*  sef=_rule
  |=  tub=nail
  =+  vex=(sef tub)
  ?@(q.vex vex ?:(=(~ q.q.u.q.vex) vex [p=p.vex q=~]))
::
++  funk
  |*  [pre=tape sef=_rule]
  |=  tub=nail
  (sef p.tub (weld pre q.tub))
::
++  here
  ~/  %here
  |*  [hez=_|=([a=pint b=*] [a b]) sef=_rule]
  ~/  %fun
  |=  tub=nail
  =+  vex=(sef tub)
  ?@  q.vex
    vex
  [p=p.vex q=[~ u=[p=(hez [p.tub p.q.u.q.vex] p.u.q.vex) q=q.u.q.vex]]]
::
++  jest
  |=  daf=@t
  |=  tub=nail
  =+  fad=daf
  |-  ^-  (like ,@t)
  ?:  =(0 daf)
    [p=p.tub q=[~ u=[p=fad q=tub]]]
  ?:  |(?=(~ q.tub) !=((end 3 1 daf) i.q.tub))
    (fail tub)
  $(p.tub (lust i.q.tub p.tub), q.tub t.q.tub, daf (rsh 3 1 daf))
::
++  just                                                ::  XX redundant, jest
  ~/  %just
  |=  daf=char
  ~/  %fun
  |=  tub=nail
  ^-  (like char)
  ?@  q.tub
    (fail tub)
  ?.  =(daf i.q.tub)
    (fail tub)
  (next tub)
::
++  knee
  |*  [gar=* sef=_|.(rule)]
  |=  tub=nail
  ^-  (like ,_gar)
  ((sef) tub)
::
++  mask
  ~/  %mask
  |=  bud=(list char)
  ~/  %fun
  |=  tub=nail
  ^-  (like char)
  ?@  q.tub
    (fail tub)
  ?.  (lien bud |=(a=char =(i.q.tub a)))
    (fail tub)
  (next tub)
::
++  next
  |=  tub=nail
  ^-  (like char)
  ?@  q.tub
    (fail tub)
  =+  zac=(lust i.q.tub p.tub)
  [zac [~ i.q.tub [zac t.q.tub]]]
::
++  sear
  ~/  %sear
  |*  [pyq=_|=(* *(unit)) sef=_rule]
  ~/  %fun
  |=  tub=nail
  =+  vex=(sef tub)
  ?@  q.vex
    vex
  =+  gey=(pyq p.u.q.vex)
  ?@  gey
    [p=p.vex q=~]
  [p=p.vex q=[~ u=[p=u.gey q=q.u.q.vex]]]
::
++  shim
  ~/  %shim
  |=  zep=[p=@ q=@]
  ~/  %fun
  |=  tub=nail
  ^-  (like char)
  ?@  q.tub
    (fail tub)
  ?.  ?&((gte i.q.tub p.zep) (lte i.q.tub q.zep))
    (fail tub)
  (next tub)
::
++  stag
  ~/  %stag
  |*  [gob=* sef=_rule]
  ~/  %fun
  |=  tub=nail
  =+  vex=(sef tub)
  ?@  q.vex
    vex
  [p=p.vex q=[~ u=[p=[gob p.u.q.vex] q=q.u.q.vex]]]
::
++  stet
  |*  leh=(list ,[?(@ [@ @]) _rule])
  |-  
  ?~  leh 
    ~ 
  [i=[p=-.i.leh q=+.i.leh] t=$(leh t.leh)]
::
++  stew
  ~/  %stew
  |*  leh=(list ,[p=?(@ [@ @]) q=_rule])
  =+  ^=  wor
      |=  [ort=?(@ [@ @]) wan=?(@ [@ @])]
      ?@  ort
        ?@(wan (lth ort wan) (lth ort -.wan))
      ?@(wan (lth +.ort wan) (lth +.ort -.wan))
  =+  ^=  hel
      =+  hel=`(tree $_(?>(?=(^ leh) i.leh)))`~
      |-  ^+  hel
      ?~  leh
        ~
      =+  yal=$(leh t.leh)
      |-  ^+  hel
      ?~  yal
        [i.leh ~ ~]
      ?:  (wor p.i.leh p.n.yal)
        =+  nuc=$(yal l.yal)
        ?>  ?=(^ nuc)
        ?:  (vor p.n.yal p.n.nuc)
          [n.yal nuc r.yal]
        [n.nuc l.nuc [n.yal r.nuc r.yal]]
      =+  nuc=$(yal r.yal)
      ?>  ?=(^ nuc)
      ?:  (vor p.n.yal p.n.nuc)
        [n.yal l.yal nuc]
      [n.nuc [n.yal l.yal l.nuc] r.nuc]
  ~%  %fun  ..^$  ~
  |=  tub=nail
  ?@  q.tub
    (fail tub)
  |-
  ?@  hel
    (fail tub)
  ?:  ?@  p.n.hel
        =(p.n.hel i.q.tub)
      ?&((gte i.q.tub -.p.n.hel) (lte i.q.tub +.p.n.hel))
    ::  (q.n.hel [(lust i.q.tub p.tub) t.q.tub])
    (q.n.hel tub)
  ?:  (wor i.q.tub p.n.hel)
    $(hel l.hel)
  $(hel r.hel)
::
++  stir
  ~/  %stir
  |*  [rud=* raq=_|*([a=* b=*] [a b]) fel=_rule]
  ~/  %fun
  |=  tub=nail
  ^-  (like ,_rud)
  =+  vex=(fel tub)
  ?@  q.vex
    [p.vex [~ rud tub]]
  =+  wag=$(tub q.u.q.vex)
  ?>  ?=(^ q.wag)
  [(last p.vex p.wag) [~ (raq p.u.q.vex p.u.q.wag) q.u.q.wag]]
::
++  stun
  ~/  %stun
  |*  [[les=@ mos=@] fel=_rule]
  ~/  %fun
  |=  tub=nail
  ^-  (like (list ,_(wonk (fel))))
  ?:  =(0 mos)
    [p.tub [~ ~ tub]]
  =+  vex=(fel tub)
  ?@  q.vex
    ?:  =(0 les)
      [p.vex [~ ~ tub]]
    vex
  =+  ^=  wag  %=  $
                 les  ?:(=(0 les) 0 (dec les))
                 mos  ?:(=(0 mos) 0 (dec mos))
                 tub  q.u.q.vex
               ==
  ?@  q.wag
    wag
  [p.wag [~ [p.u.q.vex p.u.q.wag] q.u.q.wag]]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eD, parsing (combinators)    ::
::
++  bend
  ~/  %bend
  |*  raq=_|*([a=* b=*] [~ u=[a b]])
  ~/  %fun
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?@  q.yit
    [p=yur q=q.vex]
  =+  vux=(raq p.u.q.vex p.u.q.yit)
  ?~  vux
    [p=yur q=q.vex]
  [p=yur q=[~ u=[p=u.vux q=q.u.q.yit]]]
::
++  comp
  ~/  %comp
  |*  raq=_|*([a=* b=*] [a b])
  ~/  %fun
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?@  q.yit
    [p=yur q=q.yit]
  [p=yur q=[~ u=[p=(raq p.u.q.vex p.u.q.yit) q=q.u.q.yit]]]
::
++  glue
  ~/  %glue
  |*  bus=_rule
  ~/  %fun
  |*  [vex=edge sab=_rule]
  (plug vex ;~(pfix bus sab))
::
++  pfix
  ~/  %pfix
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  [p=(last p.yit p.vex) q=q.yit]
::
++  plug
  ~/  %plug
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  =+  yur=(last p.vex p.yit)
  ?@  q.yit
    [p=yur q=q.yit]
  [p=yur q=[~ u=[p=[p.u.q.vex p.u.q.yit] q=q.u.q.yit]]]
::
++  pose
  ~/  %pose
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    =+  roq=(sab)
    [p=(last p.vex p.roq) q=q.roq]
  vex
::
++  sfix
  ~/  %sfix
  |*  [vex=edge sab=_rule]
  ?@  q.vex
    vex
  =+  yit=(sab q.u.q.vex)
  [p=(last p.vex p.yit) q=?@(q.yit ~ [~ u=[p=p.u.q.vex q=q.u.q.yit]])]
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eE, parsing (composers)      ::
::
++  bass
  |*  [wuc=@ tyd=_rule]
  %+  cook
    |=  waq=(list ,@)
    %+  roll
      waq
    =|([p=@ q=@] |.((add p (mul wuc q))))
  tyd
::
++  boss
  |*  [wuc=@ tyd=_rule]
  %+  cook
    |=  waq=(list ,@)
    %+  reel
      waq
    =|([p=@ q=@] |.((add p (mul wuc q))))
  tyd
::
++  ifix
  |*  [fel=[p=_rule q=_rule] hof=_rule]
  ;~(pfix p.fel ;~(sfix hof q.fel))
::
++  more
  |*  [bus=_rule fel=_rule]
  ;~(pose (most bus fel) (easy ~))
::
++  most
  |*  [bus=_rule fel=_rule]
  ;~(plug fel (star ;~(pfix bus fel)))
::
++  plus  |*(fel=_rule ;~(plug fel (star fel)))
++  slug
  |*  [rud=* raq=_|*([a=* b=*] [a b])]
  |*  [bus=_rule fel=_rule]
  ;~((comp raq) fel (stir rud raq ;~(pfix bus fel)))
::
++  star
  |*  fel=_rule
  (stir `(list ,_(wonk *fel))`~ |*([a=* b=*] [a b]) fel)
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eF, parsing (ascii)          ::
::
++  ace  (just ' ')
++  bar  (just '|')
++  bas  (just '\\')
++  buc  (just '$')
++  cab  (just '_')
++  cen  (just '%')
++  col  (just ':')
++  com  (just ',')
++  doq  (just '"')
++  dot  (just '.')
++  fas  (just '/')
++  gal  (just '<')
++  gar  (just '>')
++  hax  (just '#')
++  kel  (just '{')
++  ker  (just '}')
++  ket  (just '^')
++  lus  (just '+')
++  hep  (just '-')
++  pel  (just '(')
++  pam  (just '&')
++  per  (just ')')
++  pat  (just '@')
++  sel  (just '[')
++  sem  (just ';')
++  ser  (just ']')
++  sig  (just '~')
++  soq  (just '\'')
++  tar  (just '*')
++  tec  (just '`')
++  tis  (just '=')
++  wut  (just '?')
++  zap  (just '!')
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eG, parsing (whitespace)     ::
::
++  dog  ;~(plug dot gay)
++  doh  ;~(plug ;~(plug hep hep) gay)
++  dun  (cold ~ ;~(plug hep hep))
++  duq  (cold ~ ;~(plug tis hep))
++  duz  (cold ~ ;~(plug tis tis))
++  gah  (mask [`@`10 ' ' ~])
++  gap  (cold ~ ;~(plug gaq (star ;~(pose vul gah))))
++  gaq  ;~(pose (just `@`10) ;~(plug gah ;~(pose gah vul)) vul)
++  gay  ;~(pose gap (easy ~))
++  vul  (cold ~ ;~(plug col col (star (shim 32 126)) (just ^-(@ 10))))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eH, parsing (idioms)         ::
::
++  alf  ;~(pose low hig)
++  aln  ;~(pose low hig nud)
++  alp  ;~(pose low hig nud hep)
++  bet  ;~(pose (cold 2 hep) (cold 3 lus))
++  bin  (bass 2 (most gon but))
++  but  (cook |=(a=@ (sub a '0')) (shim '0' '1'))
++  dem  (bass 10 (most gon dit))
++  dit  (cook |=(a=@ (sub a '0')) (shim '0' '9'))
++  gul  ;~(pose (cold 2 gal) (cold 3 gar))
++  gon  ;~(pose ;~(plug bas gay fas) (easy ~))
++  hex  (bass 16 (most gon hit))
++  hig  (shim 'A' 'Z')
++  hit  ;~  pose
           dit
           (cook |=(a=char (sub a 87)) (shim 'a' 'f'))
           (cook |=(a=char (sub a 55)) (shim 'A' 'F'))
         ==
++  low  (shim 'a' 'z')
++  mes  (cook |=([a=@ b=@] (add (mul 16 a) b)) ;~(plug hit hit))
++  nix  (boss 256 (star ;~(pose aln cab)))
++  nud  (shim '0' '9')
++  poy  ;~(pfix bas ;~(pose bas soq mes))
++  qit  ;~(pose (shim 32 38) (shim 40 91) (shim 93 126) (shim 128 255) poy)
++  qut  (ifix [soq soq] (boss 256 (more gon qit)))
++  sym
  %+  cook
    |=(a=tape (rap 3 ^-((list ,@) a)))
  ;~(plug low (star ;~(pose nud low hep)))
::
++  ven  ;~  (comp |=([a=@ b=@] (peg a b)))
           bet
           =+  hom=`?`|
           |=  tub=nail
           ^-  (like axis)
           =+  vex=?:(hom (bet tub) (gul tub))
           ?@  q.vex
             [p.tub [~ 1 tub]]
           =+  wag=$(p.tub p.vex, hom !hom, tub q.u.q.vex)
           ?>  ?=(^ q.wag)
           [p.wag [~ (peg p.u.q.vex p.u.q.wag) q.u.q.wag]]
         ==
++  vit
  ;~  pose
    (cook |=(a=@ (sub a 65)) (shim 'A' 'Z'))
    (cook |=(a=@ (sub a 71)) (shim 'a' 'z'))
    (cook |=(a=@ (add a 4)) (shim '0' '9'))
    (cold 62 (just '-'))
    (cold 63 (just '+'))
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eI, parsing (external)       ::
::
++  rash  |*([naf=@ sab=_rule] (scan (trip naf) sab))
++  rush  |*  [naf=@ sab=_rule]
          =+  vex=((full sab) [[1 1] (trip naf)])
          ?~(q.vex ~ [~ u=p.u.q.vex])
++  scan  |*  [los=tape sab=_rule]
          =+  vex=((full sab) [[1 1] los])
          ?@  q.vex
            ~_  (show [%m '{%d %d}'] p.p.vex q.p.vex ~)
            ~|('syntax-error' !!)
          p.u.q.vex
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eJ, formatting (basic text)  ::
::
++  cass                                                ::  case-insensitive
  |=  vib=tape
  %+  rap  3
  (turn vib |=(a=@ ?.(&((gte a 'A') (lte a 'Z')) a (add 32 a))))
::
++  crip  |=(a=tape `@t`(rap 3 a))
++  mesc
  |=  vib=tape
  ^-  tape
  ?@  vib
    ~
  ?:  =('\\' i.vib)
    ['\\' '\\' $(vib t.vib)]
  ?:  ?|((gth i.vib 126) (lth i.vib 32) =(39 i.vib))
    ['\\' (weld ~(rux at i.vib) (runt [1 47] $(vib t.vib)))]
  [i.vib $(vib t.vib)]
::
++  runt
  |=  [[a=@ b=@] c=tape]
  ^-  tape
  ?:  =(0 a)
    c
  [b $(a (dec a))]
::
++  sand                                                ::  atom sanity
  |=  a=@ta
  |=  b=@  ^-  (unit ,@)
  ?.(((sane a) b) ~ [~ b])
::
++  sane                                                ::  atom sanity
  |=  a=@ta
  |=  b=@  ^-  ?
  ?.  =(%t (end 3 1 a))
    ~|(%sane-stub !!)
  =+  [inx=0 len=(met 3 b)]
  ?:  =(%tas a)
    |-  ^-  ?
    ?:  =(inx len)  &
    =+  cur=(cut 3 [inx 1] b)
    ?&  ?|  &((gte cur 'a') (lte cur 'z'))
            &(=('-' cur) !=(0 inx) !=(len inx))
        ==
        $(inx +(inx))
    ==
  ?:  =(%ta a)
    |-  ^-  ?
    ?:  =(inx len)  &
    =+  cur=(cut 3 [inx 1] b)
    ?&  ?|  &((gte cur 'a') (lte cur 'z'))
            &((gte cur 'A') (lte cur 'Z'))
            |(=('-' cur) =('~' cur) =('_' cur) =('.' cur))
        ==
        $(inx +(inx))
    ==
  |-  ^-  ?
  ?:  =(0 b)  &
  =+  cur=(end 3 1 b)
  ?:  &((lth cur 32) !=(10 cur))  |
  =+  len=(teff cur)
  ?&  |(=(1 len) =+(i=1 |-(|(=(i len) &((gte (cut 3 [i 1] b) 128) $(i +(i)))))))
      $(b (rsh 3 len b))
  ==
::
++  trim
  |=  [a=@ b=tape]
  ^-  [p=tape q=tape]
  ?@  b
    [~ ~]
  ?:  =(0 a)
    [~ b]
  =+  c=$(a (dec a), b t.b)
  [[i.b p.c] q.c]
::
++  trip
  ~/  %trip
  |=  a=@  ^-  tape
  ?:  =(0 (met 3 a))
    ~
  [^-(@ta (end 3 1 a)) $(a (rsh 3 1 a))]
::
++  teff                                                ::  length utf8
  |=  a=@t  ^-  @
  =+  b=(end 3 1 a)
  ?:  =(0 b)
    ?>(=(0 a) 0)
  ?>  |((gte b 32) =(10 b))
  ?:((lte b 127) 1 ?:((lte b 223) 2 ?:((lte b 239) 3 4)))
::
++  turf                                                ::  utf8 to utf32
  |=  a=@t
  ^-  @c
  %+  rap  5
  |-  ^-  (list ,@c)
  =+  b=(teff a)
  ?:  =(0 b)  ~
  :-  %+  can  0
      %+  turn
        ^-  (list ,[p=@ q=@])
        ?+  b  !!
          1  [[0 7] ~]
          2  [[8 6] [0 5] ~]
          3  [[16 6] [8 6] [0 4] ~]
          4  [[24 6] [16 6] [8 6] [0 3] ~]
        ==
      |=([p=@ q=@] [q (cut 0 [p q] a)])
  $(a (rsh 3 b a))
::
++  tuba                                                ::  utf8 to utf32 tape
  |=  a=tape
  ^-  (list ,@c)
  (rip 5 (turf (rap 3 a)))                              ::  XX horrible
::
++  tufa                                                ::  utf32 to utf8 tape
  |=  a=(list ,@c)
  ^-  tape
  ?~  a  ""
  (weld (rip 3 (tuft i.a)) $(a t.a))
::
++  tuft                                                ::  utf32 to utf8 text
  |=  a=@c
  ^-  @t
  %+  rap  3
  |-  ^-  (list ,@)
  ?:  =(0 a)
    ~
  =+  b=(end 5 1 a)
  =+  c=$(a (rsh 5 1 a))
  ?:  (lth b 0x7f)
    [b c]
  ?:  (lth b 0x7ff)
    :*  (mix 0b1100.0000 (cut 0 [6 5] b))
        (mix 0b1000.0000 (end 0 6 b))
        c
    ==
  ?:  (lth b 0xffff)
    :*  (mix 0b1110.0000 (cut 0 [12 4] b))
        (mix 0b1000.0000 (cut 0 [6 6] b))
        (mix 0b1000.0000 (end 0 6 b))
        c
    ==
  :*  (mix 0b1111.0000 (cut 0 [18 3] b))
      (mix 0b1000.0000 (cut 0 [12 6] b))
      (mix 0b1000.0000 (cut 0 [6 6] b))
      (mix 0b1000.0000 (end 0 6 b))
      c
  ==
::
++  wack
  |=  a=@ta
  ^-  @ta
  =+  b=(rip 3 a)
  %+  rap  3
  |-  ^-  tape
  ?~  b
    ~
  ?:  =('~' i.b)  ['~' '~' $(b t.b)]
  ?:  =('_' i.b)  ['~' '-' $(b t.b)]
  [i.b $(b t.b)]
::
++  wick
  |=  a=@
  ^-  @ta
  =+  b=(rip 3 a)
  %+  rap  3
  |-  ^-  tape
  ?~  b
    ~
  ?:  =('~' i.b)
    ?~  t.b  !!
    [?:(=('~' i.t.b) '~' ?>(=('-' i.t.b) '_')) $(b t.t.b)]
  [i.b $(b t.b)]
::
++  woad
  |=  a=@ta
  ^-  @t
  %+  rap  3
  |-  ^-  (list ,@)
  ?:  =(0 a)
    ~
  =+  b=(end 3 1 a)
  =+  c=(rsh 3 1 a)
  ?:  =('.' b)
    [' ' $(a c)]
  ?.  =('~' b)
    [b $(a c)]
  =>  .(b (end 3 1 c), c (rsh 3 1 c))
  ?+  b  =-  (weld (rip 3 (tuft p.d)) $(a q.d))
         ^=  d
         =+  d=0
         |-  ^-  [p=@ q=@]
         ?:  =('.' b)
           [d c]
         ?<  =(0 c)
         %=    $
            b  (end 3 1 c)
            c  (rsh 3 1 c)
            d  %+  add  (mul 16 d)
               %+  sub  b
               ?:  &((gte b '0') (lte b '9'))  48
               ?>(&((gte b 'a') (lte b 'z')) 87)
         ==
    %'.'  ['.' $(a c)]
    %'~'  ['~' $(a c)]
  ==
::
++  wood
  |=  a=@t
  ^-  @ta
  %+  rap  3
  |-  ^-  (list ,@)
  ?:  =(0 a)
    ~
  =+  b=(teff a)
  =+  c=(turf (end 3 b a))
  =+  d=$(a (rsh 3 b a))
  ?:  ?|  &((gte c 'a') (lte c 'z'))
          &((gte c '0') (lte c '9'))
          =('-' c)
      ==
    [c d]
  ?+  c  
    :-  '~'
    =+  e=(met 2 c)
    |-  ^-  tape
    ?:  =(0 c)
      ['.' d]
    =.  e  (dec e)
    =+  f=(rsh 2 e c)
    [(add ?:((lte f 9) 48 87) f) $(c (end 2 e c))]
  ::
    %' '  ['.' d]
    %'.'  ['~' '.' d]
    %'~'  ['~' '~' d]
  ==
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eK, formatting (layout)      ::
::
++  re
  |_  tac=tank
  ++  ram
    ^-  tape
    ?-    -.tac
        %leaf  p.tac
        %palm  ram(tac [%rose [p.p.tac (weld q.p.tac r.p.tac) s.p.tac] q.tac])
        %rose
      %+  weld
        q.p.tac
      |-  ^-  tape
      ?@  q.tac
        r.p.tac
      =+  voz=$(q.tac t.q.tac)
      (weld ram(tac i.q.tac) ?@(t.q.tac voz (weld p.p.tac voz)))
    ==
  ::
  ++  win
    |=  [tab=@ edg=@]
    =+  lug=`wall`~
    |^  |-  ^-  wall
        ?-    -.tac
            %leaf  (rig p.tac)
            %palm
          ?:  fit
            (rig ram)
          ?@  q.tac
            (rig q.p.tac)
          ?@  t.q.tac
            (rig(tab (add 2 tab), lug $(tac i.q.tac)) q.p.tac)
          =>  .(q.tac `(list tank)`q.tac)
          =+  lyn=(mul 2 (lent q.tac))
          =+  ^=  qyr
              |-  ^-  wall
              ?@  q.tac
                lug
              %=  ^$
                tac  i.q.tac
                tab  (add tab (sub lyn 2))
                lug  $(q.tac t.q.tac, lyn (sub lyn 2))
              ==
          (wig(lug qyr) q.p.tac)
        ::
            %rose
          ?:  fit
            (rig ram)
          =+  ^=  gyl
            |-  ^-  wall
            ?@  q.tac
              ?:(=(%$ r.p.tac) lug (rig r.p.tac))
            ^$(tac i.q.tac, lug $(q.tac t.q.tac), tab din)
          ?:  =(%$ q.p.tac)
            gyl
          (wig(lug gyl) q.p.tac)
        ==
    ::
    ++  din  (mod (add 2 tab) (mul 2 (div edg 3)))
    ++  fit  (lte (lent ram) (sub edg tab))
    ++  rig
      |=  hom=tape
      ^-  wall
      ?:  (lte (lent hom) (sub edg tab))
        [(runt [tab ' '] hom) lug]
      =>  .(tab (add tab 2), edg (sub edg 2))
      =+  mut=(trim (sub edg tab) hom)
      :-  (runt [(sub tab 2) ' '] ['\\' '/' (weld p.mut `_hom`['\\' '/' ~])])
      =>  .(hom q.mut)
      |-
      ?@  hom
        :-  %+  runt
              [(sub tab 2) ' ']
            ['\\' '/' (runt [(sub edg tab) ' '] ['\\' '/' ~])]
        lug
      =>  .(mut (trim (sub edg tab) hom))
      [(runt [tab ' '] p.mut) $(hom q.mut)]
    ::
    ++  wig
      |=  hom=tape
      ^-  wall
      ?@  lug
        (rig hom)
      =+  lin=(lent hom)
      =+  wug=:(add 1 tab lin)
      ?.  =+  mir=i.lug
          |-  ?@  mir
                |
              ?|(=(0 wug) ?&(=(' ' i.mir) $(mir t.mir, wug (dec wug))))
        (rig hom)
      [(runt [tab ' '] (weld hom `tape`[' ' (slag wug i.lug)])) t.lug]
    --
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eL, formatting (path)        ::
::
++  ab
  |%
  ++  bix  (bass 16 (stun [2 2] six))
  ++  hif  (boss 256 ;~(plug tip tiq (easy ~)))
  ++  huf  %+  cook
             |=([a=@ b=@] (wred:un ~(zug mu ~(zag mu [a b]))))
           ;~(plug hif ;~(pfix hep hif))
  ++  hyf  (bass 0x1.0000.0000 ;~(plug huf ;~(pfix hep huf) (easy ~)))
  ++  pev  (bass 32 ;~(plug sev (stun [0 4] siv)))
  ++  pew  (bass 64 ;~(plug sew (stun [0 4] siw)))
  ++  piv  (bass 32 (stun [5 5] siv))
  ++  piw  (bass 64 (stun [5 5] siw))
  ++  qeb  (bass 2 ;~(plug seb (stun [0 3] sib)))
  ++  qex  (bass 16 ;~(plug sex (stun [0 3] hit)))
  ++  qib  (bass 2 (stun [4 4] sib))
  ++  qix  (bass 16 (stun [4 4] six))
  ++  seb  (cold 1 (just '1'))
  ++  sed  (cook |=(a=@ (sub a '0')) (shim '1' '9'))
  ++  sev  ;~(pose sed sov)
  ++  sew  ;~(pose sed sow)
  ++  sex  ;~(pose sed sox)
  ++  sib  (cook |=(a=@ (sub a '0')) (shim '0' '1'))
  ++  siq  ;~  pose
             (shim 'a' 'z')
             (shim 'A' 'Z')
             (shim '0' '9')
             hep
             (cold 32 dot)
             ;~(pfix sig ;~(pose sig dot bix))
           ==
  ++  sid  (cook |=(a=@ (sub a '0')) (shim '0' '9'))
  ++  siv  ;~(pose sid sov)
  ++  siw  ;~(pose sid sow)
  ++  six  ;~(pose sid sox)
  ++  sov  (cook |=(a=@ (sub a 87)) (shim 'a' 'v'))
  ++  sow  ;~  pose
             (cook |=(a=@ (sub a 87)) (shim 'a' 'z'))
             (cook |=(a=@ (sub a 29)) (shim 'A' 'Z'))
             (cold 62 (just '-'))
             (cold 63 (just '~'))
           ==
  ++  sox  (cook |=(a=@ (sub a 87)) (shim 'a' 'f'))
  ++  ted  (bass 10 ;~(plug sed (stun [0 2] sid)))
  ++  tip  (sear |=(a=@ (ins:po a)) til)
  ++  tiq  (sear |=(a=@ (ind:po a)) til)
  ++  tid  (bass 10 (stun [3 3] sid))
  ++  til  (boss 256 (stun [3 3] low))
  ++  urs  %+  cook
             |=(a=tape (rap 3 ^-((list ,@) a)))
           (star ;~(pose nud low hep dot sig cab)) 
  ++  voy  ;~(pfix bas ;~(pose bas soq bix))
  ++  vym  (bass 256 ;~(plug low (star ;~(pose low nud))))
  ++  vyn  (bass 256 ;~(plug hep vym (easy ~)))
  --
++  ag
  |%
  ++  ape  |*(fel=_rule ;~(pose (cold 0 (just '0')) fel))
  ++  bay  (ape (bass 16 ;~(plug qeb:ab (star ;~(pfix dog qib:ab)))))
  ++  bip  =+  tod=(ape qex:ab)
           (bass 0x1.0000 ;~(plug tod (stun [7 7] ;~(pfix dog tod))))
  ++  dem  (ape (bass 1.000 ;~(plug ted:ab (star ;~(pfix dog tid:ab)))))
  ++  dim  (ape (bass 10 ;~(plug sed:ab (star sid:ab))))
  ++  dum  (bass 10 (plus sid:ab))
  ++  fed  ;~  pose
             (bass 0x1.0000.0000.0000.0000 (most doh hyf:ab))
             huf:ab
             hif:ab
             tiq:ab
           ==
  ++  hex  (ape (bass 0x1.0000 ;~(plug qex:ab (star ;~(pfix dog qix:ab)))))
  ++  lip  =+  tod=(ape ted:ab)
           (bass 256 ;~(plug tod (stun [3 3] ;~(pfix dog tod))))
  ++  qut  %+  ifix  [soq soq]
           %+  boss  256
           %-  star  ;~  pose
                       ;~(pfix bas ;~(pose bas soq bix:ab))
                       ;~(pose (shim 32 38) (shim 40 91) (shim 93 126))
                     ==
  ++  sym  (cook |=(a=(list ,@) (rap 3 a)) ;~(plug vym:ab (star vyn:ab)))
  ++  tyq  (cook |=(a=(list ,@) (rap 3 a)) (plus siq:ab))
  ++  viz  (ape (bass 0x200.0000 ;~(plug pev:ab (star ;~(pfix dog piv:ab)))))
  ++  wiz  (ape (bass 0x4000.0000 ;~(plug pew:ab (star ;~(pfix dog piw:ab)))))
  --
::
++  co
  =<  |_  lot=coin
      ++  rear  |=(rom=tape =>(.(rex rom) rend))
      ++  rent  `@ta`(rap 3 rend)
      ++  rend
        ^-  tape
        ?:  ?=(%blob -.lot)
          ['~' '0' ((w-co 1) (jam p.lot))]
        ?:  ?=(%many -.lot)
          :-  '.'
          |-  ^-  tape
          ?~   p.lot
            ['_' '_' rex]
          ['_' rend(lot i.p.lot, rex $(p.lot t.p.lot))]
        =+  [yed=(end 3 1 p.p.lot) hay=(cut 3 [1 1] p.p.lot)]
        |-  ^-  tape
        ?+    yed  (z-co q.p.lot)
            %c   ['~' '-' (weld (rip 3 (wood (tuft q.p.lot))) rex)]
            %d
          ?+    hay  (z-co q.p.lot)
              %a
            =+  yod=(yore q.p.lot)
            =>  ^+(. .(rex ?~(f.t.yod rex ['.' (s-co f.t.yod)])))
            =>  ^+  .
                %=    .
                    rex
                  ?:  &(=(~ f.t.yod) =(0 h.t.yod) =(0 m.t.yod) =(0 s.t.yod))
                    rex
                  =>  .(rex ['.' (y-co s.t.yod)])
                  =>  .(rex ['.' (y-co m.t.yod)])
                  ['.' '.' (y-co h.t.yod)]
                ==
            =>  .(rex ['.' (a-co d.t.yod)])
            =>  .(rex ['.' (a-co m.yod)])
            =>  .(rex ?:(a.yod rex ['-' rex]))
            ['~' (a-co y.yod)]
          ::
              %r
            =+  yug=(yell q.p.lot)
            =>  ^+(. .(rex ?~(f.yug rex ['.' (s-co f.yug)])))
            :-  '~'
            ?:  &(=(0 d.yug) =(0 m.yug) =(0 h.yug) =(0 s.yug))
              ['.' 's' '0' rex]
            =>  ^+(. ?:(=(0 s.yug) . .(rex ['.' 's' (a-co s.yug)])))
            =>  ^+(. ?:(=(0 m.yug) . .(rex ['.' 'm' (a-co m.yug)])))
            =>  ^+(. ?:(=(0 h.yug) . .(rex ['.' 'h' (a-co h.yug)])))
            =>  ^+(. ?:(=(0 d.yug) . .(rex ['.' 'd' (a-co d.yug)])))
            +.rex
          ==
        ::
            %f
          ?:  =(& q.p.lot)
            ['.' 'y' rex]
          ?:(=(| q.p.lot) ['.' 'n' rex] (z-co q.p.lot))
        ::
            %n   ['~' rex]
            %i
          ?+  hay  (z-co q.p.lot)
            %f  ((ro-co [3 10 4] |=(a=@ ~(d ne a))) q.p.lot)
            %s  ((ro-co [4 16 8] |=(a=@ ~(x ne a))) q.p.lot)
          ==
        ::
            %p
          =+  dyx=(met 3 q.p.lot)
          :-  '~'
          ?:  (lte dyx 1)
            (weld (trip (tod:po q.p.lot)) rex)
          ?:  =(2 dyx)
            ;:  weld
              (trip (tos:po (end 3 1 q.p.lot)))
              (trip (tod:po (rsh 3 1 q.p.lot)))
              rex
            ==
          =+  [dyz=(met 5 q.p.lot) fin=|]
          |-  ^-  tape
          ?:  =(0 dyz)
            rex
          %=    $
              fin      &
              dyz      (dec dyz)
              q.p.lot  (rsh 5 1 q.p.lot)
              rex
            =+  syb=(wren:un (end 5 1 q.p.lot))
            =+  cog=~(zig mu [(rsh 4 1 syb) (end 4 1 syb)])
            ;:  weld
              (trip (tos:po (end 3 1 p.cog)))
              (trip (tod:po (rsh 3 1 p.cog)))
              `tape`['-' ~]
              (trip (tos:po (end 3 1 q.cog)))
              (trip (tod:po (rsh 3 1 q.cog)))
              `tape`?:(fin ['-' ?:(=(1 (end 0 1 dyz)) ~ ['-' ~])] ~)
              rex
            ==
          ==
        ::
            %r
          ?+  hay  (z-co q.p.lot)
            %d  ['.' '~' (r-co (rlyd q.p.lot))]
            %h  ['.' '~' '~' (r-co (rlyh q.p.lot))]
            %q  ['.' '~' '~' '~' (r-co (rlyq q.p.lot))]
            %s  ['.' (r-co (rlys q.p.lot))]
          ==
        ::
            %u
          =-  (weld p.gam ?:(=(0 q.p.lot) `tape`['0' ~] q.gam))
          ^=  gam  ^-  [p=tape q=tape]
          ?+  hay  [~ ((ox-co [10 3] |=(a=@ ~(d ne a))) q.p.lot)]
            %b  [['0' 'b' ~] ((ox-co [2 4] |=(a=@ ~(d ne a))) q.p.lot)]
            %x  [['0' 'x' ~] ((ox-co [16 4] |=(a=@ ~(x ne a))) q.p.lot)]
            %v  [['0' 'v' ~] ((ox-co [32 5] |=(a=@ ~(x ne a))) q.p.lot)]
            %w  [['0' 'w' ~] ((ox-co [64 5] |=(a=@ ~(w ne a))) q.p.lot)]
          ==
        ::
            %s
          %+  weld
            ?:((syn:si q.p.lot) "--" "-")
          $(yed 'u', q.p.lot (abs:si q.p.lot))
        ::
            %t
          ?:  =('a' hay)
            ?:  =('s' (cut 3 [2 1] p.p.lot))
              (weld (rip 3 q.p.lot) rex)
            ['~' '.' (weld (rip 3 (wack q.p.lot)) rex)]
          ['~' '~' (weld (rip 3 (wood q.p.lot)) rex)]
        ==
      --
  =+  rex=*tape
  =<  |%
      ++  a-co  |=(dat=@ ((d-co 1) dat))
      ++  d-co  |=(min=@ (em-co [10 min] |=([? b=@ c=tape] [~(d ne b) c])))
      ++  r-co
        |=  [syn=? nub=@ der=@]
        =>  .(rex ['.' ((d-co 1) der)])
        =>  .(rex ((d-co 1) nub))
        ?:(syn rex ['-' rex])
      ::
      ++  s-co
        |=  esc=(list ,@)  ^-  tape
        ~|  [%so-co esc]
        ?~  esc
          rex
        :-  '.'
        =>(.(rex $(esc t.esc)) ((x-co 4) i.esc))
    ::
      ++  w-co  |=(min=@ (em-co [64 min] |=([? b=@ c=tape] [~(w ne b) c])))
      ++  x-co  |=(min=@ (em-co [16 min] |=([? b=@ c=tape] [~(x ne b) c])))
      ++  y-co  |=(dat=@ ((d-co 2) dat))
      ++  z-co  |=(dat=@ `tape`['0' 'x' ((x-co 1) dat)])
      --
  |%
  ++  em-co
    |=  [[bas=@ min=@] [par=$+([? @ tape] tape)]]
    |=  hol=@
    ^-  tape
    ?:  &(=(0 hol) =(0 min))
      rex
    =+  [rad=(mod hol bas) dar=(div hol bas)]
    %=  $
      min  ?:(=(0 min) 0 (dec min))
      hol  dar
      rex  (par =(0 dar) rad rex)
    ==
  ::
  ++  ox-co
    |=  [[bas=@ gop=@] dug=$+(@ @)]
    %+  em-co
      [|-(?:(=(0 gop) 1 (mul bas $(gop (dec gop))))) 0]
    |=  [top=? seg=@ res=tape]
    %+  weld
      ?:(top ~ `tape`['.' ~])
    %.  seg
    %+  em-co(rex res)
      [bas ?:(top 0 gop)]
    |=([? b=@ c=tape] [(dug b) c])
  ::
  ++  ro-co
    |=  [[buz=@ bas=@ dop=@] dug=$+(@ @)]
    |=  hol=@
    ^-  tape
    ?:  =(0 dop)
      rex
    =>  .(rex $(dop (dec dop)))
    :-  '.'
    %-  (em-co [bas 1] |=([? b=@ c=tape] [(dug b) c]))
    [(cut buz [(dec dop) 1] hol)]
  --
::
++  ne
  |_  tig=@
  ++  d  (add tig '0')
  ++  x  ?:((gte tig 10) (add tig 87) d)
  ++  w  ?:(=(tig 63) '~' ?:(=(tig 62) '-' ?:((gte tig 36) (add tig 29) x)))
  --
::
++  mu
  |_  [top=@ bot=@]
  ++  zag  [p=(end 4 1 (add top bot)) q=bot]
  ++  zig  [p=(end 4 1 (add top (sub 0x1.0000 bot))) q=bot]
  ++  zug  (mix (lsh 4 1 top) bot)
  --
::
++  so
  |%
  ++  bisk
    ;~  pose
      ;~  pfix  (just '0')
        ;~  pose
          (stag %ub ;~(pfix (just 'b') bay:ag))
          (stag %ux ;~(pfix (just 'x') hex:ag))
          (stag %uv ;~(pfix (just 'v') viz:ag))
          (stag %uw ;~(pfix (just 'w') wiz:ag))
        ==
      ==
      (stag %ud dem:ag)
    ==
  ++  crub
    ;~  pose
      %+  cook
        |=(det=date `dime`[%da (year det)])
      ;~  plug
        %+  cook
          |=([a=@ b=?] [b a])
        ;~(plug dim:ag ;~(pose (cold | hep) (easy &)))
        ;~(pfix dot dim:ag)   ::  month
        ;~(pfix dot dim:ag)   ::  day
        ;~  pose
          ;~  pfix
            ;~(plug dot dot)
            ;~  plug
              dum:ag
              ;~(pfix dot dum:ag)
              ;~(pfix dot dum:ag)
              ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
            ==
          ==
          (easy [0 0 0 ~])
        ==
      ==
    ::
      %+  cook
        |=  [a=(list ,[p=?(%d %h %m %s) q=@]) b=(list ,@)]
        =+  rop=`tarp`[0 0 0 0 b]
        |-  ^-  dime
        ?~  a
          [%dr (yule rop)]
        ?-  p.i.a
          %d  $(a t.a, d.rop (add q.i.a d.rop))
          %h  $(a t.a, h.rop (add q.i.a h.rop))
          %m  $(a t.a, m.rop (add q.i.a m.rop))
          %s  $(a t.a, s.rop (add q.i.a s.rop))
        ==
      ;~  plug
        %+  most
          dot
        ;~  pose
          ;~(pfix (just 'd') (stag %d dim:ag))
          ;~(pfix (just 'h') (stag %h dim:ag))
          ;~(pfix (just 'm') (stag %m dim:ag))
          ;~(pfix (just 's') (stag %s dim:ag))
        ==
        ;~(pose ;~(pfix ;~(plug dot dot) (most dot qix:ab)) (easy ~))
      ==
    ::
      (stag %p fed:ag)
      ;~(pfix dot (stag %ta (cook wick urs:ab)))
      ;~(pfix sig (stag %t (cook woad urs:ab)))
      ;~(pfix hep (stag %c (cook turf (cook woad urs:ab))))
    ==
  ++  nuck
    %+  knee  *coin  |.  ~+
    %-  stew  
    ^.  stet  ^.  limo
    :~  :-  ['a' 'z']  (cook |=(a=@ta [~ %tas a]) sym)
        :-  ['0' '9']  (stag ~ bisk)
        :-  '-'        (stag ~ tash)
        :-  '.'        ;~(pfix dot perd)
        :-  '~'        ;~(pfix sig ;~(pose twid (easy [~ %n 0])))
    ==
  ++  perd
    ;~  pose
      (stag ~ zust)
      (stag %many (ifix [cab ;~(plug cab cab)] (more cab nuck)))
    ==
  ++  royl
    =+  ^=  vox
        ;~  plug
          ;~(pose (cold | hep) (easy &))
          ;~(plug dim:ag ;~(pose ;~(pfix dot dim:ag) (easy 0)))
        ==
    ;~  pose
      (stag %rh (cook rylh ;~(pfix ;~(plug sig sig) vox)))
      (stag %rq (cook rylq ;~(pfix ;~(plug sig sig sig) vox)))
      (stag %rd (cook ryld ;~(pfix sig vox)))
      (stag %rs (cook ryls vox))
    ==
  ++  tash
    =+  ^=  neg
        |=  [syn=? mol=dime]  ^-  dime
        ?>  =('u' (end 3 1 p.mol))
        [(cat 3 's' (rsh 3 1 p.mol)) (new:si syn q.mol)]
    ;~  pfix  hep
      ;~  pose
        (cook |=(a=dime (neg | a)) bisk)
        ;~(pfix hep (cook |=(a=dime (neg & a)) bisk))
      ==
    ==
  ++  twid
    ;~  pose
      (cook |=(a=@ [%blob (cue a)]) ;~(pfix (just '0') wiz:ag))
      (stag ~ crub)
    ==
  ::
  ++  zust
    ;~  pose
      (stag %is bip:ag)
      (stag %if lip:ag)
      (stag %f ;~(pose (cold & (just 'y')) (cold | (just 'n'))))
      royl
    ==
  --
++  scot  |=(mol=dime ~(rent co %$ mol))
++  scow  |=(mol=dime ~(rend co %$ mol))
++  slaw
  |=  [mod=@tas txt=@ta]
  ^-  (unit ,@)
  =+  con=(slay txt)
  ?.(&(?=([~ %$ @ @] con) =(p.p.u.con mod)) ~ [~ q.p.u.con])
::
++  slay
  |=  txt=@ta  ^-  (unit coin)
  =+  vex=((full nuck:so) [[1 1] (trip txt)])
  ?@  q.vex
    ~
  [~ p.u.q.vex]
::
++  smyt
  |=  bon=path  ^-  tank
  :+  %rose  [['/' ~] ['/' ~] ['/' ~]]
  |-  ^-  (list tank)
  (turn bon |=(a=@ [%leaf (rip 3 a)]))
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eM, pseudo-cryptography      ::
::
++  un                                                  ::  =(x (wred (wren x)))
  |%
  ++  wren                                              ::  conceal structure
    |=  pyn=@  ^-  @
    =+  len=(met 3 pyn)
    ?:  =(0 len)
      0
    =>  .(len (dec len))
    =+  mig=(zaft (xafo len (cut 3 [len 1] pyn)))
    %+  can  3
    %-  flop  ^-  (list ,[@ @])
    :-  [1 mig]
    |-  ^-  (list ,[@ @])
    ?:  =(0 len)
      ~
    =>  .(len (dec len))
    =+  mog=(zyft :(mix mig (end 3 1 len) (cut 3 [len 1] pyn)))
    [[1 mog] $(mig mog)]
  ::
  ++  wred                                              ::  restore structure
    |=  cry=@  ^-  @
    =+  len=(met 3 cry)
    ?:  =(0 len)
      0
    =>  .(len (dec len))
    =+  mig=(cut 3 [len 1] cry)
    %+  can  3
    %-  flop  ^-  (list ,[@ @])
    :-  [1 (xaro len (zart mig))]
    |-  ^-  (list ,[@ @])
    ?:  =(0 len)
      ~
    =>  .(len (dec len))
    =+  mog=(cut 3 [len 1] cry)
    [[1 :(mix mig (end 3 1 len) (zyrt mog))] $(mig mog)]
  ::
  ++  xafo  |=([a=@ b=@] +((mod (add (dec b) a) 255)))
  ++  xaro  |=([a=@ b=@] +((mod (add (dec b) (sub 255 (mod a 255))) 255)))
  ::
  ++  zaft                                              ::  forward 255-sbox
    |=  a=@
    =+  ^=  b
        0xcc.75bc.86c8.2fb1.9a42.f0b3.79a0.92ca.21f6.1e41.cde5.fcc0.
        7e85.51ae.1005.c72d.1246.07e8.7c64.a914.8d69.d9f4.59c2.8038.
        1f4a.dca2.6fdf.66f9.f561.a12e.5a16.f7b0.a39f.364e.cb70.7318.
        1de1.ad31.63d1.abd4.db68.6a33.134d.a760.edee.5434.493a.e323.
        930d.8f3d.3562.bb81.0b24.43cf.bea5.a6eb.52b4.0229.06b2.6704.
        78c9.45ec.d75e.58af.c577.b7b9.c40e.017d.90c3.87f8.96fa.1153.
        0372.7f30.1c32.ac83.ff17.c6e4.d36d.6b55.e2ce.8c71.8a5b.b6f3.
        9d4b.eab5.8b3c.e7f2.a8fe.9574.5de0.bf20.3f15.9784.9939.5f9c.
        e609.564f.d8a4.b825.9819.94aa.2c08.8e4c.9b22.477a.2840.3ed6.
        3750.6ef1.44dd.89ef.6576.d00a.fbda.9ed2.3b6c.7b0c.bde9.2ade.
        5c88.c182.481a.1b0f.2bfd.d591.2726.57ba
    (cut 3 [(dec a) 1] b)
  ::
  ++  zart                                              ::  reverse 255-sbox
    |=  a=@
    =+  ^=  b
        0x68.4f07.ea1c.73c9.75c2.efc8.d559.5125.f621.a7a8.8591.5613.
        dd52.40eb.65a2.60b7.4bcb.1123.ceb0.1bd6.3c84.2906.b164.19b3.
        1e95.5fec.ffbc.f187.fbe2.6680.7c77.d30e.e94a.9414.fd9a.017d.
        3a7e.5a55.8ff5.8bf9.c181.e5b6.6ab2.35da.50aa.9293.3bc0.cdc6.
        f3bf.1a58.4130.f844.3846.744e.36a0.f205.789e.32d8.5e54.5c22.
        0f76.fce7.4569.0d99.d26e.e879.dc16.2df4.887f.1ffe.4dba.6f5d.
        bbcc.2663.1762.aed7.af8a.ca20.dbb4.9bc7.a942.834c.105b.c4d4.
        8202.3e61.a671.90e6.273d.bdab.3157.cfa4.0c2e.df86.2496.f7ed.
        2b48.2a9d.5318.a343.d128.be9c.a5ad.6bb5.6dfa.c5e1.3408.128d.
        2c04.0339.97a1.2ff0.49d0.eeb8.6c0a.0b37.b967.c347.d9ac.e072.
        e409.7b9f.1598.1d3f.33de.8ce3.8970.8e7a
    (cut 3 [(dec a) 1] b)
  ::
  ++  zyft                                              ::  forward 256-sbox
    |=  a=@
    =+  ^=  b
        0xbb49.b71f.b881.b402.17e4.6b86.69b5.1647.115f.dddb.7ca5.
          8371.4bd5.19a9.b092.605d.0d9b.e030.a0cc.78ba.5706.4d2d.
          986a.768c.f8e8.c4c7.2f1c.effe.3cae.01c0.253e.65d3.3872.
          ce0e.7a74.8ac6.daac.7e5c.6479.44ec.4143.3d20.4af0.ee6c.
          c828.deca.0377.249f.ffcd.7b4f.eb7d.66f2.8951.042e.595a.
          8e13.f9c3.a79a.f788.6199.9391.7fab.6200.4ce5.0758.e2f1.
          7594.c945.d218.4248.afa1.e61a.54fb.1482.bea4.96a2.3473.
          63c2.e7cb.155b.120a.4ed7.bfd8.b31b.4008.f329.fca3.5380.
          9556.0cb2.8722.2bea.e96e.3ac5.d1bc.10e3.2c52.a62a.b1d6.
          35aa.d05e.f6a8.0f3b.31ed.559d.09ad.f585.6d21.fd1d.8d67.
          370b.26f4.70c1.b923.4684.6fbd.cf8b.5036.0539.9cdc.d93f.
          9068.1edf.8f33.b632.d427.97fa.9ee1
    (cut 3 [a 1] b)
  ::
  ++  zyrt                                              ::  reverse 256-sbox
    |=  a=@
    =+  ^=  b
        0x9fc8.2753.6e02.8fcf.8b35.2b20.5598.7caa.c9a9.30b0.9b48.
          47ce.6371.80f6.407d.00dd.0aa5.ed10.ecb7.0f5a.5c3a.e605.
          c077.4337.17bd.9eda.62a4.79a7.ccb8.44cd.8e64.1ec4.5b6b.
          1842.ffd8.1dfb.fd07.f2f9.594c.3be3.73c6.2cb6.8438.e434.
          8d3d.ea6a.5268.72db.a001.2e11.de8c.88d3.0369.4f7a.87e2.
          860d.0991.25d0.16b9.978a.4bf4.2a1a.e96c.fa50.85b5.9aeb.
          9dbb.b2d9.a2d1.7bba.66be.e81f.1946.29a8.f5d2.f30c.2499.
          c1b3.6583.89e1.ee36.e0b4.6092.937e.d74e.2f6f.513e.9615.
          9c5d.d581.e7ab.fe74.f01b.78b1.ae75.af57.0ec2.adc7.3245.
          12bf.2314.3967.0806.31dc.cb94.d43f.493c.54a6.0421.c3a1.
          1c4a.28ac.fc0b.26ca.5870.e576.f7f1.616d.905f.ef41.33bc.
          df4d.225e.2d56.7fd6.1395.a3f8.c582
    (cut 3 [a 1] b)
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eN, virtualization           ::
::
++  mack
  |=  [sub=* fol=*]
  ^-  (unit)
  =+  ton=(mink [sub fol] |=(* ~))
  ?.(?=([0 *] ton) ~ [~ p.ton])
::
++  mink
  ~/  %mink
  |=  [[sub=* fol=*] sky=$+(* (unit))]
  =+  tax=*(list ,[@ta *])
  |-  ^-  tone
  ?@  fol
    [%2 tax]
  ?:  ?=(^ -.fol)
    =+  hed=$(fol -.fol)
    ?:  ?=(2 -.hed)
      hed
    =+  tal=$(fol +.fol)
    ?-  -.tal
      0  ?-(-.hed 0 [%0 p.hed p.tal], 1 hed)
      1  ?-(-.hed 0 tal, 1 [%1 (weld p.hed p.tal)])
      2  tal
    ==
  ?-    fol
  ::
      [0 b=@]
    ?:  =(0 b.fol)  [%2 tax]
    ?:  =(1 b.fol)  [%0 sub]
    ?:  ?=(@ sub)   [%2 tax]
    =+  [now=(cap b.fol) lat=(mas b.fol)]
    $(b.fol lat, sub ?:(=(2 now) -.sub +.sub))
  ::
      [1 b=*]
    [%0 b.fol]
  ::
      [2 b=^ c=*]
    =+  ben=$(fol [b.fol c.fol])
    ?.  ?=(0 -.ben)  ben
    ?>(?=(^ p.ben) $(sub -.p.ben, fol +.p.ben))
  ::
      [3 b=*]
    =+  ben=$(fol b.fol)
    ?.  ?=(0 -.ben)  ben
    [%0 .?(p.ben)]
  ::
      [4 b=*]
    =+  ben=$(fol b.fol)
    ?.  ?=(0 -.ben)  ben
    ?.  ?=(@ p.ben)  [%2 tax]
    [%0 .+(p.ben)]
  ::
      [5 b=*]
    =+  ben=$(fol b.fol)
    ?.  ?=(0 -.ben)  ben
    ?.  ?=(^ p.ben)  [%2 tax]
    [%0 =(-.p.ben +.p.ben)]
  ::
      [6 b=* c=* d=*]
    $(fol =>(fol [2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]))
  ::
      [7 b=* c=*]       $(fol =>(fol [2 b 1 c]))
      [8 b=* c=*]       $(fol =>(fol [7 [[0 1] b] c]))
      [9 b=* c=*]       $(fol =>(fol [7 c 0 b]))
      [10 @ c=*]        $(fol c.fol)
      [10 [* c=*] d=*]
    =+  ben=$(fol c.fol)
    ?.  ?=(0 -.ben)  ben
    ?:  ?=(?(%hunk %lose %mean %spot) +<-.fol)
      $(fol d.fol, tax [[+<-.fol p.ben] tax])
    $(fol d.fol)
  ::
      [11 b=*]
    =+  ben=$(fol b.fol)
    ?.  ?=(0 -.ben)  ben
    =+  val=(sky p.ben)
    ?@(val [%1 p.ben ~] [%0 +.val])
  ::
      *
    [%2 tax]
  ==
::
++  mock
  |=  [[sub=* fol=*] sky=$+(* (unit))]
  (mook (mink [sub fol] sky))
::
++  mook
  |=  ton=tone
  ^-  toon
  ?.  ?=([2 *] ton)  ton
  :-  %2
  =+  yel=(lent p.ton)
  =.  p.ton
    ?.  (gth yel 256)  p.ton
    %+  weld
      (scag 128 p.ton)
    ^-  (list ,[@ta *])
    :_  (slag (sub yel 128) p.ton)
    :-  %lose
    %+  rap  3
    ;:  weld
      "[skipped "
      ~(rend co %$ %ud (sub yel 256))
      " frames]"
    ==
  |-  ^-  (list tank)
  ?~  p.ton  ~
  =+  rex=$(p.ton t.p.ton)
  ?+    -.i.p.ton  rex
      %hunk  [(tank +.i.p.ton) rex]
      %lose  [[%leaf (rip 3 (,@ +.i.p.ton))] rex]
      %mean  :_  rex
             ?@  +.i.p.ton  [%leaf (rip 3 (,@ +.i.p.ton))]
             =+  mac=(mack +.i.p.ton +<.i.p.ton)
             ?~(mac [%leaf "####"] (tank u.mac))
      %spot  :_  rex
             =+  sot=(spot +.i.p.ton)
             :-  %leaf
             ;:  weld
               ~(ram re (smyt p.sot))
               ":<["
               ~(rend co ~ %ud p.p.q.sot)
               " "
               ~(rend co ~ %ud q.p.q.sot)
               "].["
               ~(rend co ~ %ud p.q.q.sot)
               " "
               ~(rend co ~ %ud q.q.q.sot)
               "]>"
             ==
  ==
::
++  mang
  |=  [[gat=* sam=*] sky=$+(* (unit))]
  ^-  (unit)
  =+  ton=(mong [[gat sam] sky])
  ?.(?=([0 *] ton) ~ [~ p.ton])
::
++  mong
  |=  [[gat=* sam=*] sky=$+(* (unit))]
  ^-  toon
  ?.  &(?=(^ gat) ?=(^ +.gat))
    [%2 ~]
  (mock [[-.gat [sam +>.gat]] -.gat] sky)
::
++  mung
  |=  [[gat=* sam=*] sky=$+(* (unit))]
  ^-  tone
  ?.  &(?=(^ gat) ?=(^ +.gat))
    [%2 ~]
  (mink [[-.gat [sam +>.gat]] -.gat] sky)
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eO, diff (move me)           ::
::
::
++  berk                                                ::  invert diff patch
  |*  bur=(urge)
  |-  ^+  bur
  ?~  bur  ~
  :_  $(bur t.bur)
  ?-  -.i.bur
    &  i.bur
    |  [%| q.i.bur p.i.bur]
  ==
::
++  diff                                                ::  generate patch
  |=  pum=umph
  |=  [old=* new=*]  ^-  udon
  :-  pum
  ?+  pum  ~|(%unsupported !!)
    %a  [%a old new]
    %c  =+  [hel=(lore ((hard ,@) old)) hev=(lore ((hard ,@) new))]
        [%c (lusk hel hev (loss hel hev))]
  ==
::
++  loss                                                ::  longest subsequence
  ~/  %loss
  |*  [hel=(list) hev=(list)]
  |-  ^+  hev
  =+  ^=  sev
      =+  [inx=0 sev=*(map ,@t (list ,@ud))]
      |-  ^+  sev
      ?~  hev  sev
      =+  guy=(~(get by sev) i.hev)
      $(hev t.hev, inx +(inx), sev (~(put by sev) i.hev [inx ?~(guy ~ u.guy)]))
  =|  gox=[p=@ud q=(map ,@ud ,[p=@ud q=_hev])]
  =<  abet
  =<  main
  |%
  ++  abet                                              ::  subsequence
    ^+  hev
    ?:  =(0 p.gox)  ~
    (flop q:(need (~(get by q.gox) (dec p.gox))))
  ::
  ++  hink                                              ::  extend fits top
    |=  [inx=@ud goy=@ud]  ^-  ?
    |(=(p.gox inx) (lth goy p:(need (~(get by q.gox) inx))))
  ::
  ++  lonk                                              ::  extend fits bottom
    |=  [inx=@ud goy=@ud]  ^-  ?
    |(=(0 inx) (gth goy p:(need (~(get by q.gox) (dec inx)))))
  ::
  ++  lune                                              ::  extend
    |=  [inx=@ud goy=@ud]
    ^+  +>
    %_    +>.$
        gox
      :-  ?:(=(inx p.gox) +(p.gox) p.gox)
      %+  ~(put by q.gox)  inx
      [goy (snag goy hev) ?:(=(0 inx) ~ q:(need (~(get by q.gox) (dec inx))))]
    ==
  ::
  ++  merg                                              ::  merge all matches
    |=  gay=(list ,@ud)
    ^+  +>
    =+  ^=  zes
        =+  [inx=0 zes=*(list ,[p=@ud q=@ud])]
        |-  ^+  zes
        ?:  |(?=(~ gay) (gth inx p.gox))  zes
        ?.  (lonk inx i.gay)  $(gay t.gay)
        ?.  (hink inx i.gay)  $(inx +(inx))
        $(inx +(inx), gay t.gay, zes [[inx i.gay] zes])
    |-  ^+  +>.^$
    ?~(zes +>.^$ $(zes t.zes, +>.^$ (lune i.zes)))
  ::
  ++  main
    =+  hol=hel
    |-  ^+  +>
    ?~  hol  +>
    =+  guy=(~(get by sev) i.hol)
    $(hol t.hol, +> (merg (flop `(list ,@ud)`?~(guy ~ u.guy))))
  --
::
++  locz                                                ::  trivial algorithm
  |=  [hel=tape hev=tape]
  ^-  tape
  =+  [leh=(lent hel) veh=(lent hev)]
  =-  (flop q.yun)
  ^=  yun
  |-  ^-  [p=@ud q=tape]
  ~+
  ?:  |(=(0 leh) =(0 veh))  [0 ~]
  =+  [dis=(snag (dec leh) hel) dat=(snag (dec veh) hev)]
  ?:  =(dis dat)
    =+  say=$(leh (dec leh), veh (dec veh))
    [+(p.say) [dis q.say]]
  =+  [lef=$(leh (dec leh)) rig=$(veh (dec veh))]
  ?:((gth p.lef p.rig) lef rig)
::
++  lore                                                ::  atom to line list
  ~/  %lore
  |=  lub=@
  =|  tez=(list ,@t)
  |-  ^+  tez
  ?:  =(0 lub)  (flop tez)
  =+  ^=  meg
      =+  meg=0
      |-  ^-  @ud
      =+  gam=(cut 3 [meg 1] lub)
      ?:(|(=(10 gam) =(0 gam)) meg $(meg +(meg)))
  $(lub (rsh 3 +(meg) lub), tez [(end 3 meg lub) tez])
::
++  role                                                ::  line list to atom
  |=  tez=(list ,@t)
  (rap 3 (turn tez |=(a=@t (cat 3 a 10))))
::
++  lump                                                ::  apply patch
  |=  [don=udon src=*]
  ^-  *
  ?+    p.don  ~|(%unsupported !!)
      %a
    ?+  -.q.don  ~|(%unsupported !!)
      %a  q.p.q.don
      %c  (lurk ((hard (list)) src) p.q.don)
    ==
  ::
      %c
    =+  dst=(lore ((hard ,@) src))
    %-  role
    ?+  -.q.don  ~|(%unsupported !!)
      %a  ((hard (list ,@t)) q.p.q.don)
      %c  (lurk dst p.q.don)
    ==
  ==
::
++  limp                                                ::  invert patch
  |=  don=udon  ^-  udon
  :-  p.don
  ?+  -.q.don  ~|(%unsupported !!)
    %a  [%a q.p.q.don p.p.q.don]
    %c  [%c (berk p.q.don)]
    %d  [%d q.q.don p.q.don]
  ==
::
++  hump                                                ::  general prepatch
  |=  [pum=umph src=*]  ^-  *
  ?+  pum  ~|(%unsupported !!)
    %a   src
    %c  (lore ((hard ,@) src))
  ==
::
++  husk                                                ::  unprepatch
  |=  [pum=umph dst=*]  ^-  *
  ?+  pum  ~|(%unsupported !!)
    %a  dst
    %c  (role ((hard (list ,@)) dst))
  ==
::
++  lurk                                                ::  apply list patch
  |*  [hel=(list) rug=(urge)]
  ^+  hel
  =+  war=`_hel`~
  |-  ^+  hel
  ?~  rug  (flop war)
  ?-    -.i.rug
      &
    %=   $
      rug  t.rug
      hel  (slag p.i.rug hel)
      war  (weld (flop (scag p.i.rug hel)) war)
    ==
  ::
      |
    %=  $
      rug  t.rug
      hel  =+  gur=(flop p.i.rug)
           |-  ^+  hel
           ?~  gur  hel
           ?>(&(?=(^ hel) =(i.gur i.hel)) $(hel t.hel, gur t.gur))
      war  (weld q.i.rug war)
    ==
  ==
::
++  lusk                                                ::  lcs to list patch
  |*  [hel=(list) hev=(list) lcs=(list)]
  =+  ^=  rag
      ^-  $%  [& p=@ud]
              [| p=_lcs q=_lcs]
          ==
      [%& 0]
  =>  .(rag [p=rag q=*(list ,_rag)])
  =<  abet  =<  main
  |%
  ++  abet  =.(q.rag ?:(=([& 0] p.rag) q.rag [p.rag q.rag]) (flop q.rag))
  ++  done
    |=  new=_p.rag
    ^+  rag
    ?-  -.p.rag
      |   ?-  -.new
            |  [[%| (weld p.new p.p.rag) (weld q.new q.p.rag)] q.rag]
            &  [new [p.rag q.rag]]
          ==
      &   ?-  -.new
            |  [new ?:(=(0 p.p.rag) q.rag [p.rag q.rag])]
            &  [[%& (add p.p.rag p.new)] q.rag]
          ==
    ==
  ::
  ++  main
    |-  ^+  +
    ?~  hel
      ?~  hev
        ?>(?=(~ lcs) +)
      $(hev t.hev, rag (done %| ~ [i.hev ~]))
    ?~  hev
      $(hel t.hel, rag (done %| [i.hel ~] ~))
    ?~  lcs
      +(rag (done %| (flop hel) (flop hev)))
    ?:  =(i.hel i.lcs)
      ?:  =(i.hev i.lcs)
        $(lcs t.lcs, hel t.hel, hev t.hev, rag (done %& 1))
      $(hev t.hev, rag (done %| ~ [i.hev ~]))
    ?:  =(i.hev i.lcs)
      $(hel t.hel, rag (done %| [i.hel ~] ~))
    $(hel t.hel, hev t.hev, rag (done %| [i.hel ~] [i.hev ~]))
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eY, SHA-256 (move me)        ::
::
++  shad  |=(ruz=@ (shax (shax ruz)))                   ::  double sha-256
++  shaf                                                ::  half sha-256
  |=  [sal=@ ruz=@]
  =+  haz=(shas sal ruz)
  (mix (end 7 1 haz) (rsh 7 1 haz))
::
++  shak                                                ::  XX shd be PBKDF
  |=  [who=@p wud=@]
  (shas (mix %shak who) wud)
::
++  sham                                                ::  noun hash
  |=  yux=*  ^-  @uvH  ^-  @
  ?@  yux
    (shaf %mash yux)
  (shaf %sham (jam yux))
::
++  shas                                                ::  salted hash
  |=  [sal=@ ruz=@]
  (shax (mix sal (shax ruz)))
::
++  shax                                                ::  sha-256
  ~/  %shax
  |=  ruz=@  ^-  @
  ~|  %sha
  =+  [few==>(fe .(a 5)) wac=|=([a=@ b=@] (cut 5 [a 1] b))]
  =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
  =+  ral=(lsh 0 3 (met 3 ruz))
  =+  ^=  ful
      %+  can  0
      :~  [ral ruz]
          [8 128]
          [(mod (sub 960 (mod (add 8 ral) 512)) 512) 0]
          [64 (~(net fe 6) ral)]
      ==
  =+  lex=(met 9 ful)
  =+  ^=  kbx  0xc671.78f2.bef9.a3f7.a450.6ceb.90be.fffa.
                 8cc7.0208.84c8.7814.78a5.636f.748f.82ee.
                 682e.6ff3.5b9c.ca4f.4ed8.aa4a.391c.0cb3.
                 34b0.bcb5.2748.774c.1e37.6c08.19a4.c116.
                 106a.a070.f40e.3585.d699.0624.d192.e819.
                 c76c.51a3.c24b.8b70.a81a.664b.a2bf.e8a1.
                 9272.2c85.81c2.c92e.766a.0abb.650a.7354.
                 5338.0d13.4d2c.6dfc.2e1b.2138.27b7.0a85.
                 1429.2967.06ca.6351.d5a7.9147.c6e0.0bf3.
                 bf59.7fc7.b003.27c8.a831.c66d.983e.5152.
                 76f9.88da.5cb0.a9dc.4a74.84aa.2de9.2c6f.
                 240c.a1cc.0fc1.9dc6.efbe.4786.e49b.69c1.
                 c19b.f174.9bdc.06a7.80de.b1fe.72be.5d74.
                 550c.7dc3.2431.85be.1283.5b01.d807.aa98.
                 ab1c.5ed5.923f.82a4.59f1.11f1.3956.c25b.
                 e9b5.dba5.b5c0.fbcf.7137.4491.428a.2f98
  =+  ^=  hax  0x5be0.cd19.1f83.d9ab.9b05.688c.510e.527f.
                 a54f.f53a.3c6e.f372.bb67.ae85.6a09.e667
  =+  i=0
  |-  ^-  @
  ?:  =(i lex)
    (rep 5 (turn (rip 5 hax) net))
  =+  ^=  wox
      =+  dux=(cut 9 [i 1] ful)
      =+  wox=(rep 5 (turn (rip 5 dux) net))
      =+  j=16
      |-  ^-  @
      ?:  =(64 j)
        wox
      =+  :*  l=(wac (sub j 15) wox)
              m=(wac (sub j 2) wox)
              n=(wac (sub j 16) wox)
              o=(wac (sub j 7) wox)
          ==
      =+  x=:(mix (ror 7 l) (ror 18 l) (rsh 0 3 l))
      =+  y=:(mix (ror 17 m) (ror 19 m) (rsh 0 10 m))
      =+  z=:(sum n x o y)
      $(wox (con (lsh 5 j z) wox), j +(j))
  =+  j=0
  =+  :*  a=(wac 0 hax)
          b=(wac 1 hax)
          c=(wac 2 hax)
          d=(wac 3 hax)
          e=(wac 4 hax)
          f=(wac 5 hax)
          g=(wac 6 hax)
          h=(wac 7 hax)
      ==
  |-  ^-  @
  ?:  =(64 j)
    %=  ^$
      i  +(i)
      hax  %+  rep  5
           :~  (sum a (wac 0 hax))
               (sum b (wac 1 hax))
               (sum c (wac 2 hax))
               (sum d (wac 3 hax))
               (sum e (wac 4 hax))
               (sum f (wac 5 hax))
               (sum g (wac 6 hax))
               (sum h (wac 7 hax))
           ==
    ==
  =+  l=:(mix (ror 2 a) (ror 13 a) (ror 22 a))          ::  s0
  =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
  =+  n=(sum l m)                                       ::  t2
  =+  o=:(mix (ror 6 e) (ror 11 e) (ror 25 e))          ::  s1
  =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
  =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
  $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
::
++  shaw                                                ::  hash to nbits
  |=  [sal=@ len=@ ruz=@]
  (~(raw og (shas sal (mix len ruz))) len)
::
++  og                                                  ::  shax-powered rng
  ~/  %og
  |_  a=@
  ++  rad                                               ::  random in range
    |=  b=@  ^-  @
    =+  c=(raw (met 0 b))
    ?:((lth c b) c $(a +(a)))
    ::
  ++  raw                                               ::  random bits
    ~/  %raw
    |=  b=@  ^-  @
    %+  can
      0
    =+  c=(shas %og-a (mix b a))
    |-  ^-  (list ,[@ @])
    ?:  =(0 b)
      ~
    =+  d=(shas %og-b (mix b (mix a c)))
    ?:  (lth b 256)
      [[b (end 0 b d)] ~]
    [[256 d] $(c d, b (sub b 256))]
  --
++  shaz                                                ::  sha-512
  |=  ruz=@  ^-  @
  (shal [(met 3 ruz) ruz])
++  shal                                                ::  sha-512 with length
  ~/  %shal
  |=  [len=@ ruz=@]  ^-  @
  =>  .(ruz (cut 3 [0 len] ruz))
  =+  [few==>(fe .(a 6)) wac=|=([a=@ b=@] (cut 6 [a 1] b))]
  =+  [sum=sum.few ror=ror.few net=net.few inv=inv.few]
  =+  ral=(lsh 0 3 len)
  =+  ^=  ful
      %+  can  0
      :~  [ral ruz]
          [8 128]
          [(mod (sub 1.920 (mod (add 8 ral) 1.024)) 1.024) 0]
          [128 (~(net fe 7) ral)]
      ==
  =+  lex=(met 10 ful)
  =+  ^=  kbx  0x6c44.198c.4a47.5817.5fcb.6fab.3ad6.faec.
                 597f.299c.fc65.7e2a.4cc5.d4be.cb3e.42b6.
                 431d.67c4.9c10.0d4c.3c9e.be0a.15c9.bebc.
                 32ca.ab7b.40c7.2493.28db.77f5.2304.7d84.
                 1b71.0b35.131c.471b.113f.9804.bef9.0dae.
                 0a63.7dc5.a2c8.98a6.06f0.67aa.7217.6fba.
                 f57d.4f7f.ee6e.d178.eada.7dd6.cde0.eb1e.
                 d186.b8c7.21c0.c207.ca27.3ece.ea26.619c.
                 c671.78f2.e372.532b.bef9.a3f7.b2c6.7915.
                 a450.6ceb.de82.bde9.90be.fffa.2363.1e28.
                 8cc7.0208.1a64.39ec.84c8.7814.a1f0.ab72.
                 78a5.636f.4317.2f60.748f.82ee.5def.b2fc.
                 682e.6ff3.d6b2.b8a3.5b9c.ca4f.7763.e373.
                 4ed8.aa4a.e341.8acb.391c.0cb3.c5c9.5a63.
                 34b0.bcb5.e19b.48a8.2748.774c.df8e.eb99.
                 1e37.6c08.5141.ab53.19a4.c116.b8d2.d0c8.
                 106a.a070.32bb.d1b8.f40e.3585.5771.202a.
                 d699.0624.5565.a910.d192.e819.d6ef.5218.
                 c76c.51a3.0654.be30.c24b.8b70.d0f8.9791.
                 a81a.664b.bc42.3001.a2bf.e8a1.4cf1.0364.
                 9272.2c85.1482.353b.81c2.c92e.47ed.aee6.
                 766a.0abb.3c77.b2a8.650a.7354.8baf.63de.
                 5338.0d13.9d95.b3df.4d2c.6dfc.5ac4.2aed.
                 2e1b.2138.5c26.c926.27b7.0a85.46d2.2ffc.
                 1429.2967.0a0e.6e70.06ca.6351.e003.826f.
                 d5a7.9147.930a.a725.c6e0.0bf3.3da8.8fc2.
                 bf59.7fc7.beef.0ee4.b003.27c8.98fb.213f.
                 a831.c66d.2db4.3210.983e.5152.ee66.dfab.
                 76f9.88da.8311.53b5.5cb0.a9dc.bd41.fbd4.
                 4a74.84aa.6ea6.e483.2de9.2c6f.592b.0275.
                 240c.a1cc.77ac.9c65.0fc1.9dc6.8b8c.d5b5.
                 efbe.4786.384f.25e3.e49b.69c1.9ef1.4ad2.
                 c19b.f174.cf69.2694.9bdc.06a7.25c7.1235.
                 80de.b1fe.3b16.96b1.72be.5d74.f27b.896f.
                 550c.7dc3.d5ff.b4e2.2431.85be.4ee4.b28c.
                 1283.5b01.4570.6fbe.d807.aa98.a303.0242.
                 ab1c.5ed5.da6d.8118.923f.82a4.af19.4f9b.
                 59f1.11f1.b605.d019.3956.c25b.f348.b538.
                 e9b5.dba5.8189.dbbc.b5c0.fbcf.ec4d.3b2f.
                 7137.4491.23ef.65cd.428a.2f98.d728.ae22
  =+  ^=  hax  0x5be0.cd19.137e.2179.1f83.d9ab.fb41.bd6b.
                 9b05.688c.2b3e.6c1f.510e.527f.ade6.82d1.
                 a54f.f53a.5f1d.36f1.3c6e.f372.fe94.f82b.
                 bb67.ae85.84ca.a73b.6a09.e667.f3bc.c908
  =+  i=0
  |-  ^-  @
  ?:  =(i lex)
    (rep 6 (turn (rip 6 hax) net))
  =+  ^=  wox
      =+  dux=(cut 10 [i 1] ful)
      =+  wox=(rep 6 (turn (rip 6 dux) net))
      =+  j=16
      |-  ^-  @
      ?:  =(80 j)
        wox
      =+  :*  l=(wac (sub j 15) wox)
              m=(wac (sub j 2) wox)
              n=(wac (sub j 16) wox)
              o=(wac (sub j 7) wox)
          ==
      =+  x=:(mix (ror 1 l) (ror 8 l) (rsh 0 7 l))
      =+  y=:(mix (ror 19 m) (ror 61 m) (rsh 0 6 m))
      =+  z=:(sum n x o y)
      $(wox (con (lsh 6 j z) wox), j +(j))
  =+  j=0
  =+  :*  a=(wac 0 hax)
          b=(wac 1 hax)
          c=(wac 2 hax)
          d=(wac 3 hax)
          e=(wac 4 hax)
          f=(wac 5 hax)
          g=(wac 6 hax)
          h=(wac 7 hax)
      ==
  |-  ^-  @
  ?:  =(80 j)
    %=  ^$
      i  +(i)
      hax  %+  rep  6
           :~  (sum a (wac 0 hax))
               (sum b (wac 1 hax))
               (sum c (wac 2 hax))
               (sum d (wac 3 hax))
               (sum e (wac 4 hax))
               (sum f (wac 5 hax))
               (sum g (wac 6 hax))
               (sum h (wac 7 hax))
           ==
    ==
  =+  l=:(mix (ror 28 a) (ror 34 a) (ror 39 a))         ::  S0
  =+  m=:(mix (dis a b) (dis a c) (dis b c))            ::  maj
  =+  n=(sum l m)                                       ::  t2
  =+  o=:(mix (ror 14 e) (ror 18 e) (ror 41 e))         ::  S1
  =+  p=(mix (dis e f) (dis (inv e) g))                 ::  ch
  =+  q=:(sum h o p (wac j kbx) (wac j wox))            ::  t1
  $(j +(j), a (sum q n), b a, c b, d c, e (sum d q), f e, g f, h g)
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2eZ, OLD rendering (kill me)  ::
::
++  show                            ::  XX deprecated, use type
  |=  vem=*
  |^  ^-  tank
      ?:  ?=(@ vem)
        [%leaf (mesc (trip vem))]
      ?-    vem
          [s=~ c=*]
        [%leaf '\'' (weld (mesc (tape +.vem)) `tape`['\'' ~])]
      ::
          [s=%a c=@]        [%leaf (mesc (trip c.vem))]
          [s=%b c=*]        (shop c.vem |=(a=@ ~(rub at a)))
          [s=[%c p=@] c=*]
        :+  %palm
          [['.' ~] ['-' ~] ~ ~]
        [[%leaf (mesc (trip p.s.vem))] $(vem c.vem) ~]
      ::
          [s=%d c=*]        (shop c.vem |=(a=@ ~(rud at a)))
          [s=%k c=*]        (tank c.vem)
          [s=%h c=*]
        ?:  =(0 c.vem)      ::  XX remove after 220
          [%leaf '#' ~]
        :+  %rose
          [['/' ~] ['/' ~] ~]
        =+  yol=`(list ,@ta)`[(,@ta -.c.vem) (flop ((list ,@ta) +.c.vem))]
        (turn yol |=(a=@ta [%leaf (trip a)]))
      ::
          [s=%o c=*]
        %=    $
            vem
          :-  [%m '%h:<[%d %d].[%d %d]>']
          [-.c.vem +<-.c.vem +<+.c.vem +>-.c.vem +>+.c.vem ~]
        ==
      ::
          [s=%p c=*]        (shop c.vem |=(a=@ ~(rup at a)))
          [s=%q c=*]        (shop c.vem |=(a=@ ~(r at a)))
          [s=%r c=*]        $(vem [[%r ' ' '{' '}'] c.vem])
          [s=%t c=*]        (shop c.vem |=(a=@ ~(rt at a)))
          [s=%v c=*]        (shop c.vem |=(a=@ ~(ruv at a)))
          [s=%x c=*]        (shop c.vem |=(a=@ ~(rux at a)))
          [s=[%m p=@] c=*]  (shep p.s.vem c.vem)
          [s=[%r p=@] c=*]
        $(vem [[%r ' ' (cut 3 [0 1] p.s.vem) (cut 3 [1 1] p.s.vem)] c.vem])
      ::
          [s=[%r p=@ q=@ r=@] c=*]
        :+  %rose
          :*  p=(mesc (trip p.s.vem))
              q=(mesc (trip q.s.vem))
              r=(mesc (trip r.s.vem))
          ==
        |-  ^-  (list tank)
        ?@  c.vem
          ~
        [^$(vem -.c.vem) $(c.vem +.c.vem)]
      ::
          [s=%z c=*]        $(vem [[%r %$ %$ %$] c.vem])
          *                 !!
      ==
  ++  shep
    |=  [fom=@ gar=*]
    ^-  tank
    =+  l=(met 3 fom)
    =+  i=0
    :-  %leaf
    |-  ^-  tape
    ?:  (gte i l)
      ~
    =+  c=(cut 3 [i 1] fom)
    ?.  =(37 c)
      (weld (mesc [c ~]) $(i +(i)))
    =+  d=(cut 3 [+(i) 1] fom)
    ?.  .?(gar)
      ['\\' '#' $(i (add 2 i))]
    (weld ~(ram re (show d -.gar)) $(i (add 2 i), gar +.gar))
  ::
  ++  shop
    |=  [aug=* vel=$+(a=@ tape)]
    ^-  tank
    ?:  ?=(@ aug)
      [%leaf (vel aug)]
    :+  %rose
      [[' ' ~] ['[' ~] [']' ~]]
    =>  .(aug `*`aug)
    |-  ^-  (list tank)
    ?:  ?=(@ aug)
      [^$ ~]
    [^$(aug -.aug) $(aug +.aug)]
  --
++  at
  |_  a=@
  ++  r
    ?:  ?&  (gte (met 3 a) 2)
            |-
            ?:  =(0 a)
              &
            =+  vis=(end 3 1 a)
            ?&  ?|(=('-' vis) ?&((gte vis 'a') (lte vis 'z')))
                $(a (rsh 3 1 a))
            ==
        ==
      rtam
    ?:  (lte (met 3 a) 2)
      rud
    rux
  ::
  ++  rf    `tape`[?-(a & '&', | '|', * !!) ~]
  ++  rn    `tape`[?>(=(0 a) '~') ~]
  ++  rt    `tape`['\'' (weld (mesc (trip a)) `tape`['\'' ~])]
  ++  rta   rt
  ++  rtam  `tape`['%' (trip a)]
  ++  rub   `tape`['0' 'b' (rum 2 ~ |=(b=@ (add '0' b)))]
  ++  rud   (rum 10 ~ |=(b=@ (add '0' b)))
  ++  rum
    |=  [b=@ c=tape d=$+(@ @)]
    ^-  tape
    ?:  =(0 a)
      [(d 0) c]
    =+  e=0
    |-  ^-  tape
    ?:  =(0 a)
      c
    =+  f=&(!=(0 e) =(0 (mod e ?:(=(10 b) 3 4))))
    %=  $
      a  (div a b)
      c  [(d (mod a b)) ?:(f [?:(=(10 b) ',' '-') c] c)]
      e  +(e)
    ==
  ::
  ++  rup
    =+  b=(met 3 a)
    ^-  tape
    :-  '-'
    |-  ^-  tape
    ?:  (gth (met 5 a) 1)
      %+  weld
        $(a (rsh 5 1 a), b (sub b 4))
      `tape`['-' '-' $(a (end 5 1 a), b 4)]
    ?:  =(0 b)
      ['~' ~]
    ?:  (lte b 1)
      (trip (tos:po a))
    |-  ^-  tape
    ?:  =(2 b)
      =+  c=(rsh 3 1 a)
      =+  d=(end 3 1 a)
      (weld (trip (tod:po c)) (trip (tos:po (mix c d))))
    =+  c=(rsh 3 2 a)
    =+  d=(end 3 2 a)
    (weld ^$(a c, b (met 3 c)) `tape`['-' $(a (mix c d), b 2)])
  ::
  ++  ruv
    ^-  tape
    :+  '0'
      'v'
    %^    rum
        64
      ~
    |=  b=@
    ?:  =(63 b)
      '+'
    ?:  =(62 b)
      '-'
    ?:((lth b 26) (add 65 b) ?:((lth b 52) (add 71 b) (sub b 4)))
  ::
  ++  rux  `tape`['0' 'x' (rum 16 ~ |=(b=@ (add b ?:((lth b 10) 48 87))))]
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  ::
::::              chapter 2f, Hoon proper               ::::
::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fA, miscellaneous funs       ::
::                                                      ::
++  bull
  |=  [bid=twin der=type]
  ^-  type
  ?:(|(=(%void der) =(%void s.bid)) %void [%bull bid der])
::
++  cain  |=(vax=vase (sell vax))
++  cell
  ~/  %cell
  |=  [hed=type tal=type]
  ^-  type
  ?:(=(%void hed) %void ?:(=(%void tal) %void [%cell hed tal]))
::
++  core
  ~/  %core
  |=  [pac=type con=coil]
  ^-  type
  ?:(=(%void pac) %void [%core pac con])
::
++  cube
  ~/  %cube
  |=  [dil=* goq=type]
  ^-  type
  ?:  =(%void goq)
    %void
  [%cube dil goq]
::
++  face
  ~/  %face
  |=  [cog=term der=type]
  ^-  type
  ?:  =(%void der)
    %void
  [%face cog der]
::
++  bean  ^-(type [%fork [%cube 0 %atom %f] [%cube 1 %atom %f]])
++  flay
  ~/  %flay
  |=  pok=port
  ^-  [p=axis q=type]
  :-  p.pok
  ?-  -.q.pok
    &  p.q.pok
    |  (roll q.q.pok =+([p=[p=*type q=*foot] q=`type`%void] |.((fork p.p q))))
  ==
::
++  flee
  |=  poy=post
  ^-  port
  ?-  -.q.poy
    0  [p.poy %& p.q.poy]
    1  [p.poy %| p.q.poy q.q.poy]
    2  [(peg p.poy r.p.q.poy) %& s.p.q.poy]
  ==
::
++  foil
  ~/  %foil
  |=  pok=port
  ^-  prop
  ?-  -.q.pok
    &  [p.pok [~ [[p.q.pok [%elm ~ 1]] ~]]]
    |  [p.pok [p.q.pok q.q.pok]]
  ==
::
++  fork
  ~/  %fork
  |=  [hoz=type bur=type]
  ^-  type
  ?:  =(hoz bur)
    hoz
  ?:  =(%void hoz)
    bur
  ?:  =(%void bur)
    hoz
  [%fork hoz bur]
::
++  cove
  |=  nug=nock
  ?-    nug
      [0 *]   p.nug
      [10 *]  $(nug q.nug)
      *       ~|([%cove nug] !!)
  ==
++  comb
  ~/  %comb
  |=  [mal=nock buz=nock]
  ^-  nock
  ?:  ?&(?=([0 *] mal) !=(0 p.mal))
    ?:  ?&(?=([0 *] buz) !=(0 p.buz))
      [%0 (peg p.mal p.buz)]
    ?:  ?=([2 [0 *] [0 *]] buz)
      [%2 [%0 (peg p.mal p.p.buz)] [%0 (peg p.mal p.q.buz)]]
    [%7 mal buz]
  ?:  ?=([^ [0 1]] mal)
    [%8 p.mal buz]
  ?:  =([0 1] buz)
    mal
  [%7 mal buz]
::
++  cond
  ~/  %cond
  |=  [pex=nock yom=nock woq=nock]
  ^-  nock
  ?-  pex
    [1 0]  yom
    [1 1]  woq
    *      [%6 pex yom woq]
  ==
::
++  cons
  ~/  %cons
  |=  [vur=nock sed=nock]
  ^-  nock
  ?:  ?=([[0 *] [0 *]] +<)
    ?:  ?&(=(+(p.vur) p.sed) =((div p.vur 2) (div p.sed 2)))
      [%0 (div p.vur 2)]
    [vur sed]
  ?:  ?=([[1 *] [1 *]] +<)
    [%1 p.vur p.sed]
  [vur sed]
::
++  fitz
  ~/  %fitz
  |=  [yaz=term wix=term]
  =+  ^=  fiz
      |=  mot=@ta  ^-  [p=@ q=@ta]
      =+  len=(met 3 mot)
      ?:  =(0 len)
        [0 %$]
      =+  tyl=(rsh 3 (dec len) mot)
      ?:  &((gte tyl 'A') (lte tyl 'Z'))
        [(sub tyl 64) (end 3 (dec len) mot)]
      [0 mot]
  =+  [yoz=(fiz yaz) wux=(fiz wix)]
  ?&  ?|  =(0 p.yoz)
          =(0 p.wux)
          &(!=(0 p.wux) (lte p.wux p.yoz))
      ==
      |-  ?|  =(%$ p.yoz)
              =(%$ p.wux)
              ?&  =((end 3 1 p.yoz) (end 3 1 p.wux))
                  $(p.yoz (rsh 3 1 p.yoz), p.wux (rsh 3 1 p.wux))
              ==
          ==
  ==
::
++  flan
  ~/  %flan
  |=  [bos=nock nif=nock]
  ^-  nock
  ?-    bos
      [1 1]   bos
      [1 0]   nif
      *
    ?-    nif
        [1 1]   nif
        [1 0]   bos
        *       [%6 bos nif [%1 1]]
    ==
  ==
::
++  flip
  ~/  %flip
  |=  [dyr=nock]
  [%6 dyr [%1 1] [%1 0]]
::
++  flor
  ~/  %flor
  |=  [bos=nock nif=nock]
  ^-  nock
  ?-  bos
      [1 1]   nif
      [1 0]   bos
      *
    ?-  nif
        [1 1]   bos
        [1 0]   nif
        *       [%6 bos [%1 0] nif]
    ==
  ==
::
++  hike
  ~/  %hike
  |=  [axe=axis pac=(list ,[p=axis q=nock])]
  ^-  nock
  ?~  pac
    [%0 axe]
  =+  zet=(skim pac.$ |=([p=axis q=nock] [=(1 p)]))
  ?~  zet
    =+  tum=(skim pac.$ |=([p=axis q=nock] ?&(!=(1 p) =(2 (cap p)))))
    =+  gam=(skim pac.$ |=([p=axis q=nock] ?&(!=(1 p) =(3 (cap p)))))
    %+  cons
      %=  $
        axe  (peg axe 2)
        pac  (turn tum |=([p=axis q=nock] [(mas p) q]))
      ==
    %=  $
      axe  (peg axe 3)
      pac  (turn gam |=([p=axis q=nock] [(mas p) q]))
    ==
  ?>(?=([* ~] zet) q.i.zet)
::
++  hoax
  |=  a=@ta
  ?>  =(%ho (end 3 2 a))
  %+  add
    (mod (add 13 (sub (cut 3 [3 1] a) 'a')) 26)
  %+  mul  26
  =+  b=(cut 3 [2 1] a)
  ?+(b !! %o 0, %i 1, %u 2, %e 3, %a 4, %y 5, %w 6, %l 7)
::
++  hoof
  |=  a=@  ^-  @ta
  (rap 3 'h' 'o' (snag (div a 26) "oiueaywl") (add 'a' (mod (add a 13) 26)) ~)
::
++  jock
  |=  rad=?
  |=  lot=coin  ^-  twig
  ?-    -.lot
      ~      ?:(rad [%dtzz p.lot] [%dtzy p.lot])
  ::
      %blob
    ?:  rad
      [%dtzz %$ p.lot]
    ?@(p.lot [%dtzy %$ p.lot] [$(p.lot -.p.lot) $(p.lot +.p.lot)])
  ::
      %many
    |-(^-(twig ?~(p.lot [%bczp %null] [^$(lot i.p.lot) $(p.lot t.p.lot)])))
  ==
::
++  look
  ~/  %look
  |=  [cog=term dab=(map term foot)]
  =+  axe=1
  |-
  ^-  (unit ,[p=axis q=foot])
  ?-  dab
      ~         ~
      [* ~ ~]
    ?:(=(cog p.n.dab) [~ axe q.n.dab] ~)
  ::
      [* ~ *]
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      ~
    $(axe (peg axe 3), dab r.dab)
  ::
      [* * ~]
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      $(axe (peg axe 3), dab l.dab)
    ~
  ::
      [* * *]
    ?:  =(cog p.n.dab)
      [~ (peg axe 2) q.n.dab]
    ?:  (gor cog p.n.dab)
      $(axe (peg axe 6), dab l.dab)
    $(axe (peg axe 7), dab r.dab)
  ==
::
++  make
  |=  txt=@
  q:(~(mint ut %noun) %noun (ream txt))
::
++  noah  |=(vax=vase (pave vax))
++  onan  |=(vix=vise (seer vix))
++  rain
  |=  [bon=path txt=@]
  =+  vaz=vast
  (scan (trip txt) (full (ifix [gay gay] tall:vaz(wer bon))))
::
++  ream
  |=  txt=@
  ^-  twig
  (rash txt vest)
::
++  reck
  |=  bon=path
  (rain bon ((hard ,@t) .^(%cx (weld bon `path`[%hoon ~]))))
::
++  seed
  ^-  vase
  ~+
  !;(*type ..seed)
::
++  seem  |=(toy=typo `type`toy)
++  seer  |=(vix=vise `vase`vix)
++  sell
  |=  vax=vase  ^-  tank
  ~|  %sell
  (dish:ut ~(dole ut p.vax) q.vax)
::
++  pave
  |=  vax=vase  ^-  tape
  ~(ram re (sell vax))
::
++  loot
  |=  vax=vase  ^-  @ta
  (rap 3 (pave vax))
::
++  slam
  |=  [gat=vase sam=vase]  ^-  vase
  =+  :-  ^=  typ  ^-  type
          [%cell p.gat p.sam]
      ^=  gen  ^-  twig
      [%cncl [~ 2] [~ 3]]
  =+  gun=(~(mint ut typ) %noun gen)
  [p.gun .*([q.gat q.sam] q.gun)]
::
++  slim
  |=  old=vise  ^-  vase
  old
::
++  slit
  |=  [gat=type sam=type]
  (~(play ut [%cell gat sam]) [%cncl [~ 2] [~ 3]])
::
++  slap
  |=  [vax=vase gen=twig]  ^-  vase
  =+  gun=(~(mint ut p.vax) %noun gen)
  [p.gun .*(q.vax q.gun)]
::
++  slop
  |=  [hed=vase tal=vase]
  ^-  vase
  [[%cell p.hed p.tal] [q.hed q.tal]]
::
++  skol
  |=  typ=type  ^-  tank
  ~(duck ut typ)
::
++  spat  |=(pax=path (rap 3 (spud pax)))
++  spud  |=(pax=path ~(ram re (dish:ut [~ %path] pax)))
++  slot
  |=  [axe=@ vax=vase]  ^-  vase
  (slap vax [~ axe])
::
++  slum
  |=  [vax=vase wad=(map term vase)]  ^-  vase
  ?-  wad
    ~        vax
    [* ~ ~]  [[%cell p.vax [%face p.n.wad p.q.n.wad]] [q.vax q.q.n.wad]]
    [* ~ *]  $(wad [n.wad ~ ~], vax $(wad r.wad))
    [* * ~]  $(wad [n.wad ~ ~], vax $(wad l.wad))
    [* * *]  $(wad [n.wad ~ r.wad], vax $(wad l.wad))
  ==
::
++  stab
  |=  zep=@ta  ^-  path
  (need (rush zep ;~(pfix fas ;~(sfix (more fas urs:ab) fas))))
::
++  wash
  |=  [[tab=@ edg=@] tac=tank]  ^-  wall
  (~(win re tac) tab edg)
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fB, macro expansion          ::
::
++  ah
  |_  tig=tiki
  ++  blue
    |=  gen=twig 
    ^-  twig
    ?.  &(?=(| -.tig) ?=(~ p.tig))  gen
    [%tsgr [~ 3] gen]
  ::
  ++  gray
    |=  gen=twig
    ^-  twig
    ?-  -.tig
      &  ?~(p.tig gen [%tstr u.p.tig q.tig gen])
      |  [%tsls ?~(p.tig q.tig [%ktts u.p.tig q.tig]) gen]
    ==
  ::
  ++  puce
    ^-  wing
    ?-  -.tig
      &  ?~(p.tig q.tig [u.p.tig ~])
      |  [[%& 2] ~]
    ==
  --
::
++  al
  ~%    %al
      +>+
    ==
      %bunt  bunt
      %whip  whip
    ==
  =+  [nag=`*`& gom=`axis`1]
  |_  sec=tile
  ::::
  ++  blah  ^~  [%dtzz %$ 0]
  ++  home  |=(gen=twig ^-(twig ?:(=(1 gom) gen [%tsgr [~ gom] gen])))
  ::::
  ++  bunt
    |-  ^-  twig
    ?-    sec
        [^ *]
      [$(sec p.sec) $(sec q.sec)]
    ::
        [%axil *]
      ?-  p.sec
        [%atom *]  [%dtzy p.p.sec 0]
        %noun      [%dttr [%dtzz %$ 0] [[%dtzz %$ 0] [%dtzz %$ 1]]]
        %cell      =+(nec=$(sec [%axil %noun]) [nec nec])
        %bean      [%dtts [%dtzz %$ 0] [%dtzz %$ 0]]
        %null      [%dtzz %n %$]
      ==
    ::
        [%bark *]
      [%ktts p.sec $(sec q.sec)]
    ::
        [%bush *]
      [%wtcl [%bczp %bean] $(sec p.sec) $(sec q.sec)]
    ::
        [%fern *]
      |-  ^-  twig
      ?@  t.p.sec
        ^$(sec i.p.sec)
      [%wtcl [%bczp %bean] ^$(sec i.p.sec) $(p.sec t.p.sec)]
    ::
        [%herb *]
      (home [%tsgl [%cnzy %$] p.sec])
    ::
        [%kelp *]
      |-  ^-  twig
      ?@  t.p.sec
        ^$(sec i.p.sec)
      [%wtcl [%bczp %bean] ^$(sec i.p.sec) $(p.sec t.p.sec)]
    ::
        [%leaf *]
      [%dtzz p.sec q.sec]
    ::
        [%reed *]
      [%wtcl [%bczp %bean] $(sec p.sec) $(sec q.sec)]
    ::
        [%weed *]
      (home p.sec)
    ==
  ++  clam  ^-(twig [%brts [%axil %noun] (whip(gom 7) 6)])
  ++  cloq
    |-  ^-  [p=toga q=tile]
    ?:  ?=(^ -.sec)
      =+  [one=$(sec p.sec) two=$(sec q.sec)]
      [[%2 p.one p.two] [q.one q.two]]
    ?.  ?=(%bark -.sec)  [[%0 ~] sec]
    =+  got=$(sec q.sec)
    :_  q.got
    ?:(?=([%0 ~] p.got) p.sec [%1 p.sec p.got])
  ::
  ++  whip
    |=  axe=axis
    =+  ^=  tun
        |=  noy=$+(* twig)
        ^-  twig
        ?@  nag
          =+  luz=[%cnts [[~ 1] ~] [[[%& axe] ~] bunt(sec [%axil %cell])] ~]
          ?:  =(& nag)
            [%tsgr [%wtpt [[%& axe] ~] luz [~ 1]] (noy [& &])]
          [%tsgr luz (noy [& &])]
        (noy nag)
    ^-  twig
    ?-    sec
        [^ *]
      %-  tun  |=  gon=*  =>  .(nag gon)  ^-  twig
      :-  ^$(sec -.sec, nag -.nag, axe (peg axe 2))
      ^$(sec +.sec, nag +.nag, axe (peg axe 3))
    ::
        [%axil *]
      ?-    p.sec
          [%atom *]
        =+  buv=bunt
        |-  ^-  twig
        ?@  nag
          ?:(=(& nag) [%wtpt [[%& axe] ~] $(nag |) buv] [%ktls buv [~ axe]])
        buv
      ::
          %noun
        [%kthp [%axil %noun] [~ axe]]
      ::
          %cell
        =+  buv=bunt
        |-  ^-  twig
        ?@  nag
          ?:(=(& nag) [%wtpt [[%& axe] ~] buv $(nag [& &])] buv)
        [%ktls buv [~ axe]]
      ::
          %bean
        :^    %wtcl
            [%dtts [%dtzz %$ |] [~ axe]]
          [%dtzz %f |]
        [%dtzz %f &]
      ::
          %null
        bunt
      ==
    ::
        [%bark *]
      [%ktts p.sec $(sec q.sec)]
    ::
        [%bush *]
      %-  tun  |=  gon=*  =>  .(nag gon)  ^-  twig
      ?@  -.nag
        ?:  =(& -.nag)
          [%wtpt [[%& (peg axe 2)] ~] ^$(sec q.sec) ^$(sec p.sec)]
        ^$(sec q.sec)
      ^$(sec p.sec)
    ::
        [%fern *]
      |-  ^-  twig
      ?@  t.p.sec
        ^$(sec i.p.sec)
      :+  %tsls
        ^$(sec i.p.sec)
      =>  .(axe (peg 3 axe), gom (peg 3 gom))
      :^    %wtcl
          [%dtts [~ axe] [~ 2]]
        [~ 2]
      $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec)
    ::
        [%herb *]
      [%cnhp (home p.sec) [~ axe] ~]
    ::
        [%kelp *]
      %-  tun  |=  gon=*  =>  .(nag gon)
      |-  ^-  twig
      ?@  t.p.sec
        :-  [%dtzz +.p.i.p.sec]
        ^^$(axe (peg axe 3), sec q.i.p.sec, nag &)
      :^    %wtcl
          [%dtts [~ (peg axe 2)] [%dtzz +.p.i.p.sec]]
        :-  [%dtzz +.p.i.p.sec]
        ^^$(axe (peg axe 3), sec q.i.p.sec, nag &)
      $(i.p.sec i.t.p.sec, t.p.sec t.t.p.sec)
    ::
        [%leaf *]
      [%dtzz p.sec q.sec]
    ::
        [%reed *]
      ?-  nag
        &  [%wtpt [[%& axe] ~] $(sec p.sec, nag |) $(sec q.sec, nag [& &])]
        |  $(sec p.sec)
        ^  $(sec q.sec)
        *  !!
      ==
    ::
        [%weed *]
      (home p.sec)
    ==
  --
::
++  ap
  ~%    %ap
      +>
    ==
      %etch  etch
      %open  open
      %rake  rake
    ==
  |_  gen=twig
  ++  etch
    ~|  %etch
    |-  ^-  term
    ?:  ?=([%ktts *] gen)
      ?>(?=(@ p.gen) p.gen)
    =+  voq=~(open ap gen)
    ?<(=(gen voq) $(gen voq))
  ::
  ++  feck
    |-  ^-  (unit term)
    ?-  gen
      [%dtzy %tas *]  [~ q.gen]
      [%dtzz %tas @]  [~ q.gen]
      [%zpcb *]       $(gen q.gen)
      *               ~
    ==
  ::
  ++  hock
    |-  ^-  toga
    ?-  gen
      [^ *]            [%2 $(gen p.gen) $(gen q.gen)]
      [%cnts [@ ~] ~]  i.p.gen
      [%cnzy @]        p.gen
      [%cnzz [@ ~]]    i.p.gen
      [%zpcb *]        $(gen q.gen)
      *                =+(neg=open ?:(=(gen neg) [%0 ~] $(gen neg)))
    ==
  ::
  ++  jone
    ^-  (list twig)
    ?:  ?=([%clzz *] gen)
      p.gen
    ?:  ?=([%zpcb * [%clzz *]] gen)
      p.q.gen
    [gen ~]
  ::
  ++  open
    ^-  twig
    ?-    gen
        [~ *]      [%cnts [gen ~] ~]
        [%bccm *]  ~(clam al p.gen)
        [%bccb *]  ~(bunt al p.gen)
        [%bctr *]  [%ktsg ~(bunt al p.gen)]
        [%bczp *]  [%bccb %axil p.gen]
        [%brcb *]  [%tsls [%bctr p.gen] [%brcn q.gen]]
        [%brdt *]  [%brcn (~(put by *(map term foot)) %$ [%ash p.gen])]
        [%brtr *]  ~|  %elm-tile
                   =+  lyg=~(cloq al p.gen)
                   :+  %brcb  q.lyg
                   %+  ~(put by *(map term foot))  %$
                   :-  %elm
                   :+  %tsgl  q.gen
                   :+  %cnts  ~
                   :~  [[[%& 6] ~] [%ktts p.lyg [~ 6]]]
                   ==
        [%brfs *]  ~|  %elm-tile
                   =+  lyg=~(cloq al p.gen)
                   :+  %brcb  q.lyg
                   %-  ~(run by q.gen)
                   |=  a=foot  ^-  foot
                   ?.  ?=(%elm -.a)  a
                   :-  -.a
                   :+  %tsgl  p.a
                   :+  %cnts  ~
                   :~  [[[%& 6] ~] [%ktts p.lyg [~ 6]]]
                   ==
        [%brkt *]  [%tsgr [%brcn (~(put by q.gen) %$ [%ash p.gen])] [%cnzy %$]]
        [%brls *]  [%ktbr [%brts p.gen q.gen]]
        [%brhp *]  [%tsgl [%cnzy %$] [%brdt p.gen]]
        [%brts *]  [%brcb p.gen (~(put by *(map term foot)) %$ [%ash q.gen])]
        [%brwt *]  [%ktwt %brdt p.gen]
        [%clkt *]  [p.gen q.gen r.gen s.gen]
        [%clfs *]  =+(zoy=[%dtzz %ta %$] [%clsg [zoy [%clsg [zoy p.gen] ~]] ~])
        [%clls *]  [p.gen q.gen r.gen]
        [%clcb *]  [q.gen p.gen]
        [%clcn *]  [[%clsg p.gen] [%bczp %null]]
        [%clhp *]  [p.gen q.gen]
        [%clsg *]
      |-  ^-  twig
      ?~  p.gen
        [%dtzz %n ~]
      =+  mow=jone(gen i.p.gen)
      ?:  =(mow [i.p.gen ~])
        [i.p.gen $(p.gen t.p.gen)]
      $(p.gen (weld mow t.p.gen))
    ::
        [%cltr *]
      |-  ^-  twig
      ?~  p.gen
        [%zpzp ~]
      =+  mow=jone(gen i.p.gen)
      ?:  =(mow [i.p.gen ~])
        ?~  t.p.gen
          i.p.gen
        [i.p.gen $(p.gen t.p.gen)]
      $(p.gen (weld mow t.p.gen))
    ::
        [%clzz *]  open(gen [%clsg p.gen])
        [%cncb *]  [%ktls [%cnzz p.gen] %cnts p.gen q.gen]
        [%cncl *]  [%cnsg [%$ ~] p.gen q.gen]
        [%cndt *]  [%cnhp q.gen [p.gen ~]]
        [%cnkt *]  [%cnhp p.gen q.gen r.gen s.gen ~]
        [%cnls *]  [%cnhp p.gen q.gen r.gen ~]
        [%cnhp *]
      ?~(q.gen [%tsgr p.gen [%cnzy %$]] [%cncl p.gen [%cltr q.gen]])
    ::
        [%cnsg *]  [%cntr p.gen q.gen [[[[%& 6] ~] r.gen] ~]]
        [%cntr *]
      :+  %tsls
        q.gen
      :+  %cnts
        (weld p.gen `wing`[[~ 2] ~])
      (turn r.gen |=([p=wing q=twig] [p [%tsgr [~ 3] q]]))
    ::
        [%cnzy *]  [%cnts [p.gen ~] ~]
        [%cnzz *]  [%cnts p.gen ~]
        [%hxgl *]  [%cnhp [%cnzy %noah] [%zpgr [%cltr p.gen]] ~]
        [%hxgr *]  [%cnhp [%cnzy %cain] [%zpgr [%cltr p.gen]] ~]
    ::
        [%ktdt *]  [%ktls [%cnhp p.gen q.gen ~] q.gen]
        [%kthp *]  [%ktls ~(bunt al p.gen) q.gen]
        [%sgbr *]  
      :+  %sggr
        :-  %mean
        =+  fek=~(feck ap p.gen)
        ?^  fek  [%dtzz %tas u.fek]
        [%brdt [%cnhp [%cnzy %cain] [%zpgr p.gen] ~]]
      q.gen 
    ::
        [%sgcb *]  [%sggr [%mean [%brdt p.gen]] q.gen]
        [%sgcn *]
      :+  %sggl
        :-  %fast
        :-  %clls
        :+  [%dtzz %$ p.gen]
          [%zpts q.gen]
        :-  %clsg
        =+  nob=`(list twig)`~
        |-  ^-  (list twig)
        ?@  r.gen
          nob
        [[[%dtzz %$ p.i.r.gen] [%zpts q.i.r.gen]] $(r.gen t.r.gen)]
      s.gen
    ::
        [%sgfs *]  [%sgcn p.gen [~ 7] ~ q.gen]
        [%sggl *]  [%tsgl [%sggr p.gen [~ 1]] q.gen]
        [%sgbc *]  [%sggr [%live [%dtzz %$ p.gen]] q.gen]
        [%sgls *]  [%sggr [%memo %dtzz %$ p.gen] q.gen]
        [%sgpm *]
      :+  %sggr
        [%slog [%dtzy %$ p.gen] [%cnhp [%cnzy %cain] [%zpgr q.gen] ~]]
      r.gen
    ::
        [%sgts *]  [%sggr [%germ p.gen] q.gen]
        [%sgwt *]  [%tsgl s.gen %wtdt q.gen [~ 1] %sgpm p.gen r.gen [~ 1]]
        [%sgzp *]  [%sggr [%mean [%brdt p.gen]] q.gen]
        [%smcl *]
      ?-    q.gen
          ~       [%zpzp ~]
          [* ~]   i.q.gen
          ^
        :+  %tsls
          p.gen
        =+  yex=`(list twig)`q.gen
        |-  ^-  twig
        ?-  yex
          [* ~]  [%tsgr [~ 3] i.yex]
          [* ^]  [%cnhp [~ 2] [%tsgr [~ 3] i.yex] $(yex t.yex) ~]
          ~      !!
        ==
      ==
    ::
        [%smdq *]                                       ::                  ;"
      :+  %tsgr  [%ktts %v ~ 1]                         ::  =>  v=.
      :-  %brhp                                         ::  |-
      :+  %ktls                                         ::  ^+
        :-  %brhp                                       ::  |-
        :^    %wtcl                                     ::  ?:
            [%bczp %bean]                               ::  ?
          [%bczp %null]                                 ::  ~
        :-  [%ktts %i [%dtzy 'tD' _@]]                  ::  :-  i=~~
        [%ktts %t [%cnzy %$]]                           ::  t=$
      |-  ^-  twig                                      ::
      ?~  p.gen                                         ::
        [%bczp %null]                                   ::  ~
      =+  res=$(p.gen t.p.gen)                          ::
      ^-  twig                                          ::
      ?@  i.p.gen                                       ::
        [[%dtzy 'tD' i.p.gen] res]                      ::  [~~{i.p.gen} {res}]
      :+  %tsls                                         ::
        :-  :+  %ktts                                   ::  ^=
              %a                                        ::  a
            :+  %ktls                                   ::  ^+
              [%cnzy %$]                                ::  $
            [%tsgr [%cnzy %v] p.i.p.gen]                ::  =>(v {p.i.p.gen})
        [%ktts %b res]                                  ::  b={res}
      ^-  twig                                          ::
      :-  %brhp                                         ::  |-
      :^    %wtpt                                       ::  ?@
          [%a ~]                                        ::  a
        [%cnzy %b]                                      ::  b
      :-  [%tsgl [~ 2] [%cnzy %a]]                      ::  :-  -.a
      :+  %cnts                                         ::  %=
        [%$ ~]                                          ::  $
      [[[%a ~] [%tsgl [~ 3] [%cnzy %a]]] ~]         ::  a  +.a
    ::
        [%smdt *]                                       ::                  ;.
      :+  %tsgr  [%ktts %v ~ 1]                         ::  =>  v=.
      :+  %tsls  [%ktts %a [%tsgr [%cnzy %v] p.gen]]    ::  =+  a==>(v {p.gen})
      |-  ^-  twig                                      ::
      ?~  q.gen                                         ::
        [%cnzy %a]                                      ::  a
      :^    %wtsg  [%a ~]                               ::  ?~  a
        [%bczp %null]                                   ::  ~
      :+  %tsgr                                         ::  =>
        :+  %cnts  [[~ 1] ~]                            ::  %=  .
        :~  :-  [%a ~]                                  ::  a
            :+  %tsgr                                   ::  =>
              [[%cnzy %v] [%tsgl [~ 3] [%cnzy %a]]]     ::  [v +.a]
            i.q.gen                                     ::
        ==                                              ::  ==
      $(q.gen t.q.gen)                                  ::
    ::
        [%smsg *]                                       ::                  ;~
      |-  ^-  twig
      ?-  q.gen
          ~       ~|(%open-smsg !!)
          ^
        :+  %tsgr  [%ktts %v ~ 1]                       ::  =>  v=.
        |-  ^-  twig                                    ::
        ?:  ?=(~ t.q.gen)                               ::
          [%tsgr [%cnzy %v] i.q.gen]                    ::  =>(v {i.q.gen})
        :+  %tsls  [%ktts %a $(q.gen t.q.gen)]          ::  =+  ^=  a
        :+  %tsls                                       ::    {$(q.gen t.q.gen)}
          [%ktts %b [%tsgr [%cnzy %v] i.q.gen]]         ::  =+  ^=  b
        :+  %tsls                                       ::    =>(v {i.q.gen})
          [%ktts %c [%tsgl [~ 6] [%cnzy %b]]]           ::  =+  c=+6.b
        :-  %brdt                                       ::  |.
        :^    %cnls                                     ::  %+
            [%tsgr [%cnzy %v] p.gen]                    ::      =>(v {p.gen})
          [%cnhp [%cnzy %b] [%cnzy %c] ~]               ::    (b c)
        [%cnts [%a ~] [[[[%& 6] ~] [%cnzy %c]] ~]]      ::  a(+6 c)
      ==
    ::
        [%smsm *]                                       ::                  ;;
      :+  %tsgr  [%ktts %v ~ 1]                         ::  =>  v=.
      :+  %tsls  [%ktts %a [%tsgr [%cnzy %v] p.gen]]    ::  =+  a==>(v {p.gen})
      :+  %tsls  [%ktts %b [%tsgr [%cnzy %v] q.gen]]    ::  =+  b==>(v {q.gen})
      :+  %tsls                                         ::  =+  c=(a b)
        [%ktts %c [%cnhp [%cnzy %a] [%cnzy %b] ~]]      ::
      [%wtgr [%dtts [%cnzy %c] [%cnzy %b]] [%cnzy %c]]  ::  ?>(=(c b) c)
    ::
        [%tsbr *]
      [%tsls ~(bunt al p.gen) q.gen]
    ::
        [%tscl *]
      [%tsgr [%cncb [[~ 1] ~] p.gen] q.gen]
    ::
        [%tsdt *]
      [%tsgr [%cncb [[~ 1] ~] [[p.gen q.gen] ~]] r.gen]
    ::
        [%tskt *]                                       ::                  =^
      =+  cog=rusk(gen p.gen)                           ::
      =+  wuy=(weld rake(gen q.gen) `wing`[%v ~])       ::
      :+  %tsgr  [%ktts %v ~ 1]                         ::  =>  v=.
      :+  %tsls  [%ktts %a %tsgr [%cnzy %v] r.gen]      ::  =+  a==>(v \r.gen)
      :^  %tsdt  wuy  [%tsgl [~ 3] [%cnzy %a]]          ::  =.  \wuy  +.a
      :+  %tsgr  :-  :+  %ktts  cog                     ::  =>  :-  ^=  \cog
                     [%tsgl [~ 2] [%cnzy %a]]           ::          -.a
                 [%cnzy %v]                             ::      v
      s.gen                                             ::  s.gen
    ::
        [%tsgl *]  [%tsgr q.gen p.gen]
        [%tsls *]  [%tsgr [p.gen [~ 1]] q.gen]
        [%tshp *]  [%tsls q.gen p.gen]
        [%tssg *]
      |-  ^-  twig
      ?~  p.gen    [%zpzp ~]
      ?~  t.p.gen  i.p.gen
      [%tsgr i.p.gen $(p.gen t.p.gen)]
        [%wtbr *]
      |-
      ?@(p.gen [%dtzz %f 1] [%wtcl i.p.gen [%dtzz %f 0] $(p.gen t.p.gen)])
    ::
        [%wtdt *]   [%wtcl p.gen r.gen q.gen]
        [%wtgl *]   [%wtcl p.gen [%zpzp ~] q.gen]
        [%wtgr *]   [%wtcl p.gen q.gen [%zpzp ~]]
        [%wtkt *]   [%wtcl [%wtts [%axil %atom %$] p.gen] r.gen q.gen]
        [%wthp *]
      |-
      ?@  q.gen
        [%zpfs [%cnzz p.gen]]
      :^    %wtcl
          [%wtts p.i.q.gen p.gen]
        q.i.q.gen
      $(q.gen t.q.gen)
    ::
        [%wtls *]   [%wthp p.gen (weld r.gen `_r.gen`[[[%axil %noun] q.gen] ~])]
        [%wtpm *]
      |-
      ?@(p.gen [%dtzz %f 0] [%wtcl i.p.gen $(p.gen t.p.gen) [%dtzz %f 1]])
    ::
        [%wtpt *]   [%wtcl [%wtts [%axil %atom %$] p.gen] q.gen r.gen]
        [%wtsg *]   [%wtcl [%wtts [%axil %null] p.gen] q.gen r.gen]
        [%wtzp *]   [%wtcl p.gen [%dtzz %f 1] [%dtzz %f 0]]
    ::
        [%wthz *]  
      =+  vaw=~(. ah p.gen)
      %-  gray:vaw
      [%wthp puce:vaw (turn q.gen |=([a=tile b=twig] [a (blue:vaw b)]))]
    ::
        [%wtlz *]
      =+  vaw=~(. ah p.gen)
      %-  gray:vaw
      ^-  twig
      :+  %wtls  puce:vaw 
      [(blue:vaw q.gen) (turn r.gen |=([a=tile b=twig] [a (blue:vaw b)]))]
    ::
        [%wtsz *]
      =+  vaw=~(. ah p.gen)
      %-  gray:vaw
      [%wtsg puce:vaw (blue:vaw q.gen) (blue:vaw r.gen)]
    ::
        [%wtkz *]
      =+  vaw=~(. ah p.gen)
      %-  gray:vaw
      [%wtkt puce:vaw (blue:vaw q.gen) (blue:vaw r.gen)]
    ::
        [%wtpz *]
      =+  vaw=~(. ah p.gen)
      %-  gray:vaw
      [%wtpt puce:vaw (blue:vaw q.gen) (blue:vaw r.gen)]
    ::
        [%zpcb *]   q.gen
        [%zpgr *]   
      [%cnhp [%cnzy %onan] [%zpsm [%bctr [%herb [%cnzy %abel]]] p.gen] ~]
    ::
        [%zpwt *]
      ?:  ?:  ?=(@ p.gen)
            (lte stub p.gen)
          &((lte stub p.p.gen) (gte stub q.p.gen))
        q.gen 
      ~|([%stub-fail stub p.gen] !!)
    ::
        *           gen
    ==
  ::
  ++  rake
    ^-  wing
    ?-  gen
      [~ *]         [gen ~]
      [%cnzy *]     [p.gen ~]
      [%cnzz *]     p.gen
      [%cnts * ~]   p.gen
      [%zpcb *]     rake(gen q.gen)
      *             ~|(%rake-twig !!)
    ==
  ++  rusk
    ^-  term
    =+  wig=rake
    ?.  ?=([@ ~] wig)
      ~|(%rusk-twig !!)
    i.wig
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fC, compilation proper       ::
::
++  ut
  ~%    %ut
      +>+
    ==
      %fan    fan
      %rib    rib
      %vet    vet
      %fab    fab
      %burn   burn
      %busk   busk
      %crop   crop
      %duck   duck
      %dune   dune
      %dunk   dunk
      %fink   fink
      %fire   fire
      %firm   firm
      %fish   fish
      %fuse   fuse
      %gain   gain
      %heal   heal
      %lose   lose
      %mint   mint
      %moot   moot
      %mull   mull
      %nest   nest
      %play   play
      %park   park
      %peek   peek
      %repo   repo
      %rest   rest
      %sift   sift
      %seek   seek
      %tack   tack
      %tock   tock
      %wrap   wrap
    ==
  =+  :*  fan=*(set ,[type twig])
          rib=*(set ,[type type twig])
          vet=`?`&
          fab=`?`&
      ==
  =+  sut=`type`%noun
  |%
  ++  burn
    =+  gil=*(set type)
    |-  ^-  *
    ?-    sut
        [%atom *]   0
        [%bull *]   ~|(%burn-bull !!)
        [%cell *]   [$(sut p.sut) $(sut q.sut)]
        [%core *]   [p.r.q.sut $(sut p.sut)]
        [%cube *]   p.sut
        [%face *]   $(sut repo)
        [%fork *]   $(sut p.sut)
        [%hold *]   ?:  (~(has in gil) sut)
                      ~_  (dunk %type)
                    ~|(%burn-loop !!)
                    $(sut repo, gil (~(put in gil) sut))
        %noun       0
        %void       ~|(%burn-void !!)
    ==
  ::
  ++  busk
    ~/  %busk
    |=  [cog=term hyp=wing]
    ^-  type
    (bull [cog hyp (seep %both hyp)] sut)
  ::
  ++  conk
    |=  got=toga
    ^-  type
    ?@  got  [%face got sut]
    ?-  -.got
      0  sut
      1  [%face p.got $(got q.got)]
      2  ?>  |(!vet (nest(sut [%cell %noun %noun]) & sut))
         :+  %cell
           $(got p.got, sut (peek %both 2)) 
         $(got q.got, sut (peek %both 3))
    ==
  ::
  ++  crop
    ~/  %crop
    |=  ref=type
    =+  bix=*(set ,[type type])
    =<  dext
    |%
    ++  dext
      ^-  type
      ~|  %crop-dext
      ::  ~_  (dunk 'dext: sut')
      ::  ~_  (dunk(sut ref) 'dext: ref')
      ?:  |(=(sut ref) =(%noun ref))
        %void
      ?:  =(%void ref)
        sut
      ?-    sut
          [%atom *]
        ?-    ref
            [%atom *]   %void
            [%cell *]   sut
            *           sint
        ==
      ::
          [%bull *]   (bull p.sut dext(sut q.sut))
          [%cell *]
        ?-  ref
          [%atom *]  sut
          [%cell *]  ?:  (nest(sut p.ref) | p.sut)
                       (cell p.sut dext(sut q.sut, ref q.ref))
                     sut
          *          sint
        ==
      ::
          [%core *]
        ?:  ?=(?([%atom *] [%cell *]) ref)
          sut
        sint
      ::
          [%cube *]
        ?:  &(?=([%cube *] ref) =(p.sut p.ref))
          %void
        ?:  ?=(?([%atom *] [%cell *]) ref)
          =+  foz=dext(sut q.sut)
          ?:  (firm(sut foz) p.sut)
            (cube p.sut foz)
          %void
        sint
      ::
          [%face *]   (face p.sut dext(sut q.sut))
          [%fork *]   (fork dext(sut p.sut) dext(sut q.sut))
          [%hold *]
        ?:  (~(has in bix) [sut ref])
          ~|(%crop-loop !!)
        (reco |=(a=type dext(sut a, bix (~(put in bix) [sut ref]))))
      ::
          %noun       (reco |=(a=type dext(sut a)))
          %void       %void
      ==
    ::
    ++  sint
      ^-  type
      ?-    ref
        [%core *]  sut
        [%cube *]  sut
        [%face *]  dext(ref repo(sut ref))
        [%fork *]  dext(sut dext(ref p.ref), ref q.ref)
        [%hold *]  dext(ref repo(sut ref))
        *          !!
      ==
    --
  ::
  ++  cool
    |=  [pol=? hyp=wing ref=type]
    ^-  type
    =+  peh=`wing`(flop hyp)
    |-  ^-  type
    ?~  peh
      ?:(pol (fuse ref) (crop ref))
    =>  .(i.peh ?^(i.peh i.peh [%| p=0 q=i.peh]))
    =+  ^=  poz  ^-  post
        ?-  -.i.peh
          &  [p.i.peh %& (peek %both p.i.peh)]
          |  (finq p.i.peh %both q.i.peh)
        ==
    |-  ^-  type
    ?:  =(1 p.poz)
      ?-  -.q.poz
        0  ?-  -.i.peh
              &  ^$(peh t.peh)
              |  (face q.i.peh ^$(peh t.peh, sut p.q.poz))
           ==
        1  ^$(peh t.peh)
        2  %+  bull 
             [p.p.q.poz q.p.q.poz r.p.q.poz ^$(peh t.peh, sut s.p.q.poz)] 
           q.q.poz
      ==
    =+  [now=(cap p.poz) lat=(mas p.poz)]
    =+  vil=*(set type)
    |-  ^-  type
    ?-    sut
        [%atom *]   %void
        [%bull *]   (reco |=(p=type (bull p.sut ^$(sut p))))
        [%cell *]
      ?:  =(2 now)
        (cell ^$(p.poz lat, sut p.sut) q.sut)
      (cell p.sut ^$(p.poz lat, sut q.sut))
    ::
        [%core *]   ?.(=(3 now) sut (core ^$(p.poz lat, sut p.sut) q.sut))
        [%cube *]   (reco |=(p=type ^$(sut p)))
        [%face *]   (reco |=(p=type (face p.sut ^$(sut p))))
        [%fork *]
      ?:  (~(has in vil) sut)
        %void
      =>  .(vil (~(put in vil) sut))
      (fork $(sut p.sut) $(sut q.sut))
    ::
        [%hold *]   (reco |=(p=type ^$(sut p)))
        %noun       (reco |=(p=type ^$(sut p)))
        %void       %void
    ==
  ::
  ++  dank  |=(pax=path ^-(tank (dish [~ %path] pax)))
  ++  dart  |=(pax=path ^-(tape ~(ram re (dank pax))))
  ++  deal  |=(lum=* (dish dole lum))
  ++  dial
    |=  ham=calf
    =+  gid=*(set ,@ud)
    |-  ^-  tank
    ?-    q.ham
        %noun      [%leaf '*' ~]
        %path      [%leaf '/' ~]
        %tank      [%leaf '*' 't' ~]
        %void      [%leaf '#' ~]
        %wool      [%leaf '*' '"' '"' ~]
        %wall      [%leaf '*' '\'' '\'' ~]
        %yarn      [%leaf '"' '"' ~]
        [%atom *]  [%leaf '@' (trip p.q.ham)]
        [%core *]
      :+  %rose
        [[' ' ~] ['<' ~] ['>' ~]]
      |-  ^-  (list tank)
      ?~  p.q.ham
        [^$(q.ham q.q.ham) ~]
      [[%leaf (rip 3 i.p.q.ham)] $(p.q.ham t.p.q.ham)]
    ::
        [%face *]
      [%palm [['=' ~] ~ ~ ~] [%leaf (trip p.q.ham)] $(q.ham q.q.ham) ~]
    ::
        [%list *]
      [%rose [" " (weld (trip p.q.ham) "(") ")"] $(q.ham q.q.ham) ~]
    ::
        [%pick *]
      :+  %rose
        [[' ' ~] ['{' ~] ['}' ~]]
      |-  ^-  (list tank)
      ?~(p.q.ham ~ [^$(q.ham i.p.q.ham) $(p.q.ham t.p.q.ham)])
    ::
        [%plot *]
      :+  %rose
        [[' ' ~] ['[' ~] [']' ~]]
      |-  ^-  (list tank)
      ?~(p.q.ham ~ [^$(q.ham i.p.q.ham) $(p.q.ham t.p.q.ham)])
    ::
        [%pear *]
      [%leaf '%' ~(rend co [~ p.q.ham q.q.ham])]
    ::
        [%stop *]
      ?:  (~(has in gid) p.q.ham)
        [%leaf '$' ~(rend co [~ %ud p.q.ham])]
      :+  %palm
        [['.' ~] ['^' '$' ~(rend co [~ %ud p.q.ham])] ~ ~]
      [$(gid (~(put in gid) p.q.ham), q.ham (need (~(get by p.ham) p.q.ham))) ~]
    ::
        [%tree *]
      [%rose [" " (weld (trip p.q.ham) "(") ")"] $(q.ham q.q.ham) ~]
    ::
        [%unit *]
      [%rose [" " (weld (trip p.q.ham) "(") ")"] $(q.ham q.q.ham) ~]
    ==
  ::
  ++  dish
    |=  [ham=calf lum=*]  ^-  tank
    ~|  [%dish-h ?@(q.ham q.ham -.q.ham)]
    ::  ~|  [%lump lum]
    %-  need
    |-  ^-  (unit tank)
    ?-    q.ham
        %noun
      %=    $
          q.ham
        ?:  ?=(@ lum)
          [%atom %$]
        :-  %plot
        |-  ^-  (list wine)
        [%noun ?:(?=(@ +.lum) [[%atom %$] ~] $(lum +.lum))]
      ==
    ::
        %path
      :-  ~
      :+  %rose
        [['/' ~] ['/' ~] ~]
      |-  ^-  (list tank)
      ?@  lum
        ?>(?=(~ lum) ~)
      ?>  ?=(@ -.lum)
      [[%leaf (rip 3 -.lum)] $(lum +.lum)]
    ::
        %tank
      =+  cis=(tank lum)
      ?.(=(lum cis) ~ [~ cis])
    ::
        %wall
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '|' ~] ['|' '>' ~]]
      |-  ^-  (list tank)
      ?@  lum
        ?>(?=(~ lum) ~)
      [[%leaf (trip ((hard ,@) -.lum))] $(lum +.lum)]
    ::
        %wool
      :-  ~
      :+  %rose
        [[' ' ~] ['<' '<' ~] ['>' '>' ~]]
      |-  ^-  (list tank)
      ?@  lum
        ?>(?=(~ lum) ~)
      [(need ^$(q.ham %yarn, lum -.lum)) $(lum +.lum)]
    ::
        %yarn
      [~ %leaf '"' (weld (tape lum) `tape`['"' ~])]
    ::
        %void
      ~
    ::
        [%atom *]
      ?.  ?=(@ lum)
        ~
      :+  ~
        %leaf 
      ?:  =(%$ p.q.ham)    ~(rend co [~ %ud lum]) 
      ?:  =(%t p.q.ham)    ['\'' (weld (rip 3 lum) `tape`['\'' ~])]
      ?:  =(%tas p.q.ham)  ['%' ?.(=(0 lum) (rip 3 lum) ['$' ~])]
      ~(rend co [~ p.q.ham lum])
    ::
        [%core *]
      ::  XX  needs rethinking for core metal
      ::  ?.  ?=(^ lum)  ~
      ::  =>  .(lum `*`lum)
      ::  =-  ?~(tok ~ [~ %rose [[' ' ~] ['<' ~] ['>' ~]] u.tok])
      ::  ^=  tok
      ::  |-  ^-  (unit (list tank))
      ::  ?~  p.q.ham
      ::    =+  den=^$(q.ham q.q.ham)
      ::    ?~(den ~ [~ u.den ~])
      ::  =+  mur=$(p.q.ham t.p.q.ham, lum +.lum)
      ::  ?~(mur ~ [~ [[%leaf (rip 3 i.p.q.ham)] u.mur]])
      [~ (dial ham)]
    ::
        [%face *]
      =+  wal=$(q.ham q.q.ham)
      ?~  wal
        ~
      [~ %palm [['=' ~] ~ ~ ~] [%leaf (trip p.q.ham)] u.wal ~]
    ::
        [%list *]
      ?:  =(~ lum)
        [~ %leaf '~' ~]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['~' '[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?:  ?=(@ lum)
        ?.(=(~ lum) ~ [~ ~])
      =+  [for=^$(q.ham q.q.ham, lum -.lum) aft=$(lum +.lum)]
      ?.  &(?=(^ for) ?=(^ aft))
        ~
      [~ u.for u.aft]
    ::
        [%pick *]
      |-  ^-  (unit tank)
      ?~  p.q.ham
        ~
      =+  wal=^$(q.ham i.p.q.ham)
      ?~  wal
        $(p.q.ham t.p.q.ham)
      wal
    ::
        [%plot *]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['[' ~] [']' ~]] u.tok]
      ^=  tok
      |-  ^-  (unit (list tank))
      ?~  p.q.ham
        ~
      ?:  ?=([* ~] p.q.ham)
        =+  wal=^$(q.ham i.p.q.ham)
        ?~(wal ~ [~ [u.wal ~]])
      ?@  lum
        ~
      =+  gim=^$(q.ham i.p.q.ham, lum -.lum)
      ?~  gim
        ~
      =+  myd=$(p.q.ham t.p.q.ham, lum +.lum)
      ?~  myd
        ~
      [~ u.gim u.myd]
    ::
        [%pear *]
      ?.  =(lum q.q.ham)
        ~
      =+  fox=~(rend co [~ p.q.ham q.q.ham])
      [~ %leaf ?:(=(['~' ~] fox) fox ['%' fox])]
    ::
        [%stop *]
      =+  kep=(~(get by p.ham) p.q.ham)
      ?~  kep
        ~|([%stop-loss p.q.ham] !!)
      $(q.ham u.kep)
    ::
        [%tree *]
      =-  ?~  tok
            ~
          [~ %rose [[' ' ~] ['{' ~] ['}' ~]] u.tok]
      ^=  tok
      =+  tuk=*(list tank)
      |-  ^-  (unit (list tank))
      ?:  =(~ lum)
        [~ tuk]
      ?.  ?=([n=* l=* r=*] lum)
        ~
      =+  rol=$(lum r.lum)
      ?~  rol
        ~
      =+  tim=^$(q.ham q.q.ham, lum n.lum)
      ?~  tim
        ~
      $(lum l.lum, tuk [u.tim u.rol])
    ::
        [%unit *]
      ?@  lum
        ?.(=(~ lum) ~ [~ %leaf '~' ~])
      ?.  =(~ -.lum)
        ~
      =+  wal=$(q.ham q.q.ham, lum +.lum)
      ?~  wal
        ~
      [~ %rose [[' ' ~] ['[' ~] [']' ~]] [%leaf '~' ~] u.wal ~]
    ==
  ::
  ++  doge
    |=  ham=calf
    =-  ?+  woz  woz
          [%list * [%atom %'ta']]  %path
          [%list * [%atom %'t']]   %wall
          [%list * [%atom %'tD']]  %yarn
          [%list * %yarn]         %wool
        ==
    ^=  woz
    ^-  wine
    ?.  ?=([%stop *] q.ham)
      ?:  ?&  ?=  [%pick [%pear %n 0] [%plot [%pear %n 0] [%face *] ~] ~]
                q.ham
              =(1 (met 3 p.i.t.p.i.t.p.q.ham))
          ==
        [%unit =<([p q] i.t.p.i.t.p.q.ham)]
      q.ham
    =+  may=(~(get by p.ham) p.q.ham)
    ?~  may
      q.ham
    ?.  ?&  ?=([%pick *] u.may)
            ?=(^ p.u.may)
            =([%pear %n 0] i.p.u.may)
        ==
      q.ham
    ?:  ?&  ?=([[%plot [%face *] [%face * %stop *] ~] ~] t.p.u.may)
            =(p.q.ham p.q.i.t.p.i.t.p.u.may)
            =(1 (met 3 p.i.p.i.t.p.u.may))
            =(1 (met 3 p.i.t.p.i.t.p.u.may))
        ==
      :+  %list
        (cat 3 p.i.p.i.t.p.u.may p.i.t.p.i.t.p.u.may)
      q.i.p.i.t.p.u.may
    ?:  ?&  ?=  $:  $:  %plot
                        [%face *]
                        [%face * %stop *]
                        [[%face * %stop *] ~]
                    ==
                    ~
                ==
                t.p.u.may
            =(p.q.ham p.q.i.t.p.i.t.p.u.may)
            =(p.q.ham p.q.i.t.t.p.i.t.p.u.may)
            =(1 (met 3 p.i.p.i.t.p.u.may))
            =(1 (met 3 p.i.t.p.i.t.p.u.may))
            =(1 (met 3 p.i.t.t.p.i.t.p.u.may))
        ==
      :+  %tree
        %^    cat
            3
          p.i.p.i.t.p.u.may
        (cat 3 p.i.t.p.i.t.p.u.may p.i.t.t.p.i.t.p.u.may)
      q.i.p.i.t.p.u.may
    q.ham
  ::
  ++  dole
    ^-  calf
    =+  gil=*(set type)
    =+  dex=[p=*(map type ,@) q=*(map ,@ wine)]
    =<  [q.p q]
    |-  ^-  [p=[p=(map type ,@) q=(map ,@ wine)] q=wine]
    =-  [p.tez (doge q.p.tez q.tez)]
    ^=  tez
    ^-  [p=[p=(map type ,@) q=(map ,@ wine)] q=wine]
    ?-    sut
        %noun      [dex sut]
        %void      [dex sut]
        [%atom *]  [dex sut]
        [%bull *]  $(sut q.sut)   ::  something better here
        [%cell *]
      =+  hin=$(sut p.sut)
      =+  yon=$(dex p.hin, sut q.sut)
      :-  p.yon
      :-  %plot
      ?:(?=([%plot *] q.yon) [q.hin p.q.yon] [q.hin q.yon ~])
    ::
        [%core *]
      =+  yad=$(sut p.sut)
      :-  p.yad
      =+  ^=  doy  ^-  [p=(list ,@ta) q=wine]
          ?:  ?=([%core *] q.yad)
            [p.q.yad q.q.yad]
          [~ q.yad]
      :-  %core
      :_  q.doy
      :_  p.doy
      %^  cat  3
        %~  rent  co
        :+  ~  %ud
        |-  ^-  @
        ?-  q.r.q.sut
          ~        0
          [* ~ ~]  1
          [* ~ *]  +($(q.r.q.sut r.q.r.q.sut))
          [* * ~]  +($(q.r.q.sut l.q.r.q.sut))
          [* * *]  .+  %+  add
                         $(q.r.q.sut l.q.r.q.sut)
                       $(q.r.q.sut r.q.r.q.sut)
        ==
      %^  cat  3
        ?-(p.q.sut %gold '.', %iron '|', %lead '?', %zinc '&')
      =+  gum=(mug q.r.q.sut)
      %+  can  3
      :~  [1 (add 'a' (mod gum 26))]
          [1 (add 'a' (mod (div gum 26) 26))]
          [1 (add 'a' (mod (div gum 676) 26))]
      ==
    ::
        [%cube *]
      ?.  ?=(@ p.sut)
        $(sut repo)
      =+  pum=$(sut q.sut)
      ?>  ?=([%atom *] q.pum)
      [p.pum [%pear p.q.pum p.sut]]
    ::
        [%face *]
      =+  yad=$(sut q.sut)
      [p.yad [%face p.sut q.yad]]
    ::
        [%fork *]
      =+  hin=$(sut p.sut)
      =+  yon=$(dex p.hin, sut q.sut)
      :-  p.yon
      ?:  =(%void q.hin)
        q.yon
      ?:  |(=(%void q.yon) =(q.hin q.yon))
        q.hin
      :-  %pick
      ?.  ?=([%pick *] q.yon)
        [q.hin q.yon ~]
      ?>  ?=(^ p.q.yon)
      ?:(=(q.hin i.p.q.yon) p.q.yon [q.hin p.q.yon])
    ::
        [%hold *]
      =+  hey=(~(get by p.dex) sut)
      ?^  hey
        [dex [%stop u.hey]]
      ?:  (~(has in gil) sut)
        =+  dyr=~(wyt by p.dex)
        [[(~(put by p.dex) sut dyr) q.dex] [%stop dyr]]
      =+  rom=$(gil (~(put in gil) sut), sut repo)
      =+  rey=(~(get by p.p.rom) sut)
      ?~  rey
        rom
      [[p.p.rom (~(put by q.p.rom) u.rey q.rom)] [%stop u.rey]]
    ==
  ::
  ++  duck  ^-(tank (dial dole))
  ++  dune  |.(duck)
  ++  dunk
    |=  paz=term  ^-  tank
    :+  %palm
      [['.' ~] ['-' ~] ~ ~]
    [[%leaf (mesc (trip paz))] duck ~]
  ::
  ++  fino
    |=  [dep=@ud way=?(%read %rite %both %free) cog=term]
    =+  gil=*(set type)
    |-  ^-  [p=@ud q=(unit post)]
    ?+    sut  [dep ~]
        [%bull *]
      ?.  =(cog p.p.sut)
        $(sut q.sut)
      ?.  ?=(0 dep)
        $(dep (dec dep), sut q.sut)
      [0 ~ 1 %2 p.sut q.sut]
    ::
        [%cell *]
      =+  taf=$(sut p.sut)
      ?~  q.taf
        =+  bov=$(dep p.taf, sut q.sut)
        ?~  q.bov
          bov
        [p.bov ~ (peg 3 p.u.q.bov) q.u.q.bov]
      [p.taf ~ (peg 2 p.u.q.taf) q.u.q.taf]
    ::
        [%core *]
      =+  zem=(look cog q.r.q.sut)
      =>  ^+(. ?:(|(=(~ zem) =(0 dep)) . .(dep (dec dep), zem ~)))
      ?^  zem
        [dep ~ 1 [%1 (peg 2 p.u.zem) [[sut(p.q %gold) q.u.zem] ~]]]
      =+  taf=$(sut p.sut)
      ?~  q.taf
        taf
      ?.  (park way p.u.q.taf)
        ~|(%find-park !!)
      [p.taf ~ (peg 3 p.u.q.taf) q.u.q.taf]
    ::
        [%cube *]
      $(sut repo)
    ::
        [%face *]
      ?:  =(cog p.sut)
        ?.  ?=(0 dep)
          [(dec dep) ~]
        [0 ~ 1 %0 q.sut]
      [dep ~]
    ::
        [%fork *]
      ~|  %fork
      ?:  (~(has in gil) q.sut)
        $(sut p.sut)
      ?:  (~(has in gil) p.sut)
        $(sut q.sut)
      =+  [hax=$(sut p.sut) yor=$(sut q.sut)]
      ~|  %find-fork
      ?:  =(hax yor)
        hax
      ?>  &(?=(^ q.hax) ?=(^ q.yor) =(p.hax p.yor) =(p.u.q.hax p.u.q.yor))
      :-   p.hax
      ?-  -.q.u.q.hax
        0  ?>  ?=(0 -.q.u.q.yor)
           [~ p.u.q.hax %0 (fork p.q.u.q.hax p.q.u.q.yor)]
        1  ?>  &(?=(1 -.q.u.q.yor) =(p.q.u.q.yor p.q.u.q.hax))
           [~ p.u.q.hax %1 p.q.u.q.hax (weld q.q.u.q.hax q.q.u.q.yor)]
        2  ?>  ?&  ?=(2 -.q.u.q.yor) 
                   =(p.p.q.u.q.hax p.p.q.u.q.yor)
                   =(q.p.q.u.q.hax q.p.q.u.q.yor)
                   =(r.p.q.u.q.hax r.p.q.u.q.yor)
               ==
           :*  ~
               p.u.q.hax
               %2
               :*  p.p.q.u.q.hax 
                   q.p.q.u.q.hax 
                   r.p.q.u.q.hax 
                   (fork s.p.q.u.q.hax s.p.q.u.q.yor)
               ==
               (fork q.q.u.q.hax q.q.u.q.yor)
           ==
      ==
    ::
        [%hold *]
      ?:  (~(has in gil) sut)
        [dep ~]
      $(gil (~(put in gil) sut), sut repo)
    ==
  ::
  ++  fink
    ~/  %fink
    |=  [dep=@ud way=?(%read %rite %both %free) cog=term]
    ^-  port
    ~_  (dunk 'type')
    ~_  (show [%c 'find-limb'] ?:(=(%$ cog) '$' [%a cog]))
    =+  hoq=(fino dep way cog)
    ?~  q.hoq
      ~|(%find-none !!)
    (flee u.q.hoq)
  ::
  ++  finq
    |=  [dep=@ud way=?(%read %rite %both %free) cog=term]
    ^-  post
    ::  ~_  (dunk 'type')
    ~_  (show [%c 'find-limb'] ?:(=(%$ cog) '$' [%a cog]))
    =+  hoq=(fino dep way cog)
    ?~  q.hoq
      ~|(%find-none !!)
    u.q.hoq
  ::
  ++  fire
    ~/  %fire
    |=  hag=(list ,[p=type q=foot])
    ^-  type
    ?:  ?=([[* [%elm ~ 1]] ~] hag)
      p.i.hag
    :-  %hold
    %+  turn
      hag.$
    |=  [p=type q=foot]
    ?.  ?=([%core *] p)
      ~|(%fire-core !!)
    =+  dox=[%core q.q.p q.p]
    ?:  ?=(%ash -.q)
      ~|  %fire-ash
      ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-dry)
      ?>  ?|(!vet (nest(sut q.q.p) & p.p))
      [dox p.q]
    ~|  [%fire-odd -.q]
    ?>  ?=(%elm -.q)
    ~|  %fire-elm
    ::  ~_  (dunk(sut [%cell q.q.p p.p]) %fire-wet)
    ?>  ?|  !vet
            (~(has in rib) [sut dox p.q])
            (mull(sut p, rib (~(put in rib) [sut dox p.q])) %noun dox p.q)
        ==
    [p p.q]
  ::
  ++  firm
    ~/  %firm
    |=  dib=*
    =+  bix=*(set ,[type *])
    |-  ^-  ?
    ?-    sut
        [%atom *]  !.?(dib)
        [%bull *]  &($(sut q.sut) $(sut s.p.sut, dib .*(dib [0 r.p.sut])))
        [%cell *]  &(.?(dib) $(sut p.sut, dib -.dib) $(sut q.sut, dib +.dib))
        [%core *]
      ?&  .?(dib)
          $(sut p.sut, dib -.dib)
          =(+.dib ?:(=(~ p.r.q.sut) ~|(%firm-core !!) p.r.q.sut))
      ==
    ::
        [%cube *]  =(dib p.sut)
        [%face *]  $(sut q.sut)
        [%fork *]  |($(sut p.sut) $(sut q.sut))
        [%hold *]
      ?|  (~(has in bix) [sut dib])
          $(bix (~(put in bix) [sut dib]), sut repo)
      ==
    ::
        %noun      &
        %void      |
    ==
  ::
  ++  fish
    ~/  %fish
    |=  axe=axis
    =+  vot=*(set type)
    |-  ^-  nock
    ?-  sut
        %void       [%1 1]
        %noun       [%1 0]
        [%atom *]   (flip [%3 %0 axe])
        [%bull *]   ~|(%bull-fish !!)
        [%cell *]
      %+  flan
        [%3 %0 axe]
      (flan $(sut p.sut, axe (peg axe 2)) $(sut q.sut, axe (peg axe 3)))
    ::
        [%core *]   [%0 0]
        [%cube *]   [%5 [%1 p.sut] [%0 axe]]
        [%face *]   $(sut q.sut)
        [%fork *]   (flor $(sut p.sut) $(sut q.sut))
        [%hold *]
      ?:  (~(has in vot) sut)
        [%0 0]
      =>  %=(. vot (~(put in vot) sut))
      $(sut repo)
    ==
  ::
  ++  fuse
    ~/  %fuse
    |=  ref=type
    =+  bix=*(set ,[type type])
    |-  ^-  type
    ?:  ?|(=(sut ref) =(%noun ref))
      sut
    ?-    sut
        [%atom *]
      ?-    ref
          [%atom *]   ?:((fitz p.ref p.sut) sut ref)
          [%cell *]   %void
          *           $(sut ref, ref sut)
      ==
        [%bull *]   (bull p.sut $(sut q.sut))
        [%cell *]
      ?-  ref
        [%cell *]   (cell $(sut p.sut, ref p.ref) $(sut q.sut, ref q.ref))
        *           $(sut ref, ref sut)
      ==
    ::
        [%core *]     $(sut repo)
        [%cube *]
      =+  foz=$(sut q.sut)
      ?:  (firm(sut foz) p.sut)
        (cube p.sut foz)
      %void
    ::
        [%face *]     (face p.sut $(sut q.sut))
        [%fork *]     (fork $(sut p.sut) $(sut q.sut))
        [%hold *]
      ?:  (~(has in bix) [sut ref])
        ~|(%fuse-loop !!)
      (reco |=(a=type ^$(sut a, bix (~(put in bix) [sut ref]))))
    ::
        %noun       ref
        %void       %void
    ==
  ::
  ++  gain
    ~/  %gain
    |=  gen=twig  ^-  type
    (chip & gen)
  ::
  ++  hang
    ~/  %hang
    |=  [dab=(map term foot) rud=(map term foot)]
    ^-  (map term foot)
    =+  goy=(~(tap by rud) ~)
    =+  waf=dab
    |-  ^+  dab
    ?@  goy
      waf
    ~|  [%hang-on p.i.goy]
    =+  yeq=(~(get by dab) p.i.goy)
    ?<  ?=(~ yeq)
    ?-    -.u.yeq
        %ash
      ?>  ?=([%ash *] q.i.goy)
      $(goy t.goy, waf (~(put by waf) p.i.goy q.i.goy))
    ::
        %elm
      ~|([%hang-elm p.i.goy] !!)
    ::
        %oak
      ?>  ?=([%yew *] q.i.goy)
      $(goy t.goy, waf (~(put by waf) p.i.goy q.i.goy))
    ::
        %yew
      ?>  ?=([%yew *] q.i.goy)
      %=    $
          goy  t.goy
          waf
        %+  ~(put by waf)
          p.i.goy
        [%yew ^$(dab p.u.yeq, rud p.q.i.goy)]
      ==
    ==
  ::
  ++  harp
    |=  dab=(map term foot)
    ^-  ?(~ ^)
    ?:  ?=(~ dab)
      ~
    =+  ^=  vad
        ?+  -.q.n.dab  !!
          %ash  q:(mint %noun p.q.n.dab)
          %elm  q:(mint(vet |) %noun p.q.n.dab)
        ==
    ?-    dab
        [* ~ ~]   vad
        [* ~ *]   [vad $(dab r.dab)]
        [* * ~]   [vad $(dab l.dab)]
        [* * *]   [vad $(dab l.dab) $(dab r.dab)]
    ==
  ::
  ++  lose
    ~/  %lose
    |=  gen=twig  ^-  type
    (chip | gen)
  ::
  ++  chip
    ~/  %chip
    |=  [way=? gen=twig]  ^-  type
    ?:  ?=([%wtts *] gen)
      (cool way q.gen (play ~(bunt al p.gen)))
    ?:  ?&(way ?=([%wtpm *] gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    ?:  ?&(!way ?=([%wtbr *] gen))
      |-(?~(p.gen sut $(p.gen t.p.gen, sut ^$(gen i.p.gen))))
    =+  neg=~(open ap gen)
    ?:(=(neg gen) sut $(gen neg))
  ::
  ++  heal
    ~/  %heal
    |=  [qog=(unit term) axe=axis ref=type]
    ^-  type
    ?:  =(1 axe)
      ?~  qog
        ref
      |-  ^-  type
      ?-    sut
          [%core *]   ref
          [%bull *]   (busk(sut $(sut q.sut)) p.p.sut q.p.sut)
          [%face *]   ?.(=(u.qog p.sut) ~|('heal-name' !!) (face p.sut ref))
          [%fork *]   (fork $(sut p.sut) $(sut q.sut))
          [%hold *]   $(sut repo)
          *           ~|([%name u.qog] ~|('heal-name' !!))
      ==
    =+  [now=(cap axe) lat=(mas axe)]
    =+  gil=*(set type)
    |-  ^-  type
    ?-    sut
        [%atom *]   %void
        [%bull *]   (busk(sut $(sut q.sut)) p.p.sut q.p.sut)
        [%cell *]
      ?:  =(2 now)
        (cell ^$(sut p.sut, axe lat) q.sut)
      (cell p.sut ^$(sut q.sut, axe lat))
    ::
        [%core *]
      ?.  =(3 now)
        ~|(%heal-core !!)
      (core ^$(sut p.sut, axe lat) q.sut)
    ::
        [%fork *]   (fork $(sut p.sut) $(sut q.sut))
        [%hold *]
      ?:((~(has in gil) sut) %void $(gil (~(put in gil) sut), sut repo))
    ::
        *           $(sut repo)
    ==
  ::
  ++  mint
    ~/  %mint
    |=  [gol=type gen=twig]
    ^-  [p=type q=nock]
    |^  ^-  [p=type q=nock]
    ?:  ?&(=(%void sut) !?=([%zpcb *] gen))
      ?.  |(!vet ?=([%zpfs *] gen) ?=([%zpzp *] gen))
        ~|(%mint-vain !!)
      [%void %0 0]
    ?-    gen
    ::
        [^ *]
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cons q.hed q.tal)]
    ::
        [%bcpt *]  $(gen (~(whip al q.gen) p:(seep %read p.gen)))
        [%brcn *]  (grow %gold [~ 1] p.gen)
    ::
        [%cnts *]
      =+  lar=(foil (seek %read p.gen))
      =+  mew=(snub q.gen)
      =-  [(nice p.yom) ?:(=(0 p.q.lar) q.yom [%9 p.q.lar q.yom])]
      ^=  yom
      =+  hej=*(list ,[p=axis q=nock])
      |-  ^-  [p=type q=nock]
      ?~  mew
        [(fire q.q.lar) (hike p.lar hej)]
      =+  zil=^$(gen q.i.mew, gol %noun)
      =+  wip=(tock p.i.mew p.zil q.q.lar)
      $(mew t.mew, q.q.lar q.wip, hej [[p.wip q.zil] hej])
    ::
        [%dtkt *]  [(nice %noun) [%11 q:$(gen p.gen, gol %noun)]]
        [%dtls *]  [(nice [%atom %$]) [%4 q:$(gen p.gen, gol [%atom %$])]]
        [%dtzy *]  [(nice (play gen)) [%1 q.gen]]
        [%dtzz *]  [(nice (play gen)) [%1 q.gen]]
        [%dttr *]
      [(nice %noun) [%2 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        [%dtts *]
      [(nice bean) [%5 q:$(gen p.gen, gol %noun) q:$(gen q.gen, gol %noun)]]
    ::
        [%dtwt *]  [(nice bean) [%3 q:$(gen p.gen, gol %noun)]]
        [%ktbr *]  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) q.vat])
        [%ktls *]
      =+(hif=(nice (play p.gen)) [hif q:$(gen q.gen, gol hif)])
    ::
        [%ktpm *]  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) q.vat])
        [%ktsg *]
      =+  nef=$(gen p.gen)
      =+  moc=(mink [burn q.nef] |=(* ~))
      [p.nef ?:(?=(0 -.moc) [%1 p.moc] q.nef)]
    ::
        [%ktts *]  =+(vat=$(gen q.gen) [(conk(sut p.vat) p.gen) q.vat])
        [%ktwt *]  =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) q.vat])
        [%sgzp *]  ~_(duck(sut (play p.gen)) $(gen q.gen))
        [%sggr *]
      =+  hum=$(gen q.gen)
      :-  p.hum
      :+  %10
        ?-    p.gen
            @   p.gen
            ^   [p.p.gen q:$(gen q.p.gen, gol %noun)]
        ==
      q.hum
    ::
        [%tsgr *]
      =+  fid=$(gen p.gen, gol %noun)
      =+  dov=$(sut p.fid, gen q.gen)
      [p.dov (comb q.fid q.dov)]
    ::
        [%tstr *]
      $(gen r.gen, sut (busk p.gen q.gen))
    ::
        [%wtcl *]
      =+  nor=$(gen p.gen, gol bean)
      =+  fex=(gain p.gen)
      =+  wux=(lose p.gen)
      =+  ^=  duy
          ?:  =(%void fex)
            ?:(=(%void wux) [%0 0] [%1 1])
          ?:(=(%void wux) [%1 0] q.nor)
      =+  hiq=$(sut fex, gen q.gen)
      =+  ran=$(sut wux, gen r.gen)
      [(fork p.hiq p.ran) (cond duy q.hiq q.ran)]
    ::
        [%wtts *]
      :-  (nice bean)
      %-  fish(sut (play ~(bunt al p.gen))) 
      (cove q:$(gen [%cnzz q.gen], gol %noun))
    ::
        [%zpcb *]
      ~_  (show %o p.gen)
      =+  hum=$(gen q.gen)
      [p.hum [%10 [%spot %1 p.gen] q.hum]]
    ::
        [%zpcm *]  [(nice (play p.gen)) [%1 q.gen]]
        [%zpcn ~]  =+(pet=seed [(nice p.pet) [%1 q.pet]])
        [%zpfs *]
      ?:  vet
        ~_  (dunk(sut (play p.gen)) 'lost')
        ~|(%mint-lost !!)
      [%void [%0 0]]
    ::
        [%zpsm *]
      =+  vos=$(gol %noun, gen q.gen)       ::  XX validate!
      ::  [(nice (cell (sift (play p.gen)) p.vos)) (cons [%1 p.vos] q.vos)]
      [(nice (cell (play p.gen) p.vos)) (cons [%1 p.vos] q.vos)]
    ::
        [%zpts *]  [(nice %noun) [%1 q:$(vet |, gen p.gen)]]
        [%zpzp ~]  [%void [%0 0]]
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~|(%mint-open !!)
      $(gen doz)
    ==
    ::
    ++  nice
      |=  typ=type
      ~|  %mint-nice
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  [mel=?(%gold %iron %lead %zinc) ruf=twig dab=(map term foot)]
      ^-  [p=type q=nock]
      =+  dan=^$(gen ruf, gol %noun)
      =+  toc=(core p.dan [%gold p.dan [~ dab]])
      =+  dez=(harp(sut toc) dab)
      :-  (nice (core p.dan mel p.dan [dez dab]))
      (cons [%1 dez] q.dan)
    --
  ::
  ++  moot
    =+  gil=*(set type)
    |-  ^-  ?
    ?-  sut
      [%atom *]  |
      [%bull *]  $(sut q.sut)
      [%cell *]  |($(sut p.sut) $(sut q.sut))
      [%core *]  $(sut p.sut)
      [%cube *]  |
      [%face *]  $(sut q.sut)
      [%fork *]  &($(sut p.sut) $(sut q.sut))
      [%hold *]  |((~(has in gil) sut) $(gil (~(put in gil) sut), sut repo))
      %noun      |
      %void      &
    ==
  ::
  ++  mull
    ~/  %mull
    |=  [gol=type dox=type gen=twig]
    ^-  ?
    ?.  vet
      &
    =<  &
    |^  ^-  [p=type q=type]
    ?:  =(%void sut)
      ~|(%mull-none !!)
    ?-    gen
    ::
        [^ *]
      =+  hed=$(gen p.gen, gol %noun)
      =+  tal=$(gen q.gen, gol %noun)
      [(nice (cell p.hed p.tal)) (cell q.hed q.tal)]
    ::
        [%bcpt *]
      =+  sep=(seep %read p.gen)
      =+  pox=(seep(sut dox) %read p.gen)
      ?.  =(p.pox p.sep)  ~|(%mull-bonk-wing !!)
      $(gen (~(whip al q.gen) p.sep))
    ::
        [%brcn *]  (grow %gold [~ 1] p.gen)
        [%cnts *]
      =+  lar=(foil (seek %read p.gen))
      =+  vug=(foil (seek(sut dox) %read p.gen))
      ?.  &(=(p.lar p.vug) =(p.q.lar p.q.vug))
        ~|(%mull-bonk-e !!)
      =+  mew=(snub q.gen)
      =-  [(nice (fire p.yom)) (fire(vet |) q.yom)]
      ^=  yom
      |-  ^-  [p=(list ,[p=type q=foot]) q=(list ,[p=type q=foot])]
      ?@  mew
        [q.q.lar q.q.vug]
      =+  zil=^$(gen q.i.mew, gol %noun)
      =+  cuf=(tock p.i.mew p.zil q.q.lar)
      =+  dof=(tock p.i.mew q.zil q.q.vug)
      ?.  .=(p.cuf p.dof)
        ~|(%mull-bonk-f !!)
      $(mew t.mew, q.q.lar q.cuf, q.q.vug q.dof)
    ::
        [%dtkt *]  =+($(gen p.gen, gol %noun) (both %noun))
        [%dtls *]  =+($(gen p.gen, gol [%atom %$]) (both [%atom %$]))
        [%dtzy *]  (both (play gen))
        [%dtzz *]  (both (play gen))
        [%dttr *]
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (both %noun))
    ::
        [%dtts *]
      =+([$(gen p.gen, gol %noun) $(gen q.gen, gol %noun)] (both bean))
    ::
        [%dtwt *]  =+($(gen p.gen, gol %noun) (both bean))    ::  XX  =|
        [%ktbr *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %iron) (wrap(sut q.vat) %iron)])
    ::
        [%ktls *]
      =+  hif=[p=(nice (play p.gen)) q=(play(sut dox) p.gen)]
      =+($(gen q.gen, gol p.hif) hif)
    ::
        [%ktpm *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %zinc) (wrap(sut q.vat) %zinc)])
    ::
        [%ktts *]
      =+(vat=$(gen q.gen) [(conk(sut p.vat) p.gen) (conk(sut q.vat) p.gen)])
    ::
        [%ktwt *]
      =+(vat=$(gen p.gen) [(wrap(sut p.vat) %lead) (wrap(sut q.vat) %lead)])
    ::
        [%ktsg *]  $(gen p.gen)
        [%sgzp *]  ~_(duck(sut (play p.gen)) $(gen q.gen))
        [%sggr *]  $(gen q.gen)
        [%tsgr *]
      =+  lem=$(gen p.gen, gol %noun)
      $(gen q.gen, sut p.lem, dox q.lem)
    ::
        [%tstr *]
      %=  $
        gen  r.gen
        sut  (busk p.gen q.gen)
        dox  (busk(sut dox) p.gen q.gen)
      ==
    ::
        [%wtcl *]
      =+  nor=$(gen p.gen, gol bean)
      =+  ^=  hiq  ^-  [p=type q=type]
          =+  fex=[p=(gain p.gen) q=(gain(sut dox) p.gen)]
          ?:  =(%void p.fex)
            [%void ?:(=(%void q.fex) %void ~|(%wtcl-z (play(sut q.fex) q.gen)))]
          ?:  =(%void q.fex)
            ~|(%mull-bonk-b !!)
          $(sut p.fex, dox q.fex, gen q.gen)
      =+  ^=  ran  ^-  [p=type q=type]
          =+  wux=[p=(lose p.gen) q=(lose(sut dox) p.gen)]
          ?:  =(%void p.wux)
            [%void ?:(=(%void q.wux) %void ~|(%wtcl-a (play(sut q.wux) r.gen)))]
          ?:  =(%void q.wux)
            ~|(%mull-bonk-c !!)
          $(sut p.wux, dox q.wux, gen r.gen)
      [(nice (fork p.hiq p.ran)) (fork q.hiq q.ran)]
    ::
        [%wtts *]
      =+  nob=~(bunt al p.gen)
      =+  waz=[p=(play nob) q=(play(sut dox) nob)]
      =+  ^=  syx  :-  p=(cove q:(mint %noun [%cnzz q.gen]))
                   q=(cove q:(mint(sut dox) %noun [%cnzz q.gen]))
      =+  pov=[p=(fish(sut p.waz) p.syx) q=(fish(sut q.waz) q.syx)]
      ?.  &(=(p.syx q.syx) =(p.pov q.pov))
        ~|(%mull-bonk-a !!)
      (both bean)
    ::
        [%zpcb *]  ~_((show %o p.gen) $(gen q.gen))
        [%zpcm *]  [(nice (play p.gen)) (play(sut dox) p.gen)]
        [%zpcn ~]  =+(pet=seed [(nice p.pet) p.pet])
        [%zpfs *]
      ?:  vet
        ::  ~_  (dunk(sut (play p.gen)) 'also')
        ~|(%mull-skip !!)
      (both %void)
    ::
        [%zpts *]  (both %noun)
        [%zpsm *]
      =+  vos=$(gol %noun, gen q.gen)       ::  XX validate!
      [(nice (cell (play p.gen) p.vos)) (cell (play(sut dox) p.gen) q.vos)]
    ::
        [%zpzp ~]  (both %void)
        *
      =+  doz=~(open ap gen)
      ?:  =(doz gen)
        ~_  (show [%c 'hoon'] [%q gen])
        ~|(%mull-open !!)
      $(gen doz)
    ==
    ::
    ++  both
      |=  typ=type
      [(nice typ) typ]
    ::
    ++  nice
      |=  typ=type
      ::  ~_  (dunk(sut gol) 'need')
      ::  ~_  (dunk(sut typ) 'have')
      ~|  %mull-nice
      ?>  ?|(!vet (nest(sut gol) & typ))
      typ
    ::
    ++  grow
      |=  [mel=?(%gold %iron %lead %zinc) ruf=twig dab=(map term foot)]
      ~|  %mull-grow
      ^-  [p=type q=type]
      =+  dan=^$(gen ruf, gol %noun)
      =+  ^=  toc  :-  p=(core p.dan [%gold p.dan [~ dab]])
                   q=(core q.dan [%gold q.dan [~ dab]])
      =+  (bake(sut p.toc, dox q.toc) dab)
      :-  (nice (core p.dan mel p.dan [[%0 0] dab]))
      (core q.dan [mel q.dan [[%0 0] dab]])
    ::
    ++  bake
      |=  dab=(map term foot)
      ^-  *
      ?:  ?=(~ dab)
        ~
      =+  ^=  vad
          ?+  -.q.n.dab  !!
            %ash  ^$(gol %noun, gen p.q.n.dab)
            %elm  ~
          ==
      ?-    dab
          [* ~ ~]   vad
          [* ~ *]   [vad $(dab r.dab)]
          [* * ~]   [vad $(dab l.dab)]
          [* * *]   [vad $(dab l.dab) $(dab r.dab)]
      ==
    --
  ::
  ++  meet  |=(ref=type &((nest | ref) (nest(sut ref) | sut)))
  ++  nest
    ~/  %nest
    |=  [tel=? ref=type]
    ^-  ?
    =+  gil=*(set ,[p=type q=type])
    =<  dext
    |%
    ++  cong
      ^-  ?
      ?>  ?&(?=([%core *] sut) ?=([%core *] ref))
      ?:  =(q.sut q.ref)
        dext(sut p.sut, ref p.ref)
      ?.  ?&  dext(sut q.q.sut, ref p.sut)
              dext(sut p.sut, ref q.q.sut)
              dext(sut q.q.ref, ref p.ref)
          ==
        |
      ?&
        ?|(=(p.q.sut p.q.ref) =(%gold p.q.ref))
      ::
        ?|  (~(has in gil) [sut ref])
            %+  %=  cram
                  gil  (~(put in gil) [sut ref])
                  sut  sut(p q.q.sut)
                  ref  ref(p q.q.ref)
                ==
              q.r.q.sut
            q.r.q.ref
        ==
      ::
        ?-    p.q.sut
            %gold
          =+  pac=[s=q.q.sut r=q.q.ref]
          ?&  dext(sut s.pac, ref r.pac)
              dext(sut r.pac, ref s.pac)
          ==
        ::
            %iron
          =+  sam=[s=(peek(sut q.q.sut) %rite 2) r=(peek(sut q.q.ref) %rite 2)]
          dext(sut r.sam, ref s.sam)
        ::
            %lead  &
            %zinc
          =+  pal=[s=(peek(sut q.q.sut) %read 2) r=(peek(sut q.q.ref) %read 2)]
          dext(sut s.pal, ref r.pal)
        ==
      ==
    ::
    ++  cram
      |=  [dab=(map term foot) hem=(map term foot)]
      ^-  ?
      ?-    dab
          ~   =(hem ~)
          ^
        ?&  ?=(^ hem)
            =(p.n.dab p.n.hem)
            $(dab l.dab, hem l.hem)
            $(dab r.dab, hem r.hem)
            ?-    -.q.n.dab
                %ash
              ?&  ?=(%ash -.q.n.hem)
                  dext(sut (play p.q.n.dab), ref (play(sut ref) p.q.n.hem))
              ==
                %elm  =(q.n.dab q.n.hem)
                %oak  ?=(?(%oak %yew) -.q.n.hem)
                %yew
              ?&  ?=(%yew -.q.n.hem)
                  $(dab p.q.n.dab, hem p.q.n.hem)
              ==
            ==
        ==
      ==
    ::
    ++  dext
      ^-  ?
      =-  ?:  tyn
            &
          ?:  tel
            ::  ~_  (dunk %need)
            ::  ~_  (dunk(sut ref) %have)
            ~|(%type-fail !!)
          |
      ^=  tyn
      ?:  =(sut ref)
        &
      ?-    sut
          %void       sint
          %noun       &
      ::
          [%atom *]
        ?.  ?=([%atom *] ref)
          sint
        (fitz p.sut p.ref)
      ::
          [%cell *]
        ?.  ?=([%cell *] ref)
          sint
        ?&
          dext(sut p.sut, ref p.ref)
          dext(sut q.sut, ref q.ref)
        ==
      ::
          [%core *]
        ?.  ?=([%core *] ref)
          sint
        cong
      ::
          [%cube *]
        ?:  ?=([%cube *] ref)
          =(p.sut p.ref)
        sint
      ::
          [%bull *]   
        ?&  dext(sut q.sut) 
            dext(sut s.p.sut, ref (peek(sut ref) %free r.p.sut))
        ==
          [%face *]   dext(sut q.sut)
          [%fork *]
        ?.  ?=(?([%atom *] %noun [%cell *] [%cube *] [%core *]) ref)
          sint
        ?|(dext(tel |, sut p.sut) dext(tel |, sut q.sut))
      ::
          [%hold *]
        ?|
          (~(has in gil) [sut ref])
          dext(gil (~(put in gil) [sut ref]), sut repo)
        ==
      ==
    ::
    ++  sint
      ^-  ?
      ?-  ref
          [%atom *]   |
          [%cell *]   |
          [%fork *]   ?&(dext(ref p.ref) dext(ref q.ref))
          [%hold *]
        ?|
          (~(has in gil) [sut ref])
          dext(gil (~(put in gil) [sut ref]), ref repo(sut ref))
        ==
      ::
          %noun       |
          %void       &
          *           dext(ref repo(sut ref))
      ==
    --
  ::
  ++  park
    ~/  %park
    |=  [way=?(%read %rite %both %free) axe=axis]
    ^-  ?
    ?>  ?=([%core *] sut)
    ?|
      !vet
      ?-    way
          %both  =(%gold p.q.sut)
          %free  &
          %read
        ?-    p.q.sut
            %gold   &
            %iron   |
            %lead   |
            %zinc   =(2 (cap axe))
        ==
      ::
          %rite
        ?-    p.q.sut
            %gold   &
            %iron   =(2 (cap axe))
            %lead   |
            %zinc   |
        ==
      ==
    ==
  ::
  ++  peek
    ~/  %peek
    |=  [way=?(%read %rite %both %free) axe=axis]
    ^-  type
    ?:  =(1 axe)
      sut
    =+  [now=(cap axe) lat=(mas axe)]
    =+  gil=*(set type)
    |-  ^-  type
    ?-    sut
        [%atom *]   %void
        [%cell *]   ?:(=(2 now) ^$(sut p.sut, axe lat) ^$(sut q.sut, axe lat))
        [%core *]
      ?:  =(3 now)
        ?.  (park way lat)
          ::  ~_  (dunk 'type')
          ~_  (show [%c 'axis'] [%d axe])
          ~|(%peek-park !!)
        ^$(sut p.sut, axe lat)
      %noun
    ::
        [%fork *]   (fork $(sut p.sut) $(sut q.sut))
        [%hold *]
      ?:  (~(has in gil) sut)
        %void
      $(gil (~(put in gil) sut), sut repo)
    ::
        %void       %void
        %noun       %noun
        *           $(sut repo)
    ==
  ::
  ++  play
    ~/  %play
    =>  .(vet |)
    |=  gen=twig
    ^-  type
    ?-  gen
      [^ *]      (cell $(gen p.gen) $(gen q.gen))
      [%bcpt *]  $(gen (~(whip al q.gen) p:(seep %read p.gen)))
      [%brcn *]  (core sut %gold sut [[%0 0] p.gen])
      [%cnts *]  =+  lar=(foil (seek %read p.gen))
                 =+  mew=(snub q.gen)
                 =+  rag=q.q.lar
                 %-  fire
                 |-  ^-  (list ,[p=type q=foot])
                 ?@  mew
                   rag
                 $(mew t.mew, rag q:(tock p.i.mew ^$(gen q.i.mew) rag))
      [%dtkt *]  %noun
      [%dtls *]  [%atom %$]
      [%dtzy *]  ?:(=(%f p.gen) ?>((lte q.gen 1) bean) [%atom p.gen])
      [%dtzz *]  [%cube q.gen ?:(.?(q.gen) %noun [%atom p.gen])]
      [%dttr *]  %noun
      [%dtts *]  bean
      [%dtwt *]  bean
      [%ktbr *]  (wrap(sut $(gen p.gen)) %iron)
      [%ktls *]  $(gen p.gen)
      [%ktpm *]  (wrap(sut $(gen p.gen)) %zinc)
      [%ktsg *]  $(gen p.gen)
      [%ktts *]  (conk(sut $(gen q.gen)) p.gen)
      [%ktwt *]  (wrap(sut $(gen p.gen)) %lead)
      [%sgzp *]  ~_(duck(sut ^$(gen p.gen)) $(gen q.gen))
      [%sggr *]  $(gen q.gen)
      [%tsgr *]  $(gen q.gen, sut $(gen p.gen))
      [%tstr *]  $(gen r.gen, sut (busk p.gen q.gen))
      [%wtcl *]  =+  [fex=(gain p.gen) wux=(lose p.gen)]
                 %+  fork
                   ?:(=(%void fex) %void $(sut fex, gen q.gen))
                 ?:(=(%void wux) %void $(sut wux, gen r.gen))
      [%zpcb *]  ~_((show %o p.gen) $(gen q.gen))
      [%zpcm *]  (play p.gen)
      [%zpcn ~]  p:seed
      [%zpfs *]  %void
      [%zpsm *]  (cell $(gen p.gen) $(gen q.gen))
      [%zpts *]  %noun
      [%zpzp ~]  %void
      *          =+  doz=~(open ap gen)
                 ?:  =(doz gen)
                   ~_  (show [%c 'hoon'] [%q gen])
                   ~|(%play-open !!)
                 $(gen doz)
    ==
  ::
  ++  reco
    |*  fuy=_|=(p=type p)
    =+  por=repo
    =+  yot=(fuy por)
    ?:  =(yot por)
      ?:(=(%void por) por sut)
    yot
  ::
  ++  repo
    ^-  type
    ?-  sut
      [%bull *]   q.sut
      [%core *]   [%cell %noun p.sut]
      [%cube *]   q.sut
      [%face *]   q.sut
      [%hold *]   (rest p.sut)
      %noun       [%fork [%atom %$] [%cell %noun %noun]]
      *           ~|(%repo-fltt !!)
    ==
  ::
  ++  rest
    ~/  %rest
    |=  leg=(list ,[p=type q=twig])
    ^-  type
    ?:  (lien leg |=([p=type q=twig] (~(has in fan) [p q])))
      ~|(%rest-loop !!)
    =>  .(fan (~(gas in fan) leg))
    %+  roll
      %-  %~  tap
            in
          %-  ~(gas in *(set type))
          (turn leg |=([p=type q=twig] (play(sut p) q)))
      ~
    =+([p=*type q=`type`%void] |.((fork p q)))
  ::
  ++  seek
    ~/  %seek
    |=  [way=?(%read %rite %both %free) hyp=wing]
    ^-  port
    ?~  hyp
      [1 %& sut]
    =>  .(i.hyp ?^(i.hyp i.hyp [%| p=0 q=i.hyp]))
    =+  zar=$(hyp t.hyp)
    =+  ^=  syp
        ?-    -.q.zar
            &  p.q.zar
            |  (fire (turn q.q.zar |=([p=type q=foot] [p [%ash ~ 1]])))
        ==
    ?-    i.hyp
        [& *]
      [(peg p.zar p.i.hyp) %& (peek(sut syp) way p.i.hyp)]
    ::
        [| *]
      =>  .(sut syp)
      =+  hud=(fink p.i.hyp way q.i.hyp)
      [(peg p.zar p.hud) q.hud]
    ==
  ::
  ++  seep
    |=  [way=?(%read %rite %both %free) hyp=wing]
    ^-  [p=axis q=type]
    =+  zar=(seek way hyp)
    ?>(?=(& -.q.zar) [p.zar p.q.zar])
  ::
  ++  sift
    |=  ref=type
    ^-  type
    ~|(%sift-lose ?>((nest(sut ref) & -:!>(*typo)) ref))
  ::
  ++  snub
    ~/  %snub
    |=  har=(list ,[p=wing q=twig])
    ^-  (list ,[p=wing q=twig])
    (turn har |=([a=wing b=twig] [(flop a) b]))
  ::
  ++  tack
    ~/  %tack
    |=  [peh=wing mur=type]
    =+  axe=1
    |-  ^-  [p=axis q=type]
    ?~  peh
      [axe mur]
    =>  .(i.peh ?^(i.peh i.peh [%| p=0 q=i.peh]))
    ?-    i.peh
        [& *]
      =+  vas=(peek %rite p.i.peh)
      =+  gav=$(peh t.peh, sut vas, axe (peg axe p.i.peh))
      [p.gav (heal ~ p.i.peh q.gav)]
    ::
        [| *]
      =+  wuf=(flay (fink p.i.peh %rite q.i.peh))
      =+  gav=$(peh t.peh, sut q.wuf, axe (peg axe p.wuf))
      [p.gav (heal [~ q.i.peh] p.wuf q.gav)]
    ==
  ::
  ++  tock
    ~/  %tock
    |=  [peh=wing mur=type men=(list ,[p=type q=foot])]
    ^-  [p=axis q=(list ,[p=type q=foot])]
    =-  [(need p.wib) q.wib]
    ^=  wib
    |-  ^-  [p=(unit axis) q=(list ,[p=type q=foot])]
    ?@  men
      [*(unit axis) ~]
    =+  geq=(tack(sut p.i.men) peh mur)
    =+  mox=$(men t.men)
    [(mate p.mox `_p.mox`[~ p.geq]) [[q.geq q.i.men] q.mox]]
  ::
  ++  wrap
    ~/  %wrap
    |=  yoz=?(%lead %iron %zinc)
    ^-  type
    ?-  sut
      [%core *]  ?.(=(%gold p.q.sut) ~|(%wrap-metl !!) sut(p.q yoz))
      [%fork *]  (fork $(sut p.sut) $(sut q.sut))
      [%hold *]  $(sut repo)
      *          ~|(%wrap-type !!)
    ==
  --
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 2fD, grammar                  ::
::
++  vang
  |=  [bug=? wer=path]
  %*(. vast bug bug, wer wer)
::
++  vast
  =+  [bug=`?`| was=*(set path) wer=*path]
  |%
  ++  gash  %+  cook
              |=  a=(list tyke)  ^-  tyke
              ?~(a ~ (weld i.a $(a t.a)))
            (more fas gasp)
  ++  gasp  ;~  pose
              %+  cook
                |=([a=tyke b=tyke c=tyke] :(weld a b c))
              ;~  plug
                (cook |=(a=(list) (turn a |=(b=* ~))) (star tis))
                (cook |=(a=twig [[~ a] ~]) hasp)
                (cook |=(a=(list) (turn a |=(b=* ~))) (star tis))
              ==
              (cook |=(a=(list) (turn a |=(b=* ~))) (plus tis))
            ==
  ++  glam  ~+((glue ace))
  ++  hasp  ;~  pose
              (ifix [sel ser] wide)
              (stag %cnhp (ifix [pel per] (most ace wide)))
              %+  cook
                |=(a=coin [%dtzy ?:(?=([~ %tas *] a) %tas %ta) ~(rent co a)])
              nuck:so
            ==
  ++  mota  %+  cook
              |=([a=tape b=tape] (rap 3 (weld a b)))
            ;~(plug (star low) (star hig))
  ::
  ++  plex
    |=  gen=twig  ~|  [%plex gen]  ^-  path
    ?:  ?=([%zpcb *] gen)
      $(gen q.gen)
    ?>  ?=([%clsg *] gen)
    (turn p.gen |=(a=twig ?>(?=(%dtzy -.a) q.a)))
  ::
  ++  pray
    |=  gen=twig  ~|  %pray  ^-  twig
    =+  rev=(plex gen)
    ?:  (~(has in was) rev)
      ~|(%pray-loop !!)
    =+  txt=(,@ta .^(%cx (weld rev `path`[%hoon ~])))
    %+  rash  txt
    (ifix [gay gay] tall(was (~(put in was) rev), wer rev))
  ::
  ++  prey
    |=  gun=(list twig)  ^-  twig
    ?~  gun    [~ 1]
    ?~  t.gun  (pray i.gun)
    [%tsgr (pray i.gun) $(gun t.gun)]
  ::
  ++  phax
    |=  ruw=(list (list beer))
    =+  [yun=*(list twig) cah=*(list ,@)]
    =+  wod=|=([a=tape b=(list twig)] ^+(b ?~(a b [[%clfs %smdq (flop a)] b])))
    |-  ^+  yun
    ?~  ruw
      (flop (wod cah yun))
    ?~  i.ruw  $(ruw t.ruw)
    ?@  i.i.ruw
      $(i.ruw t.i.ruw, cah [i.i.ruw cah])
    $(i.ruw t.i.ruw, cah ~, yun [p.i.i.ruw (wod cah yun)])
  ::
  ++  posh
    |=  [pre=(unit tyke) pof=(unit ,[p=@ud q=tyke])]
    ^-  (list twig)
    ~|  %posh-fail
    =+  wom=(poof wer)
    =+  ^=  yez
        ?~  pre  wom
        =+  moz=(poon wom u.pre)
        ?~(pof moz (weld moz (slag (lent u.pre) wom)))
    ?~  pof  yez
    =+  zey=(flop yez)
    =+  [moz=(scag p.u.pof zey) gul=(slag p.u.pof zey)]
    (weld (flop gul) (poon (flop moz) q.u.pof))
  ::
  ++  poof  |=(pax=path ^-(tusk (turn pax |=(a=@ta [%dtzy %ta a]))))
  ++  poon
    |=  [pag=tusk goo=tyke]
    ^-  tusk
    ?~  goo  ~
    :-  ?^(i.goo u.i.goo ?>(?=(^ pag) i.pag))
    $(goo t.goo, pag ?~(pag ~ t.pag))
  ::
  ++  poor
    %+  cook  posh
    ;~  plug
      (stag ~ gash)
      ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
    ==
  ::
  ++  porc
    ;~  plug
      (cook |=(a=(list) (lent a)) (star cen))
      ;~(pfix fas gash)
    ==
  ::
  ++  rood
    ;~  pfix  fas
      (stag %clsg poor)
    ==
  ++  scat
    %+  knee  *twig  |.  ~+
    %-  stew  
    ^.  stet  ^.  limo
    :~
      :-  '!'
        ;~  pose
          (stag %wtzp ;~(pfix zap wide))
          (stag %zpzp (cold ~ ;~(plug zap zap)))
          (stag %zpcn (cold ~ ;~(plug zap cen)))
        ==
      :-  '$'
        (cook |=(a=wing [%cnts a ~]) rope)
      :-  '%'
        ;~  pfix  cen
          ;~  pose
            (cook |=([a=@ud b=tyke] [%clsg (posh ~ ~ a b)]) porc)
            (stag %dtzz (stag %tas (cold %$ buc)))
            (stag %dtzz (stag %f (cold & pam)))
            (stag %dtzz (stag %f (cold | bar)))
            (stag %dtzz (stag %ta qut))
            (stag %clcn (ifix [sel ser] (most ace wide)))
            (cook (jock &) nuck:so)
            (cook |=(a=(list) [%clsg (posh ~ ~ (lent a) ~)]) (star cen))
            ::  (easy [%clsg (poof wer)])
          ==
        ==
      :-  '&'
        ;~  pose
          (cook |=(a=wing [%cnts a ~]) rope)
          (stag %wtpm ;~(pfix pam (ifix [pel per] (most ace wide))))
          (stag %dtzy (stag %f (cold & pam)))
        ==
      :-  '\''
        (stag %dtzy (stag %t qut))
      :-  '('
        (stag %cnhp (ifix [pel per] (most ace wide)))
      :-  '*'
        ;~  pose
          (stag %bctr ;~(pfix tar hill))
        ==
      :-  '+'
        ;~  pose
          (stag %dtls ;~(pfix lus (ifix [pel per] wide)))
        ::
          %+  cook
            |=  a=(list (list beer))
            :-  %clfs
            [%smdq |-(?~(a ~ (weld i.a $(a t.a))))]
          (most dog ;~(pfix lus soil))
        ::
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  '-'
        ;~  pose
          (stag %dtzy tash:so)
        ::
          %+  cook
            |=  a=(list (list beer))
            [%clzz (phax a)]
          (most dog ;~(pfix hep soil))
        ::
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  '.'
        ;~  pose
          %+  cook
            |=  a=coin  ^-  twig
            ?-  -.a
              ~      [%dtzy p.a]
              %blob  [%dtzz %$ p.a]
              %many  [%cltr (turn p.a |=(b=coin ^$(a b)))]
            ==
          ;~(pfix dot perd:so)
        ::
          (cook |=(a=wing [%cnts a ~]) rope)
        ==
      :-  ['0' '9']
        (stag %dtzy bisk:so)
      :-  ':'
        (stag %smcl ;~(pfix col (ifix [pel per] (most ace wide))))
      :-  '='
        (stag %dtts ;~(pfix tis (ifix [pel per] ;~(glam wide wide))))
      :-  '?'
        (stag %bccm (stag %fern ;~(pfix wut (ifix [pel per] (most ace toil)))))
      :-  '['
        %+  stag
          %cltr
        ;~  pfix  sel
          %+  cook
            |=  [a=(list twig) b=?(~ [~ ~])]
            ?~(b a (weld a `_a`[[%bczp %null] ~]))
          ;~  plug
            ;~  pose
              (ifix [ace gap] (most gap tall))
              (most ace wide)
            ==
            ;~  pose
              (cold [~ ~] ;~(plug (just ']') (just '~')))
              (cold ~ (just ']'))
            ==
          ==
        ==
      :-  ','
        (stag %bccm ;~(pfix com hill))
      :-  '^'
        ;~  pose
          ;~  pfix  ket
            ;~  pose
              ;~  pfix  col
                %+  cook
                  |=  [a=tile b=twig]                     ::  XX shd be static
                  =+  rev=(plex b)
                  :+  %smsm  ~(clam al a)
                  [%dtkt %dtzz %$ %cx rev]
                ;~(plug hill rood)
              ==
              (cook prey (most ket rood))
            ==
          ==
          (stag %cnzz rope)
          (stag %bczp (cold %cell ket))
        ==
      :-  '_'
        (stag %bccb ;~(pfix cab hill))
      :-  '`'
        ;~  pfix  tec
          ;~  pose
            %+  cook
              |=([a=@ta b=twig] [%ktls [%dtzy a 0] [%ktls [%dtzy %$ 0] b]])
            ;~(pfix pat ;~(plug mota ;~(pfix tec wide)))
            (stag %kthp ;~(plug toil ;~(pfix tec wide)))
            (stag %ktls ;~(pfix lus ;~(plug wide ;~(pfix tec wide))))
          ==
        ==
      :-  '"'
        %+  cook
          |=  a=(list (list beer))
          [%smdq |-(?~(a ~ (weld i.a $(a t.a))))]
        (most dog soil)
      :-  ['a' 'z']
        %+  sear
          |=  [a=wing b=(unit twig)]  ^-  (unit twig)
          ?~(b [~ %cnzz a] ?.(?=([@ ~] a) ~ [~ [%dtzz %tas i.a] u.b]))
        ;~(plug rope ;~(pose (stag ~ ;~(pfix fas wide)) (easy ~)))
      :-  '|'
        ;~  pose
          (cook |=(a=wing [%cnts a ~]) rope)
          (stag %wtbr ;~(pfix bar (ifix [pel per] (most ace wide))))
          (stag %dtzy (stag %f (cold | bar)))
        ==
      :-  '~'
        ;~  pose
          %+  cook
            |=  a=(list (list beer))
            :_  [%bczp %null]
            :-  %clfs
            [%smdq |-(?~(a ~ (weld i.a $(a t.a))))]
          (most dog ;~(pfix sig soil))
        ::
          ;~  pfix  sig
            ;~  pose
              (stag %clsg (ifix [sel ser] (most ace wide)))
            ::
              %+  stag  %cnsg
              %+  ifix
                [pel per]
              ;~(glam rope wide (stag %cltr (most ace wide)))
            ::
              (cook (jock |) twid:so)
              (easy [%bczp %null])
            ==
          ==
        ==
      :-  '/'
        rood
      :-  '<'
        (ifix [gal gar] (stag %hxgl (most ace wide)))
      :-  '>'
        (ifix [gar gal] (stag %hxgr (most ace wide)))
    ==
  ++  soil
    %+  ifix
      [doq doq]
    %-  star
    ;~  pose
      ;~(pfix bas ;~(pose bas doq kel bix:ab))
      ;~  pose
        (shim 32 33)
        (shim 35 91)
        (shim 93 122)
        (shim 124 126)
        (shim 128 255)
      ==
      (stag ~ (ifix [kel ker] (stag %cltr (most ace wide))))
    ==
  ++  noil
    |=  tol=?
    =<  ;~  pfix  buc
          %-  stew  
          ^.  stet  ^.  limo
          :~
            ['|' (rung bar %reed exqb)]
            ['&' (rung pam %bush exqb)]
            ['?' (rung wut %fern exqc)]
            ['_' (rung cab %weed exqd)]
            ['^' (rung ket %herb exqd)]
            ['=' (rung tis %bark exqe)]
            :-  '+'
              %+  cook
                |=([a=tile b=tile] [%weed [%brls a [%bccb b]]])
              ;~(pfix lus (toad exqb))
            :-  '%'
              ;~  pfix  cen
                %+  sear
                  |=  a=(list tile)  ^-  (unit tile)
                  =-  ?~(b ~ ?~(u.b ~ [~ %kelp i.u.b t.u.b]))
                  ^=  b
                  |-  ^-  (unit (list line))
                  ?~  a  [~ ~]
                  =+  c=$(a t.a)
                  ?~  c  ~
                  ?.  ?=([[%leaf *] *] i.a)  ~
                  [~ [p.i.a q.i.a] u.c]
                (toad exqc)
              ==
            :-  ':'
              ;~  pfix  col 
                %+  cook
                  |=(a=(list tile) ?~(a !! ?~(t.a i.a [i.a $(a t.a)])))
                (toad exqc)
              ==
          ==
        ==
    |%
    ++  toad
      |*  har=_exqa
      =+  dur=(ifix [pel per] $:har(tol |))
      ?:(tol ;~(pose ;~(pfix gap $:har(tol &)) dur) dur)
    ::
    ++  rung
      |*  [dif=_rule tuq=* har=_exqa]
      ;~(pfix dif (stag tuq (toad har)))
    ::
    ++  gunk  ~+((glue muck))
    ++  muck  ?:(tol gap ace)
    ++  butt  |*(zor=_rule ?:(tol ;~(sfix zor ;~(plug gap duz)) zor))
    ++  loaf  ?:(tol howl toil)
    ++  lobe  ?:(tol tall wide)
    ++  exqa  |.(loaf)
    ++  exqb  |.(;~(gunk loaf loaf))
    ++  exqc  |.((butt (most muck loaf)))
    ++  exqd  |.(lobe)
    ++  exqe  |.(;~(gunk sym loaf))
    --
  ++  norm
    |=  tol=?
    =<  %-  stew
        ^.  stet  ^.  limo
        :~  :-  '|'
              ;~  pfix  bar
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['_' (rune cab %brcb expu)]
                    ['%' (rune cen %brcn expe)]
                    ['.' (rune dot %brdt expa)]
                    ['/' (rune fas %brfs expu)]
                    ['-' (rune hep %brhp expa)]
                    ['^' (rune ket %brkt expr)]
                    ['+' (rune lus %brls expo)]
                    ['*' (rune tar %brtr expo)]
                    ['=' (rune tis %brts expo)]
                    ['?' (rune wut %brwt expa)]
                ==
              ==
            :-  '%'
              ;~  pfix  cen
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['_' (rune cab %cncb exph)]
                    [':' (rune col %cncl expb)]
                    ['.' (rune dot %cndt expb)]
                    ['^' (rune ket %cnkt expd)]
                    ['+' (rune lus %cnls expc)]
                    ['-' (rune hep %cnhp expk)]
                    ['~' (rune sig %cnsg expq)]
                    ['*' (rune tar %cntr expm)]
                    ['=' (rune tis %cnts exph)]
                ==
              ==
            :-  '$'
              ;~  pose
                (rune cab %bccb expv)
                (rune com %bccm expv)
                (stag %bccm (noil tol))
              ==
            :-  ':'
              ;~  pfix  col
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['_' (rune cab %clcb expb)]
                    ['~' (rune cen %clcn exps)]
                    ['/' (rune fas %clfs expa)]
                    ['^' (rune ket %clkt expd)]
                    ['+' (rune lus %clls expc)]
                    ['-' (rune hep %clhp expb)]
                    ['~' (rune sig %clsg exps)]
                    ['*' (rune tar %cltr exps)]
                ==
              ==
            :-  '.'
              ;~  pfix  dot
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['+' (rune lus %dtls expa)]
                    ['*' (rune tar %dttr expb)]
                    ['=' (rune tis %dtts expb)]
                    ['?' (rune wut %dtwt expa)]
                    ['^' (rune ket %dtkt expn)]
                ==
              ==
            :-  '^'
              ;~  pfix  ket
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %ktbr expa)]
                    ['.' (rune dot %ktdt expb)]
                    ['-' (rune hep %kthp expo)]
                    ['+' (rune lus %ktls expb)]
                    ['&' (rune pam %ktpm expa)]
                    ['~' (rune sig %ktsg expa)]
                    ['=' (rune tis %ktts expg)]
                    ['?' (rune wut %ktwt expa)]
                ==
              ==
            :-  '~'
              ;~  pfix  sig
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %sgbr expb)]
                    ['$' (rune buc %sgbc expg)]
                    ['_' (rune cab %sgcb expb)]
                    ['%' (rune cen %sgcn hind)]
                    ['/' (rune fas %sgfs hine)]
                    ['<' (rune gal %sggl hinb)]
                    ['>' (rune gar %sggr hinb)]
                    ['+' (rune lus %sgls hinc)]
                    ['&' (rune pam %sgpm hinf)]
                    ['?' (rune wut %sgwt hing)]
                    ['=' (rune tis %sgts expb)]
                    ['!' (rune zap %sgzp expb)]
                ==
              ==
            :-  ';'
              ;~  pfix  sem
                %-  stew  
                ^.  stet  ^.  limo
                :~  [':' (rune col %smcl expi)]
                    ['.' (rune dot %smdt expi)]
                    ['~' (rune sig %smsg expi)]
                    [';' (rune sem %smsm expb)]
                ==
              ==
            :-  '='
              ;~  pfix  tis
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %tsbr expo)]
                    ['.' (rune dot %tsdt expq)]
                    ['^' (rune ket %tskt expd)]
                    [':' (rune col %tscl expp)]
                    ['<' (rune gal %tsgl expb)]
                    ['>' (rune gar %tsgr expb)]
                    ['-' (rune hep %tshp expb)]
                    ['*' (rune tar %tstr expj)]
                    ['+' (rune lus %tsls expb)]
                    ['~' (rune sig %tssg expi)]
                ==
              ==
            :-  '?'
              ;~  pfix  wut
                %-  stew  
                ^.  stet  ^.  limo
                :~  ['|' (rune bar %wtbr exps)]
                    [':' (rune col %wtcl expc)]
                    ['.' (rune dot %wtdt expc)]
                    ['<' (rune gal %wtgl expb)]
                    ['>' (rune gar %wtgr expb)]
                    ['-' (rune hep %wthp expx)]
                    ['^' (rune ket %wtkz expf)]
                    ['=' (rune tis %wtts expw)]
                    ['+' (rune lus %wtls expy)]
                    ['&' (rune pam %wtpm exps)]
                    ['@' (rune pat %wtpz expf)]
                    ['~' (rune sig %wtsz expf)]
                    ['!' (rune zap %wtzp expa)]
                ==
              ==
            :-  '!'
              ;~  pfix  zap
                %-  stew  
                ^.  stet  ^.  limo
                :~  [':' ;~(pfix col (toad expz))]
                    [',' (rune com %zpcm expb)]
                    [';' (rune sem %zpsm expb)]
                    ['^' ;~(pfix ket (cook prey (toad exps)))]
                    ['>' (rune gar %zpgr expa)]
                    ['=' (rune tis %zpts expa)]
                    ['?' (rune wut %zpwt hinh)]
                ==
              ==
        ==
    |%
    ++  boog
      %+  knee  [p=*term q=*foot]  |.  ~+
      ;~  pfix  lus
        ;~  pose
          %+  cook
            |=([a=%ash b=term c=twig] [b a c])
          ;~  gunk
            (cold %ash (just '+'))
            ;~(pose (cold %$ buc) sym)
            loaf
          ==
        ::
          %+  cook
            |=([a=%elm b=term c=twig] [b a c])
          ;~  gunk
            (cold %elm (just '-'))
            ;~(pose (cold %$ buc) sym)
            loaf
          ==
        ::
          %+  cook
            |=([a=%oak b=term] [b a ~])
          ;~  gunk
            (cold %oak (just '|'))
            ;~(pose (cold %$ buc) sym)
          ==
        ==
      ==
    ::
    ++  wisp
      %-  ulva
      %+  cook
        |=(a=(list ,[p=term q=foot]) (~(gas by *(map term foot)) a))
      (most muck boog)
    ::
    ++  toad
      |*  har=_expa
      =+  dur=(ifix [pel per] $:har(tol |))
      ?:(tol ;~(pose ;~(pfix gap $:har(tol &)) dur) dur)
    ::
    ++  rune
      |*  [dif=_rule tuq=* har=_expa]
      ;~(pfix dif (stag tuq (toad har)))
    ::
    ++  glop  ~+((glue mash))
    ++  gunk  ~+((glue muck))
    ++  butt  |*(zor=_rule ?:(tol ;~(sfix zor ;~(plug gap duz)) zor))
    ++  ulva  |*(zor=_rule ?.(tol fail ;~(sfix zor ;~(plug gap dun))))
    ++  neck  ;~(pose duz ;~(pfix ;~(plug duq gap) wisp))
    ++  hank  (most muck loaf)
    ++  loaf  ?:(tol tall wide)
    ++  lobe  ?:(tol howl toil)
    ++  mash  ?:(tol gap ;~(plug com ace))
    ++  muck  ?:(tol gap ace)
    ++  teak  %+  knee  *tiki  |.  ~+
              =+  ^=  gub
                  |=  [a=term b=$%([& p=wing] [| p=twig])]
                  ^-  tiki
                  ?-(-.b %& [%& [~ a] p.b], %| [%| [~ a] p.b])
              =+  ^=  wyp
                  ;~  pose  
                     %+  cook  gub
                     ;~  plug  
                       sym
                       ;~(pfix tis ;~(pose (stag %& rope) (stag %| wide)))
                     ==
                  ::
                     (stag %& (stag ~ rope))
                     (stag %| (stag ~ wide))
                  ==
              ?.  tol  wyp
              ;~  pose
                wyp
              ::
                ;~  pfix
                  ;~(plug ket tis gap)
                  %+  cook  gub
                  ;~  plug  
                    sym
                    ;~(pfix gap ;~(pose (stag %& rope) (stag %| tall)))
                  ==
                ==
              ::
                (stag %| (stag ~ tall))
              ==
    ++  race  (most mash ;~(gunk lobe loaf))
    ++  rack  (most mash ;~(gunk loaf loaf))
    ++  rick  (most mash ;~(gunk rope loaf))
    ++  expa  |.(loaf)
    ++  expb  |.(;~(gunk loaf loaf))
    ++  expc  |.(;~(gunk loaf loaf loaf))
    ++  expd  |.(;~(gunk loaf loaf loaf loaf))
    ++  expe  |.(wisp)
    ++  expf  |.(;~(gunk teak loaf loaf))
    ++  expg  |.(;~(gunk sym loaf))
    ++  exph  |.((butt ;~(gunk rope rick)))
    ++  expi  |.((butt ;~(gunk loaf hank)))
    ++  expj  |.(;~(gunk sym rope loaf))
    ++  expk  |.(;~(gunk loaf ;~(plug loaf (easy ~))))
    ++  expm  |.((butt ;~(gunk rope loaf rick)))
    ++  expn  |.((stag %cltr (butt hank)))
    ++  expo  |.(;~(gunk lobe loaf))
    ++  expp  |.(;~(gunk (butt rick) loaf))
    ++  expq  |.(;~(gunk rope loaf loaf))
    ++  expr  |.(;~(gunk loaf wisp))
    ++  exps  |.((butt hank))
    ++  expt  |.((butt ;~(gunk loaf race)))
    ++  expu  |.(;~(gunk lobe wisp))
    ++  expv  |.(lobe)
    ++  expw  |.(;~(gunk lobe rope))
    ++  expx  |.((butt ;~(gunk rope race)))
    ++  expy  |.((butt ;~(gunk rope loaf race)))
    ++  expz  |.(loaf(bug &))
    ++  hina  |.(;~(gunk (ifix [sel ser] ;~(gunk dem dem)) loaf))
    ++  hinb  |.(;~(gunk bont loaf))
    ++  hinc  |.(;~(pose ;~(gunk bony loaf) ;~(plug (easy ~) loaf)))
    ++  hind  |.(;~(gunk bonk loaf bonz loaf))
    ++  hine  |.(;~(gunk bonk loaf))
    ++  hinf  |.
      ;~  pose
        ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf)
        (stag 0 ;~(gunk loaf loaf))
      ==
    ++  hing  |.
      ;~  pose
        ;~(gunk (cook lent (stun [1 3] gar)) loaf loaf loaf)
        (stag 0 ;~(gunk loaf loaf loaf))
      ==
    ++  bonk
      ;~  pfix  cen
        ;~  pose
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot ;~(pfix dot dem)))))
          ;~(plug sym ;~(pfix col ;~(plug sym ;~(pfix dot dem))))
          ;~(plug sym ;~(pfix dot dem))
          sym
        ==
      ==
    ++  hinh  |.
        ;~  gunk 
          ;~  pose 
            dem 
            (ifix [sel ser] ;~(plug dem ;~(pfix ace dem)))
          ==
          loaf
        ==
    ++  bont  ;~  (bend) 
                ;~(pfix cen sym) 
                ;~(pfix dot ;~(pose wide ;~(pfix muck loaf)))
              ==
    ++  bony  (cook |=(a=(list) (lent a)) (plus tis))
    ++  bonz
      ;~  pose
        (cold ~ sig)
        %+  ifix
          ?:(tol [;~(plug duz gap) ;~(plug gap duz)] [pel per])
        (more mash ;~(gunk ;~(pfix cen sym) loaf))
      ==
    --
  ::
  ++  lung
    ~+
    %-  bend
    |=  $:  ros=twig
            $=  vil
            $%  [%tis p=twig]
                [%col p=twig]
                [%ket p=twig]
                [%pat p=tile]
                [%pel p=tray]
            ==
        ==
    ^-  (unit twig)
    ?-    -.vil
        %tis  [~ %ktts ~(hock ap ros) p.vil]
        %col  [~ %tsgl ros p.vil]
        %pel  [~ %cnts ~(rake ap ros) p.vil]
        %pat  [~ %bcpt ~(rake ap ros) p.vil]
        %ket  [~ ros p.vil]
    ==
  ::
  ++  long
    %+  knee  *twig  |.  ~+
    ;~  lung
      scat
      ;~  pose
        ;~(plug (cold %tis tis) wide)
        ;~(plug (cold %col col) wide)
        ;~(plug (cold %ket ket) wide)
        ;~(plug (cold %pat pat) hill)
        ;~  plug
          (easy %pel)
          (ifix [pel per] lobo)
        ==
      ==
    ==
  ::
  ++  lobo  (most ;~(plug com ace) ;~(glam rope wide))
  ++  loon  (most ;~(plug com ace) ;~(glam wide wide))
  ++  lute
    ~+
    %+  stag  %cltr
    %+  ifix
      [;~(plug sel gap) ;~(plug gap ser)]
    (most gap tall)
  ::
  ++  rope
    %+  knee  *wing
    |.  ~+
    %+  (slug `wing`~ |=([a=wing b=wing] (weld a b)))
      dot
    ;~  pose
      %+  cook
        |=([a=(list) b=term] [?~(a b [%| (lent a) b]) ~])
      ;~(plug (star ket) ;~(pose sym (cold %$ buc)))
    ::
      %+  cook
        |=(a=limb [a ~])
      %+  cook
        |=(a=axis [%& a])
      ;~  pose
        ;~(pfix lus dim:ag)
        ;~(pfix pam (cook |=(a=@ ?:(=(0 a) 0 (mul 2 +($(a (dec a)))))) dim:ag))
        ;~(pfix bar (cook |=(a=@ ?:(=(0 a) 1 +((mul 2 $(a (dec a)))))) dim:ag))
        ven
        (cold 1 dot)
      ==
    ==
  ::
  ++  tall  (knee *twig |.(~+((wart ;~(pose (norm &) long lute)))))
  ++  wide  (knee *twig |.(~+((wart ;~(pose (norm |) long)))))
  ++  hill  (knee *tile |.(~+(;~(pose (noil |) toil))))
  ++  howl  (knee *tile |.(~+(;~(pose (noil &) toil))))
  ++  toil
    %+  knee  *tile  |.  ~+
    %-  stew  
    ^.  stet  ^.  limo
    :~
      :-  '%'
        ;~  pfix  cen
          ;~  pose
            (stag %leaf (stag %tas (cold %$ buc)))
            (stag %leaf (stag %f (cold & pam)))
            (stag %leaf (stag %f (cold | bar)))
            (stag %leaf (stag %ta qut))
            %+  cook
              |=  lot=coin  ^-  tile
              ?-    -.lot
                  ~   [%leaf p.lot]
                  %blob
                ?@(p.lot [%leaf %$ p.lot] [$(p.lot -.p.lot) $(p.lot +.p.lot)])
              ::
                  %many
                ?~(p.lot [%leaf %n ~] [$(lot i.p.lot) $(p.lot t.p.lot)])
              ==
            nuck:so
          ==
        ==
      :-  '&'
        (stag %leaf (stag %f (cold & pam)))
      :-  '*'
        (cold [%axil %noun] tar)
      :-  '?'
        ;~  pose
          (stag %fern ;~(pfix wut (ifix [pel per] (most ace toil))))
          (stag %axil (cold %bean wut))
        ==
      :-  '@'
        ;~(pfix pat (stag %axil (stag %atom mota)))
      :-  '^'
        ;~  pose
          (stag %herb (stag %cnzz rope))
          (cold [%axil %cell] ket)
        ==
      :-  '('
        (stag %herb wide)
      :-  '.' 
        (stag %herb (stag %cnzz rope))
      :-  '['
        %+  ifix  [sel ser]
        %+  cook
          |=  a=(list tile)
          ?~(a !! ?~(t.a i.a [i.a $(a t.a)]))
        (most ace toil)   
      :-  '_'
        (stag %weed ;~(pfix cab wide))
      :-  ['0' '9']
        (stag %leaf bisk:so)
      :-  ['a' 'z']
        ;~  pose
          (stag %bark ;~(plug sym ;~(pfix tis toil)))
          (stag %herb wide)
        ==
      :-  '$'
        ;~  pose
          (noil |)
          (stag %herb wide)
        ==
      :-  '|'
        (stag %leaf (stag %f (cold | bar)))
      :-  '~'
        (stag %leaf (stag %n (cold ~ sig)))
    == 
  ++  wart
    |*  zor=_rule
    %+  here
      |=  [a=pint b=twig]
      ?:(bug [%zpcb [wer a] b] b)
    zor
  --
::
++  vest
  ~/  %vest
  |=  tub=nail
  ~|  %vest
  ^-  (like twig)
  %.  tub
  %-  full
  (ifix [gay gay] tall:vast)
::
++  vice
  |=  txt=@ta
  ^-  twig
  (rash txt wide:vast)
--
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    volume 3, Arvo models and skeleton    ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
|%
++  curd  ,[p=@tas q=*]                                 ::  typeless card
++  duct  (list wire)                                   ::  causal history
++  helm                                                ::  privilege
          $|  ?(%gold %iron)                            ::  root, user
          $%  [%lead p=ship]                            ::  foreign
          ==                                            ::
++  hilt  ?(0 1 2)                                      ::  lead iron gold
++  move  ,[p=(unit writ) q=duct r=curd]                ::  typeless move
++  ovum  ,[p=wire q=curd]                              ::  typeless ovum
++  pane  (list ,[p=@tas q=vase])                       ::  kernel modules
++  pone  (list ,[p=@tas q=vise])                       ::  kernel modules, old
++  ship  ,@p                                           ::  network identity
++  vane  $_                                            ::  kernel actor
          |+  [now=@da eny=@ sky=$+(* (unit))]          ::  activate
          ^?  |%                                        ::
              ++  beat                                  ::  update
                        |=  $:  wru=(unit writ)         ::  calling identity
                                pax=wire                ::  pretext
                                hen=duct                ::  channel
                                fav=curd                ::  event
                            ==                          ::
                        :-  p=*(list move)              ::  actions
                        q=*vane                         ::  consequence
              ++  come                                  ::  load state, stale
                        |=  [sam=? old=vase]            ::
                        *vane                           ::
              ++  doze                                  ::  next wakeup
                        |=  [now=@da hen=duct]          ::  channel
                        *(unit ,@da)                    ::  alarm if any
              ++  load                                  ::  load state, fresh
                        |=  old=vase                    ::
                        *vane                           ::
              ++  raze  *vane                           ::  erase all state
              ++  scry                                  ::  inspect
                        |=  $:  our=ship                ::  observer
                                ren=@tas                ::  submode
                                his=ship                ::  target
                                syd=@tas                ::  project
                                lot=coin                ::  version
                                tyl=path                ::  location
                            ==                          ::
                        *(unit)                         ::  record
              ++  stay  *vase                           ::  save state, new
              ++  vern  [_@ud _@ud]                     ::  hoon/vane version
              --                                        ::
++  vile                                                ::  reflexive constants
          $:  bet=type                                  ::  beat
              nim=type                                  ::  scry
              vin=type                                  ::  vane
              hoz=type                                  ::  doze
              viz=type                                  ::  vane
          ==
++  wire  path                                          ::  event pretext
++  writ  ,[p=helm q=ship]                              ::  authority
::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
::                section 3bE, Arvo core                ::
::
::
::  ++  able                                            ::  simplify privilege
::    |=  hem=helm  ^-  hilt
::    ?-(hem %gold 2, %iron 1, [%lead *] 0)
::
++  adit                                                ::  duct privilege
  |=  hen=duct
  ^-  ?(%gold %iron %lead)
  ?~  hen
    %lead
  ?~  t.hen
    ?:  ?=([%gold *] i.hen)  %gold
    ?:  ?=([%iron *] i.hen)  %iron
    %lead
  $(hen t.hen)
::
++  vent                                                ::  vane core
  !:
  |=  [vil=vile bud=vase ves=vase]
  |%
  ++  ruck                                              ::  update vase
    |=  [pax=path txt=@ta]
    ^+  +>
    =+  arg=`vase`[vin.vil ~2000.1.1 0 =>(~ |+(* ~))]
    =+  rig=(slam ves arg)
    =+  rev=(slam (slap bud (rain pax txt)) `vase`[viz.vil bud])
    =+  syg=(slam rev arg)
    =+  ole=(slap rig [%cnzy %stay])
    =+  [vol=(slap rig [%cnzy %vern]) vew=(slap syg [%cnzy %vern])]
    ?>  =(stub -.q.vew)
    ?:  =(q.vol q.vew)
      +>.$(ves (slam (slap syg [%cnzy %load]) ole))
    =+  ^=  sam
        ?:  =(stub -.q.vol)
          ?>(=(+.q.vew +(((hard ,@) +.q.vol))) &)
        ?>(&(=(0 +.q.vew) =(-.q.vol +(stub))) |)
    %=  +>.$
      ves  (slam (slap syg [%cnzy %come]) (slop [[%cube sam %atom %f] sam] ole))
    ==
  ::
  ++  wink                                              ::  deploy
    |=  [now=@da eny=@ sky=$+(* (unit))]
    =+  arg=`vase`[vin.vil +<]
    =+  rig=(slam ves arg)
    |%
    ++  beat
      |=  $:  wru=(unit writ)
              pax=wire
              hen=duct
              fav=curd
          ==
      ^-  [p=(list move) q=_+>.^$]
      =+  pro=(slam (slap rig [%cnzy %beat]) [bet.vil +<])
      :-  ((hard (list move)) q:(slap pro [%cnzy %p]))
      =+  sev=(slap pro [%cnzy %q])
      %=    +>.^$
          ves
        ?:  &(=(-.q.ves -.q.sev) =(+>.q.ves +>.q.sev))
          ves
        sev(+<.q [_@da _@ =>(~ |+(* ~))])                 ::  cure memory leak
      ==
    ::
    ++  doze
      |=  [now=@da hen=duct]
      ^-  (unit ,@da)
      ((hard (unit ,@da)) q:(slam (slap rig [%cnzy %doze]) [hoz.vil +<]))
    ::
    ++  scry
      |=  $:  our=ship
              ren=@tas
              his=ship
              syd=@tas
              lot=coin
              tyl=path
          ==
      =+  pro=(slam (slap rig [%cnzy %scry]) [nim.vil +<])
      ((hard (unit)) q.pro)
    --
  --
::
++  vial                                                ::  vane tools
  |=  but=type
  ^-  vile
  =+  pal=|=(a=@t ^-(type (~(play ut but) (vice a))))
  :*  bet=(pal '_[(unit writ) wire duct curd]')
      nim=(pal '_[ship @tas ship @tas coin path]')
      vin=(pal '_[@da @ $+(* (unit))]')
      hoz=(pal '_[@da duct]')
      viz=(pal '_vase')
  ==
::
++  vint                                                ::  create vane
  |=  [vil=vile bud=vase pax=path txt=@ta]              ::
  (vent vil bud (slam (slap bud (rain pax txt)) [viz.vil bud]))
::
++  is                                                  ::  operate in time
  |=  [eny=@ vil=vile bud=vase fan=(list ,[p=@tas q=vase])]
  |_  now=@da
  ++  beck
    |=  wru=(unit writ)
    |+  hap=*
    ^-  (unit)
    =>  .(hap ((hard path) hap))
    ?.  ?=([@ @ @ @ *] hap)  ~
    =+  :*  hyr=(slay i.hap)
            fal=(slay i.t.hap)
            dyc=(slay i.t.t.hap)
            ved=(slay i.t.t.t.hap)
            ::  ved=(slay i.t.hap)
            ::  fal=(slay i.t.t.hap)
            ::  dyc=(slay i.t.t.t.hap)
            tyl=t.t.t.t.hap
        ==
    ?.  ?=([~ %$ %tas @] hyr)  ~
    ?.  ?=([~ %$ %p @] fal)  ~
    ?.  ?=([~ %$ %tas @] dyc)  ~
    ?.  ?=(^ ved)  ~
    =+  his=`@p`q.p.u.fal
    =>  .(wru ?^(wru wru [~ u=[p=[%lead his] q=his]]))  ::  XX dubious
    =+  dis=(end 3 1 q.p.u.hyr)
    =+  rem=(rsh 3 1 q.p.u.hyr)
    |-  ^-  (unit)
    ?~  fan  ~
    ?.  =(dis p.i.fan)  $(fan t.fan)
    %-  scry:(wink:(vent vil bud q.i.fan) now (shax now) ..^$)
    [q.u.wru rem his q.p.u.dyc u.ved tyl]
  ::
  ++  dink                                              ::  vase by char
    |=  din=@tas  ^-  vase
    ?~(fan !! ?:(=(din p.i.fan) q.i.fan $(fan t.fan)))
  ::
  ++  dint                                              ::  input to vane
    |=  hap=path  ^-  @tas
    ?+  hap  !!
      [@ %ames *]  %a
      [@ %batz *]  %b
      [@ %clay *]  %c
      [@ %sync *]  %c
      [@ %term *]  %d
      [@ %http *]  %e
    ==
  ::
  ++  doos                                              ::  sleep until
    |=  hap=path  ^-  (unit ,@da)
    (doze:(wink:(vent vil bud (dink (dint hap))) now 0 (beck ~)) now [hap ~])
  ::
  ++  hurl                                              ::  start loop no id
    |=  ovo=ovum
    ^-  [p=(list ovum) q=(list ,[p=@tas q=vase])]
    (kick [[~ [[(dint p.ovo) ~] p.ovo ~] q.ovo] ~])
  ::
  ++  hymn                                              ::  start loop with id
    |=  [who=ship ovo=ovum]
    ^-  [p=(list ovum) q=(list ,[p=@tas q=vase])]
    (kick [[[~ %iron who] [[(dint p.ovo) ~] p.ovo ~] q.ovo] ~])
  ::
  ++  kick                                              ::  complete loop
    |=  mor=(list move)
    =|  ova=(list ovum)
    =+  rom=mor
    |-  ^-  [p=(list ovum) q=(list ,[p=@tas q=vase])]
    ?~  mor
      [(flop ova) fan]
    ::  ~&  [%kick-move q.i.mor -.r.i.mor]
    ?.  ?=(^ q.i.mor)
      ~&  [%kick-flat -.r.i.mor]
      ~&  [%kick-rom rom]
      $(mor t.mor)
    ?~  t.q.i.mor
      $(mor t.mor, ova [[i.q.i.mor r.i.mor] ova])
    ?>  ?=(^ i.q.i.mor)
    =-  $(mor (weld p.nyx t.mor), fan q.nyx)
    ^=  nyx
    =+  naf=fan
    |-  ^-  [p=(list move) q=_fan]
    ?~  naf  [~ ~]
    ?.  =(i.i.q.i.mor p.i.naf)
      =+  tuh=$(naf t.naf)
      [p.tuh [i.naf q.tuh]]
    =+  ven=(vent vil bud q.i.naf)
    =+  win=(wink:ven now (shax now) (beck p.i.mor))
    =+  ^=  yub
        %-  beat:win
        [p.i.mor t.i.q.i.mor t.q.i.mor r.i.mor]
    [p.yub [[p.i.naf ves:q.yub] t.naf]]
  --
--
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
::::::  ::::::    Postface                              ::::::
::::::  ::::::::::::::::::::::::::::::::::::::::::::::::::::::
=+  pit=`vase`!>(.)                                     ::
=+  bud=pit                                             ::  becomes tang
=+  vil=(vial p.bud)                                    ::
=|  eny=@                                               ::  entropy
=|  fan=(list ,[p=@tas q=vase])                         ::
=|  hom=path                                            ::  source path 
=|  haz=@uw                                             ::  hash
=<  |%
    ++  come  |=  [@ (list ovum) pone]                  ::  11
              ^-  [(list ovum) _+>]
              ~&  %hoon-come
              =^  rey  +>+  (^come +<)
              [rey +>.$]
    ++  keep  |=(* (^keep ((hard ,[@da path]) +<)))     ::  4
    ++  load  |=  [@ (list ovum) pane]                  ::  86
              ^-  [(list ovum) _+>]
              ~&  %hoon-load
              =^  rey  +>+  (^load +<)
              [rey +>.$]
    ++  peek  |=(* (^peek ((hard ,[@da path]) +<)))     ::  87
    ++  poke  |=  *                                     ::  42
              ^-  [(list ovum) *]
              =>  .(+< ((hard ,[now=@da ovo=ovum]) +<))
              ?:  ?=(%veer -.q.ovo)
                [~ +>.$(+ (veer +.q.ovo))]
              =^  ova  +>+  (^poke now ovo)
              |-  ^-  [(list ovum) *]
              ?~  ova
                [~ +>.^$]
              ?:  ?=(%veer -.q.i.ova)
                $(ova t.ova, +>+.^$ (veer +.q.i.ova))
              ?:  ?=(%vega -.q.i.ova)
                (vega now t.ova (path +.q.i.ova))
              =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
    ++  wish  |=(* (^wish ((hard ,@ta) +<)))            ::  20
    --
|%
++  come                                                ::  load incompatible
  |=  [yen=@ ova=(list ovum) nyf=pone]
  ^+  [ova +>]
  (load yen ova (turn nyf |=([a=@tas b=vise] [a (slim b)])))
::
++  keep                                                ::  wakeup delay
  |=  [now=@da hap=path]
  =>  .(+< ((hard ,[now=@da hap=path]) +<))
  (~(doos (is eny vil bud fan) now) hap)
::
++  load                                                ::  load compatible
  |=  [yen=@ ova=(list ovum) nyf=pane]
  ^+  [ova +>]
  =:  eny  yen
      fan  nyf
    ==
  |-  ^+  [ova +>.^$]
  ?~  ova
    [~ +>.^$]
  ?:  ?=(%veer -.q.i.ova)
    $(ova t.ova, +>.^$ (veer +.q.i.ova))
  =+(avo=$(ova t.ova) [[i.ova -.avo] +.avo])
::
++  peek                                                ::  external inspect
  |=  [now=@da hap=path]
  ^-  (unit)
  ?~  hap  [~ stub]
  ((~(beck (is eny vil bud fan) now) ~) hap)
::
++  poke                                                ::  external apply
  |=  [now=@da ovo=ovum]
  ^-  [(list ovum) _+>]
  =^  zef  fan
    (~(hurl (is eny vil bud fan) now) ovo)
  [zef +>.$]
::
++  vega                                                ::  reboot kernel
  |=  [now=@da ova=(list ovum) hap=path]
  ^-  [p=(list ovum) q=*]
  =+  pax=(weld hap `path`[%hoon ~])
  ~&  [%vega-start hap]
  =+  src=((hard ,@t) (need (peek now cx/pax)))
  =+  saz=(shax src)
  =+  gen=(rain hap src) 
  ~&  %vega-parsed
  =+  ken=.*(0 q:(~(mint ut %noun) %noun gen))
  =+  ^=  nex
      =+  gat=.*(ken .*(ken [0 87]))
      (need ((hard (unit ,@)) .*([-.gat [[now ~] +>.gat]] -.gat)))
  ~&  [%vega-compiled stub nex]
  ?>  (lte nex stub)
  =+  gat=.*(ken .*(ken [0 ?:(=(nex stub) 86 11)]))
  =+  sam=[eny ova fan]
  =+  raw=.*([-.gat [sam +>.gat]] -.gat)
  [((list ovum) -.raw) +.raw]
::
++  veer                                                ::  install vane/tang
  |=  *
  =>  .(+< ((hard ,[lal=@ta pax=path txt=@t]) +<))
  ?:  =(%$ lal)
    ~&  [%tang pax `@p`(mug txt)]
    =+  gen=(rain pax txt)
    =+  vax=(slap pit gen)
    +>.$(bud vax)
  %_    +>
      fan
    |-  ^+  fan
    ?~  fan
      ~&  [%vane `@tas`lal pax `@p`(mug txt)]
      [[lal ves:(vint vil bud pax txt)] fan]
    ?.  =(lal p.i.fan)
      [i.fan $(fan t.fan)]
      ~&  [%vane `@tas`lal pax `@p`(mug txt)]
    [[p.i.fan ves:(ruck:(vent vil bud q.i.fan) pax txt)] t.fan]
  ==
::
++  wish                                                ::  external compute
  |=  txt=@
  q:(slap bud (ream txt))
--
.  ==
