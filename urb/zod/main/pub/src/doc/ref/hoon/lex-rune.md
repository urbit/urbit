Lexicon: Runes
=======

Irregular 
--------

    ,p           $,(p)
    *p           $*(p)
    _p           $_(p)
    p@q          $@(p q)
    !p           ?!(p)
    &(p q)       ?&(p q)
    |(p q)       ?|(p q)
    `p`q         ^-(p q)
    p=q          ^=(p q)
    ~[p q]       :~(a b)
    [p q]~       :~(a)
    `[p]         [~ p]
    p^q          [p q]
    [p q]        :*(p)
    +(p)         .+(p)
    =(p q)       .=(p)
    p:q          =<(p q)
    p(q r)       %=(p q r)
    (p list)     %-(p)
    ~(p q r)     %~(p q r)
    >p<          #<(p)
    <p>          #>(p)
    :(p q)       ;:(p q)

Regular Runes
============

Core construction: `|`
---------------------

    |_     dry %gold door

    Twig:  [%brcb p=tile q=(map term foot)]

    Tall:  |_  p
           ++  p.n.q
             q.n.q
           --

---

    |%     generic %gold core

    Twig:  [%brcn p=(map term foot)]

    Tall:  |%
           ++  p.n.p
             q.n.p
           +-  p.n.l.p
             q.n.l.p
           --

    [Note: dry arms are specified with ++, wet arms with +-] 

---


    |.     dry %gold trap

    Twig:  [%brdt p=twig]

    Tall:  |.  p

    Wide:  |.(p)

---

    |/     vulcanized gold door

    Twig:  [%brfs p=tile q=(map term foot)]

    Tall:  |/  p
           +-  p.n.q
             q.n.q
           --

    [Note: |/ only accepts wet arms with +-] 

---

    |^     kick a %gold book

    Twig:  [%brkt p=twig q=(map term foot)]
    
    Tall:  |^  p
           ++  p.n.q
             q.n.q
           --
---


    |-     kick a %gold trap

    Twig:  [%brhp p=twig]

    Tall:  |-  p

    Wide:  |-(p)

----

    |+     dry %iron gate

    Twig:  [%brls p=tile q=twig]

    Tall:  |+  p  q

    Wide:  |+(p q)

---

    |*     vulcanized wet gate

    Twig:  [%brtr p=tile q=twig]

    Tall:  |*  p  q

    Wide:  |*(p q)

---

    |=     dry %gold gate

    Twig:  [%brts p=tile q=twig]

    Tall:  |=  p  q

    Wide:  |=(p q)

---

    |?     dry %lead trap

    Twig:  [%brwt p=twig]

    Tall:  |?  p

    Wide:  |?(p)

---

Tiles and tiling: `$`
---------------------

    $_     bunt a tile

    Twig:  [%bccb p=tile]

    Tall:  $_  p

    Wide:  $_(p)

    Irrg:  _p

---

    $,     clam a tile

    Twig:  [%bccm p=tile]

    Tall:  $,  p

    Wide:  none

    Irrg:  ,p

---

    $@     whip a wing into a tile
    
    Twig:  [%bcpt p=wing q=tile]

    Tall:  $@  p  q
    
    Wide:  $@(p q)

    Irrg:  p@q

---

    $*     bunt a tile statically

    Twig:  [%bctr p=tile]

    Tall:   $*  p

    Wide:   $*(p)

    Irrg:   *p

---

###Tile runes

    $^     plant a %herb
  
    Tile:  [%herb p=twig]

    Tall:  $^  p

    Wide:  $^(p)

---

    $:     build a tile 

    Tile:  [p=tile q=tile]

    Tall:  $:  p
               q
           ==

    Wide:  [p q]

---

    $=      plant a %bark

    Tile:  [%bark p=term q=tile]
    
    Tall:  $=  p  q

    Wide:  none

    Irrg:  p=q
    
---

    $&     plant a %bush

    Tile:  [%bush p=tile q=tile]

    Tall:  $&  p  q

    Wide:  $&(p q)

    Irrg:  none

---

    $?    plant a %fern 

    Tile:  [%fern p=[i=tile t=(list tile)]]

    Tall:  $?  i.p
               i.t.p
           ==

    Wide:  none

    Irrg:  ?(i.p i.t.p)

---

    $%    plant a %kelp

    Tile:  [%kelp p=[i=line t=(list line)]]          

    Tall:  $%  p 
               q 
           ==

    Wide:  none

    Irrg:  none

---

    $|    plant a %reed

    Tile:  [%reed p=tile q=tile]

    Tall:  $|  p 
               q
           ==

    Wide:  $|(p q)

    Irrg:  none

---

###Axils glyphs

    @     atom axil
    ^     cell axil
    *     noun axil
    ?     bean axil
    ~     null axil

---

Invocations: `%`
---------------

    %_     invoke with changes and cast

    Twig:  [%cncb p=wing q=tram]                  

    Tall:  %_  p
             p.i.q  q.i.q
             p.i.t.q  q.i.t.q
           ==

    Wide:  %_(p p.i.q q.i.q, p.i.t.q q.i.t.q)

---

    %:     pull %$ of a door with a sample
    
    Twig:  [%cncl p=twig q=twig]                  

    Tall:  %:  p  q

    Wide:  %:(p q)

---

    %.    inverse order %-
    
    Twig:  [%cndt p=twig q=twig]                  

    Tall:  %.  p  q

    Wide:  %.(p q)

---

    %-    slam a core with a sample
    
    Twig:  [%cnhp p=twig q=tusk]                  

    Tall:  %-  p  q

    Wide:  %-(p q)

    Irrg:  (p q)

---

    %*     pull wing from tray with changes
    
    Twig:  [%cntr p=wing q=twig r=tram]           
        
    Tall:  %*  p  q
             p.i.r  q.i.r
             p.i.t.r  q.i.t.r
           ==

    Wide:  %*(p q p.i.r q.i.r, p.i.t.r q.i.t.r)

---

    %^     slam gate with triple
    
    Twig:  [%cnkt p=twig q=twig r=twig s=twig]         

    Tall:  %^    p
               q
             r
           s

    Wide:  %^(p q r s)

---

    %+     slam gate with pair
    
    Twig:  [%cnls p=twig q=twig r=twig]                

    Tall:  %+  p
             r
           s

    Wide:  %+(p q r)

---

    %~     pull from tray with sample

    Twig:  [%cnsg p=wing q=twig r=twig]                

    Tall:  %~  p
             q
           r

    Wide:  %~(p q r)

---

    %=     evaluate with changes
    
    Twig:  [%cnts p=wing q=tram]                       

    Tall:  %=  p
             p.i.q    q.i.q
             p.i.t.q  q.i.t.q
           ==

    Wide:  %=(p p.i.q q.i.q, p.i.t.q q.i.t.q)

    Irrg:  p(p.i.q q.i.q, p.i.t.q q.i.t.q)

---

Tuples: `:`
---------

    :_     reverse pair [q p]
    
    Twig:  [%clcb p=twig q=twig]

    Tall:  :_  p  q

    Wide:  :_(p q)

---

    :%   produce [[p ~] ~]

    Twig:  [%clcn p=tusk]

    Tall:  :%  i.p
             i.t.p
             i.t.t.p
           ==

    Wide:  :%(i.p i.t.p i.t.t.p)

---

    :/   produce [%$ [%$ p ~] ~]

    Twig:  [%clfs p=twig]

    Tall:  :/  p

    Wide:  :/(p)

---
      
    :^   produce [p q r s] 

    Twig:  [%clkt p=twig q=twig r=twig s=twig]

    Tall:  :^    p     
               q
             r
           s
        
    Wide:  :^(p q r s)

---

    :-   produce [p q]

    Twig:  [%clhp p=twig q=twig]

    Tall:  :-  p  q

    Wide:  :-(p q)

    Irrg:  [p q]

---

    :+   produce [p q r] 

    Twig:  [%clls p=twig q=twig r=twig]

    Tall:  :+  p
             q
           r

    Wide:  :+(p q r)

---

    :~  produce null-terminated tuple

    Twig:  [%clsg p=tusk]

    Tall:  :~  i.p
             i.t.p
             i.t.t.p
           ==

    Wide:  :~(i.p i.t.p i.t.t.p)
        
    Irrg:  ~[i.p i.t.p i.t.t.p]

---

    :*  produce n-ary tuple

    Twig:  [%cltr p=tusk]

    Tall:  :*  i.p
             i.t.p
             i.t.t.p
           ==

    Wide:  :*(i.p i.t.p i.t.t.p)

    Irrg:  [i.p i.t.p i.t.t.p]

---

Nock operators:  `.`
--------------

    .^  
    .+
    .*
    .=
    .?

Type conversions:  `^`
--------------------

    ^|
    ^.
    ^+
    ^-
    ^&
    ^~
    ^=
    ^?

Miscellaneous macros: `;`
------------------------

    ;:
    ;.
    ;"
    ;~
    ;;

Hints:  `~`
----------

    ~|
    ~_
    ~%
    ~/
    ~<
    ~>
    ~$
    ~+
    ~&
    ~=
    ~?
    ~!

Compositions: `=`
----------------

    =|
    =:
    =%
    =.
    =/
    =>
    =<
    =-
    =^
    =+
    =&
    =@
    =*
    =~

Conditionals, booleans, tests: `?`
---------------------------------

    ?|
    ?-
    ?:
    ?.
    ?^
    ?>
    ?<
    ?+
    ?&
    ?~
    ?=
    ?!

Special operations: `!`
-----------------------

    !:
    !_
    !,
    !%
    !/
    !;
    !?
    !!

Wing Runes
---------

Limbs
  Names
    a
    ^a

  Axes
    .
    +3
    &3
    ->-

Wings: concatenate limbs separated by .

Punctuation runes

++  dry arm
+-  wet arm

==  terminate list
--  terminate map or set



    [%brcb p=tile q=(map term foot)]

|_ ("barcab") is a synthetic rune that produces a %gold tray with
sample p, arms q. q is an associative array of names and
expressions, each pair of which is called an arm. After any number
of dry (%ash, ++) and/or wet (%elm, +-) arms, the array is
terminated with --

        Tall
          |_  p
          ++  p.n.q
            q.n.q
          --

        Wide
          none
      
        Irregular
          none

        Reduction
          See ++open

    [%brcn p=(map term foot)]

|% ("barcen") is a natural rune that produces a %gold core from an
associative array of names and expressions, each pair of which is
called an arm. After any number of dry (%ash, ++) and/or wet (%elm,
+-) arms, the array is terminated with --

        Tall
          |%
          ++  p.n.q
            q.n.q
          +-  p.n.l.q
            q.n.l.q
          --

        Wide
          none

        Irregular
          none

        Reduction
          none, natural

    [%brdt p=twig]

|. ("bardot") is a synthetic rune that produces a dry %gold trap
from twig p.

        Tall
          |.  p

        Wide
          |.(p)

        Irregular
          none

        Reduction
          See ++open

    [%brfs p=tile q=(map term foot)]

|/ ("barfas") is a synthetic rune that produces a vulcanized %gold
tray with arms q, sample p.

        Tall
          |/  p
          +-  p.n.q
            q.n.q
          --

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%brkt p=twig q=(map term foot)]

|^ ("barket") is a synthetic rune that produces a %gold book with
arms q, with p as %$, and kicks it.

        Tall
          |^  p
          ++  p.n.q
            q.n.q
          -- 
     
        Wide
          none

        Irregular
          none

        Reduction
          See ++open 

    [%brhp p=twig]

|- ("barhep") is a synthetic rune that produces a dry %gold trap
from twig p, and kicks it.

        Tall
          |-
          p

        Wide
          |-(p)

        Irregular
          none

        Reduction
          See ++open

    [%brls p=tile q=twig]

|+ ("barlus") is a synthetic rune that produces a dry %iron gate
with arm q, sample p.

        Tall
          |+  p
          q

        Wide
          |+(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brpt p=tile q=tile r=twig]

         XX not used

    [%brtr p=tile q=twig]

|* ("bartar") is a synthetic rune that produces a vulcanized wet
gate with arm q, sample p.

        Tall
          |*  p
          q

        Wide
          |*(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brts p=tile q=twig]

|= ("bartis") is a synthetic hoon that produces a dry %gold gate
with arm q, sample p.

        Tall
          |=  p
          q

        Wide
          |=(p q)

        Irregular
          none

        Reduction
          See ++open

    [%brwt p=twig]

|? ("barwut") is a synthetic rune that produces a dry %lead trap.

        Tall
          |?  p

        Wide
          |?(p)

        Irregular
          none

        Reduction
          See ++open

    [%clcb p=twig q=twig]

:_ ("colcab") is a synthetic rune that produces the cell [q p].

        Tall
          :_  p
          q

        Wide
          :_(p q)

        Irregular
          none

        Reduction
          See ++open

    [%clcn p=tusk]

:% ("colcen") is a synthetic rune that produces a cell [[p ~] ~]
from a list of twigs p, terminated by a ==

        Tall
          :%  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :%(i.p i.t.p i.t.t.p)

        Irregular
          %[i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%clfs p=twig]

:/ ("colfas") is a synthetic rune that, given a twig p, produces
[%$ [%$ p ~] ~], i.e., [0 [0 p 0] 0]. Used in practice only in
string interpolation.

        Tall
          :/  p

        Wide
          :/(p)

        Irregular
          none

        Reduction
          See ++open
         
    [%clkt p=twig q=twig r=twig s=twig]

:^ ("colket") is a synthetic rune that produces a cell [p q r s]
from twigs p, q, r, and s.

        Tall
          :^    p     
              q
            r
          s
        
        Wide
          :^(p q r s)

        Irregular
          none

        Reduction
          See ++open

    [%clhp p=twig q=twig]

:- ("colhep") is a synthetic rune that produces the cell [p q] from
twigs p and q.

        Tall
          :-  p
          q

        Wide
          :-(p q)

        Irregular
          [p q]

        Reduction
          See ++open

    [%clls p=twig q=twig r=twig]

:+ ("collus") is a synthetic rune that produces a cell [p q r] from
twigs p, q, and r.

        Tall
          :+  p
            q
          r

        Wide
          :+(p q r)
        
        Irregular
          none

        Reduction
          See ++open

    [%clsg p=tusk]

:~ ("colsig") is a synthetic rune that produces a null-terminated
tuple of a list of twigs p.

        Tall
          :~  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :~(i.p i.t.p i.t.t.p)
        
        Irregular
          ~[i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%cltr p=tusk]

:* ("coltar") is a synthetic hoon that produces a tuple from p, a
list of twigs.

        Tall
          :*  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          :*(i.p i.t.p i.t.t.p)

        Irregular
          [i.p i.t.p i.t.t.p]

        Reduction
          See ++open

    [%clzz p=tusk]

"colzaz" is a synthetic internal rune that promotes its tusk p
within a %clsg or %cltr tusk.

        Not used at present.

    [%cncb p=wing q=tram]                  

%_ ("cencab") is a synthetic rune that evaluates the wing p with
the changes specified in tram q, then casts the product back to p.

        Tall
          %_  p
            p.i.q  q.i.q
            p.i.t.q  q.i.t.q
          ==

        Wide
          %_(p p.i.q q.i.q, p.i.t.q q.i.t.q)

        Irregular
          none

        Reduction            
          See ++open

    [%cncl p=twig q=twig]                  

%: ("cencol") is a synthetic rune that pulls %$ from the twig p
with the with its sample set to q.

        Tall
          %:  p
          q

        Wide
          %:(p q)
        
        Irregular
          none
      
        Reduction
          See ++open

    [%cndt p=twig q=twig]                  

%. ("cendot") is a synthetic rune that slams the gate q with 
[%cltr p]. The dual of %cnhp.

        Tall
          %.  p
          q

        Wide
          %.(p q)

        Irregular
          none

        Reduction
          %-  q
          p

          See ++open

    [%cnhp p=twig q=tusk]                  

%- ("cenhep") is a synthetic rune that slams the gate p with 
[%cltr q].

        Tall
          %-  p
          q

        Wide
          %-(p q)

        Irregular
          (p q)

        Reduction
          See ++open

    [%cntr p=wing q=twig r=tram]           

%* is a synthetic rune that pulls the wing p from tray q with changes r.

        Tall
          %*  p  q
          p.i.r  q.i.r
          p.i.t.r  q.i.t.r
          ==

        Wide
          %*(p q p.i.r q.i.r, p.i.t.r q.i.t.r)

        Irregular
          none

        Reduction
          See ++open

    [%cnkt p=twig q=twig r=twig s=twig]         

%^ ("cenket") is a synthetic rune that slams gate p with [%cntr q r s].

        Tall
          %^    p
              q
            r
          s

        Wide
          %^(p q r s)
      
        Irregular
          none  

        Reduction
          See ++open

    [%cnls p=twig q=twig r=twig]                

%+ ("cenlus") is a synthetic rune that slams gate p with [%cntr q r].

        Tall
          %+  p
            r
          s

        Wide
          %+(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%cnsg p=wing q=twig r=twig]                

%~ ("censig") is a synthetic rune that pulls p from the tray q with its
sample set to r.

        Tall
          %~  p
            q
          r

        Wide
          %~(p q r)

        Irregular
          ~(p q r)

        Reduction
          See ++open

    [%cnts p=wing q=tram]                       

%= ("centis") is a natural rune that evaluates p with the changes 
specified in q.

        Tall
          %=  p
            p.i.q    q.i.q
            p.i.t.q  q.i.t.q
          ==

        Wide
          %=(p p.i.q q.i.q, p.i.t.q q.i.t.q)

        Irregular
          p(p.i.q q.i.q, p.i.t.q q.i.t.q)

        Reduction
          See ++open

    [%cnzy p=term]                              

"cenzey" is a synthetic internal rune that pulls limb p from the subject.
        
        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%cnzz p=wing]                              

"cenzaz" is a synthetic internal rune that pulls wing p from the subject.
        
        Form
          none, internal

        Reduction
          See ++open

    [%dtkt p=twig]                              

.^ ("dotket") is a natural rune that generates Nock operator 11, which in
virtual userspace Nock (++mock) loads a file from the global namespace.

        Tall
          .^  p

        Wide
          .^(p)

        Irregular
          ^:type/path

          ^/path

        Reduction
          none, natural

    [%dtls p=twig]                              

.+ ("dotlus") is a natural rune that generates Nock operator 4, which 
increments an atomic operand.
      
        Tall
          .+  p

        Wide
          .+(p)

        Irregular
          +(p)

        Reduction
          none, natural

    [%dtzy p=term q=@]                          

"dotzey" is a natural internal rune that produces a non-cubed atomic 
constant of odor p and value q.

        Tall/Wide/Irregular
          none, internal

        Reduction
          none, natural
        
    [%dtzz p=term q=*]                          

"dotzaz" is a natural internal rune that produces a cubed noun constant of
value q and odor p, if q is an atom.

        Tall/Wide/Irregular
          none, internal

        Reduction
          none, natural

    [%dttr p=twig q=twig]                       

.* ("dottar") is a natural rune that calculates the Nock of subject p,
formula q.
     
        Tall
          .*  p
              q

        Wide
          .*(p q)

        Irregular
          none

        Reduction
          none, natural

    [%dtts p=twig q=twig]                       

.= ("dottis") is a natural rune that applies Nock 5 (equals) to determine
if the products of p and q are equivalent.

        Tall
          .=  p
              q

        Wide
          .=(p q)

        Irregular
          =(p q)

        Reduction
          none, natural

    [%dtwt p=twig]                              

.? ("dotwut") is a natural hoon that applies Nock 3 to a noun: if the 
noun is a cell, it returns the loobean & (true); if the noun is an atom, 
it returns the loobean | (false).

        Tall
          .?  p

        Wide
          .?(p)

        Irregular
          none

        Reduction
          none, natural

    [%hxgl p=tusk]                              

#< ("haxgal") is a synthetic rune that slams the assumed gate noah on
[%zpgr %cntr p]. See the Biblical names.

        Tall/Wide
          none

        Irregular
          >i.p i.t.p i.t.t.p<

        Reduction
          See ++open

    [%hxgr p=tusk]                              

#> ("haxgar") is a synthetic rune that slams the assumed gate cain on
[%zpgr %cntr p]. See the Biblical names.

        Tall/Wide
          none

        Irregular
          <i.p i.t.p i.t.t.p>

        Reduction
          See ++open

    [%ktbr p=twig]                              

^| ("ketbar") is a natural rune that converts a %gold core into an %iron
core. See geometric polymorphism.

        Tall
          ^|  p

        Wide
          ^|(p)

        Irregular
          none

        Reduction
          none, natural
        
    [%ktdt p=twig q=twig]                       

^. ("ketdot") is a synthetic rune that casts q to the type of (p q).

        Tall
          ^.  p
              q

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%ktls p=twig q=twig]                       

^+ ("ketlus") is a natural rune that casts the product of q to the 
type of p, verifying that it contains the type of q.

        Tall
          +^  p
              q

        Wide
          ^+(p q)

        Irregular
          none

        Reduction
          none, natural

    [%kthp p=tile q=twig]                       

^- ("kethep") is a synthetic rune that casts q to ~(bunt al p), 
i.e., the icon of p.

        Tall
          ^-  p
              q

        Wide
          ^-(p q)

        Irregular
          `p`q

        Reduction
          See ++open

    [%ktpm p=twig]                              

^& ("ketpam") is a natural rune that converts a %gold core to %zinc core.
See geometric polymorphism.

        Tall
          ^&  p

        Wide
          ^&(p)

        Irregular
          none

        Reduction
          none, natural

    [%ktsg p=twig]                              

^~ ("ketsig") is a natural rune that tries to execute p statically at
compile time; if this fails, p remains dynamic.

        Tall
        ^~  p

        Wide
        ^~(a)

        Irregular
          none

        Reduction
          none, natural

    [%ktts p=toga q=twig]                       

^= ("kettis") is a natural rune that wraps q in the toga p. The 
toga is a powerful naming device that can assign an entire name 
tree to a properly typed result. For instance, if foo produces 
an unlabeled tuple [x y z], [a b=[c d]]=foo produces 
[a=x b=[c=y d=z]].

        Tall
          ^=  p
              q

        Wide
          ^=(p q)

        Irregular
          none

        Reduction
          none, natural

    [%ktwt p=twig]                              

^? ("ketwut") is a natural hoon that converts a %gold core into a
%lead core. See geometric polymorphism.

        Tall
          ^?  p

        Wide
          ^?(p)

        Irregular
          none

        Reduction
          none, natural

    [%sgbr p=twig q=twig]                       

~| ("sigbar") is a synthetic rune that presents the product of p
in the stack trace if q crashes. Only performed as needed. 
Generates %cain - see the Biblical names.

        Tall
          ~|  p
              q

        Wide
          ~|(p q)

        Irregular
          none

        Reduction
          See ++open, ++feck

    [%sgcb p=twig q=twig]                       

~_ ("sigcab") is a synthetic rune that inserts p, a trap producing a tank,
into the trace of q.

        Tall
          ~_  p
              q

        Wide
          ~_(p q)

        Irregular
          none

        Reduction
          See ++open

    [%sgcn p=chum q=twig r=tyre s=twig]         

~% ("sigcen") is a synthetic rune that identifies a core for specific
optimization. See jet propulsion.

        Tall
          ~%    p
              q 
            ==
              p.i.r  q.i.r
              p.i.t.r  q.i.t.r
            ==
          s

          ~%    p
              q
            ~
          s

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%sgfs p=chum q=twig]                       

~/ ("sigfas") is a synthetic rune that identifies an arm for specific
optimization. See jet propulsion.

        Tall
          ~/  p
          q

        Wide
          ~/(p q)

        Irregular
          none

        Reduction
          See ++open

    [%sggl p=$|(term [p=term q=twig]) q=twig]   

~< ("siggal") is a synthetic rune that applies arbitrary hint p to 
the product of q. Does not wake the hint engine until the
computation is finished.

        Tall
          ~<  p
              q

        Wide
          ~<(p q)

        Irregular
          none

        Reduction
          See ++open
        
    [%sggr p=$|(term [p=term q=twig]) q=twig]
        
~> ("siggar") is a natural rune that applies arbitrary hint p to q.
         
        Tall
          ~>  p
              q

        Wide
          ~>(p q)

        Irregular
          none

        Reduction
          See ++open             

    [%sgbc p=term q=twig]
   
~$ ("sigbuc") is a synthetic rune that labels computation q as p 
for profiling (not currently enabled).

        Tall
          ~$  p
              q
        
        Wide
          ~$(p q)
        
        Irregular
          none

        Reduction
          See ++open
  
    [%sgls p=@ q=twig]
        
XX Solve        ~+ ("siglus") is a synthetic rune that memoizes computation q
        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%sgpm p=@ud q=twig r=twig]

~& ("sigpam") is a synthetic rune that prints q on the console
before computing r. p is the log priority 0-3, defaulting to 0.

        Tall
          0, debug
            ~&  q
            r

          1, notice
            ~&  >  q
            r

          2, warning
            ~&  >>  q
            r

          3, alarm
            ~&  >>>  q
            r              

        Wide
          ~&(>>> q r)

        Irregular
          none

        Reduction
          See ++open

    [%sgts p=twig q=twig]

~= ("sigtis") is a synthetic rune that hints to the interpreter
that q may produce a noun equal to the already existing p,
avoiding duplication.

        Tall
          ~=  p
              q

        Wide
          ~=(p q)
        
        Irregular
          none

        Reduction
          See ++open

    [%sgwt p=@ud q=twig r=twig s=twig]

~? ("sigwut") is a synthetic rune that prints r to the console
before computing s, iff q produces yes. p is the log priority,
0-3, 0 by default

        Tall
          0, debug
            ~?  q
              r
            s                

          1, notice
            ~?  >  q
              r 
            s

          2, warning
            ~?  >>  q                
              r
            s

          3, alarm
            ~?  >>>  q
              r
            s
            
        Wide
          ~?(>>> q r s)

        Irregular
          none

        Reduction
          See ++open

    [%sgzp p=twig q=twig]

~! ("sigzap") is a natural rune for debugging uses only,
semantically equivalent to its own twig q. Should compilation
fail within q, ~! will show the type of p on the stacktrace.

        Tall
          ~!  p
              q

        Wide
          ~!(p q)

        Irregular
          none

        Reduction
          none, natural

    [%smcl p=twig q=tusk]

;: ("semcol") is a synthetic gate that applies p, a binary gate,
to the n-ary tuple q.

        Tall
          ;:  p
            i.q
            i.t.q
            i.t.t.q
          ==

        Wide
          ;:(p i.q i.t.q i.t.t.q)

        Irregular
          :(p i.q i.t.q i.t.t.q)

        Reduction
          See ++open

    [%smdt p=twig q=tusk]
XX determine function

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smdq p=(list beer)]
XX determine if internal/external

;" ("semdoq") is a synthetic rune used to make strings,
interpolated or not.

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smsg p=twig q=tusk]

XX to do            

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%smsm p=twig q=twig]

;; ("semsem") is a synthetic rune that types q as a fixpoint of p.
Semantically identical to ((hard p) q).

        Tall
          ;;  p
              q

        Wide
          ;;(p q)

        Irregular
          none

        Reduction
          See ++open

    [%tsbr p=tile q=twig]

=| ("tisbar") is a synthetic rune that pushes ~(bunt al p) on the
subject and sends it to q.

        Tall
          =|  p
              q

        Wide
          =|(p q)

        Irregular
          none

        Reduction
          =+(_p q)
          See ++open, ++bunt in ++al

    [%tscl p=tram q=twig]

=: ("tiscol") is a synthetic rune that produces q with the subject
by p. Uses %cncb, and so cannot change the subject type.

        Tall
          =:  p.i.p  q.i.p
              p.i.t.p  q.i.t.p
              p.i.t.t.p  q.i.t.t.p
            ==
          q

        Wide
          none

        Irregular
          noen

        Reduction
          See ++open

    [%tscn p=twig q=twig]
XX to do
        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%tsdt p=wing q=twig r=twig]

=. ("tisdot") is a synthetic rune that produces r with p in the
subject set to q. Uses %cncb, and so cannot change the subject
        p.

        Tall
          =.  p
            q
          r
        
          =.  p  q
          r

        Wide
          =.(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%tsfs p=twig q=twig]
        XX not used

    [%tsgl p=twig q=twig]

=< ("tisgal") is a synthetic rune that uses the product of q as 
the subject of p.

        Tall
          =<  p
          q
        
        Wide
           =<(p q)

        Irregular

        Reduction
          See ++open

    [%tshp p=twig q=twig]

=- ("tishep") is a synthetic rune that pushes q on the subject
and sends it to p. Dual of =+ ("tislup")
        

        Tall
          =-  p
          q

        Wide
          =-

        Irregular
          none

        Reduction
          See ++open

    [%tsgr p=twig q=twig]

=> ("tisgar") is a natural rune that uses the product of p as the
subject of q.

        Tall
          =>  p
          q

        Wide
          =>(p q)

        Irregular
          none

        Reduction
          none, natural

    [%tskt p=twig q=twig r=twig s=twig]

=^ ("tisket") is a synthetic rune that handles a product which is
a cell of the new result, and a mutation to the subject.

        Tall
          Kingside
            =^    p
                q
              r
           s

          Queenside
            =^  p  q
              r
            s      

        Wide
          =^(p q r s)

        Irregular
          none

        Reduction
          See ++open

    [%tsls p=twig q=twig]

=+ ("tislus") is a synthetic rune that pushes p on the subject
and sends it to q. Semantically equavlent to Nock 8.Dual of =- ("tishep")

        Tall
          =+  p
          q

        Wide
          =+(p q)

        Irregular
          none           

        Reduction
          See ++open

    [%tspm p=tile q=twig]

        XX not used

    [%tspt p=tile q=twig]

        XX not used

    [%tstr p=term q=wing r=twig]

=* ("tistar") is a natural rune that creates a %bull, or alias,
type.

        Tall
          =*  p  q
              r

        Wide
          =*(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%tssg p=tusk]

=~ ("tissig") is a synthetic rune that composes a list of twigs.

        Tall
          Kingside
            =~    i.p
                i.t.p
                i.t.t.p
            ==

          Queenside
            =~  i.p
                i.t.p
                i.t.t.p
            ==

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%wtbr p=tusk]

?| ("wutbar") is a synthetic rune that computes the "or" of the
loobeans in p.

        Tall
          ?|  i.p
              i.t.p
              i.t.t.p
          ==            

        Wide
          ?|(i.p i.t.p i.t.t.p)

        Irregular
          |(i.p i.t.p i.t.t.p)

        Reduction
          See ++open

    [%wthp p=wing q=tine]

?- ("wuthep") is a synthetic rune that selects a case in q for
the actual type of p.

        Tall
          Kingside
            ?-  p
              p.i.q  q.i.q
              p.i.t.q  q.i.t.q
              p.i.t.t.q  q.i.t.t.q
            ==  
          
          Queenside
            ?-    p
                p.i.q
              q.i.q
                p.i.t.q
              q.i.t.q
                p.i.t.t.q
              q.i.t.t.q
            ==

        Wide
          ?-(p p.i.q q.i.q, p.i.t.q q.i.t.q, p.i.t.t.q q.i.t.t.q) 

        Irregular
          none
        
        Reduction
          See ++open

    [%wthz p=tiki q=tine]

"wuthaz" is a synthetic internal rune that selects a case in q 
for the actual type of p.

        Tall/Wide/Irregular
          none, internal            

        Reduction
          See ++open

    [%wtcl p=twig q=twig r=twig]

?: ("wutcol") is a natural rune that produces q if p is yes (&, 0),
or r if p is no (|, 1).

        Tall
          ?:  p
            q 
          r

        Wide
          ?:(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%wtdt p=twig q=twig r=twig]

?. ("wutdot") is a synthetic rune that prduces r if p is yes 
(&, 0), of q if p is no (|, 1).

        Tall
          ?.  p
            q
          r

        Wide
          ?:(p q r)

        Irregular
          none

        Reduction
          none, natural

    [%wtkt p=wing q=twig r=twig]

?^ ("wutkey") is a synthetic rune that evaluates r if p is 
equivalent to the bunt for its tile, otherwise q is evaluted.

        Tall
          ?^  p
            q
          r

        Wide
          ?^(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtkz p=tiki q=twig r=twig]

"wutkaz" is a synthetic, internal rune that evaluates r if p is
equivalent to the bunt for its tile, otherwise q is evaluated.
See tikis.            

        Tall
          ?^  p
            q
          r

        Wide
          ?^(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtgl p=twig q=twig]

?< ("wutgal") is a synthetic hoon that produces q, asserting that
p is no (|, 1).

        Tall
          ?<  p
              q

        Wide
          ?<(p q)

        Irregular
          none 

        Reduction
          See ++open

    [%wtgr p=twig q=twig]

?> ("wutgar") is a synthetic hoon that produces q, asserting that
p is yes (&, 0).

        Tall
          ?>  p
              q

        Wide
          ?>(p q)

        Irregular
          none

        Reduction
          See ++open

    [%wtls p=wing q=twig r=tine]

?+ ("wutlus") is a synthetic rune that selects a case in q for
the actual type of p.

        Tall
          Kingside
            ?+  p
              q
              p.i.r  q.i.r
              p.i.t.r  q.i.t.r
              p.i.t.t.r  q.i.t.t.r
            ==

          Queenside
            ?+    p
              q
                p.i.r
              q.i.r
                p.i.t.r
              q.i.t.r
                p.i.t.t.r
              q.i.t.t.r
            ==

        Wide
          ?+(p p.i.r q.i.r, p.i.t.r q.i.t.r, p.i.t.t.r q.i.t.t.r)

        Irregular
          none
        
        Reduction
          See ++open

    [%wtlz p=tiki q=twig r=tine]
        
"wutlaz" is a synthetic, internal rune that selects a case in q for
the actual type of p.

        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%wtpm p=tusk]

?& ("wutpam") is a synthetic hoon that computes the "and" of the 
loobeans in p.

        Tall
          ?&  i.p
              i.t.p
              i.t.t.p
          ==

        Wide
          ?&(i.p i.t.p i.t.t.p)

        Irregular
          none

        Reduction
          See ++open

    [%wtpt p=wing q=twig r=twig]

?@ ("wutpat") is a synthetic hoon that produces q if p is an 
atom, r otherwise.

        Tall
          Kingside
            ?@  p
              q
            r
      
          Queenside
            ?@  p
              q
            r

        Wide
          ?@(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtpz p=tiki q=twig r=twig]

"wutpaz" is a synthetic hoon that produces q if p is an atom, r 
otherwise.

        Tall
          ?@  p
            q
          r

        Wide
          ?@(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtsg p=wing q=twig r=twig]

?~ ("wutsig") is a synthetic rune that produces q if p is ~, r
otherwise.

        Tall
          ?~  p
            q
          r

        Wide
          ?~(p q r)

        Irregular
          none

        Reduction
          See ++open

    [%wtsz p=tiki q=twig r=twig]

"wutsaz" is a synthetic internal rune that produces q if p is ~, 
r otherwise.

        Tall/Wide/Irregular
          none, internal

        Reduction
          See ++open

    [%wtts p=tile q=wing]

?= ("wuttis") is a natural rune that produces true if the leg at
wing q is in tile p.

        Tall
          ?=  p
              q

        Wide
          ?=(p q)

        Irregular
          none

        Reduction
          none, natural

    [%wtzp p=twig]

?! ("wutzap") is a synthetic rune that produces the logical "not"
of p. 

        Tall
          ?!  p

        Wide
          ?!(p)

        Irregular
          !p

        Reduction
          See ++open

    [%zpcb p=spot q=twig]
XX tall/wide form
!_ ("zapcab") is a natural rune that puts debugging information
in the stack trace.

        Tall
             
        Wide

        Irregular
          none

        Reduction
          none, natural

    [%zpcm p=twig q=twig]

!, ("zapcom") is a natural rune that inserts twig q as a 
constant, typed with the type of twig p.

        Tall
          !,  p
              q

        Wide
          !,(p q)

        Irregular
          none

        Reduction
          none, natural

    [%zpcn ~]
XX determine function
!% ("zapcen")

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%zpfs p=twig]
XX tall/wide
!/ ("zapfas") is a natural rune that should never be compiled. 
When compiled with error checking turned on, it reports its 
subject as an error.

        Tall

        Wide

        Irregular

        Reduction
          See ++open

    [%zpgr p=twig]

!> ("zapgar") is a synthetic rune that produces a vase (a 
[type noun] cell) with the value p.

        Tall
          !>  p

        Wide
          !>(p)

        Irregular
          none

        Reduction
          See ++open

    [%zpsm p=twig q=twig]

!; ("zapsem") is a natural rune that produces the product of twig
q as a [type noun] pair, with twig p defining the type of the type.

        Tall
          !;  p
              q

        Wide
          !;(p q)

        Irregular
          none
        
        Reduction
          none, natural

    [%zpts p=twig]

!= ("zaptis") is a natural rune that produces the formula of twig
p as a noun.

        Tall
          !=  p

        Wide
          !=(p)

        Irregular
          none

        Reduction
          none, natural

    [%zpwt p=$|(p=@ [p=@ q=@]) q=twig]

!? ("zapwut") is a synthetic rune that enforces a Hoon version
restriction.

        Tall
          !?  p
          q

        Wide
          none

        Irregular
          none

        Reduction
          See ++open

    [%zpzp ~]

!! ("zapzap") is a natural rune that always causes a crash when
executed. 

        Tall
          none

        Wide
          !!

        Irregular
          none

        Reduction
          none, natural

---

