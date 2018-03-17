::
=,  gall
=,  ford
=,  format
|_  {bowl $~}
++  peek  _~
++  made-a-core
  |=  {a/spur @uvH b/gage}
  :_  +>.$
  ?>  ?=([%tabl ^ ~] b)
  =/  cur  p.i.p.b
  %-  %-  slog
      ?+  -.cur  !!
        %|  (flop p.cur)
        %&  ~ ::[(sell q.p.cur)]~
      ==
  =/  nex/(list spur)
    =<(p ;;(,[%& %cont * p=(list spur)] q.i.p.b))
  ?~  nex  ~&(%cores-tested ~)
  [ost (build-core nex)]~
::
++  build-core
  |=  [a=spur b=(list spur)]
  ~&  >>  (flop a)
  :^  %exec  a-core+a  our
  %-  some
  ^-  bilk
  :-  now-beak
  :~  %tabl
    [[%core now-beak a] [%$ %cont !>(b)]]
  ==
::
++  poke-noun
  |=  [%cores a=path]  ::TODO restore historical [%marks ~] handler
  :_  +>
  ?:  [dry=|]
    ~&((list-cores a) ~)
  [ost (build-core [- +]:(list-cores a))]~
::
++  list-cores
  |=  a/path  ^-  (list spur)
  =/  sup  (flop a)
  ~&  [%findining-hoons under=a]
  |-  ^-  (list spur)
  %-  zing
  %+  turn
    =-  (sort ~(tap by -) aor)
    dir:.^(arch %cy (en-beam now-beak sup))
  |=  [a=knot ~]  ^-  (list spur)
  =.  sup  [a sup]
  ?:  (~(has in skips) (flop sup))
    ~&(> [(flop sup) %skipped] ~)
  ?~  [fil:.^(arch %cy (en-beam now-beak [%hoon sup]))]
    ^$
  ~&  (flop sup)
  [sup ^$]
::
++  now-beak  %_(byk r [%da now])
++  skips
  %-  sy  :~ ::TODO don't hardcode
    /app/gh          :: hangs for some reason
    /mar/gh          :: hangs for some reason
    /mar/twit        :: slow and/or crash
    /ren/test-tree   :: recursive
    /sys             :: generally out of scope
  ::
    /app/pipe        ::  wants "flavor"
    /gen/capitalize  :: wants unicode-data/txt
  ::
    /lib/down-jet/parse    :: // nonsense
    /lib/down-jet/rend     :: // nonsense
    /lib/hood/drum         :: ford can't handle surs from libs
    /lib/hood/kiln         :: ford can't handle surs from libs
    /lib/sole              :: ford can't handle surs from libs
    /lib/hall              :: ford can't handle surs from libs
    /lib/twitter           :: ford can't handle surs from libs
    /ren/css               :: not meant to be called outside /web/pack
    /ren/js                :: not meant to be called outside /web/pack
    /ren/rss-xml           :: uses /$
    /ren/run               :: not meant to be called except on a (different) hoon file
    /ren/tree/body         :: uses ren/tree
    /ren/tree/head         :: uses /$  
    /ren/tree/json         :: uses /$  
    /ren/urb               :: uses ren/tree
    /ren/urb/tree          :: uses ren/tree
    /sys/arvo              :: BROKEN
    /sys/vane/jael         :: expects our
    /sys/vane/xmas         :: expects our
  ==
--
