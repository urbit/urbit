::  task: herm task for passthrough to dill
::TODO  deduplicate with /mar/belt, or remove one or the other
::
/-  herm
::
|_  =task:herm
++  grad  %noun
::  +grab: convert from
::
++  grab
  |%
  ++  noun  task:herm
  ::
  ++  json
    |=  jon=^json
    ^+  task
    ~|  jon
    ?>  ?=([%o *] jon)
    =+  ses=(~(got by p.jon) 'session')
    ?>  ?=([%s *] ses)
    :-  ?:  =('' p.ses)  %$
        (slav %tas p.ses)
    =.  p.jon  (~(del by p.jon) 'session')
    %.  jon
    =,  dejs:format
    |^  task
    ++  task
      %-  of
      :~  belt+belt
          blew+(ot 'w'^ni 'h'^ni ~)
          hail+ul
          open+(ot 'term'^(se %tas) 'apps'^(ar gill) ~)
          shut+ul
      ==
    ::
    ++  gill
      (ot 'who'^(se %p) 'app'^(se %tas) ~)
    ::
    ++  belt
      |=  j=json
      ^-  belt:dill
      ?:  ?=([%s *] j)
        (taft p.j)
      %.  j
      %-  of
      :*  mod+(ot 'mod'^mod 'key'^bot ~)
          txt+(ar (cu taft so))
          bol
      ==
    ::
    ++  bol
      :~  aro+(su (perk %d %l %r %u ~))
          bac+ul
          del+ul
          hit+(ot 'r'^ni 'c'^ni ~)
          ret+ul
      ==
    ::
    ++  bot
      |=  j=json
      ^-  bolt:dill
      ?+  j  !!
        [%s *]  (taft p.j)
        [%o *]  ((of bol) j)
      ==
    ::
    ++  mod
      |=  j=json
      ((su (perk %ctl %met %hyp ~)) j)
    --
  --
::  +grow: convert to
::
++  grow
  |%
  ++  noun  task
  --
--
