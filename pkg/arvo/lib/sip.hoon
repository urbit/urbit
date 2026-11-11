::  sip: lazy cursor over a jammed noun
::
::    A read-only cursor over a jammed noun.  It classifies nodes, skips
::    subtrees, follows backreferences, and extracts atoms at a given axis --
::    all without materializing the noun.  See the +sip design doc.
::
::    Bits are read LSB-first from the jammed atom.  At any node offset:
::
::      0    atom     +mat encoding follows
::      1 0  cell     head node, then tail node
::      1 1  backref  +mat-encoded absolute bit offset of referent
::
::    Every arm crashes on malformed input, out-of-range axes, or type
::    mismatch, mirroring +cue: hostile or truncated input is a deterministic
::    %exit, never a wrong answer.  +cue is ground truth.
::
::    The hot path +grab (atom at axis) is jetted in Vere (i-tier, %non/%sip);
::    the jet subsumes +dive, +gaze, +fetch, and the +hop skips on the path.
::
=<  sip
~%  %non  ..part  ~  ::  nest non in hex for now, as in lib/lagoon
|%
++  sip
  =+  ~                                             ::  pad context so ~/ +7 lands on %non
  ~/  %sip
  |%
  +$  slip  [a=@ off=@ud]                           ::  buffer + bit cursor
  +$  kind  ?(%atom %cell %back)                    ::  node classification
  ::
  ++  peek                                          ::  classify node, O(1)
    |=  s=slip
    ^-  kind
    ?:  =(0 (cut 0 [off.s 1] a.s))     %atom
    ?:  =(0 (cut 0 [+(off.s) 1] a.s))  %cell
    %back
  ::
  ++  hop                                           ::  bit span of subtree
    |=  s=slip
    ^-  @ud
    ?-  (peek s)
      %atom  +(p:(rub +(off.s) a.s))                ::  tag + mat span
      %back  (add 2 p:(rub (add 2 off.s) a.s))      ::  skip index; don't follow
      %cell  =+  hed=$(off.s (add 2 off.s))
             :(add 2 hed $(off.s :(add 2 off.s hed)))
    ==
  ::
  ++  jump                                          ::  follow one backref
    |=  s=slip
    ^-  slip
    ?>  ?=(%back (peek s))
    =+  tgt=q:(rub (add 2 off.s) a.s)
    ?>  (lth tgt off.s)                             ::  backward only, or crash
    s(off tgt)
  ::
  ++  gaze                                          ::  resolve backref chain
    |=  s=slip
    ^-  slip
    |-  ?.  ?=(%back (peek s))  s
    $(s (jump s))
  ::
  ++  head                                          ::  cursor at cell head
    |=  s=slip
    ^-  slip
    =.  s  (gaze s)
    ?>  ?=(%cell (peek s))
    s(off (add 2 off.s))
  ::
  ++  tail                                          ::  cursor at cell tail
    |=  s=slip
    ^-  slip
    =.  s  (gaze s)
    ?>  ?=(%cell (peek s))
    =+  hed=s(off (add 2 off.s))
    hed(off (add off.hed (hop hed)))
  ::
  ++  fetch                                         ::  atom value at cursor
    |=  s=slip
    ^-  @
    =.  s  (gaze s)
    ?>  ?=(%atom (peek s))
    q:(rub +(off.s) a.s)
  ::
  ++  dive                                          ::  cursor at axis b
    |=  [s=slip b=@ud]
    ^-  slip
    ?<  =(0 b)
    |-  ^-  slip
    ?:  =(1 b)  (gaze s)
    =/  rest  (mas b)
    $(b rest, s ?:(=(2 (cap b)) (head s) (tail s)))
  ::
  ++  grab                                          ::  atom at axis: hot path
    ~/  %grab
    |=  [s=slip b=@ud]
    ^-  @
    (fetch (dive s b))
  --
--
