::
::
|%
+$  place  @tasplace
::  $chain: loose graph of questions
::
+$  chain
  $:  flow=(lest place)
      bits=(map place point)
  ==
::  $point: point (message) in conversation
::
+$  point  $@(@t query)
::  $query: question awaiting answer
::
+$  query
  $~  [%loose '']
  $%  [%loose q=@t]                                     ::  open question
      [%yesno q=@t]                                     ::  binary question
      [%multi q=@t a=(list @t)]                         ::  multiple choice
      [%drift next=$-(datum place) q=query]             ::  branch on answer
  ==
::  $datum:  answer to query
::
+$  datum
  $%  [%loose d=@t]
      [%yesno d=?]
      [%multi d=@ud]
  ==
::
::
::  +hop: drift function builders
::
++  hop
  |%
  ++  yesno
    |=  [y=place n=place]
    |=  d=datum
    ~|  [want=%yesno have=-.d]
    ?>  ?=(%yesno -.d)
    ?:(d.d y n)
  ::
  ++  multi
    |=  h=(list place)
    |=  d=datum
    ~|  [want=%multi have=-.d]
    ?>  ?=(%multi -.d)  ::TODO  what happens if we crash? how do we recover?
    ~|  [want=d.d max=(lent h)]
    (snag d.d h)
  --
::  +build-chain: adds start and end phrases
::
++  build-chain
  |=  [desc=@t points=(lest [p=place n=point])]
  ^-  chain
  ::TODO  kinda wish we could sanity-check jumps/references,
  ::      but they're hidden inside the drift gates...
  =.  points
    :*  :-  %eliza-ask-for-permission
        :+  %drift
          (yesno:hop p.i.points %eliza-end)
        :-  %yesno
        :((cury cat 3) 'May I ask you some questions about ' desc '?')
      ::
        %+  snoc  points
        :-  %eliza-end
        'That is all for now. Thank you for your time!'
    ==
  :-  =+  (turn points head)
      ?<(?=(~ -) -)
  (~(gas by *(map place point)) points)
--
