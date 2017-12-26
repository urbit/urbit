::
/?    310
/-  hall
/+  old-zuse
[old-zuse .]
=>
|%
++  audience  (map partner (pair envelope delivery))    ::  destination+state
++  bouquet  (set flavor)                               ::  complete aroma
++  delivery                                            ::  delivery state
  $?  $pending                                          ::  undelivered
      $received                                         ::  delivered
      $rejected                                         ::  undeliverable
      $released                                         ::  sent one-way
      $accepted                                         ::  fully processed
  ==                                                    ::
++  envelope  (pair ? (unit partner))                   ::  visible sender
++  flavor  path                                        ::  content flavor
++  passport                                            ::  foreign flow
  $%  {$twitter p/@t}                                   ::  twitter
  ==                                                    ::
++  presence   ?($gone $hear $talk)                     ::  status type
++  speech                                              ::  narrative action
  $%  {$lan p/knot q/@t}                                ::  local announce
      {$exp p/@t}                                       ::  hoon line
      {$non $~}                                         ::  no content (yo)
      {$ext p/@tas q/*}                                 ::  extended action
      {$fat p/torso q/speech}                           ::  attachment
      {$url p/purf}                                     ::  parsed url
      {$ire p/serial q/speech}                          ::  in-reply-to
      {$lin p/? q/@t}                                   ::  no/@ text line
      {$mor p/(list speech)}                            ::  multiplex
      {$app p/@tas q/@t}                                ::  app message
      $:  $api                                          ::  api message
          service/@tas                                  ::  service name
          id/@t                                         ::  id on the service
          id-url/purf                                   ::  link to id
          summary/@t                                    ::  summary of event
          body/@t                                       ::  body of event
          url/purf                                      ::  link to event
          meta/json                                     ::  other data for web
      ==                                                ::
  ==                                                    ::
++  serial     @uvH                                     ::  unique identity
++  partner    (each station passport)                  ::  interlocutor
++  statement  (trel @da bouquet speech)                ::  when this
++  station    (pair ship knot)                         ::  domestic flow
++  telegram   (pair ship thought)                      ::  who which whom what
++  thought    (trel serial audience statement)         ::  which whom what
++  torso                                               ::  attachment
  $%  {$name (pair @t torso)}                           ::  named attachment
      {$text (list @t)}                                 ::  text lines
      {$tank (list tank)}                               ::  tank list
  ==                                                    ::
--
|%
++  from-json
  =>  [jo ..telegram]
  |=  a/^json  ^-  (list telegram:hall)
  =-  %-  zing
      %+  turn
        (need ((ar (ot ship+(su fed:ag) thought+thot ~)) a))
      convert-telegram
  |%
  ++  of
    |*  a/(pole {@tas fist})
    |=  b/^json
    %.  ((of:jo a) b)
    %-  slog
    ?+  b  ~
        {$o *}
      %+  murn  `(list {@tas fist})`a
      |=  {c/term d/fist}  ^-  (unit tank)
      =+  (~(get by p.b) c)
      ?~  -  ~
      =+  (d u)
      ?~  -  (some >[c u]<)
      ~
    ==
  ++  op                                              ::  parse keys of map
    |*  {fel/rule wit/fist}
    %+  cu  malt
    %+  ci
      |=  a/(map cord _(need *wit))
      ^-  (unit (list _[(wonk *fel) (need *wit)]))
      (zl (turn ~(tap by a) (head-rush fel)))
    (om wit)
  ::
  ++  as                                              ::  array as set
    |*  a/fist
    (cu ~(gas in *(set _(need *a))) (ar a))
  ::
  ++  ke                                              ::  callbacks
    |*  {gar/* sef/_|.(fist)}
    |=  jon/^json
    ^-  (unit _gar)
    =-  ~!  gar  ~!  (need -)  -
    ((sef) jon)
  ::
  ++  lake  |*(a/_* $-(^json (unit a)))
  ++  head-rush
    |*  a/rule
    |*  {cord *}
    =+  nit=(rush +<- a)
    ?~  nit  ~
    (some [u.nit +>->])
  ::
  ++  thot
    ^-  $-(^json (unit thought))
    %-  ot  :~
      serial+`$-(^json (unit serial))`(ci (slat %uv) so)
      audience+`$-(^json (unit audience))`audi
      statement+`$-(^json (unit statement))`stam
    ==
  ::
  ++  audi  `$-(^json (unit audience))`(op parn memb)
  ++  auri  (op parn (ci (soft presence) so))
  ++  memb  ^-  $-(^json (unit (pair envelope delivery)))
            (ot envelope+lope delivery+(ci (soft delivery) so) ~)
  ++  lope  (ot visible+bo sender+(mu (su parn)) ~)
  ::
  ++  parn
    ^-  $-(nail (like partner))
    %+  pick
      ;~((glue fas) ;~(pfix sig fed:ag) urs:ab)
    %+  sear  (soft passport)
    ;~((glue fas) sym urs:ab)                         ::  XX  [a-z0-9_]{1,15}
  ::
  ++  stam  (ot date+di bouquet+(as (ar so)) speech+spec ~)
  ++  spec
    %+  ke  *speech  |.  ~+
    %-  of  :~
      lin+(ot say+bo txt+so ~)
      url+(ot txt+(su aurf:urlp) ~)
      exp+(ot txt+so ~)
      app+(ot txt+so src+so ~)
      fat+(ot tor+tors taf+spec ~)
      ext+(ot nom+so txe+blob ~)
      non+ul
      mor+(ar spec)
      ::  inv+(ot ship+(su fed:ag) party+(su urs:ab) ~)
    ==
  ++  tors
    %+  ke  *torso  |.  ~+
    %-  of  :~
      name+(ot nom+so mon+tors ~)
      text+(cu to-wain:format so)
      tank+(ot dat+(cu (hard (list tank)) blob) ~)
    ==
  ::
  ++  blob  (cu cue (su fel:ofis))
  ::
  ::
  ++  convert-telegram
    |=  t/telegram
    ^-  (list telegram:hall)
    =+  aud=(convert-audience q.q.t)
    %+  turn  (convert-speech r.r.q.t)
    |=  s/speech:hall
    [p.t p.q.t aud p.r.q.t s]
  ::
  ++  convert-audience
    |=  a/audience
    ^-  audience:hall
    %-  sy
    ^-  (list circle:hall)
    %+  murn  ~(tap in ~(key by a))
    |=  p/partner
    ^-  (unit circle:hall)
    ?-  -.p
      $&  :+  ~  p.p.p
          ?:  ?|  =(q.p.p 'porch')
                  =(q.p.p 'court')
                  =(q.p.p 'floor')
              ==
            %inbox
          q.p.p
      $|  ~
    ==
  ::
  ++  convert-speech
    |=  s/speech
    ^-  (list speech:hall)
    ?+  -.s  ~&([%ignoring -.s] ~)
      $lin  [%lin !p.s q.s]~
      $url  [%url p.s]~
      $exp  [%exp p.s ~]~
      $ire  %+  turn  (convert-speech q.s)
            |=  i/speech:hall
            [%ire p.s i]
      $app  [%app p.s [%lin | q.s]]~
      $fat  ?:  &(?=($exp -.q.s) ?=($tank -.p.s))
              [%exp p.q.s +.p.s]~
            =+  ses=(convert-speech q.s)
            =?  ses  =(0 (lent ses))  [%lin | '']~
            [[%fat p.s (snag 0 ses)] (slag 1 ses)]
      $mor  (zing (turn p.s convert-speech))
    ==
  --
--
