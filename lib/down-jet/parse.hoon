::    Core markdown parser, exposed as ++parse
::
::::  /hoon/parse/down-jet/lib
  ::
/?    310
=,  html
=,  format
=+  =~
|%
++  loca  |*(a/_* {p/@u q/a})  
++  stack  (list (loca tops))
--
|%
++  htm-enty                                            ::  XX belongs in zuse
  =,  space:userlib
  ~+  
  =-  |=  tub/nail  ^-  (like @t)  %.  tub              ::  export context
      =+(de-xml enty(ent mapping))
  ^-  mapping/(map knot @tF)
  =+  pax=/==2%%/html5-entities/json                    ::  XX  %%/
  =+  maf=%.(pax ;~(biff file (soft json) (om so):dejs-soft))
  ?^  maf  u.maf
  ~&  no-enty+pax
  (my amp+'&' quot+'"' apos+'\'' lt+'<' gt+'>' ~)       ::  fallback  
::
++  skem-symb  (star ;~(pose dot alp))
++  skem-set  ~+  ^-  (set cord)                        ::  URI schemes
  ::     (silt `wain`/http/https/javascript)
  =-  (silt `wain`(rash - (more gah (cook crip skem-symb))))
  '''
  coap doi javascript aaa aaas about acap cap cid crid data dav dict dns file
  ftp geo go gopher h323 http https iax icap im imap info ipp iris iris.beep
  iris.xpc iris.xpcs iris.lwz ldap mailto mid msrp msrps mtqp mupdate news nfs
  ni nih nntp opaquelocktoken pop pres rtsp service session shttp sieve sip sips
  sms snmp soap.beep soap.beeps tag tel telnet tftp thismessage tn3270 tip tv
  urn vemmi ws wss xcon xcon-userid xmlrpc.beep xmlrpc.beeps xmpp z39.50r
  z39.50s adiumxtra afp afs aim apt attachment aw beshare bitcoin bolo callto
  chrome chrome-extension content cvs com-eventbrite-attendee dlna-playsingle
  dlna-playcontainer dtn dvb ed2k facetime feed finger fish gg git gizmoproject
  gtalk hcp icon ipn irc irc6 ircs itms jar jms keyparc lastfm ldaps magnet maps
  market message mms ms-help msnim mumble mvn notes oid palm paparazzi platform
  proxy psyc query res resource rmi rsync rtmp secondlife sftp sgn skype smb
  soldat spotify ssh steam svn teamspeak things udp unreal ut2004 ventrilo
  view-source webcal wtai wyciwyg xfire xri ymsgr
  '''
::
++  uri-skem   (sear (flit |=(a/tape (~(has in skem-set) (crip (cass a))))) skem-symb)
--
::
::::
  ::
|%
++  nal  (just `@`10)
++  end  (full (easy ~))
++  eol  ;~(pose (cold ~ nal) end)
::++  match  |=(a/rege (curr scan (ra a)))
::
++  tab
  |=  tub/nail
  ^-  (like cord)  
  ?.  ?=({$9 *} q.tub)
    (fail tub)
  =+  sto=+((~(inv fe 1) (dec q.p.tub)))                ::  virt spaces produced
  =+  neu=(weld (reap sto ' ') t.q.tub)
  (next tub(q neu))
::
++  whif  |*(a/rule ;~(sfix a (star ace)))
++  ahed  |*(a/rule ;~(simu a (easy ~)))
++  opts  |*(a/rule ;~((bend) (easy ~) a))
++  lots  |*({a/@u b/rule} ;~(pfix (stun [a a] b) (star b)))
++  leas  |*(a/rule ;~(pfix ;~(less (stun 4^4 ace) (stun 0^3 ace)) a))
++  pech  
  |*  a/{rule rule}
  |=  tub/nail  
  ^-  (like (each _(wonk (-.a)) _(wonk (+.a))))
  %.  tub
  ;~(pose (stag %& -.a) (stag %| +.a))
::
++  lite                                                ::  literal matched
  |*  a/rule
  |=  tub/nail  ^-  (like tape)
  =+  vex=(a tub)
  ?~  q.vex  vex
  =+  tap=q.q.u.q.vex
  =-  vex(p.u.q -)
  |-  ^+  ""
  ?:  =(tap q.tub)  ~
  ?~  q.tub  !!
  [i.q.tub $(q.tub t.q.tub)]
::
++  enrule                                              ::  XX
  |*  a/$-(tape *)
  |=  tub/nail
  ^-  (like a)
  [[0 0] ~ (a q.tub) [0 0] ~]
--  ==
  =~
|%
++  strip
  |=  {a/$-(nail edge) b/tape}
  ^-  tape
  =+  q:(a 1^1 b)
  ?~(- b q.q.u.-)
::
++  inli                                                ::  inline parsers
  =+  [bug=| rec="" nol=|]
  |_  ref/(map cord {p/tape q/(unit tape)})
  ++  unesc
    |=  a/tape
    (scan a (star ;~(pose nore(rec ~) nal)))          ::  XX
  ::
  ++  esc
    ;~  pfix  bas
      (mask "!\"#$%&'()*+,-./:;<=>?@[\\]^_`\{|}~")
    ==
  ++  spec  (mask rec)
  ++  norc  ~+  ;~(pose esc ;~(less spec prn))
  ++  nore  ~+  ;~(pose htm-enty norc)
  ::
  ++  trim-sp
    |=  a/tape
    ^-  tape
    %+  strip   (star gah)
    (flop (strip (star gah) (flop a)))
  ::
  ++  child
    =<  |=  tub/nail  ^-  (like inline)  %.  tub        ::  expose parsers
        ;~(pose code line link blot:link htmt)
    |%
    ++  fens
      |=  a/tape
      %+  knee  *tape  |.  ~+
      |=  tub/nail  ^-  (like tape)
      =+  vex=((plus tec) tub)
      ?~  q.vex
        (plug (codc tub) ^^$)
      ?:  =(a p.u.q.vex)
        vex(p.u.q ~)
      =+  neu=|=(b/tape (weld `tape`p.u.q.vex b))
      ((cook neu ^^$) q.u.q.vex)
    ::
    ++  codc  ;~(pose (cold ' ' (plus gah)) prn)
    ++  code
      =-  ;~(pose - (stag %$ (plus tec)))
      %+  stag  %code
      %+  cook  trim-sp
      |=  tub/nail
      ?.  ?=({$'`' ^} q.tub)
        (fail tub)
      =+  vex=((plus tec) tub)
      (pfix vex (fens (wonk vex)))
    ::
    ++  madn
      |=  a/mane  ^-  ?
      =>  |=(b/knot ?=(^ (rush b ;~(plug alf (star aln)))))
      ?@  a  (. a)  
      &((. -.a) (. +.a))
    ::
    ++  htmt  
      %+  stag  %htmt
      =-  (cook crip (lite -))
      %+  sear
        %-  flit
        |=  a/$^({marx $~} $%({$& marx} {$| p/mane}))
        ?-  -.a
          ^   (madn n.a) 
          $&  (madn n.a) 
          $|  (madn p.a)
        ==
      ;~(pose empt:de-xml (pech [head tail]:de-xml))
    ::
    ++  line 
      %+  cold  [%line ~]
      ;~(plug ;~(pose (lots 2 ace) bas) nal)  
    ::
    ++  empty
      |=  a/inline
      ^-  ?
      ?&  ?=($$ -.a)
          ?=($~ (strip (star ;~(pose ace nal)) p.a))
      ==
    ::
    ++  link  ::=+(blot=fail |=(nail (fail +<)))
      =<  =-  |=(tub/nail (pra tub))                      ::  expose parsers
          ?:  nol  pra=auto ::;~(pose auto pexp)
          =-  pra=;~(pose auto (cook - apex))
          |=({a/kids b/{tape (unit tape)}} [[%link b] a])
      |%
      ++  apex  
        =+  ^-  rob/$-(nail (like {kids $@(cord {tape (unit tape)})}))
            ;~(plug text(nol &) labe)
        ::
        |=  tub/nail  
        ^-  (like (pair kids {tape (unit tape)}))
        ::  (fail tub)
        =+  vex=(rob tub)
        ?~  q.vex
          vex
        =-  ?~(rez vex(q ~) vex(+.p.u.q u.rez))
        ^-  rez/(unit {tape (unit tape)})
        =+  [~ atr]=(wonk vex)
        ?^  atr
          `atr
        ?.  ?=($~ atr)
          (~(get by ref) atr)
        =+  vux=(text:href tub)
        ?~  q.vux  ~
        (~(get by ref) (wonk vux))  
      ::
      ++  text  
        =.  rec  ['[' ']' rec]
        (ifix sel^ser many)
      ::
      ++  titl  (opts ;~(pfix (plus gah) titl:href))
      ++  labe  
        =+  wra=[;~(plug pel .) ;~(plug . per)]:(star gah)
        ;~  pose
          (ifix wra ;~(plug dest:href titl))  
          ;~(pfix (star gah) text:href)
          (easy '')
        ==
      ::
      ++  blot
        %+  cook
          |=({a/kids b/{tape (unit tape)}} [[%blot b] a])  
        ;~(pfix zap [.(rob ;~(plug text labe))]:apex)
      ::
      ++  mail  
        %+  cook  |=(_[a="" b='' c="" d=*wall] :(welp a b^c (zing d)))
        ;~  plug
          (plus ;~(less (mask "\"\\ (),:;<>@[]") prn))
          pat
          (plus alp) 
          (star ;~(plug dot (plus alp)))
        ==
      ::
      ++  auto
        %+  ifix  gal^gar
        ;~  pose
          (cook |=(a/tape [link+["mailto:{a}" ~] ~[[%$ a]]]) mail)  
        ::
          =+  cha=;~(pose htm-enty ;~(less (mask "<> ") prn))
          %+  cook
            |=  a/_["" ""]
            [link+[(weld -.a +.a) ~] ~[[%$ (weld -.a +.a)]]]
          ;~(plug uri-skem col (star cha))   
        ==
      ::  
      ++  pexp                                          ::  XX non-link nested
        %+  cook  
          |=(a/kids (reso:many "[" (welp a [[%$ "]"] ~])))  
        (ifix sel^ser many)
      --
    --
  ::
  ++  href
    =<  |=  tub/nail  %.  tub                           ::  expose parsers
        ;~  plug
          ;~(sfix text col (star gah))
          (sear (flit |=(tape ?=(^ +<))) dest)
          ;~(sfix (opts ;~(pfix (plus gah) titl)) (star ace) eol)
        ==
    |%
    ++  dest
      ;~  pose
        (ifix gal^gar (star ;~(pose esc nore(rec "<>"))))
        (pexp nore(rec " "))
      ==
    ++  pexp
      |*  cha/rule
      =+  chu=;~(less pel per cha)
      |-                                                ::  XX  Chesterton fence
      %+  knee  *tape  |.  ~+
      ;~  pose
        ;~(plug chu ^$)
        ;~(plug pel (cook welp ;~(plug (star chu) per ^$)))
        (easy ~)
      ==
    ++  text
      =-  (ifix sel^ser (cook |=(a/tape (crip (cass a))) (star -)))
      ;~  pose
        (cook |=(a/char (cat 3 '\\' a)) esc)
        (cold ' ' (plus gah))
        ;~(less sel ser prn)
      ==
    ++  titl
      %.  ~[soq^soq doq^doq pel^per]
      |*  a/(pole {rule rule})
      ?~  a  fail
      ;~  pose
      (ifix -.a (star ;~(pose esc htm-enty ;~(less ->.a prn))))
        $(a +.a)
      ==
    --
  ++  consol
    |=  a/kids
    ^-  kids
    ?~  a  ~
    ?+    -.i.a  [i.a $(a t.a)]
        $$
      ?~  t.a  a
      ?:  ?=(_-.i.a -.i.t.a)
        $(a t.a(p.i (weld p.i.a p.i.t.a)))
      [i.a $(a t.a)]
    ==
  ++  pars                                      ::  XX    
    =>  |%
        ++  nufh  {tape kids}
        ++  fens  ?($'*' $'_')
        ++  nuft  {nufh ?(tape {tape ?($| fens)})}
        ++  reso
          |=  a/nufh
          ^-  kids
          ?~  -.a  +.a
          [[%$ -.a] +.a]  
        ::
        ++  veld
          |=  {a/$@(char inline) _[b c]=*nuft}
          ^-  nuft
          :_  c
          ?@  a
            [[a -.b] +.b]
          ?~  -.a
            [(weld p.a -.b) +.b]
          [~ a (reso b)]
        ::
        ++  rend
          |=  nuf/nufh
          %.  (reso nuf)
          |=  a/kids
          ^-  tape
          ?~  a  ~
          %+  weld  
            ?@  -.i.a
              ?+  -.i.a  <i.a>
                $~  p.i.a
              ==
            =+  [p q]=i.a
            ?+  -.p  "[{(trip -.p)}]<{$(a q)}>"
              $emph  "({$(a q)})"
            ==
          $(a t.a)  
        --
    |=  tap/tape  ^-  kids
    =.  tap  (trim-sp tap)
    =+  vex=(many 1^1 tap)
    ?~  q.vex
      ~
    =+  [a ~ b]=u.q.vex
    ?~(b a (welp a [[%$ b]]~))  
  ::
  ++  many
    =>  pars                            ::  XX  
    |=  tub/nail  ^-  (like kids)
    =-  [[0 0] ~ (reso -<) [0 0] ;;(tape ->)]           ::  XX
    =+  [ins=`?(fens $~)`~ bof=| noz=|]
    ?~  q.tub  [`~ ~]
    =+  [p=*char [i t]=[i t]:q.tub]                     ::  prev current next
    =<  ?<(&(?=({* @} +) !?=($~ +>)) .)                  ::  XX  do type stuff
    |^  ^-  nuft
        ?:  ?=(?($'*' $'_') i)
          dlim
        =+  elm=(;~(pose child ;~(pfix (star ace) nal) nore) [1 1] i t)
        ?~  q.elm
          done(t [i t])                                 ::  XX  next?
        =+  [a ~ b]=u.q.elm
        ~?  bug  [a b]
        (push(t b) a)
    ::
    ++  next  (push i)
    ++  push                                            ::  continue with head
      |=  a/$@(char inline)
      ^-  nuft
      ~?  ?@(a | bug)  fon+a
      ?~  t  (veld a done)
      =:  noz  &
          i  i ::?+(i i fens p)
        ==
      (veld a ^$(+< [i t]))
    ::
    ++  done  [[~ ~] ?~(ins t [t %|])]
    ::
    ++  pull                                            ::  consume chunk
      ^-  nuft
      ?>  ?=(fens i)                                    ::  XX  do type stuff
      =:  bof  ?~(ins | |(bof !=(ins i)))
          ins  i
          noz  |
        ==
      ?~  t  
        (veld i done)
      $(+<+ t)
    ::
    ++  flome
      |=  a/_+:*nuft
      ^-  {(unit ?($| fens)) tape}
      ?.  ?=({* ?($| fens)} a)
        [~ a]
      [[~ +.a] -.a]
    ::
    ++  empa
      |=  a/nufh
      ^-  inline
      [[%emph |] (reso a)]
    ::
    ++  ends
      |=  {a/tape b/tape}
      ?:  &(?=(^ b) =(i i.b))
        $(b t.b)
      ?:  &(?=(^ a) =(i i.a))
        $(a t.a)
      ?>  ?=(fens i)                                    ::  XX  do type stuff
      ?&  |(?=($~ b) !=(' ' i.b))
        |(=('*' i) [?=($~ q)]:(aln 1^1 a))
      ==
    ::
    ++  dlim
      ^-  nuft
      ~?  bug  [&3 &2]
      =+  clo=&(noz !=(~ `*`ins) (ends t p ~))      ::  can close delim
      ?:  &(clo =(`*`ins i))                           ::  correct close  
        [`~ t]
      ?:  &(bof !=(`*`ins i))                          ::  outer close
        ?>  ?=(fens i)                                  ::  XX  do type stuff
        [`~ t i]
      ?~  t
        (veld i done)
      ?.  (ends [p]~ t)
        next
      =+  [a tak]=pull
      =>  .(t `_""`t)                                ::  XX  do type stuff
      =^  b  t  (flome tak)
      ?~  b
        (push (empa a))
      ~?  >  bug  clot+[a i t]
      ?:  =(i `*`u.b)
        (push (empa a))
      ?~  ins                                           ::  outermost
        [a(- [i -.a]) t]    ::(veld i a t)
      [a(- [i -.a]) t u.b]  ::(veld i a t u.b)
    --
  ::
  --
--  
::
::::
  ::
|%
++  nesting  $%  {$bloq *}                              ::  Used for fishing
                 {$item *}
                 {$list {$item $~}}
             ==
++  accepting  ?($para $code)
++  list-nest                                           ::  can add list item?
  =+  sam=[?>(?=($list -.p..) p..)]:(tops [%list ~]~)   ::  XX do type stuff
  |=  {a/_sam b/_sam}  ^-  ?
  .=  ?@(q.a q.a q.q.a)               ::  by checking delimiter characters
  ?@(q.b q.b q.q.b)
:: 
::   =-  =((cha p.a) (cha p.b))
::   ^=  cha
::   |=  a=_sam
::   ?@(q.a q.a q.q.a)
::
++  closes-code
 =+  sam=[?>(?=($code -) p..)]:(node [%code ~])         ::  XX do type stuff
 |=  {a/_sam b/_sam}
 ?~  a  ?=($~ b)
 ?~  b  |
 ?^  r.u.b  |
 ?&  =(p.u.a p.u.b)
     (gte q.u.b q.u.a)
 ==
::
++  blank
   |=  a/tape  ^-  ?
   ?~  a  &
   &(=(' ' i.a) $(a t.a))
::
++  dehax                                                ::  strip trailing hax
  =+  nobas=;~(sfix (plus hax) ;~(pose ace end))
  |=  a/tape
  %-  flop
  :(strip nobas (star ace) (flop a))
::
++  scab
  |*  {a/tape b/rule}
  (wonk (b [1 1] a))
::
++  donp
  |%
  ++  blok  
    :~  %article   %aside  %blockquote  %body  %button  %canvas  %caption  %col
        %colgroup  %dd  %div  %dl  %dt  %embed  %fieldset  %figcaption  %figure
        %footer  %footer  %form  %h1  %h2  %h3  %h4  %h5  %h6  %header  %hgroup
        %hr      %iframe  %li  %map  %object  %ol  %output  %p  %pre  %progress
        %script  %section  %style    %table   %tbody   %td  %textarea    %tfoot
        %th   %thead  %tr  %ul  %video  
    ==
  ++  htm-head  =+  blu=(flit ~(has in (silt `wain`blok)))
                =+  blo=(sear blu (cook |=(a/tape (crip (cass a))) (star aln)))
                %+  stag  %html
                ;~  plug  gal
                  ;~  pose
                   ;~(plug blo ;~(pose fas gar gah))
                   ;~(plug fas blo ;~(pose gar gah))
                   (mask "?!")
                  ==
                ==
  ++  leaf  (leas ;~(pose head hrul fcode))             ::  any node
  ++  head  
    %+  cook  |=(a/tape [%head (lent a) ~]) 
    ;~(sfix (stun 1^6 hax) ;~(pose ace (ahed eol)))
  ::
  ++  hrul
    %+  cold  [%hrul ~]
    %.  ~[tar hep cab]  ::  (vary "*-_")
    |*  a/(pole rule)
    ?~  a   fail
    ;~(pose ;~(plug (lots 3 (whif -.a)) (ahed eol)) $(a +.a))
  ::
  ++  limar  ::  list marker
    %+  stag  %list
    %-  leas
    %+  stag  &
    =-  ;~(sfix - ;~(pose (ahed eol) ;~(sfix ace ;~(pose (leas) (easy)))))
    ;~  pose
      (mask "*+-")
      ;~(plug dem (mask ".)"))
    ==
  ::
  ++  line
    ;~(sfix (star prn) eol)
  ::
  ++  blomar
    %+  cold  [%bloq ~]
    %-  leas
    ;~  pose
      ;~(plug gar ace)
      gar
    ==
  ++  setext 
    %-  leas
    ;~(sfix ;~(pose (cold 2 (plus hep)) (cold 1 (plus tis))) (star ace))
  ++  icode  (cold `node`[%code ~ ~] (stun 4^4 ace))
  ++  fcode
    %.  ~['`' '~']   ::  (vary "`~")
    |*  a/(pole char) 
    ?~  a   fail
    =-  ;~(pose fel $(a +.a))
    ^=  fel
    %+  cook      
      |=  {b/(list) c/tape $~}
      ^+  [?>(?=($code -) .)]:*node                       ::  XX do type stuff
      [%code `[-.a (add 3 (lent b)) c] ~]
    ;~  plug
      (lots 3 (just -.a))
      (star ;~(less tec prn))
      (ahed eol)
    ==
  --
::
++  normalize
  |=  a/down  ^-  down
  %+  turn  a  |=  b/elem
  ?^  -.b  b(q (turn q.b ..$))
  =-  ?+(-.b b $para b(p (- p.b)), $head b(q (- q.b)))
  |=  c/kids  ^-  kids
  ?~  c  ~
  ?:  ?&  ?=(^ t.c)
          ?=($$ -.i.c)
          ?=($$ -.i.t.c)
      ==
    $(c t.c(p.i (weld p.i.c p.i.t.c)))
  =.  i.c  
    ?.(?=($$ -.i.c) i.c [%$ (trip (crip p.i.c))])       ::  XX  valid tapes
  :_  $(c t.c)
  ?@  -.i.c  i.c
  ?~  q.i.c  
    i.c
  ?.  ?&  ?=({* $~} q.i.c)
          ?=({$emph $|} -.i.c)  
          ?=({$emph $|} -.i.q.i.c)
      ==
    i.c(q $(c q.i.c))
  [[%emph %&] $(c q.i.q.i.c)]
::
++  parseb  =>(parse .(bug &))
++  parse
  =+  [bug=| bugi=|]
  |=  tub/nail
  =.  q.tub
    %+  scan  q.tub                   ::  tab hackery  ::  XX per line
    (star ;~(pose prn tab nal))
  =|  $:  $:  top/down                ::  finished toplevel elements
              {sap/@u nod/node}       ::  spacing currrent leaf block
              cur/stack               ::  stack of nested current blocks
          ==
          {bun/_| hat/_|}             ::  prev blank? halt?
          ref/(map cord {p/tape q/(unit tape)})        ::  link references
      ==
  |^  ^-  (like _top)
      ?.  hat
        $:eat-line
      ?^  cur
        $:pop
      =>  cull
      =-  [p.tub `[- tub]]
      (flop (turn top (proc-inline [-(bug bugi)]:[~(pars inli ref) .])))
  ::
  ++  self  .
  ::
  ++  halt  .(hat &)
  ::
  ++  debu  [&2 &2.-]:&2
  ::
  ++  proc-inline                                       ::  parse inline kids
    |=  pac/_pars:inli                               ::  cache
    |=  a/elem
    ?^  -.a  a(q (flop (turn q.a ..$)))
    ?+  -.a  a
      $code  
        ?~  p.a  a
        a(r.u.p (unesc:inli r.u.p.a))
      $para
        ?>  ?=({{$$ *} $~} p.a)                         ::  XX do type stuff
        a(p (pac p.i.p.a))
      $head
        ?~  q.a  a
        ?>  ?=({{$$ *} $~} q.a)                         ::  XX do type stuff
        a(q (pac p.i.q.a))
    ==
  ::
  ++  snack                                             ::  advance by parser
    |*  a/rule
    ^-  {(unit _(wonk (a))) nail}
    =+  vex=(a tub)
    ?~  q.vex  [~ tub]
    [`p q]:u.q.vex
  ::
  ++  snake                                             ::  advance with trace
    |*  fel/rule
    =-  (snack (here - fel))
    |*  {{{@ a/@u} {@ b/@u}} c/*}
    [p=(sub +<->+ +<-<+) q=c]
  ::
  ++  pop                                               ::  resolve container
    ^+  self
    =>  cull
    ?~  cur  self
    =-  =>  .(cur t.cur, q.p.tub p.i.cur)
        ?~  cur  self(top [hed top])
        self(q.q.i.cur [hed q.q.i.cur])
    ^-  hed/tops
    =+  cub=q.i.cur
    ?+    -.p.cub  cub
        $list
      %_    cub
          p.p
        p.p.cub  ::  XX set this upon parsing blank-headed block
      ==
    == 
  :: 
  ++  bye                                               ::  resolution arms
    |%
    ++  leaf                                            ::  resolve node
      ^+  self
      =^  nol  nod
        [nod [%defn ~]]
      ?:  ?=($defn -.nol)  self
      ~?  >  bug  clod+[nol tub]
      ?~  cur  self(top [nol top])
      self(q.q.i.cur [nol q.q.i.cur])
    ::
    ++  pop-til                                         ::  unwind stack
      |=  a/stack
      ^+  self
      ?~  cur  self
      ?:  =(a cur)  self
      $(self pop)
    ::
    ++  top-list
      =+  laz=cur
      |-
      ?~  cur  laz
      =.  laz  ?:(?=($list -.p.q.i.cur) cur laz)
      $(cur t.cur)
    ::
    ++  top-bloq
      =+  laz=cur
      |-
      ?~  cur  laz
      =.  laz  ?:(?=($bloq -.p.q.i.cur) cur laz)
      $(cur t.cur)
    --
  ::
  ++  cull                                              ::  resolve node block
    =<  leaf:bye
    ^+  self
    =.  sap  0
    ?+    -.nod  self
        $html
      self(p.nod (flop p.nod))
        $code
      =<  self(q.nod (flop q.nod))
      |-
      ?^  p.nod  .
      ?~  q.nod  .
      ?:  (blank (trip i.q.nod))  $(q.nod t.q.nod)
      .
        $para
      ?~  p.nod  self(nod [%defn ~])
      =+  olt=tub
      =.  q.tub
        %-  of-wall
        %+  turn 
          ;;((list {$$ p/tape}) (flop p.nod))         ::  XX do type stuff  
        |=({@ a/tape} a)
      |-  ^+  self
      =^  ren  tub  (snack (leas href):inli)
      ?^  ren
        ?:  (~(has by ref) -.u.ren)  $
        $(ref (~(put by ref) -.u.ren +.u.ren))  
      =.  q.tub  (strip (star gah) q.tub)
      ?~  q.tub  self(nod [%defn ~], tub olt)
      self(nod [%para [%$ q.tub]~], tub olt)
    ==
  ::
  ++  push                                              ::  open block
    |=  nit/(loca _p:*tops)  ^+  +>  
    =.  self  cull
    =+  toz=[q.nit ~]
    ?.  ?=({$list ^} q.nit)  
      (shove p.nit toz)
    =.  self  (shove p.nit toz)
    (shove p.nit [%item ~]~)
  ::
  ++  shove
    |=  a/(loca tops)  ^+  +>
    ?~  cur  +>(cur [a cur])
    ::  =*  cub  q.i.cur
    ?.  ?=(nesting [-.p.q.i.cur -.q.a])
      $(+> pop)
    +>(cur [a cur])
  ::
  ++  pump                                              ::  push leaf block
    |=  a/$^({p/node q/@u} node)
    ^+  +>
    =+  nex=cull
    ?@  -.a  nex(nod a)
    nex(nod p.a, sap q.a)
  ::
  ++  match                                             ::  check top element
    |=  a/elem  ^-  ?
    ?~  cur  |
    =(-.a -.q.i.cur)
  ::
  ::
  ++  collapse                                          ::  skim elems off top
    |=  a/(list (pair @ tops))  ^+  +>
    ?~  a  +>
    :: ?:  ?=([[@ *] ~] a)  +>
    ~?  bug  yank+[i.a blos]
    ?>  (match q.i.a)
     :: ~&  [%no-match a cur]
     :: !!
    $(a t.a, +> pop)
  ::
  ++  offset
    ^-  @u
    ?~  cur  0
    ?:  ?=($bloq -.p.q.i.cur)
      p.i.cur
    offset(cur t.cur)
  ::
  ++  delist  (pop-til top-list):bye
  ++  debloq
    |=  ruc/_(flop cur)
    ^+  self
    ?~  ruc  self
    ?.  ?=($bloq -.p.q.i.ruc)
      $(ruc t.ruc)
    (collapse (flop ^+(cur ruc)))  
  ::
  ++  nil-li  
    ?&  ?=($defn -.nod) 
        ?=(^ cur)
        ?=({{$item $~} $~} q.i.cur)
    ==
  ++  widen  ^+  cur                                    ::  list loosening
    =<  ?~  cur  ~
        ?.  ?=($item -.p.q.i.cur)
          (. cur)
        [i.cur (. t.cur)]
    |=  a/_cur  ^+  a
    ~?  >  bug  naro+[debu nil-li a cur]
    ?~  a  a
    ?:  ?=({{$item $~} $~} q.i.a)
      a
    ?.  ?=($list -.p.q.i.a)
      [i.a $(a t.a)]
    a(p.p.q.i |)
  ::
  ++  blos                                              ::  extract elem list
    (flop (turn cur |*({@ tops} +<+)))  
  ::
  ++  eat-line                                          ::  high-level line nom
    ^+  .  
    ~?  >>  bug  line+curlin
     :: =>  [bup=bun sepa:eat]
     :: ?:  bun:+  +                                      ::  blank line nommed
     :: =<  .(bun |)
     :: =.  bun  bup
    ~?  bug  line-stat+[debu cur]
    ?:  ?=($html -.nod)
      =+  sep=(sepa:eat)
      ?:  bun.sep  sep
      knot:eat.+
    =>  [ruc .]=olds:eat  
    ?:  &(?=($~ ruc) ?=({$code ^ *} nod))
      code:eat
    =+  sep=(sepa:eat ruc)
    ?:  bun.sep
      ~?  bug  nilli+[debu nil-li nod cur]:sep
      =.  bun.sep
        ?^(ruc & ?^(cur | !nil-li:sep))       ::  XX  Chesterton fence
      sep
    =<  .(bun |)
    =~  [ruc=ruc sep(bun bun)]
      (lazy:eat ruc)
      news:eat 
      node:eat
      knot:eat
    ::  ~?  bug  seated+[nod blos]  .
    ==
  ::
  ++  curlin  (scab q.tub (star prn))
  ++  eat
    |%
    ++  sepa                                            ::  consume blank line
      |=  ruc/_(flop cur)  ^+  self
      ?:  ?=({$code ^ *} nod)                           ::  ignore in blocks
        self
      =^  buf  tub  (snack ;~(sfix (star ace) nal))
      ?~  buf
        self(bun |)
      ~?  bug  seat+cur
      =.  self
        ?:  bun  
          delist
        =.  bun  &
        (debloq ruc)
      ?+    -.nod  self
        $para  cull
        $html  cull
        $code  =-  self(q.nod -)
               ?~  q.nod  q.nod
               [(crip (slag 4 u.buf)) q.nod]
      ==
    ::
    ++  horz                                            ::  horizontal rule
      ^+  self
      =^  neu  tub  (snack (leas hrul:donp))
      ?~  neu  self
      (pump u.neu)
    ::
    ++  olds                                            ::  previous nest levels
      =+  [ruc=(flop cur) ovs=0]
      |-  ^+  [ruc self]
      ?:  =(~ q.tub)
        [~ halt]
      ?~  ruc  [ruc self]
      ~?  bug  heat+[debu q.i.ruc cur]
      ?-    -.p.q.i.ruc
          $bloq
        =^  neu  tub  (snack blomar:donp)
        ?^  neu  $(ruc t.ruc, ovs p.i.ruc)
        [ruc self]
      ::
          $list
        ?~  t.ruc  !! 
          :: $(t.ruc [p.i.ruc [%item ~] ~]~)               ::  XX  why this
        ?>  ?=($item -.p.q.i.t.ruc)
        ~?  bug  leat+[p.i.t.ruc debu]
        =^  den  tub  (snack (stun [p p]:i.t.ruc ace))
        ?^  den  $(ruc t.t.ruc)
        ?.  =(self horz)  [ruc self]                    ::  XX  efficiency  
        ?:  ?=({$code ^ *} nod)
          [~ self]                                      ::  XX  correct?  
        =^  neu  tub  (snake limar:donp)
        ?~  neu  [ruc self]
        =>  .(q.u.neu ^+(p.q.i.ruc q.u.neu))            ::  XX do type stuff
        ?.  (list-nest p.q.i.ruc q.u.neu)
          =.  self  (collapse (flop ^+(cur ruc)))    
          [~ (push u.neu)]
        =.  self  (collapse (flop ^+(cur t.ruc)))
        [~ (push p.u.neu [%item ~])]
      ::
          $item
        !!
      ==
    ::
    ++  aces  |=(a/@u ^+(tub +:(snack (stun 0^a ace)))) ::  nom optional leading
    ::
    ++  lazy                                            ::  errant prev bloqs
      |=  ruc/(list (pair @ tops))
      ^+  self
      ~?  bug  laze+[debu ruc]
      ?.  ?=($para -.nod)
        (collapse (flop ruc))
      ?:  |([?=(^ -)]:lead [?=(^ -)]:(snack leaf:donp))     ::  XX  efficiency
        (collapse (flop ruc))
      :: =.  tub  +:(snack (star ace))
      self
        ::self(tub (aces p.i.cur))                          :: XX correct?
        :: =<  (collapse (flop ruc))
        :: |-  ^+  .
        :: ?~  ruc  ..$
        :: ?.  ?=([%bloq ~] -.q.i.ruc)
        ::   ..$
        :: $(ruc t.ruc)
    ::
    ++  lead                                            ::  enter into tops
      %-  snake
      =>  donp
      ;~(plug ;~(pose blomar limar))
    ::
    ++  news                                            ::  enter nest levels
      |-  ^+  self
      ?.  =(self horz)  self                   ::  XX  efficiency
      =^  neu  tub
        lead
      ?~  neu
        self
      =.  bun  |                                        ::  don't wide new lists
      $(self (push u.neu))
    ::
    ++  node                                            ::  consume leaf node
      ^+  self
      ~?  bug  neat+curlin  
      ::=.  self  ?.(bun self cull)
      ?^  [q]:((leas htm-head):donp tub)                ::  XX  efficiency
        (pump [%html ~])  
      =+  ^=  hez
          %+  stag  %heas
          ?.  ?=({$para {$$ *} $~} nod)
            fail
          ;~(plug setext:donp (cold p.nod (ahed eol)))
      =+  ico=?:(?=($para -.nod) fail icode:donp)
      =+  saf=q.p.+:(snack (star ace))          ::  XX  efficiency
      =^  neu  tub  
        (snack ;~(pose hez ico leaf:donp))
      ~?  bug  feat+[bun saf blos neu]
      =.  cur  
        ?.(bun cur widen)
      ?~  neu
        =.  tub  +:(snack (star ace))
        ?.  ?=($para -.nod)
          cull
        self
      ?+  -.u.neu  (pump u.neu)
        $heas   self(nod u.neu(- %head))     ::  replace extant para
        $code   ?^  p.u.neu
                  (pump u.neu (dec saf))
                ?:  ?=({$code $~ *} nod)
                  self
                (pump u.neu)
      ==
    ::
    ++  code
      ^+  self
      ?>  ?=({$code ^ *} nod)                          ::  XX  do type stuff
      ~?  bug  ceas+[sap]
      =.  tub  (aces sap)
      =+  [neu tup]=(snack ;~(sfix (leas fcode):donp eol))
      ?:  &(?=(^ neu) (closes-code p.nod p.u.neu))
        =.  q.nod  q.nod
        cull(tub tup)
      =^  buf  tub  (snack ;~(sfix (star ace) nal))
      ?^  buf  
        self(q.nod [(crip u.buf) q.nod])
      knot
    ::
    ++  knot                                          ::  raw text
      ^+  self
      ?:  =(~ q.tub)  halt
      =^  lin  tub
        (snack line:donp)
      ?~  lin 
        halt
      ~?  bug  adva+u.lin
      |-
      ?~  u.lin
        ?+  -.nod  cull
          $code  self
          $html  self(p.nod ['' p.nod])
        ==
      ?+  -.nod  (pump para+~[[%$ u.lin]])
        $para  self(p.nod :_(p.nod [%$ u.lin]))
        $head  ?^  q.nod  $(self cull)
               self(q.nod [[%$ (dehax u.lin)]]~)   
        $code  self(q.nod :_(q.nod (crip u.lin)))
        $html  self(p.nod :_(p.nod (crip u.lin)))
      ==
    ::
  --
--  --
  ==
