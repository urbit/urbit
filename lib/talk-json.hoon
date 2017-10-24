::
::::  /lib/talk-json/hoon
  ::
/-    talk
::
::>  proposed rules for sur-json conversion, adhered to below:
::>  1. to save the devs from having to learn multiple interfaces, try to match
::>     the hoon structure as closely as possible, including attribute names.
::>  2. if these names are p/q/r, ~slightly more semantic naming is preferred~
::>     fix that in the sur file.
::>  2. when dealing with $%, the tag is the variable name. ie `{$x ...}` -> `x`
::>     for tagged unions with one piece of data, `x` holds that value.
::>     for tagged unions with multiple pieces of data, `x` is an object.
::>  3. lists and sets become arrays. maps become objects.
::>  4. stringify cells only when/in a way such that it benefits the majority of
::>     foreseen usecases.
::>
::>  q: should parsing be strict or forgiving? ie, accept "ship" and/or "~ship"?
::
|%
++  en-tape                                             ::>  sur to tape
  =,  talk
  |%
  ++  circ                                              ::>  circle
    |=  a/circle
    :(weld (scow %p hos.a) "/" (trip nom.a))
  ::
  ++  rang                                              ::>  range
    |=  a/range
    ?~  a  ~
    ;:  weld
      "/"  (scow hed.u.a)
      ?~  tal.u.a  ~
      (scow u.tal.u.a)
    ==
  ::
  ++  sorc                                              ::>  source
    |=  a/source
    (weld (circ cir.a) (rang ran.a))
  --
::
++  de-tape                                             ::>  tape to sur (parse)
  =,  talk
  |%
  ++  circ                                              ::>  circle
    ;~((glue fas) ;~(pfix sig fed:ag) urt:ab)
  ::
  ++  rang                                              ::>  range
    =/  pont
      ;~  pose
        (stag %ud dim:ag)
        %+  stag  %da
        %+  sear
          |=  a/coin
          ^-  (unit @da)
          ?.  ?=({$$ $da @da} a)  ~
          `q.p.a
        nuck:so
      ==
    =+  ;~  pose
          (cook some ;~(pfix fas pont))
          (easy ~)
        ==
    ;~  pose
      (cook some ;~(plug ;~(pfix fas pont) -))
      (easy ~)
    ==
  ::
  ++  sorc                                              ::>  source
    ;~(plug circ rang)
  --
::
++  enjs                                                ::>  sur to json
  =,  talk
  =,  enjs:format
  |%
  ::TODO  these first few should probably make their way
  ::      into the stdlib...
  ++  sa                                                ::>  set as array
    |*  {a/(set) b/$-(* json)}
    ^-  json
    [%a (turn ~(tap in a) b)]
  ::
  ++  mo                                                ::>  map as object
    |*  {a/(map) b/$-(* @t) c/$-(* json)}
    ^-  json
    =-  (pairs (turn ~(tap by a) -))
    |*  {k/* v/*}  [(b k) (c v)]
  ::
  ++  dank                                              ::>  tank
    |=  a/tank
    ?:  ?=($leaf -.a)  (frond %leaf (tape p.a))
    %+  frond  -.a
    %-  pairs
    ?-  -.a
        $palm
      :+  :-  %style
          %-  pairs  :~
            mid+(tape p.p.a)
            cap+(tape q.p.a)
            open+(tape r.p.a)
            close+(tape s.p.a)
          ==
        lines+a+(turn q.a dank)
      ~
        $rose
      :+  :-  %style
          %-  pairs  :~
            mid+(tape p.p.a)
            open+(tape q.p.a)
            close+(tape r.p.a)
          ==
        lines+a+(turn q.a dank)
      ~
    ==
  ::
  ++  cord                                              ::>  string from cord
    |=  a/@t
    s+a
  ::
  ++  mabe                                              ::>  null or value
    |*  {a/(unit) b/$-(* json)}
    ^-  json
    ?~(a ~ (b u.a))
  ::
  ::
  ++  pire                                              ::>  prize-reader
    |=  a/prize-reader
    ^-  json
    %-  pairs  :~
      :-  %gys
      =-  (pairs ~(tap by (~(run by gys.a) -)))
      |=((set (set circle)) (sa +< audi))
      ::
      nis+(mo nis.a (cury scot %p) cord)
    ==
  ::
  ++  rure                                              ::>  rumor-reader
    |=  a/rumor-reader
    ^-  json
    %+  frond  -.a
    ?-  -.a
      $glyph  (digy +.a)
      $nick   (dini +.a)
    ==
  ::
  ++  digy                                              ::>  diff-glyph
    |=  a/diff-glyph
    ^-  json
    %-  pairs  :~
      bin+b+bin.a
      gyf+s+gyf.a
      aud+(audi aud.a)
    ==
  ::
  ++  dini                                              ::>  diff-nick
    |=  a/diff-nick
    ^-  json
    (pairs who+(ship who.a) nic+s+nic.a ~)
  ::
  ++  ruso                                              ::>  rumor-story
    |=  a/rumor-story
    ^-  json
    %+  frond  -.a
    ?+  -.a  !!
      $new      (conf cof.a)
      ::  $bear not needed
      $config   (pairs cir+(circ cir.a) dif+(dico dif.a) ~)
      $status   %-  pairs  :~
                  cir+(circ cir.a)
                  who+(ship who.a)
                  dif+(disa dif.a)
                ==
      $remove   b+&
      $gram     (enve nev.a)
    ==
  ::
  ++  dico                                              ::>  diff-config
    |=  a/diff-config
    ^-  json
    %+  frond  -.a
    ?-  -.a
      $full     (conf cof.a)
      $source   (pairs add+b+add.a src+(sorc src.a) ~)
      $caption  s+cap.a
      $filter   (filt fit.a)
      $secure   s+sec.a
      $permit   (pairs add+b+add.a sis+(sa sis.a ship) ~)
      $remove   b+&
    ==
  ::
  ++  disa                                              ::>  diff-status
    |=  a/diff-status
    ^-  json
    %+  frond  -.a
    ?-  -.a
      $full       (stat sat.a)
      $presence   s+pec.a
      $human      (dihu dif.a)
      $remove     b+&
    ==
  ::
  ++  dihu                                              ::>  diff-human
    |=  a/diff-human
    ^-  json
    %+  frond  -.a
    ?-  -.a
      $full     (huma man.a)
      $handle   (frond %han (mabe han.a cord))
      $true     (frond %tru (mabe tru.a trun))
    ==
  ::
  ++  pack                                              ::>  package
    |=  a/package
    ^-  json
    %-  pairs  :~
      nes+a+(turn nes.a enve)  ::TODO  maybe map
      cos+(loby cos.a)
      pes+(crow pes.a)
    ==
  ::
  ++  circ                                              ::>  circle
    |=  a/circle
    ^-  json
    s+(crip (circ:en-tape a))
  ::
  ++  loby                                              ::>  lobby
    |=  a/lobby
    %-  pairs  :~
      loc+(conf loc.a)
      rem+(mo rem.a (cork circ:en-tape crip) conf)
    ==
  ::
  ++  conf                                              ::>  config
    |=  a/config
    ^-  json
    %-  pairs  :~
      src+(sa src.a sorc)
      cap+s+cap.a
      fit+(filt fit.a)
      con+(cont con.a)
    ==
  ::
  ++  sorc                                              ::>  source
    |=  a/source
    ^-  json
    s+(crip (sorc:en-tape a))
  ::
  ++  filt                                              ::>  filter
    |=  a/filter
    ^-  json
    (pairs cas+b+cas.a utf+b+utf.a ~)
  ::
  ++  cont                                              ::>  control
    |=  a/control
    ^-  json
    (pairs sec+s+sec.a ses+(sa ses.a ship) ~)
  ::
  ++  crow                                              ::>  crowd
    |=  a/crowd
    ^-  json
    %-  pairs  :~
      loc+(grop loc.a)
      rem+(mo rem.a tmp grop)
    ==
  ::
  ++  tmp
    |=  a/circle
    ^-  @t
    (crip (circ:en-tape a))
  ::
  ++  grop                                              ::>  group
    |=  a/group
    ^-  json
    (mo a (cury scot %p) `stat)
  ::
  ++  stat                                              ::>  status
    |=  a/status
    ^-  json
    (pairs pec+s+pec.a man+(huma man.a) ~)
  ::
  ++  huma                                              ::>  human
    |=  a/human
    ^-  json
    (pairs han+(mabe han.a cord) tru+(mabe tru.a trun) ~)
  ::
  ++  trun                                              ::>  truename
    |=  a/truename
    ^-  json
    (pairs fir+s+fir.a mid+(mabe mid.a cord) las+s+las.a ~)
  ::
  ++  enve                                              ::>  envelope
    |=  a/envelope
    ^-  json
    (pairs num+(numb num.a) gam+(gram gam.a) ~)
  ::
  ++  gram                                              ::>  telegram
    |=  a/telegram
    ^-  json
    %-  pairs  :~
      ::aut+ship
      ::TODO  can we avoid this code duplication somehow?
      uid+s+(scot %uv uid.a)
      aud+(audi aud.a)
      wen+(time wen.a)
      sep+(spec sep.a)
    ==
  ::
  ++  thot                                              ::>  thought
    |=  a/thought
    ^-  json
    %-  pairs  :~
      uid+s+(scot %uv uid.a)
      aud+(audi aud.a)
      wen+(time wen.a)
      sep+(spec sep.a)
    ==
  ::
  ++  spec                                              ::>  speech
    |=  a/speech
    ^-  json
    ::  only %url has just a single piece of data.
    ?:  ?=($url -.a)
      (frond %url s+(crip (apix:en-purl:html url.a)))
    %+  frond  -.a
    %-  pairs
    ?-  -.a
      $lin  ~[pat+b+pat.a msg+s+msg.a]
      $exp  ~[exp+s+exp.a res+a+(turn res.a dank)]
      $ire  ~[top+s+(scot %uv top.a) sep+(spec sep.a)]  ::TODO  @uv as number?
      $fat  ~[tac+(atta tac.a) sep+(spec sep.a)]
      $inv  ~[inv+b+inv.a cir+(circ cir.a)]
      $app  ~[app+s+app.a msg+s+msg.a]
    ==
  ::
  ++  atta                                              ::>  attache
    |=  a/attache
    ^-  json
    %+  frond  -.a
    ?-  -.a
      $name  (pairs nom+s+nom.a tac+(atta tac.a) ~)
      $text  s+(of-wain:format +.a)
      $tank  a+(turn +.a dank)
    ==
  ::
  ++  audi                                              ::>  audience
    |=  a/audience
    ^-  json
    (sa a circ)
  --
::
++  dejs                                                ::>  json to sur
  =,  talk
  =,  dejs-soft:format
  |%
  ::TODO  these first few should maybe make their way
  ::      into the stdlib...
  ++  re                                                ::>  recursive reparsers
    |*  {gar/* sef/_|.(fist)}
    |=  jon/json
    ^-  (unit _gar)
    =-  ~!  gar  ~!  (need -)  -
    ((sef) jon)
  ::
  ++  as                                                ::>  array as set
    |*  a/fist
    (cu ~(gas in *(set _(need *a))) (ar a))
  ::
  ++  dank                                              ::>  tank
    ^-  $-(json (unit tank))
    %+  re  *tank  |.  ~+
    %-  of  :~
      leaf+sa
      palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
      rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
    ==
  ::
  ::
  ++  pire                                              ::>  prize-reader
    ^-  $-(json (unit prize-reader))
    %-  ot  :~
      gys+(om (as (as circ)))
      nis+(op fed:ag so)
    ==
  ::
  ++  rure                                              ::>  rumor-reader
    ^-  $-(json (unit rumor-reader))
    %-  of  :~
      glyph+digy
      nick+dini
    ==
  ::
  ++  pack                                              ::>  package
    ^-  $-(json (unit package))
    %-  ot  :~
      nes+(ar enve)
      cos+loby
      pes+crow
    ==
  ::
  ++  digy                                              ::>  diff-glyph
    ^-  $-(json (unit diff-glyph))
    (ot bin+bo gyf+so aud+audi ~)
  ::
  ++  dini                                              ::>  diff-nick
    ^-  $-(json (unit diff-nick))
    (ot who+(su fed:ag) nic+so ~)
  ::
  ++  ruso                                              ::>  rumor-story
    ^-  $-(json (unit rumor-story))
    %-  of  :~
      new+conf
      ::  bear not needed
      config+(ot cir+circ dif+dico ~)
      status+(ot cir+circ who+(su fed:ag) dif+disa ~)
      remove+ul
      gram+enve
    ==
  ::
  ++  dico                                              ::>  diff-config
    ^-  $-(json (unit diff-config))
    %-  of  :~
      full+conf
      source+(ot add+bo src+sorc ~)
      caption+so
      filter+filt
      secure+secu
      permit+(ot add+bo sis+(as (su fed:ag)) ~)
      remove+ul
    ==
  ::
  ++  disa                                              ::>  diff-status
    ^-  $-(json (unit diff-status))
    %-  of  :~
      full+(ot pec+pres man+huma ~)
      presence+pres
      human+dihu
      remove+ul
    ==
  ::
  ++  dihu                                              ::>  diff-human
    ^-  $-(json (unit diff-human))
    %-  of  :~
      full+huma
      handle+(mu so)
      true+(mu trun)
    ==
  ::
  ::>  ||
  ::>  ||  %circles
  ::>  ||
  ::>    messaging targets and their metadata.
  ::+|
  ::
  ::TODO  maybe just an object?
  ++  circ                                              ::>  circle
    ^-  $-(json (unit circle))
    (su circ:de-tape)
  ::
  ++  loby                                              ::>  lobby
    ^-  $-(json (unit lobby))
    (ot loc+conf rem+(op circ:de-tape conf) ~)
  ::
  ++  conf                                              ::>  config
    ^-  $-(json (unit config))
    %-  ot  :~
      src+(as sorc)
      cap+so
      fit+filt
      con+cont
    ==
  ::
  ::TODO  maybe just an object?
  ++  sorc                                              ::>  source
    ^-  $-(json (unit source))
    (su sorc:de-tape)
  ::
  ++  filt                                              ::>  filter
    ^-  $-(json (unit filter))
    (ot cas+bo utf+bo ~)
  ::
  ++  cont                                              ::>  control
    ^-  $-(json (unit control))
    (ot sec+secu ses+(as (su fed:ag)) ~)
  ::
  ++  secu                                              ::>  security
    ^-  $-(json (unit security))
    (su (perk %black %white %green %brown ~))
  ::
  ++  crow                                              ::>  crowd
    ^-  $-(json (unit crowd))
    (ot loc+grop rem+(op circ:de-tape grop) ~)
  ::
  ++  grop                                              ::>  group
    ^-  $-(json (unit group))
    (op fed:ag stat)
  ::
  ++  stat                                              ::>  status
    ^-  $-(json (unit status))
    (ot pec+pres man+huma ~)
  ::
  ++  pres                                              ::>  presence
    ^-  $-(json (unit presence))
    (su (perk %gone %idle %hear %talk ~))
  ::
  ++  huma                                              ::>  human
    ^-  $-(json (unit human))
    (ot han+(mu so) tru+(mu trun) ~)
  ::
  ++  trun                                              ::>  truename
    ^-  $-(json (unit truename))
    (ot fir+so mid+(mu so) las+so ~)
  ::
  ::>  ||
  ::>  ||  %message-data
  ::>  ||
  ::>    structures for containing main message data.
  ::+|
  ::
  ++  enve                                              ::>  envelope
    ^-  $-(json (unit envelope))
    (ot num+ni gam+gram ~)
  ::
  ++  gram                                              ::>  telegram
    ^-  $-(json (unit telegram))
    %-  ot  :~
      aut+(su fed:ag)
      ::TODO  can we do anything about this duplication?
      uid+seri
      aud+audi
      wen+di
      sep+spec
    ==
  ::
  ++  thot                                              ::>  thought
    ^-  $-(json (unit thought))
    %-  ot  :~
      uid+seri
      aud+audi
      wen+di
      sep+spec
    ==
  ::
  ++  spec                                              ::>  speech
    ^-  $-(json (unit speech))
    %+  re  *speech  |.  ~+
    %-  of  :~
      lin+(ot pat+bo txt+so ~)
      url+(su aurf:de-purl:html)
      exp+(ot exp+so res+(ar dank) ~)
      ire+(ot top+seri sep+spec ~)
      fat+(ot tac+atta sep+spec ~)
      inv+(ot inv+bo cir+circ ~)
    ==
  ::
  ++  atta                                              ::>  attache
    ^-  $-(json (unit attache))
    %-  of  :~
      name+(ot nom+so tac+atta ~)
      text+(cu to-wain:format so)
      tank+(ar dank)
    ==
  ::
  ::>  ||
  ::>  ||  %message-metadata
  ::>  ||
  ::     structures for containing message metadata.
  ::+|
  ::
  ++  seri                                              ::>  serial
    ^-  $-(json (unit serial))
    (ci (slat %uv) so)
  ::
  ++  audi                                              ::>  audience
    ^-  $-(json (unit audience))
    (as circ)
  --
--
