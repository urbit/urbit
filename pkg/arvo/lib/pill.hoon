::  |pill: helper functions for making pills
::
^?
|%
::
+$  pill
  $:  boot-ova=*
      kernel-ova=(list unix-event)
      userspace-ova=(list unix-event)
  ==
::
+$  unix-event
  %+  pair  wire
  $%  [%wack p=@]
      [%whom p=ship]
      [%boot ? $%($>(%fake task:able:jael) $>(%dawn task:able:jael))]
      unix-task
  ==
::  +module-ova: vane load operations
::
::    sys: full path to /sys directory
::
++  module-ova
  |=  sys=path
  ^-  (list [wire [%veer term path cord]])
  %+  turn
    ^-  (list (pair term path))
    :~  [%$ /zuse]       ::  standard library
        [%a /vane/ames]  ::  network
        [%b /vane/behn]  ::  timer
        [%c /vane/clay]  ::  revision control
        [%d /vane/dill]  ::  console
        [%e /vane/eyre]  ::  http server
        [%g /vane/gall]  ::  applications
        [%i /vane/iris]  ::  http client
        [%j /vane/jael]  ::  identity and security
    ==
  |=  [=term =path]
  =/  pax  (weld sys path)
  =/  txt  .^(@ %cx (weld pax /hoon))
  [[%vane path] [%veer term pax txt]]
::
++  default-directories
  ^-  (list path)
  :~  /app    ::  %gall applications
      /gen    ::  :dojo generators
      /lib    ::  libraries
      /mar    ::  mark definitions
      /sur    ::  structures
      /sys    ::  system files
      /ted    ::  :spider strands
      /tests  ::  unit tests
      /web    ::  %eyre web content
  ==
::  +file-ovum: userspace filesystem load
::
::    bas: full path to / directory
::
++  file-ovum
  =/  directories  default-directories
  |=  bas=path  ^-  unix-event
  ::
  ::  hav: all user files
  ::
  =/  hav=mode:clay  (file-mimes bas directories)
  ~&  user-files+(lent hav)
  [[%$ %sync ~] [%into %$ & hav]]
::
++  file-mimes
  ::  sal: all spurs to load from
  ::
  |=  [bas=path sal=(list spur)]
  =|  hav/(list [path ~ mime])
  |-  ^+  hav
  ?~  sal  ~
  =.  hav  $(sal t.sal)
  ::
  ::  tyl: spur
  ::
  =/  tyl  i.sal
  |-  ^+  hav
  ::
  ::  pax: full path at `tyl`
  ::  lon: directory at `tyl`
  ::
  =/  pax  (weld bas (flop tyl))
  =/  lon  .^(arch %cy pax)
  ::  XX this serialization should use marks
  ::
  =?  hav  ?=(^ fil.lon)
      ::  XX  this whitelist needs to be reviewed
      ::
      ?.  ?=  ?($css $hoon $html $js $json $md $png $txt $udon $umd)
          -.tyl
        ::
        ::  install only files with whitelisted marks
        ::
        ~&  ignoring+pax
        hav
      ::
      ::  cot: file as plain-text octet-stream
      ::
      =;  cot  [[(flop `path`tyl) `[/text/plain cot]] hav]
      ^-  octs
      ?-    tyl
          {$json *}
        =/  dat  .^(json %cx pax)
        (as-octt:mimes:html (en-json:html dat))
      ::
          {$txt *}
        =/  dat  .^(wain %cx pax)
        (as-octs:mimes:html (of-wain:format dat))
      ::
          *
        =/  dat  .^(@t %cx pax)
        [(met 3 dat) dat]
      ==
  =/  all  ~(tap by dir.lon)
  |-  ^+  hav
  ?~  all  hav
  $(all t.all, hav ^$(tyl [p.i.all tyl]))
::
:: glass pill
::
++  escape
  |=  a=octs  ^-  (list octs)
  ?:  =('\0A/' (cut 3 [0 2] q.a))
    [(octo '\0A/\0A') $(a [(dec p.a) (rsh 3 1 q.a)])]
  |-  ^-  (list octs)
  =/  sep  (next-sep:bootstrap.tide 0 q.a)
  ?~  sep  [a]~
  ::~&  [u.sep `@t`(cut 3 [(dec u.sep) 20] q.a) (end 3 40 q.a)]
  :+  [u.sep (end 3 u.sep q.a)]  (octo '\0A/')
  $(p.a (sub p.a u.sep), q.a (rsh 3 u.sep q.a))
::
++  octo
  |=  a=$@(cord octs)  ^-  octs
  ?^(a a [(met 3 a) a])
::
++  encode
  |=  [a=path b=tape c=$@(cord octs)]  ^-  (list octs)
  ::~&  encode/a
  ?.  =([~ a] (rust (spud a) fel:stab))
    ~&(lost-path/(spat a) ~)
  [(octo (crip "\0a{(spud a)}{b}\0a")) (escape (octo c))]
::
++  collect-all
  |=  sys=path  ^-  @t
  %+  can  3
  :: ;:  weld
  %-  zing  ^-  (list (list octs))  :~
    (encode /sys/hoon/hoon ~ .^(@t %cx (welp sys /hoon/hoon)))
    (encode /sys/arvo/hoon ~ .^(@t %cx (welp sys /arvo/hoon)))
  ::
    %-  zing
    %+  turn  (module-ova sys)
    |=  [wire %veer van=@tasD pax=path src=@t]
    =.  pax  (weld (slag 3 pax) /hoon)
    (encode pax " %{?~(van '$' van) ~}" src)
  ::
    =/  userspace  (skip default-directories |=(a=spur =(a /sys)))
    ~&  userspace/`(list path)`userspace
    %-  zing
    %+  turn  (file-mimes (scag 3 sys) userspace)
    |=  [pax=path ~ mite oct=octs]
    =/  unfin
      |(=('' q.oct) !=('\0A' (cut 3 [(dec p.oct) 1] q.oct)))
    ?.  unfin  (encode pax ~ oct)
    (welp (encode pax " ~" oct) [(octo '\0A')]~)
  ::
    (encode /'~' ~ '')  :: trailer, helps detect truncation, cap trailing zeroes
  ==
::
++  fake-boot
  |=  [[our=ship now=time eny=@uv] txt=@t]  ^-  wain
  (fake-boot-pill [our now eny] [[[%9 2 0 1] |.((tide %pill txt))] ~ ~])
::
++  fake-boot-pill
  |=  [[our=ship now=time eny=@uv] pil=pill]  ^-  wain
  |^  (pretty (eventful (boot pil)))
  ++  boot  |=(pil=pill [.*(->.pil -<.pil) +.pil])
  ++  eventful
    |=  [arv=* mod=(list unix-event) usr=(list unix-event)]  ^-  (list ovum)
    =/  ins=(list unix-event)
      %+  welp
        [[/ %whom our] [/ %wack eny] mod]
      [[/$/term/1 [%boot & %fake our]] usr]
    |-  ^-  (list ovum)
    ?~  ins  ~  :: throw away arv
    =^  out  +>.arv  (apply +>.arv now i.ins)
    (weld out $(ins t.ins))
  ::
  ++  apply
    |=  [arv=* ven=[@da unix-event]]
    =|  pok=$-([@da ovum] ^)
    =/  kop  .*(arv +47.arv)
    ;;([(list ovum) *] .*(.(pok kop) !=((pok ven))))
  ::
  ++  pretty
    =|  [out=wain cur=tape]
    |=  a=(list ovum)  ^-  wain
    ?~  a  (flop out)
    ?+  p.q.i.a  $(a t.a, out :_(out (crip <p.q.i.a>)))
      %blit
        =>  .(q.i.a ;;($>(%blit gift:able:dill) q.i.a))
        =/  bis=(list blit:dill)  p.q.i.a
        |-
        ?~  bis  ^$(a t.a)
        ?+  -.i.bis  $(bis t.bis)
          %lin  $(bis t.bis, cur (tufa p.i.bis))
          %sag  $(bis t.bis, out :_(out (crip <%jam p.i.bis>)))
          %mor  $(bis t.bis, out :_(out (crip cur)))
        ==
    ==
  --
--
