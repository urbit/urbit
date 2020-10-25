::  |pill: helper functions for making pills
::
/+  *tide
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
++  fake-arvo
  =,  format
  |=  bek=path
  =/  get  |=(a=path (to-wain .^(@t %cx :(weld bek a /hoon))))
  %-  of-wain
  ;:  welp
    ~['!:  =~']
    (get /lib/tide)
    (slag 1 (get /gen/pillarvo))
    ~['    ==' '']
  ==
::
++  coalesce
  |=  sys=path  ^-  @t
  %+  can  3
  :: ;:  weld
  %-  zing  ^-  (list (list octs))  :~
    (encode /sys/hoon/hoon ~ .^(@t %cx (welp sys /hoon/hoon)))
    ::(encode /sys/arvo/hoon ~ .^(@t %cx (welp sys /arvo/hoon)))
    (encode /sys/arvo/hoon ~ (fake-arvo (scag 3 sys)))
  ::
    ?:  &  ~
    %-  zing
    %+  turn  (module-ova sys)
    |=  [wire %veer van=@tasD pax=path src=@t]
    =.  pax  (weld (slag 3 pax) /hoon)
    (encode pax " %{?~(van '$' van) ~}" src)
  ::
    %-  zing
    %+  turn  (file-mimes (scag 3 sys) ~[/pill/sys/tests])
    |=  [pax=path ~ mite oct=octs]
    (encode pax ~ oct)
  ::
    (encode /wip-padding ~ (octo (lsh 3 4.500.000 '\0A')))
    (encode /'~' ~ '')  :: trailer, helps detect truncation, cap trailing zeroes
  ==
--
