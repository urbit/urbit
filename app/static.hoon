::
::::  /hoon/static/app
  ::
::
::  XX  REVIEW: should this be a generator?
::  - it makes sense, since it doesnt need to store state between calls, its just producing a result
::  - but we can't pass arbitrary run-time arguments to ford runes.... yet
::    and we need that to specify the input file
::  - but we could have a special fixed directory to place our input files, so that we don't
::    have to pass the location as an argument at runtime
::  - but then could you have multiple separate static builds?
::  - let it take all files in a directory, build them as html
::    spider the links, then keep a map from all top-level files a set of their connected files
::  - then let the generator args specify which top-level file to write out and where to write it
::  - how do we know what a top-level file is? or can we treat all files as top levels files
::    mapped to their corresponding subset of linked files?
::
::
/?  309
/-  static
|%
::::
::::  structures
::::
+=  move  [bone card]
+=  card 
  $%  [%poke wire dock poke]
      [%exec wire ship (unit bilk:ford)]
      [%info wire ship toro:clay]
  ==
+=  poke
  $%  [%static-action action:static]
  ==
::::
::::  utilities
::::
::
::  +find-links: takes a manx and returns a list of all href and src links in it
::
++  find-links
  |=  max=manx
  ^-  (list tape)
  %+  weld
  ::
  %+  roll  a.g.max
  |=  [[n=mane v=tape] out=(list tape)]
  ?:  ?=(?(%href %src) n)
    [v out]
  out
  ::
  %+  roll  c.max
  |=  [m=manx out=(list tape)]
  (weld out (find-links m))
::
+=  relative
  $%  [%root wir=wire ext=mark]
      [%here wir=wire ext=mark]
      [%up n=@ wir=wire ext=mark]
  ==
::
++  parse-relative
  |=  pax=tape
  ^-  (unit relative)
  %+  rust  pax
  %+  cook
  |=  a=^
  ?:  ?=([%up @] -.a)
    ((hard relative) [%up ->.a +.a])
  ((hard relative) a)
  ;~  plug
  ::
    ;~  pose  
      (cold %root fas)
    ::
      (cold [%up 1] ;~(plug dot fas))
    ::
      %+  cook
      |=  a=(list %up)
      [%up +((lent a))]
      (plus (cold %up ;~(plug dot dot fas)))
    ::
      (cold [%up 1] (easy ~))
    ==
  ::
    %+  cook
    |=  a=(list coin)
    %+  turn  a
    |=  b=coin
    ~(rent co b)
    (more fas nuck:so)
  ::
    ;~  pose 
      (cold %html (jest '.html')) 
      (cold %md (jest '.md')) 
      (cold %umd (jest '.umd')) 
      (cold %hoon (jest '.hoon')) 
      (cold %js (jest '.js')) 
      (cold %css (jest '.css')) 
      (easy %$)
    ==
  ==
::
++  process-relative
  |=  [loc=beam rel=relative]
  ^-  [loc=beam ext=mark]
  |-
  ?-    -.rel
      %root
    [loc(s (flop wir.rel)) ext.rel]
  ::
      %here
    [loc(s (flop (weld (flop s.loc) wir.rel))) ext.rel]
  ::
      %up
    %=  $
      s.loc  (slag n.rel s.loc)
      rel  [%here wir.rel ext.rel]
    ==
  ==
::
+=  state
  $:  waiting=(map wire [in=beam out=beam])
      finished=(set beam)
  ==
--
::::
::::  app proper
::::
|_  [bol=bowl:gall sta=state]
::
++  this  .
::
::  +prep: we don't need no state
::
++  prep
  |=  old=(unit)
  :-  ~
  this(sta *state)
::
::  +poke-static-action: send build request to ford and wait for result
::
++  poke-static-action
  |=  act=action:static
  ^-  (quip move _this)
  ?-  -.act
    %build
  ?>  !?=(~ s.in.act)
  =.  s.out.act  [-.s.in.act s.out.act]
  =/  sil  (find-file in.act ext.act)
  =/  wir=wire  /build/(scot %p (sham [in.act out.act]))
  =/  car  [%exec wir our.bol `[-.in.act sil]]
  :-  [ost.bol car]~ 
    %=  this
      waiting.sta  (~(put by waiting.sta) wir [in.act out.act])
    ==
  ==
::
::  +find-file: checks for file at %umd, %md, %html, %js, %css, and %hoon extensions
::              returns a silk to send to ford
::
++  find-file
  |=  [loc=beam ext=mark]
  ^-  silk:ford
  ?>  !?=(~ s.loc)
  ::
  =/  try-exts=(list @tas)
    :~  %hoon
        %md
        %umd
        %html
    ==
  ::  if ext is unspecified then try the extensions defined in try-exts
  ::
  =/  normed-ext=@tas
  ?:  =(%$ ext)
    |-
    ?~  try-exts
      ~_  leaf+"Error: Non-existent file: {<(en-beam:format loc)>}"  !!
    =/  pax  (weld (en-beam:format loc) [i.try-exts ~])
    =/  ark  .^(arch %cy pax)
    ?~  fil.ark
      $(try-exts t.try-exts)
    i.try-exts
  ::  otherwise just try the extension given
  ::
  =/  pax  (weld (en-beam:format loc) [ext ~])
  =/  ark  .^(arch %cy pax)
  ?~  fil.ark
    ~_  leaf+"Error: Non-existent file: {<pax>}"  !!
  ext
  ::  form silk for the given mark
  ::
  ?:  ?=(%hoon normed-ext)
::    [%cast %hymn [%bake %noun *coin loc]]
    [%cast %hymn [%ride [%cnts p=~[[%.y p=1]] q=~] [%core loc]]]
  ?:  ?=(%md normed-ext)
    [%cast %elem [%cast %down [%file loc(s [normed-ext s.loc])]]]
  ?:  ?=(%umd normed-ext)
    [%cast %elem [%file loc(s [normed-ext s.loc])]]
  ?:  ?=(%html normed-ext)
    [%cast %hymn [%file loc(s [normed-ext s.loc])]]
  ?:  ?=(?(%js %css) normed-ext)
    [%file loc(s [normed-ext s.loc])]
  ~_  leaf+"Error: Unsupported filetype: {<normed-ext>}"  !!
::
::  +coup: handle errors
::
++  coup
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  ?~  err
    [~ this]
  (mean u.err)
::
::  +made: process ford result
::
++  made
  |=  [wir=path hash=@uvJ gag=gage:ford]
  ^-  (quip move _this)
  ::  if ford build failed, print out error and crash
  ::  otherwise, assert we got single result, not a table
  ::
  ?:  ?=(%| -.gag)
    ~_  p.gag  !!
  ?>  ?=(%& -.gag)
  ::  retrieve the build in/out beams  
  ::
  =/  bem=[in=beam out=beam]  (need (~(get by waiting.sta) wir))
  ?>  &(!?=(~ s.in.bem) !?=(~ s.out.bem))
  ::
  =/  file-contents   q.q.p.gag
  =/  file-type=mark  p.p.gag
  ::  finish by removing build from waiting list
  ::  and adding it to our finished list, to prevent infinite build loops
  ::
  :_  %=  this
        waiting.sta   (~(del by waiting.sta) wir)
        finished.sta  (~(put in finished.sta) in.bem)
      ==
  ::
  ^-  (list move)
  ::  if js or css, pass it through without processing it
  ::
  ?.  ?=(?(%hymn %elem) file-type)
    ?>  ?=(?(%js %css) file-type)
    =.  s.out.bem  [file-type s.out.bem]
    =/  pax  (en-beam:format out.bem)
    ?>  ?=(@t file-contents)
    =/  fol  (foal:space:userlib pax [file-type !>(file-contents)])
    :_  ~
    [ost.bol %info /write our.bol fol]
  ::  get all links from manx
  ::
  =/  file-contents=manx  ((hard manx) q.q.p.gag)
  =/  links=(list tape)  (find-links file-contents)
  ::  process list of links into list of static-action
  ::
  =/  new-actions=(list move)
  %+  roll  links
  |=  [link=tape output=(list move)]
  =/  parsed  (parse-relative link)
  ?~  parsed
    output
  =/  new-in   (process-relative in.bem u.parsed)
  =/  new-out  (process-relative out.bem u.parsed)
  =.  s.loc.new-out  (slag 1 s.loc.new-out)
  ?:  (~(has in finished.sta) loc.new-in)
    output
  :_  output
  [ost.bol %poke /act [our.bol dap.bol] %static-action [%build loc.new-in loc.new-out ext.new-in]]
  ::  write html output file to clay
  ::
  =.  s.out.bem  [%html s.out.bem]
  ~&  (flop s.out.bem)
  =/  pax  (en-beam:format out.bem)
  =/  fol  (foal:space:userlib pax [%html !>((crip (en-xml:html file-contents)))])
  :_  new-actions
  [ost.bol %info /write our.bol fol]
--



