::
::::  /hoon/static/app
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
  |=  html-content=tape
  ^-  (list tape)
  =/  src-loc   (fand "src" html-content) 
  =/  href-loc  (fand "href" html-content)
  =/  src
  %+  turn  src-loc
  |=  loc=@
  ^-  tape
  =/  src-pre  (slag (add loc 5) html-content)
  (scag (need (find "\"" src-pre)) src-pre)
  =/  href
  %+  turn  href-loc
  |=  loc=@
  ^-  tape
  =/  href-pre  (slag (add loc 6) html-content)
  (scag (need (find "\"" href-pre)) href-pre)
  (weld src href)
::
+=  relative
  $%  [%root wir=wire ext=mark]
      [%here wir=wire ext=mark]
      [%up n=@ wir=wire ext=mark]
  ==
::
++  parse-relative
  |=  pax=tape
::  ^-  (unit relative)
  %+  scan  pax
  %+  cook
  |=  a=^
  ?:  ?=([%up @] -.a)
    ((soft relative) [%up ->.a +.a])
  ((soft relative) a)
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
::      (cold %umd (jest '.umd')) 
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
++  prep
  |=  old=(unit)
  :-  ~
  this(sta *state)
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  =/  b  ((hard tape) a)
  ~&  (parse-relative b)
  [~ this]
::
::  +poke-static-action
::
++  poke-static-action
  |=  act=action:static
  ^-  (quip move _this)
  ?-  -.act
    %build
  ?>  !?=(~ s.in.act)
  =.  s.out.act  [-.s.in.act s.out.act]
  ~&  in+[(flop s.in.act) ext.act]
  =/  sil  (find-file in.act ext.act)
  =/  wir=wire  /build/(scot %p (sham [in.act out.act]))
  =/  car  [%exec wir our.bol `[-.in.act sil]]
  :-  [ost.bol car]~ 
    %=  this
      waiting.sta  (~(put by waiting.sta) wir [in.act out.act])
    ==
  ==
::
::  +find-file: checks for file at .md .html .js .css and .hoon extensions
::              returns which silk we should send to ford (%bake or %file)
::
++  find-file
  |=  [loc=beam ext=mark]
  ^-  silk:ford
  ?>  !?=(~ s.loc)
  ::
  =/  try-exts=(list @tas)
    :~  %hoon
        %md
::        %umd
        %html
    ==
  ::  if ext is null then try the extensions defined above
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
  =/  pax  (weld (en-beam:format loc) [ext ~])
  =/  ark  .^(arch %cy pax)
  ?~  fil.ark
    ~_  leaf+"Error: Non-existent file: {<pax>}"  !!
  ext
  ::
  ?:  ?=(%hoon normed-ext)
    [%bake %hymn *coin loc]      :: XX TODO: cast to html?
  ?:  ?=(%md normed-ext)
    [%cast %hymn [%cast %down [%file loc(s [ext s.loc])]]]    :: XX TODO: cast to html?
  ?:  ?=(?(%html %js %css) normed-ext)
    [%file loc(s [ext s.loc])]
  ~_  leaf+"Error: Unsupported filetype: {<normed-ext>}"  !!
::
::  +coup
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
  ~&  -.gag
  ?>  ?=(%& -.gag)
  =/  bem=[in=beam out=beam]  (need (~(get by waiting.sta) wir))
  ?>  &(!?=(~ s.in.bem) !?=(~ s.out.bem))
  ::
  =/  file-contents   q.q.p.gag
  =/  file-type=mark  p.p.gag
  ::  
  :_  %=  this
        waiting.sta  (~(del by waiting.sta) wir)
        finished.sta  (~(put in finished.sta) in.bem)
      ==
  ::
  ^-  (list move)
  ?.  =(%hymn file-type)
    =.  s.out.bem  [file-type s.out.bem]
    :_  ~
    :*  ost.bol  %info  /write  our.bol
        (foal:space:userlib (en-beam:format out.bem) [file-type !>(file-contents)])
    ==
  ::  get all links from html
  ::
  =/  file-contents=tape  (en-xml:html ((hard manx) q.q.p.gag))
  =/  links=(list tape)  (find-links file-contents)
  ::  process list of links into new static-action's
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
  ::  write html file to new clay path
  ::
  =.  s.out.bem  [%html s.out.bem]
  ~&  out+(flop s.out.bem)
  :_  new-actions
  :*  ost.bol  %info  /write  our.bol
      (foal:space:userlib (en-beam:format out.bem) [%html !>((crip file-contents))])
  ==
--








