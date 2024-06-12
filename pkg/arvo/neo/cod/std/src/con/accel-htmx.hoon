/@  accel
/@  accel-cell
/@  accel-conf
/@  htmx
/-  feather-icons
:-  [%accel %$ %htmx]
|=  =accel
|=  =bowl:neo
|^
  ;div.top.wf.hf
    =style
      """
      display: grid;
      grid-template-rows: 1fr 200px;
      grid-template-columns: 1fr;
      overflow: hidden;
      grid-template-areas:
        "table"
        "dashboard";
      """
    ;+  table
    ;+  dashboards
    ;+  refresher
    ;+  script
  ==
++  resizer
  ;form.fr.ac.je.g1.p2
    =hx-post  "/neo/sky"
    =hx-confirm  "not yet implemented. bother ~migrev about this"
    ;input.p-1.br1.bd1.b0
      =style  "width: 80px;"
      =type  "number"
      =min  "2"
      =max  "20"
      =step  "1"
      =value  "10"
      ;
    ==
    ;div: x
    ;input.p-1.br1.bd1.b0
      =style  "width: 80px;"
      =type  "number"
      =min  "2"
      =max  "20"
      =step  "1"
      =value  "10"
      ;
    ==
    ;button.loader.p-1.br1.bd1.b1.hover
      ;span.loaded: resize
      ;span.loading
        ;+  loading.feather-icons
      ==
    ==
  ==
++  table
  ;div.scroll-x.scroll-y
    =style  "grid-area: table"
    ;+  resizer
    ;+  grid
  ==
++  rows  10
++  cols  10
++  grid
  ;div
    =style
      """
      width: max-content;
      display: grid;
      grid-template-rows: repeat({<+(rows)>}, auto);
      grid-template-columns: repeat({<+(cols)>}, fit-content(220px));
      """
    ;*
    %+  turn  (gulf 0 cols)
    |=  n=@
    (index n)
    ;*
    %+  turn  (gulf 0 (dec (mul rows +(cols))))
    |=  i=@
    ?:  =(0 (~(sit fo +(cols)) i))
      (index +((div i +(cols))))
    =/  d  (dvr i +(cols))
    =/  x  q.d
    =/  y  +(p.d)
    (cell x y)
  ==
++  out-preview
  ::  XX  cool kids would use a role conversion...
  ::  YY  or maybe cool kids prefer locality of behaviour
  ::  ZZ  SOMEONE EXPLAIN ME POLYMORPHISM NOW!
  |=  =pail:neo
  ^-  manx
  ?+    p.pail
        ;div: bruh, how did that a {<p.pail>} stud even get in here?
      %vase
    =/  vax  q.pail
    ?:  =(->-:vax %n)  ;/("")  ::  empty if cell is null
    ;/((of-wall:format (~(win re (sell vax)) 0 50)))
  ::
      %tang
    ;div.f-3: ERROR
  ==
++  cell
  |=  [x=@ y=@]
  =/  pix=pith:neo  #/[ud/x]/[ud/y]/out
  ;button.p-1.bd1.b0.hover.tl.mono.cell-btn
    =morph-retain  "class"
    =onclick  "accelRevealDashboard(this, '{<x>}', '{<y>}')"
    =style
      """
      min-height: 30px;
      max-height: 100px;
      min-width: 50px;
      max-width: 400px;
      box-sizing: border-box;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      """
    ;+
    ?~  res=(~(get of:neo kids.bowl) pix)
      ;div.f-2: EMPTY
    (out-preview q.saga.u.res)
  ==
++  index
  |=  n=@
  ?:  =(0 n)  ;div;
  ;div.fc.jc.ac.b0.p2.f3.s-2: {<n>}
++  dashboards
  ;div.b0.fc
    =style  "border-top: 1px solid var(--f2); max-height: 200px;"
    ;*
    %+  turn  (gulf 0 (dec (mul rows cols)))
    |=  i=@
    =/  d  (dvr i cols)
    =/  x  +(q.d)
    =/  y  +(p.d)
    (dashboard x y)
    ::
    ;div.grow.fc.jc.ac.f3
      =cell  "empty"
      =morph-retain  "class"
      ; select a cell
    ==
  ==
++  dashboard
  |=  [x=@ y=@]
  ;div.grow.fr.af.js.hidden
    =style  "max-height: 200px;"
    =cell  "{<x>}-{<y>}"
    =morph-retain  "class"
    ;+  (out x y)
    ;+  (in x y)
  ==
++  in
  |=  [x=@ y=@]
  =/  pax=pith:neo  #/[ud/x]/[ud/y]
  =/  kid=bowl:neo  bowl
  =.  kids.kid  [~ ~]
  =.  here.kid  :(welp here.bowl pax /in)
  =.  deps.kid  ~
  ;div.basis-half.bd1
    =style  "min-width: 350px;"
    ;+
    ?~  res=(~(get of:neo kids.bowl) (snoc pax %in))
      ;div: none - in {<[x y]>}
    (!<(htmx q.pail.u.res) kid)
  ==
++  out
  |=  [x=@ y=@]
  =/  pax=pith:neo  #/[ud/x]/[ud/y]/out
  =/  kid=bowl:neo  bowl
  =.  kids.kid  [~ ~]
  =.  here.kid  (welp here.bowl pax)
  =.  deps.kid  ~
  ;div.basis-half.bd1.fc.g2
    =style  "min-width: 350px;"
    ;div.b0.fr.g2.ac.je.p-1.bd1
      ;div.s-1: {(en-tape:pith:neo (snip pax))}
      ;button.p-1.b1.hover.br1.bd1.s-2
        =onclick  "navigator.clipboard.writeText('{(en-tape:pith:neo here.kid)}');"
        ; copy full path
      ==
    ==
    ;div.p2.pre.mono.scroll-y.scroll-x
      ;+
      ?~  res=(~(get of:neo kids.bowl) pax)
        ;div: none out - {<[x y]>}
      (!<(htmx q.pail.u.res) kid)
    ==
  ==
++  refresher
  ;div.hidden
    =hx-get  "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}?no-save"
    =hx-trigger  "every 3s"
    =hx-target  "closest .top"
    =hx-select  ".top"
    =hx-swap  "morph"
    ;
  ==
++  script
  ;script
    ;+  ;/  %-  trip
    '''
    function accelRevealDashboard(el, x, y) {
      let top = $(el).closest('.top');
      top.find('.cell-btn').removeClass('toggled');
      $(el).addClass('toggled');
      let dashboards = top.find('[cell]');
      dashboards.addClass('hidden');
      dashboards.filter(`[cell='${x}-${y}']`).removeClass('hidden');
    }
    '''
  ==
--
