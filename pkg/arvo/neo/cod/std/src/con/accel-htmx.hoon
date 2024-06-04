/@  accel
/@  accel-cell
/@  accel-conf
/@  htmx
:-  [%accel %$ %htmx]
|=  =accel
|=  =bowl:neo
|^
  ;div.accel-top.wf.hf
    ;+  table
    ;+  dashboard-stub
    ;+  refresher
    ;+  style
  ==
++  id
  ^-  tape
  %-  zing
  %+  turn  (pout (tail here.bowl))
  |=  smeg=@ta
  %+  weld  "--"
  (trip smeg)
  ::
++  table
  ;div
  ;table
    =id  "table-{id}"
    ;tbody
      ;tr
        ;th.mono;
        ;*
        %+  turn  (gulf 1 10)
        |=  n=@
        ;th.mono.f2.tc: {<n>}
      ==
      ;*
      %+  turn  (gulf 1 10)
      |=  x=@
      ;tr
        ;td.mono.tr.f2(style "width: 1px;"): {<x>}
        ;*
        %+  turn  (gulf 1 10)
        |=  y=@
        =/  pax=pith:neo  #/[ud/x]/[ud/y]
        =/  kid  bowl
        =.  kids.kid  [~ ~]
        =.  here.kid  :(welp here.bowl pax)
        =.  deps.kid  ~
        =/  in=manx
          ?~  res=(~(get of:neo kids.bowl) (snoc pax %in))
            *manx
          =.  here.kid  (snoc here.kid %in)
          (!<(htmx q.pail.u.res) kid)
        =/  out=manx
          ?~  res=(~(get of:neo kids.bowl) (snoc pax %out))
            *manx
          =.  here.kid  (snoc here.kid %out)
          (!<(htmx q.pail.u.res) kid)
        ;td.border
          ;+
          ;button.b1.scroll-none.hover.cell-btn.p2.wf.hf
            =id  "cell-{id}-{<x>}-{<y>}"
            =hx-get  "/neo/hawk{(en-tape:pith:neo here.bowl)}/{<x>}/{<y>}/in"
            =hx-target  "#dashboard-{id}"
            =hx-select  "form"
            =hx-swap  "innerHTML"
            =morph-retain  "class"
            =onclick  "$('.cell-btn').removeClass('toggled');$(this).addClass('toggled');"
            ;div.mono
              ;+  out
            ==
          ==
        ==
      ==
    ==
  ==
  ==
++  dashboard-stub
  ;div.b0.fc
    =style  "grid-area: dashboard;"
    =id  "dashboard-{id}"
    ;
  ==
++  refresher
  ;div.hidden
    =hx-get  "{(en-tape:pith:neo :(weld /neo/hawk here.bowl))}"
    =hx-trigger  "every 3s"
    =hx-target  "table"
    =hx-select  "table"
    =hx-swap  "morph"
    ;
  ==
++  style
  ;style.hidden
    ;+  ;/  %-  trip
    '''
    table {
      border-collapse: collapse;
      width: 100%;
      grid-area: table;
    }
    .scroll-none {
      overflow-x: auto;
    }
    td, tr {
      margin: 0;
      padding: 0;
      height: fit-content;
    }
    .accel-top {
      display: grid;
      grid-template-rows: 1fr min(auto, 300px);
      grid-template-columns: 1fr;
      grid-template-areas:
        "table"
        "dashboard";
    }
    '''
  ==
--
