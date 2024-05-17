/@  sky
/@  sky-settings
:-  [%sky %htmx]
|=  =sky
|=  =bowl:neo
^-  manx
|^
  shell
::
++  map-to-css-tape
  |=  m=(map @t @t)
  ^-  tape
  %-  zing
  %+  turn  ~(tap by m)
  |=  [key=@t val=@t]
  """
  --{(trip key)}: {(trip val)};
  """
++  shell
  =/  settings
    ^-  (unit sky-settings)
    =/  s  (~(get by kids.bowl) /settings)
    ?~  s  ~
    :-  ~
    !<  sky-settings
    q.u.s
  ;s-k-y.wf.hf(open "", hawks "{<slots.sky>}")
    ;*
    =<  p
    %^  spin  hawks.sky
          1
        |=  [=pith a=@]
      :_  +(a)
    ;ha-wk
      =slot  "s{<a>}"
      =here  (en-tape:pith:neo pith)
      ;div
        =hx-get  "/neo/hawk{(en-tape:pith:neo pith)}"
        =hx-trigger  "load"
        =hx-target  "this"
        =hx-swap  "outerHTML"
        ;div.wf.hf.fc.jc.ac.f2
          ; loading . . .
        ==
      ==
    ==
    ;style
      ;+  ;/
      ?~  settings
        ""
      """
      html \{
        {(map-to-css-tape u.settings)}
      }
      """
    ==
  ==
--
