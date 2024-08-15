/-  serv=sky-server
/*  feather
/*  s-k-y
/*  wi-nd
/*  hawk-icon
=<
|%
++  state  pro/%sig
++  poke   (sy %gift %bind-static-assets ~)
++  kids
  :+  ~  %z
  schema.serv
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?+    stud  ~|(bad-stud/stud !!)
        %gift
      [~ sig/!>(~)]
    ::
        %bind-static-assets
      :_  sig/!>(~)
      :~
        [#/[p/our.bowl]/static/feather %make %css `css/!>(feather) ~]
        [#/[p/our.bowl]/static/s-k-y %make %js `js/!>(s-k-y) ~]
        [#/[p/our.bowl]/static/wi-nd %make %js `js/!>(wi-nd) ~]
        [#/[p/our.bowl]/static/hawk-icon %make %png `png/!>(hawk-icon) ~]
        [#/[p/our.bowl]/static/sky-manifest %make %json `json/!>(~(manifest web bowl)) ~]
      ==
    ==
      ::
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :_
      ::  return pail
      :::-  %manx
      ::!>
      ::;div: initializing sky
      :-  %sig
      !>
      ~
    ::
    ::  return cards
    :~
      [~[p/our.bowl %home] %make %home ~ ~]
      [here.bowl %poke bind-static-assets/!>(~)]
    ==
  --
--
::
|%  ++  web  |_  =bowl:neo
++  manifest
  ^-  json
  %-  pairs:enjs:format
  :~
    ['name' s+'sky']
    ['description' s+'an urbit namespace viewer']
    ['start_url' s+'/sky']
    ['display' s+'standalone']
    ['background_color' s+'black']
    ['theme_color' s+'black']
    :+  'icons'  %a
    :~
      %-  pairs:enjs:format
      :~
        ['src' s+(crip "/blue/blue-mime/{(scow %p our.bowl)}/static/hawk-icon")]
        ['sizes' s+'196x196']
        ['type' s+'image/png']
      ==
    ==
  ==
--  --
