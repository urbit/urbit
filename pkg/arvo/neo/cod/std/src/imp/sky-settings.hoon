/@  sky-settings
|%
++  state  pro/%sky-settings
++  poke   (sy %sky-settings ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    `pail
  ++  init
    |=  pal=(unit pail:neo)
    ^-  (quip card:neo pail:neo)
    :-  ~
    :-  %sky-settings
    !>
    :*
      %-  malt
      ^-  (list [@t @t])
      :~
        :-  'font'  'Urbit Sans, sans-serif'
        :-  'font-mono'  'monospace'
        :-  'mono-scale'  '0.8'
        :-  'letter-spacing'  '0.024em'
        :-  'line-height'  '1.4'
        :-  '1in'  '4px'
        ::
        :-  'sky-opacity'  '0.88'
        :-  'sky-bg-size'  'contain'
        :-  'sky-bg-url'  ''
        :-  'sky-outer-gap'  '8px'
        :-  'sky-inner-gap'  '8px'
        ::
        :-  'light-b-3'  '#dd5522'
        :-  'light-b-2'  '#ddaa33'
        :-  'light-b-1'  '#55dd33'
        :-  'light-b0'  '#dddddd'
        :-  'light-b1'  '#cccccc'
        :-  'light-b2'  '#bbbbbb'
        :-  'light-b3'  '#aaaaaa'
        :-  'light-b4'  '#999999'
        :-  'light-f-3'  '#993311'
        :-  'light-f-2'  '#aaaa22'
        :-  'light-f-1'  '#339911'
        :-  'light-f0'  '#111111'
        :-  'light-f1'  '#333333'
        :-  'light-f2'  '#444444'
        :-  'light-f3'  '#555555'
        :-  'light-f4'  '#777777'
        ::
        :-  'dark-b-3'  '#551111'
        :-  'dark-b-2'  '#555511'
        :-  'dark-b-1'  '#225511'
        :-  'dark-b0'  '#222222'
        :-  'dark-b1'  '#333333'
        :-  'dark-b2'  '#444444'
        :-  'dark-b3'  '#555555'
        :-  'dark-b4'  '#666666'
        :-  'dark-f-3'  '#ee7755'
        :-  'dark-f-2'  '#ccbb33'
        :-  'dark-f-1'  '#55cc33'
        :-  'dark-f0'  '#eeeeee'
        :-  'dark-f1'  '#cccccc'
        :-  'dark-f2'  '#bbbbbb'
        :-  'dark-f3'  '#aaaaaa'
        :-  'dark-f4'  '#888888'
      ==
    ==
  --
--
