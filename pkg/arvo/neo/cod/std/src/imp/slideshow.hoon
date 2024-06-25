/@  slideshow
/@  slideshow-diff
/-  sh=slideshow
^-  kook:neo
=<
|%
++  state  pro/%slideshow
++  poke   (sy %slideshow %slideshow-diff ~)
++  kids   *kids:neo
++  deps   *deps:neo
++  form
  ^-  form:neo
  |_  [=bowl:neo =aeon:neo =pail:neo]
  ++  poke
    |=  [=stud:neo vax=vase]
    ^-  (quip card:neo pail:neo)
    ?:  =(stud %slideshow)
      `slideshow/vax
    ?>  =(stud %slideshow-diff)
    =/  poke  !<(slideshow-diff vax)
    =/  state  !<(slideshow q.pail)
    :+  ~  %slideshow
    !>
    ?-  -.poke
      %mode  state(mode mode.poke)
      %slide  state(slide slide.poke)
    ==
  ++  init
    |=  pal=(unit pail:neo)
    :-  ~
    ?^  pal  u.pal
    :-  %slideshow
    !>
    ^-  slideshow
    :*
      default-doc
      'prose slide'
      `(render-slideshow:sh default-doc)
      %both
      0
    ==
  --
--
|%
++  default-doc
  '''
  # slideshow

  - write your slides in a single document
  - use the `---` to separate slides
  - udon/sail syntax supported

  ---

  # special classes

  - classes added in the top input form will apply to every slide individually
  - `slide` adds sizing and padding to slides
  - `prose` adds sizing and typography to headings, paragraphs, and lists
  - feather classes are also supported
    - learn more at /docs/guides/feather
  '''
--
