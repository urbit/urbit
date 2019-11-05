::  lifted directly from ford, should probably be in zuse
=<  parse-scaffold
=,  ford
|%
++  parse-scaffold
  |=  src-beam=beam
  ::
  =/  hoon-parser  (vang & (en-beam:format src-beam))
  |^  ::
      %+  cook
        |=  a=[@ud (list ^cable) (list ^cable) (list ^crane) (list hoon)]
        ^-  scaffold
        [[[p q] s]:src-beam a]
      ::
      %+  ifix  [gay gay]
      ;~  plug
      ::  parses the zuse version, eg "/?  309"
      ::
        ;~  pose
          (ifix [;~(plug net wut gap) gap] dem)
          (easy zuse)
        ==
      ::  pareses the structures, eg "/-  types"
      ::
        ;~  pose
          (ifix [;~(plug net hep gap) gap] (most ;~(plug com gaw) cable))
          (easy ~)
        ==
      ::  parses the libraries, eg "/+  lib1, lib2"
      ::
        ;~  pose
          (ifix [;~(plug net lus gap) gap] (most ;~(plug com gaw) cable))
          (easy ~)
        ==
      ::
        (star ;~(sfix crane gap))
      ::
        (most gap tall:hoon-parser)
      ==
  ::  +beam: parses a hood path and converts it to a beam
  ::
  ++  beam
    %+  sear  de-beam:format
    ;~  pfix
      net
      (sear plex (stag %clsg poor)):hoon-parser
    ==
  ::  +cable: parses a +^cable, a reference to something on the filesystem
  ::
  ::    This parses:
  ::
  ::      `library`       ->  wraps `library` around the library `library`
  ::      `face=library`  ->  wraps `face` around the library `library`
  ::      `*library`      ->  exposes `library` directly to the subject
  ::
  ++  cable
    %+  cook  |=(a=^cable a)
    ;~  pose
      (stag ~ ;~(pfix tar sym))
      (cook |=([face=term tis=@ file=term] [`face file]) ;~(plug sym tis sym))
      (cook |=(a=term [`a a]) sym)
    ==
  ::  +crane: all runes that start with / which aren't /?, /-, /+ or //.
  ::
  ++  crane
    =<  apex
    ::  whether we allow tall form
    =|  allow-tall-form=?
    ::
    |%
    ++  apex
      %+  knee  *^crane  |.  ~+
      ;~  pfix  net
        ;~  pose
          ::  `/~`  hoon literal
          ::
          (stag %fssg ;~(pfix sig hoon))
          ::  `/$`  process query string
          ::
          (stag %fsbc ;~(pfix bus hoon))
          ::  `/|`  first of many options that succeeds
          ::
          (stag %fsbr ;~(pfix bar parse-alts))
          ::  `/=`  wrap a face around a crane
          ::
          (stag %fsts ;~(pfix tis parse-face))
          ::  `/.`  null terminated list
          ::
          (stag %fsdt ;~(pfix dot parse-list))
          ::  `/,`  switch by path
          ::
          (stag %fscm ;~(pfix com parse-switch))
          ::  `/&`  pass through a series of mark
          ::
          (stag %fspm ;~(pfix pad parse-pipe))
          ::  `/_`  run a crane on each file in the current directory
          ::
          (stag %fscb ;~(pfix cab subcrane))
          ::  `/;`  passes date through a gate
          ::
          (stag %fssm ;~(pfix mic parse-gate))
          ::  `/:`  evaluate at path
          ::
          (stag %fscl ;~(pfix col parse-at-path))
          ::  `/^`  cast
          ::
          (stag %fskt ;~(pfix ket parse-cast))
          ::  `/*`  run a crane on each file with current path as prefix
          ::
          (stag %fstr ;~(pfix tar subcrane))
          ::  `/!mark/ evaluate as hoon, then pass through mark
          ::
          (stag %fszp ;~(pfix zap ;~(sfix sym net)))
          ::  `/mark/` passes current path through :mark
          ::
          (stag %fszy ;~(sfix sym net))
        ==
      ==
    ::  +parse-alts: parse a set of alternatives
    ::
    ++  parse-alts
      %+  wide-or-tall
        (ifix [lit rit] (most ace subcrane))
      ;~(sfix (star subcrane) gap duz)
    ::  +parse-face: parse a face around a subcrane
    ::
    ++  parse-face
      %+  wide-or-tall
        ;~(plug sym ;~(pfix tis subcrane))
      ;~(pfix gap ;~(plug sym subcrane))
    ::  +parse-list: parse a null terminated list of cranes
    ::
    ++  parse-list
      %+  wide-or-tall
        fail
      ;~(sfix (star subcrane) gap duz)
    ::  +parse-switch: parses a list of [path crane]
    ::
    ++  parse-switch
      %+  wide-or-tall
        fail
      =-  ;~(sfix (star -) gap duz)
      ;~(pfix gap net ;~(plug static-path subcrane))
    ::  +parse-pipe: parses a pipe of mark conversions
    ::
    ++  parse-pipe
      %+  wide-or-tall
        ;~(plug (plus ;~(sfix sym pad)) subcrane)
      =+  (cook |=(a=term [a ~]) sym)
      ;~(pfix gap ;~(plug - subcrane))
    ::  +parse-gate: parses a gate applied to a crane
    ::
    ++  parse-gate
      %+  wide-or-tall
        ;~(plug ;~(sfix wide:hoon-parser mic) subcrane)
      ;~(pfix gap ;~(plug tall:hoon-parser subcrane))
    ::  +parse-at-path: parses a late bound bath
    ::
    ++  parse-at-path
      %+  wide-or-tall
        ;~(plug ;~(sfix late-bound-path col) subcrane)
      ;~(pfix gap ;~(plug late-bound-path subcrane))
    ::  +parse-cast: parses a mold and then the subcrane to apply that mold to
    ::
    ++  parse-cast
      %+  wide-or-tall
        ;~(plug ;~(sfix wyde:hoon-parser ket) subcrane)
      ;~(pfix gap ;~(plug till:hoon-parser subcrane))
    ::  +subcrane: parses a subcrane
    ::
    ++  subcrane
      %+  wide-or-tall
        apex(allow-tall-form |)
      ;~(pfix gap apex)
    ::  +wide-or-tall: parses tall form hoon if :allow-tall-form is %.y
    ::
    ++  wide-or-tall
      |*  [wide=rule tall=rule]
      ?.  allow-tall-form  wide
      ;~(pose wide tall)
    ::  +hoon: parses hoon as an argument to a crane
    ::
    ++  hoon
      %+  wide-or-tall
        (ifix [lac rac] (stag %cltr (most ace wide:hoon-parser)))
      ;~(pfix gap tall:hoon-parser)
    --
  ::  +static-path: parses a path
  ::
  ++  static-path
    (sear plex (stag %clsg (more net hasp))):hoon-parser
  ::  +late-bound-path: a path whose time varies
  ::
  ++  late-bound-path
    ;~  pfix  net
      %+  cook  |=(a=truss a)
      =>  hoon-parser
      ;~  plug
        (stag ~ gash)
        ;~(pose (stag ~ ;~(pfix cen porc)) (easy ~))
      ==
    ==
  --
--
