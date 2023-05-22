|%
::  $soil: path on local ship, designating the root of a namespace
::  hierarchy, i.e. somewhere where shrubs are installed
::
::    Example: /books/a-book
+$  soil  pith
+$  land
  $%  [%sing =plot]
      [%mult =plat]
  ==
+$  plot
  $:  vax=vase
      case=@ud
      src=twig
  ==
+$  plat
  $:  vax=vase
      case=@ud
      src=twig
      data=(map iota *)
  ==
+$  card
  $%  [%give p=gift]
      [%tend p=bush]
  ==
+$  gift
  %+  pair  iota
  $%  [%add =twig content=vase] :: dependent typing to remove bad typing equivalent to a (~(put by))
      [%del ~]
      [%edit new-content=vase]
  ==


::  $twig: fully qualified path, designating a schema + implemtation
+$  twig  pith
+$  area  (pair ship soil)
::  $bath: fully qualified path, designating a foreign publication
+$  bath  pith
++  bowl
  $:  our=area
      now=time
      =twig
      ::  src=twig
  ==
+$  bush
  $%  [%poke =twig =area data=*] :: XX: expand
      :: if .scm is not available, queue poke until it is, and ensure
      :: validation on way out, this encourages subscription + poke
      :: unification because if a subscription is already open then
      :: pokes are always released instantaneously
      :: ex: [%poke /~zod/latest-case/notes/schema ~tondes-sitrym /books/lightning-rfcs/notes/(scot %da now) %add (some-theorycel-nonsense)] :: /pokes.hoon
      [%soak =pith]        :: start synchronisation
      [%wipe =pith]        :: stop synchronisation
      [%grow =twig =soil]  :: start publication, installing twig into plot
      [%trim =soil]            :: stop publication, removing plot
      [%seed ~]                    :: continue poke propagation
  ==
:: TODO: no outside listen
++  sing
  |$  [typ dif]
  $_  ^|
  |_  [=bowl val=typ kid=(map term vase)]
  ++  pull   |~(bush *(quip bush typ))
  ++  poke   |~(d=dif *(quip bush typ))
  ++  peek   |~(pax=path *cage)  :: TODO: flesh out scry-spec
  --
--
