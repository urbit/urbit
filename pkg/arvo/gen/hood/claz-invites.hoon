::  |claz-invites: generate invite tickets for star's children
::
::    writes to .txt with lines in the format ~planet,~ticket,0xaddress
::
::    eg: |claz-invites ~marzod 1 10 %/example-invites/txt
::
/+  keygen
=,  ethereum
::
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [star=ship min-child=@ud max-child=@ud out=path ~]
        ~
    ==
?:  (gth min-child max-child)
  ~|  [%weird-range min=min-child max=max-child]
  !!
?:  (gth max-child 0xffff)
  ~|  [%max-beyond-planet-space max-child]
  !!
~&  'patience, slow derivation...'
:-  %kiln-info
:-  "wrote generated invites to {(spud out)}"
%-  some
%+  foal:space:userlib  out
:-  %txt
!>
%+  turn  (gulf min-child max-child)
|=  child=@ud
=/  who=ship  (cat 4 star child)
=/  ticket=@q  (end 3 8 (shas who eny))
=/  owner=address
  =<  addr.keys
  ::NOTE  ~zod because invite wallet convention
  (ownership-wallet-from-ticket:keygen ~zod 8^ticket ~)
%-  crip
;:  weld
  (scow %p who)  ","
  (slag 1 (scow %q ticket))  ","
  (address-to-hex:ethereum owner)
==
