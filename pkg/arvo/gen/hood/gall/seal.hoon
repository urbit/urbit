::  Grant or revoke permissions to a desk.
::
::::
/+  *generators
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [des=desk add=? per=(list perm:clay) ~]  ~
    ==
=+  .^(desks=(set desk) %cd /(scot %p p.bec)//(scot %da now))
?.  (~(has in desks) des)
  ((slog (crip "kiln: {<des>} doesn't exist") ~) ~)
[%kiln-pass [%pass /kiln/seal %arvo %c %seal des add (silt per)]]