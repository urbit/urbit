|%
::  +provider: DNS service provider (gcloud only for now)
::
+$  provider
  $%  [%fcloud zone=@ta auth=[email=@t key=@t]]
      $:  %gcloud
          project=@ta
          zone=@ta
          ::  XX passed as params since we can't scry in +mule
          ::
          scry=[code=@t =hart:eyre secrets=@t]
          auth=(unit [access=@t refresh=@t])
      ==
  ==
::  +authority: responsibility for a DNS zone
::
+$  authority
  $:  :: dom: authority over a fully-qualified domain
      ::
      dom=turf
      :: pro: DNS service provider
      ::
      pro=provider
  ==
::  +target: a ship is bound to a ...
::
+$  target
  $%  :: %direct: an A record
      ::
      [%direct %if p=@if]
      :: %indirect: a CNAME record
      ::
      [%indirect p=ship]
  ==
::  +bound: an established binding, plus history
::
+$  bound
  $:  :: wen: established
      ::
      wen=@da
      ::  id:  binding UUID (unused by gcloud)
      ::
      id=@ta
      :: cur: current target
      ::
      cur=target
      :: hit: historical targets
      ::
      hit=(list (pair @da target))
  ==
::  +nameserver: a b s o l u t e  p o w e r
::
+$  nameserver
  $:  aut=authority
      bon=(map ship bound)
      dep=(jug ship (pair ship target))
  ==
--
