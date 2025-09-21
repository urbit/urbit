/+  noun-diff
|%
++  clog
  |$  [stut]
  $%
    [%flush stut]
    [%drain patch:noun-diff]
  ==
++  sink
  |*  pats=(list path)
  |*  stat=*  
  |@    
  ++  sync 
    |=  [stat=_stat]
    ^-  [card:agent:gall _..sync]
    =/  dif
      %+  diff:noun-diff  ^stat  stat
    :-
    [%give %fact pats %noun !>(^-((clog) [%drain dif]))] 
    ..sync(stat stat)
  ++  paths  pats
  ++  flush
    ^-  card:agent:gall
    [%give %fact pats %noun !>(^-((clog) [%flush stat]))]
  --
--