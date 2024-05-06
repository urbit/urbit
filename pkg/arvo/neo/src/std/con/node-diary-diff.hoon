/@  node
/@  diary-diff
/-  _/manx-utils
:-  [%node %diary-diff]
|=  nod=node
^-  diary-diff
=/  head  (@tas (crip (need (~(get-attribute manx-utils nod) %head))))
=/  id
  %+  slav  %da
  (crip (need (~(value manx-utils nod) "id")))
=/  text
  (crip (need (~(value manx-utils nod) "text")))
[%put-entry id text]
