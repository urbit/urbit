!:
%-  epic
|=  [quy=quay own=@p ced=cred pos=pred suf=path but=path for=@tas]
^-  manx
:~  %html
  :~  %body
    ~[%h3 +"{(scow %p own)} hails you from afar"]
    +"Hi, neighbor!{?:(=(~ quy) "" " You ask: {<quy>}")}?"
    %br
    +"You addressed the {?:(p.hut.ced "" "in")}secure, "
    +"{?:(q.hut.ced "" "un")}bound "
    +"{?:(-.r.hut.ced "DNS host {<p.r.hut.ced>}" "IPv4 host {<p.r.hut.ced>}")}."
    %br
    +"Your CSRF token is '{<orx.ced>}'."
    %br
    ?:  ?=(~ acc.ced)
      +"You sent no accept headers."
    +"You accept {<p.u.acc.ced>}; language {<q.u.acc.ced>}."
    %br
    ?-  -.cip.ced
      &  +"You came from the IPv4 address {<p.cip.ced>}."
      |  +"You came from the IPv6 address {<p.cip.ced>}."
    ==
    %br
    ?:  =(~ aut.ced)
      +"You have no authenticated identities."
    +"Authenticated identities: {<aut.ced>}."
    %br
    +"This synthesis was computed within {<`path`pos>}.  "
    +"It was found as {<for>}, under {<suf>}"
    +"{?:(=(~ but) "." ", in {<but>}.")}"
  ==
==
