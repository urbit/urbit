!:
%-  give
|=  [quy=quay own=@p ced=cred pos=pred suf=path but=path for=@tas nep=@tas]
^-  manx
:~  %html
  :~  %body
    [%h3 -"{:/(scow %p own)} is generally a happy camper."]
    [%p -"a paragraph with {i/-"italics"} is nice."]
    [%p -"we can go crazy by embedding {:/(scow %p own)}."]
    +"Hi, neighbor!{?:(=(~ quy) "" " You ask: {<quy>}?")}"
    %br
    +"You addressed the {?:(p.hut.ced "" "in")}secure,"
    +"{?:(q.hut.ced "" "un")}bound "
    +"{?:(-.s.hut.ced "DNS host {<p.s.hut.ced>}" "IPv4 host {<p.s.hut.ced>}")}"
    ?~(r.hut.ced +"" +" at port {<u.r.hut.ced>}.")
    %br
    +"Your CSRF token is '{<orx.ced>}'."
    %br
    ?:  ?=(~ acl.ced)
      +"You sent no accept-language."
    +"Your language is {<u.acl.ced>}."
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
    +"This {<for>} page was produced from {<`path`pos>}, under {<suf>}"
    +", as {<nep>}"
    +"{?:(=(~ but) "." ", in {<but>}.")}"
  ==
==
