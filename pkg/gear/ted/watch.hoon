/-  spider
/+  strandio
=,  strand=strand:spider
=,  clay
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =ship dap=term =path] arg)
;<  ~      bind:m  (watch:strandio /watch [ship dap] path)
|-  ^-  form:m
=*  loop  $
;<  =cage  bind:m  (take-fact:strandio /watch)
%-  %^  slog  leaf+"On subscription {<[ship dap path]>} received"
      (sell q.cage)
    ~
loop
