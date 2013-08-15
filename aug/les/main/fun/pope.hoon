|=  *
|=  *
|=  ~
^-  bowl
:-  ~
:-  ~
:-  :~  [~ [%up %pass "kernel passphrase: "]]
    ==
|=  [now=@da pax=path nut=note]
^-  bowl
?>  ?=(%up -.nut)
=+  fra=p.nut
:-  :~  [%la %leaf "passphrase check: {<`@p`(end 5 1 (shaf %pass fra))>}"]
    ==
:-  ~
:-  :~  [~ [%up %text "generate keys (y/n)? "]]
    ==
|=  [now=@da pax=path nut=note]
^-  bowl
?>  ?=(%up -.nut)
?:  !=(%y p.nut)
  :-  :~  [%la %leaf "okay, then!"]
      ==
  ~
=+  ^=  guy
    =+  inx=0
    =|  goy=(list ,[p=@ud q=@uv r=@uv s=@ud])
    |-  ^+  goy
    ?:  =(256 inx)  goy
    ~&  [%minting inx]
    =+  bur=(shax (add inx (shax fra)))
    =+  arc=(brew 2.048 bur)
    %=  $
      inx  +(inx)
      goy  :_  goy
           :^    inx
               bur
             fig:ex:arc
           sec:ex:arc
    ==
=.  guy  (flop guy)
=+  ^=  fiz
    %+  rap  3
    %+  turn  guy
    |=  [p=@ud q=@uv r=@uv s=@ud]
    (rap 3 (weld `tape`~(rend co %% %uw q) `tape`[`@`10 ~]))
=+  ^=  guz
    %-  role
    |-  ^-  (list ,@t)
    ?~  guy  ~
    ?>  ?=(^ t.guy)
    :_  $(guy t.t.guy)
    %+  rap  3
    "{(scow %uw r.i.guy)}  {(scow %uw r.i.t.guy)}  ::   {(scow %ud p.i.guy)}"
=+  ^=  hoz
    %+  rap  3
    %+  turn  guy
    |=  [p=@ud q=@uv r=@uv s=@ud]
    %+  rap  3
    ;:  weld 
      ~(rend co %% %ud p) 
      " "
      ~(rend co %% %ud s) 
      `tape`[`@`10 ~]
    ==
:-  :~  [%la %leaf "done!"]
        [%xx %save /generators/txt fiz]
        [%xx %save /fingerprints/txt guz]
        [%xx %save /rings/txt hoz]
    ==
~
