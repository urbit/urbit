/-  *hood
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        $@(~ [?(%suspended %running %blocking %nonexistent) ~])
        $:  verb=?
            show-suspended=?
            show-running=?
            show-blocking=?
            show-nonexistent=?
        ==
    ==
=+  :-  verb
    ?~  +<+<        +<+>+
    ?-  -.+<+<
      %suspended    [& | | |]
      %running      [| & | |]
      %blocking     [| | & |]
      %nonexistent  [| | | &]
    ==
tang+((report-vats p.bec now) -)
