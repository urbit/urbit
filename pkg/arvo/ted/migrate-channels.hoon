/-  spider, *metadata-store, *publish
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  [og-path=path ng-path=path ~]  !<([path path ~] arg)
;<  bol=bowl:spider  bind:m  get-bowl:strandio
|^
::
=/  og=(unit (set ship))  (scry-for (unit (set ship)) %group-store og-path)
?~  og
  (pure:m !>("no such group: {<og-path>}"))
=/  ng=(unit (set ship))  (scry-for (unit (set ship)) %group-store ng-path)
?~  ng
  (pure:m !>("no such group: {<ng-path>}"))
::
=/  assoc=associations  (scry-for associations %metadata-store [%group og-path])
=/  assoc-list=(list [[group-path resource] metadata])  ~(tap by assoc)
::
|-
=*  loop  $
?~  assoc-list
  ;<  ~  bind:m
    (poke-our:strandio %group-store %group-action !>([%unbundle og-path]))
  (pure:m !>("done"))
=/  [[g-path=group-path res=resource] meta=metadata]  i.assoc-list
?.  =(our.bol creator.meta)
  loop(assoc-list t.assoc-list)
?>  =(g-path og-path)
=/  output=(list card:agent:gall)
  ?+  app-name.res  ~
  ::
      ?(%chat %link)
    %-  (slog leaf+"migrating {<app-name.res>} : {<app-path.res>}" ~)
    :~  :*  %pass  /poke  %agent
            [our.bol %metadata-hook]
            %poke  %metadata-action
            !>([%add ng-path res meta])
        ==
        :*  %pass  /poke  %agent
            [our.bol %metadata-hook]
            %poke  %metadata-action
            !>([%remove g-path res])
        ==
    ==
      %publish
    %-  (slog leaf+"migrating {<app-name.res>} : {<app-path.res>}" ~)
    =/  book  (scry-for notebook %publish [%book app-path.res])
    ?>  ?=([@tas @tas ~] app-path.res)
    :~  :*  %pass  /poke  %agent
            [our.bol %publish]
            %poke  %publish-action
            !>
            :*  %edit-book
                i.t.app-path.res
                title.book
                description.book
                comments.book
                `[ng-path ~ %.y %.n]
            ==
        ==
        :*  %pass  /poke  %agent
            [our.bol %metadata-hook]
            %poke  %metadata-action
            !>([%remove g-path res])
        ==
    ==
  ==
::
;<  ~  bind:m  (send-raw-cards:strandio output)
loop(assoc-list t.assoc-list)
::
++  scry-for
  |*  [mol=mold app=term pax=path]
  .^  mol
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `path`pax %noun)
  ==
--
