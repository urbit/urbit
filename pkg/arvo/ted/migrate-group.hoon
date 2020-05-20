/-  spider, *metadata-store, *publish
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  [og-path=path ng-path=path ~]  !<([path path ~] arg)
;<  bol=bowl:spider  bind:m  get-bowl:strandio
::
=/  gs-path  /(scot %p our.bol)/group-store/(scot %da now.bol)
=/  ms-path  /(scot %p our.bol)/metadata-store/(scot %da now.bol)
=/  p-path   /(scot %p our.bol)/publish/(scot %da now.bol)
::
=/  og=(unit (set ship))
  .^((unit (set ship)) %gx :(weld gs-path og-path /noun))
?~  og
  (pure:m !>("no such group: {<og-path>}"))
=/  ng=(unit (set ship))
  .^((unit (set ship)) %gx :(weld gs-path ng-path /noun))
?~  ng
  (pure:m !>("no such group: {<ng-path>}"))
::
=/  ass=associations  .^(associations %gx :(weld ms-path /group og-path /noun))
=/  ass-list=(list [[group-path resource] metadata])  ~(tap by ass)
::
|-
=*  loop  $
?~  ass-list
  (pure:m !>("done"))
=/  [[g-path=group-path res=resource] meta=metadata]  i.ass-list
?.  =(our.bol creator.meta)
  loop(ass-list t.ass-list)
?.  =(g-path og-path)
  loop(ass-list t.ass-list)
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
    =/  book  .^(notebook %gx :(weld p-path /book app-path.res /noun))
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
;<  ~  bind:m
  (poke-our:strandio %group-store %group-action !>([%unbundle og-path]))
loop(ass-list t.ass-list)
