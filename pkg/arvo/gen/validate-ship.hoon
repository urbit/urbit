::  Validate userspace state
::
::  /hoon/validate-ship/gen
::
/+  resource
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [~ ~]
    ==
:-  %noun
=|  issues=(list tape)
|^
=.  issues
  check-group-syncs
issues
::
++  scry-synced
  |=  app=term
  ^-  (set path)
  %~  key  by
  .^  (map path ship)
    %gx
    (scot %p p.beak)
    app
    (scot %da now)
    /synced/noun
  ==
::
++  scry-grp-push-syncs
  ^-  (set resource)
  .^  (set resource) 
    %gx
    (scot %p p.beak)
    %group-push-hook
    (scot %da now)
    /synced/noun
  ==
::
++  scry-grp-pull-syncs
  ^-  (set resource)
  %~  key  by
  .^  (map resource ship)
    %gx
    (scot %p p.beak)
    %group-pull-hook
    (scot %da now)
    /synced/noun
  ==
::
++  report-missing-syncs
  |*  [app=term syncs=(set)]
  ^+  issues
  %+  weld
    issues
  %+  turn
    ~(tap in syncs)
  |=(thing=* "{<app>}-hook missing: {<thing>}")
::  +check-group-syncs: validate sync for group
::
::    ensure that syncs are correctly set up for each
::    hook that has group associated resources
++  check-group-syncs
  ^+  issues
  =/  groups=(set resource)
    %-  sy
    %+  turn
      %~  tap  in
      %~  key  by
      =<  dir
      .^  arch
        %gy
        (scot %p p.beak)
        %group-store
        (scot %da now)
        /groups
      ==
    (corl de-path:resource stab)
  =/  [ours=(list resource) theirs=(list resource)]
     (skid ~(tap by groups) |=(rid=resource =(p.beak entity.rid)))
  =/  pull-syncs=(set resource)
    scry-grp-pull-syncs
  =/  push-syncs=(set resource)
    scry-grp-push-syncs
  =/  group-paths=(set path)
    (sy (turn ~(tap in groups) en-path:resource))
  =/  md-syncs=(set path)
    (scry-synced %metadata-hook)
  =/  contact-syncs=(set path)
    (scry-synced %contact-hook)
  =.  issues
    %+  report-missing-syncs
      %metadata
    (~(dif in md-syncs) group-paths)
  =.  issues
    %+  report-missing-syncs
      %contact
    (~(dif in contact-syncs) group-paths)
  =.  issues
    %+  report-missing-syncs
      %group-push
    (~(dif in push-syncs) (silt ours))
  =.  issues
    %+  report-missing-syncs
      %group-pull
    (~(dif in pull-syncs) (silt theirs))
  issues
--

