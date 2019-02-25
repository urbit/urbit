::  watcher: ethereum event log collector
::
|%
++  name  @tas
::
++  config
  $:  node=purl:eyre
      from-block=@ud
      to-block=(unit @ud)
      contracts=(list address:ethereum)
      topics=(list $@(@ux (list @ux)))
  ==
::
++  action
  $%  [%watch =name =config]
      ::TODO  support modifying existing config for future polling
      [%clear =name]
  ==
::
++  update
  $%  ::  %snap: all known-good logs, sent on-subscribe and on-reorg
      ::TODO  there's probably a way to be more nuanced about what we forgot
      ::      to cope with a reorg
      ::
      [%snap =snapshot]
      ::  %vent: newly added logs
      ::
      [%logs =loglist]
  ==
::
++  snapshot
  $:  last-heard-block=@ud
      heard=(set event-id:ethereum)
      logs=loglist
  ==
::
++  loglist
  (list event-log:rpc:ethereum)  ::  newest first
--
