::  eth-watcher: ethereum event log collector
::
=,  able:jael
|%
+$  config
  $:  url=@ta
      eager=?
      refresh-rate=@dr
      from=number:block
      contracts=(list address:ethereum)
      =topics
  ==
::
+$  loglist  (list event-log:rpc:ethereum)
+$  topics   (list ?(@ux (list @ux)))
+$  watchpup
  $:  config
      =number:block
      =pending-logs
      blocks=(list block)
  ==
::
::  disavows: newest block first
+$  disavows      (list id:block)
+$  pending-logs  (map number:block loglist)
::
+$  poke
  $%  ::  %watch: configure a watchdog and fetch initial logs
      ::
      [%watch =path =config]
      ::  %clear: remove a watchdog
      ::
      [%clear =path]
  ==
::
+$  diff
  $%  ::  %history: full event log history, oldest first
      ::
      [%history =loglist]
      ::  %log: newly added log
      ::
      [%log =event-log:rpc:ethereum]
      ::  %disavow: forget logs
      ::
      ::    this is sent when a reorg happens that invalidates
      ::    previously-sent logs
      ::
      [%disavow =id:block]
  ==
--
