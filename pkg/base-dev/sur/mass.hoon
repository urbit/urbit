::  /hoon/mass/sur: requests and responses to/from %mass agent
|%
::  action: poke request to %mass agent
::
+$  action
      ::  %mass: run memory report NOW
      ::
  $%  [%mass ~]
      ::  %poll: set memory report frequency.
      ::    Null unit disables polling, @dr runs every
      ::    period specified (e.g ~h1 for every hour),
      ::    d/h/m runs every d days at h:m time. Note h
      ::    must be <12 and m must be <60. If d=0 it'll
      ::    run daily & maybe today if it's currently
      ::    earlier than h:m, as opposed to d=1 where it
      ::    runs daily starting tomorrow.
      [%poll every=(unit $@(@dr [d=@ud h=@ud m=@ud]))]
      ::  %free: delete old entries
      ::    If before is null, it wipes all entries for path.
      ::    Otherwise, it wipes entries older than @da for path.
      ::    If path is empty, it's applied to all records,
      ::    otherwise it's only for the specified one e.g.
      ::    /arvo/hoon.
      [%free path=(list @t) before=(unit @da)]
      ::  %obey: add/remove remote ships to whitelist.
      ::    If p is null, all remote ships are disallowed. Otherwise,
      ::    the ships in add are added to the whitelist, and the ships
      ::    in rm are removed from the whitelist. Whitelisted ships
      ::    can run memory reports, change polling frequency and
      ::    subscribe for results. Whitelisted ships cannot change
      ::    the whitelist, read the whitelist or delete records.
      ::
      [%obey p=(unit [add=(set @p) rm=(set @p)])]
  ==
::  update: %fact update/scry result from %mass agent
::
+$  update
  ::  %new: memory report for particular path (e.g. /arvo/hoon)
  ::    time is the time of the report, path is the particular record,
  ::    size is the memory size in bytes, base is base hash at the time
  ::    of report.
  $%  [%new =time path=(list @t) size=@ud base=@uvI]
  ::  %new-all: memory report for all paths
  ::    time is the time of the report, data is a map from the path
  ::    (e.g. /arvo/hoon) to a pair of the size in bytes and the base
  ::    hash at the time of report.
      [%new-all =time data=(map (list @t) [size=@ud base=@uvI])]
  ::  %old: all memory report records for a particular path
  ::    path is the particular record path, list is a list of the time
  ::    of each report, the size in bytes and the base hash at that
  ::    time. List starts from the oldest record
      [%old path=(list @t) list=(list [=time size=@ud base=@uvI])]
  ::  %old-all: all memory report records for all paths
  ::    data is a map from paths (e.g. /arvo/hoon) to lists of
  ::    report time, size in bytes and base hash at report time.
  ::    Lists are oldest entry first.
      [%old-all data=(jar (list @t) [=time size=@ud base=@uvI])]
  ::  %newest: latest memory report for a particular path
  ::    May be null if no records. Otherwise time is report time,
  ::    path is record path (e.g. /arvo/hoon), size is in bytes and
  ::    base is base hash and report time.
      [%newest p=(unit [=time path=(list @t) size=@ud base=@uvI])]
  ::  %raw: raw memory report from dill
  ::    p is null if it's never been run before. Otherwise, time
  ::    is the report time, base is the base hash at that time, and
  ::    quacs is the memory report structure itself.
      [%raw p=(unit [=time base=@uvI quacs=(list quac:dill)])]
  ::  %poll: an update to memory report frequency configuration
  ::    if every is null, automatic reports have been disabled.
  ::    Otherwise, if it's a @dr it runs every @dr, and if it's
  ::    d/h/m it runs every d days at h:m
      [%poll every=(unit $@(@dr [d=@ud h=@ud m=@ud]))]
  ::  %acl: remote ship whitelist change
  ::    if null, all remote ships have been de-whitelisted. Otherwise
  ::    add contains ships added to whitelist and rm contains ships
  ::    removed from whitelist.
      [%acl p=(unit [add=(set @p) rm=(set @p)])]
  ==
--
