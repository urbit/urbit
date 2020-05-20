::  link-listen-hook: actions for getting your friends' bookmarks
::
::    Note that /app/link-listen-hook auto-joins any new collections in groups
::    you're a part of. You only need the watch action here for leaving and
::    re-joining.
::
|%
+$  action
  $%  [%watch =path]
      [%leave =path]
  ==
::
+$  update
  $%  [%listening paths=(set path)]
      action
  ==
--
