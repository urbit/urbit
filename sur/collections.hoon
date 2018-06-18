=<  |=(a=* %.(a (map time collection)))             ::  collections by name
|%                                                  ::
++  collection                                      ::
  $:  conf/{mod/@da config}                         ::  configuration
      tops/(map @da topicful)                       ::
  ==                                                ::
::++  coll-full
::  {host/(unit @p) col/time}
++  topicful                                        ::
  $:  info/{mod/@da topic}                          ::
      coms/(map @da {mod/@da comment})              ::
  ==                                                ::
::                                                  ::
++  topicshow
  $:  top/topicful
      snip/{hed/marl tal/marl}
      $~
  ==
++  config                                          ::
  $:  desc/cord                                     ::  description
      visi/?                                        ::  visible or hidden
      read/rule:clay                                ::  read permissions
      write-post/rule:clay                          ::  top-level posting permissions
      write-reply/rule:clay                         ::  comment/reply posting permissions
  ==                                                ::
++  topic                                           ::
  $:  tit/cord                                      ::  title
      who/ship                                      ::  author
      wat/wain                                      ::  content
  ==                                                ::
++  comment                                         ::
  $:  who/ship                                      ::  author
      wat/wain                                      ::  content
  ==                                                ::
::
++  api
  |%
  ++  action
    $%  {$create cof/config}                                     ::  create a collection
        {$change-config col/@da cof/config}                      ::  edit config
        {$submit host/@p col/@da tit/cord wat/wain}              ::  submit a post/note
        {$resubmit host/@p col/@da top/@da tit/cord wat/wain}    ::  edit a post/note
        {$comment host/@p col/@da top/@da com/?(~ @da) wat/wain} ::  submit a comment
        {$delete host/@p col/@da}                                ::  delete a collection
        {$delete-topic host/@p col/@da top/@da}                  ::  delete a topic
        {$delete-comment host/@p col/@da top/@da com/@da}        ::  delete a comment
    ==
  --
--
