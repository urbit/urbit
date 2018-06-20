=<  |=(a=* %.(a (map time collection)))             ::  collections by name
|%                                                  ::
++  collection                                      ::
  $:  conf/{mod/@da config}                         ::  configuration
      tops/(map @da topicful)                       ::
      $~                                            ::  /.  compatibility
  ==                                                ::
++  topicful                                        ::
  $:  info/{mod/@da topic}                          ::
      coms/(map @da {mod/@da comment})              ::
      $~                                            ::  /.  compatibility
  ==                                                ::
::                                                  ::
++  topicshow
  $:  top/topicful
      snip/{hed/marl tal/marl}
      $~
  ==
++  config                                          ::
  $:  desc/cord                                     ::  description
      publ/?                                        ::  public or private
      visi/?                                        ::  visible or hidden
      comm/?                                       ::  comments
      xeno/?                                        ::  foreign posters?
      mems/(set ship)                               ::  ships on list
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
  ++  kind  ?($blog $fora $note)                      ::
  ++  action                                          ::
    $%  $:  $create                                   ::  create a collection
            ::wat/kind                                  ::  collection kind
            desc/cord                                 ::  name
            publ/?                                    ::  public or private
            visi/?                                    ::  visible or hidden
            comm/?                                    ::  others can comment
            xeno/?                                    ::  others can post
            ses/(set ship)                            ::  black/whitelist
        ==                                            ::
        {$submit col/time tit/cord wat/wain}          ::  submit a post/note
        {$resubmit col/time top/@da tit/cord wat/wain} ::  edit a post/note
        {$comment col/time top/@da com/?(~ @da) wat/wain} ::  submit a comment
        {$delete col/time}                            ::  delete a collection
        ::
        ::REVIEW names? nest collection/topic/comment actions?
        {$delete-topic col/time top/@da}              ::  delete a collection
        {$delete-comment col/time top/@da com/@da}    ::  delete a collection
    ==                                                ::
  --
--
