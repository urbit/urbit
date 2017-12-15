=<  |=(a=* %.(a (map time collection)))             ::  collections by name
|%                                                  ::
++  collection                                      ::
  $:  conf/config                                   ::  configuration
      tops/(map @da topicful)                       ::
      $~                                            ::  /.  compatibility
  ==                                                ::
:: ++  topicful  {topic comt/(map @da comment)}     ::
++  topicful  {topic comt/(map @da comment) $~}     ::
::                                                  ::
++  config                                          ::
  $:  desc/cord                                     ::  description
      publ/?                                        ::  public or private
      visi/?                                        ::  visible or hidden
      mems/(set ship)                               ::  ships on list
  ==                                                ::
++  topic                                           ::
  $:  tit/cord                                      ::  title
      comment                                       ::
  ==                                                ::
++  comment                                         ::
  $:  who/ship                                      ::  author
      wed/@da                                       ::  editted
      wat/wain                                      ::  content
  ==                                                ::
::
++  api
  |%
  ++  kind  ?($blog $fora $note)                      ::
  ++  action                                          ::
    $%  $:  $create                                   ::  create a collection
            wat/kind                                  ::  collection kind
            des/cord                                  ::  name
            pub/?                                     ::  public or private
            vis/?                                     ::  visible or hidden
            ses/(set ship)                            ::  black/whitelist
        ==                                            ::
        {$submit col/time tit/cord wat/wain}          ::  submit a post/note
        {$comment col/time top/@da com/@da wat/wain}  ::  submit a comment
        {$delete col/time}                            ::  delete a collection
    ==                                                ::
  --
--
