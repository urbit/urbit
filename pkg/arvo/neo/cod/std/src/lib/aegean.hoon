|%
+$  signature   @uvH
+$  signal                   ::  what gets broadcast
  $:
    =hops
    =post
  ==
+$  hops  ?(%0 %1 %2)        ::  do not allow >2 hops
+$  post  path               ::  FQSP such that mark is %entry
::  author in the head
::  require timestamp, other cases rejected
::  originating desk included
+$  entry                    ::  /mar/entry/hoon
  $:
    text=(unit @t)           ::  max length 256 bytes
    media=(unit preview)
    quote=(unit post)
  ==
+$  preview  (map path coin) ::  OGP data
+$  saved  (set post)
+$  hidden  (set post)
+$  boosts  (map post [ship @da])  ::  ships who have boosted and when
+$  store  (map post entry)  ::  the content that gets sorted
+$  sort  (map post weight=@sd)
+$  weight  @sd
+$  fresh  (list [post weight])  :: populated upon hearing new post
::
+$  locker  (set evidence)   ::  reported posts from friends
+$  evidence
  $:
    mine=signature
    %disavow
    =post
    theirs=signature
    =entry
  ==
::
+$  action
  $%
    [%broadcast =signal]
    [%save =post]
    [%hide =post]
    [%boost =post]
    [%report =post]
  ==
+$  message
  $%
    [%gossip =signal]
    [%praise =entry]
    [%tattle =evidence]
  ==
--
