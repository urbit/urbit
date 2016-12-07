::  
::::  /hoon/talk/sur
  ::
|%
++  audience  (map partner (pair envelope delivery))    ::  destination+state
++  atlas  (map ship status)                            ::  presence map
++  bouquet  (set flavor)                               ::  complete aroma
++  command                                             ::  effect on party
  $%  {$design (pair knot (unit config))}               ::  configure+destroy
      {$publish (list thought)}                         ::  originate
      {$review (list thought)}                          ::  deliver
  ==                                                    ::
++  cabal                                               ::  metaconfiguration
  $:  loc/config                                        ::  local config
      ham/(map station config)                          ::  neighborhood configs
  ==                                                    ::
++  config                                              ::  party configuration
  $:  sources/(set partner)                             ::  pulls from
      caption/cord                                      ::  about
      cordon/control                                    ::  restricted to
  ==                                                    ::
++  control  (pair posture (set ship))                  ::  access control
++  delivery                                            ::  delivery state
  $?  $pending                                          ::  undelivered
      $received                                         ::  delivered
      $rejected                                         ::  undeliverable
      $released                                         ::  sent one-way
      $accepted                                         ::  fully processed
  ==                                                    ::
++  envelope  (pair ? (unit partner))                   ::  visible sender
++  flavor  path                                        ::  content flavor
++  human                                               ::  human identifier
  $:  true/(unit (trel @t (unit @t) @t))                ::  true name
      hand/(unit @t)                                    ::  handle
  ==                                                    ::
++  passport                                            ::  foreign flow
  $%  {$twitter p/@t}                                   ::  twitter
  ==                                                    ::
++  posture                                             ::  security posture
  $?  $black                                            ::  channel
      $white                                            ::  chamber
      $green                                            ::  journal
      $brown                                            ::  mailbox
  ==                                                    ::
++  presence   ?($gone $hear $talk)                     ::  status type
++  register  (pair atlas (map partner atlas))          ::  ping me, ping srcs
++  shelf  (map knot (pair posture cord))               ::  ship shape
++  report                                              ::  talk update
  $%  {$cabal cabal}                                    ::  config neighborhood
  ::  {$folder (list report)}                           ::  multiple
      {$grams (pair @ud (list telegram))}               ::  beginning thoughts
      {$group register}                                 ::  presence
      {$house shelf}                                    ::  station set
      {$glyph (jug char (set partner))}                 ::  relevant binding
  ==                                                    ::
++  speech                                              ::  narrative action
  $%  {$lan p/knot q/@t}                                ::  local announce
      {$exp p/@t}                                       ::  hoon line
      {$non $~}                                         ::  no content (yo)
      {$ext p/@tas q/*}                                 ::  extended action
      {$fat p/torso q/speech}                           ::  attachment
      ::  {$inv p/station}                              ::  invite to station
      {$url p/purf:eyre}                               ::  parsed url
      {$ire p/serial q/speech}                          ::  in-reply-to
      {$lin p/? q/@t}                                   ::  no/@ text line
      {$mor p/(list speech)}                            ::  multiplex
      {$app p/@tas q/@t}                                ::  app message
      $:  $api                                          ::  api message
          service/@tas                                  ::  service name
          id/@t                                         ::  id on the service
          id-url/purf:eyre                             ::  link to id
          summary/@t                                    ::  summary of event
          body/@t                                       ::  body of event
          url/purf:eyre                                ::  link to event
          meta/json                                     ::  other data for web
      ==                                                ::
  ==                                                    ::
++  serial     @uvH                                     ::  unique identity
++  partner    (each station passport)                  ::  interlocutor
++  status     (pair presence human)                    ::  participant
++  statement  (trel @da bouquet speech)                ::  when this
++  station    (pair ship knot)                         ::  domestic flow
++  telegram   (pair ship thought)                      ::  who which whom what
++  thought    (trel serial audience statement)         ::  which whom what
++  torso                                               ::  attachment
  $%  {$name (pair @t torso)}                           ::  named attachment
      {$text (list @t)}                                 ::  text lines
      {$tank (list tank)}                               ::  tank list
  ==                                                    ::
--
