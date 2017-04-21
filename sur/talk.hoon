::
::::  /hoon/talk/sur
  !:
|%
::
::TODO  station -> circle
::
::>  ||
::>  ||  %reader-communication
::>  ||
::>    broker interfaces for readers.
::+|
::
++  action                                              ::>  user action
  $%  ::  station configuration                         ::
      {$create (trel knot cord posture)}                ::<  create station
      {$source (trel knot ? (set partner))}             ::<  un/sub p to/from r
      {$depict (pair knot cord)}                        ::<  change description
      {$permit (trel knot ? (set ship))}                ::<  invite/banish
      {$delete (pair knot (unit cord))}                 ::<  delete + announce
      ::  messaging                                     ::
      {$convey (list thought)}                          ::<  post exact
      {$phrase (pair (set partner) (list speech))}      ::<  post easy
      ::  personal metadata                             ::
      ::TODO  change to target partners, not only our stations.
      {$status (pair (set knot) status)}                ::<  our status update
      ::  changing shared ui                            ::
      {$human (pair ship human)}                        ::<  new identity
      {$glyph (trel char (set partner) ?)}              ::<  un/bind a glyph
  ==                                                    ::
++  reaction                                            ::>  user information
  $:  kind/?($info $fail)                               ::<  result
      what/@t                                           ::<  explain
      why/(unit action)                                 ::<  cause
  ==                                                    ::
++  lowdown                                             ::>  new/changed state
  $%  ::  story state                                   ::
      {$confs (unit config) (map station (unit config))}::<  configs
      {$precs register}                                 ::<  presences
      {$grams (pair @ud (list telegram))}               ::<  messages
      ::  ui state                                      ::
      {$glyph (jug char (set partner))}                 ::<  glyph bindings
      {$names (map ship (unit human))}                  ::<  nicknames
  ==                                                    ::
::
::>  ||
::>  ||  %broker-communication
::>  ||
::>    structures for communicating between brokers.
::+|
::
++  command                                             ::>  effect on story
  $%  {$review (list thought)}                          ::<  deliver
  ==                                                    ::
++  report                                              ::>  update
  $%  {$cabal cabal}                                    ::<  config neighborhood
      {$group register}                                 ::<  presence
      {$grams (pair @ud (list telegram))}               ::<  thoughts
  ==                                                    ::
::
::>  ||
::>  ||  %circles
::>  ||
::>    messaging targets and their metadata.
::+|
::
++  partner    (each station passport)                  ::<  message target
++  station    (pair ship knot)                         ::<  native target
++  passport                                            ::>  foreign target
  $%  {$twitter p/@t}                                   ::<  twitter handle
  ==                                                    ::
::>  circle configurations.
++  cabal                                               ::>  metaconfiguration
  $:  loc/config                                        ::<  local config
      ham/(map station config)                          ::<  neighborhood configs
  ==                                                    ::
++  config                                              ::>  station config
  $:  sources/(set partner)                             ::<  pulls from
      caption/cord                                      ::<  description
      cordon/control                                    ::<  restrictions
  ==                                                    ::
++  control    (pair posture (set ship))                ::<  access control
++  posture                                             ::>  security kind
  $?  $black                                            ::<  channel, blacklist
      $white                                            ::<  village, whitelist
      $green                                            ::<  journal, author list
      $brown                                            ::<  mailbox, our r, bl w
  ==                                                    ::
::>  participant metadata.
++  register   (pair atlas (map partner atlas))         ::<  our & srcs presences
++  atlas      (map ship status)                        ::<  presence map
++  status     (pair presence human)                    ::<  participant
++  presence   ?($gone $hear $talk)                     ::<  status type
++  human                                               ::>  human identifier
  $:  true/(unit (trel @t (unit @t) @t))                ::<TODO  unused true name
      hand/(unit @t)                                    ::<  handle
  ==                                                    ::
::
::>  ||
::>  ||  %message-data
::>  ||
::>    structures for containing main message data.
::+|
::
++  telegram   (pair ship thought)                      ::<  who thought
++  thought    (trel serial audience statement)         ::<  which whom what
++  statement  (trel @da bouquet speech)                ::<  when this
++  speech                                              ::>  narrative action
  $%  {$non $~}                                         ::<  no content (yo)
      {$lin p/? q/@t}                                   ::<  no/@ text line
      {$ire p/serial q/speech}                          ::<  in-reply-to
      {$url p/purf}                                     ::<  parsed url
      {$exp p/@t}                                       ::<  hoon line
      {$fat p/torso q/speech}                           ::<  attachment
      {$lan p/knot q/@t}                                ::<  local announce
      {$inv p/? q/station}                              ::<  inv/ban for station
      {$mor p/(list speech)}                            ::<  multiplex
      {$ext p/@tas q/*}                                 ::<  extended action
      {$app p/@tas q/@t}                                ::<  app message
      $:  $api                                          ::<  api message
          service/@tas                                  ::<  service name
          id/@t                                         ::<  id on the service
          id-url/purf                                   ::<  link to id
          summary/@t                                    ::<  summary of event
          body/@t                                       ::<  body of event
          url/purf                                      ::<  link to event
          meta/json                                     ::<  other data for web
      ==                                                ::
  ==                                                    ::
++  torso                                               ::>  attachment
  $%  {$name (pair @t torso)}                           ::<  named attachment
      {$text (list @t)}                                 ::<  text lines
      {$tank (list tank)}                               ::<  tank list
  ==                                                    ::
::
::>  ||
::>  ||  %message-metadata
::>  ||
::     structures for containing message metadata.
::+|
::
++  serial     @uvH                                     ::<  unique identifier
++  audience   (map partner (pair envelope delivery))   ::<  destination + state
++  envelope   (pair ? (unit partner))                  ::<  visible sender
++  delivery                                            ::>  delivery state
  $?  $pending                                          ::<  undelivered
      $received                                         ::<  delivered
      $rejected                                         ::<  undeliverable
      $released                                         ::<  sent one-way
      $accepted                                         ::<  fully processed
  ==                                                    ::
::TODO  what is ++bouquet even for? not yet used...
++  bouquet    (set flavor)                             ::<  complete aroma
++  flavor     path                                     ::<  content flavor
--
