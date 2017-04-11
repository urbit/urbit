::
::::  /hoon/talk/sur
  !:
|%
++  audience  (map partner (pair envelope delivery))    ::  destination+state
++  atlas  (map ship status)                            ::  presence map
++  bouquet  (set flavor)                               ::  complete aroma
++  command                                             ::  effect on party
  $%  {$design (pair knot (unit config))}               ::  configure+destroy
      {$publish (list thought)}                         ::  originate
      {$review (list thought)}                          ::  deliver
  ==                                                    ::
++  action                                              ::  user action
  $%  ::  station configuration                         ::
      {$create (pair knot (pair cord posture))}         ::  create station
      {$permit (trel knot ? (set ship))}                ::  invite/banish
      ::  personal metadata                             ::
      ::TODO  change to target partners, not only our stations.
      {$status (pair (set knot) status)}                ::  our status update
      ::  changing shared ui                            ::
      {$human (pair ship human)}                        ::  new identity
      {$glyph (pair char (set partner))}                ::  bind a glyph
  ==                                                    ::
++  reaction                                            ::  user information
  $:  kind/?($info $fail)                               ::  result
      what/@t                                           ::  explain
      why/(unit action)                                 ::  cause
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
  $:  true/(unit (trel @t (unit @t) @t))                ::TODO  used?  true name
      hand/(unit @t)                                    ::  handle
  ==                                                    ::
++  passport                                            ::  foreign flow
  $%  {$twitter p/@t}                                   ::  twitter
  ==                                                    ::
++  posture                                             ::  security posture
  $?  $black                                            ::  channel, blacklist
      $white                                            ::  village, whitelist
      $green                                            ::  journal, author list
      $brown                                            ::  mailbox, our + black
  ==                                                    ::
++  presence   ?($gone $hear $talk)                     ::  status type
++  register  (pair atlas (map partner atlas))          ::  ping me, ping srcs
++  shelf  (map knot (pair posture cord))               ::  ship shape
++  report                                              ::  talk update
  $%  {$cabal cabal}                                    ::  config neighborhood
  ::  {$folder (list report)}                           ::  multiple
      {$grams (pair @ud (list telegram))}               ::  beginning thoughts
      {$group register}                                 ::  presence
  ==                                                    ::
++  lowdown                                             ::  changed shared state
  ::TODO  change these so that they're always just the diff?
  ::      re-check existing implementations too!
  ::      this will aid with ++sh's printing.
  $%  {$glyph (jug char (set partner))}                 ::  new bindings
      {$names (map ship (unit human))}                  ::  new identities
      {$tales (pair knot (unit config))}                ::  changed config
      {$remco (map station config)}                     ::  remote configs
      {$precs (pair knot atlas)}                        ::  changed presence
      {$rempe (map partner atlas)}                      ::  remote presences
      {$grams (pair knot (pair @ud (list telegram)))}   ::  new grams
  ==                                                    ::
++  speech                                              ::  narrative action
  $%  {$lan p/knot q/@t}                                ::  local announce
      {$exp p/@t}                                       ::  hoon line
      {$non $~}                                         ::  no content (yo)
      ::TODO  so, this is extensibility tacked on, rather than built-in?
      ::      really, talk has been two-split but the guardian is still two
      ::      things at once.  you want base-guardian and talk-guardian.
      ::      ...that's probably what %gall is, but then why do we have $ext?
      {$ext p/@tas q/*}                                 ::  extended action
      {$fat p/torso q/speech}                           ::  attachment
      {$inv p/? q/station}                              ::  inv/ban for station
      {$url p/purf}                                     ::  parsed url
      {$ire p/serial q/speech}                          ::  in-reply-to
      {$lin p/? q/@t}                                   ::  no/@ text line
      {$mor p/(list speech)}                            ::  multiplex
      {$app p/@tas q/@t}                                ::  app message
      $:  $api                                          ::  api message
          service/@tas                                  ::  service name
          id/@t                                         ::  id on the service
          id-url/purf                                   ::  link to id
          summary/@t                                    ::  summary of event
          body/@t                                       ::  body of event
          url/purf                                      ::  link to event
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
