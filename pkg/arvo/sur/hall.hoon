::
::::  /sur/hall/hoon
  ::
^?
|%
::
::TODO  use different words for different kinds of burdens
::TODO  rename det/delta in most place? they may be (different kinds of) deltas,
::      but location in control flow already indicates delta-ness.
::
::  #
::  #  %wrappers
::  #
::    wrapper molds, for semantic clarity.
+|  %wrappers
::
::TODO  rename
++  name  term                                          ::  circle name
++  nick  cord                                          ::  local nickname
++  tags  (set knot)                                    ::  usage tags
::
::  #
::  #  %query-models
::  #
::    models relating to queries, their results and updates.
+|  %query-models
::
++  query                                               ::  query paths
  $%  {$client ~}                                       ::  shared ui state
      {$circles who/ship}                               ::  readable circles
      {$public ~}                                       ::  public memberships
      {$burden who/ship}  ::TODO  eventually, nom/name. ::  duties to share
      {$report ~}                                       ::  duty reports
      {$peers nom/name}                                 ::  readers of story
      $:  $circle                                       ::  story query
          nom/name                                      ::  circle name
          wer/(unit circle)                             ::  from source
          wat/(set circle-data)                         ::  data to get
          ran/range                                     ::  query duration
      ==                                                ::
      ::TODO  in the future, we may want much more      ::
      ::      detailed querying abilities.              ::
  ==                                                    ::
++  circle-data                                         ::  kinds of circle data
  $?  $grams                                            ::  messages
      $group-l                                          ::  local presence
      $group-r                                          ::  remote presences
      $config-l                                         ::  local config
      $config-r                                         ::  remote configs
  ==                                                    ::
++  range                                               ::  inclusive msg range
  %-  unit                                              ::  ~ means everything
  $:  hed/place                                         ::  start of range
      tal/(unit place)                                  ::  opt end of range
  ==                                                    ::
++  place                                               ::  range indicators
  $%  {$da @da}                                         ::  date
      {$ud @ud}                                         ::  message number
      {$sd @sd}                                         ::  previous messages
  ==                                                    ::
++  prize                                               ::  query result
  $%  {$client prize-client}                            ::  /client
      {$circles cis/(set name)}                         ::  /circles
      {$public cis/(set circle)}                        ::  /public
      {$burden sos/(map name burden)}                   ::  /burden
      {$report ~}                                       ::  /report
      {$peers pes/(jar ship query)}                     ::  /peers
      {$circle package}                                 ::  /circle
  ==                                                    ::
++  prize-client                                        ::  shared ui state
  $:  gys/(jug char audience)                           ::  glyph bindings
      nis/(map ship nick)                               ::  local nicknames
  ==                                                    ::
++  rumor                                               ::  query result change
  $%  {$client rum/rumor-client}                        ::  /client
      {$circles add/? cir/name}                         ::  /circles
      {$public add/? cir/circle}                        ::  /public
      {$burden nom/name rum/rumor-story}                ::  /burden
      {$peers add/? who/ship qer/query}                 ::  /peers
      {$circle rum/rumor-story}                         ::  /circle
  ==                                                    ::
++  rumor-client                                        ::  changed ui state
  $%  {$glyph diff-glyph}                               ::  un/bound glyph
      {$nick diff-nick}                                 ::  changed nickname
  ==                                                    ::
++  shipment                                            ::  standard payload
  $:  cos/lobby                                         ::  loc & rem configs
      pes/crowd                                         ::  loc & rem presences
  ==                                                    ::
++  burden                                              ::  full story state
  $:  gaz/(list telegram)                               ::  all messages
      shipment                                          ::  metadata
  ==                                                    ::
++  package                                             ::  story state
  $:  nes/(list envelope)                               ::  messages
      shipment                                          ::  metadata
  ==                                                    ::
++  diff-glyph  {bin/? gyf/char aud/audience}           ::  un/bound glyph
++  diff-nick   {who/ship nic/nick}                     ::  changed nickname
++  diff-story                                          ::  story change
  $%  {$new cof/config}                                 ::  new story
      {$bear bur/burden}                                ::  new inherited story
      {$peer add/? who/ship qer/query}                  ::  gain/lose subscriber
      {$config cir/circle dif/diff-config}              ::  new/changed config
      {$status cir/circle who/ship dif/diff-status}     ::  new/changed status
      {$remove ~}                                       ::  removed story
  ==                                                    ::
++  rumor-story                                         ::  story rumor
  $%  {$gram src/circle nev/envelope}                   ::  new/changed message
      diff-story                                        ::  both in & outward
  ==                                                    ::
++  diff-config                                         ::  config change
  $%  {$full cof/config}                                ::  set w/o side-effects
      {$source add/? src/source}                        ::  add/rem sources
      {$caption cap/cord}                               ::  changed description
      {$usage add/? tas/tags}                           ::  add/rem usage tags
      {$filter fit/filter}                              ::  changed filter
      {$secure sec/security}                            ::  changed security
      {$permit add/? sis/(set ship)}                    ::  add/rem to b/w-list
      {$remove ~}                                       ::  removed config
      {$read red/@ud}                                   ::  changed read message
  ==                                                    ::
++  diff-status                                         ::  status change
  $%  {$full sat/status}                                ::  fully changed status
      {$presence pec/presence}                          ::  changed presence
      {$human dif/diff-human}                           ::  changed name
      {$remove ~}                                       ::  removed status
  ==                                                    ::
++  diff-human                                          ::  name change
  $%  {$full man/human}                                 ::  fully changed name
      {$handle han/(unit cord)}                         ::  changed handle
      {$true tru/(unit truename)}                       ::  changed true name
  ==                                                    ::
::
::  #
::  #  %client-communication
::  #
::    hall interfaces for clients.
+|  %client-communication
::
++  action                                              ::  user action
  $%  ::  circle configuration                          ::
      {$create nom/name des/cord sec/security}          ::  create circle
      {$design nom/name cof/config}                     ::  create with config
      {$delete nom/name why/(unit cord)}                ::  delete + announce
      {$depict nom/name des/cord}                       ::  change description
      {$filter nom/name fit/filter}                     ::  change message rules
      {$permit nom/name inv/? sis/(set ship)}           ::  invite/banish
      {$source nom/name sub/? srs/(set source)}         ::  un/sub to/from src
      {$read nom/name red/@ud}                          ::  change read message
      {$newdm sis/(set ship)}
      {$usage nom/name add/? tas/tags}                  ::  add/rem usage tags
      ::  messaging                                     ::
      {$convey tos/(list thought)}                      ::  post exact
      {$phrase aud/audience ses/(list speech)}          ::  post easy
      ::  personal metadata                             ::
      {$notify aud/audience pes/(unit presence)}        ::  our presence update
      {$naming aud/audience man/human}                  ::  our name update
      ::  changing shared ui                            ::
      {$glyph gyf/char aud/audience bin/?}              ::  un/bind a glyph
      {$nick who/ship nic/nick}                         ::  new identity
      ::  misc changes                                  ::
      {$public add/? cir/circle}                        ::  show/hide membership
  ==                                                    ::
::
::  #
::  #  %hall-communication
::  #
::    structures for communicating between halls.
+|  %hall-communication
::
++  command                                             ::  effect on story
  $%  {$publish tos/(list thought)}                     ::  deliver
      {$present nos/(set name) dif/diff-status}         ::  status update
      {$bearing ~}                                      ::  prompt to listen
  ==                                                    ::
::
::  #
::  #  %circles
::  #
::    messaging targets and their metadata.
+|  %circles
::
++  circle     {hos/ship nom/name}                      ::  native target
::  circle configurations.                              ::
++  lobby      {loc/config rem/(map circle config)}     ::  our & srcs configs
++  config                                              ::  circle config
  $:  src/(set source)                                  ::  active sources
      cap/cord                                          ::  description
      tag/tags                                          ::  usage tags
      fit/filter                                        ::  message rules
      con/control                                       ::  restrictions
      red/@ud                                           ::  last read message
  ==                                                    ::
++  source  {cir/circle ran/range}                      ::  subscription target
++  filter                                              ::  content filters
  $:  cas/?                                             ::  dis/allow capitals
      utf/?                                             ::  dis/allow non-ascii
      ::TODO  maybe message length
  ==                                                    ::
++  control    {sec/security sis/(set ship)}            ::  access control
++  security                                            ::  security mode
  $?  $channel                                          ::  blacklist
      $village                                          ::  whitelist
      $journal                                          ::  pub r, whitelist w
      $mailbox                                          ::  our r, blacklist w
      $custom                                           ::  according to custom-rule
  ==                                                    ::
::  participant metadata.                               ::
++  crowd      {loc/group rem/(map circle group)}       ::  our & srcs presences
++  group      (map ship status)                        ::  presence map
++  status     {pec/presence man/human}                 ::  participant
++  presence                                            ::  status type
  $?  $gone                                             ::  absent
      $idle                                             ::  idle
      $hear                                             ::  present
      $talk                                             ::  typing
  ==                                                    ::
++  human                                               ::  human identifier
  $:  han/(unit cord)                                   ::  handle
      tru/(unit truename)                               ::  true name
  ==                                                    ::
++  truename   {fir/cord mid/(unit cord) las/cord}      ::  real-life name
::
::  #
::  #  %message-data
::  #
::    structures for containing main message data.
+|  %message-data
::
::TODO  some structure for extra message state
::      local (to clients): delivery state, read flags
::      remote (to halls): sequence nr
++  envelope   {num/@ud gam/telegram}                   ::  outward message
++  telegram   {aut/ship thought}                       ::  whose message
++  thought                                             ::  inner message
  $:  uid/serial                                        ::  unique identifier
      aud/audience                                      ::  destinations
      wen/@da                                           ::  timestamp
      sep/speech                                        ::  content
  ==                                                    ::
++  speech                                              ::  content body
  $%  {$lin pat/? msg/cord}                             ::  no/@ text line
      {$url url/purf:eyre}                              ::  parsed url
      {$exp exp/cord res/(list tank)}                   ::  hoon line
      {$ire top/serial sep/speech}                      ::  in reply to
      {$fat tac/attache sep/speech}                     ::  attachment
      {$app app/term sep/speech}                        ::  app message
      {$inv inv/? cir/circle}                           ::  inv/ban for circle
  ==                                                    ::
++  attache                                             ::  attachment
  $%  {$name nom/cord tac/attache}                      ::  named attachment
      {$text (list cord)}                               ::  text lines
      {$tank (list tank)}                               ::  tank list
  ==                                                    ::
::
::  #
::  #  %message-metadata
::  #
::    structures for containing message metadata.
+|  %message-metadata
::
++  serial     @uvH                                     ::  unique identifier
++  audience   (set circle)                             ::  destinations
++  tracking   (map circle delivery)                    ::  delivery per target
++  delivery                                            ::  delivery state
  $?  $pending                                          ::  undelivered
      $accepted                                         ::  received
      $rejected                                         ::  denied
  ==                                                    ::
--
