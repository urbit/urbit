::
::::  /hoon/talk/sur
  !:
|%
::
::TODO  use different words for different kinds of burdens
::
::>  ||
::>  ||  %wrappers
::>  ||
::>    wrapper molds, for semantic clarity.
::+|
::
::TODO  rename
++  naem  term                                          ::<  circle name
++  nick  cord                                          ::<  local nickname
::
::>  ||
::>  ||  %query-models
::>  ||
::>    models relating to queries, their results and updates.
::+|
::
++  query                                               ::>  query paths
  $%  {$reader $~}                                      ::<  shared ui state
      {$burden who/ship}                                ::<  duties to share
      {$report $~}                                      ::<  duty reports
      {$circle nom/naem ran/range}                      ::<  story query
      ::{$circle nom/naem wat/circle-data}                ::<  story query
      ::  okay, the problem here is that we want to have
      ::  separate subscriptions for the different kinds
      ::  of story data. we can do that, but then if we
      ::  want all data and a specific range for the
      ::  messages (very common) then we need to do
      ::  three separate subscriptions.
      ::  possible solution would be adding range to all
      ::  story queries, but that feels weird irt
      ::  presence and configs.
      ::  (also, that would make for poor query design:
      ::   having a ~ halfway through is ugly.)
      ::
      ::  /circle/name/range
      ::  /circle/name/all
      ::  /circle/name/grams/range
      ::  /circle/name/crowd/local
      ::  /circle/name/grams&crowd/range/local
      ::  /circle/name/grams
      ::  /circle/name/crowd
      ::
      ::TODO  look at graphql and think about doing
      ::      multiple queries in a single %peer.
      ::TODO  being able to specify desired message
      ::      types may or may not also be nice to have.
      ::      ie just %app msgs, for a dashboard display.
  ==                                                    ::
::++  circle-data                                         ::>  queried data
::  $%  {$all $~}                                         ::<  everything, always
::      {$grams ran/range}                                ::<  messages (in range)
::      {$crowd wer/where}                                ::<  presence
::      {$lobby wer/where}                                ::<  configurations
::  ==                                                    ::
++  range                                               ::>  inclusive msg range
  %-  unit                                              ::<  ~ means everything
  $:  hed/place                                         ::<  start of range
      tal/(unit place)                                  ::<  opt end of range
  ==                                                    ::
++  place                                               ::>  range indicators
  $%  {$da @da}                                         ::<  date
      {$ud @ud}                                         ::<  message number
  ==                                                    ::
::TODO  overlaps with agent's ++where
::++  where                                               ::>  data from
::  %-  unit                                              ::<  ~ means everywhere
::  ?($local $remote)                                     ::<  local or remote only
++  prize                                               ::>  query result
  $%  {$reader prize-reader}                            ::<  /reader
      {$friend cis/(set circle)}                        ::<  /friend
      {$burden sos/(map naem burden)}                   ::<  /burden
      ::TODO  do we ever use remote things from remote circles?
      {$circle package}                                 ::<  /circle
  ==                                                    ::
++  prize-reader                                        ::
  $:  gys/(jug char audience)                           ::<  glyph bindings
      nis/(map ship nick)                               ::<  local nicknames
  ==                                                    ::
++  rumor                                               ::<  query result change
  $%  {$reader rum/rumor-reader}                        ::<  /reader
      {$friend add/? cir/circle}                        ::<  /friend
      {$burden nom/naem rum/rumor-story}                ::<  /burden
      {$circle rum/rumor-story}                         ::<  /circle
  ==                                                    ::
++  rumor-reader                                        ::<  changed ui state
  $%  {$glyph diff-glyph}                               ::<  un/bound glyph
      {$nick diff-nick}                                 ::<  changed nickname
  ==                                                    ::
++  shipment                                            ::>  standard payload
  $:  cos/lobby                                         ::<  loc & rem configs
      pes/crowd                                         ::<  loc & rem presences
  ==                                                    ::
++  burden                                              ::<  full story state
  $:  gaz/(list telegram)  ::TODO  envelope?            ::<  all messages
      shipment
  ==                                                    ::
++  package                                             ::<  story state
  $:  nes/(list envelope)                               ::<  messages
      shipment
  ==                                                    ::
++  diff-glyph  {bin/? gyf/char aud/audience}           ::<  un/bound glyph
++  diff-nick   {who/ship nic/nick}                     ::<  changed nickname
++  diff-story                                          ::>  story change
  $%  {$new cof/config}                                 ::<  new story
      {$bear bur/burden}                                ::<  new inherited story
      {$config cir/circle dif/diff-config}              ::<  new/changed config
      {$status cir/circle who/ship dif/diff-status}     ::<  new/changed status
      {$remove $~}                                      ::<  removed story
  ==                                                    ::
++  rumor-story                                         ::>  story rumor
  $?  diff-story                                        ::<  both in & outward
  $%  {$gram nev/envelope}                              ::<  new/changed msgs
  ==  ==                                                ::
++  diff-config                                         ::>  config change
  ::TODO  maybe just full? think.
  $%  {$full cof/config}                                ::<  set w/o side-effects
      {$source add/? src/source}                        ::<  add/rem sources
      {$caption cap/cord}                               ::<  changed description
      {$filter fit/filter}                              ::<  changed filter
      {$secure sec/security}                            ::<  changed security
      {$permit add/? sis/(set ship)}                    ::<  add/rem to b/w-list
      {$remove $~}                                      ::<  removed config
  ==                                                    ::
++  diff-status                                         ::>  status change
  $%  {$full sat/status}                                ::<  fully changed status
      {$presence pec/presence}                          ::<  changed presence
      {$human dif/diff-human}                           ::<  changed name
      {$remove $~}                                      ::<  removed status
  ==                                                    ::
++  diff-human                                          ::>  name change
  $%  {$full man/human}                                 ::<  fully changed name
      {$handle han/(unit cord)}                         ::<  changed handle
      {$true tru/(unit truename)}                       ::<  changed true name
  ==                                                    ::
::
::>  ||
::>  ||  %reader-communication
::>  ||
::>    broker interfaces for readers.
::+|
::
++  action                                              ::>  user action
  $%  ::  circle configuration                          ::
      {$create nom/naem des/cord sec/security}          ::<  create circle
      {$delete nom/naem why/(unit cord)}                ::<  delete + announce
      {$depict nom/naem des/cord}                       ::<  change description
      {$filter nom/naem fit/filter}                     ::<  change message rules
      {$permit nom/naem inv/? sis/(set ship)}           ::<  invite/banish
      {$source nom/naem sub/? srs/(set source)}         ::<  un/sub to/from src
      ::  messaging                                     ::
      {$convey tos/(list thought)}                      ::<  post exact
      {$phrase aud/audience ses/(list speech)}          ::<  post easy
      ::  personal metadata                             ::
      {$notify aud/audience pes/(unit presence)}        ::<  our presence update
      {$naming aud/audience man/human}                  ::<  our name update
      ::  changing shared ui                            ::
      {$glyph gyf/char aud/audience bin/?}              ::<  un/bind a glyph
      {$nick who/ship nic/nick}                         ::<  new identity
  ==                                                    ::
::
::>  ||
::>  ||  %broker-communication
::>  ||
::>    structures for communicating between brokers.
::+|
::
++  command                                             ::>  effect on story
  $%  {$publish tos/(list thought)}                     ::<  deliver
      {$present nos/(set naem) dif/diff-status}         ::<  status update
      {$bearing $~}                                     ::<  prompt to listen
  ==                                                    ::
::
::>  ||
::>  ||  %circles
::>  ||
::>    messaging targets and their metadata.
::+|
::
++  circle     {hos/ship nom/naem}                      ::<  native target
::  circle configurations.                              ::
++  lobby      {loc/config rem/(map circle config)}     ::<  our & srcs configs
++  config                                              ::>  circle config
  $:  src/(set source)                                  ::<  active sources
      cap/cord                                          ::<  description
      fit/filter                                        ::<  message rules
      con/control                                       ::<  restrictions
  ==                                                    ::
++  source  {cir/circle ran/range}                      ::<  subscription target
++  filter                                              ::>  content filters
  $:  cas/?                                             ::<  dis/allow capitals
      utf/?                                             ::<  dis/allow non-ascii
      ::TODO  maybe message length
  ==                                                    ::
++  control    {sec/security sis/(set ship)}            ::<  access control
++  security                                            ::>  security mode
  $?  $channel                                          ::<  channel, blacklist
      $village                                          ::<  village, whitelist
      $journal                                          ::<  journal, author list
      $mailbox                                          ::<  mailbox, our r, bl w
  ==                                                    ::
::  participant metadata.                               ::
::TODO  think about naming more
++  crowd      {loc/group rem/(map circle group)}       ::<  our & srcs presences
++  group      (map ship status)                        ::<  presence map
++  status     {pec/presence man/human}                 ::<  participant
++  presence                                            ::>  status type
  $?  $gone                                             ::<  absent
      $idle                                             ::<  idle
      $hear                                             ::<  present
      $talk                                             ::<  typing
  ==                                                    ::
++  human                                               ::>  human identifier
  $:  han/(unit cord)                                   ::<  handle
      tru/(unit truename)                               ::<  true name
  ==                                                    ::
++  truename   {fir/cord mid/(unit cord) las/cord}      ::<  real-life name
::
::>  ||
::>  ||  %message-data
::>  ||
::>    structures for containing main message data.
::+|
::
::TODO  some structure for extra message state
::      local (to readers): delivery state, read flags
::      remote (to halls): sequence nr
++  envelope   {num/@ud gam/telegram}                   ::<  outward message
++  telegram   {aut/ship thought}                       ::<  whose message
++  thought                                             ::>  inner message
  $:  uid/serial                                        ::<  unique identifier
      aud/audience                                      ::<  destinations
      wen/@da                                           ::<  timestamp
      sep/speech                                        ::<  content
  ==                                                    ::
++  speech                                              ::>  content body
  $%  {$lin pat/? msg/cord}                             ::<  no/@ text line
      {$url url/purf:eyre}                              ::<  parsed url
      {$exp exp/cord res/(list tank)}                   ::<  hoon line
      {$ire top/serial sep/speech}                      ::<  in reply to
      {$fat tac/attache sep/speech}                     ::<  attachment
      {$inv inv/? cir/circle}                           ::<  inv/ban for circle
      {$app app/term msg/cord}                          ::<  app message
  ==                                                    ::
++  attache                                             ::>  attachment
  $%  {$name nom/cord tac/attache}                      ::<  named attachment
      {$text (list cord)}                               ::<  text lines
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
++  audience   (set circle)                             ::<  destinations
++  tracking   (map circle delivery)                    ::>  delivery per target
++  delivery                                            ::>  delivery state
  $?  $pending                                          ::<  undelivered
      $accepted                                         ::<  received
      $rejected                                         ::<  denied
  ==                                                    ::
--
