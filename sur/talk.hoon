::
::::  /hoon/talk/sur
  !:
|%
::
::>  ||
::>  ||  %reader-communication
::>  ||
::>    broker interfaces for readers.
::+|
::
++  action                                              ::>  user action
  $%  ::  circle configuration                          ::
      {$create nom/knot des/cord sec/security}          ::<  create circle
      {$source nom/knot sub/? src/(set partner)}        ::<  un/sub p to/from r
      {$depict nom/knot des/cord}                       ::<  change description
      {$permit nom/knot inv/? sis/(set ship)}           ::<  invite/banish
      {$delete nom/knot ano/(unit cord)}                ::<  delete + announce
      {$enlist nom/knot fed/? sis/(set ship)}           ::<  dis/allow federation
      {$burden circle}                                  ::<  help federate
      ::  messaging                                     ::
      {$convey tos/(list thought)}                      ::<  post exact
      {$phrase aud/(set partner) ses/(list speech)}     ::<  post easy
      ::  personal metadata                             ::
      ::TODO  change to target partners, not only our circles.
      {$status nos/(set knot) sat/status}               ::<  our status update
      ::  changing shared ui                            ::
      {$human sip/ship man/human}                       ::<  new identity
      {$glyph gyf/char pas/(set partner) bin/?}         ::<  un/bind a glyph
  ==                                                    ::
++  reaction                                            ::>  user information
  $:  res/?($info $fail)                                ::<  result
      wat/@t                                            ::<  explain
      why/(unit action)                                 ::<  cause
  ==                                                    ::
++  lowdown                                             ::>  new/changed state
  $%  ::  story state                                   ::
      $:  $confs                                        ::<  configs
          loc/(unit config)                             ::<  local config
          rem/(map circle (unit config))                ::<  remote configs
      ==                                                ::
      {$precs reg/crowd}                                ::<  presences
      {$grams num/@ud gaz/(list telegram)}              ::<  messages
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
  $%  {$review tos/(list thought)}                      ::<  deliver
      $:  $burden                                       ::<  starting fed state
          nom/knot
          cof/lobby
          pes/crowd
          gaz/(list telegram)
      ==
      {$relief nom/knot who/(set ship)}                 ::<  federation ended
  ==                                                    ::
++  report                                              ::>  update
  $%  {$lobby cab/lobby}                                ::<  config neighborhood
      {$crowd reg/crowd}                                ::<  presence
      {$grams num/@ud gaz/(list telegram)}              ::<  thoughts
  ==                                                    ::
::
::>  ||
::>  ||  %circles
::>  ||
::>    messaging targets and their metadata.
::+|
::
++  partner    (each circle passport)                   ::<  message target
++  circle     {hos/ship nom/knot}                      ::<  native target
++  passport                                            ::>  foreign target
  $%  {$twitter p/@t}                                   ::<  twitter handle
  ==                                                    ::
::  circle configurations.                              ::
++  lobby      {loc/config rem/(map circle config)}     ::<  our & srcs configs
++  config                                              ::>  circle config
  $:  src/(set partner)                                 ::<  pulls from
      cap/cord                                          ::<  description
      con/control                                       ::<  restrictions
      fed/federal                                       ::<  federators
  ==                                                    ::
++  control    {sec/security ses/(set ship)}            ::<  access control
++  security                                            ::>  security kind
  $?  $black                                            ::<  channel, blacklist
      $white                                            ::<  village, whitelist
      $green                                            ::<  journal, author list
      $brown                                            ::<  mailbox, our r, bl w
  ==                                                    ::
++  federal    {may/(set ship) fes/(set ship)}          ::<  federation control
::  participant metadata.                               ::
++  crowd      {loc/group rem/(map partner group)}      ::<  our & srcs presences
++  group      (map ship status)                        ::<  presence map
++  status     {pec/presence man/human}                 ::<  participant
++  presence                                            ::>  status type
  $?  $gone                                             ::<  left
      $idle                                             ::<  idle
      $hear                                             ::<  present
      $talk                                             ::<  typing
  ==                                                    ::
++  human                                               ::>  human identifier
  $:  tru/(unit (trel @t (unit @t) @t))                 ::<TODO  unused true name
      han/(unit @t)                                     ::<  handle
  ==                                                    ::
::
::>  ||
::>  ||  %message-data
::>  ||
::>    structures for containing main message data.
::+|
::
++  telegram   {aut/ship tot/thought}                   ::<  who thought
++  thought    {uid/serial aud/audience sam/statement}  ::<  which whom what
++  statement  {wen/@da boq/bouquet sep/speech}         ::<  when this
++  speech                                              ::>  narrative action
  $%  {$non $~}                                         ::<  no content (yo)
      {$lin pat/? msg/@t}                               ::<  no/@ text line
      {$ire tos/serial sep/speech}                      ::<  in-reply-to
      {$url url/purf}                                   ::<  parsed url
      {$exp exp/@t}                                     ::<  hoon line
      {$fat tac/attache sep/speech}                     ::<  attachment
      {$lan nom/knot msg/@t}                            ::<  local announce
      {$inv inv/? cir/circle}                           ::<  inv/ban for circle
      {$mor ses/(list speech)}                          ::<  multiplex
      {$ext nom/@tas dat/*}                             ::<  extended action
      {$app app/@tas msg/@t}                            ::<  app message
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
++  attache                                             ::>  attachment
  $%  {$name nom/@t tac/attache}                        ::<  named attachment
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
++  envelope   {vis/? sen/(unit partner)}               ::<  visible sender
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
