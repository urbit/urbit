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
      {$create nom/knot des/cord sec/posture}           ::<  create circle
      {$source nom/knot sub/? src/(set partner)}        ::<  un/sub p to/from r
      {$depict nom/knot des/cord}                       ::<  change description
      {$permit nom/knot inv/? sis/(set ship)}           ::<  invite/banish
      {$delete nom/knot ano/(unit cord)}                ::<  delete + announce
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
      {$precs reg/register}                             ::<  presences
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
  ==                                                    ::
++  report                                              ::>  update
  $%  {$cabal cab/cabal}                                ::<  config neighborhood
      {$group reg/register}                             ::<  presence
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
::>  circle configurations.
++  cabal                                               ::>  metaconfiguration
  $:  loc/config                                        ::<  local config
      rem/(map circle config)                           ::<  neighborhood configs
  ==                                                    ::
++  config                                              ::>  circle config
  $:  src/(set partner)                                 ::<  pulls from
      cap/cord                                          ::<  description
      con/control                                       ::<  restrictions
  ==                                                    ::
++  control    {sec/posture ses/(set ship)}             ::<  access control
++  posture                                             ::>  security kind
  $?  $black                                            ::<  channel, blacklist
      $white                                            ::<  village, whitelist
      $green                                            ::<  journal, author list
      $brown                                            ::<  mailbox, our r, bl w
  ==                                                    ::
::>  participant metadata.
++  register   {loc/atlas rem/(map partner atlas)}      ::<  our & srcs presences
++  atlas      (map ship status)                        ::<  presence map
++  status     {pec/presence man/human}                 ::<  participant
++  presence                                            ::>  status type
  $?  $gone                                             ::<  left
      $idle                                             ::<  idle
      $hear                                             ::<  present
      $talk                                             ::<  typing
  ==
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
      {$fat tac/torso sep/speech}                       ::<  attachment
      {$lan nom/knot msg/@t}                            ::<  local announce
      {$inv inv/? sat/circle}                           ::<  inv/ban for circle
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
++  torso                                               ::>  attachment
  $%  {$name nom/@t tac/torso}                          ::<  named attachment
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
