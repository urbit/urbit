::
::::  /hoon/sole/sur
  ::
^?
|%
++  sole-action                                         ::  sole to app
  $:  id=@ta                                            ::  duct id
    $=  dat
    $%  ::  {$abo ~}                                    ::  reset interaction
        {$det sole-change}                              ::  command line edit
        {$ret ~}                                        ::  submit and clear
        {$clr ~}                                        ::  exit context
        {$tab pos/@ud}                                  ::  tab complete
    ==                                                  ::
  ==
++  sole-buffer  (list @c)                              ::  command state
++  sole-change                                         ::  network change
  $:  ler/sole-clock                                    ::  destination clock
      haw/@uvH                                          ::  source hash
      ted/sole-edit                                     ::  state change
  ==                                                    ::
++  sole-clock  {own/@ud his/@ud}                       ::  vector clock
++  sole-edit                                           ::  shared state change
  $%  {$del p/@ud}                                      ::  delete one at
      {$ins p/@ud q/@c}                                 ::  insert at
      {$mor p/(list sole-edit)}                         ::  combination
      {$nop ~}                                          ::  no-op
      {$set p/sole-buffer}                              ::  discontinuity
  ==                                                    ::
++  sole-effect                                         ::  app to sole
  $%  {$bel ~}                                          ::  beep
      {$blk p/@ud q/@c}                                 ::  blink+match char at
      {$clr ~}                                          ::  clear screen
      {$det sole-change}                                ::  edit command
      {$err p/@ud}                                      ::  error point
      {$klr p/styx}                                     ::  styled text line
      {$mor p/(list sole-effect)}                       ::  multiple effects
      {$nex ~}                                          ::  save clear command
      {$pro sole-prompt}                                ::  set prompt
      {$sag p/path q/*}                                 ::  save to jamfile
      {$sav p/path q/@}                                 ::  save to file
      {$tab p/(list {=cord =tank})}                     ::  tab-complete list
      {$tan p/(list tank)}                              ::  classic tank
  ::  {$taq p/tanq}                                     ::  modern tank
      {$txt p/tape}                                     ::  text line
      {$url p/@t}                                       ::  activate url
  ==                                                    ::
++  sole-command                                        ::  command state
  $:  pos/@ud                                           ::  cursor position
      say/sole-share                                    ::  cursor
  ==                                                    ::
++  sole-prompt                                         ::  prompt definition
  $:  vis/?                                             ::  command visible
      tag/term                                          ::  history mode
      cad/styx                                          ::  caption
  ==                                                    ::
++  sole-share                                          ::  symmetric state
  $:  ven/sole-clock                                    ::  our vector clock
      leg/(list sole-edit)                              ::  unmerged edits
      buf/sole-buffer                                   ::  sole state
  ==                                                    ::
::                                                      ::
::                                                      ::
++  sole-dialog                                         ::  standard dialog
  |*  out/$-(* *)                                       ::  output structure
  $-(sole-input (sole-result out))                      ::  output function
::                                                      ::
++  sole-input  tape                                    ::  prompt input
++  sole-result                                         ::  conditional result
  |*  out/$-(* *)                                       ::  output structure
  $@(@ud (sole-product out))                            ::  error position
::                                                      ::
++  sole-product                                        ::  success result
  |*  out/$-(* *)                                       ::
  %+  pair  (list tank)                                 ::
  %+  each  (unit out)                                  ::  ~ is abort
  (pair sole-prompt (sole-dialog out))                  ::  ask and continue
::                                                      ::
++  sole-gen                                            ::  XX virtual type
  $%  {$say $-((sole-args) (cask))}                     ::  direct noun
      {$ask $-((sole-args) (sole-product (cask)))}      ::  dialog
  ==                                                    ::
++  sole-args                                           ::  generator arguments
  |*  _[* *]                                            ::
  {{now/@da eny/@uvJ bek/beak} {,+<- ,+<+}}             ::
::                                                      ::
::                                                      ::
--
