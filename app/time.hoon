::
::::  /hoon/time/app
  ::
/?    310
|%
++  card  {$wait wire @da}
--
|_  {bowl:gall ~}
++  poke-noun
  |=  *
  :_  +>.$  :_  ~
  [ost %wait /(scot %da now) +(now)]
::
++  wake
  |=  {wir/wire error=(unit tang)}
  ?>  ?=({@ ~} wir)
  ?^  error
    %-  (slog u.error)
    ~&  %time-behn-failed
    [~ +>.$]
  ~&  [%took `@dr`(sub now (slav %da i.wir))]
  [~ +>.$]
--
