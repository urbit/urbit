|_  [=bowl:gall level=@ud prefix=tape]
++  log
  |=  pri=@
  |=  mes=tape
  ^+  same
  ?:  (lth pri level)  same
  ~>  %slog.[pri %leaf (weld prefix mes)]
  same
++  info  (log 1)
++  warn  (log 2)
++  err   (log 3)
--
