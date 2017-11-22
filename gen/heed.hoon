::BROKEN
=>  |%
    --
|*  $:  :>  vinyl: historical state (including version)
        :>  brain: working state of the application (not including version)
        :>  delta: grain of change across all state
        :>  prize: (pair mark noun) for namespace value
        :>  rumor: (pair mark noun) for namespace diff
        :>  opera: (pair bone card) for operation (old ++move)
        :>
        vinyl/mold
        brain/mold
        delta/mold 
        prize/mold         
        rumor/mold
        opera/mold
    ==
|_  $:  :>  ops: pending operations, in reverse order
        :>  ego: current state
        :>
        ops/(list opera)
        ego/brain
    ==
::                                                      ::  ++bake
++  bake                                                :<  apply delta
  |=  $:  :>  del: change
          :>  
          del/delta
      ==
  :>  core after change (including operations)
  ^-  _+>
  !!
::                                                      ::  ++cope
++  cope                                                :<  transaction result
  |=  $:  :>  weg: forward identity
          :>  het: success or error report
          :>
          weg/(list coin)
          het/(unit tang)
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !!
::                                                      ::  ++fail
++  fail                                                :<  process error
  |=  $:  :>  why: error dump
          :>
          why/tang
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !!
::                                                      ::  ++feel
++  feel                                                :<  update 
  |=  $:  :>  del: change
          :>  pex: preparsed path, inside-first
          :>
          del/delta
          pex/(list coin)
      ==
  :>  query updates in reverse order
  :>
  ^-  (list rumor)
  !!
::                                                      ::  ++hear
++  hear                                                :<  subscription update
  |=  $:  :>  weg: forward identity
          :>  
          weg/(list coin)
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !!
::                                                      ::  ++pull
++  pull                                                :<  subscription cancel
  |=  $:  :>  weg: forward identity
          :>  het: error report, if any
          :>  
          weg/(list coin)
          het/(unit tang)
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !!
::                                                      ::  ++leak
++  leak                                                :<  check access
  |=  $:  :>  lec: leakset (~ means public)
          :>  pex: preparsed path, inside-first
          :>
          lec/(unit (set ship))
          pex/(list coin)
      ==
  :>  if path `pex` is visible to ships in `lec`
  ^-  ?
  !!
::                                                      ::  ++load
++  look                                                :<  asynchronous read
  |=  $:  :>  pex: preparsed path, inside-first
          :>  
          pex/(list coin)
      ==
  :>  actions in reverse order
  ^-  _+>
  !!
::                                                      ::  ++prep 
++  prep                                                :<  load system
  |=  $:  old/vinyl
      ==
  :>  core after boot
  ^-  _+>
  !!
::                                                      ::  ++peek
++  peek                                                :<  synchronous read
  |=  $:  :>  pex: preparsed path, inside-first
          :>  
          pex/(list coin)
      ==
  :>  value at `pec`; ~ for unavailable, [~ ~] for invalid
  :>
  ^-  (unit (unit prize))
  !!
::                                                      ::  ++poke
++  poke                                                :<  generic poke
  |=  $:  :>  ost: opaque cause
          :>  msg: message with mark and vase
          :>
          ost/bone 
          msg/cage
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !! 
::                                                      ::  ++pour
++  pour                                                :<  arvo response
  |=  $:  :>  weg: forward identity
          :>  sin: response card
          :>
          weg/(list coin)
          sin/sign
      ==
  :>  actions in reverse order
  :>
  ^-  (list delta)
  !!
--
