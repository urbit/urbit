::   hoon data to json 
::
::::  /hoon/map-to-json/lib
  ::
/?    310                        
=,  format
|*  {a/_cord b/_json}                 ::  XX {a/$-(* cord) b/$-(* json)}
|=  c/(map _+<.a _+<.b)
(pairs:enjs (turn (~(tap by c)) |*(d/^ [(a -.d) (b +.d)])))
