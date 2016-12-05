::   hoon data to json 
::
::::  /hoon/map-to-json/lib
  ::
/?    310                        
|*  {a/_cord b/_json}                 ::  XX {a/$-(* cord) b/$-(* json)}
|=  c/(map _+<.a _+<.b)
(jobe:js:eyre (turn (~(tap by c)) |*(d/^ [(a -.d) (b +.d)])))
