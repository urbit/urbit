|=  [vax=vase eny=@uv size=@ud]
=+  ran=~(. og eny)
=^  sam  ran  (rads:ran +(size))
=+  res=(slam vax !>(sam))
?:  =(+:res 0)
    ~&  "success"
    %.y
~&  "failure"
~&  "sample:"
~&  sam
%.n