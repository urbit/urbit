|%
++  bit-any
  $%  {$bit-get-token bit-get-token}
      {$bit-api-call tok+@t bit-api-call}
  ==
++  bit-api-call
  $%  {$send bit-send}
      {$txt-send to+@t btc+@t}
      {$buy amount+@t currency+@t}
      {$sell amount+@t currency+@t}
      {$list $~}
  ==
++  bit-send  {adr+@uc btc+@t}
++  bit-ship-send  {who+@p btc+@t}
++  bit-get-token  {oat+@t {cid+@t sec+@t} red+purl}
++  bit-accounts  (list {id+@t name+@t balance+{num+@t cur+term}})
--

