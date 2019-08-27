/-  *inbox
|_  act=inbox-action
++  grab
  |%
  ++  noun  inbox-action
  ++  json  
    |=  jon=^json
    %-  action:publish
    =<  (action jon)
    |%
    ++  action
      %-  of:dejs
      :~  create+create
          delete+delete
          message+message
          read+read
      ==
    --
  --
--
