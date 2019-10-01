/-  *lyre
/+  *lyre, *lyre-theme
/=  send  /:  /===/lib/lyre/send  /png/
^-  dom
:+  %list
  ~[[%axis %row] w-100 h-100]
:~  :+  %list
      ~[[%axis %col] [%basis %pix 320] [%br %.y]]
    :~  [%box ~[[%basis %pix 88] [%bb %.y]] [%text ~[fs-6 fw-1 gray-2 [%ml 10]] 'Chat']]
        [%box ~[[%grow 1]] [%empty ~]]
    ==
    :+  %list
      ~[[%axis %col] [%grow 1]]
    :~  [%box ~[[%basis %pix 88] [%bb %.y]] [%empty ~]]
        [%box ~[[%grow 1] [%bb %.y]] [%empty ~]]
        [%box ~[[%basis %pix 96]] [%empty ~]]
    ==
==
