::  Test url +https://api.dropboxapi.com/2/files/list_folder
::::  /hoon/dropboxapi/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://www.dropbox.com/1/oauth2/authorize?response_type=code'
    'https://api.dropboxapi.com/1/oauth2/token'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /)
++  out
  |=  a/hiss
  =;  mow  ~&  db-authorized+mow  mow
  %.  a
  (out-math:aut tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--
::  
