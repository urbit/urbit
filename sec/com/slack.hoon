/+    oauth2
::
::::
  ::
=+  aut=(oauth2 [`/com/slack /oauth/authorize ~] /api/'oauth.access')
|_  [(bale keys:oauth2) tok=token.aut]
++  aut  ~(. ^aut +<- /client/admin)
++  out  (out-quay:aut 'token'^tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok=token:aut +>(tok tok)))
--
