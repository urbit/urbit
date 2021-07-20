/+  libstrand=strand
=,  strand=strand:libstrand
|%
+$  thread  $-(vase _*form:(strand ,vase))
+$  input   [=tid =cage]
+$  tid     tid:strand
+$  bowl    bowl:strand
+$  http-error
  $?  %bad-request   :: 400
      %forbidden     :: 403
      %nonexistent   :: 404
      %offline       :: 504
  ==
--
