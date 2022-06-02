^?
|%
+$  prose       [title=@t body=@t]
+$  proses      (set prose)
+$  story       (jug tako:clay prose)              :: set len > 1 means conflicting messages have been assigned
+$  chapter     [tako:clay proses]                 :: prose entry type
+$  cash        $%([%tako p=tako:clay] case)       :: used in generators to accept a tako directly as well
+$  story-diff  [additions=story deletions=story]
--