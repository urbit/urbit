|%
+$  word
  $%  [%lam p=$-(word word)]
      [%pi p=word q=$-(word word)]
      [%appl p=word q=word]
      [%ann p=word q=word]
      [%free p=@]
      [%star ~]
      [%box ~]
  ==
+$  unfurl
  |=  [lvl=@ f=$-(word word)]
  (f %free lvl)
::+$  unfurl2
:: |=  [lvl=@ f=$-(word word) g=$-(word word
::  (f %free lvl)
::let unfurl lvl f = f (FreeVar lvl)
::let unfurl2 lvl (f, g) = (unfurl lvl f, unfurl lvl g)
::let rec print lvl =
::  let plunge f = print (lvl + 1) (unfurl lvl f) in
::  function
::  | Lam f -> "(λ" ^ plunge f ^ ")"
::  | Pi (a, f) -> "(Π" ^ print lvl a ^ "." ^ plunge f ^ ")"
::  | Appl (m, n) -> "(" ^ print lvl m ^ " " ^ print lvl n ^ ")"
::  | Ann (m, a) -> "(" ^ print lvl m ^ " : " ^ print lvl a ^ ")"
::  | FreeVar x -> string_of_int x
::  | Star -> "*"
::  | Box -> "☐"
++  eval
  |=  wor=word
  ^-  word
  ?-    -.wor  !!
      %lam   [%lam |=(w=word (eval (p.wor w)))]
      %pi    [%pi (eval p.wor) |=(w=word (eval (q.wor w)))]
      %ann   (eval p.wor)
      ?(%free %star %box)  wor
      %appl
    =/  [m=word n=word]  [(eval p.wor) (eval q.wor)]
    ?.  ?=(%lam -.m)
      [%appl m n]
    (p.m q)
  ==
++  equate
  |=  [lvl=@ f=word g=word]
  =*  eq  $
  |^  ^-  ?
  ?+  [-.f -.g]  %.n
    [%lam %lam]  ?>(?=(%lam -.g) (plunge p.f p.g))
    [%pi %pi]    ?>(?=(%pi -.g) &(eq(p.f p.g) eq(q.f q.g)))
    [%appl %appl]  ?>(?=(%appl -.g) &(eq(p.f p.g) eq(q.f q.g)))
    [%ann %ann]  ?>(?=(%ann -.g) &(eq(p.f p.g) eq(q.f q.g)))
    [%free %free]  ?>(?=(%free -.g) =(p.f p.g))
    [%star %star]  %.y
    [%box %box]  %.y
  ==
  ++  plunge
    |=  [f=word q=word]  eq(lvl +(lvl), f (unfurl lvl f), g (unfurl lvl g))
  --
++  type
  |_  [lvl=@ud ctx=(list word)]
  ++  infer
    |=  w=word
    ^-  word
    ?-    -.w
        %pi 
      =+  sort
      run(lvl +(lvl), [(eval p.w) ctx] (unfurl lvl f))
    ::
        %appl
      =/  m  (run p.w)
      ?.  ?=(%pi -.m)
        ~|  "want a pi type got {<m>}"  !!
      =+  (check lvl ctx p.w p.m)
      (p.m q.w)
    ::
        %ann
      =+  (sort lvl ctx q.w)

    ==
    ++  check
      |=  [p=word q=word]


    ==
let rec infer_ty lvl ctx = function
  | Ann (m, a) ->
      let _s = infer_sort lvl ctx a in
      check_ty lvl ctx (m, eval a)
  | FreeVar x -> List.nth ctx (lvl - x - 1)
  | Star -> Box
  | Box -> panic lvl Box "Has no type"
  | t -> panic lvl t "Not inferrable"

and infer_sort lvl ctx a =
  match infer_ty lvl ctx a with
  | (Star | Box) as s -> s
  | ty -> panic lvl a "Want a sort, got %s" (print lvl ty)

and check_ty lvl ctx = function
  | Lam f, Pi (a, g) ->
      let _ = check_ty (lvl + 1) (a :: ctx) (unfurl2 lvl (f, g)) in
      Pi (a, g)
  | Lam f, ty -> panic lvl (Lam f) "Want a Pi type, got %s" (print lvl ty)
  | t, ty ->
      let got_ty = infer_ty lvl ctx t in
      if equate lvl (ty, got_ty) then ty
      else panic lvl t "Want type %s, got %s" (print lvl ty) (print lvl got_ty)
--
