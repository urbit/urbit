# Some utility functions:

rec {

  # The inverse of builtins.listToAttrs
  attrsToList = o:
    map (a: { name=a; value=builtins.getAttr a o; })
      (builtins.attrNames o);

  # ∀o,x,y. produce o' such that o'.y == o.x.y (assuming no conflicts)
  flattenSet = o:
    builtins.foldl' (acc: v: acc // v) {}
      (builtins.attrValues o);

  prefixSetAttrs = prefix: o:
    builtins.listToAttrs
      (map ({name, value}: { name=prefix + name; value=value; })
        (attrsToList o));

  # ∀o,x,y. produce o' such that o'.x-y == o.x.y
  flattenSetPrefix = o:
    (builtins.foldl' (acc: o: acc // o) {}
      (map ({name, value}: prefixSetAttrs name value)
        (attrsToList o)));

}
