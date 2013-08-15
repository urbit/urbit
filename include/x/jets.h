/* include/x/jets.h
**
** This file is in the public domain.
*/
  /** Jet definitions - generic include.
  ***
  ***   A wet jet is a gate jet for which a C function is defined.
  ***   A dry jet is one for which there is none.
  ***
  ***   For both, the declaration is (name source priority).
  **/
    /** Tier 1.
    **/
      _xj_dry(watt, 0)

      _xj_wet(add, 0x7add81a3)
      _xj_wet(bex, 0x798b3541)
      _xj_wet(dec, 0x34d5c589)
      _xj_wet(div, 0x7114bb2d)
      _xj_wet(gte, 0x253afebb)
      _xj_wet(gth, 0x7b997088)
      _xj_wet(lte, 0x789b3377)
      _xj_wet(lth, 0x749cbb04)
      _xj_wet(mod, 0x6cebd374)
      _xj_wet(mul, 0x5ed515e)
      _xj_dry(nop, 0)
      _xj_wet(sub, 0x793fa8ef)

      _xj_dry(cap, 0)
      _xj_dry(cat, 0)
      _xj_dry(con, 0)
      _xj_dry(cut, 0)
      _xj_dry(dis, 0)
      _xj_dry(dor, 0)
      _xj_dry(end, 0x366a0662)
      _xj_dry(gor, 0x626117ed)
      _xj_dry(lsh, 0)
      _xj_dry(mix, 0)
      _xj_dry(met, 0)
      _xj_dry(mas, 0)
      _xj_wet(mug, 0x1f9ebe4e)
      _xj_dry(peg, 0x2795f950)
      _xj_dry(rap, 0)
      _xj_dry(rip, 0x5cab12bf)
      _xj_dry(rsh, 0x2460fed5)
      _xj_dry(vor, 0)

      _xj_wet(make, 0)
      _xj_wet(mill, 0)
      _xj_wet(pass, 0)
      _xj_wet(play, 0)
      _xj_dry(plow, 0)
      _xj_wet(read, 0)
      _xj_dry(rose, 0)
      _xj_wet(shop, 0)
      _xj_wet(show, 0)
      _xj_wet(wish, 0)
