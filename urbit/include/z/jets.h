/* include/z/jets.h
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
      _zj_dry(watt, 0)

      _zj_wet(add, 0x7add81a3)
      _zj_wet(bex, 0x798b3541)
      _zj_wet(dec, 0x34d5c589)
      _zj_wet(div, 0x7114bb2d)
      _zj_wet(gte, 0x253afebb)
      _zj_wet(gth, 0x7b997088)
      _zj_wet(lte, 0x789b3377)
      _zj_wet(lth, 0x749cbb04)
      _zj_wet(mod, 0x6cebd374)
      _zj_wet(mul, 0x5ed515e)
      _zj_dry(nop, 0)
      _zj_wet(sub, 0x793fa8ef)

      _zj_dry(cap, 0)
      _zj_dry(cat, 0)
      _zj_dry(con, 0)
      _zj_dry(cut, 0)
      _zj_dry(dis, 0)
      _zj_dry(dor, 0)
      _zj_dry(end, 0x366a0662)
      _zj_dry(gor, 0x626117ed)
      _zj_dry(lsh, 0)
      _zj_dry(mix, 0)
      _zj_dry(met, 0)
      _zj_dry(mas, 0)
      _zj_wet(mug, 0x1f9ebe4e)
      _zj_dry(peg, 0x2795f950)
      _zj_dry(rap, 0)
      _zj_dry(rip, 0x5cab12bf)
      _zj_dry(rsh, 0x2460fed5)
      _zj_dry(vor, 0)

      _zj_wet(make, 0)
      _zj_wet(mill, 0)
      _zj_wet(pass, 0)
      _zj_wet(play, 0)
      _zj_dry(plow, 0)
      _zj_wet(read, 0)
      _zj_dry(rose, 0)
      _zj_wet(shop, 0)
      _zj_wet(show, 0)
      _zj_wet(wish, 0)
