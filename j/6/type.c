/* j/6/type.c
**
** This file is in the public domain.
*/

/* new template for type switching
*/
{
  u3_noun p_sut, q_sut, r_sut;

  if ( c3n == u3du(sut) ) switch ( sut ) {
    default: return u3_cm_bail(c3__fail);

    case c3__noun:
    {
    }
    case c3__void:
    {
    }
  }
  else switch ( u3h(sut) ) {
    default: return u3_cm_bail(c3__fail);

    case c3__atom: p_sut = u3t(sut);
    {
    }
    case c3__cell: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__core: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__cube: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__face: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__fork: u3_cx_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__hold: p_sut = u3t(sut);
    {
    }
  }
}
