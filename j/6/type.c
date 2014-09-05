/* j/6/type.c
**
** This file is in the public domain.
*/

/* new template for type switching
*/
{
  u2_noun p_sut, q_sut, r_sut;

  if ( u2_no == u2du(sut) ) switch ( sut ) {
    default: return u2_cm_bail(c3__fail);

    case c3__noun:
    {
    }
    case c3__void:
    {
    }
  }
  else switch ( u2h(sut) ) {
    default: return u2_cm_bail(c3__fail);

    case c3__atom: p_sut = u2t(sut);
    {
    }
    case c3__cell: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
    {
    }
    case c3__core: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
    {
    }
    case c3__cube: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
    {
    }
    case c3__face: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
    {
    }
    case c3__fork: u2_cx_cell(u2t(sut), &p_sut, &q_sut);
    {
    }
    case c3__hold: p_sut = u2t(sut);
    {
    }
  }
}
