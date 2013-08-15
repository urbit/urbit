/* j/6/type.c
**
** This file is in the public domain.
*/

/* new template for type switching
*/
{
  u2_noun p_sut, q_sut;

  if ( u2_no == u2_dust(sut) ) switch ( sut ) {
    default: return u2_bl_bail(wir_r, c3__fail);

    case c3__atom:
    {
    }
    case c3__noun: 
    {
    }
    case c3__void:
    {
    }
  }
  else switch ( u2_h(sut) ) {
    default: return u2_bl_bail(wir_r, c3__fail);

    case c3__cell: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
    {
    }
    case c3__core: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
    {
    }
    case c3__cube: p_sut = u2_t(sut);
    {
    }
    case c3__face: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
    {
    }
    case c3__fork: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
    {
    }
    case c3__hold: u2_bi_cell(wir_r, u2_t(sut), &p_sut, &q_sut);
    {
    }
  }
}
