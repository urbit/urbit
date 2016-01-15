/* j/6/type.c
**
*/

/* new template for type switching
*/
{
  u3_noun p_sut, q_sut, r_sut;

  if ( c3n == u3du(sut) ) switch ( sut ) {
    default: return u3m_bail(c3__fail);

    case c3__noun:
    {
    }
    case c3__void:
    {
    }
  }
  else switch ( u3h(sut) ) {
    default: return u3m_bail(c3__fail);

    case c3__atom: u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__cell: u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__core: u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__face: u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__fuss: u3x_cell(u3t(sut), &p_sut, &q_sut);
    {
    }
    case c3__fork: p_sut = u3t(sut);
    {
    }
    case c3__hold: p_sut = u3t(sut);
    {
    }
  }
}
