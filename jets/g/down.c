/* j/g/down.c
**
*/
#include "all.h"
#include <cmark.h>
#include <node.h>
#include <buffer.h>

u3_noun node_to_noun(cmark_node * nod);

u3_noun list_elems_to_noun(cmark_node * nod)
{
  u3_noun elems = u3_nul;

  cmark_node * child;
  for ( child = nod->last_child; child; child = child->prev ) {
    elems = u3nc(node_to_noun(child),elems);
  }

  return elems;
}

u3_noun document_to_noun(cmark_node * nod)
{
  return list_elems_to_noun(nod);
}

u3_noun block_quote_to_noun(cmark_node * nod)
{
  return u3nc(u3nc(c3__bloq,u3_nul),list_elems_to_noun(nod));
}

u3_noun list_to_noun(cmark_node * nod)
{
  return
    u3nc(
      u3nt(
        c3__list,
        __(nod->as.list.tight),
        (nod->as.list.list_type == CMARK_BULLET_LIST)
          ? nod->as.list.bullet_char                         /*  XX convert?  */
          : u3nc(nod->as.list.start,
            (nod->as.list.delimiter == CMARK_PERIOD_DELIM)
            ? '.'
            : ')')),
      list_elems_to_noun(nod));
}

u3_noun list_item_to_noun(cmark_node * nod)
{
  return u3nc(u3nc(c3__item,u3_nul),list_elems_to_noun(nod));
}

u3_noun code_block_to_noun(cmark_node * nod)
{
  u3_atom str = u3i_string((c3_c *) nod->string_content.ptr);    /*  XX  u3i_bytes  */
  u3_noun res =
    u3nt(
      c3__code,
      nod->as.code.fenced
        ? u3nq(
            u3_nul,
            nod->as.code.fence_char,
            nod->as.code.fence_length,
            u3i_tape((c3_c *) nod->as.code.info.ptr)
          )
        : u3_nul,
      u3qe_lore(str));
  u3z(str);
  return res;
}

u3_noun html_to_noun(cmark_node * nod)
{
  u3_atom str = u3i_string((c3_c *) nod->string_content.ptr);    /*  XX  u3i_bytes  */
  u3_noun res = u3nc(c3__html, u3qe_lore(str));
  u3z(str);
  return res;
}

u3_noun paragraph_to_noun(cmark_node * nod)
{
  return u3nc(c3__para, list_elems_to_noun(nod));
}

u3_noun header_to_noun(cmark_node * nod)
{
  /* see also nod->as.header.setext */
  return u3nt(c3__head, nod->as.header.level, list_elems_to_noun(nod));
}

u3_noun hrule_to_noun(cmark_node * nod)
{
  return u3nc(c3__hrul, u3_nul);
}

u3_noun reference_def_to_noun(cmark_node * nod)
{
  return u3nc(c3__defn, u3_nul);
}

u3_noun text_to_noun(cmark_node * nod)
{
  return u3nc(u3_blip, u3i_tape((c3_c *) cmark_chunk_to_cstr(&nod->as.literal)));
}

u3_noun softbreak_to_noun(cmark_node * nod)  //  XXX
{
  return u3nt(0, 10, 0);
}

u3_noun linebreak_to_noun(cmark_node * nod)
{
  return u3nc(c3__line, u3_nul);
}

u3_noun inline_code_to_noun(cmark_node * nod)
{
  return u3nc(c3__code, u3i_tape((c3_c *) cmark_chunk_to_cstr(&nod->as.literal)));
}

u3_noun inline_html_to_noun(cmark_node * nod)  // XXX
{
  return u3nc(c3__htmt, u3i_string((c3_c *) cmark_chunk_to_cstr(&nod->as.literal)));
}

u3_noun emph_to_noun(cmark_node * nod)
{
  return u3nc(u3nc(c3__emph, c3n), list_elems_to_noun(nod));
}

u3_noun strong_to_noun(cmark_node * nod)
{
  return u3nc(u3nc(c3__emph, c3y), list_elems_to_noun(nod));
}

u3_noun link_to_noun(cmark_node * nod)
{
  return u3nc(u3nt(c3__link,
                   nod->as.link.url
                     ? u3i_tape((c3_c *) nod->as.link.url)
                     : u3_nul,
                   nod->as.link.title
                     ? u3nc(u3_nul, u3i_tape((c3_c *) nod->as.link.title))
                     : u3_nul),
              list_elems_to_noun(nod));
}

u3_noun image_to_noun(cmark_node * nod)
{
  return u3nc(u3nt(c3__blot,
                   u3i_tape((c3_c *) nod->as.link.url),
                   nod->as.link.title
                     ? u3nc(u3_nul, u3i_tape((c3_c *) nod->as.link.title))
                     : u3_nul),
              list_elems_to_noun(nod));
}

u3_noun node_to_noun(cmark_node * nod)
{
  if (!nod) {
    fprintf(stderr, "markdown null node");
    return u3m_bail(c3__fail);
  }
  switch ( nod->type ) {
    /* Block */
    case CMARK_NODE_DOCUMENT:       return document_to_noun(nod);
    case CMARK_NODE_BLOCK_QUOTE:    return block_quote_to_noun(nod);
    case CMARK_NODE_LIST:           return list_to_noun(nod);
    case CMARK_NODE_LIST_ITEM:      return list_item_to_noun(nod);
    case CMARK_NODE_CODE_BLOCK:     return code_block_to_noun(nod);
    case CMARK_NODE_HTML:           return html_to_noun(nod);
    case CMARK_NODE_PARAGRAPH:      return paragraph_to_noun(nod);
    case CMARK_NODE_HEADER:         return header_to_noun(nod);
    case CMARK_NODE_HRULE:          return hrule_to_noun(nod);
    case CMARK_NODE_REFERENCE_DEF:  return reference_def_to_noun(nod);
    /* Inline */
    case CMARK_NODE_TEXT:           return text_to_noun(nod);
    case CMARK_NODE_SOFTBREAK:      return softbreak_to_noun(nod);
    case CMARK_NODE_LINEBREAK:      return linebreak_to_noun(nod);
    case CMARK_NODE_INLINE_CODE:    return inline_code_to_noun(nod);
    case CMARK_NODE_INLINE_HTML:    return inline_html_to_noun(nod);
    case CMARK_NODE_EMPH:           return emph_to_noun(nod);
    case CMARK_NODE_STRONG:         return strong_to_noun(nod);
    case CMARK_NODE_LINK:           return link_to_noun(nod);
    case CMARK_NODE_IMAGE:          return image_to_noun(nod);
    default: fprintf(stderr, "bad markdown parsing");
             return u3m_bail(c3__fail);
  }
}

/* functions
*/
  u3_noun
  u3qg_down(u3_atom a)
  {
    c3_c *tex = u3r_string(a);

    /* XX better strlen */
    cmark_node * doc = cmark_parse_document(tex, strlen(tex));

    u3_noun res = document_to_noun(doc);

    cmark_node_free(doc);
    // free out, tex?
    return res;
  }
  u3_noun
  u3wg_down(u3_noun cor)
  {
    u3_noun a;

    if ( (u3_none == (a = u3r_at(u3x_sam, cor))) ||
         (c3n == u3ud(a)) )
    {
      return u3m_bail(c3__exit);
    } else {
      return u3qg_down(a);
    }
  }
