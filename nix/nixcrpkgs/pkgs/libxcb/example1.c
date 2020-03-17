// Source: https://en.wikipedia.org/wiki/XCB#Example

#include <xcb/xcb.h>
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
  xcb_connection_t    *c;
  xcb_screen_t        *s;
  xcb_window_t         w;
  xcb_gcontext_t       g;
  xcb_generic_event_t *e;
  uint32_t             mask;
  uint32_t             values[2];
  int                  done = 0;
  xcb_rectangle_t      r = { 20, 20, 60, 60 };

                        /* open connection with the server */
  c = xcb_connect(NULL,NULL);
  if (xcb_connection_has_error(c)) {
    printf("Cannot open display\n");
    exit(1);
  }
                        /* get the first screen */
  s = xcb_setup_roots_iterator( xcb_get_setup(c) ).data;

                       /* create black graphics context */
  g = xcb_generate_id(c);
  w = s->root;
  mask = XCB_GC_FOREGROUND | XCB_GC_GRAPHICS_EXPOSURES;
  values[0] = s->black_pixel;
  values[1] = 0;
  xcb_create_gc(c, g, w, mask, values);

                       /* create window */
  w = xcb_generate_id(c);
  mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
  values[0] = s->white_pixel;
  values[1] = XCB_EVENT_MASK_EXPOSURE | XCB_EVENT_MASK_KEY_PRESS;
  xcb_create_window(c, s->root_depth, w, s->root,
                    10, 10, 100, 100, 1,
                    XCB_WINDOW_CLASS_INPUT_OUTPUT, s->root_visual,
                    mask, values);

                        /* map (show) the window */
  xcb_map_window(c, w);

  xcb_flush(c);

                        /* event loop */
  while (!done && (e = xcb_wait_for_event(c))) {
    switch (e->response_type & ~0x80) {
    case XCB_EXPOSE:    /* draw or redraw the window */
      xcb_poly_fill_rectangle(c, w, g,  1, &r);
      xcb_flush(c);
      break;
    case XCB_KEY_PRESS:  /* exit on key press */
      done = 1;
      break;
    }
    free(e);
  }
                        /* close connection to server */
  xcb_disconnect(c);

  return 0;
}
