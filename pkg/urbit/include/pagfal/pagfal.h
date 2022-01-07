/* include/pagfal/pagfal.h
**
** This file is in the public domain.
*/
#ifndef URBIT_PAGFAL_PAGFAL_H
#define URBIT_PAGFAL_PAGFAL_H

//#define USE_LIBSIGSEGV

/* u3f_read: allow read access to a page range.
*/
#define u3f_read  (1 << 0)

/* u3f_write: allow write access to a page range.
*/
#define u3f_write (1 << 1)


/* u3f_hand: the signature of a page fault handler.
** @param adr_v: the address that generated the page fault.
** @param unu_i: unused parameter used to maintain compatibility with libsigsegv.
**
** @return 0   the page fault was successfully handled.
** @return >0  the page fault could not be handled.
*/
typedef c3_i (*u3f_hand)(void* adr_v, c3_i unu_i);


/* u3f_install_handler(): install a page fault handler.
**
** @param han_f: the page fault handler to install. Pass NULL to effectively
**               uninstall the currently installed page handler.
**
** @return c3y  if the page fault handler was installed.
** @return c3n  otherwise.
*/
c3_o
u3f_install_handler(u3f_hand han_f);


/* u3f_protect(): change access protections of a page range.
**
** @param adr_v: start address of the page range.
** @param len_w: number of bytes in the page range.
** @param pro_w: bitmask of the protections to apply to the page range. Should
**               be one of (1) u3f_none, (2) u3f_read, (3) u3f_write, or
**               (4) u3f_read | u3f_write.
**
** @return c3y  if the access protections were successfully changed.
** @return c3n  otherwise.
*/
c3_o
u3f_protect(void* adr_v, c3_w len_w, c3_w pro_w);

/* u3f_fetch(): fetch a page range missing from memory.
**
** @param adr_v: start address of the page range.
** @param len_w: number of bytes in the page range.
**
** @return c3y  if the page range was successfully fetched.
** @return c3n  otherwise.
*/
c3_o
u3f_fetch(void* adr_v, c3_w len_w);

#endif /* URBIT_PAGFAL_PAGFAL_H */
