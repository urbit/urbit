//! @file events.h

#ifndef U3_EVENTS_H
#define U3_EVENTS_H

//==============================================================================
// Types
//==============================================================================

//! 
typedef struct {
  c3_w pag_w; //!< page number
  c3_w mug_w; //!< mug of page
} u3e_line;

//! Patch control file.
typedef struct {
  c3_w     ver_y;    //!< version number
  c3_w     nor_w;    //!< new page count north
  c3_w     sou_w;    //!< new page count south
  c3_w     pgs_w;    //!< number of changed pages
  u3e_line mem_u[0]; //!< one entry per changed page
} u3e_control;

//! A patch, which is an incremental memory change to a snapshot.
typedef struct {
  c3_i         ctl_i; //!< patch control file descriptor
  c3_i         mem_i; //!< patch memory file descriptor
  u3e_control* con_u; //!< patch control file contents
} u3_ce_patch;

//! An image of a snapshot.
typedef struct {
  const c3_c* nam_c; //!< segment name
  c3_i        fid_i; //!< open file, or 0
  c3_w        pgs_w; //!< length in pages
} u3e_image;

//! Incremental snapshot system.
typedef struct {
  c3_c*     dir_c;                 //!< path to
  c3_w      dit_w[u3a_pages >> 5]; //!< pages touched since last u3e_save()
  u3e_image nor_u;                 //!< north segment
  u3e_image sou_u;                 //!< south segment
} u3e_pool;

//==============================================================================
// Global variables
//==============================================================================

//! Global memory control.
c3_global u3e_pool u3e_Pool;
#define u3P u3e_Pool

//==============================================================================
// Constants
//==============================================================================

#define u3e_version 1

//==============================================================================
// Functions
//==============================================================================

//! SIGSEGV handler conforming to the libsigsegv protocol.
//!
//! @param[in] adr_v  Fault address with the bits
//!                   (SIGSEGV_FAULT_ADDRESS_ALIGNMENT - 1) cleared.
//! @param[in] ser_i  Zero if the fault might be a stack overflow, or 1 if the
//!                   handler should seriously try to fix the fault.
//!
//! @return 0   Decline responsibility for the fault.
//! @return !0  Fault was resolved and no other handler should be called.
c3_i
u3e_fault(void* adr_v, c3_i ser_i);

//! Take an incremental snapshot.
void
u3e_save(void);

//! Copy the incremental snapshot to `dir_c`.
//!
//! @param[in] dir_c  Will be created if it doesn't already exist.
//!
//! @return c3n  `dir_c` could not be created.
//! @return c3n  Could not create new image files.
//! @return c3n  Could not copy contents of original image files to new
//!              image files.
//! @return c3y  Image files copied to `dir_c`.
c3_o
u3e_copy(const c3_c* const dir_c);

//! Load a snapshot copy from `dir_c` into memory.
//!
//! @param[in] dir_c  Must contain a valid snapshot.
//!
//! @return c3n  Existing snapshot loaded.
c3_o
u3e_load(c3_c* dir_c);

//! Start the incremental snapshot system.
//!
//! @param[in]
//! 
//! @return c3y  No existing snapshot.
//! @return c3n  Existing snapshot loaded.
c3_o
u3e_live(c3_c* dir_c);

//! Disable dirty page tracking allowing read/write of entire loom.
c3_o
u3e_yolo(void);

//! Mark all pages of the loom as dirty.
void
u3e_foul(void);

#endif /* ifndef U3_EVENTS_H */
