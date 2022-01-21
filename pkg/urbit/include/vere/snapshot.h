//! @file snapshot.h


//==============================================================================
// Functions
//==============================================================================

//! TODO(peter): comment once interface freezes.
c3_i
u3_snap_fault(void* adr_v, c3_i ser_i);

//! TODO(peter): comment once interface freezes.
void
u3_snap_save(void);

//! Start the snapshot system.  Return c3y if no image.
//! TODO(peter): comment once interface freezes.
c3_o
u3_snap_live(c3_o nuu_o, c3_c* dir_c);

//! Disable dirty page tracking, read/write whole loom.
//! TODO(peter): comment once interface freezes.
c3_o
u3_snap_yolo(void);

//! Dirty all the pages of the loom.
//! TODO(peter): comment once interface freezes.
void
u3_snap_foul(void);
