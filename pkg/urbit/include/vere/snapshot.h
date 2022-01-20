//! @file snapshot.h


//==============================================================================
// Functions
//==============================================================================

//! TODO(peter)
c3_i
u3_el_fault(void* adr_v, c3_i ser_i);

//! TODO(peter)
void
u3_el_save(void);

//! TODO(peter): start the persistence system.  Return c3y if no image.
c3_o
u3_el_live(c3_o nuu_o, c3_c* dir_c);

//! TODO(peter): disable dirty page tracking, read/write whole loom.
c3_o
u3_el_yolo(void);

//! TODO(peter): dirty all the pages of the loom.
void
u3_el_foul(void);
