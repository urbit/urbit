/// @file events.h

#ifndef U3_EVENTS_H
#define U3_EVENTS_H

  /** Data structures.
  **/
      /// An entry for dirty page in patch control file.
      typedef struct _u3e_line {
        /// Page number.
        c3_w pag_w;

        /// Mug of page.
        c3_w mug_w;
      } u3e_line;

      /// A patch control file.
      typedef struct _u3e_control {
        /// Version number.
        c3_w     ver_y;

        /// New page count for north image.
        c3_w     nor_w;

        /// New page count for south image.
        c3_w     sou_w;

        /// Number of changed pages.
        c3_w     pgs_w;

        /// List of dirty pages.
        u3e_line mem_u[0];
      } u3e_control;

      /// A patch, which is an incremental change to a snapshot.
      typedef struct _u3_cs_patch {
        // Patch control file descriptor.
        c3_i         ctl_i;

        /// Patch memory file descriptor.
        c3_i         mem_i;

        /// Patch control file contents.
        u3e_control* con_u;
      } u3_ce_patch;

      /// An image of a snapshot.
      typedef struct _u3e_image {
        /// Image file name.
        const c3_c* nam_c;

        /// Image file descriptor or 0 if image file is not open.
        c3_i        fid_i;

        /// Length of image in pages.
        c3_w        pgs_w;
      } u3e_image;

      /// An incremental snapshot system.
      typedef struct _u3e_pool {
        /// Path to.
        const c3_c* dir_c;

        /// Tracking structure for pages touched since last save.
        c3_w        dit_w[u3a_pages >> 5];

        /// Number of pages (<= u3a_pages).
        c3_w pag_w;

        /// North segment (heap).
        u3e_image   nor_u;

        /// South segment (stack).
        u3e_image   sou_u;
      } u3e_pool;

  /** Globals.
  **/
      /// Global memory control.
      c3_global u3e_pool u3e_Pool;
#     define u3P u3e_Pool

  /** Constants.
  **/
#     define u3e_version 1

  /** Functions.
  **/

      /// SIGSEGV handler conforming to the libsigsegv protocol.
      ///
      /// @param[in] adr_v  Fault address with the bits
      ///                   (SIGSEGV_FAULT_ADDRESS_ALIGNMENT - 1) cleared.
      /// @param[in] ser_i  Zero if the fault might be a stack overflow, or 1 if the
      ///                   handler should seriously try to fix the fault.
      ///
      /// @return 0   Decline responsibility for the fault.
      /// @return !0  Fault was resolved and no other handler should be called.
      c3_i
      u3e_fault(void* adr_v, c3_i ser_i);

      /// Take an incremental snapshot.
      void
      u3e_save(void);

      /// Copy the incremental snapshot to another directory.
      ///
      /// @param[in] dir_c  Target directory. Will be created if it doesn't already
      ///                   exist.
      ///
      /// @return c3n  `dir_c` could not be created.
      /// @return c3n  Could not create new image files.
      /// @return c3n  Could not copy contents of original image files to new
      ///              image files.
      /// @return c3y  Image files copied to `dir_c`.
      c3_o
      u3e_copy(const c3_c* const dir_c);

      /// Load a snapshot copy from a directory into memory.
      ///
      /// @param[in] dir_c  Must contain a valid snapshot.
      void
      u3e_load(const c3_c* dir_c);

      /// Start the incremental snapshot system.
      ///
      /// @param[in] dir_c  Directory to run the incremental snapshot system in. Will
      ///                   be created if it doesn't exist.
      ///
      /// @return c3y  No existing snapshot.
      /// @return c3n  Existing snapshot loaded.
      c3_o
      u3e_live(const c3_c* dir_c);

      /// Disable dirty page tracking allowing read/write of entire loom.
      ///
      /// @return c3n  Page tracking could not be disabled.
      /// @return c3y  Otherwise.
      c3_o
      u3e_yolo(void);

      /// Mark all pages of the loom as dirty.
      void
      u3e_foul(void);

      /// Initialize guard page.
      void
      u3e_init(void);

#endif /* ifndef U3_EVENTS_H */
