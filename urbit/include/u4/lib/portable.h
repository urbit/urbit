/* portable.h: porting layer.
**
** This file is in the public domain.
*/
  /** System include files.
  ***
  *** Do not put include files that are only used in the
  *** porting layer here.  Include them directly in the
  *** C file.
  **/
#   if defined(U4_OS_linux)
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <byteswap.h> 
#     include <setjmp.h>
#     include <stdio.h> // delete me

#   elif defined(U4_OS_osx)
#     include <stdlib.h>
#     include <string.h>
#     include <stdarg.h>
#     include <unistd.h>
#     include <stdint.h>
#     include <assert.h>
#     include <setjmp.h>
#     include <machine/endian.h> 
#     include <machine/byte_order.h> 
#     include <stdio.h> // delete me

#   endif

  /** Private C "extensions."
  ***
  *** Except for these and main(), any function, macro, or structure
  *** names must be prefixed either by u4_/U4_ (for public names),
  *** or _ (for static and other file-local names).
  **/
    /* Endianness.
    */
#     define u4_endian_little     0
#     define u4_endian_big        1

#     ifdef U4_ENDIAN_little
#       define u4_portable_endian u4_endian_little
#     elif defined(U4_ENDIAN_big)
#       define u4_portable_endian u4_endian_big
#     else
#       error "port: U4_ENDIAN"
#     endif

    /* Byte swapping.
    */
#      if defined(U4_OS_linux)
#        define u4_bswap_16(x)  bswap_16(x)
#        define u4_bswap_32(x)  bswap_32(x)
#        define u4_bswap_64(x)  bswap_64(x)

#      elif defined(U4_OS_osx)
#        define u4_bswap_16(x)  NXSwapShort(x)
#        define u4_bswap_32(x)  NXSwapInt(x)
#        define u4_bswap_64(x)  NXSwapLongLong(x)
#      endif

    /* Readdir properties.
    */


#      if defined(U4_OS_linux)
#        define u4_c_dirent_namlen(dp) (strlen((dp)->d_name))

#      elif defined(U4_OS_osx)
#        define u4_c_dirent_namlen(dp) ((dp)->d_namlen)
#      endif

#if 0
  /** Data structures.
  **/
    /* Static porting state. 
    **
    ** Members of this structure must mean the same thing on any
    ** platform u4 runs on, and must not change during the lifetime
    ** of the server process.
    */
      struct _u4_portable_static {
        /* Configuration options. See below.
        */
        struct u4_portable_option *options;

        /* Root of the boot folder tree. See below.
        **
        ** $root_folder->name is always 0.
        */
        struct u4_portable_folder *root_folder;

        /* Some MAC (Ethernet) address physically associated with
        ** this system, and chosen by a deterministic algorithm.
        */
        uint64_t mac;

        /* Log, in bytes, of the system page size (eg, getpagesize()
        ** in Linux). For example, log 10 means 1K pages.
        */
        uint8_t page_log;

        /* Base address of the disk image.
        */
        uint8_t *disk_base;

        /* Number of pages in the disk image.
        **
        ** $disk_npages reflects the maximum virtual memory window
        ** that the OS can map, not the actual number of pages
        ** that contain actual data.
        */
        uint32_t disk_npages;

        /* Offset, in bytes, of portable storage in the disk image.
        **
        ** Everything before $disk_start is control information
        ** used by the portability layer.
        */
        uint32_t disk_start;

        /* Base address of the RAM image.
        */
        uint8_t *ram_base;

        /* Number of pages in the RAM image.
        */
        uint32_t ram_npages;
      };
      U4_GLOBAL struct _u4_portable_static u4_portable_static;


    /* Portable options. A simple name-value pair.
    */
      struct u4_portable_option {
        const uint8_t *name;
        const uint8_t *value;

        struct u4_portable_option *next;
      };

    /* A portable source file.
    */
      struct u4_portable_source {
        /* File extension.
        */
        uint8_t *extension;

        /* File data.
        */
        uint8_t *text;

        /* File size.
        */
        uint32_t nbytes;
      };
        

  /** Functions.
  **/
    /* Initialize the porting layer. Complete any interrupted
    ** checkpoint operations, etc.
    **
    ** Exits if there is a problem.
    */
      void
      u4_portable_init(int argc, char *const argv[]);

    /* Checkpoint disk to hard storage and sync.
    **
    ** Exits if there is a problem.
    */
      void
      u4_portable_save(void);

    /* Stop the server, discarding any unsaved state.
    */
      void
      u4_portable_exit(void);

    /* Wait for a console input.
    **
    ** Returns 0 iff the console has exited.  Otherwise, caller must 
    ** free the input string.  
    */
      char *
      u4_portable_wait_console(void);

    /* Dump a text block to the console.
    */
      void
      u4_portable_dump_console(const uint8_t *text);

    /* Get an option named $name.  Return null if it is not set.
    **
    ** The result is static and must not be freed.
    */
      const uint8_t *
      u4_portable_get_option(const uint8_t *name);

    /* Release a source handle.
    */
      void
      u4_portable_source_release(struct u4_portable_source *source);

    /* Load the source for the boot code.
    **
    ** Caller must release the result with u4_portable_source_release().
    */
      struct u4_portable_source *
      u4_portable_source_load_boot(void);

    /* Load the source for detail $n.
    **
    ** Caller must release the result with u4_portable_source_release().
    **
    ** If the detail is not found, returns 0.
    */
      struct u4_portable_source *
      u4_portable_source_load_detail(uint32_t n);

    /* Load the source for foo noun $name.
    */
      struct u4_portable_source *
      u4_portable_source_load_foo(const uint8_t *name);
#endif
