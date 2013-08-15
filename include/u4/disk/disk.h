/* include/disk/disk.h
**
** This file is in the public domain.
*/
  /** Functions.
  ***
  *** The disk functions are used to load the gear tree, 
  **/
    /* u4_disk_read_file():
    **
    **   Load (cl_file) as an atom on (lane).
    */
      u4_noun
      u4_disk_read_file(u4_lane     lane,
                        const u4_cl *cl_file);

    /* u4_disk_write_file():
    **
    **   Save (text) as (cl_file), overwriting present contents.
    */
      void
      u4_disk_write_file(const u4_cl *cl_file,
                         u4_atom     text);
                        
    /* u4_disk_read_dir():
    **
    **   Load (cl_dir) as a log *((name ext) text).
    */
      u4_noun
      u4_disk_read_dir(u4_lane     lane,
                       const u4_cl *cl_dir);
 
    /* u4_disk_read_tree():
    **
    **   Load (cl_directory) as a directory tab on (lane), 
    **   appending to (tab_tree).
    */
      u4_tab
      u4_disk_read_tree(u4_lane     lane,
                        u4_tab      tab_gear,
                        u4_tab      tab_tree, 
                        const u4_cl *cl_directory);

    /* u4_disk_read_gear():
    **
    **   Load (cl_directory) as the gear directory.
    */
      u4_tab
      u4_disk_read_gear(u4_lane     lane,
                        const u4_cl *cl_directory);

