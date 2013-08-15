/* disk/disk.c
**
** This file is in the public domain.
*/
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include "u4/all.h"

/* u4_disk_read_file():
**
**   Load (cl_file) as an atom on (lane).
*/
u4_noun
u4_disk_read_file(u4_lane     lane,
                  const u4_cl *cl_file)
{
  u4_int      fd         = open(cl_file, O_RDONLY, 0666);
  struct stat stat_path;
  u4_sb       sb;
  u4_xb       *xb;
  u4_atom     atom;

  if ( (fd < 0) || (fstat(fd, &stat_path) < 0) ) {
    perror(cl_file);
    return u4_trip;
  }

  sb = stat_path.st_size;
  xb = malloc(stat_path.st_size);

  if ( sb != read(fd, xb, sb) ) {
    perror(cl_file);
    return u4_trip;
  }
  close(fd);

  atom = u4_k_atom_sb(lane, xb, sb); 
  free(xb);

  return atom;
}

/* u4_disk_write_file():
**
**   Save (text) as (cl_file), overwriting present contents.
*/
void
u4_disk_write_file(const u4_cl *cl_file,
                   u4_atom     text)
{
  u4_int  fd = open(cl_file, O_WRONLY | O_CREAT, 0666);
  u4_sb   sb;
  u4_xb   *xb;

  sb = u4_a_bin(text, 3);
  xb = malloc(sb);
  u4_a_bytes(text, xb, 0, sb);

  if ( sb != write(fd, xb, sb) ) {
    perror(cl_file);
    u4_trip;
  }
  close(fd);
  free(xb);
}
                        
/* Load (cl_file), as a pair hog.(tag dog), on u4_hat(road).
*/
static u4_hog
_load_item_(u4_road     road,
            u4_tab      tab_gear,
            const u4_cl *cl_file)
{
  enum u4_bail_code bail_code;

  if ( (bail_code = u4_bail_in) ) {
    switch ( bail_code ) {
      default: printf("[%s: weird!]\n", cl_file); break;

      case u4_bail_exit: printf("[%s: exit]\n", cl_file); break;
      case u4_bail_tank: printf("[%s: tank]\n", cl_file); break;
      case u4_bail_trip: printf("[%s: trip]\n", cl_file); break;
      case u4_bail_stub: printf("[%s: stub]\n", cl_file); break;
    }
    return u4_bull;
  } 
  else {
    const u4_cl *cl_slash = strrchr(cl_file, '/');
    const u4_cl *cl_dot   = strrchr(cl_file, '.');
    const u4_cl *cl_name  = cl_slash ? (cl_slash + 1) : cl_file;
    u4_sb       sb_name   = cl_dot ? (cl_dot - cl_name) : strlen(cl_name);
    {
      u4_tag tag = u4_k_atom_sb(u4_hat(road), (u4_xb *)cl_name, sb_name);
      u4_dog dog;

      if ( !cl_dot ) {
        /* Extensionless files must be directories.
        */
        dog = u4_disk_read_tree(u4_hat(road), tab_gear, u4_noun_0, cl_file);
      }
      else {
        // u4_atom file = u4_disk_read_file(u4_cap(road), cl_file);

        if ( !strcmp(cl_dot, ".goof") ) {
          dog = u4_stub;
        }
        else if ( !strcmp(cl_dot, ".dogo") ) {
          dog = u4_stub;
        }
        else {
          return u4_bull;
        }
      }
      return u4_k_cell(u4_hat(road), tag, dog);
    }
  }
}

/* _disk_read_tree_road(): as u4_disk_read_tree(), on u4_hat(road).
*/
static u4_tab
_disk_read_tree_road(u4_road     road,
                     u4_tab      tab_gear,
                     u4_tab      tab_tree, 
                     const u4_cl *cl_directory)
{
  u4_bar bar_cap = u4_road_bar_cap(road);
  u4_tab tab_pro;
  {
    u4_sb  sb_dir  = strlen(cl_directory);
    u4_log log_hog = u4_noun_0;

    /* Fill (log_hog) with (tag dog) pairs.
    */
    {
      DIR *dir = opendir(cl_directory);

      if ( !dir ) {
        perror(cl_directory);
        return u4_noun_0;
      }
      else while ( 1 ) {
        struct dirent dirent_item, *dirent_result;

        if ( readdir_r(dir, &dirent_item, &dirent_result) ) {
          perror(cl_directory);
          exit(1);
        }
        else if ( !dirent_result ) {
          break;
        }
        else if ( dirent_result->d_name[0] != '.' ) {
          u4_sb  sb_item  = (sb_dir + 1 + u4_c_dirent_namlen(dirent_result));
          u4_cl  *cl_item = alloca(sb_item + 1);
          u4_hog hog;

          strcpy(cl_item, cl_directory);
          cl_item[sb_dir] = '/';
          strcpy(cl_item + sb_dir + 1, dirent_result->d_name);

          if ( u4_bull != (hog = _load_item_(road, tab_gear, cl_item)) ) {
            log_hog = u4_k_cell(u4_cap(road), hog, log_hog);
          }
        }
      }
      closedir(dir);
    }

    tab_pro = u4_tab_add_log(u4_hat(road), log_hog, tab_tree);
  }
  u4_road_bar_cap(road) = bar_cap;
  return tab_pro;
}

/* u4_disk_read_tree():
**
**   Load (cl_directory) as a directory tab on (lane), 
**   appending to (tab_tree).
*/
u4_tab
u4_disk_read_tree(u4_lane     lane,
                  u4_tab      tab_gear,
                  u4_tab      tab_tree, 
                  const u4_cl *cl_directory)
{
  u4_road road = u4_lane_road(lane);

  if ( u4_lane_side(lane) == u4_side_hat ) {
    return _disk_read_tree_road(road, tab_gear, tab_tree, cl_directory); 
  }
  else {
    u4_noun pro;
    u4_road road_nest;

    road_nest = u4_r_nest_in(road);
    pro = _disk_read_tree_road(road_nest, tab_gear, tab_tree, cl_directory); 
    u4_r_nest_out(road, road_nest);

    return pro;
  }
}

/* u4_disk_read_gear():
**
**   Load (cl_directory) as the gear directory.
*/
u4_tab
u4_disk_read_gear(u4_lane     lane,
                  const u4_cl *cl_directory)
{
  u4_tab tab_gear;
  {
    u4_cl *cl_gear_a = alloca(strlen(cl_directory) + 3);
    u4_cl *cl_gear_b = alloca(strlen(cl_directory) + 3);

    strcpy(cl_gear_a, cl_directory); strcat(cl_gear_a, "/a");
    strcpy(cl_gear_b, cl_directory); strcat(cl_gear_b, "/b");

    tab_gear = u4_disk_read_tree(lane, u4_noun_0, u4_noun_0, cl_gear_a);
    tab_gear = u4_disk_read_tree(lane, tab_gear, tab_gear, cl_gear_b);
  }
  return tab_gear;
}

/* _item_prefix(): filename to prefix.
*/
static u4_noun
_item_prefix(u4_lane lane,
             u4_cl   *cl_name)
{
  u4_cl   *cl_dot;
  u4_noun prefix;

  if ( (cl_dot = strchr(cl_name, '.')) ) {
    *cl_dot = 0;
    prefix = u4_k_atom_c(lane, cl_name);

    *cl_dot = '.';
    return prefix;
  }
  else return u4_k_atom_c(lane, cl_name);
}

/* _item_suffix(): filename to suffix.
*/
static u4_noun
_item_suffix(u4_lane lane,
             u4_cl   *cl_name)
{
  u4_cl   *cl_dot;

  if ( (cl_dot = strchr(cl_name, '.')) ) {
    return u4_k_atom_c(lane, (cl_dot + 1));
  }
  else return u4_noun_0;
}

/* u4_disk_read_dir():
**
**   Load (cl_dir) as a log *((name type) text).
*/
u4_noun
u4_disk_read_dir(u4_lane     lane,
                 const u4_cl *cl_dir)
{
  DIR     *dir   = opendir(cl_dir);
  u4_sb   sb_dir = strlen(cl_dir);
  u4_noun log    = u4_noun_0;

  if ( !dir ) {
    // perror(cl_dir);
    return u4_noun_0;
  }
  else while ( 1 ) {
    struct dirent dirent_item, *dirent_result;

    if ( readdir_r(dir, &dirent_item, &dirent_result) ) {
      perror(cl_dir);
      exit(1);
    }
    else if ( !dirent_result ) {
      break;
    }
    else if ( DT_DIR & dirent_result->d_type ) {
      continue;
    }
    else if ( dirent_result->d_name[0] != '.' ) {
      u4_sb  sb_item  = (sb_dir + 1 + u4_c_dirent_namlen(dirent_result));
      u4_cl  *cl_item = alloca(sb_item + 1);
      u4_noun name, ext;

      strcpy(cl_item, cl_dir);
      cl_item[sb_dir] = '/';
      strcpy(cl_item + sb_dir + 1, dirent_result->d_name);

      name = _item_prefix(lane, dirent_result->d_name);
      ext = _item_suffix(lane, dirent_result->d_name);

      log = u4_k_cell(lane, u4_k_cell(lane, name, ext), log);
    }
  }
  closedir(dir);

  return log;
}
