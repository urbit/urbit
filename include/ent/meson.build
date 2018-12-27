conf_data = configuration_data()
compiler = meson.get_compiler('c')

code = '''#include <unistd.h>
#include <sys/random.h>
int main() { return getentropy((void*)0, 0); }
'''
have_getentropy = compiler.links(code, name : 'system getentropy')
conf_data.set('ENT_USE_GETENTROPY', have_getentropy)

if not have_getentropy
  code = '''#include <fcntl.h>
  int main() { return -1 != open("/dev/urandom", O_RDONLY) ? 0 : 1; }
  '''
  use_urandom = compiler.run(code, name : 'open /dev/urandom')
  conf_data.set('ENT_USE_URANDOM', use_urandom.returncode() == 0)
endif

configure_file(output : 'config.h',
               configuration : conf_data,
               install_dir : 'include/ent')

install_headers('ent.h', subdir : 'ent')
