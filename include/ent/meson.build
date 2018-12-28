conf_data = configuration_data()
compiler = meson.get_compiler('c')

code = '''#include <stddef.h>
#include <inttypes.h> /* OSX sys/random.h needs Availability.h from this */
#include <sys/random.h>
int main() { return getentropy((void*)0, 0); }
'''
sysrandom = compiler.links(code, name : 'getentropy in sys/random.h')
conf_data.set('ENT_GE_SYSRANDOM', sysrandom)
have_getentropy = sysrandom

if not have_getentropy
  code = '''#include <unistd.h>
  int main() { return getentropy((void*)0, 0); }
  '''
  unistd = compiler.links(code, name : 'getentropy is in unistd.h')
  conf_data.set('ENT_GE_UNISTD', unistd)
  have_getentropy = unistd
endif

conf_data.set('ENT_GETENTROPY', have_getentropy)

if not have_getentropy
  code = '''#include <fcntl.h>
  int main() { return -1 != open("/dev/urandom", O_RDONLY) ? 0 : 1; }
  '''
  use_urandom = compiler.run(code, name : 'can open /dev/urandom')
  conf_data.set('ENT_URANDOM', use_urandom.returncode() == 0)
endif

configure_file(output : 'config.h',
               configuration : conf_data)
