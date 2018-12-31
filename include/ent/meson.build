conf_data = configuration_data()
compiler = meson.get_compiler('c')
older_oses = get_option('support-older-oses')

if not older_oses
  code = '''#include <stddef.h>
  #include <inttypes.h> /* OSX sys/random.h needs Availability.h from this */
  #include <sys/random.h>
  int main() { return getentropy((void*)0, 0); }
  '''
  sysrandom = compiler.links(code, name : 'getentropy in sys/random.h')
  conf_data.set('ENT_GE_SYSRANDOM', sysrandom)

  if not sysrandom
    code = '''#include <unistd.h>
    int main() { return getentropy((void*)0, 0); }
    '''
    unistd = compiler.links(code, name : 'getentropy is in unistd.h')
    conf_data.set('ENT_GE_UNISTD', unistd)
  endif
  have_getentropy = sysrandom or unistd
  conf_data.set('ENT_GETENTROPY', have_getentropy)
else
  have_getentropy = false
endif

if not have_getentropy
  code = '''#define _GNU_SOURCE
  #include <asm/unistd.h>
  #include <unistd.h>
  #include <sys/syscall.h>
  int main() { return !syscall(__NR_getrandom, (void*)0, 0, 0); }
  '''
  getrandom = compiler.links(code, name : 'getrandom syscall available')
  conf_data.set('ENT_GETRANDOM', getrandom)

  if not getrandom
    code = '''#include <fcntl.h>
    int main() { return open("/dev/urandom", O_RDONLY) >= 0 ? 0 : 1; }
    '''
    urandom = compiler.run(code, name : 'can open /dev/urandom').returncode() == 0
    conf_data.set('ENT_URANDOM', urandom)
  endif
endif


configure_file(output : 'config.h',
               configuration : conf_data)
