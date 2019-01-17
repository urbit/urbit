conf_data = configuration_data()
cc = meson.get_compiler('c')
# OpenBSD has had getentropy since 5.6. It's fine.
compat = get_option('ent_compat') and host_machine.system() != 'openbsd'

if not compat
  code = '''#include <stddef.h>
  #include <unistd.h>
  #include <sys/random.h>
  int main() { return getentropy((void*)0, 0); }
  '''
  sysrandom = cc.links(code, name: 'getentropy in sys/random.h')
  conf_data.set('ENT_GE_SYSRANDOM', sysrandom)

  if not sysrandom
    code = '''#include <unistd.h>
    int main() { return getentropy((void*)0, 0); }
    '''
    unistd = cc.links(code, name: 'getentropy in unistd.h')
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
  int main() { return !syscall(SYS_getrandom, (void*)0, 0, 0); }
  '''
  getrandom = cc.links(code, name: 'getrandom syscall available')
  conf_data.set('ENT_GETRANDOM', getrandom)

  if not getrandom
    code = '''#include <stdio.h>
    int main() { return fopen("/dev/urandom", "r") ? 0 : 1; }
    '''
    urandom = cc.run(code, name: 'can open /dev/urandom').returncode() == 0
    conf_data.set('ENT_URANDOM', urandom)
  endif
endif

config_file = configure_file(
  output: 'config.h',
  input: 'config.h.in',
  configuration: conf_data,
)

ent_sources = ['ent.c', config_file]

libent = static_library(
  'ent',
  ent_sources,
  include_directories: inc,
  c_args: ['-Wall', '-pedantic'],
  install: get_option('ent_install'),
)
