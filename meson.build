project('libent', 'c', 'cpp',
        default_options : ['c_std=c99', 'cpp_std=c++11'],
        version : '0.0.1',
        license : 'MIT')

conf_data = configuration_data()
compiler = meson.get_compiler('c')

code = '''#include <unistd.h>
#include <sys/random.h>
int main() { return getentropy((void*)0, 0); }
'''
have_getentropy = compiler.links(code,name : 'system getentropy')
conf_data.set('USE_GETENTROPY', have_getentropy)

if not have_getentropy
  code = '''#include <fcntl.h>
  int main() { return -1 != open("/dev/urandom", O_RDONLY) ? 0 : 1; }
  '''
  use_urandom = compiler.run(code, name : 'open /dev/urandom')
  conf_data.set('USE_URANDOM', use_urandom.returncode() == 0)
endif

configure_file(output : 'config.h',
               configuration : conf_data)

inc = include_directories('.', 'include')

install_headers('include/ent.h')

ent_sources = ['src/ent.c']

libent = shared_library('ent',
                        ent_sources,
                        include_directories : inc,
                        install : true)

pkg_mod = import('pkgconfig')
pkg_mod.generate(libraries : libent,
                 version : '0.0',
                 name : 'libent',
                 filebase : 'ent',
                 description : 'A library to get entropy.')
