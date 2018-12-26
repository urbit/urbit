project('libent', 'c', 'cpp',
        default_options : ['c_std=c99', 'cpp_std=c++11'],
        version : '0.0.1',
        license : 'MIT')

inc = include_directories('include')

install_headers('include/ent/ent.h', subdir : 'ent')

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
