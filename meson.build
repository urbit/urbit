project('libent', 'c', 'cpp',
        default_options : ['c_std=c99', 'cpp_std=c++11'],
        version : '0.0.1',
        license : 'MIT')

subdir('include/ent')

inc = include_directories('include')

ent_sources = ['ent.c']

libent = both_libraries('ent',
                        ent_sources,
                        include_directories : inc,
                        install : true)

pkg_mod = import('pkgconfig')
pkg_mod.generate(libraries : libent,
                 version : '0.0',
                 name : 'libent',
                 filebase : 'ent',
                 description : 'A library to get entropy.')
