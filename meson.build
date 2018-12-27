project('libent', 'c',
        default_options : ['c_std=c99', 'cpp_std=c++11'],
        version : '0.0.1',
        license : 'MIT')

subdir('include/ent')

inc = include_directories('include')

ent_sources = ['ent.c']

libent = static_library('ent',
                        sources : ent_sources,
                        include_directories : inc,
                        c_args : ['-Wall', '-pedantic'],
                        install : false)

libent_dep = declare_dependency(include_directories : inc, link_with : libent)

pkg_mod = import('pkgconfig')
pkg_mod.generate(libraries : libent,
                 version : '0.0',
                 name : 'libent',
                 filebase : 'ent',
                 description : 'A library to get entropy.')

ent_sample = executable('sample',
                        ['sample.c'],
                        dependencies : [libent_dep])
