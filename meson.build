project('libent', 'c', 'cpp',
        default_options : ['c_std=c99', 'cpp_std=c++11'],
        version : '0.0.1',
        license : 'MIT')

subdir('include/ent')

inc = include_directories('include')

ent_sources = ['ent.c']

ent_dep = declare_dependency(sources : ent_sources,
                             compile_args : ['-Wall', '-pedantic'],
                             include_directories : inc)

libent = both_libraries('ent',
                        dependencies : [ent_dep],
                        install : false)

pkg_mod = import('pkgconfig')
pkg_mod.generate(libraries : libent,
                 version : '0.0',
                 name : 'libent',
                 filebase : 'ent',
                 description : 'A library to get entropy.')
