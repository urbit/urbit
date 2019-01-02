project('libent', 'c', version: '0.1.2', license: 'MIT')

inc = include_directories('include')

subdir('include')
subdir('src')

ent_sample = executable('sample',
                        ['sample.c'],
                        dependencies: [libent_dep])
