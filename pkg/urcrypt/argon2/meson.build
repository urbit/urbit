project('argon2', 'c', version : '1')

legacy_meson = false

detect_meson_version = run_command('meson', '--version')
meson_ver = detect_meson_version.stdout()

if(meson_ver == '0.29.0\n')
  legacy_meson = true
elif(not meson.version().version_compare('>=0.40.0'))
  error('Meson 0.29.0 is last legacy version supported. Otherwise please upgrade to 0.40.0 or higher.')
endif

lib_src = ['src/argon2.c',
'src/bench.c',
'src/blake2/blake2b.c',
'src/core.c',
'src/encoding.c',
'src/genkat.c',
'src/opt.c',
'src/ref.c',
'src/run.c',
'src/test.c',
'src/thread.c']

inc = include_directories(['./include'])

lib = static_library('argon2', sources: lib_src,
include_directories: inc,
c_args: ['-Wall', '-Wno-unused-value', '-Wno-unused-function', '-DARGON2_NO_THREADS'],
install: false)

argon2_dep = declare_dependency(include_directories : inc, link_with : lib)
