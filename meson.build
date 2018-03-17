project('urbit', 'c', meson_version: '>=0.29.0')

legacy_meson = false

detect_meson_version = run_command('meson', '--version')
meson_ver = detect_meson_version.stdout()

if(meson_ver == '0.29.0\n')
  legacy_meson = true
elif(not meson.version().version_compare('>=0.40.0'))
  error('Meson 0.29.0 is last legacy version supported. Otherwise please upgrade to 0.40.0 or higher.')
endif

jets_a_src =  [
'jets/a/add.c',
'jets/a/dec.c',
'jets/a/div.c',
'jets/a/gte.c',
'jets/a/gth.c',
'jets/a/lte.c',
'jets/a/lth.c',
'jets/a/mod.c',
'jets/a/mul.c',
'jets/a/sub.c',]

jets_b_src = [
'jets/b/bind.c',
'jets/b/clap.c',
'jets/b/drop.c',
'jets/b/flop.c',
'jets/b/lent.c',
'jets/b/levy.c',
'jets/b/lien.c',
'jets/b/murn.c',
'jets/b/need.c',
'jets/b/reap.c',
'jets/b/reel.c',
'jets/b/roll.c',
'jets/b/skid.c',
'jets/b/skim.c',
'jets/b/skip.c',
'jets/b/scag.c',
'jets/b/slag.c',
'jets/b/snag.c',
'jets/b/sort.c',
'jets/b/turn.c',
 'jets/b/weld.c' ]

jets_c_src = [
'jets/c/bex.c',
'jets/c/xeb.c',
'jets/c/can.c',
'jets/c/cap.c',
'jets/c/cat.c',
'jets/c/con.c',
'jets/c/cut.c',
'jets/c/dor.c',
'jets/c/dvr.c',
'jets/c/dis.c',
'jets/c/end.c',
'jets/c/gor.c',
'jets/c/hor.c',
'jets/c/lsh.c',
'jets/c/mas.c',
'jets/c/met.c',
'jets/c/mix.c',
'jets/c/mug.c',
'jets/c/muk.c',
'jets/c/peg.c',
'jets/c/po.c',
'jets/c/pow.c',
'jets/c/rap.c',
'jets/c/rep.c',
'jets/c/rip.c',
'jets/c/rsh.c',
'jets/c/sqt.c',
'jets/c/vor.c',
]

jets_d_src = [
'jets/d/in_has.c',
'jets/d/in_int.c',
'jets/d/in_gas.c',
'jets/d/in_mer.c',
'jets/d/in_put.c',
'jets/d/in_tap.c',
'jets/d/in_uni.c',
'jets/d/in_wyt.c',
'jets/d/in_bif.c',
'jets/d/in_dif.c',
'jets/d/by_gas.c',
'jets/d/by_get.c',
'jets/d/by_has.c',
'jets/d/by_int.c',
'jets/d/by_put.c',
'jets/d/by_uni.c',
'jets/d/by_bif.c',
'jets/d/by_dif.c'
]

jets_e_src = [
'jets/e/aes_ecb.c',
'jets/e/aes_cbc.c',
'jets/e/aesc.c',
'jets/e/cue.c',
'jets/e/fl.c',
'jets/e/jam.c',
'jets/e/mat.c',
'jets/e/mink.c',
'jets/e/mule.c',
'jets/e/parse.c',
'jets/e/rd.c',
'jets/e/rq.c',
'jets/e/rs.c',
'jets/e/rh.c',
'jets/e/rub.c',
'jets/e/scr.c',
'jets/e/shax.c',
'jets/e/lore.c',
'jets/e/loss.c',
'jets/e/lune.c',
'jets/e/trip.c'
]

jets_e_ed_src = [
'jets/e/ed_puck.c',
'jets/e/ed_sign.c',
'jets/e/ed_veri.c',
'jets/e/ed_shar.c'

]
jets_f_src = [
'jets/f/ap.c',
'jets/f/cell.c',
'jets/f/comb.c',
'jets/f/cons.c',
'jets/f/core.c',
'jets/f/face.c',
'jets/f/fitz.c',
'jets/f/flan.c',
'jets/f/flip.c',
'jets/f/flor.c',
'jets/f/fork.c',
'jets/f/help.c',
'jets/f/hike.c',
'jets/f/look.c',
'jets/f/loot.c'
]

jets_f_ut_src =  [
'jets/f/ut.c',
'jets/f/ut_burn.c',
'jets/f/ut_buss.c',
'jets/f/ut_conk.c',
'jets/f/ut_crop.c',
'jets/f/ut_find.c',
'jets/f/ut_fire.c',
'jets/f/ut_fish.c',
'jets/f/ut_fuse.c',
'jets/f/ut_gain.c',
'jets/f/ut_lose.c',
'jets/f/ut_mint.c',
'jets/f/ut_mull.c',
'jets/f/ut_nest.c',
'jets/f/ut_peek.c',
'jets/f/ut_peel.c',
'jets/f/ut_play.c',
'jets/f/ut_repo.c',
'jets/f/ut_rest.c',
'jets/f/ut_tack.c',
'jets/f/ut_toss.c',
'jets/f/ut_wrap.c'
]

jets_g_src = [
'jets/g/down.c'
]

jets_src = [
'jets/tree.c'
]
noun_src = ['noun/allocate.c',
  'noun/events.c',
  'noun/hashtable.c',
  'noun/imprison.c',
  'noun/jets.c',
  'noun/manage.c',
  'noun/nock.c',
  'noun/retrieve.c',
  'noun/trace.c',
  'noun/vortex.c',
  'noun/xtract.c',
  'noun/zave.c']

vere_src = ['vere/ames.c',
  'vere/behn.c',
  'vere/cttp.c',
  'vere/http.c',
  'vere/loop.c',
  'vere/main.c',
  'vere/raft.c',
  'vere/reck.c',
  'vere/save.c',
  'vere/sist.c',
  'vere/term.c',
  'vere/time.c',
  'vere/unix.c',
  'vere/walk.c']

src_list = [
vere_src, noun_src,
jets_a_src, jets_b_src,
jets_c_src, jets_d_src,
jets_e_src, jets_e_ed_src, jets_f_src, jets_f_ut_src,
jets_g_src, jets_src]

sources = []
foreach s : src_list
  sources += s
endforeach

incdir = include_directories('include/')

conf_data = configuration_data()
conf_data.set('URBIT_VERSION', '"0.5.1"')

osdet = build_machine.system()
os_c_flags = []
os_deps = []
os_link_flags = []

if osdet == 'linux'
  conf_data.set('U3_OS_linux', true)

  if(legacy_meson)
    pthread_dep = find_library('pthread')
  else
    pthread_dep = meson.get_compiler('c').find_library('pthread')
  endif

  ncurses_dep = dependency('ncurses')
  os_deps = os_deps + [pthread_dep, ncurses_dep]

elif osdet == 'darwin'
  conf_data.set('U3_OS_osx', true)

  os_c_flags = os_c_flags + ['-bind_at_load']
  #  os_link_flags = ['-framework CoreServices', '-framework CoreFoundation']
  if(legacy_meson)
    ncurses_dep = find_library('ncurses')
  else
    ncurses_dep = meson.get_compiler('c').find_library('ncurses')
  endif

  os_deps = os_deps + [ncurses_dep]

elif osdet == 'bsd'
  conf_data.set('U3_OS_bsd', true)

  pthread_dep = meson.get_compiler('c').find_library('pthread')
  kvm_dep = meson.get_compiler('c').find_library('kvm')
  ncurses_dep = dependency('ncurses')
  os_deps = os_deps + [kvm_dep, pthread_dep, ncurses_dep]
else
  error('Unsupported OS detected:' + osdet)
endif

endian = build_machine.endian()

if endian == 'little'
  conf_data.set('U3_OS_ENDIAN_little', true)
else
  error('Little endian encoding required')
endif

configure_file(input : 'include/config.h.in',
               output : 'config.h',
               configuration : conf_data)

# We expect these libs to supplied with the distribution
openssl_dep = dependency('openssl', version: '>=1.0.0')
curl_dep = dependency('libcurl', version: '>=7.35.0')
libuv_dep = dependency('libuv', version: '>=1.8.0')

if(legacy_meson)
  gmp_dep = find_library('gmp')
  sigsegv_dep = find_library('sigsegv')
else
  gmp_dep = meson.get_compiler('c').find_library('gmp')
  sigsegv_dep = meson.get_compiler('c').find_library('sigsegv')
endif

# For these libs we provide fallback bundle
cmark_dep = dependency('libcmark', version: '0.12.0', fallback: ['commonmark-legacy', 'cmark_dep'])
urbitscrypt_dep = dependency('libscrypt', version: '>=0.1.21', fallback: ['libscrypt', 'libscrypt_dep'])

ed25519_dep = dependency('ed25519', version: '>=0.1.0', fallback: ['ed25519', 'ed25519_dep'])
murmur3_dep = dependency('murmur3', version: '>=0.1.0', fallback: ['murmur3', 'murmur3_dep'])
http_parser_dep = dependency('http-parser',  version: '0.1.0', fallback: ['http-parser-legacy', 'http_parser_dep'])
softfloat3_dep = dependency('softfloat3', version: '>=3.0.0', fallback: ['softfloat3', 'softfloat3_dep'])


opt_flags = []
if get_option('debug')
  opt_flags = ['-g']
else
  opt_flags = ['-O3']
endif

executable('urbit',
sources : sources,
include_directories : incdir,
c_args : opt_flags + os_c_flags,
link_args: os_link_flags,
dependencies: [openssl_dep,
               curl_dep,
               libuv_dep,
               cmark_dep,
               gmp_dep,
               sigsegv_dep,
               urbitscrypt_dep,
               ed25519_dep,
               murmur3_dep,
               http_parser_dep,
               softfloat3_dep] + os_deps,
install: true)
