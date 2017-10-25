require 'pathname'
require 'fileutils'
include FileUtils

STDOUT.sync = true

Os = ENV.fetch('os')
QtVersionString = ENV.fetch('version')
QtVersionMajor = QtVersionString.split('.').first.to_i

QtBaseDir = Pathname(ENV.fetch('qtbase'))
OutDir = Pathname(ENV.fetch('out'))
OutPcDir = OutDir + 'lib' + 'pkgconfig'
OutIncDir = OutDir + 'include'

DepGraph = {}
DepGraphBack = {}

DepInfo = {}
DepInfo.default_proc = proc do |hash, name|
  hash[name] = find_dep_info(name)
end

case Os
when "windows"
  prl_prefix = ''
else
  prl_prefix = 'lib'
end

# Note: These dependencies just came from me fixing link errors for specific
# programs.  There are likely misisng dependencies in this graph, and there
# might be a few dependencies that could be safely removed because they are
# purely transitive.
def make_dep_graph
  add_dep 'Qt5Widgets.x', 'libQt5Widgets.a'
  add_dep 'Qt5Widgets.x', '-I' + (OutIncDir + 'QtWidgets').to_s
  add_dep 'Qt5Widgets.x', 'Qt5Gui.x'
  add_dep 'Qt5Gui.x', 'Qt5GuiNoPlugins.x'
  add_dep 'Qt5GuiNoPlugins.x', 'libQt5Gui.a'
  add_dep 'Qt5GuiNoPlugins.x', '-I' + (OutIncDir + 'QtGui').to_s
  add_dep 'Qt5Core.x', 'libQt5Core.a'
  add_dep 'Qt5Core.x', '-I' + OutIncDir.to_s
  add_dep 'Qt5Core.x', '-I' + (OutIncDir + 'QtCore').to_s

  add_dep 'libQt5Widgets.a', 'libQt5Gui.a'
  add_dep 'libQt5FontDatabaseSupport.a', 'libqtfreetype.a'
  add_dep 'libQt5Gui.a', 'libQt5Core.a'
  add_dep 'libQt5Gui.a', 'libqtlibpng.a'
  add_dep 'libQt5Gui.a', 'libqtharfbuzz.a'
  add_dep 'libQt5Core.a', 'libqtpcre.a'

  if Os == 'windows'
    add_dep 'Qt5Gui.x', 'qwindows.x'
    add_dep 'qwindows.x', 'libqwindows.a'

    add_dep 'libqwindows.a', '-ldwmapi'
    add_dep 'libqwindows.a', '-limm32'
    add_dep 'libqwindows.a', '-loleaut32'
    add_dep 'libqwindows.a', 'libQt5Gui.a'
    add_dep 'libqwindows.a', 'libQt5EventDispatcherSupport.a'
    add_dep 'libqwindows.a', 'libQt5FontDatabaseSupport.a'
    add_dep 'libqwindows.a', 'libQt5ThemeSupport.a'

    add_dep 'libQt5Core.a', '-lole32'
    add_dep 'libQt5Core.a', '-luuid'
    add_dep 'libQt5Core.a', '-lwinmm'
    add_dep 'libQt5Core.a', '-lws2_32'

    add_dep 'libQt5Gui.a', '-lopengl32'

    add_dep 'libQt5Widgets.a', '-luxtheme'
  end

  if Os == 'linux'
    add_dep 'Qt5Gui.x', 'qlinuxfb.x'
    add_dep 'Qt5Gui.x', 'qxcb.x'
    add_dep 'qlinuxfb.x', 'libqlinuxfb.a'
    add_dep 'qxcb.x', 'libqxcb.a'

    add_dep 'libqlinuxfb.a', 'libQt5FbSupport.a'
    add_dep 'libqlinuxfb.a', 'libQt5InputSupport.a'

    add_dep 'libqxcb.a', 'libQt5XcbQpa.a'

    add_dep 'libQt5DBus.a', 'libQt5Core.a'
    add_dep 'libQt5DBus.a', 'libQt5Gui.a'
    add_dep 'libQt5DeviceDiscoverySupport.a', 'libudev.pc'
    add_dep 'libQt5InputSupport.a', 'libQt5DeviceDiscoverySupport.a'
    add_dep 'libQt5LinuxAccessibilitySupport.a', 'libQt5AccessibilitySupport.a'
    add_dep 'libQt5LinuxAccessibilitySupport.a', 'libQt5DBus.a'
    add_dep 'libQt5LinuxAccessibilitySupport.a', 'xcb-aux.pc'
    add_dep 'libQt5ThemeSupport.a', 'libQt5DBus.a'

    add_dep 'libQt5XcbQpa.a', 'libQt5EventDispatcherSupport.a'
    add_dep 'libQt5XcbQpa.a', 'libQt5FontDatabaseSupport.a'
    add_dep 'libQt5XcbQpa.a', 'libQt5Gui.a'
    add_dep 'libQt5XcbQpa.a', 'libQt5LinuxAccessibilitySupport.a'
    add_dep 'libQt5XcbQpa.a', 'libQt5ServiceSupport.a'
    add_dep 'libQt5XcbQpa.a', 'libQt5ThemeSupport.a'
    add_dep 'libQt5XcbQpa.a', 'x11.pc'
    add_dep 'libQt5XcbQpa.a', 'x11-xcb.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-icccm.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-image.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-keysyms.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-randr.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-renderutil.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-shape.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-shm.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-sync.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-xfixes.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-xinerama.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-xkb.pc'
    add_dep 'libQt5XcbQpa.a', 'xi.pc'
  end
end

def parse_prl_file(filename)
  filename = Pathname(filename)
  filename = filename.sub_ext("d.prl") if !filename.exist?
  attrs = { prl_filename: filename }
  File.foreach(filename.to_s) do |line|
    md = line.match(/(\w+) = (.*)/)
    attrs[md[1]] = md[2]
  end
  attrs
end

def libs_from_prl(prl)
  libs = []

  target = prl.fetch('QMAKE_PRL_TARGET')
  if !Pathname(target).absolute?
    libs << "-L#{prl.fetch(:prl_filename).dirname}"
  end
  if md = target.match(/lib(\w+).a/)
    libs << target
  end

  listed_libs = prl.fetch('QMAKE_PRL_LIBS')
  listed_libs.gsub!(/\$\$\[QT_INSTALL_LIBS\]/, (OutDir + 'lib').to_s)
  libs.concat listed_libs.split(' ')

  libs
end

def add_dep(library, *deps)
  a = DepGraph[library] ||= []
  DepGraphBack[library] ||= []
  deps.each do |dep|
    DepGraph[dep] ||= []
    a << dep unless a.include? dep
    (DepGraphBack[dep] ||= []) << library
  end
end

# Given a name of a dep in the graph, figure out what kind of dep
# it use.
def determine_dep_type(name)
  extension = Pathname(name).extname
  case
  when extension == '.a' then :a
  when extension == '.pc' then :pc
  when extension == '.x' then :x
  when name.start_with?('-I') then :cflag
  when name.start_with?('-l') then :ldflag
  end
end

def find_pkg_config_file(name)
  ENV.fetch('PKG_CONFIG_CROSS_PATH').split(':').each do |dir|
    path = Pathname(dir) + name
    return path if path.exist?
  end
  nil
end

def find_qt_library(name)
  debug_name = Pathname(name).sub_ext("d.a").to_s

  search_dirs = [ OutDir + 'lib' ] +
    (OutDir + 'plugins').children

  search_dirs.each do |dir|
    lib = dir + name
    return lib if lib.exist?
  end

  search_dirs.each do |dir|
    lib = dir + debug_name
    return lib if lib.exist?
  end

  nil
end

def find_dep_info(name)
  case determine_dep_type(name)
  when :a then find_qt_library(name)
  when :pc then find_pkg_config_file(name)
  end
end

# Given an array of dependencies and a block for retrieving dependencies of an
# dependency, returns an array of dependencies with three guarantees:
#
# 1) Contains all the listed dependencies.
# 2) Has no duplicates.
# 3) For any dependency in the list, all of its dependencies before after it.
#
# Guarantee 3 only holds if the underlying graph has no circul dependencies.  If
# there is a circular dependency, it will not be detected, but it will not cause
# an infinite loop either.
def flatten_deps(deps)
  work = deps
  expanded = {}
  output = {}
  while !work.empty?
    dep = work.last
    if expanded[dep]
      output[dep] = true
      work.pop
    else
      expanded[dep] = true
      deps = yield dep
      work.concat(deps)
    end
  end
  output.keys  # relies on Ruby's ordered hashes
end

def canonical_x_file(dep)
  return nil if determine_dep_type(dep) != :a
  x_files = DepGraphBack.fetch(dep).select do |name|
    determine_dep_type(name) == :x
  end
  if x_files.size > 2
    raise "There is more than one .x file #{dep}."
  end
  x_files.first
end

# Note: It would be nice to find some solution so that Qt5Widgets.pc does not
# require on Qt5GuiNoPlugins, since it already depends on Qt5Gui.
def flatten_deps_for_pc_file(pc_file)
  flatten_deps(DepGraph[pc_file]) do |dep|
    deps = case determine_dep_type(dep)
           when :x then []
           else DepGraph.fetch(dep)
           end

    # Replace .a files with a canonical .x file if there is one.
    deps.map do |name|
      substitute = canonical_x_file(name)
      substitute = nil if substitute == pc_file
      substitute || name
    end
  end
end

def create_pc_file(name)
  requires = []
  libdirs = []
  ldflags = []
  cflags = []

  deps = flatten_deps_for_pc_file(name)

  deps.each do |dep|
    dep = dep.dup
    case determine_dep_type(dep)
    when :a then
      full_path = DepInfo[dep]
      raise "Could not find library: #{dep}" if !full_path
      libdir = full_path.dirname.to_s
      libdir.sub!((OutDir + 'lib').to_s, '${libdir}')
      libdir.sub!(OutDir.to_s, '${prefix}')
      libname = full_path.basename.to_s
      libname.sub!(/\Alib/, '')
      libname.sub!(/.a\Z/, '')
      libdirs << "-L#{libdir}"
      ldflags << "-l#{libname}"
    when :x then
      dep.chomp!('.x')
      requires << dep
    when :pc then
      dep.chomp!('.pc')
      requires << dep
    when :ldflag then
      ldflags << dep
    when :cflag then
      dep.sub!(OutIncDir.to_s, '${includedir}')
      cflags << dep
    end
  end

  r = ""
  r << "prefix=#{OutDir}\n"
  r << "libdir=${prefix}/lib\n"
  r << "includedir=${prefix}/include\n"
  r << "Version: #{QtVersionString}\n"
  if !libdirs.empty? || !ldflags.empty?
    r << "Libs: #{libdirs.reverse.uniq.join(' ')} #{ldflags.reverse.join(' ')}\n"
  end
  if !cflags.empty?
    r << "Cflags: #{cflags.join(' ')}\n"
  end
  if !requires.empty?
    r << "Requires: #{requires.sort.join(' ')}\n"
  end

  path = OutPcDir + Pathname(name).sub_ext(".pc")
  File.open(path.to_s, 'w') do |f|
    f.write r
  end
end

# For .pc files we depend on, add symlinks to the .pc file and any other .pc
# files in the same directory which might be transitive dependencies.
def symlink_pc_file_closure(name)
  dep_pc_dir = DepInfo[name].dirname
  dep_pc_dir.each_child do |target|
    link = OutPcDir + target.basename

    # Skip it if we already made this link.
    next if link.symlink?

    # Link directly to the real PC file.
    target = target.realpath

    ln_s target, link
  end
end

def create_pc_files
  mkdir OutPcDir
  DepGraph.each_key do |name|
    case determine_dep_type(name)
    when :x then create_pc_file(name)
    when :pc then symlink_pc_file_closure(name)
    end
  end
end

# Symlink the include, bin, and plugins directories into $out.

mkdir OutDir
ln_s QtBaseDir + 'include', OutDir + 'include'
ln_s QtBaseDir + 'bin', OutDir + 'bin'
ln_s QtBaseDir + 'plugins', OutDir + 'plugins'
ln_s QtBaseDir + 'src', OutDir + 'src'

# Symlink the .a files and copy the .prl files into $out/lib.

mkdir OutDir + 'lib'
(QtBaseDir + 'lib').each_child do |c|
  ln_s c, OutDir + 'lib' if c.extname == '.a'
  cp c, OutDir + 'lib' if c.extname == '.prl'
end

make_dep_graph

create_pc_files

CMakeDir = OutDir + 'lib' + 'cmake'
mkdir CMakeDir

mkdir CMakeDir + 'Qt5Widgets'

File.open(CMakeDir + 'core.cmake', 'w') do |f|
  f.puts "set(QT_VERSION_MAJOR #{QtVersionMajor})"
  f.puts

  moc_exe = OutDir + 'bin' + 'moc'
  f.puts "set(QT_MOC_EXECUTABLE #{moc_exe})"
  f.puts "add_executable(Qt5::moc IMPORTED)"
  f.puts "set_target_properties(Qt5::moc PROPERTIES " \
         "IMPORTED_LOCATION ${QT_MOC_EXECUTABLE})"
  f.puts

  rcc_exe = OutDir + 'bin' + 'rcc'
  f.puts "add_executable(Qt5::rcc IMPORTED)"
  f.puts "set_target_properties(Qt5::rcc PROPERTIES " \
         "IMPORTED_LOCATION #{rcc_exe})"
  f.puts "set(Qt5Core_RCC_EXECUTABLE Qt5::rcc)"
  f.puts

  # These macros come from src/corelib/Qt5CoreMacros.cmake originally.
  # Perhaps we should just copy that file from Qt's source directly if
  # it doesn't need any patches.
  f.puts <<EOF
macro(QT5_MAKE_OUTPUT_FILE infile prefix ext outfile )
  string(LENGTH ${CMAKE_CURRENT_BINARY_DIR} _binlength)
  string(LENGTH ${infile} _infileLength)
  set(_checkinfile ${CMAKE_CURRENT_SOURCE_DIR})
  if(_infileLength GREATER _binlength)
    string(SUBSTRING "${infile}" 0 ${_binlength} _checkinfile)
    if(_checkinfile STREQUAL "${CMAKE_CURRENT_BINARY_DIR}")
      file(RELATIVE_PATH rel ${CMAKE_CURRENT_BINARY_DIR} ${infile})
    else()
      file(RELATIVE_PATH rel ${CMAKE_CURRENT_SOURCE_DIR} ${infile})
    endif()
  else()
    file(RELATIVE_PATH rel ${CMAKE_CURRENT_SOURCE_DIR} ${infile})
  endif()
  if(WIN32 AND rel MATCHES "^([a-zA-Z]):(.*)$") # absolute path
    set(rel "${CMAKE_MATCH_1}_${CMAKE_MATCH_2}")
  endif()
  set(_outfile "${CMAKE_CURRENT_BINARY_DIR}/${rel}")
  string(REPLACE ".." "__" _outfile ${_outfile})
  get_filename_component(outpath ${_outfile} PATH)
  get_filename_component(_outfile ${_outfile} NAME_WE)
  file(MAKE_DIRECTORY ${outpath})
  set(${outfile} ${outpath}/${prefix}${_outfile}.${ext})
endmacro()

function(_QT5_PARSE_QRC_FILE infile _out_depends _rc_depends)
  get_filename_component(rc_path ${infile} PATH)
  if(EXISTS "${infile}")
    file(READ "${infile}" RC_FILE_CONTENTS)
    string(REGEX MATCHALL "<file[^<]+" RC_FILES "${RC_FILE_CONTENTS}")
    foreach(RC_FILE ${RC_FILES})
      string(REGEX REPLACE "^<file[^>]*>" "" RC_FILE "${RC_FILE}")
      if(NOT IS_ABSOLUTE "${RC_FILE}")
        set(RC_FILE "${rc_path}/${RC_FILE}")
      endif()
      set(RC_DEPENDS ${RC_DEPENDS} "${RC_FILE}")
    endforeach()
    qt5_make_output_file("${infile}" "" "qrc.depends" out_depends)
    configure_file("${infile}" "${out_depends}" COPYONLY)
  else()
    set(out_depends)
  endif()
  set(${_out_depends} ${out_depends} PARENT_SCOPE)
  set(${_rc_depends} ${RC_DEPENDS} PARENT_SCOPE)
endfunction()

function(QT5_ADD_RESOURCES outfiles )
  set(options)
  set(oneValueArgs)
  set(multiValueArgs OPTIONS)
  cmake_parse_arguments(_RCC "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
  set(rcc_files ${_RCC_UNPARSED_ARGUMENTS})
  set(rcc_options ${_RCC_OPTIONS})

  if("${rcc_options}" MATCHES "-binary")
    message(WARNING "Use qt5_add_binary_resources for binary option")
  endif()

  foreach(it ${rcc_files})
    get_filename_component(outfilename ${it} NAME_WE)
    get_filename_component(infile ${it} ABSOLUTE)
    set(outfile ${CMAKE_CURRENT_BINARY_DIR}/qrc_${outfilename}.cpp)
    _QT5_PARSE_QRC_FILE(${infile} _out_depends _rc_depends)
    add_custom_command(OUTPUT ${outfile}
                       COMMAND ${Qt5Core_RCC_EXECUTABLE}
                       ARGS ${rcc_options} --name ${outfilename} --output ${outfile} ${infile}
                       MAIN_DEPENDENCY ${infile}
                       DEPENDS ${_rc_depends} "${out_depends}" VERBATIM)
    list(APPEND ${outfiles} ${outfile})
  endforeach()
  set(${outfiles} ${${outfiles}} PARENT_SCOPE)
endfunction()
EOF
end

# TODO: create CMake files like this using the dependency graph we used for .pc files.
File.open(CMakeDir + 'Qt5Widgets' + 'Qt5WidgetsConfig.cmake', 'w') do |f|
  widgets_a = find_qt_library('libQt5Widgets.a') || raise
  core_a = find_qt_library('libQt5Core.a') || raise

  includes = [
    QtBaseDir + 'include',
    QtBaseDir + 'include' + 'QtWidgets',
    QtBaseDir + 'include' + 'QtCore',
    QtBaseDir + 'include' + 'QtGui',
  ]

  libs = [ core_a ]
  prls = [
    OutDir + 'lib' + (prl_prefix + 'Qt5Widgets.prl'),
    OutDir + 'lib' + (prl_prefix + 'Qt5Gui.prl'),
    OutDir + 'lib' + (prl_prefix + 'Qt5Core.prl'),
  ]
  if Os == "windows"
    prls << OutDir + 'plugins' + 'platforms' + 'qwindows.prl'
  end
  if Os == "linux"
    prls << OutDir + 'plugins' + 'platforms' + 'libqlinuxfb.prl'
    prls << OutDir + 'plugins' + 'platforms' + 'libqxcb.prl'
  end

  prls.each do |prl|
    prl_libs = libs_from_prl(parse_prl_file(prl))
    libs.concat(prl_libs)
  end

  properties = {
    IMPORTED_LOCATION: widgets_a,
    IMPORTED_LINK_INTERFACE_LANGUAGES: 'CXX',
    IMPORTED_LINK_INTERFACE_LIBRARIES: libs.join(' '),
    INTERFACE_INCLUDE_DIRECTORIES: includes.join(' '),
    INTERFACE_COMPILE_DEFINITIONS: 'QT_STATIC',
  }

  f.puts "add_library(Qt5::Widgets STATIC IMPORTED)"
  properties.each do |name, value|
    f.puts "set_property(TARGET Qt5::Widgets PROPERTY #{name} #{value})"
  end

  f.puts "include(#{CMakeDir + 'core.cmake'})"
end
