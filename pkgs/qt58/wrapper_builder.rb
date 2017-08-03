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

DependencyGraph = {}
LibTypes = {}
PcFiles = {}
PluginGroup = {}

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
def make_dependency_graph
  add_dep 'Qt5Widgets', 'Qt5Gui'
  add_dep 'Qt5Gui', 'Qt5Core'
  add_dep 'Qt5Gui', 'qtlibpng'
  add_dep 'Qt5Gui', 'qtharfbuzz'
  add_dep 'Qt5Core', 'qtpcre'

  if Os == 'linux'
    add_dep 'Qt5DBus', 'Qt5Core'
    add_dep 'Qt5DBus', 'Qt5Gui'
    # add_dep 'Qt5Gui', 'qxcb'   # TODO: this can't be a "dep" because that makes it circular
    add_dep 'Qt5FontDatabaseSupport', 'qtfreetype'
    add_dep 'Qt5LinuxAccessibilitySupport', 'Qt5AccessibilitySupport'
    add_dep 'Qt5LinuxAccessibilitySupport', 'Qt5DBus'
    add_dep 'Qt5LinuxAccessibilitySupport', 'xcb-aux'
    add_dep 'Qt5ThemeSupport', 'Qt5DBus'

    # TODO: add_dep 'Qt5Gui', 'qlinuxfb'
    add_dep 'Qt5XcbQpa', 'Qt5EventDispatcherSupport'
    add_dep 'Qt5XcbQpa', 'Qt5FontDatabaseSupport'
    add_dep 'Qt5XcbQpa', 'Qt5Gui'
    add_dep 'Qt5XcbQpa', 'Qt5LinuxAccessibilitySupport'
    add_dep 'Qt5XcbQpa', 'Qt5ServiceSupport'
    add_dep 'Qt5XcbQpa', 'Qt5ThemeSupport'
    add_dep 'Qt5XcbQpa', 'x11'
    add_dep 'Qt5XcbQpa', 'x11-xcb'
    add_dep 'Qt5XcbQpa', 'xcb'
    add_dep 'Qt5XcbQpa', 'xcb-icccm'
    add_dep 'Qt5XcbQpa', 'xcb-image'
    add_dep 'Qt5XcbQpa', 'xcb-keysyms'
    add_dep 'Qt5XcbQpa', 'xcb-randr'
    add_dep 'Qt5XcbQpa', 'xcb-shape'
    add_dep 'Qt5XcbQpa', 'xcb-shm'
    add_dep 'Qt5XcbQpa', 'xcb-sync'
    add_dep 'Qt5XcbQpa', 'xcb-xfixes'
    add_dep 'Qt5XcbQpa', 'xcb-xinerama'
    add_dep 'Qt5XcbQpa', 'xcb-xkb'
    add_dep 'Qt5XcbQpa', 'xi'

    add_dep 'qxcb', 'Qt5XcbQpa'
  end
end

def parse_prl_file(filename)
  attrs = { prl_filename: Pathname(filename) }
  File.foreach(filename) do |line|
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
  a = DependencyGraph[library] ||= []
  deps.each do |dep|
    DependencyGraph[dep] ||= []
    a << dep unless a.include? dep
  end
end

def find_pkg_config_cross_file(name)
  ENV.fetch('PKG_CONFIG_CROSS_PATH').split(':').each do |dir|
    path = Pathname(dir) + "#{name}.pc"
    if path.exist?
      puts "found pc file for #{name}"
      return path
    end
  end
  nil
end

# Determine if this library:
# - comes from Qt,
# - comes from a .pc file,
# - or comes from the compiler toolchain
def determine_lib_type(name)
  if (OutDir + 'lib' + "lib#{name}.a").exist?
    LibTypes[name] = :qt
    return
  end

  plugin_paths = Pathname.glob(OutDir + 'plugins' + '*' + "lib#{name}.a")
  if plugin_paths.size == 1
    LibTypes[name] = :qt
    PluginGroup[name] = plugin_paths.first.dirname.basename
    return
  end

  path = find_pkg_config_cross_file(name)
  if path
    LibTypes[name] = :pc
    PcFiles[name] = path
    return
  end

  LibTypes[name] = :compiler
end

def determine_lib_types
  DependencyGraph.keys.each do |lib|
    determine_lib_type(lib)
  end
end

def create_pc_file_for_qt_library(name)
  puts "Creating pc file for Qt library #{name}"

  requires = []
  libs = []
  cflags = []

  DependencyGraph[name].each do |dep|
    case LibTypes[dep]
    when :qt then requires << dep
    when :pc then requires << dep
    when :compiler then libs << dep
    end
  end

  name_no_num = name.gsub(/Qt\d/, 'Qt')
  if (OutDir + 'include' + name_no_num).directory?
    cflags << "-I${includedir}/#{name_no_num}"
  end
  cflags << "-I${includedir}"

  libdir = '${libdir}'
  if PluginGroup[name]
    libdir = "${prefix}/plugins/#{PluginGroup[name]}"
  end

  path = OutPcDir + "#{name}.pc"
  File.open(path.to_s, 'w') do |f|
    f.write <<EOF
prefix=#{OutDir}
libdir=${prefix}/lib
includedir=${prefix}/include
Version: #{QtVersionString}
Libs: -L#{libdir} -l#{name} #{libs.join(' ')}
Cflags: #{cflags.join(' ')}
Requires: #{requires.join(' ')}
EOF
  end
end

# For .pc files we depend on, add symlinks to the .pc file and any other .pc
# files in the same directory which might be transitive dependencies.
def symlink_pc_file_closure(name)
  puts "Symlinking pc files for #{name}"
  dep_pc_dir = PcFiles.fetch(name).dirname
  dep_pc_dir.each_child do |target|
    link = OutPcDir + target.basename

    # Skip it if we already made this link.
    next if link.symlink?

    puts "  Symlinking lib/pkgconfig/#{link.basename}"

    # Link directly to the real PC file.
    target = target.readlink while target.symlink?

    ln_s target, link
  end
end

def create_pc_files
  mkdir OutPcDir
  LibTypes.each do |name, type|
    case type
    when :qt
      create_pc_file_for_qt_library(name)
    when :pc
      symlink_pc_file_closure(name)
    end
  end
end

# Symlink the include, bin, and plugins directories into $out.

mkdir OutDir
symlink QtBaseDir + 'include', OutDir + 'include'
symlink QtBaseDir + 'bin', OutDir + 'bin'
symlink QtBaseDir + 'plugins', OutDir + 'plugins'

# Symlink the .a files and copy the .prl files into $out/lib.

mkdir OutDir + 'lib'
(QtBaseDir + 'lib').each_child do |c|
  symlink c, OutDir + 'lib' if c.extname == '.a'
  cp c, OutDir + 'lib' if c.extname == '.prl'
end

make_dependency_graph

determine_lib_types

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

File.open(CMakeDir + 'Qt5Widgets' + 'Qt5WidgetsConfig.cmake', 'w') do |f|
  afile = OutDir + 'lib' + 'libQt5Widgets.a'

  includes = [
    QtBaseDir + 'include',
    QtBaseDir + 'include' + 'QtWidgets',
    QtBaseDir + 'include' + 'QtCore',
    QtBaseDir + 'include' + 'QtGui',
  ]

  libs = [ OutDir + 'lib' + 'libQt5Core.a' ]
  prls = [
    OutDir + 'lib' + (prl_prefix + 'Qt5Widgets.prl'),
    OutDir + 'lib' + (prl_prefix + 'Qt5Gui.prl'),
    OutDir + 'lib' + (prl_prefix +'Qt5Core.prl'),
  ]
  if Os == "windows"
    prls << OutDir + 'plugins' + 'platforms' + 'qwindows.prl'
  end

  prls.each do |prl|
    prl_libs = libs_from_prl(parse_prl_file(prl))
    libs.concat(prl_libs)
  end

  properties = {
    IMPORTED_LOCATION: afile,
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
