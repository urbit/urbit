require 'pathname'
require 'fileutils'
include FileUtils

STDOUT.sync = true

ENV['PATH'] = ENV.fetch('_PATH')

Os = ENV.fetch('os')
QtVersionString = ENV.fetch('version')
QtVersionMajor = QtVersionString.split('.').first.to_i

QtBaseDir = Pathname(ENV.fetch('qtbase'))

OutDir = Pathname(ENV.fetch('out'))
OutPcDir = OutDir + 'lib' + 'pkgconfig'
CMakeDir = OutDir + 'lib' + 'cmake'
OutIncDir = OutDir + 'include'
MocExe = OutDir + 'bin' + 'moc'
RccExe = OutDir + 'bin' + 'rcc'

DepGraph = {}
DepGraphBack = {}

DepInfo = {}
DepInfo.default_proc = proc do |hash, name|
  hash[name] = find_dep_info(name)
end

case Os
when "windows"
  PrlPrefix = ''
else
  PrlPrefix = 'lib'
end

# Note: These dependencies just came from me fixing errors for specific
# programs.  There are likely misisng dependencies in this graph, and there
# might be a few dependencies that could be safely removed because they are
# purely transitive.
def make_dep_graph
  # High-level dependencies.
  add_dep 'Qt5Widgets.x', 'Qt5WidgetsNoPlugins.x'
  add_dep 'Qt5WidgetsNoPlugins.x', 'libQt5Widgets.a'
  add_dep 'Qt5WidgetsNoPlugins.x', 'Qt5Gui.x'
  add_dep 'Qt5Gui.x', 'Qt5GuiNoPlugins.x'
  add_dep 'Qt5GuiNoPlugins.x', 'libQt5Gui.a'
  add_dep 'Qt5GuiNoPlugins.x', 'Qt5Core.x'
  add_dep 'Qt5Core.x', 'libQt5Core.a'

  # Include directories.
  add_dep 'Qt5Core.x', '-I' + OutIncDir.to_s
  add_dep 'Qt5Core.x', '-I' + (OutIncDir + 'QtCore').to_s
  add_dep 'Qt5GuiNoPlugins.x', '-I' + (OutIncDir + 'QtGui').to_s
  add_dep 'Qt5WidgetsNoPlugins.x', '-I' + (OutIncDir + 'QtWidgets').to_s

  # Libraries that Qt depends on.
  add_dep 'libQt5Widgets.a', 'libQt5Gui.a'
  add_dep 'libQt5FontDatabaseSupport.a', 'libqtfreetype.a'
  add_dep 'libQt5Gui.a', 'libQt5Core.a'
  add_dep 'libQt5Gui.a', 'libqtlibpng.a'
  add_dep 'libQt5Gui.a', 'libqtharfbuzz.a'
  add_dep 'libQt5Core.a', 'libqtpcre2.a'

  if Os == 'windows'
    add_dep 'Qt5Gui.x', 'qwindows.x'

    add_dep 'qwindows.x', 'libqwindows.a'

    add_dep 'libqwindows.a', '-ldwmapi'
    add_dep 'libqwindows.a', '-limm32'
    add_dep 'libqwindows.a', '-loleaut32'
    add_dep 'libqwindows.a', '-lwtsapi32'
    add_dep 'libqwindows.a', 'libQt5Gui.a'
    add_dep 'libqwindows.a', 'libQt5EventDispatcherSupport.a'
    add_dep 'libqwindows.a', 'libQt5FontDatabaseSupport.a'
    add_dep 'libqwindows.a', 'libQt5ThemeSupport.a'
    add_dep 'libqwindows.a', 'libQt5WindowsUIAutomationSupport.a'

    add_dep 'Qt5Widgets.x', 'qwindowsvistastyle.x'
    add_dep 'qwindowsvistastyle.x', 'libqwindowsvistastyle.a'

    add_dep 'libqwindowsvistastyle.a', '-luxtheme'
    add_dep 'libqwindowsvistastyle.a', 'libQt5Widgets.a'

    add_dep 'libQt5Core.a', '-lnetapi32'
    add_dep 'libQt5Core.a', '-lole32'
    add_dep 'libQt5Core.a', '-luserenv'
    add_dep 'libQt5Core.a', '-luuid'
    add_dep 'libQt5Core.a', '-lversion'
    add_dep 'libQt5Core.a', '-lwinmm'
    add_dep 'libQt5Core.a', '-lws2_32'

    add_dep 'libQt5Gui.a', '-lopengl32'
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

    add_dep 'libQt5XcbQpa.a', 'libQt5EdidSupport.a'
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
    add_dep 'libQt5XcbQpa.a', 'xcb-xinput.pc'
    add_dep 'libQt5XcbQpa.a', 'xcb-xkb.pc'
    add_dep 'libQt5XcbQpa.a', 'xi.pc'
    add_dep 'libQt5XcbQpa.a', 'xkbcommon.pc'
    add_dep 'libQt5XcbQpa.a', 'xkbcommon-x11.pc'
  end

  if Os == 'macos'
    add_dep 'Qt5Gui.x', 'qcocoa.x'
    add_dep 'qcocoa.x', 'libqcocoa.a'

    add_dep 'libqcocoa.a', 'libcocoaprintersupport.a'
    add_dep 'libqcocoa.a', 'libQt5AccessibilitySupport.a'
    add_dep 'libqcocoa.a', 'libQt5ClipboardSupport.a'
    add_dep 'libqcocoa.a', 'libQt5GraphicsSupport.a'
    add_dep 'libqcocoa.a', 'libQt5FontDatabaseSupport.a'
    add_dep 'libqcocoa.a', 'libQt5ThemeSupport.a'
    add_dep 'libqcocoa.a', 'libQt5PrintSupport.a'
    add_dep 'libqcocoa.a', '-lcups'  # Also available: -lcups.2
    add_dep 'libqcocoa.a', '-framework IOKit'
    add_dep 'libqcocoa.a', '-framework IOSurface'
    add_dep 'libqcocoa.a', '-framework CoreVideo'
    add_dep 'libqcocoa.a', '-framework Metal'
    add_dep 'libqcocoa.a', '-framework QuartzCore'

    add_dep 'libqtlibpng.a', '-lz'

    add_dep 'libQt5Core.a', '-lobjc'
    add_dep 'libQt5Core.a', '-framework CoreServices'
    add_dep 'libQt5Core.a', '-framework CoreText'
    add_dep 'libQt5Core.a', '-framework Security'
    add_dep 'libQt5Gui.a', '-framework CoreGraphics'
    add_dep 'libQt5Gui.a', '-framework OpenGL'
    add_dep 'libQt5Widgets.a', '-framework Carbon'
    add_dep 'libQt5Widgets.a', '-framework AppKit'
  end

  add_deps_of_pc_files
end

# Qt depends on some system libraries with .pc files.  It tends to only depend
# on these things at link time, not compile time.  So use pkg-config with --libs
# to get those dependencies, for use in .cmake files.
def add_deps_of_pc_files
  DepGraph.keys.each do |dep|
    next if determine_dep_type(dep) != :pc
    name = dep.chomp('.pc')
    new_deps = `pkg-config-cross --libs #{name}`.split(' ')
    raise "Failed to #{dep} libs" if $?.exitstatus != 0
    new_deps.each do |new_dep|
      add_dep dep, new_dep
    end
  end
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
  when name.start_with?('-I') then :incdirflag
  when name.start_with?('-L') then :libdirflag
  when name.start_with?('-l') then :ldflag
  when name.start_with?('-framework') then :ldflag
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
# 3) For any dependency in the list, all of its dependencies are before it.
#
# Guarantee 3 only holds if the underlying graph has no circul dependencies.  If
# there is a circular dependency, it will not be detected, but it will not cause
# an infinite loop either.
def flatten_deps(deps)
  work = [].concat(deps)
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
# require Qt5GuiNoPlugins, since it already requires Qt5Gui.
def flatten_deps_for_pc_file(pc_file)
  flatten_deps(DepGraph[pc_file]) do |dep|
    deps = case determine_dep_type(dep)
           when :x, :pc then
             # Don't expand dependencies for a .pc file because we can just
             # refer to them with the Requires line in our .pc file.
             []
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

def flatten_deps_for_cmake_file(cmake_file)
  flatten_deps(DepGraph[cmake_file]) do |dep|
    DepGraph.fetch(dep)
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
    when :libdirflag then
      libdirs << dep
    when :incdirflag then
      dep.sub!(OutIncDir.to_s, '${includedir}')
      cflags << dep
    end
  end

  pkg_name = Pathname(name).sub_ext('').to_s

  r = ""
  r << "prefix=#{OutDir}\n"
  r << "libdir=${prefix}/lib\n"
  r << "includedir=${prefix}/include\n"
  r << "Name: #{pkg_name}\n"
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

  path = OutPcDir + (pkg_name + ".pc")
  File.open(path, 'w') do |f|
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

def set_property(f, target_name, property_name, value)
  if value.is_a?(Array)
    value = value.map do |entry|
      if entry.to_s.include?(' ')
        "\"#{entry}\""
      else
        entry
      end
    end.join(' ')
  end

  f.puts "set_property(TARGET #{target_name} " \
         "PROPERTY #{property_name} #{value})"
end

def set_properties(f, target_name, properties)
  properties.each do |property_name, value|
    set_property(f, target_name, property_name, value)
  end
end

def import_static_lib(f, target_name, properties)
  f.puts "add_library(#{target_name} STATIC IMPORTED)"
  set_properties(f, target_name, properties)
end

def create_cmake_core_files
  File.open(CMakeDir + 'core.cmake', 'w') do |f|
    f.puts "set(QT_VERSION_MAJOR #{QtVersionMajor})"
    f.puts

    f.puts "set(QT_MOC_EXECUTABLE #{MocExe})"
    f.puts "add_executable(Qt5::moc IMPORTED)"
    f.puts "set_target_properties(Qt5::moc PROPERTIES " \
           "IMPORTED_LOCATION ${QT_MOC_EXECUTABLE})"
    f.puts

    f.puts "add_executable(Qt5::rcc IMPORTED)"
    f.puts "set_target_properties(Qt5::rcc PROPERTIES " \
           "IMPORTED_LOCATION #{RccExe})"
    f.puts "set(Qt5Core_RCC_EXECUTABLE Qt5::rcc)"
    f.puts

    f.write File.read(ENV.fetch('core_macros'))
  end
end

def create_cmake_qt5widgets
  mkdir CMakeDir + 'Qt5Widgets'

  widgets_a = find_qt_library('libQt5Widgets.a') || raise

  deps = flatten_deps_for_cmake_file('Qt5Widgets.x')

  incdirs = []
  libdirflags = []
  ldflags = []
  deps.each do |dep|
    dep = dep.dup
    case determine_dep_type(dep)
    when :a then
      full_path = DepInfo[dep]
      raise "Could not find library: #{dep}" if !full_path
      libdir = full_path.dirname.to_s
      libname = full_path.basename.to_s
      libname.sub!(/\Alib/, '')
      libname.sub!(/.a\Z/, '')
      libdirflags << "-L#{libdir}"
      ldflags << "-l#{libname}"
    when :ldflag then
      ldflags << dep
    when :libdirflag then
      libdirflags << dep
    when :incdirflag then
      incdir = dep.sub(/\A-I/, '')
      incdirs << incdir
    end
  end

  File.open(CMakeDir + 'Qt5Widgets' + 'Qt5WidgetsConfig.cmake', 'w') do |f|
    import_static_lib f, 'Qt5::Widgets',
      IMPORTED_LOCATION: widgets_a,
      IMPORTED_LINK_INTERFACE_LANGUAGES: 'CXX',
      INTERFACE_LINK_LIBRARIES: libdirflags.reverse.uniq + ldflags.reverse,
      INTERFACE_INCLUDE_DIRECTORIES: incdirs,
      INTERFACE_COMPILE_DEFINITIONS: 'QT_STATIC'

    f.puts "include(#{CMakeDir + 'core.cmake'})"
  end
end

def main
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

  mkdir CMakeDir
  create_cmake_core_files
  create_cmake_qt5widgets
end

main
