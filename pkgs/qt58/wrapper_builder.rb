require 'pathname'
require 'fileutils'
include FileUtils

QtBaseDir = Pathname(ENV.fetch('qtbase'))
OutDir = Pathname(ENV.fetch('out'))

puts
puts "qtbase: #{QtBaseDir}"
puts "out: #{OutDir}"
puts

mkdir OutDir
symlink QtBaseDir + 'include', OutDir + 'include'
symlink QtBaseDir + 'bin', OutDir + 'bin'
symlink QtBaseDir + 'plugins', OutDir + 'plugins'

mkdir OutDir + 'lib'

(QtBaseDir + 'lib').each_child do |c|
  if c.extname == '.a'
    symlink c, OutDir + 'lib'
  end
end

CMakeDir = OutDir + 'lib' + 'cmake'
mkdir CMakeDir

mkdir CMakeDir + 'Qt5Widgets'

File.open(CMakeDir + 'Qt5Widgets' + 'Qt5WidgetsConfig.cmake', 'w') do |f|
  afile = OutDir + 'lib' + 'libQt5Widgets.a'

  includes = [
    QtBaseDir + 'include',
    QtBaseDir + 'include' + 'QtWidgets',
    QtBaseDir + 'include' + 'QtCore',
    QtBaseDir + 'include' + 'QtGui',
  ]

  properties = {
    IMPORTED_LOCATION: afile,
    IMPORTED_LINK_INTERFACE_LANGUAGES: 'CXX',
    INTERFACE_INCLUDE_DIRECTORIES: includes.join(' '),
    INTERFACE_COMPILE_DEFINITIONS: '',
  }

  f.puts "add_library(Qt5::Widgets STATIC IMPORTED)"
  properties.each do |name, value|
    f.puts "set_property(TARGET Qt5::Widgets PROPERTY #{name} #{value})"
  end
end
