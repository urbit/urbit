require 'pathname'
require 'fileutils'
include FileUtils

OutDir = Pathname(ENV.fetch('out'))
LibDirs = ENV.fetch('libs').split(' ').map { |s| Pathname(s) }

def symlink_file(target, dest)
  real_target = target.realpath

  if dest.exist?
    if !dest.symlink?
      raise "Want to link #{dest} (to #{target}) " \
            "but it already exists and is not a symlink."
    end

    current_target = dest.readlink
    if current_target != real_target
      raise "Conflict: #{dest} links to #{current_target} " \
            "but we want to link it to #{real_target}."
    end
  else
    dest.make_symlink(real_target)
  end
end

def recursive_symlink(target, dest)
  if target.directory?
    dest.mkdir if !dest.directory?
    target.children(false).each do |c|
      recursive_symlink(target + c, dest + c)
    end
  else
    symlink_file(target, dest)
  end
end

LibDirs.each do |libdir|
  recursive_symlink(libdir, OutDir)
end
