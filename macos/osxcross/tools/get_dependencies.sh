#!/usr/bin/env sh
#
# auto-install dependency packages using the systems package manager.
# this assumes you are running as root or are using sudo
#

get_fedora_deps()
{
 yum install clang llvm-devel libxml2-devel libuuid-devel openssl-devel \
  bash patch libstdc++-static make
}

get_freebsd_deps()
{
 for pkgname in llvm-devel libxml2 e2fsprogs-libuuid \
   openssl bash gmake; do
    echo $pkgname
    pkg install $pkgname
 done
}

get_netbsd_deps()
{
 pkgin install clang libxml2 libuuid openssl bash patch gmake
}

get_opensuse_deps()
{
 zypper install llvm-clang-devel llvm-clang libclang \
  libxml2-devel libuuid-devel openssl bash patch make
}

get_mageia_deps()
{
 urpmi ctags
 urpmi task-c-devel task-c++-devel clang lib64llvm-devel llvm \
  libxml2-devel libuuid-devel openssl libopenssl-devel bash patch \
  make libstdc++-devel
}

get_debian_deps()
{
 apt-get install -y --force-yes clang llvm-dev libxml2-dev uuid-dev \
  libssl-dev bash patch make  tar xz-utils bzip2 gzip sed cpio
}

unknown()
{
 echo "Unknown system type. Please get dependencies by hand "
 echo "following README.md. Or update get_dependencies.sh and submit a patch."
}

if [ -e /etc/issue ]; then
 if [ "`grep -i ubuntu /etc/issue`" ]; then
  get_debian_deps
 elif [ "`grep -i debian /etc/issue`" ]; then
  get_debian_deps
 elif [ "`grep -i raspbian /etc/issue`" ]; then
  get_debian_deps
 elif [ "`grep -i mint /etc/issue`" ]; then
  get_debian_deps
 elif [ "`grep -i suse /etc/issue`" ]; then
  get_opensuse_deps
 elif [ "`grep -i fedora /etc/issue`" ]; then
  get_fedora_deps
 elif [ "`grep -i red.hat /etc/issue`" ]; then
  get_fedora_deps
 elif [ "`grep -i mageia /etc/issue`" ]; then
  get_mageia_deps
 else
  unknown
 fi
elif [ "`uname | grep -i freebsd `" ]; then
 get_freebsd_deps
elif [ "`uname | grep -i netbsd`" ]; then
 get_netbsd_deps
else
 unknown
fi


