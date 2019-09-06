ver=6500f622dfff11dbe314b682e8b206f352f618c2

dir=resources/linux
mkdir -p $dir
( cd $dir; tar xfz ../../../../release/urbit-linux64-$ver.tgz )

dir=resources/mac
mkdir -p $dir
( cd $dir; tar xfz ../../../../release/urbit-darwin-$ver.tgz )
