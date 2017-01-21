source $stdenv/setup

cat > $out <<EOF
set(CMAKE_SYSTEM_NAME ${cmake_system_name})
set(CMAKE_C_COMPILER ${host}-gcc)
set(CMAKE_CXX_COMPILER ${host}-g++)
set(CMAKE_RC_COMPILER ${host}-windres)
EOF
