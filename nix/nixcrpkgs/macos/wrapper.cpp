#include <vector>
#include <string>
#include <iostream>
#include <cstring>
#include <cstdlib>
#include <unistd.h>

int do_exec(const std::string & compiler_name,
  const std::vector<std::string> & args)
{
  char ** exec_args = new char *[args.size() + 1];
  size_t i = 0;
  for (const std::string & arg : args)
  {
    exec_args[i++] = (char *)arg.c_str();
  }
  exec_args[i] = nullptr;

  execvp(compiler_name.c_str(), exec_args);

  int result = errno;
  std::cerr << "execvp failed: " << compiler_name << ": "
            << strerror(result) << std::endl;
  return 1;
}

int compiler_main(int argc, char ** argv,
  const std::string & compiler_name)
{
  std::vector<std::string> args;

  args.push_back(compiler_name);

  args.push_back("-target");
  args.push_back(WRAPPER_HOST);

  args.push_back("-mmacosx-version-min=" WRAPPER_OS_VERSION_MIN);

  // The ld64 linker will just assume sdk_version is the same as
  // macosx-version-min if we don't supply it.  That probably will not
  // do any harm.
  // args.push_back("-Wl,-sdk_version," WRAPPER_SDK_VERSION);

  // Suppress warnings about the -Wl arguments not being used when we're just
  // compiling and not linking.
  args.push_back("-Wno-unused-command-line-argument");

  args.push_back("--sysroot");
  args.push_back(WRAPPER_SDK_PATH);

  // Causes clang to pass -demangle, -no_deduplicate, and other
  // options that could be useful.  Version 274.2 is the version number used here:
  // https://github.com/tpoechtrager/osxcross/blob/474f359/build.sh#L140
  if (WRAPPER_LINKER_VERSION[0])
  {
    args.push_back("-mlinker-version=" WRAPPER_LINKER_VERSION);
  }

  if (compiler_name == "clang++")
  {
    args.push_back("-stdlib=libc++");
    args.push_back("-cxx-isystem");
    args.push_back(WRAPPER_SDK_PATH "/usr/include/c++");
  }

  for (int i = 1; i < argc; ++i)
  {
    args.push_back(argv[i]);
  }

  return do_exec(compiler_name, args);
}

int c_compiler_main(int argc, char ** argv)
{
  return compiler_main(argc, argv, "clang");
}

int cxx_compiler_main(int argc, char ** argv)
{
  return compiler_main(argc, argv, "clang++");
}

int wrapper_main(int argc, char ** argv)
{
  std::cout <<
    "host: " WRAPPER_HOST "\n"
    "path: " WRAPPER_PATH "\n"
    "sdk_path: " WRAPPER_SDK_PATH "\n";
  return 0;
}

struct {
  const char * name;
  int (*main_func)(int argc, char ** argv);
} prgms[] = {
  { WRAPPER_HOST "-gcc", c_compiler_main },
  { WRAPPER_HOST "-cc", c_compiler_main },
  { WRAPPER_HOST "-clang", c_compiler_main },
  { WRAPPER_HOST "-g++", cxx_compiler_main },
  { WRAPPER_HOST "-c++", cxx_compiler_main },
  { WRAPPER_HOST "-clang++", cxx_compiler_main },
  { WRAPPER_HOST "-wrapper", wrapper_main },
  { nullptr, nullptr },
};

const char * get_program_name(const char * path)
{
  const char * p = strrchr(path, '/');
  if (p) { path = p + 1; }
  return path;
}

int main(int argc, char ** argv)
{
  // We only want this wrapper and the compiler it invokes to access a certain
  // set of tools that are determined at build time.  Ignore whatever is on the
  // user's path and use the path specified by our Nix expression instead.
  int result = setenv("PATH", WRAPPER_PATH, 1);
  if (result)
  {
    std::cerr << "wrapper failed to set PATH" << std::endl;
    return 1;
  }

  result = setenv("COMPILER_RT_PATH", WRAPPER_COMPILER_RT_PATH, 1);
  if (result)
  {
    std::cerr << "wrapper failed to set COMPILER_RT_PATH" << std::endl;
    return 1;
  }

  std::string program_name = get_program_name(argv[0]);

  for (auto * p = prgms; p->name; p++)
  {
    if (program_name == p->name)
    {
      return p->main_func(argc, argv);
    }
  }

  std::cerr << "compiler wrapper invoked with unknown program name: "
            << argv[0] << std::endl;
  return 1;
}
