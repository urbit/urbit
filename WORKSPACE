workspace(name = "urbit")

load("//bazel:repo.bzl", "versioned_http_archive")

#
# Rules Repositories
#

versioned_http_archive(
    name = "rules_cc",
    sha256 = "19d93d9a54487343dec4fabccf9974a9a5e8749297a448d1f310459eeca5091e",
    strip_prefix = "rules_cc-{version}",
    url = "https://github.com/bazelbuild/rules_cc/archive/{version}.tar.gz",
    version = "8bb0eb5c5ccd96b91753bb112096bb6993d16d13",
)

versioned_http_archive(
    name = "rules_foreign_cc",
    sha256 = "fa3a6638443ac7c16e236c60f4405b09b26cf3008626da77eb03d7ec7aa60ea8",
    strip_prefix = "rules_foreign_cc-{version}",
    url = "https://github.com/bazelbuild/rules_foreign_cc/archive/{version}.tar.gz",
    version = "9e70029a5c0a910099e3b5978c30d9a8fae6dc91",
)

versioned_http_archive(
    name = "rules_python",
    sha256 = "",
    strip_prefix = "rules_python-{version}",
    url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/{version}.tar.gz",
    version = "0.8.1",
)

versioned_http_archive(
    name = "bazel-zig-cc",
    sha256 = "8f198ee98fa122d299b008db281329b80147d04fcdab03a55c31007b5edc8fdf",
    strip_prefix = "bazel-zig-cc-{version}",
    # patch_args = ["-p1"],
    # patches = ["//third_party:bazel-zig-cc.patch"],
    url = "https://git.sr.ht/~motiejus/bazel-zig-cc/archive/{version}.tar.gz",
    version = "v0.7.3",
)

#
# Rules Setup
#

load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")

# Avoid downloading/registering cmake and use one from the system (or flake.nix) instead.
rules_foreign_cc_dependencies(
    register_built_tools = False,
    register_default_tools = False,
    register_preinstalled_tools = True,
)

#
# Toolchains
#

load("@bazel-zig-cc//toolchain:defs.bzl", zig_toolchains = "toolchains")

zig_toolchains()

#
# Third Party Dependencies
#

versioned_http_archive(
    name = "zlib",
    build_file = "//third_party/zlib:BUILD.bazel",
    sha256 = "91844808532e5ce316b3c010929493c0244f3d37593afd6de04f71821d5136d9",
    strip_prefix = "zlib-{version}",
    url = "https://zlib.net/zlib-{version}.tar.gz",
    version = "1.2.12",
)

versioned_http_archive(
    name = "openssl",
    build_file = "//third_party/openssl:BUILD.bazel",
    sha256 = "ee0078adcef1de5f003c62c80cc96527721609c6f3bb42b7795df31f8b558c0b",
    strip_prefix = "openssl-{version}",
    url = "https://www.openssl.org/source/openssl-{version}.tar.gz",
    version = "3.0.3",
)

versioned_http_archive(
    name = "gmp",
    build_file = "//third_party/gmp:BUILD.bazel",
    sha256 = "fd4829912cddd12f84181c3451cc752be224643e87fac497b69edddadc49b4f2",
    strip_prefix = "gmp-{version}",
    url = "https://gmplib.org/download/gmp/gmp-{version}.tar.xz",
    version = "6.2.1",
)

versioned_http_archive(
    name = "secp256k1",
    build_file = "//third_party/secp256k1:BUILD.bazel",
    patch_args = ["-p1"],
    patches = ["//third_party/secp256k1:autogen-libtoolize.patch"],
    sha256 = "e5f49f896cff608d3f415bbdd0ed7483ab9f721e50be40fb149de48a41169f0f",
    strip_prefix = "secp256k1-{version}",
    url = "https://github.com/bitcoin-core/secp256k1/archive/{version}.tar.gz",
    version = "44c2452fd387f7ca604ab42d73746e7d3a44d8a2",
)

versioned_http_archive(
    name = "aes_siv",
    build_file = "//third_party/aes_siv:BUILD.bazel",
    sha256 = "1916a428dff480e06b09dc0fb1c9d849c048f838dc9b8d141452233b508f6bb1",
    strip_prefix = "libaes_siv-{version}",
    url = "https://github.com/dfoxfranke/libaes_siv/archive/{version}.tar.gz",
    version = "9681279cfaa6e6399bb7ca3afbbc27fc2e19df4b",
)

versioned_http_archive(
    name = "murmur3",
    build_file = "//third_party/murmur3:BUILD.bazel",
    sha256 = "c6c9a10c4588df747606525149a4a157c99719b448a75d4189a24302f7039661",
    strip_prefix = "murmur3-{version}",
    url = "https://github.com/urbit/murmur3/archive/{version}.tar.gz",
    version = "71a75d57ca4e7ca0f7fc2fd84abd93595b0624ca",
)

versioned_http_archive(
    name = "softfloat3",
    build_file = "//third_party/softfloat3:BUILD.bazel",
    patch_args = ["-p1"],
    patches = ["//third_party/softfloat3:make-install.patch"],
    sha256 = "f75944ae5068fb9ce264bf1a5d6f35bd212cd7b47c0b908f6d17ea10b0530400",
    strip_prefix = "berkeley-softfloat-3-{version}",
    url = "https://github.com/urbit/berkeley-softfloat-3/archive/{version}.tar.gz",
    version = "ec4c7e31b32e07aad80e52f65ff46ac6d6aad986",
)

versioned_http_archive(
    name = "uv",
    build_file = "//third_party/uv:BUILD.bazel",
    sha256 = "e91614e6dc2dd0bfdd140ceace49438882206b7a6fb00b8750914e67a9ed6d6b",
    strip_prefix = "libuv-{version}",
    url = "https://github.com/libuv/libuv/archive/refs/tags/v{version}.tar.gz",
    version = "1.44.1",
)

versioned_http_archive(
    name = "sigsegv",
    build_file = "//third_party/sigsegv:BUILD.bazel",
    sha256 = "cdac3941803364cf81a908499beb79c200ead60b6b5b40cad124fd1e06caa295",
    strip_prefix = "libsigsegv-{version}",
    url = "https://ftp.gnu.org/gnu/libsigsegv/libsigsegv-{version}.tar.gz",
    version = "2.14",
)

versioned_http_archive(
    name = "lmdb",
    build_file = "//third_party/lmdb:BUILD.bazel",
    sha256 = "22054926b426c66d8f2bc22071365df6e35f3aacf19ad943bc6167d4cae3bebb",
    strip_prefix = "lmdb-LMDB_{version}/libraries/liblmdb",
    url = "https://github.com/LMDB/lmdb/archive/refs/tags/LMDB_{version}.tar.gz",
    version = "0.9.29",
)

versioned_http_archive(
    name = "h2o",
    build_file = "//third_party/h2o:BUILD.bazel",
    patch_args = ["-p1"],
         patches = [
        "//third_party/h2o:configure-file-version.patch",
        "//third_party/h2o:remove-build-files.patch",
    ],
    sha256 = "e3f745a645fa7a4bf6629914b967e24e1522f65ea17c07d9d16d8d2223fc03c5",
    strip_prefix = "h2o-{version}",
    url = "https://github.com/h2o/h2o/archive/refs/tags/v{version}.tar.gz",
    version = "2.3.0-beta2",
)

versioned_http_archive(
    name = "nghttp2",
    build_file = "//third_party/nghttp2:BUILD.bazel",
    sha256 = "db98735b30f1586edf3212ada57f85feaceff483a1c0f1c1c8285abcf37e3444",
    strip_prefix = "nghttp2-{version}",
    url = "https://github.com/nghttp2/nghttp2/archive/refs/tags/v{version}.tar.gz",
    version = "1.47.0",
)

versioned_http_archive(
    name = "ares",
    build_file = "//third_party/ares:BUILD.bazel",
    sha256 = "414872549eec4e221b576693fdc9c9bce44ff794d0f1f06f2515b56a7f6ec9c9",
    strip_prefix = "c-ares-cares-{version}",
    url = "https://github.com/c-ares/c-ares/archive/refs/tags/cares-{version}.tar.gz",
    version = "1_18_1",
)

versioned_http_archive(
    name = "curl",
    build_file = "//third_party/curl:BUILD.bazel",
    sha256 = "93fb2cd4b880656b4e8589c912a9fd092750166d555166370247f09d18f5d0c0",
    strip_prefix = "curl-{version}",
    url = "https://curl.haxx.se/download/curl-{version}.tar.gz",
    version = "7.83.1",
)
