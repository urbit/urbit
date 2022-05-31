load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")

def versioned_http_archive(name, version, **kwargs):
    if kwargs.get("urls") != None:
        for index, url in enumerate(kwargs["urls"]):
            kwargs["urls"][index] = url.format(version = version)

    if kwargs.get("url") != None:
        kwargs["url"] = kwargs["url"].format(version = version)

    if kwargs.get("strip_prefix") != None:
        kwargs["strip_prefix"] = kwargs["strip_prefix"].format(version = version)

    maybe(
        http_archive,
        name,
        **kwargs
    )
