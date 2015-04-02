OTP Compatibility Modules
=========================

This repository exists to provide a small dependency that can be included in
[Rebar](github.com/rebar/rebar)-based Erlang/OTP projects to access features
in a version-independent manner.

The general philosophy is to provide a common API across versions. For example,
as of OTP-17 the `dict` operations have moved fromthe `erlang` namespace to
`dict`. Using this library, you can access those operations in the `dict`
namespace regardless of their placement in the running OTP instance.

How to Use It
-------------

Include the following entries in your `rebar.config` file:

```erlang
{erl_opts, [
    . . .
    {platform_define, "^[0-9]+", namespaced_types}
    . . .
]}.

{deps, [
    . . .
    {otp_compat, ".*", {git, "https://github.com/tburghart/otp_compat.git"}, {branch, "master"}}
    . . .
]}.
```

_More to Come_ ...
