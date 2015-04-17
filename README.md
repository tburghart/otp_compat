OTP Compatibility Modules
=========================

This repository exists to provide a small dependency that can be included in
[Rebar](https://github.com/rebar/rebar)-based Erlang/OTP projects to access
features in a version-independent manner.

The general philosophy is to provide a common API across versions. For example,
as of OTP-17 the `dict` type has moved to the `dict` namespace and is
deprecated in the global namespace. Using this package, you can use that type
as `dict_t` regardless of the OTP version you're building with.

_Ideally, I'll be able to work out the details of making the types available
dynamically at runtime based on the running OTP release before OTP-18 (which
removes them from the global namespace entirely) sees wide adoption, but that
code's not ready for prime time quite yet. In the interim, code compiled with
OTP-R16 or earlier *may not* run properly of OTP-18, and code compiled with
OTP-17 or later *may not* run properly on OTP-R16 or earlier._

How to Use It
-------------

Include the following entries in your `rebar.config` file:

```erlang
{erl_opts, [
    . . .
    {platform_define, "^[1-9][0-9]+", namespaced_types}
    . . .
]}.

{deps, [
    . . .
    {otp_compat, ".*", {git, "git://github.com/tburghart/otp_compat.git"}, {branch, "master"}}
    . . .
]}.
```
_Do *not* use any branch other than `master` unless you want much pain and
suffering!_

Then, in your Erlang source, include the following line to make the target
types accessible as `typename_t` _(see below)_.

```erlang
-include("otp_compat/include/otp_compat.hrl").
```

The `_t` suffix was chosen __not__ because I want to make your Erlang code
look like C, but because it seemed like a pattern that _a)_ would be easy to
remember and _b)_ would be unlikely to conflict with existing Erlang code,
which generally eschews C-like conventions.

The Types
---------

The macro `?NAMESPACED_TYPES_LIST` is defined as a list of the types declared
in the `otp_compat.hrl` file. As it seems likely that the list will be pretty
static, they are reproduced here:

* `array_t/0`
* `array_t/1`
* `dict_t/0`
* `dict_t/2`
* `digraph_t/0`
* `gb_set_t/0`
* `gb_set_t/1`
* `gb_tree_t/0`
* `gb_tree_t/2`
* `queue_t/0`
* `queue_t/1`
* `set_t/0`
* `set_t/1`
* `tid_t/0`

Functions
---------

Along with type definitions, a short list of (possibly helpful) functions
are exposed. The public API documentation is generated by running `make docs`.
Generally, the most interesting of these functions to users not rummaging
around in the weeds will be `otp_compat:otp_version()`, which returns the
current OTP major release as an integer, saving you the annoyance of parsing
the variety of formats returned by `erlang:system_info(otp_release)`.

Issues
------

Dialyzer prior to R16 will fail due to what it perceives to be duplicate
type definitions for parameterized types. The Erlang compiler has no such
problem, and since the target audience for this package is developers using
newer, not older, versions of Erlang, I've chosen not to bother adding more
macros to get around it.



_More to Come_ ...
