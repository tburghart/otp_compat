OTP Compatibility Goodies
=========================

This repository exists to provide a small dependency that can be included in
[Rebar](https://github.com/rebar/rebar)-based Erlang/OTP projects to access
features in a version-independent manner. Obviously, anything you can do in
`rebar`, you can do without it, but ___my___ goal is ease-of-use in `rebar`.

The general philosophy is to provide a common API across versions, starting
with _(but not limited to)_ the global types that have moved into namespaces
as of OTP-17. For example, the `dict` type has moved to the `dict` namespace
and is deprecated in the global namespace. Using this package, you can use
that type as `dict_t` regardless of the OTP version you're building with
_(see [Caveats](#Ramblings), below)_.

<a name="Copyright">Copyright and Ownership</a>
-----------------------------------------------

This work is published under an
[Apache](http://www.apache.org/licenses/LICENSE-2.0) license.
The owner of the copyright ___may___ change, the license terms will not.

<a name="Using">How to Use It</a>
---------------------------------

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
_Do **not** use any branch other than_ `master` _unless you want much pain
and suffering ... you have been warned!_

Then, in your Erlang source, include the following line to make the target
types accessible as `typename_t` _(see below)_.

```erlang
-include_lib("otp_compat/include/ns_types_.hrl").
```

The `_t` suffix was chosen **not** because I want to make your Erlang code
look like C, but because it seemed like a pattern that would be _a)_ easy to
remember and _b)_ unlikely to conflict with existing Erlang code, which
generally eschews C-like conventions.

<a name="MappedTypes">The Types</a>
-----------------------------------

The macro `?NAMESPACED_TYPES_LIST` is defined as a list of the types declared
in the `ns_types.hrl` file in a form suitable for use in `-export_type()`.
As it seems likely that the list will be pretty static, they are reproduced
here:

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

<a name="InfoFuncs">Functions</a>
---------------------------------

Along with type definitions, a short list of (possibly helpful) functions
are exposed. The public API documentation is generated by running `make docs`.
Generally, the most interesting of these functions to users not rummaging
around in the weeds will be `otp_compat:otp_version()`, which returns the
current OTP major release as an integer, saving you the annoyance of parsing
the variety of formats returned by `erlang:system_info(otp_release)`.

<a name="Issues">Issues</a>
---------------------------

Dialyzer prior to R16 will fail due to what it perceives to be duplicate
type definitions for parameterized types. The Erlang compiler has no such
problem, and since the target audience for this package is developers using
newer, not older, versions of Erlang, I've chosen not to bother adding more
macros to get around it.

If you come across issues other than the above, please **do** file them, and
I'll have a look.

<a name="Ramblings">Under the Hood</a>
--------------------------------------

Ideally, I'll be able to work out the details of making the types available
dynamically at runtime based on the running OTP release before OTP-18 (which
removes them from the global namespace entirely) sees wide adoption, but that
code's not ready for prime time quite yet. In the interim, code compiled with
OTP-R16 or earlier **may not** work properly on OTP-18, and code compiled
with OTP-17 or later **may not** work properly on OTP-R16 or earlier.

Unless you're rooting about in beam file internals, you're unlikely to stumble
across any problems with the current static compile-time typing, even across
OTP versions. Dialyzer, however, does just that, so if you compile a beam on
one side of the type change boundary and run dialyzer from the other side of
the boundary on it, expect copious warnings _(see below about outright dialyzer
breakage in older versions)_. Realistically, dialyzer is generally a build
step, so it should be in sync, but if you're running it against sources with
the `--src` option be sure `namespaced_types` is defined (or not) as it is
for compilation.

I'm also leaning toward making the whole thing a `parse_tranform`, which will
allow me to add new capabilities without changing target source code at all.
Among those capabilities would be options selecting whether the result of
the transformation should be targetted at only the OTP version on which the
compilation is performed (no performance impact on mapped functions), or on
any OTP version (some performance impact on mapped functions).
The possibilities are not technically endless, but there's far more that can
be done transforming the intermediate forms than I can do with static macros
and type definitions.

This is ___very much___ a work in progress, _More to Come_ ...
