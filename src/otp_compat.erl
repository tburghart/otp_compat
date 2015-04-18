%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%
%%  @doc    The {@module} module provides operations to initialize the
%%          package and query runtime capabilities.
%%
-module(otp_compat).

-include("otp_compat.hrl").

%%======================================================================
%%  Public API
%%======================================================================

-export([
    init/0,
    otp_version/0,
    get_mapped_type/1,
    is_mapped_type/1,
    is_type_mapped/1,
    types_mapped/0
]).

-export_type([
    namespace/0,
    typename/0, otp_type/0, loc_type/0,
    typespec/0, typemap/0
]).

%%======================================================================
%%  Types
%%======================================================================

-type namespace() :: module().
-type typename() :: atom().
-type otp_type() :: typename().
-type loc_type() :: typename().
-type typespec() :: {typename(), arity()}.
-type typemap() :: {otp_type(), arity(), loc_type()}.

%%======================================================================
%%  API functions
%%======================================================================

-spec init() -> ok.
%%
%%  @doc    Forces initialization of the {@module} package.
%%
%%  Any operation within this module forces initialization by making sure
%%  the module is loaded. This one does nothing else.
%%
init() ->
    ok.

-spec otp_version() -> pos_integer().
%%
%%  @doc    Retrieve the major version of the current ERTS as an integer.
%%
%%  Prior to OTP-17, runtime versions included major (R<i>nn</i>) and minor
%%  ([AB]<i>nn</i>) components. From OTP-17 onward, the system only reports
%%  its major version. Since that's all you should need anyway, assuming there
%%  are no changes to the public API across minor versions, returning a
%%  single integer provides the most consistent (and useable) information.
%%
otp_version() ->
    otp_version(erlang:system_info(otp_release)).

-spec get_mapped_type(OtpType :: typespec()) -> typespec() | false.
%%
%%  @doc    Returns the type that the specified OTP type (not namespace
%%          qualified) is mapped to by this package.
%%
get_mapped_type(OtpType) ->
    type_find_left(OtpType, types_mapped()).

-spec is_mapped_type(MappedType :: typespec()) -> boolean().
%%
%%  @doc    Reports whether the specified Mapped type is included in this
%%          package.
%%
is_mapped_type(MappedType) ->
    lists:any(fun(MapElem) ->
        type_match_right(MappedType, MapElem)
    end, types_mapped()).

-spec is_type_mapped(OtpType :: typespec()) -> boolean().
%%
%%  @doc    Reports whether the specified OTP type (not namespace qualified)
%%          is mapped by this package.
%%
is_type_mapped(OtpType) ->
    lists:any(fun(MapElem) ->
        type_match_left(OtpType, MapElem)
    end, types_mapped()).

-spec types_mapped() -> [typemap()].
%%
%%  @doc    Retrieves the list of type mappings that are included in this
%%          package.
%%
types_mapped() ->
    ?NAMESPACED_TYPE_MAPS.

%%  @end
%%======================================================================
%%  Internal functions
%%======================================================================

-spec otp_version(Rel :: string()) -> pos_integer().
%
% Starting with OTP 17, the Release string is just the major version.
% Before that, the string starts with 'R', followed by some decimal digits,
% usually followed by some non-decimal characters. string:to_integer will
% conveniently just parse until it hits a non-digit, and we don't care what
% that is.
%
otp_version([$R | Rel]) ->
    {Ver, _} = string:to_integer(Rel),
    Ver;
otp_version(Rel) ->
    {Ver, _} = string:to_integer(Rel),
    Ver.

-spec type_find_left(typespec(), [typemap()]) -> typespec() | false.
type_find_left(_, []) ->
    false;
type_find_left({LeftType, Arity}, [{LeftType, Arity, RightType} | _]) ->
    {RightType, Arity};
type_find_left(Type, [_ | Types]) ->
    type_find_left(Type, Types).

-spec type_match_left(typespec(), typemap()) -> boolean().
type_match_left({Type, Arity}, {Type, Arity, _}) ->
    true;
type_match_left(_, _) ->
    false.

-spec type_match_right(typespec(), typemap()) -> boolean().
type_match_right({Type, Arity}, {_, Arity, Type}) ->
    true;
type_match_right(_, _) ->
    false.
