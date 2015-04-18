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

-module(otp_compat_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("otp_compat.hrl").

init_test() ->
    ?assertEqual(ok, otp_compat:init()).

type_map_test() ->
    lists:foreach(fun({OtpType, Arity, LocType}) ->
        ?assert(otp_compat:is_type_mapped({OtpType, Arity})),
        ?assert(otp_compat:is_mapped_type({LocType, Arity}))
    end, otp_compat:types_mapped()),
    ?assertNot(otp_compat:is_type_mapped({?MODULE, 0})).

version_test() ->
    ?assertEqual(prj_test_utils:otp_version(), otp_compat:otp_version()).

-endif. % TEST
