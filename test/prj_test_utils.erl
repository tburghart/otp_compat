%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 T. R. Burghart.
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

-module(prj_test_utils).
-ifdef(TEST).

-export([
    otp_version/0,
    dialyzer_filepath/1,
    project_dir/0, plt_file/0,
    temp_dir/0, temp_file/0, temp_file/1,
    cleanup/0
]).

-include_lib("eunit/include/eunit.hrl").

-define(StateKey(K),    {?MODULE, K}).
-define(get_val(K),     erlang:get(?StateKey(K))).
-define(set_val(K,V),   erlang:put(?StateKey(K), V)).

%%======================================================================
%%  API functions
%%======================================================================

-spec otp_version() -> pos_integer().
otp_version() ->
    case ?get_val(otp_version) of
        undefined ->
            NewVal = parse_otp_version(erlang:system_info(otp_release)),
            ?set_val(otp_version, NewVal),
            NewVal;
        Val ->
            Val
    end.

-spec project_dir() -> file:filename_all() | false.
project_dir() ->
    case ?get_val(project_dir) of
        undefined ->
            NewVal = locate_project_dir(),
            ?set_val(project_dir, NewVal),
            NewVal;
        Val ->
            Val
    end.

-spec plt_file() -> file:filename_all() | false.
plt_file() ->
    case ?get_val(plt_file) of
        undefined ->
            NewVal = locate_plt_file(),
            ?set_val(plt_file, NewVal),
            NewVal;
        Val ->
            Val
    end.

-spec temp_dir() -> file:filename_all().
temp_dir() ->
    case ?get_val(temp_dir) of
        undefined ->
            NewVal = locate_temp_dir(),
            ?set_val(temp_dir, NewVal),
            NewVal;
        Val ->
            Val
    end.

-spec temp_file() -> file:filename_all().
temp_file() ->
    {_, _, N} = erlang:now(),
    add_temp_file(temp_file(temp_dir(), N)).

-spec temp_file(file:filename_all()) -> file:filename_all().
temp_file(FileName) ->
    add_temp_file(filename:join(temp_dir(), FileName)).

-spec dialyzer_filepath(Type :: atom() | string()) -> file:filename_all().
dialyzer_filepath(Type) ->
    BN = case ?get_val(dialyzer_basepath) of
        undefined ->
            %
            % MUST match the filename pattern used in the Makefile!
            %
            NewVal = filename:join(project_dir(), io_lib:format(
                        "dialyzer_~s", [erlang:system_info(otp_release)])),
            ?set_val(dialyzer_basepath, NewVal),
            NewVal;
        Val ->
            Val
    end,
    lists:flatten(io_lib:format("~s.~s", [BN, Type])).

-spec cleanup() -> ok.
cleanup() ->
    case ?get_val(temp_files) of
        undefined ->
            ok;
        TempFiles ->
            ?set_val(temp_files, undefined),
            lists:foreach(fun file:delete/1, TempFiles)
    end.

%%======================================================================
%%  Internal functions
%%======================================================================

parse_otp_version([_Nhi, _Nlo] = Rev) ->
    erlang:list_to_integer(Rev);
parse_otp_version([$R, Nhi, Nlo | _]) ->
    parse_otp_version([Nhi, Nlo]).

locate_project_dir() ->
    % in the environment, maybe?
    case os:getenv("PRJDIR") of
        false ->
            crawl_up(fun is_project_dir/1);
        Path ->
            case filelib:is_dir(Path) of
                true ->
                    Path;
                _ ->
                    crawl_up(fun is_project_dir/1)
            end
    end.

locate_plt_file() ->
    % if it's in the environment, don't search
    FP = case os:getenv("PLT") of
        false ->
            dialyzer_filepath(plt);
        Path ->
            Path
    end,
    case filelib:is_regular(FP) of
        true ->
            FP;
        _ ->
            false
    end.

locate_temp_dir() ->
    locate_temp_dir(["TMPDIR", "TMP", "TEMP"]).

locate_temp_dir([]) ->
    case filelib:is_dir("/tmp") of
        true ->
            "/tmp";
        _ ->
            BD = case project_dir() of
                false ->
                    {ok, CWD} = file:get_cwd(),
                    CWD;
                Dir ->
                    Dir
            end,
            TD = filename:join(BD, "tmp"),
            case filelib:is_dir(TD) of
                true ->
                    TD;
                _ ->
                    BD
            end
    end;
locate_temp_dir([EV | EVs]) ->
    case os:getenv(EV) of
        false ->
            locate_temp_dir(EVs);
        Dir ->
            case filelib:is_dir(Dir) of
                true ->
                    Dir;
                _ ->
                    locate_temp_dir(EVs)
            end
    end.

add_temp_file(FP) ->
    FL = case ?get_val(temp_files) of
        undefined ->
            [];
        Val ->
            Val
    end,
    ?set_val(temp_files, FL ++ [FP]),
    FP.

temp_file(Dir, Next) ->
    FN = filename:join(Dir, lists:flatten(io_lib:format("~b.tmp", [Next]))),
    case filelib:is_file(FN) of
        true ->
            temp_file(Dir, Next + 1);
        _ ->
            case file:write_file(FN, "", [exclusive]) of
                ok ->
                    FN;
                _ ->
                    temp_file(Dir, Next + 1)
            end
    end.

is_project_dir(Dir) ->
    test_files(Dir, ["Makefile", "rebar.config"], 0) > 1.

test_files(_Dir, [], Count) ->
    Count;
test_files(Dir, [FN | FNs], Count) ->
    case filelib:is_regular(filename:join(Dir, FN)) of
        true ->
            test_files(Dir, FNs, Count + 1);
        _ ->
            test_files(Dir, FNs, Count)
    end.

crawl_up(Predicate) ->
    {ok, CWD} = file:get_cwd(),
    crawl_up(CWD, Predicate).

crawl_up(Dir, Predicate) ->
    case Predicate(Dir) of
        true ->
            Dir;
        _ ->
            UP = filename:dirname(Dir),
            case UP == Dir of
                true ->
                    false;  % at root directory
                _ ->
                    crawl_up(UP, Predicate)
            end
    end.

-endif. % TEST
