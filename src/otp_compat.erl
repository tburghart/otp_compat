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
%%  Once loaded, this package provides a subset of the namespace-qualified
%%  types and operations that were moved from the `erlang' module to their
%%  own modules as of OTP-17. Note that the entire new APIs are not provided,
%%  only those elements that were available through OTP-R16.
%%
-module(otp_compat).

%-on_load(init_hooks/0).
-compile([export_all]).

%%======================================================================
%%  Public API
%%======================================================================

-export([
    init/0,
    init_hooks/0,
    otp_version/0,
    namespaces/0,
    supports/1
]).

%%======================================================================
%%  Types
%%======================================================================

-type token() :: erl_scan:token().
-type tokens() :: [token()].
-type dot_token() :: {dot, pos_integer()}.
-type statement() :: [token() | dot_token()].
-type statements() :: [statement()].
-type abstract() :: erl_parse:abstract_form().
-type object() :: binary().

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

-spec namespaces() -> [module()].
%%
%%  @doc    Retrieves the list of namespaces (modules) that are assured of
%%          being present as a result of this module being loaded.
%%
%%  This is the complete set of namespaces for which {@link supports/1}
%%  returns `true'.
%%
namespaces() ->
    [Ns || {Ns, _Src} <- module_source(otp_version())].

-spec supports(Namespace :: module()) -> boolean().
%%
%%  @doc    Reports whether the specified namespace is assured to be present
%%          as a result of this module being loaded.
%%
%%  Invoking this operation with any item returned from {@link namespaces/0}
%%  returns `true'; all other inputs return `false'.
%%
supports(Namespace) ->
    lists:any(
        fun({Ns,_Src}) ->
            Ns == Namespace
        end,
        module_source(otp_version())).

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

%%======================================================================
%%  This is where the magic happens
%%======================================================================

-spec init_hooks() -> ok | no_return().
%
% Invoked when the module is loaded. 
%
init_hooks() ->
    maybe_insert(module_source(otp_version())).

-spec maybe_insert([{module(), string()}]) -> ok | no_return().
maybe_insert([]) ->
    ok;
maybe_insert([{Module, Source} | Rest]) ->
    case code:is_loaded(Module) of
        false ->
            compile_and_load(Module, Source),
            maybe_insert(Rest);
        _ ->
            maybe_insert(Rest)
    end.

-spec compile_and_load(Module :: module(), Source :: string()) -> ok | no_return().
compile_and_load(Module, Source) ->
    Statements = source_to_statements(Module, Source),
    Abstract = statements_to_abstract(Statements),
    Object = abstract_to_object(Abstract),
    %
    % The docs for code:load_binary appear to lie - if you actually pass
    % something that looks like a filename as the second parameter, it'll
    % return a badarg error. Passing an atom gets past the check in
    % code_server.erl ... at least in where I've tried it so far.
    %
    case code:load_binary(Module, Module, Object) of
        {module, Module} ->
            ok;
        {error, What} ->
            erlang:error(What)
    end.

%
% Template of a scanned module statement.
%
% When matching the pattern, allow any value for the second argument, as the
% 'dot' token has been seen with a value other than '1'. When creating one,
% suplying a value of '1' seems to be just fine.
%
-define(MODULE_STATEMENT(M,N),
    [{'-', 1}, {atom, 1, module}, {'(', 1}, {atom, 1, M}, {')', 1}, {dot, N}]).

-spec source_to_statements(Module :: module(), Source :: string()) ->
    statements() | no_return().
%%
%%  @doc    Tokenize the supplied string into statements.
%%
%%  The first statement in the result is a generated module definition, if
%%  the Source contains a module definition that does not match the one that
%%  would be generated for the specified Module, an error is returned.
%%
source_to_statements(Module, Source) ->
    Tokens = source_to_tokens(Source),
    Statements = tokens_to_statements(Tokens),
    case module_statement(Statements) of
        undefined ->
            [?MODULE_STATEMENT(Module, 1)] ++ Statements;
        Module ->
            Statements;
        Conflict ->
            erlang:error({mod_conflict, Module, Conflict})
    end.

-spec source_to_tokens(Source :: string()) -> tokens() | no_return().
source_to_tokens(Source) ->
    case erl_scan:string(Source) of
        {ok, Tokens, _End} ->
            Tokens;
        {error, What, Where} ->
            erlang:error({What, Where})
    end.

-spec module_statement(statements()) -> module() | undefined.
module_statement([]) ->
    undefined;
module_statement([Statement | Statements]) ->
    case Statement of
        ?MODULE_STATEMENT(Module, _) ->
            Module;
        _ ->
            module_statement(Statements)
    end.

-spec tokens_to_statements(Tokens :: tokens()) -> statements() | no_return().
tokens_to_statements(Tokens) ->
    tokens_to_statements(Tokens, {[], []}).
    
-spec tokens_to_statements(Tokens :: tokens(),
    {Incomplete :: tokens(), Statements :: statements()}) ->
    statements() | no_return().
tokens_to_statements([], {[], Statements}) ->
    Statements;
tokens_to_statements([], {Incomplete, Statements}) ->
    erlang:error({Incomplete, Statements});
tokens_to_statements([Token | Tokens], {Incomplete, Statements}) ->
    Acc = Incomplete ++ [Token],
    case Token of
        {dot, _N} ->
            tokens_to_statements(Tokens, {[], Statements ++ [Acc]});
        _ ->
            tokens_to_statements(Tokens, {Acc, Statements})
    end.

-spec statements_to_abstract(Statements :: statements()) ->
    abstract() | no_return().
%%
%%  @doc    Parse statements into abstract form.
%%
statements_to_abstract(Statements) ->
    statements_to_abstract(Statements, []).

statements_to_abstract([], Abstract) ->
    Abstract;
statements_to_abstract([Statement | Statements], Abstract) ->
    case erl_parse:parse_form(Statement) of
        {ok, AbsForm} ->
            statements_to_abstract(Statements, Abstract ++ [AbsForm]);
        {error, ParseErr} ->
            erlang:error(ParseErr)
    end.

-define(COMPILE_OPTS, [binary, no_auto_import, no_line_info, return_errors]).
% -define(COMPILE_OPTS, [binary, debug_info, warnings_as_errors, report]).
% -define(COMPILE_OPTS, [binary, warnings_as_errors, return_errors]).

-spec abstract_to_object(Abstract :: abstract()) -> object() | no_return().
%%
%%  @doc    Compile abstract form into a loadable binary.
%%
abstract_to_object(Abstract) ->
    case compile:forms(Abstract, ?COMPILE_OPTS) of
        {ok, _Module, Beam} ->
            Beam;
        {ok, _Module, _Beam, Warnings} ->
            erlang:error({warnings, Warnings});
        {error, Errors, Warnings} ->
            erlang:error({Errors, Warnings})
    end.

-spec module_source(OtpVer :: pos_integer()) -> [{module(), string()}].
%
% The goal is to provide reasonable facsimilies of the following OTP-17+
% modules:
%
%   array, dict, digraph, gb_set, gb_tree, queue, sets
%
module_source(_OtpVer) ->
[{
sample_module,
"-export([sample_func/0]).
-export_type([sample_type/0]).
-type sample_type() :: term().
sample_func() ->
    ok.
"
},{
simple_module,
"-export([simple_func/0]).
simple_func() ->
    ok.
"
}].

%%  @end
%%======================================================================
%%  EUnit Tests
%%======================================================================
-ifdef(TEST).


-endif. % TEST

