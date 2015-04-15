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

-ifndef(OTP_COMPAT_HRL_INCLUDED).
-define(OTP_COMPAT_HRL_INCLUDED, true).

-define(NAMESPACED_TYPES_LIST, [
    array_t/0, array_t/1, dict_t/0, dict_t/2, digraph_t/0, gb_set_t/0,
    gb_set_t/1, gb_tree_t/0, gb_tree_t/2, queue_t/0, queue_t/1, set_t/0,
    set_t/1, tid_t/0
]).

%
% Statically mapped types. These depend on whether the 'namespaced_types'
% macro is defined in the compilation environment.
%
% I'm working on a better way to do this, so the types will be valid when
% compiled beam files are deployed "across the boundary" from where they
% were built, but this works for now.
%
-ifdef(namespaced_types).
%
% Type mapping for OTP-17+
%
-type array_t()         :: array:array().
-type array_t(T)        :: array:array(T).
-type dict_t()          :: dict:dict().
-type dict_t(K,V)       :: dict:dict(K,V).
-type digraph_t()       :: digraph:graph().
-type gb_set_t()        :: gb_set:set().
-type gb_set_t(T)       :: gb_set:set(T).
-type gb_tree_t()       :: gb_tree:tree().
-type gb_tree_t(K,V)    :: gb_tree:tree(K,V).
-type queue_t()         :: queue:queue().
-type queue_t(T)        :: queue:queue(T).
-type set_t()           :: sets:set().
-type set_t(T)          :: sets:set(T).
-type tid_t()           :: ets:tid().

-else.  % not namespaced_types
%
% Type mapping prior to OTP-17
%
-type array_t()         :: array().
-type array_t(_T)       :: array().
-type dict_t()          :: dict().
-type dict_t(_K,_V)     :: dict().
-type digraph_t()       :: digraph().
-type gb_set_t()        :: gb_set().
-type gb_set_t(_T)      :: gb_set().
-type gb_tree_t()       :: gb_tree().
-type gb_tree_t(_K,_V)  :: gb_tree().
-type queue_t()         :: queue().
-type queue_t(_T)       :: queue().
-type set_t()           :: set().
-type set_t(_T)         :: set().
-type tid_t()           :: ets:tid().

-endif. % namespaced_types

-endif. % OTP_COMPAT_HRL_INCLUDED
