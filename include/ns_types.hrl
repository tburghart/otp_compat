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

-ifndef(OTP_COMPAT_NS_TYPES_HRL_INCLUDED).
-define(OTP_COMPAT_NS_TYPES_HRL_INCLUDED, true).

%
% List of mapped types, suitable for use in an -export_types statement.
%
-define(NAMESPACED_TYPES_LIST, [
    array_t/0, array_t/1,
    dict_t/0, dict_t/2,
    digraph_t/0,
    gb_set_t/0, gb_set_t/1,
    gb_tree_t/0, gb_tree_t/2,
    queue_t/0, queue_t/1,
    set_t/0, set_t/1,
    tid_t/0
]).

%
% This is the list used by the otp_compat type mapping reporting functions.
% The structure is
%
% {
%   unqualified-otp-type    ::  atom(),
%   arity                   ::  non_neg_integer(),
%   unqualified-mapped-type ::  atom()
% }
%
-define(NAMESPACED_TYPE_MAPS, [
    {array, 0, array_t}, {array, 1, array_t},
    {dict, 0, dict_t}, {dict, 2, dict_t},
    {digraph, 0, digraph_t},
    {gb_set, 0, gb_set_t}, {gb_set, 1, gb_set_t},
    {gb_tree, 0, gb_tree_t}, {gb_tree, 2, gb_tree_t},
    {queue, 0, queue_t}, {queue, 1, queue_t},
    {set, 0, set_t}, {set, 1, set_t},
    {tid, 0, tid_t}
]).

%
% Statically mapped types.
%
% I'm working on a better way to do this, so the types will be valid when
% compiled beam files are deployed "across the boundary" from where they
% were built, but this works for now.
%
-ifndef(no_otp_namespaced_types).
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

-else.  % no_otp_namespaced_types
%
% Type mapping prior to OTP-17.
% The funky parameter syntax (_X) works around some, but not all, gotchas
% in how the compiler and friends handle type parameterization in R16-.
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

-endif. % no_otp_namespaced_types

-endif. % OTP_COMPAT_NS_TYPES_HRL_INCLUDED
