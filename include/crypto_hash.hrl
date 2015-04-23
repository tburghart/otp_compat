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

-ifndef(OTP_COMPAT_CRYPTO_HASH_HRL_INCLUDED).
-define(OTP_COMPAT_CRYPTO_HASH_HRL_INCLUDED, true).

-ifdef(have_otp_crypto_hash).
%
% Starting in R16, hashing operations move into common crypto:hash
% functions.
%
-define(crypto_hash_sha(Data), crypto:hash(sha, Data)).
-define(crypto_hash_md5(Data), crypto:hash(md5, Data)).

-else.  % not have_otp_crypto_hash
%
% Before R16, OTP had distinct hash functions per algorithm.
%
-define(crypto_hash_sha(Data), crypto:sha(Data)).
-define(crypto_hash_md5(Data), crypto:md5(Data)).

-endif. % have_otp_crypto_hash

-endif. % OTP_COMPAT_CRYPTO_HASH_HRL_INCLUDED
