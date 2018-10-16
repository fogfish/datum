%%
%%   Copyright (c) 2016, Dmitry Kolesnikov
%%   All Rights Reserved.
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%
-module(typecast_SUITE).

-export([all/0]).
-export([
   cast_int/1,
   cast_float/1,
   cast_string/1,
   cast_unicode_string/1,
   cast_characters/1,
   cast_unicode_characters/1,
   cast_atom/1,
   cast_new_atom/1,
   cast_hexdec/1
]).


all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

cast_int(_) ->
   1 = typecast:i(<<"1">>),
   1 = typecast:i('1'),
   1 = typecast:i("1"),
   1 = typecast:i(1),
   1 = typecast:i(1.0),
   undefined = try typecast:i({1}) catch _:_ -> undefined end.

cast_float(_) ->
   1.0 = typecast:f(<<"1.0">>),
   1.0 = typecast:f('1.0'),
   1.0 = typecast:f("1.0"),
   1.0 = typecast:f(1),
   1.0 = typecast:f(1.0),
   undefined = try typecast:f({1.0}) catch _:_ -> undefined end.

cast_string(_) ->
   <<>> = typecast:s(undefined),
   <<"1">> = typecast:s(<<"1">>),
   <<"1">> = typecast:s('1'),
   <<"1">> = typecast:s("1"),
   <<"1">> = typecast:s(1),
   <<"1.000000000">> = typecast:s(1.0),
   undefined = try typecast:s({<<"1">>}) catch _:_ -> undefined end.

cast_unicode_string(_) ->
   <<>> = typecast:ls(undefined),
   <<"тест"/utf8>> = typecast:ls(<<"тест"/utf8>>),
   % <<"тест"/utf8>> = typecast:ls('тест'),
   <<"тест"/utf8>> = typecast:ls("тест"),
   <<"1">> = typecast:ls(1),
   <<"1.000000000">> = typecast:ls(1.0),
   undefined = try typecast:s({<<"тест"/utf8>>}) catch _:_ -> undefined end.

cast_characters(_) ->
   [] = typecast:c(undefined),
   "1" = typecast:c(<<"1">>),
   "1" = typecast:c('1'),
   "1" = typecast:c("1"),
   "1" = typecast:c(1),
   "1.000000000" = typecast:c(1.0),
   undefined = try typecast:c({"1"}) catch _:_ -> undefined end.

cast_unicode_characters(_) ->
   [] = typecast:lc(undefined),
   "тест" = typecast:lc(<<"тест"/utf8>>),
   % "тест" = typecast:lc('тест'),
   "тест" = typecast:lc("тест"),
   "1" = typecast:lc(1),
   "1.000000000" = typecast:lc(1.0),
   undefined = try typecast:lc({"тест"}) catch _:_ -> undefined end.

cast_atom(_) ->
   '1' = typecast:a(<<"1">>),
   '1' = typecast:a('1'),
   '1' = typecast:a("1"),
   '1' = typecast:a(1),
   '1.000000000' = typecast:a(1.0),
   undefined = try typecast:a({'1'}) catch _:_ -> undefined end.

cast_new_atom(_) ->
   _ = typecast:atom(<<"10">>),
   _ = typecast:atom('11'),
   _ = typecast:atom("12"),
   _ = typecast:atom(13),
   _ = typecast:atom(14.0),
   undefined = try typecast:atom({'15.0'}) catch _:_ -> undefined end.

cast_hexdec(_) ->
   <<"000f">> = typecast:x(<<0,15>>),
   <<"6162">> = typecast:x('ab'),
   <<"000f">> = typecast:x([0,15]),
   <<"f">> = typecast:x(15),
   undefined = try typecast:x({15}) catch _:_ -> undefined end.

