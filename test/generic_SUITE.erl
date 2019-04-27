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
%%   category pattern test suite
-module(generic_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, generic}).

-export([all/0]).
-export([
   syntax_generic/1
,  struct_to_generic/1
,  generic_to_struct/1
]).

-record(adt, {a, b, c}).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

%%
%%
syntax_generic(_) ->
   ok = transform("generic:from(#adt{a = 1})."),
   ok = transform("generic:from(X#adt{})."),
   ok = transform("generic:adt(#{a => 1})."),
   ok = transform("generic:adt(X)."),
   ok = transform("a:b(X).").

%%
%%
struct_to_generic(_) ->
   #{
      a := 1,
      b := <<"test">>,
      c := 2.0
   } = generic:from(#adt{a = 1, b = <<"test">>, c = 2.0}),

   X = #adt{a = 1, b = <<"test">>, c = 2.0},
   #{
      a := 1,
      b := <<"test">>,
      c := 2.0
   } = generic:from(X#adt{}).

%%
%%
generic_to_struct(_) ->
   #adt{
      a = 1,
      b = <<"test">>,
      c = 2.0
   } = generic:adt(#{a => 1, b => <<"test">>, c => 2.0}),

   X = #{a => 1, b => <<"test">>, c => 2.0},
   #adt{
      a = 1,
      b = <<"test">>,
      c = 2.0
   } = generic:adt(X),

   #adt{
      a = 1,
      b = <<"test">>,
      c = 2.0   
   } = generic:adt([1, <<"test">>, 2.0]).


%%%------------------------------------------------------------------
%%%
%%% helpers
%%%
%%%------------------------------------------------------------------   

transform(Code) ->
   {ok, Parsed, _} = erl_scan:string(Code),
   {ok, Forms} = erl_parse:parse_exprs(Parsed),
   Fun  = [{function, 1, a, 1, [{clause, 1, [], [], Forms}]}],
   [{function, _, _, _, _}] = generic:parse_transform(Fun, []),
   ok.
