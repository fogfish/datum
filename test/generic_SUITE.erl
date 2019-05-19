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
   syntax/1
,  generic/1
,  derived/1
,  labelled/1
,  derived_labelled/1
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
syntax(_) ->
   ok = transform("generic_of:adt(#adt{a = 1})."),
   ok = transform("generic_of:adt(X)."),
   ok = transform("generic_to:adt(#{a => 1})."),
   ok = transform("generic_to:adt(X)."),
   ok = transform("generic:encode(#adt{})."),
   ok = transform("generic:decode(#adt{})."),

   ok = transform("labelled_of:adt(#adt{a = 1})."),
   ok = transform("labelled_of:adt(X)."),
   ok = transform("labelled_to:adt(#{a => 1})."),
   ok = transform("labelled_to:adt(X)."),
   ok = transform("labelled:encode(#adt{})."),
   ok = transform("labelled:decode(#adt{})."),

   ok = transform("a:b(X).").

%%
%%
generic(_) ->
   Struct  = #adt{a = 1, b = <<"test">>, c = 2.0},
   Expect  = #{a => 1, b => <<"test">>, c => 2.0},

   Expect = generic_of:adt(Struct),
   [Expect] = generic_of:adt([Struct]),
   Struct = generic_to:adt(generic_of:adt(Struct)),
   [Struct] = generic_to:adt(generic_of:adt([Struct])).

%%
%%
derived(_) ->
   Struct  = #adt{a = 1, b = <<"test">>, c = 2.0},
   Expect  = #{a => 1, b => <<"test">>, c => 2.0},

   Encode = generic:encode(#adt{}),
   Decode = generic:decode(#adt{}),

   Expect = Encode(Struct),
   [Expect] = Encode([Struct]),

   Struct = Decode(Encode(Struct)),
   [Struct] = Decode(Encode([Struct])).

%%
%%
labelled(_) ->
   Struct  = #adt{a = 1, b = <<"test">>, c = 2.0},
   Expect  = #{<<"a">> => 1, <<"b">> => <<"test">>, <<"c">> => 2.0},

   Expect = labelled_of:adt(Struct),
   [Expect] = labelled_of:adt([Struct]),
   Struct = labelled_to:adt(labelled_of:adt(Struct)),
   [Struct] = labelled_to:adt(labelled_of:adt([Struct])).
%%
%%
derived_labelled(_) ->
   Struct  = #adt{a = 1, b = <<"test">>, c = 2.0},
   Expect  = #{<<"a">> => 1, <<"b">> => <<"test">>, <<"c">> => 2.0},

   Encode = labelled:encode(#adt{}),
   Decode = labelled:decode(#adt{}),

   Expect = Encode(Struct),
   [Expect] = Encode([Struct]),

   Struct = Decode(Encode(Struct)),
   [Struct] = Decode(Encode([Struct])).


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
