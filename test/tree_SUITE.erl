%%
%%   Copyright (c) 2017, Dmitry Kolesnikov
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
-module(tree_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile({parse_transform, category}).
-compile({no_auto_import,[apply/2]}).

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

-export([
   new/1, build/1, apply/1, lookup/1, minmax/1, map/1, fold/1, 
   splitwith/1, dropwhile/1, takewhile/1, take/1, drop/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, bst}
   ].

groups() ->
   [
      {bst, [parallel], 
         [new, build, apply, lookup, minmax, map, fold,
         splitwith, dropwhile, takewhile, take, drop]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(bst, Config) ->
   [{type, bst}|Config];
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% tree primitives
%%%
%%%----------------------------------------------------------------------------   
-define(N,    10).
-define(PAIR, [{1, $a}, {2, $b}, {3, $c}, {4, $d}, {5, $e}, {6, $f}, {7, $g}, {8, $h}, {9, $i}, {10, $j}]).

%%
new(Config) ->
   Type = ?config(type, Config),
   {t, _, _} = Type:new().

%%
build(Config) ->
   Type = ?config(type, Config),
   {t, _, _} = Type:build(?PAIR).

%%
apply(Config) ->
   Type      = ?config(type, Config),
   $c        = apply(Type, 2),
   undefined = apply(Type, x). 

apply(Type, Key) ->
   [$.|| Type:build(?PAIR), Type:apply(Key, fun '+'/1, _), Type:lookup(Key, _)].

%%
lookup(Config) ->
   Type      = ?config(type, Config),
   $f        = lookup(Type, 6),
   undefined = lookup(Type, x).

lookup(Type, Key) ->      
   [$.|| Type:build(?PAIR), Type:lookup(Key, _)].

%%
minmax(Config) ->
   Type    = ?config(type, Config),
   Tree    = Type:build(?PAIR),
   {1,  $a}= Type:min(Tree),
   {10, $j}= Type:max(Tree).  

%%
map(Config) ->
   Type    = ?config(type, Config),
   Tree    = Type:map(fun(K, V) -> K + V end, Type:build(?PAIR)),
   Seq     = [{K, K + V} || {K, V} <- ?PAIR],
   lists:foreach(
      fun({K, V}) ->
         V = Type:lookup(K, Tree)
      end,
      Seq
   ).

%%
fold(Config) ->
   Type   = ?config(type, Config),
   Tree   = Type:build(?PAIR),
   Expect = lists:sum([X || {_, X} <- ?PAIR]),
   Expect = Type:foldl(fun(_, V, Acc) -> V + Acc end, 0, Tree),
   Expect = Type:foldr(fun(_, V, Acc) -> V + Acc end, 0, Tree).


%%
splitwith(Config) ->
   Type   = ?config(type, Config),
   Tree   = Type:build(?PAIR),
   {A, B} = Type:splitwith(fun(K) -> K < ?N div 2 end, Tree),
   {1, _}            = Type:min(A),
   {?N div 2 - 1, _} = Type:max(A),
   {?N div 2, _}     = Type:min(B),
   {?N, _}           = Type:max(B).

%%
takewhile(Config) ->
   Type = ?config(type, Config),
   Tree = Type:build(?PAIR),
   A    = Type:takewhile(fun(K) -> K < ?N div 2 end, Tree),
   {1, _}            = Type:min(A),
   {?N div 2 - 1, _} = Type:max(A).

%%
dropwhile(Config) ->
   Type = ?config(type, Config),
   Tree = Type:build(?PAIR),
   B    = Type:dropwhile(fun(K) -> K < ?N div 2 end, Tree),
   {?N div 2, _} = Type:min(B),
   {?N, _}       = Type:max(B).

%%
take(Config) ->
   Type = ?config(type, Config),
   Tree = Type:build(?PAIR),
   A    = Type:take(?N div 2, Tree),
   {1, _}        = Type:min(A),
   {?N div 2, _} = Type:max(A).


%%
drop(Config) ->
   Type = ?config(type, Config),
   Tree = Type:build(?PAIR),
   B    = Type:drop(?N div 2, Tree),
   {?N div 2 + 1, _} = Type:min(B),
   {?N, _}           = Type:max(B).

%%
%%
'+'(undefined) ->
   undefined;
'+'(X) ->   
   X + 1.
