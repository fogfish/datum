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
   new/1,

   append/1,
   insert/1,
   lookup/1,
   remove/1,
   has/1,
   keys/1,
   apply/1,

   build/1,
   drop/1,  
   dropwhile/1,
   filter/1,
   foreach/1,
   map/1, 
   split/1,
   splitwhile/1,
   take/1,
   takewhile/1,

   % fold/1,
   % foldl/1,
   % foldr/1,
   % unfold/1,

   min/1,
   max/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, bst},
      {group, rbtree}
   ].

groups() ->
   [
      {bst, [parallel], 
         [new, 
         append, insert, lookup, remove, has, keys, apply, 
         build, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile, 
         % fold, foldl, foldr, unfold, min, max]},
         min, max]},

      {rbtree, [parallel],
         [new, 
         append, insert, lookup, remove, has, keys, apply, 
         build, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile, 
         % fold, foldl, foldr, unfold, min, max]}
         min, max]}
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
init_per_group(Type, Config) ->
   [{type, Type}|Config].

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% tree primitives
%%%
%%%----------------------------------------------------------------------------   
-define(LENGTH, 100).

new(Config) ->
   Type = ?config(type, Config),
   tree = erlang:element(1, Type:new()).

append(Config) ->
   Type    = ?config(type, Config),
   [{1,1}] = Type:list(Type:append({1,1}, Type:new())). 

insert(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Lens = fun({Key, Val}, Acc) -> Type:insert(Key, Val, Acc) end,
   Tree = lists:foldl(Lens, Type:new(), List),
   lists:foreach(
      fun({Key, Val}) ->
         Val = Type:lookup(Key, Tree)
      end,
      shuffle(List)
   ).

lookup(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tree = Type:build(List),
   undefined = Type:lookup(any, Tree),
   lists:foreach(
      fun({Key, Val}) ->
         Val = Type:lookup(Key, Tree)
      end,
      shuffle(List)
   ).

remove(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tree = Type:build(List),
   Tree = Type:remove(any, Tree),
   Empty= Type:new(),
   Empty= lists:foldl(
      fun({Key, _}, Acc) ->
         Type:remove(Key, Acc)
      end,
      Tree,
      shuffle(List)
   ).

has(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tree = Type:build(List),
   undefined = Type:lookup(any, Tree),
   lists:foreach(
      fun({Key, _}) ->
         true = Type:has(Key, Tree)
      end,
      shuffle(List)
   ).

keys(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tree = Type:build(List),
   Keys = lists:sort([Key || {Key, _} <- List]),
   Keys = Type:keys(Tree).

apply(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tree0= Type:build(List),
   Tree1= lists:foldl(
      fun({Key, Val}, Acc) -> 
         Type:apply(Key, fun(X) -> X - Val end, Acc)
      end,
      Tree0,
      shuffle(List)
   ),
   0 = Type:foldl(fun({_, X}, Acc) -> Acc + X end, 0, Tree1).


build(Config) ->
   Type = ?config(type, Config),
   Tree = Type:build([{2, b}, {1, a}, {3, c}]),
   [{1, a}, {2, b}, {3, c}] = Type:list(Tree).


drop(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   N     = rand:uniform(?LENGTH),
   Tree  = Type:drop(N, Type:build(List)),
   Keys  = lists:seq(N + 1, ?LENGTH),
   Keys  = Type:keys(Tree).

dropwhile(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   N     = rand:uniform(?LENGTH),
   Tree  = Type:dropwhile(fun({Key, _}) -> Key =< N end, Type:build(List)), 
   Keys  = lists:seq(N + 1, ?LENGTH),
   Keys  = Type:keys(Tree).
   
filter(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   N     = rand:uniform(?LENGTH),
   Tree  = Type:filter(fun({Key, _}) -> Key =< N end, Type:build(List)), 
   Keys  = lists:seq(1, N),
   Keys  = Type:keys(Tree).

%%
foreach(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   ok = Type:foreach(fun(X) -> X end, Type:build(List)).

%%
map(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Tree   = Type:map(fun({_, _}) -> 0 end, Type:build(List)),
   0 = Type:foldl(fun({_, X}, Acc) -> Acc + X end, 0, Tree).

%%
split(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(seq(?LENGTH)),
   N      = rand:uniform(?LENGTH),
   {TreeA, TreeB} = Type:split(N, Type:build(List)),
   KeyA   = lists:seq(1, N),
   KeyA   = Type:keys(TreeA),

   KeyB   = lists:seq(N + 1, ?LENGTH),
   KeyB   = Type:keys(TreeB).

%%
splitwhile(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(seq(?LENGTH)),
   N      = rand:uniform(?LENGTH),
   {TreeA, TreeB} = Type:splitwhile(fun({Key, _}) -> Key =< N end, Type:build(List)),
   KeyA   = lists:seq(1, N),
   KeyA   = Type:keys(TreeA),

   KeyB   = lists:seq(N + 1, ?LENGTH),
   KeyB   = Type:keys(TreeB).

%%
take(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   N     = rand:uniform(?LENGTH),
   Tree  = Type:take(N, Type:build(List)),
   Keys  = lists:seq(1, N),
   Keys  = Type:keys(Tree).

%%
takewhile(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   N     = rand:uniform(?LENGTH),
   Tree  = Type:takewhile(fun({Key, _}) -> Key =< N end, Type:build(List)), 
   Keys  = lists:seq(1, N),
   Keys  = Type:keys(Tree).

%%
min(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   Tree  = Type:build(List),
   Min   = lists:min(List),
   Min   = Type:min(Tree).

%%
max(Config) ->
   Type  = ?config(type, Config),
   List  = shuffle(seq(?LENGTH)),
   Tree  = Type:build(List),
   Max   = lists:max(List),
   Max   = Type:max(Tree).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
randseq(0) -> 
   [];
randseq(N) -> 
   [{rand:uniform(1 bsl 32), N} | randseq(N - 1)].

seq(0) ->
   [];
seq(N) ->
   [{N, N} | seq(N - 1)].

%%
shuffle(List) ->
   [Y || {_, Y} <- lists:keysort(1, [{rand:uniform(), X} || X <- List])].



