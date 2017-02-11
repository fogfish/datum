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
   new/1, build/1, apply/1, lookup/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, bst}
     % ,{group, interface}
     % ,{group, utility}
     % ,{group, algorithm}
   ].

groups() ->
   [
      {bst, [parallel], 
         [new, build, apply, lookup]}
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
%%
'+'(undefined) ->
   undefined;
'+'(X) ->   
   X + 1.
