%%
%%   Copyright (c) 2012 - 2013, Dmitry Kolesnikov
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
%%   @description
%%      binary search tree 
-module(bst).
-behavior(maplike).
-behavior(traversable).
-behavior(foldable).

-include("datum.hrl").

-export([
   new/0,         %% O(1)
   new/1,         %% O(1)
   build/1,       %% O(n)
   build/2,       %% O(n)

   %%
   %% map-like
   append/2,      %% O(log n)
   insert/3,      %% O(log n)
   lookup/2,      %% O(log n)
   remove/2,      %% O(log n)
   has/2,         %% O(log n)
   keys/1,        %% O(n)
   apply/3        %% O(log n)

  ,min/1         %% O(log n)
  ,max/1         %% O(log n)
  ,map/2         %% O(n)
  ,foldl/3       %% O(n)
  ,mapfoldl/3    %% O(n)
  ,foldr/3       %% O(n)
  ,mapfoldr/3    %% O(n)
  ,splitwith/2   %% O(log n)
  ,takewhile/2   %% O(log n)
  ,take/2        %% O(log n)
  ,dropwhile/2   %% O(log n)
  ,drop/2        %% O(log n)  
  ,list/1        %% O(n)
]).

%%
%%
% -define(NULL,   nil).

%%
%% data types
-type tree() :: datum:option( {tree(), key(), val(), tree()} ). %% | ?NULL.
-type key()  :: _.
-type val()  :: _.


-type ord()  :: fun( (key(), key()) -> eq | lt | gt ).

%%
%% create new binary search tree
-spec new() -> datum:tree(_).

new()  ->
   new(fun datum:compare/2).

%%
%% create new binary search tree
-spec new(datum:compare(_)) -> datum:tree(_).

new(Ord) ->
   {t, Ord, ?NULL}.

%%
%% build tree from sorted data type
-spec build(_) -> datum:tree(_).

build(List) ->
   build(fun datum:compare/2, List).


%%
%%
-spec build(datum:compare(_), [_]) -> datum:tree(_).

build(Ord, List)
 when is_list(List) ->
   lists:foldl(fun append/2, new(Ord), List).

%    {t, Ord, list_to_tree(X)}.

% list_to_tree([]) ->
%    ?NULL;
% list_to_tree([{K, V}]) ->
%    {?NULL, K, V, ?NULL};
% list_to_tree([K])  ->
%    {?NULL, K, undefined, ?NULL};
% list_to_tree(List) ->
%    case lists:split(length(List) div 2, List) of
%       {L, [{K, V} | R]} ->
%          {list_to_tree(L), K, V, list_to_tree(R)};
%       {L, [K | R]} ->
%          {list_to_tree(L), K, undefined, list_to_tree(R)}
%    end.

%%%----------------------------------------------------------------------------   
%%%
%%% map-like
%%%
%%%----------------------------------------------------------------------------   

%%
%% append a new key/value pair to collection
-spec append({key(), val()}, datum:maplike(_, _)) -> datum:maplike(_, _).

append({Key, Val}, ?tree(_, _) = Tree) ->
   insert(Key, Val, Tree);

append(Key, ?tree(_, _) = Tree) ->
   insert(Key, ?None, Tree).

%%
%% insert a new a key/value pair to collection
-spec insert(key(), val(), datum:maplike(_, _)) -> datum:maplike(_, _).

insert(Key, Val, ?tree(Ord, T)) ->
   ?tree(Ord, insert_el(Ord, Key, Val, T)).

insert_el(_, K, V, ?None) ->
   {?None, K, V, ?None};
insert_el(Ord, K, V, {_, Kx, _, _} = T) ->
   insert_el(Ord(K, Kx), Ord, K, V, T).

insert_el(eq,   _, _, V, {A, Kx,  _, B}) ->
   {A, Kx, V, B};
insert_el(gt, Ord, K, V, {A, Kx, Vx, B}) ->
   {A, Kx, Vx, insert_el(Ord, K, V, B)};
insert_el(lt, Ord, K, V, {A, Kx, Vx, B}) ->
   {insert_el(Ord, K, V, A), Kx, Vx, B}.


%%
%% optionally returns the value associated with key
%%
-spec lookup(key(), datum:maplike(_, _)) -> datum:option( val() ).

lookup(Key, ?tree(Ord, T)) ->
   lookup_el(Ord, Key, T).

lookup_el(_, _, ?None) ->
   ?None;
lookup_el(Ord, K, {_, Kx, _, _} = T) ->
   lookup_el(Ord(K, Kx), Ord, K, T).

lookup_el(eq,   _, _, {_, _, Vx, _}) ->
   Vx;
lookup_el(gt, Ord, K, {_, _,  _, B}) -> 
   lookup_el(Ord, K, B);
lookup_el(lt, Ord, K, {A, _,  _, _}) ->
   lookup_el(Ord, K, A).


%%
%% remove key/value pair from collection 
-spec remove(key(), datum:maplike(_, _)) -> datum:maplike(_, _).

remove(K, ?tree(Ord, T)) ->
   ?tree(Ord, remove_el(Ord, K, T)).

remove_el(_, _, ?None) ->
   ?None;
remove_el(Ord, K, {_, Kx, _, _} = T) ->
   remove_el(Ord(K, Kx), Ord, K, T).

remove_el(eq,   _, _, {A, _, _, ?None}) ->
   A;
remove_el(eq,   _, _, {?None, _, _, B}) ->
   B;
remove_el(eq, Ord, _, {{_, Ka, Va, _}=A, _, _, B}) ->
   {remove_el(Ord, Ka, A), Ka, Va, B};
remove_el(gt, Ord, K, {A, Kx, Vx, B}) ->
   {A, Kx, Vx, remove_el(Ord, K, B)};
remove_el(lt, Ord, K, {A, Kx, Vx, B}) ->
   {remove_el(Ord, K, A), Kx, Vx, B}.


%%
%% check if the collection has an association
%%
-spec has(key(), datum:maplike(_, _)) -> true | false.

has(Key, Tree) ->
   lookup(Key, Tree) =/= undefined.

%%
%% collects all keys of this collection to list
%%
-spec keys(datum:maplike(_, _)) -> [_].

keys(Tree) ->
   foldr(fun(K, _, Acc) -> [K|Acc] end, [], Tree).

%%
%% apply function on element
-spec apply(key(), fun((datum:option(_)) -> _), datum:maplike(_, _)) -> datum:maplike(_, _).

apply(K, Fun, ?tree(Ord, T)) ->
   ?tree(Ord, apply_el(Ord, K, Fun, T)).

apply_el(_, K, Fun, ?None) ->
   {?None, K, Fun(?None), ?None};
apply_el(Ord, K, Fun, {_, Kx, _, _} = T) ->
   apply_el(Ord(K, Kx), Ord, K, Fun, T).

apply_el(eq,   _, _, Fun, {A, Kx, Vx, B}) ->
   {A, Kx, Fun(Vx), B};
apply_el(gt, Ord, K, Fun, {A, Kx, Vx, B}) ->
   {A, Kx, Vx, apply_el(Ord, K, Fun, B)};
apply_el(lt, Ord, K, Fun, {A, Kx, Vx, B}) ->
   {apply_el(Ord, K, Fun, A), Kx, Vx, B}.


%%%----------------------------------------------------------------------------   
%%%
%%% traversable
%%%
%%%----------------------------------------------------------------------------   


%%%----------------------------------------------------------------------------   
%%%
%%% foldable
%%%
%%%----------------------------------------------------------------------------   









%%
%% return smallest element
-spec min(tree()) -> {key(), val()} | undefined.

min({t, _, T}) ->
   min_el(T).

min_el({?NULL, K, V, _}) ->
   {K, V};
min_el({A, _, _, _}) ->
   min_el(A);
min_el(?NULL) ->
   undefined.

%%
%% return largest element
-spec max(tree()) -> {key(), val()}.

max({t, _, T}) ->
   max_el(T).

max_el({_, K, V, ?NULL}) ->
   {K, V};
max_el({_, _, _, B}) ->
   max_el(B);
max_el(?NULL) ->
   undefined.

%%
%% map tree
-spec map(function(), datum:tree()) -> datum:tree().

map(Fun, {t, Ord, T}) ->
   {t, Ord, map_el(Fun, T)}.

map_el(_Fun, ?NULL) ->
   ?NULL;
map_el(Fun, {A, K, V, B}) ->
   {map_el(Fun, A), K, Fun(K, V), map_el(Fun, B)}.


%%
%% fold function over tree 
-spec foldl(function(), any(), datum:tree()) -> any().

foldl(Fun, Acc, {t, _, T}) ->
   foldl_el(Fun, Acc, T).

foldl_el(_Fun, Acc0, ?NULL) ->
   Acc0;
foldl_el(Fun, Acc0, {A, K, V, B}) ->
   foldl_el(Fun, Fun(K, V, foldl_el(Fun, Acc0, A)), B).

%%
%% map and fold function over tree
-spec mapfoldl(function(), any(), datum:tree()) -> {datum:tree(), any()}.

mapfoldl(Fun, Acc0, {t, Ord, T}) ->
   {Tx, Acc} = mapfoldl_el(Fun, Acc0, T),
   {{t, Ord, Tx}, Acc}.

mapfoldl_el(_Fun, Acc0, ?NULL) ->
   {?NULL, Acc0};
mapfoldl_el(Fun, Acc0, {A, K, V, B}) ->
   {Ax, AccA} = mapfoldl_el(Fun, Acc0, A),
   {Vx, AccK} = Fun(K, V, AccA),
   {Bx, AccB} = mapfoldl_el(Fun, AccK, B),
   {{Ax, K, Vx, Bx}, AccB}.


%% 
%% fold function over tree 
-spec foldr(function(), any(), datum:tree()) -> any().

foldr(Fun, Acc, {t, _, T}) ->
   foldr_el(Fun, Acc, T).

foldr_el(_Fun, Acc0, ?NULL) ->
   Acc0;
foldr_el(Fun, Acc0, {A, K, V, B}) ->
   foldr_el(Fun, Fun(K, V, foldr_el(Fun, Acc0, B)), A).

%%
%% map and fold function over tree
-spec mapfoldr(function(), any(), datum:tree()) -> {datum:tree(), any()}.

mapfoldr(Fun, Acc0, {t, Ord, T}) ->
   {Tx, Acc} = mapfoldr_el(Fun, Acc0, T),
   {{t, Ord, Tx}, Acc}.

mapfoldr_el(_Fun, Acc0, ?NULL) ->
   {?NULL, Acc0};
mapfoldr_el(Fun, Acc0, {A, K, V, B}) ->
   {Bx, AccB} = mapfoldl_el(Fun, Acc0, B),
   {Vx, AccK} = Fun(K, V, AccB),
   {Ax, AccA} = mapfoldl_el(Fun, AccK, A),
   {{Ax, K, Vx, Bx}, AccA}.

%%
%% split tree on left and right according to predicate function.
%% the predicate function returns true for leftist keys and false otherwise. 
%% the function behaves as follows: {takewhile(...), dropwhile(...)}
-spec splitwith(function(), datum:tree()) -> {datum:tree(), datum:tree()}.

splitwith(Fun, {t, Ord, T}) ->
   {A, B} = splitwith_el(Fun, T),
   {{t, Ord, A}, {t, Ord, B}}.

splitwith_el(_Fun, ?NULL) ->
   {?NULL, ?NULL};

splitwith_el(Fun, {A, K, V, B}) ->
   case Fun(K) of
      false ->
         {Ax, Bx} = splitwith_el(Fun, A),
         {Ax, {Bx, K, V, B}};
      true  ->
         {Ax, Bx} = splitwith_el(Fun, B),
         {{A, K, V, Ax}, Bx}
   end.

%%
%% takes elements from tree while predicate function return true
-spec takewhile(function(), datum:tree()) -> datum:tree().

takewhile(Fun, {t, Ord, T}) ->
   {t, Ord, takewhile_el(Fun, T)}.

takewhile_el(_Fun, ?NULL) ->
   ?NULL;

takewhile_el(Fun, {A, K, V, B}) ->
   case Fun(K) of
      false ->
         takewhile_el(Fun, A);
      true  ->
         {A, K, V, takewhile_el(Fun, B)}
   end.

%%
%%
-spec take(integer(), tree()) -> tree().

take(N, {t, Ord, T}) ->
   {t, Ord, erlang:element(2, take_el(N, T))}.

take_el(N, ?NULL) ->
   {N, ?NULL};
take_el(N, {A, K, V, B}) ->
   case take_el(N, A) of
      {0, Ax} ->
         {0, Ax};
      {M, Ax} ->
         {R, Bx} = take_el(M - 1, B),
         {R, {Ax, K, V, Bx}}
   end.

%%
%% drops elements from tree while predicate function return true
-spec dropwhile(function(), tree()) -> tree().

dropwhile(Fun, {t, Ord, T}) ->
   {t, Ord, dropwhile_el(Fun, T)}.

dropwhile_el(_Fun, ?NULL) ->
   ?NULL;

dropwhile_el(Fun, {A, K, V, B}) ->
   case Fun(K) of
      false ->
         {dropwhile_el(Fun, A), K, V, B};
      true  ->
         dropwhile_el(Fun, B)
   end.

%%
%%
-spec drop(integer(), tree()) -> tree().

drop(N, {t, Ord, T}) ->
   {t, Ord, erlang:element(2, drop_el(N, T))}.

drop_el(N, ?NULL) ->
   {N, ?NULL};
drop_el(N, {A, K, V, B}) ->
   case drop_el(N, A) of
      {0, Ax} ->
         {0, {Ax, K, V, B}};
      {M,_Ax} ->
         drop_el(M - 1, B)
   end.

%%
%% 
-spec list(tree()) -> list().

list(Tree) ->
   foldr(
      fun(Key, Val, Acc) -> [{Key, Val} | Acc] end,
      [],
      Tree
   ).
