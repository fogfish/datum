%%
%%   Copyright 2012 - 2013 Dmitry Kolesnikov, All Rights Reserved
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
%% @description
%%   red-black tree (see Okasaki "Purely Functional Data Structures")
-module(rbtree).
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

]).

% -type(key()     :: any()).
-type(element() :: any()).
% -type(tree()    :: datum:rbtree() | ?NULL).

%%
%% data types
-type tree() :: datum:option( {color(), tree(), key(), val(), tree()} ).
-type color():: r | b. 
-type key()  :: _.
-type val()  :: _.


%%
%% create new empty red-black tree
-spec new() -> datum:tree(_).

new() ->
   new(fun datum:compare/2).

%%
%% create new binary search tree
-spec new(datum:compare(_)) -> datum:tree(_).

new(Ord) ->
   ?tree(Ord, ?None).

%%
%% build tree from another traversable structure
-spec build(_) -> datum:tree(_).

build(List) ->
   build(fun datum:compare/2, List).

%%
%% build tree from another traversable structure
-spec build(datum:compare(_), [_]) -> datum:tree(_).

build(Ord, List)
 when is_list(List) ->
   lists:foldl(fun append/2, new(Ord), List).


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
   ?tree(Ord, erlang:setelement(1, insert_el(Ord, Key, Val, T), b)).

insert_el(_, K, V, ?None) ->
   {r, ?None, K, V, ?None};
insert_el(Ord, K, V, {_, _, Kx, _, _} = T) ->
   insert_el(Ord(K, Kx), Ord, K, V, T).

insert_el(eq, _, _, V, {C, L, K, _, R}) ->
   {C, L, K, V, R};
insert_el(gt, Ord, K, V, {C, L, Kx, Vx, R}) ->
   balance({C, L, Kx, Vx, insert_el(Ord, K, V, R)});
insert_el(lt, Ord, K, V, {C, L, Kx, Vx, R}) ->
   balance({C, insert_el(Ord, K, V, L), Kx, Vx, R}).


%%
%% optionally returns the value associated with key
%%
-spec lookup(key(), datum:maplike(_, _)) -> datum:option( val() ).

lookup(Key, ?tree(Ord, T)) ->
   lookup_el(Ord, Key, T).

lookup_el(_, _, ?None) ->
   ?None;
lookup_el(Ord, K, {_, _, Kx, _, _} = T) ->
   lookup_el(Ord(K, Kx), Ord, K, T).

lookup_el(eq,   _, _, {_, _, _, Vx, _}) ->
   Vx;
lookup_el(gt, Ord, K, {_, _, _,  _, R}) -> 
   lookup_el(Ord, K, R);
lookup_el(lt, Ord, K, {_, L, _,  _, _}) ->
   lookup_el(Ord, K, L).


%%
%% remove key/value pair from collection 
-spec remove(key(), datum:maplike(_, _)) -> datum:maplike(_, _).

remove(K, ?tree(Ord, T)) ->
   ?tree(Ord, remove_el(Ord, K, T)).

remove_el(_, _, ?None) ->
   ?None;
remove_el(Ord, K, {_, _, Kx, _, _} = T) ->
   remove_el(Ord(K, Kx), Ord, K, T).

remove_el(eq,   _, _, {_, A, _, _, ?None}) ->
   A;
remove_el(eq,   _, _, {_, ?None, _, _, B}) ->
   B;
remove_el(eq, Ord, _, {C, {_, _, Ka, Va, _}=A, _, _, B}) ->
   balance({C, remove_el(Ord, Ka, A), Ka, Va, B});
remove_el(gt, Ord, K, {C, A, Kx, Vx, B}) ->
   balance({C, A, Kx, Vx, remove_el(Ord, K, B)});
remove_el(lt, Ord, K, {C, A, Kx, Vx, B}) ->
   balance({C, remove_el(Ord, K, A), Kx, Vx, B}).


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
   [].
   % foldr(fun(K, _, Acc) -> [K|Acc] end, [], Tree).

%%
%% apply function on element
-spec apply(key(), fun((datum:option(_)) -> _), datum:maplike(_, _)) -> datum:maplike(_, _).

apply(Key, Fun, ?tree(Ord, T)) ->
   ?tree(Ord, erlang:setelement(1, apply_el(Ord, Key, Fun, T), b)).

apply_el(_, K, Fun, ?None) ->
   {r, ?None, K, Fun(undefined), ?None};
apply_el(Ord, K, Fun, {_, _, Kx, _, _} = T) ->
   apply_el(Ord(K, Kx), Ord, K, Fun, T).

apply_el(eq,   _, _, Fun, {C, A, Kx, Vx, B}) ->
   {C, A, Kx, Fun(Vx), B};
apply_el(gt, Ord, K, Fun, {C, A, Kx, Vx, B}) ->
   balance({C, A, Kx, Vx, apply_el(Ord, K, Fun, B)});
apply_el(lt, Ord, K, Fun, {C, A, Kx, Vx, B}) ->
   balance({C, apply_el(Ord, K, Fun, A), Kx, Vx, B}).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------


%%
%% see Okasaki "Purely Functional Data Structures", p 27
balance({b, {r, {r, A, Kx, Vx, B}, Ky, Vy, C}, Kz, Vz, D}) -> 
   {r, {b, A, Kx, Vx, B}, Ky, Vy, {b, C, Kz, Vz, D}};

balance({b, {r, A, Kx, Vx, {r, B, Ky, Vy, C}}, Kz, Vz, D}) ->
   {r, {b, A, Kx, Vx, B}, Ky, Vy, {b, C, Kz, Vz, D}};

balance({b, A, Kx, Vx, {r, {r, B, Ky, Vy, C}, Kz, Vz, D}}) ->
   {r, {b, A, Kx, Vx, B}, Ky, Vy, {b, C, Kz, Vz, D}};

balance({b, A, Kx, Vx, {r, B, Ky, Vy, {r, C, Kz, Vz, D}}}) ->
   {r, {b, A, Kx, Vx, B}, Ky, Vy, {b, C, Kz, Vz, D}};

balance(T) ->
   T.


