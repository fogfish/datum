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
-include("datum.hrl").

-export([
   new/0     % O(1)
  ,insert/3  % O(log n)
  ,lookup/2  % O(log n)
  ,apply/3   % O(log n)
]).

-type(key()     :: any()).
-type(element() :: any()).
-type(tree()    :: datum:rbtree() | ?NULL).

%%
%% create new empty red-black tree
-spec(new/0 :: () -> tree()).

new() ->
	?NULL.

%%
%% apply function to element
-spec(apply/3 :: (function(), key(), tree()) -> tree()).

apply(Fun, Key, T) ->
   erlang:setelement(1, apply_el(Fun, Key, T), b).

apply_el(Fun, Key, ?NULL) ->
   {r, ?NULL, {Key, Fun(undefined)}, ?NULL};

apply_el(Fun, Key, {C, L, {Key, Val}, R}) ->
   {C, L, {Key, Fun(Val)}, R};

apply_el(Fun, Key, {C, L, {K, _}=X, R})
 when Key < K ->
   balance({C, apply_el(Fun, Key, L), X, R});

apply_el(Fun, Key, {C, L, {K, _}=X, R})
 when Key > K ->
   balance({C, L, X, apply_el(Fun, Key, R)}).


%%
%% insert element
-spec(insert/3 :: (key(), element(), tree()) -> tree()).

insert(Key, Val, T) ->
   erlang:setelement(1, ins(Key, Val, T), b).

ins(Key, Val, ?NULL) ->
   {r, ?NULL, {Key, Val}, ?NULL};

ins(Key, Val, {C, L, {Key, _}, R}) ->
   {C, L, {Key, Val}, R};

ins(Key, Val, {C, L, {K, _}=X, R})
 when Key < K ->
   balance({C, ins(Key, Val, L), X, R});

ins(Key, Val, {C, L, {K, _}=X, R})
 when Key > K ->
   balance({C, L, X, ins(Key, Val, R)}).

%%
%% lookup element
-spec(lookup/2 :: (key(), tree()) -> element()).

lookup(_Key, ?NULL) ->
   undefined;

lookup(Key, {_C, _L, {Key, Val}, _R}) ->
   Val;

lookup(Key, {_C, L, {K, _}, _R})
 when Key < K ->
   lookup(Key, L);

lookup(Key, {_C, _L, {K, _}, R})
  when Key > K ->
   lookup(Key, R).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------


%%
%% see Okasaki "Purely Functional Data Structures", p 27
balance({b, {r, {r, A, X, B}, Y, C}, Z, D}) -> 
   {r, {b, A, X, B}, Y, {b, C, Z, D}};

balance({b, {r, A, X, {r, B, Y, C}}, Z, D}) ->
   {r, {b, A, X, B}, Y, {b, C, Z, D}};

balance({b, A, X, {r, {r, B, Y, C}, Z, D}}) ->
   {r, {b, A, X, B}, Y, {b, C, Z, D}};

balance({b, A, X, {r, B, Y, {r, C, Z, D}}}) ->
   {r, {b, A, X, B}, Y, {b, C, Z, D}};

balance(T) ->
   T.


