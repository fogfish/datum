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
%%   heap ordered tree - each element at node is no large then elements at its children
-module(heap).
-behavior(maplike).
-behavior(traversable).
-behavior(foldable).

-include("datum.hrl").

-export([
   new/0,       %% O(1)
   new/1,       %% O(1)  
   build/1,     %% O(n)
   build/2,     %% O(n)

   %%
   %% map-like
   append/2,    %% O(log n)
   insert/3,    %% O(log n)
   keys/1,

   %%
   %% traversal
   head/1,      %% O(1)
   tail/1,      %% O(log n)
   is_empty/1,  %% O(1)
   drop/2,      %% O(n)
   dropwhile/2, %% O(log n)
   filter/2,    %% O(n)
   foreach/2,   %% O(n)
   map/2,       %% O(n)
   split/2 ,    %% O(n)
   splitwhile/2,%% 
   take/2,      %%
   takewhile/2, %%

   %%
   %% foldable
   fold/3,      %%
   foldl/3,     %%
   foldr/3,     %%
   unfold/2,    %%

   size/1,      %% O(1)
   list/1,      %% O(n)


   apply/3,
   has/2,
   lookup/2,
   remove/2
]).

-type heap()  :: datum:option({heap(), rank(), key(), val(), heap()}).
-type key()   :: _.
-type val()   :: _.
-type rank()  :: integer().

%%
%% create new empty heap
-spec new() -> datum:heap().

new() ->
  new(fun datum:compare/2).

%%
%% create new heap
-spec new(datum:compare(_)) -> datum:tree(_).

new(Ord) ->
   #heap{ford = Ord, heap = ?None}.

%%
%% build tree from another traversable structure
-spec build(_) -> datum:tree(_).

build(List) ->
   maplike:build(?MODULE, List).

%%
%% build tree from another traversable structure
-spec build(datum:compare(_), [_]) -> datum:tree(_).

build(Ord, List) ->
   maplike:build(?MODULE, Ord, List).


%%%----------------------------------------------------------------------------   
%%%
%%% map-like
%%%
%%%----------------------------------------------------------------------------   

%%
%% append a new key/value pair to collection
-spec append({key(), val()}, datum:maplike(_, _)) -> datum:maplike(_, _).

append({Key, Val}, #heap{} = Heap) ->
   insert(Key, Val, Heap);

append(Key, #heap{} = Heap) ->
   insert(Key, ?None, Heap).

%%
%% insert a new a key/value pair to collection
-spec insert(key(), val(), datum:maplike(_, _)) -> datum:maplike(_, _).

insert(Key, Val, #heap{ford = Ord, heap = H} = Heap) ->
   Heap#heap{heap = insert_el(Ord, Key, Val, H)}.

insert_el(Ord, Key, Val, Heap) ->
   merge(Ord, {?None, 1, Key, Val, ?None}, Heap).

%%
%% collects all keys of this collection to list
%%
-spec keys(datum:maplike(_, _)) -> [_].

keys(Tree) ->
   maplike:keys(?MODULE, Tree).


%%%----------------------------------------------------------------------------   
%%%
%%% traversal
%%%
%%%----------------------------------------------------------------------------   

%%
%% take collection and return head element of collection
%%
-spec head(datum:traversable(_)) -> datum:option(_).

head(#heap{heap = {_, _, Key, Val, _}}) ->
  {Key, Val};

head(#heap{}) ->
  undefined.


%%
%% force stream promise and return new stream (evaluates tail of stream).
-spec tail(datum:traversable(_)) -> datum:traversable(_).

tail(#heap{ford = Ord, heap = {A, _, _, _, B}} = Heap) ->
  Heap#heap{heap = merge(Ord, A, B)};

tail(#heap{heap = ?None} = Heap) ->
  Heap.


%%
%% converts the collection to Erlang list
%%
-spec list(datum:traversable(_)) -> [_].

list(#heap{heap = ?None}) ->
   [];
list(#heap{} = Heap) ->
   [head(Heap) | list(tail(Heap))].


%%
%% return true if collection is empty 
%%
-spec is_empty(datum:traversable(_)) -> true | false.

is_empty(#heap{heap = ?None}) ->
  true;
is_empty(#heap{}) ->
  false. 

%%
%% return the suffix of collection that starts at the next element after nth.
%% drop first n elements
%%
-spec drop(integer(), datum:traversable(_)) -> datum:traversable(_).

drop(0, #heap{} = Heap) ->
   Heap;
drop(_, #heap{heap = ?None} = Heap) ->
   Heap;
drop(N, #heap{} = Heap) ->
   drop(N - 1, tail(Heap)).

%%
%% drops elements from collection while predicate returns true and 
%% returns remaining stream suffix.
%%
-spec dropwhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).      

dropwhile(_, #heap{heap = ?None} = Heap) ->
   Heap;
dropwhile(Pred, #heap{} = Heap) ->
   case Pred(head(Heap)) of
      true  -> 
         dropwhile(Pred, tail(Heap)); 
      false -> 
         Heap
   end.

%%
%% returns a newly-allocated collection that contains only those elements of the 
%% input collection for which predicate is true.
%%
-spec filter(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

filter(Pred, #heap{ford = Ord, heap = H} = Heap) ->
   Heap#heap{heap = filter_el(Pred, Ord, H)}.

filter_el(_, _, ?None) ->
   ?None;
filter_el(Pred, Ord, {A0, R, K, V, B0}) ->
   A1 = filter_el(Pred, Ord, A0),
   B1 = filter_el(Pred, Ord, B0),
   case Pred({K, V}) of
      true  ->
         {A1, R, K, V, B1};
      false ->
         merge(Ord, A1, B1)
   end.


%%
%% applies a function to each collection element for its side-effects; 
%% it returns nothing.
%%
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.

foreach(_, #heap{heap = ?None}) ->
   ok;
foreach(Fun, #heap{} = Heap) ->
   _ = Fun(head(Heap)),
   foreach(Fun, tail(Heap)).

%%
%% create a new collection by apply a function to each element of input collection.
%% 
-spec map(fun((_) -> _), datum:traversable(_)) -> datum:traversable(_).

map(Fun, #heap{heap = H} = Heap) ->
   Heap#heap{heap = map_el(Fun, H)}.

map_el(_, ?None) ->
   ?None;
map_el(Fun, {A, R, K, V, B}) ->
   {map_el(Fun, A), R, K, Fun({K, V}), map_el(Fun, B)}.


%%
%% partitions collection into two collection. The split behaves as if it is defined as 
%% consequent take(N, Seq), drop(N, Seq). 
%%
-spec split(integer(), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

split(N, Heap) ->
   split(N, new(), Heap).

split(0, Acc, Heap) ->
   {Acc, Heap};

split(_, Acc, #heap{heap = ?None} = Heap) ->
   {Acc, Heap};
   
split(N, Acc, #heap{} = Heap) ->
   split(N - 1, append(head(Heap), Acc), tail(Heap)).


%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Seq), dropwhile(Pred, Seq)
%%
-spec splitwhile(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

splitwhile(Pred, Heap) ->
   splitwhile(Pred, new(), Heap).

splitwhile(_Pred, Acc, #heap{heap = ?None} = Heap) ->
   {Acc, Heap};
splitwhile(Pred, Acc, #heap{} = Heap) ->
   case Pred(head(Heap)) of
      true ->
         splitwhile(Pred, append(head(Heap), Acc), tail(Heap));
      false ->
         {Acc, Heap}
   end.

%%
%% returns a newly-allocated collection containing the first n elements of 
%% the input collection.
%%
-spec take(integer(), datum:traversable(_)) -> datum:traversable(_).

take(N, Heap) ->
   take(N, new(), Heap).

take(0, Acc, #heap{}) ->
   Acc;
take(_, Acc, #heap{heap = ?None}) ->
   Acc;
take(N, Acc, #heap{} = Heap) ->
   take(N - 1, append(head(Heap), Acc), tail(Heap)).

%%
%% returns a newly-allocated collection that contains those elements from 
%% input collection while predicate returns true.
%%
-spec takewhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

takewhile(Pred, Heap) ->
   takewhile(Pred, new(), Heap).

takewhile(_, Acc, #heap{heap = ?None}) ->
   Acc;
takewhile(Pred, Acc, #heap{} = Heap) ->
   case Pred(head(Heap)) of
      true  -> 
         takewhile(Pred, append(head(Heap), Acc), tail(Heap)); 
      false -> 
         Acc
   end.


%%%----------------------------------------------------------------------------   
%%%
%%% foldable
%%%
%%%----------------------------------------------------------------------------   

%%
%% Combine elements of a structure using a monoid
%% (with an associative binary operation)
%% 
-spec fold(datum:monoid(_), _, datum:foldable(_)) -> _.

fold(Fun, Acc, #heap{} = Heap) ->
   foldl(Fun, Acc, Heap).

%%
%% Left-associative fold of a structure
%%
-spec foldl(datum:monoid(_), _, datum:foldable(_)) -> _.

foldl(_, Acc, #heap{heap = ?None}) ->
   Acc;
foldl(Fun, Acc, #heap{} = Heap) ->
   foldl(Fun, Fun(head(Heap), Acc), tail(Heap)).

%%
%% Right-associative fold of a structure
%%
%% -spec foldr(datum:monoid(_), _, datum:foldable(_)) -> _.
foldr(Fun, Acc, #heap{} = Heap) ->
   lists:foldr(Fun, Acc, list(Heap)).

%% 
%% The fundamental recursive structure constructor, 
%% it applies a function to each previous seed element in turn
%% to determine the next element.
%%
-spec unfold(fun((_) -> _), _) -> datum:foldable(_).

unfold(Fun, Seed) ->
   unfold(Fun, Seed, new()).

unfold(Fun, Seed, Acc) ->
   case Fun(Seed) of
      {Head, Next} ->
         unfold(Fun, Next, append(Head, Acc));
      _ ->
         Acc
   end.


%%
%% return heap size
-spec size(datum:heap()) -> integer().

size({h, Size, _}) ->
   Size;
size(_) ->
   0.


apply(_, _, _) ->
   exit(not_implemented).

has(_, _) ->
   exit(not_implemented).

lookup(_, _) ->
   exit(not_implemented).

remove(_, _) ->
   exit(not_implemented).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% merge two heap keeping leftist property. 
merge(_, L, ?None) ->
   L;
merge(_, ?None, R) ->
   R;
merge(Ord, {_, _, Kx, _, _} = L, {_, _, Ky, _, _} = R) ->
   merge(Ord(Kx, Ky), Ord, L, R).

merge(eq, Ord, {A, _, Kx, Vx, B}, {_, _, _, _, _} = R) ->
   join(Kx, Vx, A, merge(Ord, B, R));
merge(lt, Ord, {A, _, Kx, Vx, B}, {_, _, _, _, _} = R) ->
   join(Kx, Vx, A, merge(Ord, B, R));
merge(gt, Ord, H, {A, _, K, V, B}) ->
   join(K, V, A, merge(Ord, H, B)).

%%
%%
rank(?None) ->
   0;
rank({_, R, _, _, _}) ->
   R.

%%
%%
join(K, V, A, B) ->
   case rank(A) >= rank(B) of
      true  ->
         {A, rank(B) + 1, K, V, B};
      false ->
         {B, rank(A) + 1, K, V, A}
   end.
