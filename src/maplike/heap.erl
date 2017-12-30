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
   new/0,     %% O(1)
   new/1,     %% O(1)  
   build/1,   %% O(n)
   build/2,   %% O(n)

   %%
   %% map-like
   append/2,   %% O(log n)
   insert/3,   %% O(log n)
   keys/1,

   %%
   %% traversal
   head/1,     %% O(1)
   tail/1,     %% O(log n)
   is_empty/1, %% O(1)
   drop/2      %% O(n)

  % ,insert/3   %% O(log n)
  ,size/1     %% O(1)
   % utility interface
  ,dropwhile/2
  ,takewhile/2
  ,splitwith/2
  ,list/1     %% O(n)
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
%% insert new value
% -spec insert(key(), val(), datum:heap()) -> heap().

% insert(Key, Val, {h, Size, Heap}) ->
%    {h, Size + 1, merge({?NULL, 1, Key, Val, ?NULL}, Heap)}.

%%
%% return heap size
-spec size(datum:heap()) -> integer().

size({h, Size, _}) ->
   Size;
size(_) ->
   0.

%%
%% dropwhile head of heap
-spec dropwhile(function(), datum:heap()) -> datum:heap().

dropwhile(_Pred, {h, _, ?NULL} = Heap) ->
   Heap;
dropwhile(Pred, {h, _, _} = Heap) ->
   {Key, _} = head(Heap),
   case Pred(Key) of
      true  -> dropwhile(Pred, tail(Heap)); 
      false -> Heap
   end.


%%
%% takewhile head of heap
-spec takewhile(function(), datum:heap()) -> datum:heap().

takewhile(Pred, Heap) ->
   takewhile(Pred, new(), Heap).

takewhile(_Pred,  Acc,  {h, _, ?NULL}) ->
   Acc;
takewhile(Pred, Acc, {h, _, _} = Heap) ->
   {Key, Val} = head(Heap),
   case Pred(Key) of
      true  -> takewhile(Pred, insert(Key, Val, Acc), tail(Heap)); 
      false -> Acc
   end.


%%
%% partitions heap into two heaps according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Queue), dropwhile(Pred, Queue)
-spec splitwith(function(), datum:q()) -> {datum:q(), datum:q()}.

splitwith(Pred, Queue) ->
   splitwith(Pred, new(), Queue).

splitwith(_Pred, Acc, {h, _, ?NULL} = Heap) ->
   {Acc, Heap};
splitwith(Pred, Acc, {h, _, _} = Heap) ->
   {Key, Val} = head(Heap),
   case Pred(Key) of
      true  -> splitwith(Pred, insert(Key, Val, Acc), tail(Heap)); 
      false -> {Acc, Heap}
   end.

%%
%% return list of elements
-spec list(datum:heap()) -> [{key(), val()}].

list({h, _, _} = Heap) -> 
   list(Heap, []).

list({h, _, ?NULL}, Acc) -> 
   lists:reverse(Acc);
list({h, _, _}=Heap,  Acc) -> 
   list(tail(Heap), [head(Heap)|Acc]).


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

merge(eq, Ord, {A, _, Kx, Vx, B}, {_, _, Ky, _, _} = R) ->
   join(Kx, Vx, A, merge(Ord, B, R));
merge(lt, Ord, {A, _, Kx, Vx, B}, {_, _, Ky, _, _} = R) ->
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
