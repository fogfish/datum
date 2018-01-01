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
   apply/3,       %% O(log n)

   %%
   %% traversable
   head/1,
   tail/1,
   list/1,        %% O(n)
   is_empty/1,    %% O(n)
   drop/2,        %% O(n)
   dropwhile/2,   %% O(log n)
   filter/2,      %% O(n)
   foreach/2,     %% O(n)
   map/2,         %% O(n)
   split/2,       %% O(n)
   splitwhile/2,  %% O(log n)
   take/2,        %% O(n)
   takewhile/2,   %% O(log n)

   %%
   %% foldable
   fold/3,
   foldl/3,
   foldr/3,
   unfold/2,

   min/1,
   max/1
]).


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
   #tree{ford = Ord, tree = ?None}.

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

append({Key, Val}, #tree{} = Tree) ->
   insert(Key, Val, Tree);

append(Key, #tree{} = Tree) ->
   insert(Key, ?None, Tree).


%%
%% insert a new a key/value pair to collection
-spec insert(key(), val(), datum:maplike(_, _)) -> datum:maplike(_, _).

insert(Key, Val, #tree{ford = Ord, tree = T} = Tree) ->
   Tree#tree{tree = erlang:setelement(1, insert_el(Ord, Key, Val, T), b)}.

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

lookup(Key, #tree{ford = Ord, tree = T}) ->
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

remove(K, #tree{ford = Ord, tree = T} = Tree) ->
   Tree#tree{tree = remove_el(Ord, K, T)}.

remove_el(_, _, ?None) ->
   ?None;
remove_el(Ord, K, {_, _, Kx, _, _} = T) ->
   remove_el(Ord(K, Kx), Ord, K, T).

remove_el(eq, _, _, {_, A, _, _, ?None}) ->
   A;
remove_el(eq, _, _, {_, ?None, _, _, B}) ->
   B;
remove_el(eq, _, _, {C, A, _, _, B0}) ->
   {{K, V}, B1} = take_left_node(B0),
   balance({C, A, K, V, B1});
remove_el(gt, Ord, K, {C, A, Kx, Vx, B}) ->
   balance({C, A, Kx, Vx, remove_el(Ord, K, B)});
remove_el(lt, Ord, K, {C, A, Kx, Vx, B}) ->
   balance({C, remove_el(Ord, K, A), Kx, Vx, B}).


take_left_node({_, ?None, K, V, B}) ->
   {{K, V}, B};
take_left_node({C, A0, K, V, B}) ->
   {N, A1} = take_left_node(A0),
   {N, {C, A1, K, V, B}}.


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
   maplike:keys(?MODULE, Tree).
   
%%
%% apply function on element
-spec apply(key(), fun((datum:option(_)) -> _), datum:maplike(_, _)) -> datum:maplike(_, _).

apply(Key, Fun, #tree{ford = Ord, tree = T} = Tree) ->
   Tree#tree{tree = erlang:setelement(1, apply_el(Ord, Key, Fun, T), b)}.

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


%%%----------------------------------------------------------------------------   
%%%
%%% traversable
%%%
%%%----------------------------------------------------------------------------   

%%
%% take collection and return head element of collection
%%
-spec head(datum:traversable(_)) -> datum:option(_).

head(_) ->
   exit(not_implemented).

%%
%% take collection and return its suffix (all elements except the first)
%%
-spec tail(datum:traversable(_)) -> datum:traversable(_).

tail(_) ->
   exit(not_implemented).


%%
%% converts the collection to Erlang list
%%
-spec list(datum:traversable(_)) -> [_].

list(Tree) ->
   foldr(fun(Pair, Acc) -> [Pair | Acc] end, [], Tree).


%%
%% return true if collection is empty 
%%
-spec is_empty(datum:traversable(_)) -> true | false.

is_empty(#tree{tree = ?None}) ->
   true;
is_empty(_) ->
   false.


%%
%% return the suffix of collection that starts at the next element after nth.
%% drop first n elements
%%
-spec drop(integer(), datum:traversable(_)) -> datum:traversable(_).

drop(N, #tree{tree = T} = Tree) ->
   Tree#tree{tree = erlang:element(2, drop_el(N, T))}.

drop_el(N, ?None) ->
   {N, ?None};
drop_el(N, {C, A, K, V, B}) ->
   case drop_el(N, A) of
      {0, Ax} ->
         {0, {C, Ax, K, V, B}};
      {M,_Ax} ->
         drop_el(M - 1, B)
   end.

%%
%% drops elements from collection while predicate returns true and 
%% returns remaining stream suffix.
%%
-spec dropwhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).      

dropwhile(Pred, #tree{tree = T} = Tree) ->
   Tree#tree{tree = dropwhile_el(Pred, T)}.

dropwhile_el(_, ?None) ->
   ?None;

dropwhile_el(Pred, {C, A, K, V, B}) ->
   case Pred({K, V}) of
      false ->
         {C, dropwhile_el(Pred, A), K, V, B};
      true  ->
         dropwhile_el(Pred, B)
   end.

%%
%% returns a newly-allocated collection that contains only those elements of the 
%% input collection for which predicate is true.
%%
-spec filter(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

filter(Pred, #tree{tree = T} = Tree) ->
   Tree#tree{tree = filter_el(Pred, T)}.

filter_el(_, ?None) ->
   ?None;
filter_el(Pred, {C, A0, K, V, B0}) ->
   A1 = filter_el(Pred, A0),
   case Pred({K, V}) of
      false  ->
         case filter_el(Pred, B0) of
            ?None ->
               A1;
            B1     ->
               {{Kx, Vx}, B2} = take_left_node(B1),
               {C, A1, Kx, Vx, B2}
         end;
      true ->
         {C, A1, K, V, filter_el(Pred, B0)}
   end.

%%
%% applies a function to each collection element for its side-effects; 
%% it returns nothing.
%%
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.

foreach(Pred, #tree{tree = T}) ->
   foreach_el(Pred, T).

foreach_el(_, ?None) ->
   ok;
foreach_el(Pred, {_, A, K, V, B}) ->
   foreach_el(Pred, A),
   Pred({K, V}),
   foreach_el(Pred, B).

%%
%% create a new collection by apply a function to each element of input collection.
%% 
-spec map(fun((_) -> _), datum:traversable(_)) -> datum:traversable(_).

map(Fun, #tree{tree = T} = Tree) ->
   Tree#tree{tree = map_el(Fun, T)}.

map_el(_, ?None) ->
   ?None;
map_el(Fun, {C, A, K, V, B}) ->
   {C, map_el(Fun, A), K, Fun({K, V}), map_el(Fun, B)}.


%%
%% partitions collection into two collection. The split behaves as if it is defined as 
%% consequent take(N, Seq), drop(N, Seq). 
%%
-spec split(integer(), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

split(N, #tree{ford = Ord, tree = T}) ->
   {_, A, B} = split_el(N, T),
   {#tree{ford = Ord, tree = A}, #tree{ford = Ord, tree = B}}.

split_el(N, ?None) ->
   {N, ?None, ?None};

split_el(N, {C, A, K, V, B}) ->
   case split_el(N, A) of
      {0, A1, A2} ->
         {0, A1, {C, A2, K, V, B}};
      {N1,  _,  _} ->
         {N2, B1, B2} = split_el(N1 - 1, B),
         {N2, {C, A, K, V, B1}, B2}
   end.

%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Seq), dropwhile(Pred, Seq)
%%
-spec splitwhile(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

splitwhile(Pred, #tree{ford = Ord, tree = T}) ->
   {A, B} = splitwhile_el(Pred, T),
   {#tree{ford = Ord, tree = A}, #tree{ford = Ord, tree = B}}.

splitwhile_el(_, ?None) ->
   {?None, ?None};

splitwhile_el(Fun, {C, A, K, V, B}) ->
   case Fun({K, V}) of
      false ->
         {Ax, Bx} = splitwhile_el(Fun, A),
         {Ax, {C, Bx, K, V, B}};
      true  ->
         {Ax, Bx} = splitwhile_el(Fun, B),
         {{C, A, K, V, Ax}, Bx}
   end.

%%
%% returns a newly-allocated collection containing the first n elements of 
%% the input collection.
%%
-spec take(integer(), datum:traversable(_)) -> datum:traversable(_).


take(N, #tree{tree = T} = Tree) ->
   Tree#tree{tree = erlang:element(2, take_el(N, T))}.

take_el(N, ?None) ->
   {N, ?None};
take_el(N, {C, A, K, V, B}) ->
   case take_el(N, A) of
      {0, Ax} ->
         {0, Ax};
      {M, Ax} ->
         {R, Bx} = take_el(M - 1, B),
         {R, {C, Ax, K, V, Bx}}
   end.

%%
%% returns a newly-allocated collection that contains those elements from 
%% input collection while predicate returns true.
%%
-spec takewhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

takewhile(Pred, #tree{tree = T} = Tree) ->
   Tree#tree{tree = takewhile_el(Pred, T)}.

takewhile_el(_, ?None) ->
   ?None;

takewhile_el(Pred, {C, A, K, V, B}) ->
   case Pred({K, V}) of
      false ->
         takewhile_el(Pred, A);
      true  ->
         {C, A, K, V, takewhile_el(Pred, B)}
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

fold(Fun, Acc, Tree) ->
   foldl(Fun, Acc, Tree).

%%
%% Left-associative fold of a structure
%%
-spec foldl(datum:monoid(_), _, datum:foldable(_)) -> _.

foldl(Fun, Acc, #tree{tree = T}) ->
   foldl_el(Fun, Acc, T).

foldl_el(_Fun, Acc0, ?None) ->
   Acc0;
foldl_el(Fun, Acc0, {_, A, K, V, B}) ->
   foldl_el(Fun, Fun({K, V}, foldl_el(Fun, Acc0, A)), B).

%%
%% Right-associative fold of a structure
%%
-spec foldr(datum:monoid(_), _, datum:foldable(_)) -> _.

foldr(Fun, Acc, #tree{tree = T}) ->
   foldr_el(Fun, Acc, T).
   
foldr_el(_Fun, Acc0, ?None) ->
   Acc0;
foldr_el(Fun, Acc0, {_, A, K, V, B}) ->
   foldr_el(Fun, Fun({K, V}, foldr_el(Fun, Acc0, B)), A).

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
%% return smallest element
-spec min(tree()) -> {key(), val()} | undefined.

min(#tree{tree = T}) ->
   min_el(T).

min_el({_, ?None, K, V, _}) ->
   {K, V};
min_el({_, A, _, _, _}) ->
   min_el(A);
min_el(?None) ->
   undefined.

%%
%% return largest element
-spec max(tree()) -> {key(), val()}.

max(#tree{tree = T}) ->
   max_el(T).

max_el({_, _, K, V, ?None}) ->
   {K, V};
max_el({_, _, _, _, B}) ->
   max_el(B);
max_el(?None) ->
   undefined.



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


