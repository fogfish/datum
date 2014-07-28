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

-include("datum.hrl").

-export([
   new/0      %% O(1)
  ,head/1     %% O(1)
  ,tail/1     %% O(log n)
  ,insert/3   %% O(log n)
  ,size/1     %% O(1)
  ,list/1     %% O(n)
]).

-type(heap()  :: {heap(), rank(), key(), val(), heap()} | ?NULL).
-type(key()   :: any()).
-type(val()   :: any()).
-type(rank()  :: integer()).

%%
%% create new empty heap
-spec(new/0 :: () -> datum:heap()).

new() ->
   {h, 0, ?NULL}.

%%
%% read head value
-spec(head/1 :: (datum:heap()) -> {key(), val()}).

head({h, _, {_, _, Key, Val, _}}) ->
   {Key, Val};
head(_) ->
   exit(badarg).

%%
%% return tail value
-spec(tail/1 :: (datum:heap()) -> datum:heap()).

tail({h, Size, {A, _, _, _, B}}) ->
   {h, Size - 1, merge(A, B)};
tail(_) ->
   exit(badarg).

%%
%% insert new value
-spec(insert/3 :: (key(), val(), datum:heap()) -> heap()).

insert(Key, Val, {h, Size, Heap}) ->
   {h, Size + 1, merge({?NULL, 1, Key, Val, ?NULL}, Heap)}.

%%
%% return heap size
-spec(size/1 :: (datum:heap()) -> integer()).

size({h, Size, _}) ->
   Size;
size(_) ->
   0.

%%
%% return list of elements
-spec(list/1 :: (datum:heap()) -> [{key(), val()}]).

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
merge(X, ?NULL) ->
   X;

merge(?NULL, X) ->
   X;

merge({A, _, Kx, Vx, B}, {_, _, Ky, _, _}=H)
 when Kx =< Ky ->
   make(Kx, Vx, A, merge(B, H));

merge(H, {A, _, K, V, B}) ->
   make(K, V, A, merge(H, B)).

%%
%%
rank(?NULL) ->
   0;
rank({_, R, _, _, _}) ->
   R.

%%
%%
make(K, V, A, B) ->
   case rank(A) >= rank(B) of
      true  ->
         {A, rank(B) + 1, K, V, B};
      false ->
         {B, rank(A) + 1, K, V, A}
   end.




