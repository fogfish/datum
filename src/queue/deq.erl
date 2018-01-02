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
%%   double ended queue
-module(deq).
-behaviour(traversable).
-behaviour(foldable).

-include("datum.hrl").

-export([
   new/0,        %% O(1)
   build/1,      %% O(n)

   %%
   %% queue
   enq/2,        %% O(1)
   deq/1,        %% O(1)
   enqh/2,       %% O(1)
   deqt/1,       %% O(1)
   last/1,
   liat/1,

   %%
   %% traversable
   list/1,
   head/1,       %% O(1)
   tail/1,       %% O(1)
   is_empty/1,   %% O(1)
   drop/2,       %% O(n)
   dropwhile/2,  %% O(n)
   filter/2,     %% O(n)  
   foreach/2,    %% O(n)
   map/2,        %% O(n)
   split/2,      %% O(n)
   splitwhile/2, %%
   take/2,
   takewhile/2,

   %%
   %% foldable
   fold/3,
   foldl/3,
   foldr/3,
   unfold/2,

   % utility interface
   length/1
]).

%%
%% create new empty queue
-spec new() -> datum:q(_).

new() ->
   q:new().

%%
%% build tree from another traversable structure 
-spec build(_) -> datum:q(_).

build(List) ->
   q:build(List).


%%
%% enqueue element to end of queue
-spec enq(_, datum:q(_)) -> datum:q(_).

enq(E, #queue{} = Queue) ->
   q:enq(E, Queue).

%%
%% dequeue element from head of queue
-spec deq(datum:q(_)) -> {datum:option(_), datum:q(_)}.

deq(#queue{} = Queue) ->
   q:deq(Queue).


%%
%% enqueue element to front of the queue
-spec enqh(_, datum:q(_)) -> datum:q(_).

enqh(E, #queue{length = N, head = [_] = Head, tail = []}) ->
   #queue{length = N + 1, head = [E], tail = Head};

enqh(E, #queue{length = N, head = Head} = Queue) ->
   Queue#queue{length = N + 1, head = [E|Head]}.


%%
%% dequeue element from tail of queue
-spec deqt(datum:q(_)) -> {datum:option(_), datum:q(_)}.

deqt(#queue{tail = [], head = [E]}) ->
   {E, new()};

deqt(#queue{length = N, tail = [], head = [Head|Tail]}) ->
   [E|T] = lists:reverse(Tail),
   {E, #queue{length = N - 1, head = [Head], tail = T}};

deqt(#queue{length = N, tail = [E], head = Head}) ->
   {E, make_deq_head(N - 1, Head)};

deqt(#queue{length = N, tail = [E|Tail]} = Queue) ->
   {E, Queue#queue{length = N - 1, tail = Tail}};

deqt(#queue{tail = [], head = []} = Queue) ->
   {?None, Queue}.


%%
%% take collection and return last element of collection
%%
-spec last(datum:traversable(_)) -> datum:option(_).

last(#queue{tail = [Last|_]}) ->
   Last;

last(#queue{head = [Last]}) ->
   Last;

last(#queue{head = [_|Head]}) ->
   lists:last(Head);

last(_) ->
   ?None.

%%
%% take collection and return its prefix (all elements except the last)
%%
-spec liat(datum:traversable(_)) -> datum:traversable(_).

liat(Queue) ->
   erlang:element(2, deqt(Queue)).



%%%------------------------------------------------------------------
%%%
%%% traversable
%%%
%%%------------------------------------------------------------------

%%
%% take collection and return head element of collection
%%
-spec head(datum:traversable(_)) -> datum:option(_).

head(#queue{} = Queue) ->
   q:head(Queue).

%%
%% take collection and return its suffix (all elements except the first)
%%
-spec tail(datum:traversable(_)) -> datum:traversable(_).

tail(#queue{} = Queue) ->
   q:tail(Queue).

%%
%% return true if collection is empty 
%%
-spec is_empty(datum:traversable(_)) -> true | false.

is_empty(#queue{} = Queue) ->
   q:is_empty(Queue).

%%
%% return the suffix of collection that starts at the next element after nth.
%% drop first n elements
%%
-spec drop(integer(), datum:traversable(_)) -> datum:traversable(_).

drop(N, #queue{} = Queue) ->
   q:drop(N, Queue).

%%
%% drops elements from collection while predicate returns true and 
%% returns remaining stream suffix.
%%
-spec dropwhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).      

dropwhile(Pred, #queue{} = Queue) ->
   q:dropwhile(Pred, Queue).

%%
%% returns a newly-allocated collection that contains only those elements of the 
%% input collection for which predicate is true.
%%
-spec filter(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

filter(Pred, #queue{} = Queue) ->
   q:filter(Pred, Queue).

%%
%% applies a function to each collection element for its side-effects; 
%% it returns nothing.
%%
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.

foreach(Pred, #queue{} = Queue) ->
   q:foreach(Pred, Queue).

%%
%% create a new collection by apply a function to each element of input collection.
%% 
-spec map(fun((_) -> _), datum:traversable(_)) -> datum:traversable(_).

map(Fun, #queue{} = Queue) ->
   q:map(Fun, Queue).

%%
%% partitions collection into two collection. The split behaves as if it is defined as 
%% consequent take(N, Seq), drop(N, Seq). 
%%
-spec split(integer(), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

split(N, #queue{} = Queue) ->
   q:split(N, Queue).

%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Seq), dropwhile(Pred, Seq)
%%
-spec splitwhile(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

splitwhile(Pred, #queue{} =  Queue) ->
   q:splitwhile(Pred, Queue).

%%
%% returns a newly-allocated collection containing the first n elements of 
%% the input collection.
%%
-spec take(integer(), datum:traversable(_)) -> datum:traversable(_).

take(N, #queue{} = Queue) ->
   q:take(N, Queue).

%%
%% returns a newly-allocated collection that contains those elements from 
%% input collection while predicate returns true.
%%
-spec takewhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

takewhile(Pred, #queue{} = Queue) ->
   q:takewhile(Pred, Queue).


%%%------------------------------------------------------------------
%%%
%%% foldable
%%%
%%%------------------------------------------------------------------

%%
%% Combine elements of a structure using a monoid
%% (with an associative binary operation)
%% 
-spec fold(datum:monoid(_), _, datum:foldable(_)) -> _.

fold(Fun, Acc, Queue) ->
   q:fold(Fun, Acc, Queue).

%%
%% Left-associative fold of a structure
%%
-spec foldl(datum:monoid(_), _, datum:foldable(_)) -> _.

foldl(Fun, Acc, Queue) ->
   q:foldl(Fun, Acc, Queue).

%%
%% Right-associative fold of a structure
%%
-spec foldr(datum:monoid(_), _, datum:foldable(_)) -> _.

foldr(Fun, Acc, Queue) ->
   q:foldr(Fun, Acc, Queue).

%% 
%% The fundamental recursive structure constructor, 
%% it applies a function to each previous seed element in turn
%% to determine the next element.
%%
-spec unfold(fun((_) -> _), _) -> datum:foldable(_).

unfold(Fun, Seed) ->
   q:unfold(Fun, Seed).


%%
%% check length of queue
-spec length(datum:q()) -> boolean().

length(#queue{length = N}) ->
   N.

%%
%%
-spec list(datum:q()) -> list().

list(#queue{head = Head, tail = Tail}) ->
   Head ++ lists:reverse(Tail).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% make dequeue from list (supplied list is tail)
% make_deq_tail(N, [_]=List) ->
%    #queue{length = N, head = List};

% make_deq_tail(N, [X,Y]) ->
%    #queue{length = N, head = [Y], tail = [X]};

% make_deq_tail(N, [X,Y|List]) ->
%    #queue{length = N, head = lists:reverse(List), tail = [X, Y]};

% make_deq_tail(_, []) ->
%    #queue{}.

%%
%% make dequeue from list (supplied list is head)
make_deq_head(N, [_]=List) ->
   #queue{length = N, head = List};

make_deq_head(N, [X,Y]) ->
   #queue{length = N, head = [X],  tail = [Y]};

make_deq_head(N, [X,Y|List]) ->
   #queue{length = N, head = [X,Y], tail = lists:reverse(List)};

make_deq_head(_, []) ->
   #queue{}.
