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
%%   pure functional queue
-module(q).
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

   %%
   %% traversable
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
   take/2,       %%
   takewhile/2,  %%

   %%
   %% foldable
   fold/3,
   foldl/3,
   foldr/3,
   unfold/2

  % q - interface
  % ,head/1
  % ,tail/1

   % utility interface
  % ,is_empty/1
  ,length/1
  ,list/1
  
]).


%%
%% create new empty queue
-spec new() -> datum:q(_).

new() ->
   #queue{}.

%%
%% build tree from another traversable structure 
-spec build(_) -> datum:q(_).

build(List) ->
   make_deq_head(erlang:length(List), List).

%%
%% enqueue element
-spec enq(_, datum:q(_)) -> datum:q(_).

enq(E, #queue{length = N, tail = [_] = Tail, head = []}) ->
   #queue{length = N + 1, tail = [E], head = Tail};

enq(E, #queue{length = N, tail = Tail} = Queue) ->
   Queue#queue{length = N + 1, tail = [E|Tail]}.

%%
%% dequeue element
-spec deq(datum:q(_)) -> {datum:option(_), datum:q(_)}.

deq(#queue{tail = [E], head = []}) ->
   {E, new()};

deq(#queue{length = N, tail = [Last|Tail], head = []}) ->
   [E|Head] = lists:reverse(Tail),
   {E, #queue{length = N - 1, head = Head, tail = [Last]}};

deq(#queue{length = N, tail = Tail, head = [E]}) ->
   {E, make_deq_tail(N - 1, Tail)};

deq(#queue{length = N, head = [E|Head]} = Queue) ->
   {E, Queue#queue{length = N - 1, head = Head}};

deq(#queue{tail = [], head = []} = Queue) ->
   {?None, Queue}.

%%%------------------------------------------------------------------
%%%
%%% traversable
%%%
%%%------------------------------------------------------------------

%%
%% take collection and return head element of collection
%%
-spec head(datum:traversable(_)) -> datum:option(_).

head(#queue{head = [Head| _]}) -> 
   Head;
head(#queue{tail = [Head]}) ->
   Head;
head(#queue{tail = [_|Tail]}) ->
   lists:last(Tail);
head(#queue{}) ->
   undefined.

%%
%% take collection and return its suffix (all elements except the first)
%%
-spec tail(datum:traversable(_)) -> datum:traversable(_).

tail(#queue{} = Queue) ->
   erlang:element(2, deq(Queue)).


%%
%% return true if collection is empty 
%%
-spec is_empty(datum:traversable(_)) -> true | false.

is_empty(#queue{head = [], tail = []}) ->
   true;
is_empty(#queue{}) ->
   false.


%%
%% return the suffix of collection that starts at the next element after nth.
%% drop first n elements
%%
-spec drop(integer(), datum:traversable(_)) -> datum:traversable(_).

drop(0, #queue{} = Queue) ->
   Queue;
drop(_, #queue{head = [], tail = []} = Queue) ->
   Queue;
drop(N, #queue{} = Queue) ->
   drop(N - 1, tail(Queue)).

%%
%% drops elements from collection while predicate returns true and 
%% returns remaining stream suffix.
%%
-spec dropwhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).      

dropwhile(_, #queue{head = [], tail = []} = Queue) ->
   Queue;
dropwhile(Pred, #queue{} = Queue) ->
   case Pred(head(Queue)) of
      true  -> 
         dropwhile(Pred, tail(Queue)); 
      false -> 
         Queue
   end.

%%
%% returns a newly-allocated collection that contains only those elements of the 
%% input collection for which predicate is true.
%%
-spec filter(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

filter(Pred, #queue{head = Head0, tail = Tail0}) ->
   Head1 = lists:filter(Pred, Head0),
   Tail1 = lists:filter(Pred, Tail0),
   #queue{length = erlang:length(Head1) + erlang:length(Tail1), head = Head1, tail = Tail1}.


%%
%% applies a function to each collection element for its side-effects; 
%% it returns nothing.
%%
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.

foreach(_, #queue{head = [], tail = []}) ->
   ok;
foreach(Fun, #queue{} = Queue) ->
   _ = Fun(head(Queue)),
   foreach(Fun, tail(Queue)).


%%
%% create a new collection by apply a function to each element of input collection.
%% 
-spec map(fun((_) -> _), datum:traversable(_)) -> datum:traversable(_).

map(Fun, #queue{head = Head, tail = Tail} = Queue) ->
   Queue#queue{head = lists:map(Fun, Head), tail = lists:map(Fun, Tail)}.

%%
%% partitions collection into two collection. The split behaves as if it is defined as 
%% consequent take(N, Seq), drop(N, Seq). 
%%
-spec split(integer(), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

split(X, #queue{length = N} = Queue)
 when X >= N ->
   {Queue, new()};

split(_, #queue{head = [], tail = []} = Queue) ->
   {Queue, Queue};

split(N, Queue) ->
   split(N, new(), Queue).

split(0, Acc, Queue) ->
   {Acc, Queue};
split(N, Acc, Queue) ->
   {Head, Tail} = deq(Queue),
   split(N - 1, enq(Head, Acc), Tail).

%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Seq), dropwhile(Pred, Seq)
%%
-spec splitwhile(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.

splitwhile(Pred, Queue) ->
   splitwhile(Pred, new(), Queue).

splitwhile(_, Acc, #queue{head = [], tail = []} = Queue) ->
   {Acc, Queue};
splitwhile(Pred, Acc, #queue{} = Queue) ->
   Head = head(Queue),
   case Pred(Head) of
      true  -> 
         splitwhile(Pred, enq(Head, Acc), tail(Queue)); 
      false -> 
         {Acc, Queue}
   end.

%%
%% returns a newly-allocated collection containing the first n elements of 
%% the input collection.
%%
-spec take(integer(), datum:traversable(_)) -> datum:traversable(_).

take(X, #queue{length = N} = Queue)
 when X >= N ->
   Queue;
take(N, Queue) ->
   take(N, new(), Queue).

take(0, Acc, _Queue) ->
   Acc;
take(_, Acc, #queue{head = [], tail = []}) ->
   Acc;
take(N, Acc, Queue) ->
   {Head, Tail} = deq(Queue),
   take(N - 1, enq(Head, Acc), Tail).


%%
%% returns a newly-allocated collection that contains those elements from 
%% input collection while predicate returns true.
%%
-spec takewhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).

takewhile(Pred, Queue) ->
   takewhile(Pred, new(), Queue).

takewhile(_, Acc, #queue{head = [], tail = []}) ->
   Acc;
takewhile(Pred, Acc, #queue{} = Queue) ->
   {Head, Tail} = deq(Queue),
   case Pred(Head) of
      true  -> 
         takewhile(Pred, enq(Head, Acc), Tail); 
      false -> 
         Acc
   end.

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
   foldl(Fun, Acc, Queue).

%%
%% Left-associative fold of a structure
%%
-spec foldl(datum:monoid(_), _, datum:foldable(_)) -> _.

foldl(Fun, Acc, #queue{head = Head, tail = Tail}) ->
   lists:foldr(Fun, lists:foldl(Fun, Acc, Head), Tail).

%%
%% Right-associative fold of a structure
%%
-spec foldr(datum:monoid(_), _, datum:foldable(_)) -> _.

foldr(Fun, Acc, #queue{head = Head, tail = Tail}) ->
   lists:foldr(Fun, lists:foldl(Fun, Acc, Tail), Head).


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
         unfold(Fun, Next, enq(Head, Acc));
      _ ->
         Acc
   end.


%%
%%
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
make_deq_tail(N, [_]=List) ->
   #queue{length = N, head = List};

make_deq_tail(N, [X,Y]) ->
   #queue{length = N, head = [Y], tail = [X]};

make_deq_tail(N, [X,Y|List]) ->
   #queue{length = N, head = lists:reverse(List), tail = [X, Y]};

make_deq_tail(_, []) ->
   #queue{}.

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

