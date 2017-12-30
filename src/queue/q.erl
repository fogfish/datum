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
   new/0,      %% O(1)
   build/1,    %% O(n)

   %%
   %% queue
   enq/2,      %% O(1)
   deq/1,      %% O(1)

   %%
   %% traversable
   head/1,     %% O(1)
   tail/1,     %% O(1)
   is_empty/1  %% O(1)



  % q - interface
  % ,head/1
  % ,tail/1

   % utility interface
  % ,is_empty/1
  ,length/1
  ,dropwhile/2
  ,takewhile/2
  ,split/2
  ,splitwith/2
  ,list/1
  ,map/2
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
   [E|Head] = lists:reverse(Tail, []),
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
%% check if the queue is empty
% -spec is_empty(datum:q()) -> boolean().

% is_empty(?NULL) ->
%    true;
% is_empty(_) ->
%    false.

%%
%%
length({q, N, _, _}) ->
   N;
length(?NULL) ->
   0.

%%
%% dropwhile head of queue
-spec dropwhile(function(), datum:q()) -> datum:q().

dropwhile(Pred, {q, _N, _Tail, _Head}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> dropwhile(Pred, Tail); 
      false -> Q
   end;

dropwhile(_,  {}) ->
   new().

%%
%% takewhile head of queue
-spec takewhile(function(), datum:q()) -> datum:q().

takewhile(Pred, Queue) ->
   takewhile(Pred, new(), Queue).

takewhile(Pred, Acc, {q, _N, _Tail, _Head}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> takewhile(Pred, enq(Head, Acc), Tail); 
      false -> Acc
   end;

takewhile(_,  Acc, {}) ->
   Acc.

%%
%% partitions queue into two queues.
-spec split(function(), datum:q()) -> {datum:q(), datum:q()}.

split(X, {q, N, _, _} = Queue)
 when X >= N ->
   {Queue, new()};

split(_, ?NULL) ->
   {new(), new()};

split(N, Queue) ->
   split(N, new(), Queue).

split(0, Acc, Queue) ->
   {Acc, Queue};
split(N, Acc, Queue) ->
   {Head, Tail} = deq(Queue),
   split(N - 1, enq(Head, Acc), Tail).

%%
%% partitions queue into two queues according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Queue), dropwhile(Pred, Queue)
-spec splitwith(function(), datum:q()) -> {datum:q(), datum:q()}.

splitwith(Pred, Queue) ->
   splitwith(Pred, new(), Queue).

splitwith(Pred, Acc, {q, _N, _Tail, _Head}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> splitwith(Pred, enq(Head, Acc), Tail); 
      false -> {Acc, Q}
   end;

splitwith(_,  Acc, {}) ->
   {Acc, new()}.

%%
%%
-spec list(datum:q()) -> list().

list({q, _, Tail, Head}) ->
   Head ++ lists:reverse(Tail, []);
list(?NULL) ->
   [].

%%
%%
-spec map(fun((_) -> _), datum:q()) -> datum:q().

map(_, ?NULL) ->
   ?NULL;
map(Fun, {q, N, Tail, Head}) ->
   {q, N, lists:map(Fun, Tail), lists:map(Fun, Head)}.

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

