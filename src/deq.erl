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
-include("datum.hrl").

-export([
   new/0,  
   new/1,
   
   %   q - interface
   head/1, 
   tail/1, 
   enq/2,  
   deq/1,  

   % deq - interface
   last/1, 
   lead/1,
   poke/2, 
   pull/1,

   % utility interface
   length/1, 
   is_empty/1, 
   dropwhile/2,
   takewhile/2,
   split/2,
   splitwith/2,
   list/1
]).

%%
%% create new dequeue
-spec new() -> datum:q().
-spec new(list()) -> datum:q().

new() ->
   ?NULL.   

new(List) ->
   make_deq_head(erlang:length(List), List).

%%%------------------------------------------------------------------
%%%
%%% q - interface
%%%
%%%------------------------------------------------------------------

%%
%% queue head element
-spec head(datum:q()) -> any().

head({q, _N, _Tail, [Head|_]}) ->
   Head;

head({q, _N, [Head], []}) ->
   Head;

head({q, _N, [_|Tail], []}) ->
   lists:last(Tail);

head(_) ->
   exit(badarg).

%%
%% queue tail (removes head element)
-spec tail(datum:q()) -> datum:q().

tail(Q) ->
   {_, Tail} = deq(Q),
   Tail.

%%
%% enqueue element (push to tail)
-spec enq(any(), datum:q()) -> datum:q().

enq(E, {q, N, [_]=Tail, []}) ->
   {q, N + 1, [E], Tail};

enq(E, {q, N, Tail, Head}) ->
   {q, N + 1, [E|Tail], Head};

enq(E, ?NULL) ->
   {q, 1, [E], []}.


%%
%% dequeue element (remove first element)
-spec deq(datum:q()) -> {any(), datum:q()}.

deq({q, _N, [E], []}) ->
   {E, deq:new()};

deq({q, N, [Last|Tail], []}) ->
   [E|Head] = lists:reverse(Tail, []),
   {E, {q, N - 1, [Last], Head}};

deq({q, N, Tail, [E]}) ->
   {E, make_deq_tail(N - 1, Tail)};

deq({q, N, Tail, [E|Head]}) ->
   {E, {q, N - 1, Tail, Head}}.


%%%------------------------------------------------------------------
%%%
%%% deq - interface
%%%
%%%------------------------------------------------------------------

%%
%% queue last element
-spec last(datum:q()) -> any().

last({q, _N, [Last|_], _Head}) ->
   Last;

last({q, _N, [], [Last]}) ->
   Last;

last({q, _N, [], [_|Head]}) ->
   lists:last(Head);

last(_) ->
   exit(badarg).

%%
%% queue lead (removes rear element)
-spec lead(datum:q()) -> datum:q().

lead(Q) ->
   {_, Lead} = pull(Q),
   Lead.


%%
%% poke element (insert element to front of queue)
-spec poke(any(), datum:q()) -> datum:q().

poke(E, {q, N, [], [_]=Head}) ->
   {q, N + 1, Head, [E]};

poke(E, {q, N, Tail, Head}) ->
   {q, N + 1, Tail, [E|Head]};

poke(E, ?NULL) ->
   {q, 1, [], [E]}.


%%
%% removes last element
-spec pull(datum:q()) -> {any(), datum:q()}.

pull({q, _N, [], [E]}) ->
   {E, deq:new()};

pull({q, N, [], [Head|Tail]}) ->
   [E|T] = lists:reverse(Tail, []),
   {E, {q, N - 1, T, [Head]}};

pull({q, N, [E], Head}) ->
   {E, make_deq_head(N - 1, Head)};

pull({q, N, [E|Tail], Head}) ->
   {E, {q, N - 1, Tail, Head}}.

%%%------------------------------------------------------------------
%%%
%%% utility - interface
%%%
%%%------------------------------------------------------------------


%%
%% check length of queue
-spec length(datum:q()) -> boolean().

length({q, N, _, _}) ->
   N;
length(?NULL) ->
   0.

%%
%% check if the queue is empty
-spec is_empty(datum:q()) -> boolean().

is_empty(?NULL) ->
   true;
is_empty(_) ->
   false.

%%
%% dropwhile head of queue
-spec dropwhile(function(), datum:q()) -> datum:q().

dropwhile(Pred, {q, _, _, _}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> dropwhile(Pred, Tail); 
      false -> Q
   end;

dropwhile(_,  ?NULL) ->
   deq:new().

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

takewhile(_,  Acc, ?NULL) ->
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

splitwith(_,  Acc, ?NULL) ->
   {Acc, new()}.

%%
%%
-spec list(datum:q()) -> list().

list({q, _, Tail, Head}) ->
   Head ++ lists:reverse(Tail, []);
list(?NULL) ->
   [].


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

%%
%% make dequeue from list (supplied list is tail)
make_deq_tail(N, [_]=List) ->
   {q, N, [], List};

make_deq_tail(N, [X,Y]) ->
   {q, N, [X],[Y]};

make_deq_tail(N, [X,Y|List]) ->
   {q, N, [X,Y], lists:reverse(List, [])};

make_deq_tail(_, []) ->
   ?NULL.

%%
%% make dequeue from list (supplied list is head)
make_deq_head(N, [_]=List) ->
   {q, N, List, []};

make_deq_head(N, [X,Y]) ->
   {q, N, [X],[Y]};

make_deq_head(N, [X,Y|List]) ->
   {q, N, lists:reverse(List, []), [X,Y]};

make_deq_head(_, []) ->
   ?NULL.


