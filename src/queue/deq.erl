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
   take/2,
   takewhile/2,

   %%
   %% foldable
   fold/3,
   foldl/3,
   foldr/3,
   unfold/2,

   %   q - interface
   % head/1, 
   % tail/1, 
   % enq/2,  
   % deq/1,  

   % deq - interface
   last/1, 
   lead/1,
   poke/2, 
   pull/1,

   % utility interface
   length/1, 
   % is_empty/1, 
   
   % takewhile/2,
   % splitwith/2,
   list/1
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
%% enqueue element
-spec enq(_, datum:q(_)) -> datum:q(_).

enq(E, #queue{} = Queue) ->
   q:enq(E, Queue).

%%
%% dequeue element
-spec deq(datum:q(_)) -> {datum:option(_), datum:q(_)}.

deq(#queue{} = Queue) ->
   q:deq(Queue).



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


%%%------------------------------------------------------------------
%%%
%%% q - interface
%%%
%%%------------------------------------------------------------------

%%
%% queue head element
% -spec head(datum:q()) -> any().

% head({q, _N, _Tail, [Head|_]}) ->
%    Head;

% head({q, _N, [Head], []}) ->
%    Head;

% head({q, _N, [_|Tail], []}) ->
%    lists:last(Tail);

% head(_) ->
%    exit(badarg).

%%
%% queue tail (removes head element)
% -spec tail(datum:q()) -> datum:q().

% tail(Q) ->
%    {_, Tail} = deq(Q),
%    Tail.

%%
%% enqueue element (push to tail)
% -spec enq(any(), datum:q()) -> datum:q().

% enq(E, {q, N, [_]=Tail, []}) ->
%    {q, N + 1, [E], Tail};

% enq(E, {q, N, Tail, Head}) ->
%    {q, N + 1, [E|Tail], Head};

% enq(E, ?NULL) ->
%    {q, 1, [E], []}.


%%
%% dequeue element (remove first element)
% -spec deq(datum:q()) -> {any(), datum:q()}.

% deq({q, _N, [E], []}) ->
%    {E, deq:new()};

% deq({q, N, [Last|Tail], []}) ->
%    [E|Head] = lists:reverse(Tail, []),
%    {E, {q, N - 1, [Last], Head}};

% deq({q, N, Tail, [E]}) ->
%    {E, make_deq_tail(N - 1, Tail)};

% deq({q, N, Tail, [E|Head]}) ->
%    {E, {q, N - 1, Tail, Head}}.


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
% -spec is_empty(datum:q()) -> boolean().

% is_empty(?NULL) ->
%    true;
% is_empty(_) ->
%    false.

%%
% %% dropwhile head of queue
% -spec dropwhile(function(), datum:q()) -> datum:q().

% dropwhile(Pred, {q, _, _, _}=Q) ->
%    {Head, Tail} = deq(Q),
%    case Pred(Head) of
%       true  -> dropwhile(Pred, Tail); 
%       false -> Q
%    end;

% dropwhile(_,  ?NULL) ->
%    deq:new().

%%
%% takewhile head of queue
% -spec takewhile(function(), datum:q()) -> datum:q().

% takewhile(Pred, Queue) ->
%    takewhile(Pred, new(), Queue).

% takewhile(Pred, Acc, {q, _N, _Tail, _Head}=Q) ->
%    {Head, Tail} = deq(Q),
%    case Pred(Head) of
%       true  -> takewhile(Pred, enq(Head, Acc), Tail); 
%       false -> Acc
%    end;

% takewhile(_,  Acc, ?NULL) ->
%    Acc.

%%
%% partitions queue into two queues.
% -spec split(function(), datum:q()) -> {datum:q(), datum:q()}.

% split(X, {q, N, _, _} = Queue)
%  when X >= N ->
%    {Queue, new()};

% split(_, ?NULL) ->
%    {new(), new()};

% split(N, Queue) ->
%    split(N, new(), Queue).

% split(0, Acc, Queue) ->
%    {Acc, Queue};
% split(N, Acc, Queue) ->
%    {Head, Tail} = deq(Queue),
%    split(N - 1, enq(Head, Acc), Tail).


%%
%% partitions queue into two queues according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Queue), dropwhile(Pred, Queue)
% -spec splitwith(function(), datum:q()) -> {datum:q(), datum:q()}.

% splitwith(Pred, Queue) ->
%    splitwith(Pred, new(), Queue).

% splitwith(Pred, Acc, {q, _N, _Tail, _Head}=Q) ->
%    {Head, Tail} = deq(Q),
%    case Pred(Head) of
%       true  -> splitwith(Pred, enq(Head, Acc), Tail); 
%       false -> {Acc, Q}
%    end;

% splitwith(_,  Acc, ?NULL) ->
%    {Acc, new()}.

%%
%%
-spec list(datum:q()) -> list().

list({q, _, Tail, Head}) ->
   Head ++ lists:reverse(Tail, []);
list(?NULL) ->
   [].

%%
%%
% -spec map(fun((_) -> _), datum:q()) -> datum:q().

% map(_, ?NULL) ->
%    ?NULL;
% map(Fun, {q, N, Tail, Head}) ->
%    {q, N, lists:map(Fun, Tail), lists:map(Fun, Head)}.


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


