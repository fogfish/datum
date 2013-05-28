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
   new/0,  new/1,
   head/1, last/1, tail/1, lead/1,
   enq/2,  poke/2, deq/1,  pull/1,
   length/1, is_empty/1, dropwhile/2
]).

%%
%% create new dequeue
-spec(new/0 :: () -> datum:q()).
-spec(new/1 :: (list()) -> datum:q()).

new() ->
   ?NULL.   

new(List) ->
   make_deq_tail(erlang:length(List), List).

%%
%% queue head element
-spec(head/1 :: (datum:q()) -> any()).

head({q, _N, _Tail, [Head|_]}) ->
   Head;

head({q, _N, [Head], []}) ->
   Head;

head({q, _N, [_|Tail], []}) ->
   lists:last(Tail);

head(_) ->
   throw(badarg).

%%
%% queue last element
-spec(last/1 :: (datum:q()) -> any()).

last({q, _N, [Last|_], _Head}) ->
   Last;

last({q, _N, [], [Last]}) ->
   Last;

last({q, _N, [], [_|Head]}) ->
   lists:last(Head);

last(_) ->
   throw(badarg).


%%
%% queue tail (removes head element)
-spec(tail/1 :: (datum:q()) -> datum:q()).

tail(Q) ->
   {_, Tail} = deq(Q),
   Tail.

%%
%% queue init (removes rear element)
-spec(lead/1 :: (datum:q()) -> datum:q()).

lead(Q) ->
   {_, Lead} = pull(Q),
   Lead.

%%
%% enqueue element (push to tail)
-spec(enq/2 :: (any(), datum:q()) -> datum:q()).

enq(E, {q, N, [_]=Tail, []}) ->
   {q, N + 1, [E], Tail};

enq(E, {q, N, Tail, Head}) ->
   {q, N + 1, [E|Tail], Head};

enq(E, ?NULL) ->
   {q, 1, [E], []}.

%%
%% poke element (insert element to front of queue)
-spec(poke/2 :: (any(), datum:q()) -> datum:q()).

poke(E, {q, N, [], [_]=Head}) ->
   {q, N + 1, Head, [E]};

poke(E, {q, N, Tail, Head}) ->
   {q, N + 1, Tail, [E|Head]};

poke(E, ?NULL) ->
   {q, 1, [], [E]}.


%%
%% dequeue element (remove first element)
-spec(deq/1 :: (datum:q()) -> {any(), datum:q()}).

deq({q, _N, [E], []}) ->
   {E, q:new()};

deq({q, N, [Last|Tail], []}) ->
   [E|Head] = lists:reverse(Tail, []),
   {E, {q, N - 1, [Last], Head}};

deq({q, N, Tail, [E]}) ->
   {E, make_deq_tail(N - 1, Tail)};

deq({q, N, Tail, [E|Head]}) ->
   {E, {q, N - 1, Tail, Head}}.

%%
%% removes last element
-spec(pull/1 :: (datum:q()) -> {any(), datum:q()}).

pull({q, _N, [], [E]}) ->
   {E, q:new()};

pull({q, N, [], [Head|Tail]}) ->
   [E|T] = lists:reverse(Tail, []),
   {E, {q, N - 1, T, [Head]}};

pull({q, N, [E], Head}) ->
   {E, make_deq_head(N - 1, Head)};

pull({q, N, [E|Tail], Head}) ->
   {E, {q, N - 1, Tail, Head}}.

%%
%% check length of queue
-spec(length/1 :: (datum:q()) -> boolean()).

length({q, N, _, _}) ->
   N.

%%
%% check if the queue is empty
-spec(is_empty/1 :: (datum:q()) -> boolean()).

is_empty(?NULL) ->
   true;
is_empty(_) ->
   false.

%%
%% dropwhile head of queue
-spec(dropwhile/2 :: (function(), datum:q()) -> datum:q()).

dropwhile(Pred, {q, _, _, _}=Q) ->
   {Head, Tail} = deq(Q),
   case Pred(Head) of
      true  -> dropwhile(Pred, Tail); 
      false -> Q
   end;

dropwhile(_,  ?NULL) ->
   q:new().


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


-ifdef(TESTS).
-include_lib("eunit/include/eunit.hrl").

q_enc_deq_test() ->
   Seq = lists:seq(1, 5),
   Q0  = lists:foldl(fun q:enq/2, q:new(), Seq),
   {1, Q1} = q:deq(Q0),   
   {2, Q2} = q:deq(Q1),   
   {3, Q3} = q:deq(Q2),   
   {4, Q4} = q:deq(Q3),   
   {5, {}} = q:deq(Q4).   

q_hd_tl_test() ->
   Seq = lists:seq(1, 5),
   Q   = lists:foldl(fun q:enq/2, q:new(), Seq),
   1   = q:hd(Q),
   2   = q:hd(q:tl(Q)),
   3   = q:hd(q:tl(q:tl(Q))),
   4   = q:hd(q:tl(q:tl(q:tl(Q)))),
   5   = q:hd(q:tl(q:tl(q:tl(q:tl(Q))))),
   {}  = q:tl(q:tl(q:tl(q:tl(q:tl(Q))))).

q_dropwhile_test() ->
   Seq = lists:seq(1, 5),
   Q   = q:dropwhile(
      fun(X) -> X =< 2 end,
      lists:foldl(fun q:enq/2, q:new(), Seq)
   ),
   3   = q:hd(Q),
   4   = q:hd(q:tl(Q)),
   5   = q:hd(q:tl(q:tl(Q))),
   {}  = q:tl(q:tl(q:tl(Q))).

-endif.

