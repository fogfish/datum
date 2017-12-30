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
%% @doc
%%   streams or lazy lists are a sequential data structure that contains 
%%   on demand computed elements. Erlang implementation derived from
%%   scheme stream interface @see http://srfi.schemers.org/srfi-41/srfi-41.html
-module(stream).
-behaviour(traversable).
-behaviour(foldable).

-include("datum.hrl").

%%
%% stream primitives - abstract data type 
-export([
   new/0,
   new/1,
   new/2,

   %%
   %% traversable
   head/1,
   tail/1,
   is_empty/1
]).

%%
%% stream interface
-export([
   '++'/2
  ,'++'/1
  ,drop/2
  ,dropwhile/2
  ,filter/2
  ,fold/3
  ,foreach/2
  ,flat/1
  ,map/2
  ,scan/2
  ,scan/3
  ,split/2
  ,splitwhile/2
  ,take/2
  ,takewhile/2
  ,unfold/2
  ,zip/1
  ,zip/2
  ,zipwith/2
  ,zipwith/3
]).

%%
%% stream utility
-export([
   reverse/1
  ,cycle/1
  ,list/1
  ,list/2
  ,build/1
]).

%%
%% inline stream primitives
-compile({inline,[new/0, new/1, new/2, head/1, tail/1]}).

%%%------------------------------------------------------------------
%%%
%%% stream primitives
%%%
%%%------------------------------------------------------------------

%%
%% creates a newly allocated stream containing stream head and promise.
%% the promise is recursive, it returns new stream pair when evaluated.
-spec new() -> datum:stream(_).
-spec new(_) -> datum:stream(_).
-spec new(_, function()) -> datum:stream(_).

new() ->
   #stream{}.

new(Head) ->
   new(Head, fun stream:new/0).

new(Head, Fun)
 when is_function(Fun, 0) ->
   #stream{head = Head, tail = Fun};
new(Head, ?None) ->
   new(Head).


%%%------------------------------------------------------------------
%%%
%%% traversable
%%%
%%%------------------------------------------------------------------

%%
%% take collection and return head element of collection
%%
-spec head(datum:traversable(_)) -> datum:option(_).

head(#stream{tail = ?None}) ->
   ?None;
head(#stream{head = Head}) ->
   Head.

%%
%% force stream promise and return new stream (evaluates tail of stream).
-spec tail(datum:traversable(_)) -> datum:traversable(_).

tail(#stream{tail = ?None} = Stream) ->
   Stream;
tail(#stream{tail = Fun}) ->
   Fun().


%%
%% return true if collection is empty 
%%
-spec is_empty(datum:traversable(_)) -> true | false.

is_empty(#stream{tail = ?None}) ->
   true;
is_empty(#stream{}) ->
   false.


%%%------------------------------------------------------------------
%%%
%%% stream interface
%%%
%%%------------------------------------------------------------------

%%
%% concatenate streams, returns newly-allocated stream composed of elements
%% copied from input streams (in order of input). 
-spec '++'(datum:stream(), datum:stream()) -> datum:stream().
-spec '++'([datum:stream()]) -> datum:stream().

'++'(?NULL, StreamB) ->
   StreamB;
'++'(StreamA, ?NULL) ->
   StreamA;
'++'(StreamA, StreamB) ->
   new(head(StreamA), fun() -> '++'(tail(StreamA), StreamB) end).

'++'([A,B|T]) ->
   '++'(['++'(A,B)|T]);
'++'([T]) ->
   T.


%%
%% returns the suffix of the input stream that starts at the next element after
%% the first n elements.
-spec drop(integer(), datum:stream()) -> datum:stream().

drop(0, Stream) ->
   Stream;
drop(N, {s, _, _} = Stream) ->
  drop(N - 1, tail(Stream));
drop(_, ?NULL) ->
   ?NULL.

%%
%% drops elements from stream while predicate returns true and returns remaining
%% stream suffix.
-spec dropwhile(function(), datum:stream()) -> datum:stream().

dropwhile(Pred, {s, _, _}=Stream) ->
   case Pred(head(Stream)) of
      true  -> 
         dropwhile(Pred, tail(Stream)); 
      false -> 
         Stream
   end;
dropwhile(_, ?NULL) ->
   ?NULL.


%%
%% returns a newly-allocated stream that contains only those elements x of the 
%% input stream for which predicate is true.
-spec filter(function(), datum:stream()) -> datum:stream().

filter(Pred, {s, _, _} = Stream) ->
   case Pred(head(Stream)) of
      true -> 
         new(head(Stream), fun() -> filter(Pred, tail(Stream)) end);
      false ->
         filter(Pred, tail(Stream))
   end;
filter(_, ?NULL) ->
   ?NULL.


%%
%% applies a function to stream head and accumulator to compute a new accumulator,
%% then applies the function to the new accumulator and the next element of stream to 
%% compute a succeeding accumulator, and so on, the final accumulated value is returned
%% when the end of the stream is reached. Stream must be finite.
-spec fold(function(), _, datum:stream()) -> _.

fold(Fun, Acc, {s, _, _} = Stream) ->
   fold(Fun, Fun(head(Stream), Acc), tail(Stream));
fold(_, Acc, ?NULL) ->
   Acc.


%%
%% applies a function to each stream element for its side-effects; 
%% it returns nothing. 
-spec foreach(function(), datum:stream()) -> ok.

foreach(Fun, {s, _, _} = Stream) ->
   _ = Fun(head(Stream)),
   foreach(Fun, tail(Stream));
foreach(_, ?NULL) ->
   ok.

%%
%% flat stream of streams
-spec flat(datum:stream()) -> datum:stream().

flat({s, {s, _, _} = Head, Tail}) ->
   new(stream:head(Head), fun() -> flat({s, stream:tail(Head), Tail}) end);

flat({s, {}, _} = Stream) ->
   flat(stream:tail(Stream));

flat(Stream) ->
   Stream.

%%
%% create a new stream by apply a function to each element of input stream. 
-spec map(function(), datum:stream()) -> datum:stream().

map(Fun, {s, _, _} = Stream) ->
   new(Fun(head(Stream)), fun() -> map(Fun, tail(Stream)) end);
map(_, ?NULL) ->
   ?NULL.

%%
%% accumulates the partial folds of an input stream into a newly-allocated stream.
%% the output stream is accumulator followed by partial fold.
-spec scan(function(), datum:stream()) -> datum:stream().
-spec scan(function(), _, datum:stream()) -> datum:stream().

scan(Fun, {s, _, _} = Stream) ->
   scan(Fun, head(Stream), tail(Stream)).

scan(Fun, Acc0, {s, _, _} = Stream) ->
   new(Acc0, fun() -> scan(Fun, Fun(head(Stream), Acc0), tail(Stream)) end);
scan(_, Acc0, ?NULL) ->
   new(Acc0).

%%
%% partitions stream into two streams. The split behaves as if it is defined as 
%% consequent take(N, Stream), drop(N, Stream). 
-spec split(integer(), datum:stream()) -> {[_], datum:stream()}.

split(N, Stream) ->
   split(N, [], Stream).

split(0, Acc, Stream) ->
   {lists:reverse(Acc), Stream};
split(N, Acc, {s, _, _} = Stream) ->
   split(N - 1, [head(Stream)|Acc], tail(Stream));
split(_, Acc, ?NULL) ->
   {lists:reverse(Acc), ?NULL}.

%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Stream), dropwhile(Pred, Stream)
-spec splitwhile(function(), datum:stream()) -> {[_], datum:stream()}.

splitwhile(Pred, Stream) ->
   splitwhile(Pred, [], Stream).

splitwhile(Pred, Acc, {s, _, _} = Stream) ->
   case Pred(head(Stream)) of
      true  ->
         splitwhile(Pred, [head(Stream)|Acc], tail(Stream));
      false ->
         {lists:reverse(Acc), Stream}
     end;
splitwhile(_, Acc, ?NULL) ->
   {lists:reverse(Acc), ?NULL}.


%%
%% returns a newly-allocated stream containing the first n elements of 
%% the input stream. 
-spec take(integer(), datum:stream()) -> datum:stream().

take(0, _) ->
   ?NULL;
take(N, {s, _, _} = Stream) ->
   new(head(Stream), fun() -> take(N - 1, tail(Stream)) end);
take(_, ?NULL) ->
   ?NULL.

%%
%% returns a newly-allocated stream that contains those elements from stream 
%% while predicate returns true.
-spec takewhile(function(), datum:stream()) -> datum:stream().

takewhile(Pred, {s, _, _} = Stream) ->
   case Pred(head(Stream)) of
      true  -> 
         new(head(Stream), fun() -> takewhile(Pred, tail(Stream)) end);
      false ->
         ?NULL
     end;
takewhile(_, ?NULL) ->
   ?NULL.


%%
%% the fundamental recursive infinite stream constructor. 
%% it applies a function to each previous seed element in turn
%% to determine the next element.
-spec unfold(function(), _) -> datum:stream().

unfold(Fun, Seed)
 when is_function(Fun) ->
   case Fun(Seed) of
      {Head, Next} ->
         new(Head, fun() -> unfold(Fun, Next) end);
      _ ->
         new()
   end.

%%
%% takes one or more input streams and returns a newly-allocated stream 
%% in which each element is a list of the corresponding elements of the input 
%% streams. The output stream is as long as the shortest input stream.
-spec zip([datum:stream()]) -> datum:stream().
-spec zip(datum:stream(), datum:stream()) -> datum:stream().

zip(Streams) ->
   case [head(X) || X <- Streams, X =/= ?NULL] of
      Head when length(Head) =:= length(Streams) ->
         new(Head, fun() -> zip([tail(X) || X <- Streams]) end);
      _ ->
      ?NULL
   end.

zip(A, B) ->
   zip([A, B]).

%%
%% takes one or more input streams and returns a newly-allocated stream,  
%% each element produced by composition function that map list of input heads to
%% new head. The output stream is as long as the longest input stream.
-spec zipwith(function(), [datum:stream()]) -> datum:stream().
-spec zipwith(function(), datum:stream(), datum:stream()) -> datum:stream().

zipwith(_, []) ->
   ?NULL;
zipwith(Fun, Streams) ->
   zipwith1(Fun, [head(X) || X <- Streams, X =/= ?NULL], Streams).

zipwith1(_Fun, [], _Streams) ->
   ?NULL;
zipwith1(Fun, Head, Streams) ->
   case Fun(Head) of
      [] ->
         ?NULL;
      Hx ->
         new(Hx, fun() -> zipwith(Fun, [tail(X) || X <- Streams, X =/= ?NULL]) end)
   end.
   
zipwith(Fun, A, B) ->
   zipwith(Fun, [A, B]).


%%%------------------------------------------------------------------
%%%
%%% stream utility
%%%
%%%------------------------------------------------------------------

%%
%% reverse order of elements in stream.
-spec reverse(datum:stream()) -> datum:stream().

reverse(Stream) ->
   reverse(Stream, new()).

reverse({s, _, _} = Stream, Acc) ->
   reverse(tail(Stream), new(head(Stream), Acc));

reverse(?NULL, Acc) ->
   Acc. 

%%
%% takes list of elements and returns a newly-allocated stream composed of 
%% list elements, repeating them in succession forever.
-spec cycle(list()) -> datum:stream().

cycle(List) -> 
   cycle([], List).

cycle([Head|Tail], List) ->
   new(Head, fun() -> cycle(Tail, List) end);
cycle([], [Head|Tail]=List) ->
   new(Head, fun() -> cycle(Tail, List) end).


%%
%% returns a newly-allocated list containing stream elements
-spec list(datum:stream()) -> list().
-spec list(integer(), datum:stream()) -> list().

list({s, _, _}=Stream) ->
   [stream:head(Stream) | list(stream:tail(Stream))];
list(_) ->
   [].

list(N, Stream) ->
   list(stream:take(N, Stream)).

%%
%% takes an object of Erlang data type are returns a newly-allocated stream
%%   list() -> stream contains the objects in the list. 
-spec build(any()) -> datum:stream().

build([]) ->
   new();
build([Head|Tail]) ->
   new(Head, fun() -> build(Tail) end);
build(X)
 when is_integer(X) ->
   new(X, fun() -> build(X + 1) end).


%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

