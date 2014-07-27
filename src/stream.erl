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
%%   streams or lazy lists are a sequential data structure that contains 
%%   on demand computed elements. This module follows Scheme interface 
%%   see http://srfi.schemers.org/srfi-41/srfi-41.html
-module(stream).
-include("datum.hrl").

-export([
   new/0
  ,new/1
  ,new/2
  ,head/1
  ,tail/1
  ,constant/1
  ,drop/2
  ,dropwhile/2
  ,dropwhile/3
  ,filter/2
  ,fold/3
  ,foreach/2
  ,map/2
  ,scan/3
  ,split/2
  ,splitwith/2
  ,take/2
  ,takewhile/2
  ,unfold/2
  ,zip/1
  ,zip/2
  % utility
  ,list/1
  ,list/2
  ,prefix/1
  ,prefix/2  
  ,is_empty/1
  ,build/1
]).

%%
%% creates a newly allocated stream
-spec(new/0 :: () -> datum:stream()).
-spec(new/1 :: (any()) -> datum:stream()).
-spec(new/2 :: (any(), function()) -> datum:stream()).

new() ->
   ?NULL.
new(Head) ->
   new(Head, fun stream:new/0).
new(Head, Fun)
 when is_function(Fun) ->
   {s, Head, Fun}.

%%
%% head element of stream
-spec(head/1 :: (datum:stream()) -> any()).

head({s, Head, _}) ->
	Head;
head(_) ->
   exit(badarg).

%%
%% stream tail
-spec(tail/1 :: (datum:stream()) -> datum:stream()).

tail({s, _, Fun}) ->
	Fun();
tail(_) ->
   {}.

%%
%% takes list of elements and returns a newly-allocated stream composed of 
%% list elements, repeating them in succession forever.
-spec(constant/1 :: (list()) -> datum:stream()).

constant(List) -> 
	constant([], List).

constant([Head|Tail], List) ->
	new(Head, fun() -> constant(Tail, List) end);
constant([], [Head|Tail]=List) ->
	new(Head, fun() -> constant(Tail, List) end).

%%
%% returns the suffix of the input stream that starts at the next element after
%% the first n elements.
-spec(drop/2 :: (integer(), datum:stream()) -> datum:stream()).

drop(0, Stream) ->
   Stream;
drop(N, {s, _Head, Tail}) ->
  drop(N - 1, Tail());
drop(_, ?NULL) ->
	?NULL.

%%
%% drops elements from stream while predicate returns true and returns remaining
%% stream suffix.
-spec(dropwhile/2 :: (function(), datum:stream()) -> datum:stream()).
-spec(dropwhile/3 :: (function(), integer(), datum:stream()) -> {integer(), datum:stream()}).

dropwhile(Pred, {s, Head, Tail}=Stream) ->
   case Pred(Head) of
      true  -> 
      	dropwhile(Pred, Tail()); 
      false -> 
      	Stream
   end;
dropwhile(_, ?NULL) ->
   ?NULL.

dropwhile(Pred, N, {s, Head, Tail}=Stream) ->
   case Pred(Head) of
      true  -> 
         dropwhile(Pred, N + 1, Tail()); 
      false -> 
         {N, Stream}
   end;
dropwhile(_, N, ?NULL) ->
   {N, ?NULL}.

%%
%% returns a newly-allocated stream that contains only those elements x of the 
%% input stream for which predicate is true.
-spec(filter/2 :: (function(), datum:stream()) -> datum:stream()).

filter(Pred, {s, Head, Tail}) ->
   case Pred(Head) of
      true -> 
         new(Head, fun() -> filter(Pred, Tail()) end);
      false ->
         filter(Pred, Tail())
   end;
filter(_, ?NULL) ->
   ?NULL.


%%
%% applies a function to stream head and accumulator to compute a new accumulator,
%% then applies the function to the new base and the next element of stream to 
%% compute a succeeding base, and so on, the final accumulated value is returned
%% when the end of the stream is reached. Stream must be finite.
-spec(fold/3 :: (function(), any(), datum:stream()) -> any()).

fold(Fun, Acc, {s, Head, Tail}) ->
	fold(Fun, Fun(Head, Acc), Tail());
fold(_, Acc, ?NULL) ->
	Acc.


%%
%% applies a function to each stream element for its side-effects; 
%% it returns nothing. 
-spec(foreach/2 :: (function(), datum:stream()) -> ok).

foreach(Fun, {s, Head, Tail}) ->
	_ = Fun(Head),
	foreach(Fun, Tail());
foreach(_, ?NULL) ->
	ok.

%%
%% create a new stream by apply a function to each element of input stream. 
%% output stream contains elements that are results of the function.
-spec(map/2 :: (function(), datum:stream()) -> datum:stream()).

map(Fun, {s, Head, Tail}) ->
   new(Fun(Head), fun() -> map(Fun, Tail()) end);
map(_, ?NULL) ->
   ?NULL.

%%
%% accumulates the partial folds of an input stream into a newly-allocated
%% output stream
-spec(scan/3 :: (function(), any(), datum:stream()) -> datum:stream()).

scan(Fun, Acc0, {s, Head, Tail}) ->
	Acc = Fun(Head, Acc0),
	new(Acc, fun() -> scan(Fun, Acc, Tail()) end);
scan(_, Acc0, ?NULL) ->
	new(Acc0).

%%
%% partitions stream into two streams. The split behaves as if it is defined as 
%% consequent take(N, Stream), drop(N, Stream). The eos marker split streams 
-spec(split/2 :: (integer(), datum:stream()) -> datum:stream()).

split(1, {s, Head, Tail}) ->
   new(Head, fun() -> new(eos, Tail) end);
split(N, {s, Head, Tail}) ->
   new(Head, fun() -> split(N - 1, Tail()) end);
split(_, ?NULL) ->
   ?NULL.

%%
%% partitions stream into two streams according to predicate.
%% The splitwith/2 behaves as if it is defined as consequent 
%% takewhile(Pred, Stream), dropwhile(Pred, Stream)
-spec(splitwith/2 :: (function(), datum:stream()) -> datum:stream()).

splitwith(Pred, {s, Head, Tail}) ->
   case Pred(Head) of
      true  -> 
         new(Head, fun() -> splitwith(Pred, Tail()) end);
      false ->
         new(eos,  fun() -> new(Head, Tail) end)
     end;
splitwith(_, ?NULL) ->
   ?NULL.

%%
%% returns a newly-allocated stream containing the first n elements of 
%% the input stream. 
-spec(take/2 :: (integer(), datum:stream()) -> datum:stream()).

take(0, _) ->
	?NULL;
take(N, {s, Head, Tail}) ->
	new(Head, fun() -> take(N - 1, Tail()) end);
take(_, ?NULL) ->
   ?NULL.

%%
%% returns a newly-allocated stream that contains those elements from stream 
%% while predicate returns true.
-spec(takewhile/2 :: (function(), datum:stream()) -> datum:stream()).

takewhile(Pred, {s, Head, Tail}) ->
   case Pred(Head) of
      true  -> 
      	new(Head, fun() -> takewhile(Pred, Tail()) end);
      false ->
      	?NULL
     end;
takewhile(_, ?NULL) ->
   ?NULL.

%%
%% the fundamental recursive stream constructor, returns newly-allocated stream
%% that is constructed  by repeatedly applying function to seed
-spec(unfold/2 :: (any(), function()) -> datum:stream()).

unfold(Head, Fun)
 when is_function(Fun) ->
   new(Head, fun() -> unfold(Fun(Head), Fun) end).


%%
%% takes one or more input streams and returns a newly-allocated stream 
%% in which each element is a list of the corresponding elements of the input 
%% streams. The output stream is as long as the shortest input stream.
-spec(zip/1 :: ([datum:stream()]) -> datum:stream()).
-spec(zip/2 :: (datum:stream(), datum:stream()) -> datum:stream()).

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
%% return list of stream elements
-spec(list/1 :: (datum:stream()) -> list()).
-spec(list/2 :: (integer(), datum:stream()) -> list()).

list({s, _, _}=Stream) ->
   [stream:head(Stream) | list(stream:tail(Stream))];
list(_) ->
   [].

list(N, Stream) ->
   list(stream:take(N, Stream)).

%%
%% accumulate stream prefix to list and return remaining stream
-spec(prefix/1 :: (datum:stream()) -> {list(), datum:stream()}).
-spec(prefix/2 :: (function(), datum:stream()) -> {list(), datum:stream()}).

prefix(Stream) ->
   prefix(fun(X) -> X =/= eos end, Stream).

prefix(Pred, Stream) ->
   prefix(Pred, [], Stream).

prefix(_Pred, Acc, {s, eos, Tail}) ->
   {lists:reverse(Acc), Tail()};
prefix(Pred, Acc, {s, Head, Tail}=Stream) ->
   case Pred(Head) of
      true  ->
         prefix(Pred, [Head|Acc], Tail());
      false ->
         {lists:reverse(Acc), Stream}
   end;
prefix(_Fun, Acc, ?NULL) ->
   {lists:reverse(Acc), ?NULL}.

%%
%% check if stream is empty
-spec(is_empty/1 :: (datum:stream()) -> boolean()).

is_empty(?NULL) ->
   true;
is_empty(_) ->
   false.

%%
%% build stream from data type
-spec(build/1 :: (any()) -> datum:stream()).

build([]) ->
   new();
build([Head|Tail]) ->
   new(Head, fun() -> build(Tail) end).


