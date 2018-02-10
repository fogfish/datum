%%
%%   Copyright (c) 2015, Dmitry Kolesnikov
%%   All Rights Reserved.
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
-module(stream_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("datum/include/datum.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

-export([
   new/1,
   empty/1,

   head/1,
   tail/1,
   is_empty/1,
   drop/1,
   dropwhile/1,
   filter/1,
   foreach/1,
   map/1,
   split/1,
   splitwhile/1,
   take/1,
   takewhile/1,

   fold/1,
   unfold/1,

   '++'/1,
   flat/1,
   scan/1,
   zip/1,
   zipwith/1,
   reverse/1,
   cycle/1   
]).

%%
%% stream algorithm
-export([
   prime/1, union/1, join/1, unique/1
]).

%%
%% assert stream prefix
-define(prefix(L, S), L = stream:list(length(L), S)).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, stream},
      {group, algorithm}
   ].

groups() ->
   [
      {stream, [parallel], 
         [new, empty, head, tail, is_empty, drop, dropwhile, filter, foreach, map,
         split, splitwhile, take, takewhile, fold, unfold, '++', flat, scan,
         zip, zipwith, reverse, cycle]},

      {algorithm,  [parallel],
         [prime, union, join, unique]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% stream
%%%
%%%----------------------------------------------------------------------------   

%%
new(_) ->
   Null = fun stream:new/0,
   undefined = stream:new(),
   {stream, 1, Null} = stream:new(1),

   Fun = fun() -> stream:new() end,
   {stream, 1, Fun} = stream:new(1, Fun).

empty(_) ->
   ?stream() = stream:new().

%%
head(_Config) ->
   Stream = stream:new(1, fun() -> stream:new(1) end),
   1 = stream:head(Stream).

%%
tail(_Config) ->
   Null = fun stream:new/0,
   Stream = stream:new(1, fun() -> stream:new(2) end),
   {stream, 2, Null} = stream:tail(Stream).

%%
is_empty(_Config) ->
   true  = stream:is_empty(stream:new()),
   false = stream:is_empty(stream:new(1)).

%%
drop(_Config) ->
   ?prefix([3, 4],
      stream:drop(2, stream:build(1))
   ).

%%
dropwhile(_Config) ->
   ?prefix([3, 4],
      stream:dropwhile(fun(X) -> X =< 2 end, stream:build(1))
   ).

%%
filter(_Config) ->
   ?prefix([2, 4, 6],
      stream:filter(fun(X) -> X rem 2 == 0 end, stream:build(1))
   ).

%%
foreach(_Config) ->
   ok = stream:foreach(fun(_) -> ok end, 
      stream:take(5, stream:build(1))
   ).

%%
map(_Config) ->
   ?prefix([1, 4, 9, 16, 25],
      stream:map(fun(X) -> X * X end, stream:build(1))
   ).

%%
split(_Config) ->
   {Head, Tail} = stream:split(3, stream:build(1)),
   ?prefix([1, 2, 3], Head),
   ?prefix([4, 5, 6], Tail).

%%
splitwhile(_Config) ->
   {Head, Tail} = stream:splitwhile(fun(X) -> X < 4 end, stream:build(1)),
   ?prefix([1, 2, 3], Head),
   ?prefix([4, 5, 6], Tail).

%%
take(_Config) ->
   ?prefix([1, 2],
      stream:take(2, stream:build(1))
   ).

%%
takewhile(_Config) ->
   ?prefix([1, 2],
      stream:takewhile(fun(X) -> X < 3 end, stream:build(1))
   ).

%%
fold(_Config) ->
   15 = stream:fold(fun erlang:'+'/2, 0, 
      stream:take(5, stream:build(1))
   ).

%%
unfold(_Config) ->
   ?prefix([1, 4, 9, 16, 25, 36, 49, 64, 81],
      stream:map(
         fun(X) -> X * X end,
         stream:takewhile(
            fun(X) -> X < 10 end,
            stream:unfold(fun(X) -> {X + 1, X + 1} end, 0)
         )
      )
   ).


%%
'++'(_Config) ->
   ?prefix([1, 2, 3, 3, 4, 5],
      stream:'++'(
         stream:take(3, stream:build(1)), 
         stream:build(3)
      )
   ),
   ?prefix([1, 2, 3, 1, 2, 3, 3, 4, 5], 
      stream:'++'([
         stream:take(3, stream:build(1)), 
         stream:take(3, stream:build(1)),
         stream:build(3)
      ])
   ).

%%
flat(_Config) ->
   ?prefix([1, 2, 3, 3, 4, 5],
      stream:flat(
         stream:map(
            fun(X) -> stream:new(X) end,
            stream:build([1, 2, 3, 3, 4, 5])
         )
      )
   ).

%%
scan(_Config) ->
   ?prefix([0, 1, 3, 6, 10, 15], 
      stream:scan(fun erlang:'+'/2, 0, stream:build(1))
   ),
   ?prefix([1, 3, 6, 10, 15, 21], 
      stream:scan(fun erlang:'+'/2, stream:build(1))
   ).

%%
zip(_Config) ->
   ?prefix([[1, 1], [2, 4], [3, 9], [4, 16]],
      stream:zip(
         stream:build(1),
         stream:map(fun(X) -> X * X end, stream:build(1))
      )
   ).

%%
zipwith(_Config) ->
   ?prefix([[1, 1], [4, 2], [9, 3], [16, 4]],
      stream:zipwith(
         fun(X) -> lists:reverse(lists:sort(X)) end,
         stream:build(1),
         stream:map(fun(X) -> X * X end, stream:build(1))
      )
   ).

%%
reverse(_Config) ->
   ?prefix([5, 4, 3, 2, 1],
      stream:reverse(stream:take(5, stream:build(1)))
   ).

%%
cycle(_Config) ->
   ?prefix([1, 2, 1, 2, 1, 2],
      stream:cycle([1, 2])
   ).

%%%----------------------------------------------------------------------------   
%%%
%%% stream algorithms (examples)
%%%
%%%----------------------------------------------------------------------------   

%%
%% prime number lookup
%% https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
%%
prime(_Config) ->
   ?prefix([2, 3, 5, 7, 11, 13, 17, 19, 23, 29],
      sieve(stream:build(2))
   ).   

sieve(Stream) ->
   sieve(stream:head(Stream), Stream).

sieve(Prime, Stream) ->
   stream:new(
      Prime, 
      fun() -> 
         sieve(
            stream:filter(
               fun(X) -> X rem Prime =/= 0 end, 
               stream:tail(Stream)
            )
         ) 
      end
   ).

%%
%% union of ordered streams - combine streams in order of heads 
%% returns newly allocated stream in which elements are ordered by key
union(_Config) ->
   ?prefix([{1,a}, {2,d}, {3,b}, {4,c}, {5,e}, {6,g}],
      stream:unfold(fun sunion/1, [
         stream:build([{2,d}, {6,g}]),
         stream:build([{1,a}, {5,e}]),
         stream:build([{3,b}, {4,c}])
      ])
   ).

sunion(Streams) ->
   % sort each stream using its head. 
   case sortby(1, Streams) of
      [Head|Tail] ->
         % the lowest head contributes to output stream,
         % the stream with lowest head is evaluated to next position
         % all remaining streams are seeded to next iteration
         {stream:head(Head), [stream:tail(Head)|Tail]};
      _     ->
         {undefined, []}
   end.

sortby(N, Streams) ->
   lists:sort(
      fun(A, B) -> 
         erlang:element(N, stream:head(A)) =< erlang:element(N, stream:head(B)) 
      end,
      [X || X <- Streams, not stream:is_empty(X)]
   ).

%%
%% takes one or more input streams and returns a newly-allocated
%% stream in which each element is a joined by key
join(_Config) ->
   ?prefix([{1,[a,d]}, {3,[b,g]}, {4,[c,e]}],
      stream:unfold(fun sjoin/1, [
         stream:build([{1,d}, {3,g}]),
         stream:build([{1,a}, {4,e}]),
         stream:build([{3,b}, {4,c}])
      ])
   ).

sjoin(Streams) ->
   % sort each stream using its head. 
   case sortby(1, Streams) of
      [H| _] = List ->
         % the key element of lowest stream is attractor,
         % it splits stream to its that share same key 
         Key = erlang:element(1, stream:head(H)),
         {Head, Tail} = lists:splitwith(
            fun(X) -> erlang:element(1, stream:head(X)) =:= Key end,
            List
         ),
         % the streams with same key are used to build new head
         Hd  = {Key, lists:sort([erlang:element(2, stream:head(X)) || X <- Head])},
         % remaining stream and its re-evaluated siblings are contribute to next iteration
         Seed= lists:foldl(fun(X, Acc) -> [stream:tail(X) | Acc] end, Tail, Head),
         {Hd, Seed};
      _     ->
         {undefined, []}
   end.

%%
%% remove continues, duplicated elements from stream
unique(_Config) ->
   ?prefix([1,2,3,4,5],
      sunique( stream:build([1,1,1,2,2,2,2,3,3,3,4,4,4,4,5,5,5]) )
   ).   

sunique(Stream) ->
   sunique(stream:is_empty(Stream), Stream).

sunique(false, Stream) ->
   stream:new(stream:head(Stream), fun() -> sunique(stream:dropwhile(fun(X) -> X =:= stream:head(Stream) end, Stream)) end);
sunique(true, Stream) ->
   Stream.

