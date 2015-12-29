-module(stream_SUITE).
-include_lib("common_test/include/ct.hrl").

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

%%
%% stream primitive
-export([
   empty/1, stream/1, head/1, tail/1
]).

%%
%% stream interface
-export([
   '++'/1, drop/1, dropwhile/1, filter/1, fold/1, foreach/1, map/1, 
   scan/1, split/1, splitwhile/1, take/1, takewhile/1, unfold/1, zip/1, zipwith/1
]).

%%
%% stream utility
-export([
   reverse/1, cycle/1
]).

%%
%% stream algorithm
-export([
   prime/1
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
      {group, primitives}
     ,{group, interface}
     ,{group, utility}
     ,{group, algorithm}
   ].

groups() ->
   [
      {primitives, [parallel], 
         [empty, stream, head, tail]}

     ,{interface,  [parallel], 
         ['++', drop, dropwhile, filter, fold, foreach, map, 
          scan, split, splitwhile, take, takewhile, unfold, zip, zipwith]}

     ,{utility,    [parallel],
         [reverse, cycle]}

     ,{algorithm,  [parallel],
         [prime]}
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
%%% stream primitives
%%%
%%%----------------------------------------------------------------------------   

empty(_Config) ->
   {} = stream:new().


stream(_Config) ->
   Null = fun stream:new/0,
   {s, test, Null} = stream:new(test),

   Lazy = fun() -> stream:new() end,
   {s, test, Lazy} = stream:new(test, Lazy).
   

head(_Config) ->
   Stream = stream:new(test, fun() -> stream:new(tset) end),
   test = stream:head(Stream).


tail(_Config) ->
   Null   = fun stream:new/0,
   Stream = stream:new(test, fun() -> stream:new(tset) end),
   {s, tset, Null} = stream:tail(Stream).

%%%----------------------------------------------------------------------------   
%%%
%%% stream interface
%%%
%%%----------------------------------------------------------------------------   

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
   
drop(_Config) ->
   ?prefix([3, 4],
      stream:drop(2, stream:build(1))
   ).

dropwhile(_Config) ->
   ?prefix([3, 4],
      stream:dropwhile(fun(X) -> X =< 2 end, stream:build(1))
   ).

filter(_Config) ->
   ?prefix([2, 4, 6],
      stream:filter(fun(X) -> X rem 2 == 0 end, stream:build(1))
   ).

fold(_Config) ->
   15 = stream:fold(fun erlang:'+'/2, 0, 
      stream:take(5, stream:build(1))
   ).

foreach(_Config) ->
   ok = stream:foreach(fun(_) -> ok end, 
      stream:take(5, stream:build(1))
   ).

map(_Config) ->
   ?prefix([1, 4, 9, 16, 25],
      stream:map(fun(X) -> X * X end, stream:build(1))
   ).

scan(_Config) ->
   ?prefix([0, 1, 3, 6, 10, 15], 
      stream:scan(fun erlang:'+'/2, 0, stream:build(1))
   ),
   ?prefix([1, 3, 6, 10, 15, 21], 
      stream:scan(fun erlang:'+'/2, stream:build(1))
   ).

split(_Config) ->
   {[1, 2, 3], Stream} = stream:split(3, stream:build(1)),
   ?prefix([4, 5, 6], Stream).

splitwhile(_Config) ->
   {[1, 2, 3], Stream} = stream:splitwhile(fun(X) -> X < 4 end, stream:build(1)),
   ?prefix([4, 5, 6], Stream).
   
take(_Config) ->
   ?prefix([1, 2],
      stream:take(2, stream:build(1))
   ).

takewhile(_Config) ->
   ?prefix([1, 2],
      stream:takewhile(fun(X) -> X < 3 end, stream:build(1))
   ).

unfold(_Config) ->
   ?prefix([0, 1, 4, 9, 16, 25, 36, 49, 64, 81],
      stream:map(
         fun(X) -> X * X end,
         stream:takewhile(
            fun(X) -> X < 10 end,
            stream:unfold(0, fun(X) -> X + 1 end)
         )
      )
   ).

zip(_Config) ->
   ?prefix([[1, 1], [2, 4], [3, 9], [4, 16]],
      stream:zip(
         stream:build(1),
         stream:map(fun(X) -> X * X end, stream:build(1))
      )
   ).

zipwith(_Config) ->
   ?prefix([[1, 1], [4, 2], [9, 3], [16, 4]],
      stream:zipwith(
         fun(X) -> lists:reverse(lists:sort(X)) end,
         stream:build(1),
         stream:map(fun(X) -> X * X end, stream:build(1))
      )
   ).


%%%----------------------------------------------------------------------------   
%%%
%%% stream utility
%%%
%%%----------------------------------------------------------------------------   

reverse(_Config) ->
   ?prefix([5, 4, 3, 2, 1],
      stream:reverse(stream:take(5, stream:build(1)))
   ).

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



