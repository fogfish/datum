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
-export([
   empty/1, stream/1, head/1, tail/1
]).
-export([
   '++'/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, primitives}
     ,{group, interface}
   ].

groups() ->
   [
      {primitives, [parallel], [empty, stream, head, tail]}
     ,{interface,  [parallel], ['++']}
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
   [2, 1, 3, 2, 1] = stream:list(
      stream:'++'(int(2), int(3))
   ),
   [2, 1, 3, 2, 1, 2, 1] = stream:list(
      stream:'++'([int(2), int(3), int(2)])
   ).
   
%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%% stream of integers
int(1) ->
   stream:new(1);
int(N) ->
   stream:new(N, fun() -> int(N - 1) end).

