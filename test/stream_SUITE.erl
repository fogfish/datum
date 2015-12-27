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

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, primitives}
   ].

groups() ->
   [
      {primitives, [parallel], [empty, stream, head, tail]}
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
%%% unit test
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
%%% unit test
%%%
%%%----------------------------------------------------------------------------   

