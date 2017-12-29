%% @doc
%%   
-module(foldable_SUITE).
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
   fold/1,
   foldl/1,
   foldr/1,
   unfold/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, stream}
   ].

groups() ->
   [
      {stream, [parallel], 
         [fold, foldl, foldr, unfold]}
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
init_per_group(Type, Config) ->
   [{type, Type}|Config].

end_per_group(_, _Config) ->
   ok.


%%%----------------------------------------------------------------------------   
%%%
%%% unit test
%%%
%%%----------------------------------------------------------------------------   
-define(LENGTH, 100).

%%
fold(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   Expect = lists:sum(List),

   Expect = Type:fold(fun erlang:'+'/2, 0, Type:build(List)). 

%%
foldl(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   Expect = lists:sum(List),

   Expect = Type:foldl(fun erlang:'+'/2, 0, Type:build(List)). 

%%
foldr(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   Expect = lists:sum(List),

   Expect = Type:foldr(fun erlang:'+'/2, 0, Type:build(List)). 

%%
unfold(Config) ->
   Type   = ?config(type, Config),
   Expect = lists:sum(lists:seq(1, ?LENGTH)),

   Expect = lists:sum(Type:list(Type:unfold(fun int/1, 1))).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

shuffle(0) -> [];
shuffle(N) -> [rand:uniform(1 bsl 32) | shuffle(N - 1)].

int(X)
 when X =< ?LENGTH ->
   {X, X + 1};
int(_) ->
   undefined.
