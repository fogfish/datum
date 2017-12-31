%% @doc
%%   
-module(foldable_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0,
   groups/0,
   init_per_suite/1,
   end_per_suite/1,
   init_per_group/2,
   end_per_group/2
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
      {group, stream},
      {group, heap}
   ].

groups() ->
   [
      {stream, [parallel], 
         [fold, foldl, foldr, unfold]},

      {heap, [parallel],
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
   List   = randseq(?LENGTH),
   Expect = lists:sum(List),
   Expect = Type:fold(fun '+'/2, 0, Type:build(List)). 

%%
foldl(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Expect = lists:sum(List),
   Expect = Type:foldl(fun '+'/2, 0, Type:build(List)). 

%%
foldr(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Expect = lists:sum(List),
   Expect = Type:foldr(fun '+'/2, 0, Type:build(List)). 

%%
unfold(Config) ->
   Type   = ?config(type, Config),
   Expect = lists:sum(lists:seq(1, ?LENGTH)),
   Struct = Type:unfold(fun int/1, 1),
   Expect = Type:fold(fun '+'/2, 0, Struct). 


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
randseq(0) -> [];
randseq(N) -> [rand:uniform(1 bsl 32) | randseq(N - 1)].

%%
shuffle(List) ->
   [Y || {_, Y} <- lists:keysort(1, [{rand:uniform(), X} || X <- List])].

%%
el1({Key, _}) ->
   Key;
el1(X) ->
   X.

int(X)
 when X =< ?LENGTH ->
   {X, X + 1};
int(_) ->
   undefined.

'+'(X, Acc) ->
   el1(X) + Acc.
