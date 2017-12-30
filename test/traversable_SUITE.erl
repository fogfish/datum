%% @doc
%%   
-module(traversable_SUITE).
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
   iterate/1,
   length/1,
   drop/1,
   dropwhile/1,
   filter/1,
   foreach/1,
   flatmap/1,
   map/1,
   partition/1,
   split/1,
   splitwhile/1,
   take/1,
   takewhile/1
]).


%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, stream},
      {group, heap},
      {group, q},
      {group, deq}
   ].

groups() ->
   [
      {stream, [parallel], 
         [iterate]},
         % [head, tail, length, drop, dropwhile, filter, foreach, flatmap, map, partition, split, splitwhile, take, takewhile]}

      {heap, [parallel],
         [iterate]},

      {q, [parallel],
         [iterate]},

      {deq, [parallel],
         [iterate]}         
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
iterate(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Tail = Type:build(List),
   false = Type:is_empty(Tail),
   iterate(Type:head(Tail), Type:tail(Tail), Type, List).

iterate(undefined, Tail, Type, _List) ->
   true = Type:is_empty(Tail);

iterate(Head, Tail, Type, List) ->
   true  = lists:member(element(Head), List),
   iterate(Type:head(Tail), Type:tail(Tail), Type, List).


%%
tail(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   Expect = tl(List),
   Result = Type:tail(Type:build(List)),
   is_equal(Type, Result, Expect).

%%
length(_Config) ->
   undefined.

%%
drop(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH),
   Type:drop(N, Type:build(List)).

%%
dropwhile(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH - 1),
   Type:dropwhile(fun(X) -> X < N end, Type:build(List)).

%%
filter(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   Pred   = fun(X) -> X rem 2 =:= 0 end,
   Expect = lists:filter(Pred, List),
   Result = Type:filter(Pred, Type:build(List)),
   is_equal(Type, Result, Expect).

%%
foreach(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   ok = Type:foreach(fun(X) -> X end, Type:build(List)).

%%
flatmap(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   Result = Type:flatmap(fun(X) -> Type:build([X]) end, Type:build(List)),
   is_equal(Type, Result, List).

%%
map(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   Expect = [X * 2 || X <- List],
   Result = Type:map(fun(X) -> X * 2 end, Type:build(List)),
   is_equal(Type, Result, Expect).


%%
partition(Config) ->
   Type   = ?config(type, Config),
   List   = shuffle(?LENGTH),
   Pred   = fun(X) -> X rem 2 =:= 0 end,
   {ExpectHead, ExpectTail} = lists:partition(Pred, List),
   {ResultHead, ResultTail} = Type:partition(Pred, Type:build(List)),
   is_equal(Type, ResultHead, ExpectHead),
   is_equal(Type, ResultTail, ExpectTail). 

%%
split(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH),
   {_, _} = Type:split(N, Type:build(List)).


%%
splitwhile(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH - 1),
   {_, _} = Type:splitwhile(fun(X) -> X < N end, Type:build(List)).

%%
take(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH),
   Type:take(N, Type:build(List)).

%%
takewhile(Config) ->
   Type   = ?config(type, Config),
   List   = seq(?LENGTH),
   N      = rand:uniform(?LENGTH - 1),
   Type:takewhile(fun(X) -> X < N end, Type:build(List)).

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
seq(N) ->
   lists:seq(1, N).

%%
randseq(0) -> [];
randseq(N) -> [rand:uniform(1 bsl 32) | randseq(N - 1)].

%%
shuffle(List) ->
   [Y || {_, Y} <- lists:keysort(1, [{rand:uniform(), X} || X <- List])].


%%
element({Key, _}) ->
   Key;
element(X) ->
   X.





%%
%% check traversable matches the list
is_equal(Type, Result, Expect) ->
   Sum = lists:sum(Expect),
   Sum = lists:sum(Type:list(Result)).

