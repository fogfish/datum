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
   drop/1,
   dropwhile/1,
   filter/1,
   foreach/1,
   map/1,
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
      {group, deq},
      {group, bst},
      {group, rbtree}
   ].

groups() ->
   [
      {stream, [parallel], 
         [iterate, drop, dropwhile, filter, foreach, map, split]},
         % [split, splitwhile, take, takewhile]}

      {heap, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split]},

      {q, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split]},

      {deq, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split]},

      {bst, [parallel],
         [drop, dropwhile, filter, foreach, map, split]},

      {rbtree, [parallel],
         [drop, dropwhile, filter, foreach, map, split]}
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
   true  = lists:member(el1(Head), List),
   iterate(Type:head(Tail), Type:tail(Tail), Type, List).


%%
drop(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   Empty  = Type:drop(?LENGTH, Type:build(List)).

%%
dropwhile(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   Empty  = lists:foldl(
      fun(Key, Acc) ->
         Type:dropwhile(fun(X) -> el1(X) =/= Key end, Acc)
      end,
      Type:build(List),
      shuffle(List)
   ).

%%
filter(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   Empty  = lists:foldl(
      fun(Key, Acc) ->
         Type:filter(fun(X) -> el1(X) =/= Key end, Acc)
      end,
      Type:build(List),
      shuffle(List)
   ).

%%
foreach(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   ok = Type:foreach(fun(X) -> X end, Type:build(List)).


%%
map(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   Empty  = Type:filter(
      fun(X) -> not el2(X) end,
      Type:map(fun(X) -> lists:member(el1(X), List) end, Type:build(List))
   ).

%%
split(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   {_, Empty} = Type:split(?LENGTH, Type:build(List)),
   {Empty, _} = Type:split(0, Type:build(List)).



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
el1({Key, _}) ->
   Key;
el1(X) ->
   X.

%%
el2({_, Val}) ->
   Val;
el2(X) ->
   X.




%%
%% check traversable matches the list
is_equal(Type, Result, Expect) ->
   Sum = lists:sum(Expect),
   Sum = lists:sum(Type:list(Result)).

