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
         [iterate, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]},

      {heap, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]},

      {q, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]},

      {deq, [parallel],
         [iterate, drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]},

      {bst, [parallel],
         [drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]},

      {rbtree, [parallel],
         [drop, dropwhile, filter, foreach, map, split, splitwhile, take, takewhile]}
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
   Struct = Type:build(List),
   {_, Empty} = Type:split(?LENGTH, Struct),
   {Empty, _} = Type:split(0, Struct).

%%
splitwhile(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Empty  = Type:new(),
   Struct = Type:build(List),
   {_, Empty} = Type:splitwhile(fun(_) -> true end, Struct),
   {Empty, _} = Type:splitwhile(fun(_) -> false end, Struct).

%%
take(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Struct = Type:build(List),
   Type:take(?LENGTH, Struct).


%%
takewhile(Config) ->
   Type   = ?config(type, Config),
   List   = randseq(?LENGTH),
   Struct = Type:build(List),
   Type:takewhile(fun(_) -> true end, Struct).


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

%%
el2({_, Val}) ->
   Val;
el2(X) ->
   X.

