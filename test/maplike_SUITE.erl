%% @doc
%%   
-module(maplike_SUITE).
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
   append/1,
   append1/1,
   insert/1,
   insert1/1,
   lookup/1,
   remove/1,
   has/1,
   keys/1,
   apply/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, bst},
      {group, rbtree},
      {group, heap}
   ].

groups() ->
   [
      {bst, [parallel], 
         [append, insert, lookup, remove, has, keys, apply]},

      {rbtree, [parallel], 
         [append, insert, lookup, remove, has, keys, apply]},

      {heap, [parallel], 
         [append1, insert1, keys]}         
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
append(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Keys = [Key || {Key, _} <- List],
   LMap0 = lists:foldl(fun Type:append/2, Type:new(), List),
   LMap0 = lists:foldl(fun Type:append/2, LMap0, shuffle(List)),

   LMap1 = lists:foldl(fun Type:append/2, Type:new(), Keys),
   LMap1 = lists:foldl(fun Type:append/2, LMap1, shuffle(Keys)).

%%
append1(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Keys = [Key || {Key, _} <- List],
   lists:foldl(fun Type:append/2, Type:new(), List),
   lists:foldl(fun Type:append/2, Type:new(), Keys).

%%
insert(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Lens = fun({Key, Val}, Acc) -> Type:insert(Key, Val, Acc) end,
   LMap = lists:foldl(Lens, Type:new(), List),
   LMap = lists:foldl(Lens, LMap, shuffle(List)).

%%
insert1(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   Lens = fun({Key, Val}, Acc) -> Type:insert(Key, Val, Acc) end,
   LMap = lists:foldl(Lens, Type:new(), List).


%%
lookup(Config) ->
   Type = ?config(type, Config), 
   List = randseq(?LENGTH),
   LMap = Type:build(List),
   undefined = Type:lookup(any, LMap),
   lists:foreach(
      fun({Key, Val}) ->
         Val = Type:lookup(Key, LMap)
      end,
      shuffle(List)
   ).

%%
remove(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   LMap = Type:build(List),
   LMap = Type:remove(any, LMap),
   Empty= Type:new(),
   Empty= lists:foldl(
      fun({Key, _}, Acc) ->
         Type:remove(Key, Acc)
      end,
      LMap,
      shuffle(List)
   ).


%%
has(Config) ->
   Type = ?config(type, Config), 
   List = randseq(?LENGTH),
   LMap = Type:build(List),
   false = Type:has(any, LMap),
   lists:foreach(
      fun({Key, _}) ->
         true = Type:has(Key, LMap)
      end,
      shuffle(List)
   ).


%%
keys(Config) ->
   Type = ?config(type, Config),
   List = randseq(?LENGTH),
   LMap = Type:build(List),
   Keys = lists:sort([Key || {Key, _} <- List]),
   Keys = lists:sort(Type:keys(LMap)).


%%
apply(Config) ->
   Type  = ?config(type, Config),
   List  = randseq(?LENGTH),
   LMap0 = Type:build(List),
   LMap1 = lists:foldl(
      fun({Key, _}, Acc) -> 
         Type:apply(Key, fun '+'/1, Acc)
      end,
      LMap0,
      shuffle(List)
   ),
   LMap0 = lists:foldl(
      fun({Key, _}, Acc) -> 
         Type:apply(Key, fun '-'/1, Acc)
      end,
      LMap1,
      shuffle(List)
   ),
   Type:apply(any, fun '+'/1, LMap0).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
randseq(0) -> [];
randseq(N) -> [{rand:uniform(1 bsl 32), rand:uniform(1 bsl 32)} | randseq(N - 1)].

%%
%%
shuffle(List) ->
   [Y || {_, Y} <- lists:keysort(1, [{rand:uniform(), X} || X <- List])].



%%
%%
'+'(undefined) ->
   undefined;
'+'(X) ->   
   X + 1.

'-'(undefined) ->
   undefined;
'-'(X) ->   
   X - 1.
