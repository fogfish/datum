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
   insert/1,
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
      {group, bst}
   ].

groups() ->
   [
      {bst, [parallel], 
         [insert, lookup, remove, has, keys, apply]}
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
insert(Config) ->
   Type  = ?config(type, Config),
   Type:insert(1, 1, Type:new()).


%%
lookup(Config) ->
   Type = ?config(type, Config), 
   List = shuffle(?LENGTH),
   {Key, Val} = hd(List),
   Val  = Type:lookup(Key, Type:build(List)),
   undefined = Type:lookup(undefined, Type:build(List)).

%%
remove(Config) ->
   Type = ?config(type, Config),
   List = shuffle(?LENGTH),
   {Key, Val} = hd(List),
   undefined = Type:lookup(Key, Type:remove(Key, Type:build(List))).

%%
has(Config) ->
   Type   = ?config(type, Config),
   List = shuffle(?LENGTH),
   {Key, Val} = hd(List),
   true  = Type:has(Key, Type:build(List)),
   false = Type:has(undefined, Type:build(List)).


%%
keys(Config) ->
   Type = ?config(type, Config),
   List = shuffle(?LENGTH),
   Keys = [Key || {Key, _} <- List],
   Keys = Type:keys(Type:build(List)).


%%
apply(Config) ->
   Type = ?config(type, Config),
   List = shuffle(?LENGTH),
   {Key, Val} = hd(List),
   Val = Type:lookup(Key, Type:apply(Key, fun '+'/1, Type:build(List))) - 1.

%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

%%
%%
shuffle(0) -> [];
shuffle(N) -> [{rand:uniform(1 bsl 32), rand:uniform(1 bsl 32)} | shuffle(N - 1)].

%%
%%
'+'(undefined) ->
   undefined;
'+'(X) ->   
   X + 1.