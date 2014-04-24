-module(tree_tests).
-include_lib("eunit/include/eunit.hrl").

-define(N, 20).

bst_test_() ->
   {foreach,
      fun init/0,
      fun free/1,
      [
         fun lookup/1
        ,fun insert/1
        ,fun minmax/1
        ,fun map/1
        ,fun foldl/1
        ,fun foldr/1
        ,fun splitwith/1
        ,fun takewhile/1
        ,fun dropwhile/1
        ,fun take/1
        ,fun drop/1
      ]
   }.

init()  ->
   {bst, bst:build([{X, X} || X <- lists:seq(1, ?N)])}.

free(_) ->
   ok.

%%
%%
lookup({Mod, T}) ->
   ?_assertEqual(5,  Mod:lookup(5, T)).

%%
%%
insert({Mod, T}) ->
   [
      ?_assertEqual(el, Mod:lookup(99, Mod:insert(99, el, T)))
   ].

%%
%%
minmax({Mod, T}) ->
   [ 
      ?_assertEqual({1, 1},   Mod:min(T))
     ,?_assertEqual({?N, ?N}, Mod:max(T))
   ].
    
%%
%%
map({Mod, T}) ->
   T1 = Mod:build([{X, X*X} || X <- lists:seq(1,?N)]),
   [
      ?_assertEqual(T1, Mod:map(fun(K, V) -> {K, K * V} end, T))
   ].

%%
%%
foldl({Mod, T}) ->
   L = lists:seq(?N, 1, -1),
   [
      ?_assertEqual(L, Mod:foldl(fun(_, V, Acc) -> [V | Acc] end, [], T))
   ].

%%
%%
foldr({Mod, T}) ->
   L = lists:seq(1, ?N),
   [
      ?_assertEqual(L, Mod:foldr(fun(_, V, Acc) -> [V | Acc] end, [], T))
   ].

%%
%%
splitwith({Mod, T}) ->
   {A, B} = Mod:splitwith(fun(K, _) -> K < ?N div 2 end, T),
   [
      ?_assertMatch({1, _}, Mod:min(A))
     ,?_assertMatch({?N div 2 - 1, _}, Mod:max(A))
     ,?_assertMatch({?N div 2, _}, Mod:min(B))
     ,?_assertMatch({?N, _},  Mod:max(B))
   ].

%%
%%
takewhile({Mod, T}) ->
   A = Mod:takewhile(fun(K, _) -> K < ?N div 2 end, T),
   [
      ?_assertMatch({1, _}, Mod:min(A))
     ,?_assertMatch({?N div 2 - 1, _}, Mod:max(A))
   ].

%%
%%
dropwhile({Mod, T}) ->
   B = Mod:dropwhile(fun(K, _) -> K < ?N div 2 end, T),
   [
      ?_assertMatch({?N div 2, _}, Mod:min(B))
     ,?_assertMatch({?N, _},  Mod:max(B))
   ].


%%
%%
take({Mod, T}) ->
   A = Mod:take(?N div 2, T),
   [
      ?_assertMatch({1, _}, Mod:min(A))
     ,?_assertMatch({?N div 2, _}, Mod:max(A))
   ].

%%
%%
drop({Mod, T}) ->
   B = Mod:drop(?N div 2, T),
   [
      ?_assertMatch({?N div 2 + 1, _}, Mod:min(B))
     ,?_assertMatch({?N, _},  Mod:max(B))
   ].



