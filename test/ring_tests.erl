-module(ring_tests).
-include_lib("eunit/include/eunit.hrl").

-define(HASH, sha).
-define(M,      8).
-define(N,      3).
-define(Q,     16).
-define(KEYS, [
   {<<"deadbeef">>, self()}
  ,{<<"beeff00d">>, self()}
  ,{<<"f001cafe">>, self()}
  ,{atom, self()}
  ,{list, self()}
  ,{hash, self()}
  ,{30,   self()}
  ,{128,  self()}
  ,{150,  self()}
]).

chord_test_() ->
   {foreach,
      fun chord/0,
      fun free/1,
      [
         fun size/1
        ,fun addresses/1
        ,fun address_addr/1
        ,fun address_hash/1
        ,fun address_binary/1
        ,fun address_term/1
        ,fun whereis/1
        ,fun predecessors/1
        ,fun successors/1
        ,fun lookup/1
      ] 
   }.

rint_test_() ->
   {foreach,
      fun ring/0,
      fun free/1,
      [
         fun size/1
        ,fun addresses/1
        ,fun address_addr/1
        ,fun address_hash/1
        ,fun address_binary/1
        ,fun address_term/1
        ,fun whereis/1
        ,fun predecessors/1
        ,fun successors/1
        ,fun lookup/1
      ] 
   }.


chord() ->
   {chord, 
      lists:foldl(
         fun({Key, Val}, Acc) -> chord:join(Key, Val, Acc) end,
         chord:new([{hash, ?HASH}, {m, ?M}, {n, ?N}, {q, ?Q}]), 
         ?KEYS
      )
   }.

ring() ->
   {ring, 
      lists:foldl(
         fun({Key, Val}, Acc) -> ring:join(Key, Val, Acc) end,
         ring:new([{hash, ?HASH}, {m, ?M}, {n, ?N}, {q, ?Q}]), 
         ?KEYS
      )
   }.

free(_) ->
   ok.

%%
%%
size({Mod, Ring}) ->
   ?_assertEqual(length(?KEYS), Mod:size(Ring)).

%%
%%
addresses({Mod, Ring}) ->
   Top = trunc(math:pow(2, ?M)),
   Inc = Top div ?Q,
   Set = lists:seq(Inc - 1, Top - 1, Inc),
   ?_assertEqual(Set, Mod:address(Ring)).

%%
%%
address_addr({Mod, Ring}) ->
   Top  = trunc(math:pow(2, ?M)),
   Addr = random:uniform(10 * Top),
   ?_assertEqual(Addr rem Top, Mod:address(Addr, Ring)).

address_hash({Mod, Ring}) ->
   <<Addr:?M, _/bits>> = Key = crypto:hash(?HASH, <<"qazwsxedc">>),
   ?_assertEqual(Addr, Mod:address({hash, Key}, Ring)).

address_binary({Mod, Ring}) ->
   Key = <<"qazwsxedc">>,
   <<Addr:?M, _/bits>> = crypto:hash(?HASH, Key),
   ?_assertEqual(Addr, Mod:address(Key, Ring)).

address_term({Mod, Ring}) ->
   Key = {<<"qaz">>, <<"wsx">>, <<"edc">>},
   <<Addr:?M, _/bits>> = crypto:hash(?HASH, erlang:term_to_binary(Key)),
   ?_assertEqual(Addr, Mod:address(Key, Ring)).

%%
%%
whereis({Mod, Ring}) ->
   Key  = <<"qazwsxedc">>,
   Addr = Mod:address(Key, Ring),
   ?_assert(Addr < erlang:element(1, Mod:whereis(Key, Ring))).

%%
%%
predecessors({Mod, Ring}) ->
   Key = <<"qazwsxedc">>,
   {Addr, _, _} = Mod:whereis(Key, Ring),
   {Head, Tail} = lists:splitwith(
      fun({X, _, _}) -> X =/= Addr end, 
      Mod:successors(?Q, 0, Ring) %% return shards
   ),
   ?_assert(
      lists:prefix(Mod:predecessors(Key, Ring), lists:reverse(Head) ++ lists:reverse(Tail))
   );
predecessors(_) ->
   [].

%%
%%
successors({Mod, Ring}) ->
   Key  = <<"qazwsxedc">>,
   {Addr, _, _} = Mod:whereis(Key, Ring),
   {Head, Tail} = lists:splitwith(
      fun({X, _, _}) -> X =/= Addr end, 
      Mod:successors(?Q, 0, Ring) %% return shards
   ),
   ?_assert(
      lists:prefix(Mod:successors(Key, Ring), Tail ++ Head)
   );
successors(_) ->
   [].

%%
%%
lookup({Mod, Ring}) ->
   {Key, Pid} = lists:nth(random:uniform(length(?KEYS)), ?KEYS),
   List = lists:filter(
      fun({_, X, _}) -> X =:= Key end,
      Mod:successors(?Q, 0, Ring)
   ),
   ?_assertEqual(List, Mod:lookup(Key, Ring));
lookup(_) ->
   [].




