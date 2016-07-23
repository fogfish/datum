-module(datum_benchmark).

-export([
   new/1, run/4
]).

-define(N,    512).
-define(RING, [
   {m,     64}
  ,{q,   4096}
  ,{n,      3}
  ,{hash, sha}
]).

%%%------------------------------------------------------------------
%%%
%%% factory
%%%
%%%------------------------------------------------------------------

new(_) ->
   {ok, init(basho_bench_config:get(struct, bst))}.

%%
%% datum structure
init(stream) ->  {stream, stream()};
init(bst)    ->  {bst,    bst:new()};
init(rbtree) ->  {rbtree, rbtree:new()};
init(chord)  ->  {chord,  ring(chord)};
init(ring)   ->  {ring,   ring(ring)};
init(lens)   ->  {lens,   erlang:make_tuple(?N, <<>>)};

%%
%% erlang built-in data types
init(dict)     -> {dict,  dict:new()};
init(gb_trees) -> {gb_trees, gb_trees:empty()};
init(tuple)    -> {tuple, erlang:make_tuple(?N, <<>>)}.



%%%------------------------------------------------------------------
%%%
%%% stream
%%%
%%%------------------------------------------------------------------

run(head, _KeyGen, _ValGen, {stream, S0}) ->
   _ = stream:head(S0),
   {ok, {stream, S0}};

run(tail, _KeyGen, _ValGen, {stream, S0}) ->
   S1 = stream:tail(S0),
   {ok, {stream, S1}};

%%%------------------------------------------------------------------
%%%
%%% bst
%%%
%%%------------------------------------------------------------------

run(insert, KeyGen, ValGen, {bst, S0}) ->
   S1 = bst:insert(KeyGen(), ValGen(), S0),
   {ok, {bst, S1}};

run(lookup, KeyGen, _ValGen, {bst, S0}) ->
   _ = bst:lookup(KeyGen(), S0),
   {ok, {bst, S0}};

run(remove, KeyGen, _ValGen, {bst, S0}) ->
   S1 = bst:remove(KeyGen(), S0),
   {ok, {bst, S1}};

%%%------------------------------------------------------------------
%%%
%%% rbtree
%%%
%%%------------------------------------------------------------------

run(insert, KeyGen, ValGen, {rbtree, S0}) ->
   S1 = rbtree:insert(KeyGen(), ValGen(), S0),
   {ok, {rbtree, S1}};

run(lookup, KeyGen, _ValGen, {rbtree, S0}) ->
   _ = rbtree:lookup(KeyGen(), S0),
   {ok, {rbtree, S0}};

%%%------------------------------------------------------------------
%%%
%%% chord
%%%
%%%------------------------------------------------------------------

run(whereis, _KeyGen, ValGen, {chord, S0}) ->
   _ = chord:whereis(ValGen(), S0),
   {ok, {chord, S0}};

run(predecessors, _KeyGen, ValGen, {chord, S0}) ->
   _ = chord:predecessors(ValGen(), S0),
   {ok, {chord, S0}};

run(successors,   _KeyGen, ValGen, {chord, S0}) ->
   _ = chord:successors(ValGen(), S0),
   {ok, {chord, S0}};

%%%------------------------------------------------------------------
%%%
%%% ring
%%%
%%%------------------------------------------------------------------

run(whereis, _KeyGen, ValGen, {ring, S0}) ->
   _ = ring:whereis(ValGen(), S0),
   {ok, {ring, S0}};

run(predecessors, _KeyGen, ValGen, {ring, S0}) ->
   _ = ring:predecessors(ValGen(), S0),
   {ok, {ring, S0}};

run(successors,   _KeyGen, ValGen, {ring, S0}) ->
   _ = ring:successors(ValGen(), S0),
   {ok, {ring, S0}};

%%%------------------------------------------------------------------
%%%
%%% lens
%%%
%%%------------------------------------------------------------------

run(put, KeyGen, ValGen,  {lens, S0}) ->
   L   = lens:new(KeyGen() rem ?N + 1),
   % L   = lens:new(tuple, KeyGen() rem ?N + 1),
   Val = ValGen(),
   S1  = lens:put(L, Val, S0),
   {ok, {lens, S1}};

run(get, KeyGen, _ValGen, {lens, S0}) ->
   L = lens:new(KeyGen() rem ?N + 1),
   % L = lens:new(tuple, KeyGen() rem ?N + 1),
   _ = lens:get(L, S0),
   {ok, {lens, S0}};


%%%------------------------------------------------------------------
%%%
%%% dict
%%%
%%%------------------------------------------------------------------

run(store, KeyGen, ValGen, {dict, S0}) ->
   S1 = dict:store(KeyGen(), ValGen(), S0),
   {ok, {dict, S1}};

run(find,  KeyGen, _ValGen, {dict, S0}) ->
   _ = dict:find(KeyGen(), S0),
   {ok, {dict, S0}};

%%%------------------------------------------------------------------
%%%
%%% gb_trees
%%%
%%%------------------------------------------------------------------

run(enter, KeyGen, ValGen, {gb_trees, S0}) ->
   S1 = gb_trees:enter(KeyGen(), ValGen(), S0),
   {ok, {gb_trees, S1}};

run(lookup, KeyGen, _ValGen, {gb_trees, S0}) ->
   _ = gb_trees:lookup(KeyGen(), S0),
   {ok, {gb_trees, S0}};


% run(create, KeyGen, ValGen, {bloom, S0}) ->
%    S1 = bloom:insert(KeyGen(), S0),
%    {ok, {bloom,   S1}};

% run(create, KeyGen, ValGen, {dict, S0}) ->
%    S1 = dict:store(KeyGen(), ValGen(), S0),
%    {ok, {dict,  S1}};

% run(create, KeyGen, ValGen, {array, S0}) ->
%    S1 = array:set(KeyGen(), ValGen(), S0),
%    {ok, {array, S1}};

% run(lookup, KeyGen, ValGen, {bloom, S0}) ->
%    bloom:lookup(KeyGen(), S0),
%    {ok, {bloom, S0}}.

%%%------------------------------------------------------------------
%%%
%%% tuple
%%%
%%%------------------------------------------------------------------

run(put, KeyGen, ValGen,  {tuple, S0}) ->
   Key = KeyGen() rem ?N + 1,
   Val = ValGen(),
   S1  = erlang:setelement(Key, S0, Val),
   {ok, {tuple, S1}};

run(get, KeyGen, _ValGen, {tuple, S0}) ->
   Key = KeyGen() rem ?N + 1,
   _ = erlang:element(Key, S0),
   {ok, {tuple, S0}}.

%%%------------------------------------------------------------------
%%%
%%% private
%%%
%%%------------------------------------------------------------------

stream() ->
   random:seed(os:timestamp()),
   stream:constant(
      [random:uniform(16#ffffffff) || _ <- lists:seq(1, ?N)]
   ).

%%
ring(Mod) ->
   io:format("init ring: "),
   random:seed(os:timestamp()),
   Ring = lists:foldl(
      fun(_, Acc) ->
         io:format("."),
         Key = list_to_binary(integer_to_list(random:uniform(16#ffffffff))),
         Val = list_to_binary(integer_to_list(random:uniform(16#ffffffff))),
         Mod:join(Key, Val, Acc)
      end,
      Mod:new(?RING),
      lists:seq(1, ?N)
   ),
   io:nl(),
   Ring.

