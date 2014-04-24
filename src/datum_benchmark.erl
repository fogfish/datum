-module(datum_benchmark).

-export([
   new/1, run/4
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
init(bst)    ->  {bst,    bst:new()};
init(rbtree) ->  {rbtree, bst:new()};
%%
%% erlang built-in data types
init(dict)     -> {dict,  dict:new()};
init(gb_trees) -> {gb_trees, gb_trees:empty()}.

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

%%%------------------------------------------------------------------
%%%
%%% rbtree
%%%
%%%------------------------------------------------------------------

run(insert, KeyGen, ValGen, {rbtree, S0}) ->
   S1 = bst:insert(KeyGen(), ValGen(), S0),
   {ok, {rbtree, S1}};

run(lookup, KeyGen, _ValGen, {rbtree, S0}) ->
   _ = bst:lookup(KeyGen(), S0),
   {ok, {rbtree, S0}};

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
   {ok, {gb_trees, S0}}.



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


