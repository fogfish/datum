%% @doc
%%   Class of data structures that maintain a topological relation between actors
-module(topological).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
   [
      %%
      %% Join an actor to topology and returns a new topology.
      %%
      %% -spec join(key(), datum:topological(_)) -> datum:topological().
      {join, 2},

      %%
      %% Leave an actor from topology, and returns a new topology.
      %% Note, semantic of leave operation do not assume leave due to transitive failures.
      %%
      %% -spec leave(key(), datum:topological(_)) -> datum:topological().
      {leave, 2},

      %%
      %% Check is an actor exists in topology
      %%
      %% -spec has(key(), datum:topological(_)) -> true | false.
      {has, 2},

      %%
      %% Looks up an topology and returns location of any arbitrary key
      %%
      %% -spec whereis(key(), datum:topological(_)) -> {address(), key()}.
      {whereis, 2},

      %%
      %% Return list of topology members
      %%
      %% -spec members(datum:topological(_)) -> [key()].
      {members, 1}
   ];
behaviour_info(_Other) ->
   undefined.
