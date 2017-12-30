%% @doc
%%  map-like collection associates keys of type K to values of type V
-module(maplike).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
   [
      %%
      %% append a new key/value pair to collection
      %%
      %% -spec append({key(_), val(_)}, datum:maplike(_, _)) -> datum:maplike(_, _).
      {append, 2},

      %%
      %% insert a new a key/value pair to collection
      %%
      %% -spec insert(key(_), val(_), datum:maplike(_, _)) -> datum:maplike(_, _).
      {insert, 3}, 

      %%
      %% optionally returns the value associated with key
      %%
      %% -spec lookup(key(_), datum:maplike(_, _)) -> datum:option( val(_) ).
      {lookup, 2}, 

      %%
      %% remove key/value pair from collection 
      %%
      %% -spec remove(key(_), datum:maplike(_, _)) -> datum:maplike(_, _).
      {remove, 2}, 

      %%
      %% check if the collection has an association
      %%
      %% -spec has(key(_), datum:maplike(_, _)) -> true | false.
      {has, 2},

      %%
      %% collects all keys of this collection to list
      %%
      %% -spec keys(datum:maplike(_, _)) -> [_].
      {keys, 1},

      %%
      %% optionally apply a function to value associated with key
      %%
      %% -spec apply(key(_), fun((datum:option(_)) -> _), datum:maplike(_, _)) -> datum:maplike(_, _).
      {apply, 3}
   ];
behaviour_info(_Other) ->
   undefined.
