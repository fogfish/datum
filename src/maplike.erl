%% @doc
%%  map-like collection associates keys of type K to values of type V
-module(maplike).

-export([behaviour_info/1]).
-export([
   build/2,
   build/3,

   keys/2
]).

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


%%
%% build map-like structure from another one
-spec build(atom(), [_]) -> datum:maplike(_, _).

build(Type, Any) ->
   build(Type, fun datum:compare/2, Any).


%%
%% build map-like structure from another one
-spec build(atom(), datum:compare(_), [_]) -> datum:maplike(_, _).

build(Type, Ord, List)
 when is_list(List) ->
   lists:foldl(fun Type:append/2, Type:new(Ord), List).


%%
%% collects all keys of this collection to list
-spec keys(atom(), datum:maplike(_, _)) -> [_].

keys(Type, MapLike) ->
   Type:foldr(fun(K, _, Acc) -> [K|Acc] end, [], MapLike).

