%% @doc
%%   Class of data structures that can be traversed.
-module(traversable).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
   [
      %%
      %% take collection and return head element of collection
      %%
      %% -spec head(datum:traversable(_)) -> datum:option(_).
      {head, 1},

      %%
      %% take collection and return its suffix (all elements except the first)
      %%
      %% -spec tail(datum:traversable(_)) -> datum:traversable(_).
      {tail, 1},

      %%
      %% length of the collection
      %%
      %% -spec length(datum:traversable(_)) -> integer().
      {length, 1},

      %%
      %% return the suffix of collection that starts at the next element after nth.
      %% drop first n elements
      %%
      %% -spec drop(n, datum:traversable(_)) -> datum:traversable(_).
      {drop, 2},

      %%
      %% drops elements from collection while predicate returns true and 
      %% returns remaining stream suffix.
      %%
      %% -spec dropwhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).      
      {dropwhile, 2},

      %%
      %% returns a newly-allocated collection that contains only those elements of the 
      %% input collection for which predicate is true.
      %%
      %% -spec filter(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).
      {filter, 2},

      %%
      %% applies a function to each collection element for its side-effects; 
      %% it returns nothing.
      %%
      %% -spec foreach(datum:effect(_), datum:traversable(_)) -> ok.
      {foreach, 2},

      %%
      %% build a new collection by applying a function to all elements of this collection
      %% and flattering resulting collection
      %%
      %% -spec flatmap(fun((_) -> datum:traversable(_)), datum:traversable(_)) -> datum:traversable(_).
      {flatmap, 2},

      %%
      %% create a new collection by apply a function to each element of input collection.
      %% 
      %% -spec map(fun((_) -> _), datum:traversable(_)) -> datum:traversable(_).
      {map, 2},

      %%
      %% partition the collection in two collections according to a predicate
      %%
      %% -spec partition(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.
      {partition, 2},

      %%
      %% partitions collection into two collection. The split behaves as if it is defined as 
      %% consequent take(N, Seq), drop(N, Seq). 
      %%
      %% -spec split(integer(), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.
      {split, 2},

      %%
      %% partitions stream into two streams according to predicate.
      %% The splitwith/2 behaves as if it is defined as consequent 
      %% takewhile(Pred, Seq), dropwhile(Pred, Seq)
      %%
      %% -spec splitwhile(datum:predicate(_), datum:traversable(_)) -> {datum:traversable(_), datum:traversable(_)}.
      {splitwhile, 2},

      %%
      %% returns a newly-allocated collection containing the first n elements of 
      %% the input collection.
      %%
      %% -spec take(integer(), datum:traversable(_)) -> datum:traversable(_).
      {take, 2},

      %%
      %% returns a newly-allocated collection that contains those elements from 
      %% input collection while predicate returns true.
      %%
      %% -spec takewhile(datum:predicate(_), datum:traversable(_)) -> datum:traversable(_).
      {takewhile, 2},

      %%
      %% build a new collection from Erlang list
      %%
      %% -spec build([_]) -> datum:traversable(_).
      {build, 1},

      %%
      %% converts the collection to Erlang list
      %%
      %% -spec build(datum:traversable(_)) -> [_].
      {list, 1}
   ];
behaviour_info(_Other) ->
   undefined.
