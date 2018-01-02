%% @doc
%%   Class of data structures that can be folded to a single value.
-module(foldable).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
   [
      %%
      %% Combine elements of a structure using a monoid
      %% (with an associative binary operation)
      %% 
      %% -spec fold(datum:monoid(_), _, datum:foldable(_)) -> _.
      {fold, 3},

      %%
      %% Left-associative fold of a structure
      %%
      %% -spec foldl(datum:monoid(_), _, datum:foldable(_)) -> _.
      {foldl, 3},

      %%
      %% Right-associative fold of a structure
      %%
      %% -spec foldr(datum:monoid(_), _, datum:foldable(_)) -> _.
      {foldr, 3},

      %% 
      %% The fundamental recursive structure constructor, 
      %% it applies a function to each previous seed element in turn
      %% to determine the next element.
      %%
      %% -spec unfold(fun((_) -> _), _) -> datum:foldable(_).
      {unfold, 2}
   ];
behaviour_info(_Other) ->
   undefined.
