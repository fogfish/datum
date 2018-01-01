# Features overview

* [Option type](#option-type)
* [Either type](#either-type)
* [Foldable](#foldable)
* [Traversable](#traversable)
* [Map-like](#map-like)
* [Pure functional data structures](#pure-functional-data-structures)
* [Stream](#stream)
* [Lens](#lens)
* [Category pattern](#category-pattern)
* [Monad](#monad)

<!-- * [Type safe cast](#type-safe-cast) -->
<!-- * [Topological](#topological) -->


## Option type

> In programming languages (more so functional programming languages) and type theory, an option type or maybe type is a polymorphic type that represents encapsulation of an optional value

This type is required to represent a results of computation that leads to nondeterministic state (undefined value). In Erlang, the atom `undefined` is frequently used for this purpose. The library adopts this notation to implement *option* type.

```erlang
-type option(X) :: undefined | X.
```

The library implements macro and type definition to emphasis the usage of *option* type. However, it only provides a semantical meaning and do not lead to any type safe at compile time. 

```erlang
-include_lib("datum/include/datum.hrl").

-spec f(datum:option(integer())) -> datum:option(integer()).

f(?None) -> ?None;    
f(?Some(X)) -> ?Some(X + 1).
```

**Note**: this definition of *option* type has a drawback if it is used with scalar data types (no issues with algebraic data types). The following example crashes unless developers explicitly match `undefined` atom in they code. Never the less, the given definition is compatible with existed convention in the community.

```erlang
f(?Some(X)) -> ?Some(X + 1).

f(?None).
```

## Either type

A typical usage is a representation of either correct -- right value or an error -- left value. Erlang has well established notation to use tagged tuples `{ok, _}` or `{error, _}` for this purpose. The library adopts this notation to implement *either* type.

```erlang
-type either(L, R) :: {error, L} | {ok, R}.
```

The library provides a macro to highlight *either* semantic in the code.

```erlang
-include_lib("datum/include/datum.hrl").

-spec f(datum:either(_, integer())) -> datum:either(_, integer()).

f(?EitherL(X)) -> ?EitherL(X);    
f(?EitherR(X)) -> ?EitherR(X + 1).
```

## Foldable

Foldable is a class of data structures that can be folded to a single value. The library defines a [`foldable`](../src/foldable.erl) behaviour and outline a common interface for each data structure that mixes in the behaviour:

```erlang
%%
%% Combine elements of a structure using a monoid
%% (with an associative binary operation)
%% 
-spec fold(datum:monoid(_), _, datum:foldable(_)) -> _.

%% 
%% The fundamental recursive structure constructor, 
%% it applies a function to each previous seed element in turn
%% to determine the next element.
%%
-spec unfold(fun((_) -> _), _) -> datum:foldable(_).
```

The library implements a **foldable** behaviour to *trees*, *heaps*, *queues* and *streams*. For example, you can fold a binary search tree to single value:

```erlang
bst:fold(
   fun({_, X}, Y) -> X + Y end,
   0,
   bst:build([{b, 2}, {a, 1}, {d, 4}, {c, 3}])
).
```

## Traversable

Traversable is a common interface for all kinds of collections. It defines the [`traversable`](../src/traversable.erl) behaviour common to all collections (See the interface for details).

As an example, it define a `foreach` method with similar signature to apply a side-effect to each element of collection:

```erlang
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.
``` 
Collection types provide a concrete `foreach` implementation which traverses all the elements contained in the collection.    

The traversable behaviour do not define strictness and orderedness of elements, each type has own properties:

* *Strict* collections each element is computed before they are used. *Non-strict* collection defer computation before the value is available as a value.
* *Ordered* collections ensures that its elements are always visited in the same order, even for different runs of computation. *Non-ordered* collection will the same order in same run only.

The library implements a **traversable** behaviour to *trees*, *heaps*, *queues* and *streams*. For example, you can visit each node of binary search tree:

```erlang
bst:foreach(
   fun(X) -> io:format("=> ~p~n", [X]) end,
   bst:build([{b, 2}, {a, 1}, {d, 4}, {c, 3}])
).
```

## Map-like

Map-like is a common behaviour for all kind of collection that associates keys of type `K` to values of type `V`. See [`maplike`](../src/maplike.erl) for details. The behaviour defines interface to insert, lookup and remove associations from collections.

The library implements a **map-like** behaviour to *trees* and *heaps*. For example, you can fold a list of pairs to binary search tree using append monoid:

```erlang
lists:foldl(fun bst:append/2, bst:new(), [{b, 2}, {a, 1}, {d, 4}, {c, 3}]).
```

## Pure functional data structures

Library implements a few pure functional data structures using common behvaiours:

* **Trees**: [binary search tree](../src/maplike/bst.erl), [red-black tree](../src/maplike/rbtree.erl), [hash tree (experemental)](../src/maplike/htree.erl)
* **Heaps**: [leftist heap](../src/maplike/heap.erl)
* **Queues**: [fifo](../src/queue/q.erl), [double ended queue](../src/queue/deq.erl)


## Stream

[Stream](../src/stream/stream.erl) (lazy list) is a sequential data structure that contains on demand computed elements. The library implements a data structure and stream transformers. 


## Lenses

[Lenses](../src/lens/lens.erl) resembles concept of getters and setters, which you can compose using functional concepts. In other words, this is combinator data transformation for pure functional data structure. The library uses van Laarhoven lens generalisation to solve lens scalability when you need to expends with new primitives or support new data types.

See details about [lenses](../src/lens/lens.erl)

For example, we can define a new lens to focus into binary search tree

```erlang
Lens = fun(Key) ->
   fun(Fun, Tree) -> 
      lens:fmap(
         fun(X) -> bst:insert(Key, X, Tree) end, 
         Fun(bst:lookup(Key, Tree))
      )
   end
end.

lens:get(Lens(a), lens:put(Lens(a), 1, bst:new())).
```



## Category pattern

The *composition* is a style of development to build a new things from small reusable elements. The category theory formalises principles (laws) that help us to define own abstraction applicable in functional programming through composition. 

See details about [category pattern](category.md). 


## Monads

The library implements rough Haskell's equivalent of **do-notation**, so called monadic binding form. This construction decorates computation pipeline(s) with additional rules implemented by monad, they defines programmable commas. The used syntax expresses various programming concepts in terms of a monad structures: side-effects, variable assignment, error handling, parsing, concurrency, domain specific languages, etc. 

See details about [Kleisli category](category.md) and [do-notation](monad.md). 
