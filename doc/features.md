# Features overview

* [Infix functions](#infix-functions) 
* [Option type](#option-type)
* [Either type](#either-type)
* [Foldable](#foldable)
* [Traversable](#traversable)
* [Map-like](#map-like)
* [Pure functional data types](#pure-functional-data-types)
* [Stream](#stream)
* [Lens](#lens)
* [Category pattern](#category-pattern)
* [Monad](#monad)

<!-- * [Type safe cast](#type-safe-cast) -->
<!-- * [Topological](#topological) -->

## Infix functions

In Erlang, we apply functions using prefix notation - name is followed by its arguments in brackets `plus(1, 2)`. Humans are more used to infix operators than prefix or postfix. If a function takes two arguments, we have the option of using it in infix form, where we place it between its first and second arguments `1 /plus/ 2`. To apply a function using infix notation, we enclose its name in slash characters (/). This allows us to use functions as infix operators.

See details about [infix functions](infix.md). 


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

## Pure functional data types

Library implements a few pure functional data structures using common behvaiours:

* **Trees**: [binary search tree](../src/maplike/bst.erl), [red-black tree](../src/maplike/rbtree.erl), [hash tree (experemental)](../src/maplike/htree.erl)
* **Heaps**: [leftist heap](../src/maplike/heap.erl)
* **Queues**: [fifo](../src/queue/q.erl), [double ended queue](../src/queue/deq.erl)


## Stream

[Stream](../src/stream/stream.erl) (lazy list) is a sequential data structure that contains on demand computed elements. The library implements a data structure and stream transformers. 


## Lens

Data access and transformation are common tasks in computing where changes made to the structure are reflected as updates to the original structure. This structure update problem is a classical in imperative languages.

```
user.address.street = "Blumenstraße"
```

This operation is complicated in functional languages. In order to change a value of inner structure, we need to re-assign values of multiple structures along the path. Maintenance and refactoring of this functions becomes very tedious compared to imperative languages

```erlang
set_street_name(Value, #user{address = #address{} = Address} = User) ->
   User#user{address = Address#address{street = Value}}.
```

Functional languages solve this issue using [lens](lens.md). It resembles concept of getters and setters, which you can compose using functional concepts. 

```erlang
lens_street_name() ->
   lens:c(lens:ti(#user.address), lens:ti(#address.street)).

lens:get(lens_street_name(), User).
lens:set(lens_street_name(), "Blumenstraße", User).
```

Isomorphism of data structures is another problem solved by lenses. The application often operates with the abstract formats of business objects but communication with clients requires concrete format. We can define a single common format and a collection of lenses that transform each concrete format into this abstract one.

```erlang
iso_to_map() ->
   lens:iso(
      [
         lens:ti(#user.name),
         lens:c(lens:ti(#user.address), lens:ti(#address.street))
      ], 
      [
         lens:at(name),
         lens:c(lens:at(address, #{}), lens:at(name))
      ]
   ).

lens:isof(iso_to_map(), #user{name = "Verner", address = #address{street = "Blumenstraße"}}, #{}).
lens:isob(iso_to_map(), #{name => "Verner", address => #{street => "Blumenstraße"}}, #user{}).
```

See details about [lens](lens.md)

The scalability of lens implementation is another issue, you need to expands lenses with new primitives, support new data types or define custom lenses. The library uses van Laarhoven lens generalisation to solve lens scalability.

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


## Monad

The library implements rough Haskell's equivalent of **do-notation**, so called monadic binding form. This construction decorates computation pipeline(s) with additional rules implemented by monad, they defines programmable commas. The used syntax expresses various programming concepts in terms of a monad structures: side-effects, variable assignment, error handling, parsing, concurrency, domain specific languages, etc. 

See details about [Kleisli category](category.md) and [do-notation](monad.md). 
