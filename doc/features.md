# Features overview

* [Option type](#option-type)
* [Either type](#either-type)
<!-- * [Type safe cast](#type-safe-cast) -->
* [Foldable](#foldable)
* [Traversable](#traversable)
* [Map-like](#map-like)
* [Topological](#topological)
* [Trees](#trees)
* [Queues](#queues)
* [Streams](#stream)
* [Lenses](#lenses)
* [Category pattern](#category-pattern)
* [Monads](#monads)

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

**Note**: this definition of *option* type has a drawback. The following example crashes unless developers explicitly match `undefined` atom in they code. Still, the given definition is most compatible with existed convention in the community. 

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

<!--
## Type safe cast

TBD
-->

## Foldable

Foldable is a class of data structures that can be folded to a single value. The library defines a behaviour [`foldable`](src/foldable.erl) and outline a common interface for each data structure that mixes in the behaviour.

```erlang
-spec add(integer(), integer()) -> integer().

add(A, B) -> A + B.

rbtree:fold(fun add/1, ...).
stream:fold(fun add/1, ...).
q:fold(fun add/1, ...).
```

## Traversable

Traversable is a common behaviour for all kinds of collections. It defines the [`traversable`](src/traversable.erl) interface common to all collections. As an example, it define a `foreach` method with similar signature to apply a side-effect to each element of collection:

```erlang
-spec foreach(datum:effect(_), datum:traversable(_)) -> ok.
``` 
Collection types provide a concrete `foreach` implementation which traverses all the elements contained in the collection.    

The traversable behaviour do not define strictness and orderedness of elements, each type has own properties:

* *Strict* collections each element is computed before they are used. *Non-strict* collection defer computation before the value is available as a value.
* *Ordered* collections ensures that its elements are always visited in the same order, even for different runs of computation. *Non-ordered* collection will the same order in same run only.

```erlang
-spec inc(integer(), integer()) -> integer().

inc(X) -> X + 1.

rbtree:map(fun inc/1, ...).
stream:map(fun inc/1, ...).
q:map(fun inc/1, ...).
```

## Map-like

Map-like is a common behaviour for all kind of collection that associates keys of type `K` to values of type `V`. 

Monoid to fold lists into collection

```erlang
lists:foldl(fun rbtree:append/2, rbtree:new(), ...).
```

## Topological

xxx

## Trees

xxx

## Queues

xxx


## Streams

xxx



## Lenses

xxx



## Category pattern

xxx



## Monads

xxx

