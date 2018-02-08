# Lens

Lenses resembles concept of getters and setters, which you can compose using functional concepts. In other words, this is combinator data transformation for pure functional data structure. This library implements lens using approaches on Haskell lens library, and techniques references by [1].


Lens types are defined as ... They are following the convention of Haskell lens library.

```erlang
%% type of object
-type s() :: _.

%% type of focused element (focus type)  
-type a() :: _.  
```


Originally, lenses are defined using `get` and `put` primitives. The third primitive `over` (or `map`) allows to enhance lens behavior using function (e.g. the `put` is `map` using `const` function).   

```erlang
-type lens() :: 
   {
      fun( (s()) -> a() ),                        %% get
      fun( (fun( (a()) -> a() ), s()) -> s() )    %% map
   }.
```


This naive lens structure is not scalable when you need to expends with new primitives or 
support new data types. You either grow it by implementing various flavors of getters and 
setters or extend module to support new data types.

van Laarhoven lens generalization solves the problem, the proposal to use functor to 
implement `get`, `put`, `map`, etc. The lens is defined as 

```
type Lens s a = Functor f => (a -> f a) -> s -> f s
```

Note: there is a good tutorial about type classes and functors  
* http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
* http://scalaz.github.io/scalaz/scalaz-2.9.1-6.0.2/doc.sxr/scalaz/Functor.scala.html
 
Functors do not exists in Erlang. This lens module define one with minimal runtime overhead.

```erlang
-type f(F) :: [atom()|F].
-spec fmap( fun((a()) -> _), f(a()) ) -> f(_).
```

Let's skip all details on the design decision about the function definition below. 
In the nutshell, various Erlang native containers (tuple, function, etc) are evaluated.
The list shown best performance. There is not any intent to generalize functor concept to Erlang application, it is made to support only lens implementation. Lenses implementation requires two type of functors: identity for `over` (`map`) and const for `get`.


We can define van Laarhoven lens type

```erlang
-type lens(A, S) :: fun( (fun( (A) -> f(A) ), S) -> f(S) ).
-type lens()     :: lens(a(), s()).
```


## Interface


### Map

It is defined as... Given a `lens()` that focuses on `a()` inside of `s()`, and
a function `a() -> a()` and instance of object `s()`. It returns modified `s()` by applying
the function to focus point of the lens

```haskell
over :: Lens s a -> (a -> a) -> s -> s
```

```erlang
-spec map(fun( (a()) -> a() ), lens(), s()) -> s().
```


### Get

It is defined as... Given a `lens()` that focuses on `a()` inside of `s()`, and 
instance of object `s()`. It returns value of focus point `a()`. 

```haskel
view :: Lens s a -> s -> a
```

```erlang
-spec get(lens(), s()) -> a().
```


### Put

It is defined as... Given a `lens()` that focuses on `a()` inside of `s()`, and
value `a()` and instance of object `s()`. It returns modified `s()` by setting 
a new value to focus point of the lens.


### Iso

Isomorphism translates between different data structures. Given a `lens()` that focuses on
multiple `a()` values inside of `s()`. It lifts result (ordered set of `a()`) to abstract view.
Another `lens()` puts abstract view back to another target data structure.


## Lenses

Generic lens interface above requires actual lens implementation. This library implements a basic set of lenses to focus on Erlang built-in types: lists, maps, records, tuples, keylists. Anyone can implement a custom lens.

**Lens** is a **function** of `lens()` type.

in Haskell

```haskell
Functor f => (a -> f a) -> s -> f s
```

in Erlang

```
-type lens() :: fun( (fun( (a()) -> f(a()) ), s() ) -> f(s()) ).
```

As an example, Let's define a lens that focuses on head of list `fun lens:hd/2`

```erlang
hd() ->
   fun(Fun, [H|T]) ->
      lens:fmap(fun(X) -> [X|T] end, Fun(H))
   end.
```


The lens usage is straight forward:

```erlang
lens:get(lens:hd(), [1,2]).                       %% 1
lens:put(lens:hd(), 5, [1, 2]).                   %% [5, 2]
lens:map(fun(X) -> X + 1 end, lens:hd(), [1, 2]). %% [3, 2]
```


Well behaving lens satisfies following laws

**GetPut** if we get focused element `a()` from `s()` and immediately put `a()` with no modifications back into `s()`, we must get back exactly `s()`.

```erlang
[a] = lens:put(lens:hd(), lens:get(lens:hd(), [a]), [a]).
```

**PutGet** if putting `a()` inside `s()` yields a new `s()`, then the `a()` obtained from `s()` is exactly `a()`.

```erlang
b = lens:get(lens:hd(), lens:put(lens:hd(), b, [a])).
```

**PutPut** A sequence of two puts is just the effect of the second, the first gets completely overwritten. This law is applicable to every well behaving lenses.

```
[c] = lens:put(lens:hd(), c, lens:put(lens:hd(), b, [a])).
```


### Ω-lenses

Lens fails if focus is not exists. Ω-lenses are capable to recover a create a new container `s()` from nothing. The Omega variant(s) is usable for practical application to construct nested data type but they are not well behaving.

This library implements two variant of lenses that return option type or Ω-variants:

```
undefined = lens:get(lens:hd(),  []).
1 = lens:get(lens:hd(1), []).
```



### Composition

The lens **composition** is powerful concept to produce complex lenses to deal with deeply nested data structures. The lens composition is a solution to assemble a lenses. 

Let's take an example, there is list of tuple [{1,2,3}], the composition of lens:hd(), fun lens:t2() allows to focus on second element on tuple:

```erlang
Lens = lens:c(lens:hd(), lens:t2()).
lens:get(Lens, [{1, 2, 3}]).    %% 2
lens:put(Lens, 6, [{1, 2, 3}]). %% [{1, 6, 3}]
```


The **product lens** composes lenses to spawn multiple fields at once.

```erlang
LensA = lens:c(lens:hd(), lens:t2()).
LensB = lens:c(lens:hd(), lens:t3()).
Lens  = lens:p(LensA, LensB).

lens:get(Lens, [{1, 2, 3}]).         %% [2, 3].
lens:put(Lens, [6, 5], [{1, 2, 3}]). %% [{1, 6, 5}]
```



## Refrences

1. [Combinators for Bi-Directional Tree Transformations: A Linguistic Approach to the View Update Problem](http://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf)
1. https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial
1. [Lens tutorial by Jakub Arnold](https://blog.jakuba.net/2014/08/06/lens-tutorial-stab-traversal-part-2.html)

There are other approaches to implement lens for Erlang

* https://github.com/jlouis/erl-lenses by Jesper Louis Andersen
* http://www.cs.otago.ac.nz/staffpriv/ok/lens.erl by Richard A. O'Keefe
