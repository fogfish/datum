# Category pattern

> Composition is is the essence of programming...

The *composition* is a style of development to build a new things from small reusable elements. The category theory formalises principles (laws) that help us to define own abstraction applicable in functional programming through composition. The *composition* becomes a fundamental operation: the *codomain* of *f* be the *domain* of *g* so that the composite operation *f* ◦ *g* is defined. 

A **category** is a concept that is defined in abstract terms of **objects**, **arrows** together with two functions **composition** (`.`) and **identity** (`id`). These functions shall be compliant with category laws

1. *Associativity* : `(f . g) . h = f . (g . h)` 
2. *Left identity* : `id . f = f` 
3. *Right identity* : `f . id = f` 

The category leaves the definition of *object*, *arrows*, *composition* and *identity* to us. This gives a powerful abstraction in functional programming and makes a functional programming language look very much like a category.

Let's consider an example of category `𝕽`

* objects are real number `𝐑` 
* arrows are functions `𝐑 ⟶ 𝐑`.
* identity `id x = x`
* composition `(f . g) x = g (f x)`

This example using Erlang 

```erlang
id(X) -> X.
'.'(F,G) -> fun(X) -> G(F(X)) end.

%% example (X + 3) ^ 2
f(X) -> X + 3.
g(X) -> X * X.

h(X) -> ('.'(fun f/1, fun g/1))(X).  %% g(f(X)).
```

## Composition in Erlang

Erlang do not support infix notation. This notation improves readability of functional *composition* and allows to implement complex compositions. The chaining of ordinary functions do not allow us to make arbitrary program.  

The `parse_transform` feature implements a syntax sugar for *composition*, which is compiled into valid Erlang code, using defined composition operator at compile-time.  

The *function composition* within the category are written with the following syntax

```
[Category || Arrow1, ..., ArrowN]
```
Here, `Category` is an identity of category and each `Arrow` is a morphism applied to objects. 

The example composition within category `𝕽` becomes

```erlang
h(X) ->
    [$. ||      %% composition operator (chaining of function)
        f(X),   %% arrow 𝐑 ⟶ 𝐑, feeds result to next arrow 
        g(_),   %% arrow 𝐑 ⟶ 𝐑, feeds result to next arrow  
        ...     %% and so on
    ].
```

and it is compiled into `g(f(X))`.

### Composition with state

A pure functional programming language does not have variables or assignment statements. The program is defined by applying type constructors, constants and functions. However, this principle do not closely match current architectures. Programs are implemented using variables such as memory lookups and updates. These operations are popular because memory lookup is implemented by a single indexed fetch instruction, and memory update by a single indexed store. We are not going into long debates if this feature is desirable.  

> Since so much effort has gone into developing algorithms and architectures based on arrays, we will sidestep this debate and simply assume the answer is yes.

Let's imagine a function `(1 / X - 3)^2 + (1 / Y - 5)^2 ≤ 1 / R^2`. It is trivial to implement it using chaining but intermediate state helps to understand function better.

```erlang
%% with chaining
g(X, Y, R)
    lt(pow(sub(div(1, X), 3), 2) + pow(sub(div(1, Y), 5), 2), pow(R, 2)).

%% with intermediate state
f(X, Y, R) ->    
    A = sub(div(1, X), 3),
    B = sub(div(1, Y), 5),
    C = div(1, R),
    lt(pow(A, 2) + pow(B, 2), pow(C, 2)).
```
   
We define a syntactic extension to *composition* operator that **yields the state** into variable. It is written with the following syntax

```
[Category || Var1 <- Arrow1, ..., VarN <- ArrowN]
```

Here, Category is an identity of category and each Arrow is a morphism applied to objects, `Var` is a named or blank variable that preserve intermediate state. Formally, this is a composition of functions: `f . update . g`.

Our example becomes... 

```erlang
f(X, Y, R) ->
    [$. ||
       div(1, X),
       sub(_, 3),
       A <- pow(_, 2), %% (1 / X - 3)^2
       div(1, Y),
       sub(_, 5),
       B <- pow(_, 2), %% (1 / Y - 5)^2
       pow(R, 2),
       lt(A + B, _)
    ].
```

The usage of intermediate state do not benefit for chains of ordinary functions. Unfortunately, we can't express all of our programs as chains of ordinary functions. Later' we will demonstrate the benefit of intermediate states for computation with a side-effect. 

### Composition with transformers

Category pattern provides a powerful abstraction to build computations. Each category is specialises to compose one type of *objects*. In reality, we need to use several categories at once. The composition requires a transformations such as category to category or object within category, as an example a lifting of the object into category.

Transformers are written using following syntax

```
[Category || cats:transform(...), ..., VarN /= transform(...) ]
```

Here, Category is an identity of category, `cats:` is reserved prefix that executes the defined transformation as part of the composition, `/= `yields the result of transformation to the state. Formally, this is a composition of functions: `f . transform . update . g`.


## Categories

### Identity

### Option

### Either

### Reader

### Kleisli (Monads)




# `$^` either

```erlang
[$^|| a(), b(_)].

case a() of
   {ok, X} -> 
      b(X);
   {error, _} = Error ->
      Error
end.
```

```erlang
[$^|| a(), b(_, _)].

case a() of
   {ok, X, Y} -> 
      b(X, Y);
   {error, _} = Error ->
      Error
end.
```

```erlang
[$^|| a(), b()].

case a() of
   {error, _} = Error ->
      Error
   _ -> 
      b();
end.
```

## References

1. [The category design pattern](http://www.haskellforall.com/2012/08/the-category-design-pattern.html)
2. [Category Theory for Computing Science](http://www.math.mcgill.ca/triples/Barr-Wells-ctcs.pdf)
