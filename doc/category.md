# Category

> Composition is is the essence of programming...

The category theory defines a *composition* as a fundamental operation. The *codomain* of *f* be the *domain* of *g* so that the composite operation *f* ◦ *g* is defined. The *composition* is not a new concept, this is a style of development to build a new things from small reusable elements. The category theory formalises principles (laws) that help us to define own abstraction applicable in functional programming.

A **category** is defined in terms of **objects**, **arrows** together with two functions **composition** (`.`) and **identity** (`id`). These functions shall be compliant with category laws

1. `(f . g) . h = f . (g . h)` *Associativity*
2. `id . f = f` *Left identity*
3. `f . id = f` *Right identity*

The category leaves the definition of *object*, *arrows*, *composition* and *identity* to us. It gives a powerful abstraction in functional programming and makes a functional programming language look very much like a category.

Let's consider an example: category of real objects 𝐑 and arrows `𝐑 ⟶ 𝐑`. It's identity function return input and composition chains functions together:   

```erlang
id(X)  -> X.
'.'(F,G) -> fun(X) -> G(F(X)) end.

%% example (X + 3) ^ 2
f(X) -> X + 3.
g(X) -> X * X.

h(X) -> ('.'(fun f/1, fun g/1))(X).  %% g(f(X)).
```

## Composition in Erlang

Erlang do not support infix notation. Therefore, the usage of *composition* function make code unreadable. The nested function calls `g(f(X))` is only the out-of-the-box solution. Unfortunately, chaining of ordinary functions do not allow us to make arbitrary program. 

The usage of `parse_transform` helps us to define a infix notation for *composition*, which is compiled into "nested" function calls.

```erlang
h(X) ->
    [$. ||      %% composition operator
        f(X),   %% arrow 𝐑 ⟶ 𝐑, feeds result to next arrow 
        g(_),   %% arrow 𝐑 ⟶ 𝐑, feeds result to next arrow  
        ...     %% and so on
    ].
```


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
