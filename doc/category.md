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

The `parse_transform` feature implements a syntax sugar for *composition*, which is compiled into valid Erlang code using defined composition operator at compile-time.

```erlang
-compile({parse_transform, category}).
```

The *function composition* within the category are written with the following syntax

```erlang
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

```erlang
[Category || Var1 <- Arrow1, ..., VarN <- ArrowN]
```

Here, `Category` is an identity of category and each `Arrow` is a morphism applied to objects, `Var` is a named or blank variable that preserve intermediate state. Formally, this is a composition of functions: `f . update . g`.

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

Category pattern provides a powerful abstraction to build computations. Each category is specialises to compose one type of *objects*. In reality, we need to use several categories at once. The composition requires a transformation: category to category, types within category. As an example, a lifting of the object into category.

Transformers are written using following syntax

```erlang
[Category || cats:transform(...), ..., VarN /= transform(...) ]
```

Here, `Category` is an identity of category, `cats:` is reserved prefix that executes the defined transformation as part of the composition, `/= `yields the result of transformation to the state. Formally, this is a composition of functions: `f . transform . update . g`.


## Categories

The library defines a category pattern and implements few categories. It also provides an interface for developers to implement own categories.


### Identity

The identity category chains ordinary functions.

```erlang
-type category() :: $. | identity.
-type object ()  :: _.
-type arrows()   :: fun((_) -> object()).

[identity || f(), g(_)].

%% it is transformed into
g(f()).
```

The category implements transformers

```erlang
%% Transforms `either` category into `identity`, 
%% it maps right branch into meaningful value,
%% left branch to `undefined`
-spec eitherT(either(_, _)) -> undefined | _.
```


### Option

The category operates with objects of polymorphic type that represents encapsulation of an optional value. Its arrows are functions that may or may not returns meaningful value. In Erlang, we are using `undefined` atom as empty constructor. The composition implements earlier exit, we stop chain execution, not continue in some undefined state.

```erlang
-type category() :: $? | option.
-type object()   :: option(_).
-type option(T)  :: undefined | T.
-type arrows()   :: fun((_) -> option(_)).

[option || f(), g(_)].

%% it is transformed into
case f() of
   undefined -> 
      undefined;
   X ->
      g(X)
ends
```

The category implements transformers

```erlang
%% Transforms `either` category into `option`, 
%% it maps right branch into meaningful value,
%% left branch to `undefined`
-spec eitherT(either(_, _)) -> option(_).

%% Transforms sequence of options into optional sequence. 
%% The sequence is not undefined if input has any undefined value.
-spec sequence([option()]) -> option([_]).
```



### Undefined

The category is an inverse to `option` category. The composition implements earlier exit, we stop chain execution, not continue in some defined state.

```erlang
-type category() :: undefined.
-type object()   :: option(_).
-type option(T)  :: undefined | T.
-type arrows()   :: fun((_) -> option(_)).

[undefined || f(), g()].

%% it is transformed into
case f() of
   undefined -> 
      g();
   X ->
      X
ends
```

The category implements transformers

```erlang
%% Transforms `either` category into `option`, 
%% it maps right branch into meaningful value,
%% left branch to `undefined`
-spec eitherT(either(_, _)) -> option(_).

%% Transforms sequence of options into optional sequence. 
%% The sequence is not undefined if input has any undefined value.
-spec sequence([option()]) -> option([_]).
```



### Either

The category operates with objects of polymorphic type that represents a value of two possible types either an instance of left or right. A typical usage is a representation of either correct -- right value or an error -- left value. The category requires that arrows returns a tagged tuple to represent left/right branches. The category uses `{ok, _}` as right and `{error, _}` as left notations for tuples. The composition implements earlier exit, we stop chain execution, not continue in some broken state.

```erlang
-type category() :: $^ | either.
-type object()   :: either(_, _).
-type either(L,R):: {ok, R} | {error, L}.
-type arrows()   :: fun((_) -> either(_, _)).

[either || f(), g(_)].

%% it is transformed into
case f() of
   {error, _} = Error -> 
      Error;
   {ok, X} ->
      g(X)
ends
```

The category implements transformers

```erlang
%%  Lifts value into right branch of either type
-spec unit(_) -> either(_, _). 

%% Transforms `option` category into `either`, 
%% it maps meaningful value into right branch,
%% `undefined` value into left.
-spec optionT(_, option(_)) -> either(_, _).

%% Transforms sequence of either values into either sequence.
-spec sequence([either()]) -> either(_, [_]).
```

### Reader

Often computations require access to shared environment. These computations read values from environment, apply morphism and execute sub-computation but they do not require to understand the type of environment, write anything back, etc. 

Consider the problem of instantiating templates, you need to substitute variables with they values. We can represent a shared environment of all know variables through reader category. When a variable substitution is encountered, we can ask reader to lookup the value of the variable from environment. 

The reader category defines an abstraction of readonly environments and lenses to focus inside it. In other words, the category represents the environment as an "invisible" side-effect of the composition. 

This implementation of Reader category pattern is similar to Either category. It abstracts the environment as a function from the environment to a value of computation, transformers have access to the shared environment.  

```erlang
-type category() :: reader.
-type object()   :: either(_, _).
-type either(L,R):: {ok, R} | {error, L}.
-type arrows()   :: fun((_) -> either(_, _)).

[reader || f(), _ /= g(_)].

%% it is transformed into
fun(Env) ->
   case f() of
      {error, _} = Error -> 
         Error;
      {ok, X} ->
         g(X, Env)
   end.
```

The category implements transformers

```erlang
%%  Lifts value into right branch of either type
-spec unit(_) -> either(_, _). 

%% Transforms `option` category into `either`, 
%% it maps meaningful value into right branch,
%% `undefined` value into left.
-spec optionT(_, option(_)) -> either(_, _).

%% Transforms sequence of either values into either sequence.
-spec sequence([either()]) -> either(_, [_]).
```


### Kleisli (Monads)

> A monad is just a monoid in the category of endofunctors... 

We are using a category pattern to implement the category of monadic functions, which generalises ordinary functions. We will skip the monad definitions here. These documents provide excessive explanation of monads:

* [Monads in functional programming](https://en.wikipedia.org/wiki/Monad_(functional_programming))
* [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads) 
* [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)


```erlang
-type category() :: atom().
-type object()   :: m(_).
-type arrows()   :: fun((m(_)) -> m(_)).

[m_state || f(), g(_)].

%% it is transformed into
m_state:'>>='(f(),
   fun(X) ->
      m_state:'>>='(g(X),
         fun(Y) ->
            m_state:unit(Y)
         end
      )
   end
).
```

The library implements rough Haskell's equivalent of **do-notation**, so called monadic binding form. This construction decorates computation pipeline(s) with additional rules implemented by monad, they defines programmable commas. The used syntax expresses various programming concepts in terms of a monad structures: side-effects, variable assignment, error handling, parsing, concurrency, domain specific languages, etc. 


## References

1. [The category design pattern](http://www.haskellforall.com/2012/08/the-category-design-pattern.html)
2. [Category Theory for Computing Science](http://www.math.mcgill.ca/triples/Barr-Wells-ctcs.pdf)
3. [Category Theory for Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf)

