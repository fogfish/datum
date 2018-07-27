# Monad

The library facilitates a pure functional programming by providing a set of utility functions to work with monads. It also provides the definition of several common monads and defines extension interface using Erlang parse-transform so that users can define their own monads. 

We will skip the monad definitions here. These documents provide excessive explanation of monads:

* [Monads in functional programming](https://en.wikipedia.org/wiki/Monad_(functional_programming))
* [A Fistful of Monads](http://learnyouahaskell.com/a-fistful-of-monads) 
* [Monads for functional programming](http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf)


## "do"-notation

The library implements rough Haskell's equivalent of "do"-notation, so called monadic binding form, using parse-transforms and special form of list comprehension. We used a techniques similar to [erlando](https://github.com/rabbitmq/erlando). This construction decorates computation pipeline(s) with additional rules implemented by monad, they defines programmable commas. 

```erlang
-compile({parse_transform, category}).

f() ->
   [m_identity ||
      X <- 10,
      Y <- 11,
      unit(X + Y)
   ].
```

The used syntax expresses various programming concepts in terms of a monad structures: side-effects, variable assignment, error handling, parsing, concurrency, domain specific languages, etc. 

The library transforms `[ atom() || ... ]` syntax construction to monadic binding using `atom()` as identity of monad module if it starts with **`m_`** prefix (e.g. `m_identity`, `m_state`). List comprehension generators `X <- ...` are transformed into symbol pattern matching rules. For example the following computation produces a list `[1, 2, 100, 101]`

```erlang
f() ->
   [m_identity ||
      [X, Y] <- unit([1, 2]),
      {A, B} <- unit({100, 101}),
      unit([X, Y, A, B])
   ].
```  

Monads are defined in terms of `unit`, `bind` and `fail` operations

```erlang
%% 
%% return :: a -> m a
-spec unit(A) -> m(A).

%%
%% (>>=) :: m a -> (a -> m b) -> m b
-spec '>>='(m(A), fun((A) -> m(B))) -> m(B).

%%
%% fail :: String -> m a
-spec fail(_) -> m(_).
```

Let's show these operation at "do"-notation 

```erlang
-compile({parse_transform, category}).
f() ->
   [m_identity ||      %% (Monad m)
      X <- one(),      %% (>>=) :: m a -> (a -> m b) -> m b
      Y <- two(),      %% (>>=) :: m a -> (a -> m b) -> m b
      unit(X + Y)      %% return :: (Monad m) => a -> m a
   ].
```


### unit

The operation takes non-monadic value or plain type expression and "lifts" it into container using monadic constructor. The library also implements an operand `=<` as syntax equivalence of `unit` to lift expression into monad. For instance, next computation produces a function that returns `4`. 

```erlang
f() ->
   [m_io ||
      X <- unit(2),
      Y =< X * X,
      unit(Y)
   ].
```


### fail

The operation enable failures in a special syntactic construct for monads. The operation is rarely used but allows to escalate failure within computation. The failure is either indicated using `fail` operation from specified monad.


## anonymous variables

The library defines a special syntax to chain computations and they result through do-notation without explicit definition of binding variables. An anonymous variable `_` hold the result of previous statement. For example, the computation holds the result `15` 

```erlang
f() ->
   [m_identity ||
      unit(2),         %% _ <- 2 
      unit(_ * 2),     %% _ <- 2 * 2 ( 4)
      unit(_ + 1),     %% _ <- 4 + 1 ( 5)
      unit(_ * 3)      %% _ <- 5 * 3 (15)
   ].
```


## transforms

Monads provides transforms. These function are defined by the monad class and allows to transform monadic values, executed side effect, etc. The library defines operand `/=` to call utility operation from specified monad. For example, following computation holds result 3 and puts it to state. 

```erlang
f() ->
   [m_state || 
      A =< 1,
      B =< 2,
      C =< A + B,
      cats:put(lens:t1(), C)
   ].
```

## References

1. http://stenmans.org/happi_blog/?p=181
2. http://www.rabbitmq.com/blog/2011/05/17/can-you-hear-the-drums-erlando/
3. https://github.com/rabbitmq/erlando
4. https://typelevel.org/blog/2017/05/02/io-monad-for-cats.html
